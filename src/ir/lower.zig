//! Lower lambdamono into cor-style IR.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const types = @import("types");
const lambdamono = @import("mir").Executable;
const type_mod = lambdamono.Type;
const symbol_mod = @import("symbol");
const ast = @import("ast.zig");
const ir_layout = @import("layout.zig");

const Symbol = symbol_mod.Symbol;

/// Final IR lowering result, including the lowered IR store and layout graph.
pub const Result = struct {
    store: ast.Store,
    root_defs: std.ArrayList(ast.DefId),
    symbols: symbol_mod.Store,
    layouts: ir_layout.Graph,
    strings: base.StringLiteral.Store,

    /// Release all memory owned by the IR lowering result.
    pub fn deinit(self: *Result) void {
        self.store.deinit();
        self.root_defs.deinit(self.store.allocator);
        self.symbols.deinit();
        self.layouts.deinit(self.store.allocator);
        self.strings.deinit(self.store.allocator);
    }

    pub fn take(self: *Result, allocator: std.mem.Allocator) Result {
        const result = self.*;
        self.* = .{
            .store = ast.Store.init(allocator),
            .root_defs = .empty,
            .symbols = symbol_mod.Store.init(allocator),
            .layouts = .{},
            .strings = .{},
        };
        return result;
    }
};

/// Lower lambdamono into the shared IR representation.
pub fn run(allocator: std.mem.Allocator, input: *lambdamono.Lower.Result) std.mem.Allocator.Error!Result {
    var lowerer = Lowerer.init(allocator, try input.take(allocator));
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
    current_def_ret_layout: ?ast.LayoutRef = null,
    current_def_ret_ty: ?lambdamono.Type.TypeId = null,

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

        fn init(_: std.mem.Allocator) LoweredBlock {
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
        const layouts = self.input.layouts.graph;
        self.input.layouts.graph = .{};

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
        const saved_ret_layout = self.current_def_ret_layout;
        const saved_ret_ty = self.current_def_ret_ty;
        self.current_def_ret_layout = self.input.layouts.defRetLayout(def_id);
        self.current_def_ret_ty = switch (def.value) {
            .fn_ => |fn_def| def.result_ty orelse self.input.store.getExpr(fn_def.body).ty,
            .hosted_fn => def.result_ty,
            .val => |expr_id| def.result_ty orelse self.input.store.getExpr(expr_id).ty,
            .run => |run_def| def.result_ty orelse self.input.store.getExpr(run_def.body).ty,
        };
        defer self.current_def_ret_layout = saved_ret_layout;
        defer self.current_def_ret_ty = saved_ret_ty;

        return switch (def.value) {
            .fn_ => |fn_def| blk: {
                const args = try self.lowerTypedSymbolSpan(fn_def.args);
                const env = try self.envFromVarSpan(args);
                defer self.allocator.free(env);
                const body_ty = self.input.store.getExpr(fn_def.body).ty;
                break :blk .{
                    .name = def.bind,
                    .args = args,
                    .body = try self.lowerBlockExpecting(env, fn_def.body, def.result_ty orelse body_ty, "def_ret"),
                    .ret_layout = self.input.layouts.defRetLayout(def_id),
                };
            },
            .hosted_fn => |hosted_fn| .{
                .name = def.bind,
                .args = try self.lowerTypedSymbolSpan(hosted_fn.args),
                .ret_layout = self.input.layouts.defRetLayout(def_id),
                .hosted = hosted_fn.hosted,
            },
            .val => |expr_id| blk: {
                const body_ty = self.input.store.getExpr(expr_id).ty;
                break :blk .{
                    .name = def.bind,
                    .args = try self.output.addVarSpan(&.{}),
                    .body = try self.lowerBlockExpecting(&.{}, expr_id, def.result_ty orelse body_ty, "def_ret"),
                    .ret_layout = self.input.layouts.defRetLayout(def_id),
                };
            },
            .run => |run_def| blk: {
                const body_ty = self.input.store.getExpr(run_def.body).ty;
                break :blk .{
                    .name = def.bind,
                    .args = try self.output.addVarSpan(&.{}),
                    .body = try self.lowerBlockExpecting(&.{}, run_def.body, def.result_ty orelse body_ty, "def_ret"),
                    .ret_layout = self.input.layouts.defRetLayout(def_id),
                    .entry_ty = run_def.entry_ty,
                };
            },
        };
    }

    fn lowerTypedSymbol(self: *Lowerer, value: lambdamono.Ast.TypedSymbol) std.mem.Allocator.Error!ast.Var {
        return .{
            .layout = self.input.layouts.layoutForType(value.ty),
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

    fn lowerBlockExpecting(
        self: *Lowerer,
        env: []const EnvEntry,
        expr_id: lambdamono.Ast.ExprId,
        target_ty: lambdamono.Type.TypeId,
        comptime label: []const u8,
    ) std.mem.Allocator.Error!ast.BlockId {
        var lowered = try self.lowerExpr(env, expr_id);
        defer lowered.deinit(self.allocator);
        try self.bridgeBlockTermToType(&lowered, self.input.store.getExpr(expr_id).ty, target_ty, label);
        const stmt_span = try self.addStmtSpan(lowered.stmts.items);
        return try self.output.addBlock(.{
            .stmts = stmt_span,
            .term = lowered.requireTerm(),
        });
    }

    fn lowerExpr(self: *Lowerer, env: []const EnvEntry, expr_id: lambdamono.Ast.ExprId) std.mem.Allocator.Error!LoweredBlock {
        return self.lowerExprReplacingRuntimeError(env, expr_id, null);
    }

    fn lowerExprReplacingRuntimeError(
        self: *Lowerer,
        env: []const EnvEntry,
        expr_id: lambdamono.Ast.ExprId,
        runtime_replacement: ?ast.BlockId,
    ) std.mem.Allocator.Error!LoweredBlock {
        const expr = self.input.store.getExpr(expr_id);
        var block = LoweredBlock.init(self.allocator);
        errdefer block.deinit(self.allocator);

        switch (expr.data) {
            .var_ => |symbol| {
                if (self.lookupEnv(env, symbol)) |value| {
                    block.setTerm(.{ .value = value });
                } else if (self.value_thunks.get(symbol)) |value_thunk| {
                    const temp = try self.freshVarWithLayout(self.procRetLayoutRef(value_thunk.proc), "thunk_result");
                    try block.stmts.append(self.allocator, .{ .let_ = .{
                        .bind = temp,
                        .expr = try self.output.addExpr(.{ .call_direct = .{
                            .proc = value_thunk.proc,
                            .args = try self.output.addVarSpan(&.{}),
                        } }),
                    } });
                    block.setTerm(.{ .value = temp });
                } else {
                    const symbol_entry = self.input.symbols.get(symbol);
                    std.debug.panic(
                        "ir.lower missing env binding for symbol {d} ident={any} origin {s}",
                        .{ symbol.raw(), symbol_entry.name, @tagName(symbol_entry.origin) },
                    );
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
                const lowered_fields = try self.lowerFieldValues(&block, env, expr.ty, fields);
                if (lowered_fields == null) return if (block.has_term) block else debugPanic("ir.lower record missing terminator");
                const target_layout = self.input.layouts.layoutForType(expr.ty);
                const source_field_tys = try self.orderedRecordFieldTypes(expr.ty, self.input.store.sliceFieldExprSpan(fields));
                defer self.allocator.free(source_field_tys);
                const target_field_tys = try self.recordFieldTypesOwned(expr.ty);
                defer self.allocator.free(target_field_tys);
                const bridged_fields = try self.bridgeStructFieldsToTargetLayout(
                    &block,
                    target_layout,
                    lowered_fields.?,
                    source_field_tys,
                    target_field_tys,
                );
                try self.emitLeafExpr(&block, expr.ty, "record", .{ .make_struct = bridged_fields });
            },
            .tuple => |items| {
                const lowered_items = try self.lowerValueSpan(&block, env, items);
                if (lowered_items == null) return if (block.has_term) block else debugPanic("ir.lower tuple missing terminator");
                const target_layout = self.input.layouts.layoutForType(expr.ty);
                const source_item_tys = try self.exprTypesForSpan(items);
                defer self.allocator.free(source_item_tys);
                const target_item_tys = self.tupleTypeElems(expr.ty) orelse
                    debugPanic("ir.lower tuple missing executable element types");
                const bridged_items = try self.bridgeStructFieldsToTargetLayout(
                    &block,
                    target_layout,
                    lowered_items.?,
                    source_item_tys,
                    target_item_tys,
                );
                try self.emitLeafExpr(&block, expr.ty, "tuple", .{ .make_struct = bridged_items });
            },
            .tag_payload => |tag_payload| {
                const tag_union = try self.lowerSubexprValue(&block, env, tag_payload.tag_union);
                if (tag_union == null) return if (block.has_term) block else debugPanic("ir.lower tag_payload missing terminator");
                const payload_layout = self.input.layouts.exprTagPayloadLayout(expr_id);
                const payload_var = try self.emitUnionPayloadValue(
                    &block.stmts,
                    tag_union.?,
                    tag_payload.tag_discriminant,
                    payload_layout,
                    "tag_payload",
                );
                const source_field_ty = self.tagUnionArgs(
                    self.input.store.getExpr(tag_payload.tag_union).ty,
                    tag_payload.tag_discriminant,
                ) orelse debugPanic("ir.lower tag_payload missing source payload types");
                if (tag_payload.payload_index >= source_field_ty.len) {
                    debugPanic("ir.lower tag_payload payload index out of bounds");
                }
                const temp = try self.emitStructFieldValue(
                    &block,
                    payload_var,
                    tag_payload.payload_index,
                    source_field_ty[tag_payload.payload_index],
                    expr.ty,
                    "tag_payload_field",
                );
                block.setTerm(.{ .value = temp });
            },
            .list => |items| {
                const lowered_items = try self.lowerValueSpan(&block, env, items);
                if (lowered_items == null) return if (block.has_term) block else debugPanic("ir.lower list missing terminator");
                const item_exprs = self.input.store.sliceExprSpan(items);
                const list_elem_ty = self.listTypeElem(expr.ty) orelse
                    debugPanic("ir.lower list expression missing explicit list element type");
                const storage_elem_ty = self.storageTypeForListElement(list_elem_ty);
                const elem_plans = try self.allocator.alloc(ast.BridgePlanId, item_exprs.len);
                defer self.allocator.free(elem_plans);
                for (item_exprs, 0..) |item_expr, i| {
                    elem_plans[i] = try self.bridgePlanForTypes(
                        self.input.store.getExpr(item_expr).ty,
                        storage_elem_ty,
                    );
                }
                try self.emitLeafExpr(&block, expr.ty, "list", .{ .make_list = .{
                    .elems = lowered_items.?,
                    .elem_bridge_plans = try self.output.addBridgePlanSpan(elem_plans),
                } });
            },
            .tag => |tag| {
                const lowered_args = try self.lowerValueSpan(&block, env, tag.args);
                if (lowered_args == null) return if (block.has_term) block else debugPanic("ir.lower tag missing terminator");

                const payload: ?ast.Var = blk: {
                    const args = self.output.sliceVarSpan(lowered_args.?);
                    if (args.len == 0) break :blk null;
                    const payload_layout = self.input.layouts.exprTagPayloadLayout(expr_id);
                    const payload_var = try self.freshVarWithLayout(
                        payload_layout,
                        "tag_payload",
                    );
                    const source_arg_tys = try self.exprTypesForSpan(tag.args);
                    defer self.allocator.free(source_arg_tys);
                    const target_arg_tys = self.tagUnionArgs(expr.ty, tag.discriminant) orelse
                        debugPanic("ir.lower tag missing executable payload types");
                    const bridged_args = try self.bridgeStructFieldsToTargetLayout(
                        &block,
                        payload_layout,
                        lowered_args.?,
                        source_arg_tys,
                        target_arg_tys,
                    );
                    try block.stmts.append(self.allocator, .{ .let_ = .{
                        .bind = payload_var,
                        .expr = try self.output.addExpr(.{ .make_struct = bridged_args }),
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
                const temp = try self.emitStructFieldValue(
                    &block,
                    record.?,
                    access.field_index,
                    self.fieldTypeAt(self.input.store.getExpr(access.record).ty, access.field_index) orelse
                        debugPanic("ir.lower access missing source field type"),
                    expr.ty,
                    "field",
                );
                block.setTerm(.{ .value = temp });
            },
            .tuple_access => |tuple_access| {
                const tuple = try self.lowerSubexprValue(&block, env, tuple_access.tuple);
                if (tuple == null) return if (block.has_term) block else debugPanic("ir.lower tuple_access missing terminator");
                const field_index: u16 = @intCast(tuple_access.elem_index);
                const temp = try self.emitStructFieldValue(
                    &block,
                    tuple.?,
                    field_index,
                    self.fieldTypeAt(self.input.store.getExpr(tuple_access.tuple).ty, field_index) orelse
                        debugPanic("ir.lower tuple_access missing source element type"),
                    expr.ty,
                    "tuple_field",
                );
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

                var rest = try self.lowerExprReplacingRuntimeError(extended_env, let_expr.rest, runtime_replacement);
                defer rest.deinit(self.allocator);
                try self.appendChildBlock(&block, &rest);
            },
            .bridge => |source_expr| {
                const source = try self.lowerSubexprValue(&block, env, source_expr);
                if (source == null) return if (block.has_term) block else debugPanic("ir.lower bridge missing terminator");
                const temp = try self.freshVar(expr.ty, "bridge");
                const bridge_expr = try self.makeBridgeExpr(
                    source.?,
                    self.input.store.getExpr(source_expr).ty,
                    expr.ty,
                );
                try block.stmts.append(self.allocator, .{ .let_ = .{
                    .bind = temp,
                    .expr = bridge_expr,
                } });
                block.setTerm(.{ .value = temp });
            },
            .call => |call| {
                const args = try self.lowerValueSpan(&block, env, call.args);
                if (args == null) return if (block.has_term) block else debugPanic("ir.lower call missing terminator");
                const temp = try self.freshVarWithLayout(self.procRetLayoutRef(call.proc), "call");
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

                const capture_ptr = if (packed_fn.captures) |captures_expr| blk: {
                    const lowered = try self.lowerSubexprValue(&block, env, captures_expr);
                    if (lowered == null) return if (block.has_term) block else debugPanic("ir.lower packed_fn captures missing terminator");
                    break :blk lowered.?;
                } else blk: {
                    const capture_box_ty = if (packed_fn.capture_ty) |capture_ty|
                        try self.input.types.internResolved(.{ .box = capture_ty })
                    else blk_capture: {
                        const unit_ty = try self.input.types.internResolved(.{ .record = .{
                            .fields = &.{},
                        } });
                        break :blk_capture try self.input.types.internResolved(.{ .box = unit_ty });
                    };
                    const null_ptr = try self.freshVar(capture_box_ty, "null_ptr");
                    try block.stmts.append(self.allocator, .{ .let_ = .{
                        .bind = null_ptr,
                        .expr = try self.output.addExpr(.null_ptr),
                    } });
                    break :blk null_ptr;
                };

                const size_var = if (packed_fn.capture_ty) |capture_ty| blk: {
                    const u32_ty = try self.input.types.internResolved(.{ .primitive = .u32 });
                    const capture_layout = self.input.layouts.layoutForType(capture_ty);
                    const size_var = try self.freshVar(u32_ty, "capture_size");
                    try block.stmts.append(self.allocator, .{ .let_ = .{
                        .bind = size_var,
                        .expr = try self.output.addExpr(.{ .layout_size = capture_layout }),
                    } });
                    break :blk size_var;
                } else blk: {
                    const u32_ty = try self.input.types.internResolved(.{ .primitive = .u32 });
                    const size_var = try self.freshVar(u32_ty, "capture_size");
                    try block.stmts.append(self.allocator, .{ .let_ = .{
                        .bind = size_var,
                        .expr = try self.output.addExpr(.{ .lit = .{ .int = 0 } }),
                    } });
                    break :blk size_var;
                };

                const temp = try self.freshVar(expr.ty, "packed_fn");
                try block.stmts.append(self.allocator, .{ .let_ = .{
                    .bind = temp,
                    .expr = try self.output.addExpr(.{
                        .make_struct = try self.output.addVarSpan(&.{ fn_ptr, capture_ptr, size_var }),
                    }),
                } });
                block.setTerm(.{ .value = temp });
            },
            .call_erased => |call| {
                const func = try self.lowerSubexprValue(&block, env, call.func);
                if (func == null) return if (block.has_term) block else debugPanic("ir.lower call_erased missing func terminator");
                const args = try self.lowerValueSpan(&block, env, call.args);
                if (args == null) return if (block.has_term) block else debugPanic("ir.lower call_erased missing args terminator");
                const temp = try self.freshVar(expr.ty, "call_erased");
                try block.stmts.append(self.allocator, .{ .let_ = .{
                    .bind = temp,
                    .expr = try self.output.addExpr(.{ .call_erased = .{
                        .func = func.?,
                        .args = args.?,
                        .capture_layout = if (call.capture_ty) |capture_ty|
                            self.input.layouts.layoutForType(capture_ty)
                        else
                            null,
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
            .when => |when_expr| try self.lowerWhenExpr(&block, env, expr.ty, when_expr.cond, when_expr.branches, runtime_replacement),
            .if_ => |if_expr| try self.lowerIfExpr(&block, env, expr.ty, if_expr.cond, if_expr.then_body, if_expr.else_body),
            .block => |body| try self.lowerBlockExprReplacingRuntimeError(&block, env, body.stmts, body.final_expr, runtime_replacement),
            .return_ => |ret_expr| {
                const value = try self.lowerSubexprValue(&block, env, ret_expr);
                if (value == null) return if (block.has_term) block else debugPanic("ir.lower return missing terminator");
                const ret_layout = self.current_def_ret_layout orelse
                    debugPanic("ir.lower return missing current function return layout");
                const ret_value = if (std.meta.eql(value.?.layout, ret_layout))
                    value.?
                else blk: {
                    const bridged = try self.freshVarWithLayout(ret_layout, "return");
                    const ret_ty = self.current_def_ret_ty orelse
                        debugPanic("ir.lower return missing current function return type");
                    try block.stmts.append(self.allocator, .{ .let_ = .{
                        .bind = bridged,
                        .expr = try self.makeBridgeExpr(
                            value.?,
                            self.input.store.getExpr(ret_expr).ty,
                            ret_ty,
                        ),
                    } });
                    break :blk bridged;
                };
                block.setTerm(.{ .return_ = ret_value });
            },
            .runtime_error => {
                if (runtime_replacement) |replacement| {
                    try self.appendStoredBlock(&block, replacement);
                } else {
                    block.setTerm(.runtime_error);
                }
            },
            .for_ => |for_expr| try self.lowerForExpr(&block, env, for_expr.patt, for_expr.iterable, for_expr.body),
        }

        if (!block.has_term) debugPanic("ir.lower invariant violated: expr lowered without terminator");
        return block;
    }

    fn freshVar(self: *Lowerer, ty: lambdamono.Type.TypeId, comptime label: []const u8) std.mem.Allocator.Error!ast.Var {
        const symbol = try self.freshSymbol(label);
        return .{
            .layout = self.input.layouts.layoutForType(ty),
            .symbol = symbol,
        };
    }

    fn freshVarWithLayout(
        self: *Lowerer,
        layout: ir_layout.Ref,
        comptime label: []const u8,
    ) std.mem.Allocator.Error!ast.Var {
        const symbol = try self.freshSymbol(label);
        return .{
            .layout = layout,
            .symbol = symbol,
        };
    }

    fn freshOpaqueVar(self: *Lowerer, comptime label: []const u8) std.mem.Allocator.Error!ast.Var {
        return .{
            .layout = .{ .canonical = .opaque_ptr },
            .symbol = try self.freshSymbol(label),
        };
    }

    fn procRetLayoutRef(self: *const Lowerer, proc_symbol: Symbol) ir_layout.Ref {
        for (self.input.store.defsSlice(), 0..) |def, i| {
            if (def.bind != proc_symbol) continue;
            return self.input.layouts.defRetLayout(@enumFromInt(@as(u32, @intCast(i))));
        }
        debugPanic("ir.lower missing proc return layout for direct call");
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

    fn bridgeBlockTermToType(
        self: *Lowerer,
        block: *LoweredBlock,
        source_ty: lambdamono.Type.TypeId,
        target_ty: lambdamono.Type.TypeId,
        comptime label: []const u8,
    ) std.mem.Allocator.Error!void {
        const value = switch (block.term) {
            .value => |value| value,
            else => return,
        };
        const target_layout = self.input.layouts.layoutForType(target_ty);
        if (std.meta.eql(value.layout, target_layout)) return;

        const bridged = try self.freshVar(target_ty, label);
        try block.stmts.append(self.allocator, .{ .let_ = .{
            .bind = bridged,
            .expr = try self.makeBridgeExpr(value, source_ty, target_ty),
        } });
        block.setTerm(.{ .value = bridged });
    }

    fn makeBridgeExpr(
        self: *Lowerer,
        value: ast.Var,
        source_ty: lambdamono.Type.TypeId,
        target_ty: lambdamono.Type.TypeId,
    ) std.mem.Allocator.Error!ast.ExprId {
        return try self.output.addExpr(.{ .bridge = .{
            .value = value,
            .plan = try self.bridgePlanForTypes(source_ty, target_ty),
        } });
    }

    const BridgeTypePair = struct {
        source: lambdamono.Type.TypeId,
        target: lambdamono.Type.TypeId,
    };

    fn bridgePlanForTypes(
        self: *Lowerer,
        source_ty: lambdamono.Type.TypeId,
        target_ty: lambdamono.Type.TypeId,
    ) std.mem.Allocator.Error!ast.BridgePlanId {
        var active = std.ArrayList(BridgeTypePair).empty;
        defer active.deinit(self.allocator);
        return try self.bridgePlanForTypesRec(source_ty, target_ty, &active);
    }

    fn bridgePlanForTypesRec(
        self: *Lowerer,
        source_ty: lambdamono.Type.TypeId,
        target_ty: lambdamono.Type.TypeId,
        active: *std.ArrayList(BridgeTypePair),
    ) std.mem.Allocator.Error!ast.BridgePlanId {
        const source_layout = self.input.layouts.layoutForType(source_ty);
        const target_layout = self.input.layouts.layoutForType(target_ty);
        if (std.meta.eql(source_layout, target_layout)) {
            return try self.output.addBridgePlan(.direct);
        }
        if (self.layoutRefIsZst(target_layout)) {
            return try self.output.addBridgePlan(.zst);
        }

        for (active.items) |pair| {
            if (pair.source == source_ty and pair.target == target_ty) {
                return try self.output.addBridgePlan(.nominal_reinterpret);
            }
        }
        try active.append(self.allocator, .{ .source = source_ty, .target = target_ty });
        defer _ = active.pop();

        if (self.sameNonListBackingLayout(source_layout, target_layout)) {
            return try self.output.addBridgePlan(.nominal_reinterpret);
        }

        const source_preserved = self.input.types.getTypePreservingNominal(source_ty);
        const target_preserved = self.input.types.getTypePreservingNominal(target_ty);
        const source = self.input.types.getType(source_ty);
        const target = self.input.types.getType(target_ty);

        if (self.layoutRefIsOpaquePtr(source_layout)) {
            return try self.output.addBridgePlan(.{ .box_unbox = try self.output.addBridgePlan(.direct) });
        }

        if (source_preserved == .box) {
            return try self.output.addBridgePlan(.{ .box_unbox = try self.bridgePlanForTypesRec(
                source_preserved.box,
                target_ty,
                active,
            ) });
        }

        if (target_preserved == .box) {
            return try self.output.addBridgePlan(.{ .box_box = try self.bridgePlanForTypesRec(
                source_ty,
                target_preserved.box,
                active,
            ) });
        }

        if (source == .list and target == .list) {
            const elem_plan = try self.bridgePlanForTypesRec(source.list, target.list, active);
            if (!self.bridgePlanCanListReinterpret(elem_plan)) {
                debugPanic("ir.lower list bridge element plan is not representable as a list reinterpret");
            }
            return try self.output.addBridgePlan(.list_reinterpret);
        }

        if (source == .tuple and target == .tuple) {
            return try self.structBridgePlanFromTypeSpans(source.tuple, target.tuple, active);
        }

        if (source == .record and target == .record) {
            const source_fields = source.record.fields;
            const target_fields = target.record.fields;
            if (source_fields.len != target_fields.len) {
                debugPanic("ir.lower record bridge field arity mismatch");
            }
            const ids = try self.allocator.alloc(ast.BridgePlanId, source_fields.len);
            defer self.allocator.free(ids);
            for (source_fields, target_fields, 0..) |source_field, target_field, i| {
                if (source_field.name != target_field.name) {
                    debugPanic("ir.lower record bridge field name mismatch");
                }
                ids[i] = try self.bridgePlanForTypesRec(source_field.ty, target_field.ty, active);
            }
            return try self.output.addBridgePlan(.{ .struct_ = try self.output.addBridgePlanSpan(ids) });
        }

        if (source == .tag_union and target == .tag_union) {
            return try self.tagUnionBridgePlan(
                source.tag_union.tags,
                target.tag_union.tags,
                source_layout,
                target_layout,
                active,
            );
        }

        if (self.layoutRefIsOpaquePtr(target_layout)) {
            return try self.output.addBridgePlan(.nominal_reinterpret);
        }

        if (builtin.mode == .Debug) {
            std.debug.panic(
                "ir.lower invariant violated: no explicit bridge plan from {s} to {s}",
                .{ @tagName(source), @tagName(target) },
            );
        }
        unreachable;
    }

    fn structBridgePlanFromTypeSpans(
        self: *Lowerer,
        source_items: []const lambdamono.Type.TypeId,
        target_items: []const lambdamono.Type.TypeId,
        active: *std.ArrayList(BridgeTypePair),
    ) std.mem.Allocator.Error!ast.BridgePlanId {
        if (source_items.len != target_items.len) {
            debugPanic("ir.lower struct bridge arity mismatch");
        }
        const ids = try self.allocator.alloc(ast.BridgePlanId, source_items.len);
        defer self.allocator.free(ids);
        for (source_items, target_items, 0..) |source_item, target_item, i| {
            ids[i] = try self.bridgePlanForTypesRec(source_item, target_item, active);
        }
        return try self.output.addBridgePlan(.{ .struct_ = try self.output.addBridgePlanSpan(ids) });
    }

    fn tagUnionBridgePlan(
        self: *Lowerer,
        source_tags: []const type_mod.Tag,
        target_tags: []const type_mod.Tag,
        source_layout: ir_layout.Ref,
        target_layout: ir_layout.Ref,
        active: *std.ArrayList(BridgeTypePair),
    ) std.mem.Allocator.Error!ast.BridgePlanId {
        if (source_tags.len == 1 and target_tags.len == 1) {
            const source_is_zst = self.layoutRefIsZst(source_layout);
            const target_is_zst = self.layoutRefIsZst(target_layout);
            if (source_is_zst or target_is_zst) {
                if (source_is_zst and target_is_zst) {
                    return try self.output.addBridgePlan(.zst);
                }
                debugPanic("ir.lower singleton tag bridge zst mismatch");
            }
        }

        if (source_tags.len == 1 and target_tags.len > 1) {
            const target_discriminant = self.findTagDiscriminant(target_tags, source_tags[0].name) orelse
                debugPanic("ir.lower singleton bridge source tag missing in target union");
            return try self.output.addBridgePlan(.{ .singleton_to_tag_union = .{
                .source_payload = try self.input.layouts.payloadLayoutForUnionLayout(source_layout, 0),
                .target_discriminant = target_discriminant,
                .payload_plan = try self.payloadBridgePlan(source_tags[0].args, target_tags[target_discriminant].args, active),
            } });
        }

        if (target_tags.len == 1 and source_tags.len > 1) {
            const source_discriminant = self.findTagDiscriminant(source_tags, target_tags[0].name) orelse
                debugPanic("ir.lower singleton bridge target tag missing in source union");
            return try self.output.addBridgePlan(.{ .tag_union_to_singleton = .{
                .target_payload = try self.input.layouts.payloadLayoutForUnionLayout(target_layout, 0),
                .source_discriminant = source_discriminant,
                .payload_plan = try self.payloadBridgePlan(source_tags[source_discriminant].args, target_tags[0].args, active),
            } });
        }

        if (source_tags.len != target_tags.len) {
            debugPanic("ir.lower tag-union bridge variant arity mismatch");
        }
        const ids = try self.allocator.alloc(ast.BridgePlanId, source_tags.len);
        defer self.allocator.free(ids);
        for (source_tags, target_tags, 0..) |source_tag, target_tag, i| {
            if (!tagNamesEqual(source_tag.name, target_tag.name)) {
                debugPanic("ir.lower tag-union bridge variant name mismatch");
            }
            ids[i] = (try self.payloadBridgePlan(source_tag.args, target_tag.args, active)) orelse
                try self.output.addBridgePlan(.zst);
        }
        return try self.output.addBridgePlan(.{ .tag_union = try self.output.addBridgePlanSpan(ids) });
    }

    fn payloadBridgePlan(
        self: *Lowerer,
        source_args: []const lambdamono.Type.TypeId,
        target_args: []const lambdamono.Type.TypeId,
        active: *std.ArrayList(BridgeTypePair),
    ) std.mem.Allocator.Error!?ast.BridgePlanId {
        if (source_args.len == 0 and target_args.len == 0) return null;
        return try self.structBridgePlanFromTypeSpans(source_args, target_args, active);
    }

    fn bridgePlanCanListReinterpret(self: *const Lowerer, plan_id: ast.BridgePlanId) bool {
        return switch (self.output.getBridgePlan(plan_id)) {
            .direct, .zst, .nominal_reinterpret => true,
            else => false,
        };
    }

    fn layoutRefIsZst(self: *const Lowerer, ref: ir_layout.Ref) bool {
        return switch (ref) {
            .canonical => |idx| idx == .zst,
            .local => |node_id| switch (self.input.layouts.graph.getNode(node_id)) {
                .pending => debugPanic("ir.lower layoutRefIsZst pending layout"),
                .nominal => |backing| self.layoutRefIsZst(backing),
                .box, .list, .closure => false,
                .struct_ => |fields| blk: {
                    for (self.input.layouts.graph.getFields(fields)) |field| {
                        if (!self.layoutRefIsZst(field.child)) break :blk false;
                    }
                    break :blk true;
                },
                .tag_union => |variants| blk: {
                    const variant_refs = self.input.layouts.graph.getRefs(variants);
                    if (variant_refs.len != 1) break :blk false;
                    break :blk self.layoutRefIsZst(variant_refs[0]);
                },
            },
        };
    }

    fn layoutRefIsOpaquePtr(self: *const Lowerer, ref: ir_layout.Ref) bool {
        _ = self;
        return switch (ref) {
            .canonical => |idx| idx == .opaque_ptr,
            .local => false,
        };
    }

    fn sameNonListBackingLayout(self: *Lowerer, source: ir_layout.Ref, target: ir_layout.Ref) bool {
        if (std.meta.eql(source, target)) return true;
        const source_backing = self.unwrapNominalLayoutRef(source);
        const target_backing = self.unwrapNominalLayoutRef(target);
        if (!std.meta.eql(source_backing, target_backing)) return false;
        return !self.layoutRefIsList(source) and !self.layoutRefIsList(target);
    }

    fn unwrapNominalLayoutRef(self: *Lowerer, ref: ir_layout.Ref) ir_layout.Ref {
        var current = ref;
        while (true) {
            switch (current) {
                .canonical => return current,
                .local => |node_id| switch (self.input.layouts.graph.getNode(node_id)) {
                    .nominal => |backing| current = backing,
                    else => return current,
                },
            }
        }
    }

    fn layoutRefIsList(self: *Lowerer, ref: ir_layout.Ref) bool {
        var current = ref;
        while (true) {
            switch (current) {
                .canonical => return false,
                .local => |node_id| switch (self.input.layouts.graph.getNode(node_id)) {
                    .nominal => |backing| current = backing,
                    .list => return true,
                    else => return false,
                },
            }
        }
    }

    fn findTagDiscriminant(
        self: *Lowerer,
        tags: []const type_mod.Tag,
        name: type_mod.TagName,
    ) ?u16 {
        _ = self;
        for (tags, 0..) |tag, i| {
            if (tagNamesEqual(tag.name, name)) return @intCast(i);
        }
        return null;
    }

    fn emitStructFieldValue(
        self: *Lowerer,
        block: *LoweredBlock,
        record: ast.Var,
        field_index: u16,
        source_field_ty: lambdamono.Type.TypeId,
        result_ty: lambdamono.Type.TypeId,
        comptime label: []const u8,
    ) std.mem.Allocator.Error!ast.Var {
        const actual_field = try self.freshVar(source_field_ty, label);
        try block.stmts.append(self.allocator, .{ .let_ = .{
            .bind = actual_field,
            .expr = try self.output.addExpr(.{ .get_struct_field = .{
                .record = record,
                .field_index = field_index,
                .field_bridge_plan = try self.aggregateFieldBridgePlan(
                    record.layout,
                    field_index,
                    source_field_ty,
                ),
            } }),
        } });

        const target_layout = self.input.layouts.layoutForType(result_ty);
        if (std.meta.eql(actual_field.layout, target_layout)) return actual_field;

        const bridged = try self.freshVar(result_ty, label);
        try block.stmts.append(self.allocator, .{ .let_ = .{
            .bind = bridged,
            .expr = try self.makeBridgeExpr(actual_field, source_field_ty, result_ty),
        } });
        return bridged;
    }

    fn emitUnionPayloadValue(
        self: *Lowerer,
        stmts: *std.ArrayList(ast.Stmt),
        tag_union: ast.Var,
        tag_discriminant: u16,
        target_layout: ast.LayoutRef,
        comptime label: []const u8,
    ) std.mem.Allocator.Error!ast.Var {
        const actual_payload_layout = try self.input.layouts.payloadLayoutForUnionLayout(
            tag_union.layout,
            tag_discriminant,
        );
        const actual_payload = try self.freshVarWithLayout(actual_payload_layout, label);
        try stmts.append(self.allocator, .{ .let_ = .{
            .bind = actual_payload,
            .expr = try self.output.addExpr(.{ .get_union_struct = .{
                .value = tag_union,
                .tag_discriminant = tag_discriminant,
            } }),
        } });
        _ = target_layout;
        return actual_payload;
    }

    fn bridgeStructFieldsToTargetLayout(
        self: *Lowerer,
        block: *LoweredBlock,
        target_layout: ast.LayoutRef,
        fields_span: ast.Span(ast.Var),
        source_field_tys: []const type_mod.TypeId,
        target_field_tys: []const type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.Span(ast.Var) {
        const source_fields = self.output.sliceVarSpan(fields_span);
        if (source_fields.len != source_field_tys.len or source_fields.len != target_field_tys.len) {
            debugPanic("ir.lower bridgeStructFieldsToTargetLayout field/type arity mismatch");
        }
        const bridged_fields = try self.allocator.alloc(ast.Var, source_fields.len);
        defer self.allocator.free(bridged_fields);

        for (source_fields, 0..) |source_field, i| {
            const slot_ref = try self.input.layouts.structFieldLayout(target_layout, @intCast(i));
            if (std.meta.eql(source_field.layout, slot_ref)) {
                bridged_fields[i] = source_field;
                continue;
            }

            const bridged = try self.freshVarWithLayout(slot_ref, "struct_field");
            try block.stmts.append(self.allocator, .{ .let_ = .{
                .bind = bridged,
                .expr = try self.makeBridgeExpr(source_field, source_field_tys[i], target_field_tys[i]),
            } });
            bridged_fields[i] = bridged;
        }

        return try self.output.addVarSpan(bridged_fields);
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
        record_ty: type_mod.TypeId,
        span: lambdamono.Ast.Span(lambdamono.Ast.FieldExpr),
    ) std.mem.Allocator.Error!?ast.Span(ast.Var) {
        const fields = self.input.store.sliceFieldExprSpan(span);
        const lowered_values = try self.allocator.alloc(ast.Var, fields.len);
        defer self.allocator.free(lowered_values);
        for (fields, 0..) |field, i| {
            const value = try self.lowerSubexprValue(block, env, field.value);
            if (value == null) return null;
            const expected_layout = self.input.layouts.layoutForType(self.input.store.getExpr(field.value).ty);
            if (!std.meta.eql(value.?.layout, expected_layout)) {
                const source_expr = self.input.store.getExpr(field.value);
                std.debug.panic(
                    "ir.lower field value layout mismatch field={d} expr={d} expr_tag={s} expr_ty={d} expr_ty_tag={s} symbol={?d} origin={?s} actual_layout={any} expected_layout={any}",
                    .{
                        @as(u32, field.name.idx),
                        @intFromEnum(field.value),
                        @tagName(source_expr.data),
                        @intFromEnum(source_expr.ty),
                        @tagName(self.input.types.getTypePreservingNominal(source_expr.ty)),
                        if (source_expr.data == .var_) source_expr.data.var_.raw() else null,
                        if (source_expr.data == .var_) @tagName(self.input.symbols.get(source_expr.data.var_).origin) else null,
                        value.?.layout,
                        expected_layout,
                    },
                );
            }
            lowered_values[i] = value.?;
        }

        const ordered = try self.orderRecordFieldValues(record_ty, fields, lowered_values);
        defer self.allocator.free(ordered);
        return try self.output.addVarSpan(ordered);
    }

    fn orderRecordFieldValues(
        self: *Lowerer,
        record_ty: type_mod.TypeId,
        fields: []const lambdamono.Ast.FieldExpr,
        lowered_values: []const ast.Var,
    ) std.mem.Allocator.Error![]ast.Var {
        const record_fields = self.recordTypeFields(record_ty) orelse {
            return try self.allocator.dupe(ast.Var, lowered_values);
        };
        if (record_fields.len != fields.len) {
            debugPanic("ir.lower record field arity mismatch");
        }

        var by_name = std.AutoHashMap(base.Ident.Idx, ast.Var).init(self.allocator);
        defer by_name.deinit();
        try by_name.ensureTotalCapacity(@intCast(fields.len));
        for (fields, 0..) |field, i| {
            by_name.putAssumeCapacity(field.name, lowered_values[i]);
        }

        const ordered = try self.allocator.alloc(ast.Var, record_fields.len);
        for (record_fields, 0..) |field, i| {
            ordered[i] = by_name.get(field.name) orelse
                debugPanic("ir.lower missing record field value for layout order");
        }
        return ordered;
    }

    fn recordTypeFields(
        self: *Lowerer,
        record_ty: type_mod.TypeId,
    ) ?[]const type_mod.Field {
        return switch (self.input.types.getTypePreservingNominal(record_ty)) {
            .nominal => |nominal| self.recordTypeFields(nominal.backing),
            .record => |record| record.fields,
            else => null,
        };
    }

    fn tupleTypeElems(
        self: *Lowerer,
        tuple_ty: type_mod.TypeId,
    ) ?[]const type_mod.TypeId {
        return switch (self.input.types.getTypePreservingNominal(tuple_ty)) {
            .nominal => |nominal| self.tupleTypeElems(nominal.backing),
            .tuple => |tuple| tuple,
            else => null,
        };
    }

    fn listTypeElem(
        self: *Lowerer,
        list_ty: type_mod.TypeId,
    ) ?type_mod.TypeId {
        return switch (self.input.types.getTypePreservingNominal(list_ty)) {
            .nominal => |nominal| self.listTypeElem(nominal.backing),
            .list => |elem| elem,
            else => null,
        };
    }

    fn storageTypeForListElement(
        self: *Lowerer,
        elem_ty: type_mod.TypeId,
    ) type_mod.TypeId {
        return switch (self.input.types.getTypePreservingNominal(elem_ty)) {
            .nominal => |nominal| self.storageTypeForListElement(nominal.backing),
            else => elem_ty,
        };
    }

    fn storageTypeForAggregateField(
        self: *Lowerer,
        field_ty: type_mod.TypeId,
    ) type_mod.TypeId {
        return switch (self.input.types.getTypePreservingNominal(field_ty)) {
            .nominal => |nominal| self.storageTypeForAggregateField(nominal.backing),
            else => field_ty,
        };
    }

    fn aggregateFieldBridgePlan(
        self: *Lowerer,
        aggregate_layout: ir_layout.Ref,
        field_index: u16,
        field_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!ast.BridgePlanId {
        const storage_ty = self.storageTypeForAggregateField(field_ty);
        const child_ref = try self.input.layouts.structFieldLayout(aggregate_layout, field_index);
        const child_plan = try self.bridgePlanForTypes(storage_ty, field_ty);
        if (try self.input.layouts.slotEdgeIsRecursivelyBoxed(self.allocator, aggregate_layout, child_ref)) {
            return try self.output.addBridgePlan(.{ .box_unbox = child_plan });
        }
        return child_plan;
    }

    fn tagUnionArgs(
        self: *Lowerer,
        tag_union_ty: type_mod.TypeId,
        discriminant: u16,
    ) ?[]const type_mod.TypeId {
        return switch (self.input.types.getTypePreservingNominal(tag_union_ty)) {
            .nominal => |nominal| self.tagUnionArgs(nominal.backing, discriminant),
            .tag_union => |tag_union| blk: {
                if (discriminant >= tag_union.tags.len) break :blk null;
                break :blk tag_union.tags[discriminant].args;
            },
            else => null,
        };
    }

    fn fieldTypeAt(
        self: *Lowerer,
        aggregate_ty: type_mod.TypeId,
        field_index: usize,
    ) ?type_mod.TypeId {
        return switch (self.input.types.getTypePreservingNominal(aggregate_ty)) {
            .nominal => |nominal| self.fieldTypeAt(nominal.backing, field_index),
            .record => |record| if (field_index < record.fields.len) record.fields[field_index].ty else null,
            .tuple => |tuple| if (field_index < tuple.len) tuple[field_index] else null,
            else => null,
        };
    }

    fn orderedRecordFieldTypes(
        self: *Lowerer,
        record_ty: type_mod.TypeId,
        fields: []const lambdamono.Ast.FieldExpr,
    ) std.mem.Allocator.Error![]type_mod.TypeId {
        const record_fields = self.recordTypeFields(record_ty) orelse {
            const out = try self.allocator.alloc(type_mod.TypeId, fields.len);
            for (fields, 0..) |field, i| {
                out[i] = self.input.store.getExpr(field.value).ty;
            }
            return out;
        };
        if (record_fields.len != fields.len) {
            debugPanic("ir.lower record field type arity mismatch");
        }

        var by_name = std.AutoHashMap(base.Ident.Idx, type_mod.TypeId).init(self.allocator);
        defer by_name.deinit();
        try by_name.ensureTotalCapacity(@intCast(fields.len));
        for (fields) |field| {
            by_name.putAssumeCapacity(field.name, self.input.store.getExpr(field.value).ty);
        }

        const ordered = try self.allocator.alloc(type_mod.TypeId, record_fields.len);
        for (record_fields, 0..) |field, i| {
            ordered[i] = by_name.get(field.name) orelse
                debugPanic("ir.lower missing record field type for layout order");
        }
        return ordered;
    }

    fn exprTypesForSpan(
        self: *Lowerer,
        span: lambdamono.Ast.Span(lambdamono.Ast.ExprId),
    ) std.mem.Allocator.Error![]type_mod.TypeId {
        const exprs = self.input.store.sliceExprSpan(span);
        const out = try self.allocator.alloc(type_mod.TypeId, exprs.len);
        for (exprs, 0..) |expr_id, i| {
            out[i] = self.input.store.getExpr(expr_id).ty;
        }
        return out;
    }

    fn recordFieldTypesOwned(
        self: *Lowerer,
        record_ty: type_mod.TypeId,
    ) std.mem.Allocator.Error![]type_mod.TypeId {
        const record_fields = self.recordTypeFields(record_ty) orelse
            debugPanic("ir.lower recordFieldTypesOwned expected record type");
        const out = try self.allocator.alloc(type_mod.TypeId, record_fields.len);
        for (record_fields, 0..) |field, i| {
            out[i] = field.ty;
        }
        return out;
    }

    fn lowerWhenExpr(
        self: *Lowerer,
        block: *LoweredBlock,
        env: []const EnvEntry,
        result_ty: lambdamono.Type.TypeId,
        cond_expr: lambdamono.Ast.ExprId,
        branches_span: lambdamono.Ast.Span(lambdamono.Ast.BranchId),
        runtime_replacement: ?ast.BlockId,
    ) std.mem.Allocator.Error!void {
        const cond = try self.lowerSubexprValue(block, env, cond_expr);
        if (cond == null) return;

        const discr_layout = self.input.layouts.exprDiscriminantLayout(cond_expr);
        if (discr_layout == null) {
            const ir_branches = self.input.store.sliceBranchSpan(branches_span);
            if (ir_branches.len != 0) {
                var all_var_patterns = true;
                for (ir_branches) |branch_id| {
                    const branch = self.input.store.getBranch(branch_id);
                    const pat = self.input.store.getPat(branch.pat);
                    if (pat.data != .var_) {
                        all_var_patterns = false;
                        break;
                    }
                }
                if (all_var_patterns) {
                    const chained_block = try self.lowerSequentialPatternBranches(env, cond.?, ir_branches, runtime_replacement);
                    try self.appendStoredBlock(block, chained_block);
                    return;
                }
            }
        }

        const discr = if (discr_layout) |resolved_discr_layout| blk: {
            const discr_var = try self.freshVarWithLayout(
                resolved_discr_layout,
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
                    default_block = try self.lowerPatternBranchBlock(env, cond.?, branch.pat, branch.body, result_ty, runtime_replacement);
                },
                .bool_lit => |value| {
                    try tag_branches.append(self.allocator, .{
                        .value = if (value) 1 else 0,
                        .block = try self.lowerPatternBranchBlock(env, cond.?, branch.pat, branch.body, result_ty, runtime_replacement),
                    });
                },
                .tag => |tag| {
                    try tag_branches.append(self.allocator, .{
                        .value = tag.discriminant,
                        .block = try self.lowerPatternBranchBlock(env, cond.?, branch.pat, branch.body, result_ty, runtime_replacement),
                    });
                },
            }
        }

        std.mem.sort(ast.Branch, tag_branches.items, {}, struct {
            fn lessThan(_: void, left: ast.Branch, right: ast.Branch) bool {
                return left.value < right.value;
            }
        }.lessThan);

        const default_final = default_block orelse runtime_replacement orelse try self.output.addBlock(.{
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

    fn lowerSequentialPatternBranches(
        self: *Lowerer,
        env: []const EnvEntry,
        source_var: ast.Var,
        branches: []const lambdamono.Ast.BranchId,
        runtime_replacement: ?ast.BlockId,
    ) std.mem.Allocator.Error!ast.BlockId {
        var chained_block: ?ast.BlockId = runtime_replacement;

        var index = branches.len;
        while (index > 0) {
            index -= 1;
            const branch = self.input.store.getBranch(branches[index]);
            chained_block = try self.lowerPatternBranchBlock(env, source_var, branch.pat, branch.body, null, chained_block);
        }

        return chained_block orelse debugPanic("ir.lower invariant violated: empty sequential when branches");
    }

    fn appendStoredBlock(
        self: *Lowerer,
        dst: *LoweredBlock,
        block_id: ast.BlockId,
    ) std.mem.Allocator.Error!void {
        const src = self.output.getBlock(block_id);
        for (self.output.sliceStmtSpan(src.stmts)) |stmt_id| {
            try dst.stmts.append(self.allocator, self.output.getStmt(stmt_id));
        }
        dst.setTerm(src.term);
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

        const then_block = try self.lowerBlockExpecting(env, then_expr, result_ty, "if_join");
        const else_block = try self.lowerBlockExpecting(env, else_expr, result_ty, "if_join");
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

    fn lowerBlockExprReplacingRuntimeError(
        self: *Lowerer,
        block: *LoweredBlock,
        env: []const EnvEntry,
        stmts_span: lambdamono.Ast.Span(lambdamono.Ast.StmtId),
        final_expr: lambdamono.Ast.ExprId,
        runtime_replacement: ?ast.BlockId,
    ) std.mem.Allocator.Error!void {
        var current_env = try self.allocator.dupe(EnvEntry, env);
        defer self.allocator.free(current_env);

        for (self.input.store.sliceStmtSpan(stmts_span)) |stmt_id| {
            const keep_going = try self.lowerStmt(block, &current_env, stmt_id);
            if (!keep_going) return;
        }

        var tail = try self.lowerExprReplacingRuntimeError(current_env, final_expr, runtime_replacement);
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

        const iterable_ty = self.input.store.getExpr(iterable_expr).ty;
        const iterable_elem_ty = self.listTypeElem(iterable_ty) orelse
            debugPanic("ir.lower lowerForLoop expected explicit list iterable type");
        const iterable_storage_elem_ty = self.storageTypeForListElement(iterable_elem_ty);
        const body_elem_ty = self.input.store.getPat(patt).ty;
        const elem = try self.freshVarWithLayout(self.input.layouts.patLayout(patt), "for_elem");
        const body_block = try self.lowerPatternBranchBlock(env, elem, patt, body_expr, null, null);
        try block.stmts.append(self.allocator, .{ .for_list = .{
            .elem = elem,
            .iterable = iterable.?,
            .body = body_block,
            .elem_bridge_plan = try self.bridgePlanForTypes(iterable_storage_elem_ty, body_elem_ty),
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
        branch_result_ty: ?lambdamono.Type.TypeId,
        runtime_replacement: ?ast.BlockId,
    ) std.mem.Allocator.Error!ast.BlockId {
        var prefix = std.ArrayList(ast.Stmt).empty;
        defer prefix.deinit(self.allocator);
        var additions = std.ArrayList(EnvEntry).empty;
        defer additions.deinit(self.allocator);

        try self.destructurePattern(&prefix, &additions, source_var, pat_id);

        const extended_env = try self.concatEnv(env, additions.items);
        defer self.allocator.free(extended_env);

        var body = if (runtime_replacement != null)
            try self.lowerExprReplacingRuntimeError(extended_env, body_expr, runtime_replacement)
        else
            try self.lowerExpr(extended_env, body_expr);
        defer body.deinit(self.allocator);
        if (branch_result_ty) |target_ty| {
            try self.bridgeBlockTermToType(&body, self.input.store.getExpr(body_expr).ty, target_ty, "when_join");
        }

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
                const payload_layout = self.input.layouts.patTagPayloadLayout(pat_id);
                const payload_var = try self.emitUnionPayloadValue(
                    prefix,
                    source_var,
                    tag.discriminant,
                    payload_layout,
                    "pat_payload",
                );

                for (args, 0..) |arg_pat_id, i| {
                    const arg_pat = self.input.store.getPat(arg_pat_id);
                    const source_field_ty = self.tagUnionArgs(pat.ty, tag.discriminant) orelse
                        debugPanic("ir.lower destructurePattern missing source payload types");
                    if (i >= source_field_ty.len) {
                        debugPanic("ir.lower destructurePattern payload index out of bounds");
                    }
                    const actual_field_var = try self.freshVar(source_field_ty[i], "pat_field");
                    try prefix.append(self.allocator, .{ .let_ = .{
                        .bind = actual_field_var,
                        .expr = try self.output.addExpr(.{ .get_struct_field = .{
                            .record = payload_var,
                            .field_index = @intCast(i),
                            .field_bridge_plan = try self.aggregateFieldBridgePlan(
                                payload_var.layout,
                                @intCast(i),
                                source_field_ty[i],
                            ),
                        } }),
                    } });
                    const expected_layout = self.input.layouts.layoutForType(arg_pat.ty);
                    const field_var = if (std.meta.eql(actual_field_var.layout, expected_layout))
                        actual_field_var
                    else blk: {
                        const bridged = try self.freshVarWithLayout(expected_layout, "pat_field");
                        try prefix.append(self.allocator, .{ .let_ = .{
                            .bind = bridged,
                            .expr = try self.makeBridgeExpr(actual_field_var, source_field_ty[i], arg_pat.ty),
                        } });
                        break :blk bridged;
                    };
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

fn tagNamesEqual(left: type_mod.TagName, right: type_mod.TagName) bool {
    return switch (left) {
        .ctor => |left_ctor| switch (right) {
            .ctor => |right_ctor| left_ctor == right_ctor,
            .lambda => false,
        },
        .lambda => |left_lambda| switch (right) {
            .ctor => false,
            .lambda => |right_lambda| left_lambda == right_lambda,
        },
    };
}

test "ir lower tests" {
    std.testing.refAllDecls(@This());
}
