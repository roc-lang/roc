//! Executable MIR to IR lowering boundary.

const std = @import("std");
const base = @import("base");
const layout_mod = @import("layout");
const symbol_mod = @import("symbol");
const mir = @import("mir");

const Ast = @import("ast.zig");
const Layout = @import("layout.zig");

const Allocator = std.mem.Allocator;
const Exec = mir.Executable;

pub const LowerResourceError = Allocator.Error;

pub const Program = struct {
    allocator: Allocator,
    literal_pool: mir.Ids.ProgramLiteralPool,
    symbols: symbol_mod.Store,
    store: Ast.Store,
    layouts: Layout.Graph,
    root_procs: std.ArrayList(Ast.ProcRef),

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .literal_pool = mir.Ids.ProgramLiteralPool.init(allocator),
            .symbols = symbol_mod.Store.init(allocator),
            .store = Ast.Store.init(allocator),
            .layouts = .{},
            .root_procs = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        self.root_procs.deinit(self.allocator);
        self.layouts.deinit(self.allocator);
        self.store.deinit();
        self.symbols.deinit();
        self.literal_pool.deinit();
        self.* = Program.init(self.allocator);
    }
};

pub fn fromExecutable(allocator: Allocator, executable: mir.Executable.Build.Program) LowerResourceError!Program {
    var input = executable;
    errdefer input.deinit();

    var program = Program.init(allocator);
    errdefer program.deinit();
    program.literal_pool = input.literal_pool;
    input.literal_pool = mir.Ids.ProgramLiteralPool.init(allocator);
    program.symbols = input.symbols;
    input.symbols = symbol_mod.Store.init(allocator);

    var lowerer = IrBuilder{
        .allocator = allocator,
        .input = &input,
        .output = &program,
        .value_env = std.AutoHashMap(Exec.Ast.ExecutableValueRef, Ast.Var).init(allocator),
        .expr_map = std.AutoHashMap(Exec.Ast.ExprId, Ast.Var).init(allocator),
    };
    defer lowerer.deinit();
    try lowerer.lowerAllDefs();

    try program.root_procs.appendSlice(allocator, input.root_procs.items);

    input.deinit();
    return program;
}

const IrBuilder = struct {
    allocator: Allocator,
    input: *const Exec.Build.Program,
    output: *Program,
    value_env: std.AutoHashMap(Exec.Ast.ExecutableValueRef, Ast.Var),
    expr_map: std.AutoHashMap(Exec.Ast.ExprId, Ast.Var),

    fn deinit(self: *IrBuilder) void {
        self.expr_map.deinit();
        self.value_env.deinit();
    }

    fn lowerAllDefs(self: *IrBuilder) LowerResourceError!void {
        for (self.input.ast.defs.items) |def| {
            self.value_env.clearRetainingCapacity();
            self.expr_map.clearRetainingCapacity();
            try self.lowerDef(def);
        }
    }

    fn lowerDef(self: *IrBuilder, def: Exec.Ast.Def) LowerResourceError!void {
        const args = try self.lowerArgSpan(def.value.args);
        const body = try self.lowerExprToBlock(def.value.body);
        const ret_layout = self.blockReturnLayout(body);
        _ = try self.output.store.addDef(.{
            .proc = def.proc,
            .debug_name = null,
            .args = args,
            .body = body,
            .ret_layout = ret_layout,
            .hosted = null,
        });
    }

    fn lowerArgSpan(self: *IrBuilder, span: Exec.Ast.Span(Exec.Ast.TypedValue)) LowerResourceError!Ast.Span(Ast.Var) {
        if (span.len == 0) return Ast.Span(Ast.Var).empty();
        const input_items = self.input.ast.typed_values.items[span.start..][0..span.len];
        const args = try self.allocator.alloc(Ast.Var, input_items.len);
        defer self.allocator.free(args);
        for (input_items, 0..) |arg, i| {
            const var_ = try self.freshVar(try self.layoutForType(arg.ty));
            try self.value_env.put(arg.value, var_);
            args[i] = var_;
        }
        return try self.output.store.addVarSpan(args);
    }

    fn lowerExprToBlock(self: *IrBuilder, expr_id: Exec.Ast.ExprId) LowerResourceError!Ast.BlockId {
        var stmts = std.ArrayList(Ast.StmtId).empty;
        defer stmts.deinit(self.allocator);

        const expr = self.input.ast.getExpr(expr_id);
        const term: Ast.Term = switch (expr.data) {
            .crash => |literal| .{ .crash = literal },
            .runtime_error => .runtime_error,
            .return_ => |child| blk: {
                const value = try self.lowerExpr(child, &stmts);
                break :blk .{ .return_ = value };
            },
            else => blk: {
                const value = try self.lowerExpr(expr_id, &stmts);
                break :blk .{ .value = value };
            },
        };

        return try self.output.store.addBlock(.{
            .stmts = try self.output.store.addStmtSpan(stmts.items),
            .term = term,
        });
    }

    fn lowerExpr(
        self: *IrBuilder,
        expr_id: Exec.Ast.ExprId,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        if (self.expr_map.get(expr_id)) |existing| return existing;

        const expr = self.input.ast.getExpr(expr_id);
        const lowered: Ast.Var = switch (expr.data) {
            .value_ref => |value| self.value_env.get(value) orelse irInvariant("IR lowering reached executable value_ref before value was bound"),
            .int_lit => |literal| try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .lit = .{ .int = literal } }, stmts),
            .frac_f32_lit => |literal| try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .lit = .{ .f32 = literal } }, stmts),
            .frac_f64_lit => |literal| try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .lit = .{ .f64 = literal } }, stmts),
            .dec_lit => |literal| try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .lit = .{ .dec = literal } }, stmts),
            .str_lit => |literal| try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .lit = .{ .str = literal } }, stmts),
            .bool_lit => |literal| try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .lit = .{ .bool = literal } }, stmts),
            .unit => try self.bindExpr(expr.value, .{ .canonical = .zst }, .{ .make_struct = Ast.Span(Ast.Var).empty() }, stmts),
            .record => |record| blk: {
                const fields = try self.lowerRecordFields(record.fields, stmts);
                defer if (fields.len > 0) self.allocator.free(fields);
                const layout = try self.layoutForType(expr.ty);
                break :blk try self.bindExpr(expr.value, layout, .{ .make_struct = try self.output.store.addVarSpan(fields) }, stmts);
            },
            .nominal_reinterpret => |backing| blk: {
                const lowered_backing = try self.lowerExpr(backing, stmts);
                break :blk try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{
                    .nominal_reinterpret = lowered_backing,
                }, stmts);
            },
            .tag => |tag| blk: {
                const payload = try self.lowerTagPayloadForConstruction(tag.tag, tag.payloads, stmts);
                break :blk try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .make_union = .{
                    .discriminant = @intCast(self.input.row_shapes.tag(tag.tag).logical_index),
                    .payload = payload,
                } }, stmts);
            },
            .access => |access| blk: {
                const record = try self.lowerExpr(access.record, stmts);
                const field = self.input.row_shapes.recordField(access.field);
                const direct = try self.output.store.addBridgePlan(.direct);
                break :blk try self.bindExpr(
                    expr.value,
                    try self.layoutForType(expr.ty),
                    .{ .get_struct_field = .{
                        .record = record,
                        .field_index = @intCast(field.logical_index),
                        .field_bridge_plan = direct,
                    } },
                    stmts,
                );
            },
            .block => |block| blk: {
                try self.lowerStmtSpan(block.stmts, stmts);
                break :blk try self.lowerExpr(block.final_expr, stmts);
            },
            .tuple => |items| blk: {
                const values = try self.lowerVarSpanFromExprSpan(items, stmts);
                defer if (values.len > 0) self.allocator.free(values);
                const layout = try self.structLayout(values);
                break :blk try self.bindExpr(expr.value, layout, .{ .make_struct = try self.output.store.addVarSpan(values) }, stmts);
            },
            .list => |items| blk: {
                const values = try self.lowerVarSpanFromExprSpan(items, stmts);
                defer if (values.len > 0) self.allocator.free(values);
                break :blk try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{
                    .make_list = .{
                        .elems = try self.output.store.addVarSpan(values),
                        .elem_bridge_plans = Ast.Span(Ast.BridgePlanId).empty(),
                    },
                }, stmts);
            },
            .tuple_access => |access| blk: {
                const tuple = try self.lowerExpr(access.tuple, stmts);
                const direct = try self.output.store.addBridgePlan(.direct);
                break :blk try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .get_struct_field = .{
                    .record = tuple,
                    .field_index = @intCast(access.elem_index),
                    .field_bridge_plan = direct,
                } }, stmts);
            },
            .tag_payload => |payload| try self.lowerTagPayload(expr, payload, stmts),
            .low_level => |low_level| blk: {
                const args = try self.lowerVarSpanFromExprSpan(low_level.args, stmts);
                defer if (args.len > 0) self.allocator.free(args);
                break :blk try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .call_low_level = .{
                    .op = low_level.op,
                    .args = try self.output.store.addVarSpan(args),
                } }, stmts);
            },
            .bool_not => |child| blk: {
                const arg = try self.lowerExpr(child, stmts);
                const args = [_]Ast.Var{arg};
                break :blk try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .call_low_level = .{
                    .op = .bool_not,
                    .args = try self.output.store.addVarSpan(&args),
                } }, stmts);
            },
            .return_ => |child| try self.lowerExpr(child, stmts),
            .if_ => |if_| try self.lowerIfExpr(expr, if_, stmts),
            .call_direct => |call| try self.lowerCallDirect(expr, call, stmts),
            .callable_set_value => |callable| try self.lowerCallableSetValue(expr, callable, stmts),
            .callable_match => |callable_match| try self.lowerCallableMatch(expr, callable_match, stmts),
            .tag,
            .const_ref,
            .bridge,
            .call_erased,
            .packed_erased_fn,
            .source_match,
            .for_,
            .crash,
            .runtime_error,
            => irInvariant("IR lowering reached executable expression form whose IR lowering is still missing"),
        };

        try self.expr_map.put(expr_id, lowered);
        try self.value_env.put(expr.value, lowered);
        return lowered;
    }

    fn lowerIfExpr(
        self: *IrBuilder,
        expr: Exec.Ast.Expr,
        if_: anytype,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const cond = try self.lowerExpr(if_.cond, stmts);
        const then_block = try self.lowerExprToBlock(if_.then_body);
        const else_block = try self.lowerExprToBlock(if_.else_body);
        const result = try self.freshVar(try self.layoutForType(expr.ty));
        const branches = [_]Ast.Branch{.{
            .value = 1,
            .block = then_block,
        }};
        try stmts.append(self.allocator, try self.output.store.addStmt(.{ .switch_ = .{
            .cond = cond,
            .branches = try self.output.store.addBranchSpan(&branches),
            .default_block = else_block,
            .join = result,
        } }));
        try self.value_env.put(expr.value, result);
        return result;
    }

    fn lowerCallDirect(
        self: *IrBuilder,
        expr: Exec.Ast.Expr,
        call: Exec.Ast.CallDirectPlan,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const args = try self.lowerDirectCallArgSpan(call.direct_args);
        defer if (args.len > 0) self.allocator.free(args);
        return try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .call_direct = .{
            .proc = call.executable_proc,
            .args = try self.output.store.addVarSpan(args),
        } }, stmts);
    }

    fn lowerCallableSetValue(
        self: *IrBuilder,
        expr: Exec.Ast.Expr,
        callable: Exec.Ast.CallableSetValue,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        if (callable.capture_record != null) {
            irInvariant("IR lowering captured callable_set_value requires closure layout lowering");
        }
        return try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .fn_ptr = callable.selected_executable_proc }, stmts);
    }

    fn lowerCallableMatch(
        self: *IrBuilder,
        expr: Exec.Ast.Expr,
        callable_match: anytype,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        _ = self.value_env.get(callable_match.callee) orelse irInvariant("IR lowering callable_match callee was not bound");
        const branch_ids = self.input.ast.callable_match_branches.items[callable_match.branches.start..][0..callable_match.branches.len];
        if (branch_ids.len != 1) irInvariant("IR lowering multi-branch callable_match requires callable payload dispatch lowering");
        const branch = branch_ids[0];
        const args = try self.lowerDirectCallArgSpan(branch.direct_args);
        defer if (args.len > 0) self.allocator.free(args);
        return try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .call_direct = .{
            .proc = branch.executable_proc,
            .args = try self.output.store.addVarSpan(args),
        } }, stmts);
    }

    fn lowerStmtSpan(
        self: *IrBuilder,
        span: Exec.Ast.Span(Exec.Ast.StmtId),
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!void {
        if (span.len == 0) return;
        const input_items = self.input.ast.stmt_ids.items[span.start..][0..span.len];
        for (input_items) |stmt| {
            try self.lowerStmt(stmt, stmts);
        }
    }

    fn lowerStmt(
        self: *IrBuilder,
        stmt_id: Exec.Ast.StmtId,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!void {
        const stmt = self.input.ast.stmts.items[@intFromEnum(stmt_id)];
        switch (stmt) {
            .decl => |decl| {
                const value = try self.lowerExpr(decl.body, stmts);
                try self.value_env.put(decl.value, value);
            },
            .reassign => |reassign| {
                const value = try self.lowerExpr(reassign.body, stmts);
                const target = self.value_env.get(reassign.target) orelse irInvariant("IR lowering reached reassign target before it was bound");
                try stmts.append(self.allocator, try self.output.store.addStmt(.{ .set = .{
                    .target = target,
                    .value = value,
                } }));
            },
            .expr => |expr| _ = try self.lowerExpr(expr, stmts),
            .debug => |expr| {
                const value = try self.lowerExpr(expr, stmts);
                try stmts.append(self.allocator, try self.output.store.addStmt(.{ .debug = value }));
            },
            .expect => |expr| {
                const value = try self.lowerExpr(expr, stmts);
                try stmts.append(self.allocator, try self.output.store.addStmt(.{ .expect = value }));
            },
            .crash,
            .return_,
            .break_,
            .for_,
            .while_,
            => irInvariant("IR lowering reached statement control flow that needs block splitting"),
        }
    }


    fn lowerRecordFields(
        self: *IrBuilder,
        span: Exec.Ast.Span(Exec.Ast.RecordFieldExpr),
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError![]const Ast.Var {
        if (span.len == 0) return &.{};
        const input_items = self.input.ast.record_field_exprs.items[span.start..][0..span.len];
        const values = try self.allocator.alloc(Ast.Var, input_items.len);
        for (input_items, 0..) |field, i| {
            values[i] = try self.lowerExpr(field.expr, stmts);
        }
        return values;
    }

    fn lowerTagPayloadForConstruction(
        self: *IrBuilder,
        tag_id: mir.MonoRow.TagId,
        span: Exec.Ast.Span(Exec.Ast.TagPayloadExpr),
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!?Ast.Var {
        const expected_payloads = self.input.row_shapes.tagPayloads(tag_id);
        if (expected_payloads.len == 0) return null;

        const payload_vars = try self.allocator.alloc(Ast.Var, expected_payloads.len);
        defer self.allocator.free(payload_vars);
        var seen = try self.allocator.alloc(bool, expected_payloads.len);
        defer self.allocator.free(seen);
        @memset(seen, false);

        const input_items = self.input.ast.tag_payload_exprs.items[span.start..][0..span.len];
        for (input_items) |payload| {
            const payload_info = self.input.row_shapes.tagPayload(payload.payload);
            if (payload_info.tag != tag_id) irInvariant("IR lowering tag construction payload belonged to a different tag");
            const logical_index = payload_info.logical_index;
            if (logical_index >= payload_vars.len) irInvariant("IR lowering tag construction payload index exceeded tag arity");
            if (seen[logical_index]) irInvariant("IR lowering tag construction saw duplicate payload slot");
            payload_vars[logical_index] = try self.lowerExpr(payload.expr, stmts);
            seen[logical_index] = true;
        }
        for (seen) |was_seen| {
            if (!was_seen) irInvariant("IR lowering tag construction did not provide every payload slot");
        }

        if (payload_vars.len == 1) return payload_vars[0];
        const payload_layout = try self.structLayout(payload_vars);
        return try self.bindAnonymous(payload_layout, .{
            .make_struct = try self.output.store.addVarSpan(payload_vars),
        }, stmts);
    }

    fn lowerTagPayload(
        self: *IrBuilder,
        expr: Exec.Ast.Expr,
        payload: anytype,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const tag_union = try self.lowerExpr(payload.tag_union, stmts);
        const payload_info = self.input.row_shapes.tagPayload(payload.payload);
        const tag_info = self.input.row_shapes.tag(payload_info.tag);
        const tag_payloads = self.input.row_shapes.tagPayloads(payload_info.tag);
        if (tag_payloads.len == 0) irInvariant("IR lowering tag payload projection targeted a nullary tag");

        if (tag_payloads.len == 1) {
            return try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .get_union_struct = .{
                .value = tag_union,
                .tag_discriminant = @intCast(tag_info.logical_index),
            } }, stmts);
        }

        const tag_union_expr = self.input.ast.getExpr(payload.tag_union);
        const payload_struct_layout = try self.payloadStructLayoutForTag(tag_union_expr.ty, payload_info.tag);
        const payload_struct = try self.bindAnonymous(payload_struct_layout, .{ .get_union_struct = .{
            .value = tag_union,
            .tag_discriminant = @intCast(tag_info.logical_index),
        } }, stmts);
        const direct = try self.output.store.addBridgePlan(.direct);
        return try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .get_struct_field = .{
            .record = payload_struct,
            .field_index = @intCast(payload_info.logical_index),
            .field_bridge_plan = direct,
        } }, stmts);
    }

    fn lowerVarSpanFromExprSpan(
        self: *IrBuilder,
        span: Exec.Ast.Span(Exec.Ast.ExprId),
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError![]const Ast.Var {
        if (span.len == 0) return &.{};
        const input_items = self.input.ast.expr_ids.items[span.start..][0..span.len];
        const values = try self.allocator.alloc(Ast.Var, input_items.len);
        for (input_items, 0..) |expr, i| {
            values[i] = try self.lowerExpr(expr, stmts);
        }
        return values;
    }

    fn lowerDirectCallArgSpan(
        self: *IrBuilder,
        span: Exec.Ast.Span(Exec.Ast.DirectCallArg),
    ) LowerResourceError![]const Ast.Var {
        if (span.len == 0) return &.{};
        const input_items = self.input.ast.direct_call_args.items[span.start..][0..span.len];
        const values = try self.allocator.alloc(Ast.Var, input_items.len);
        for (input_items, 0..) |arg, i| {
            if (arg.bridge != null) irInvariant("IR lowering direct call argument bridges are not implemented yet");
            values[i] = self.value_env.get(arg.value) orelse irInvariant("IR lowering direct call argument value was not bound");
        }
        return values;
    }

    fn bindExpr(
        self: *IrBuilder,
        value_ref: Exec.Ast.ExecutableValueRef,
        layout: Ast.LayoutRef,
        expr: Ast.Expr,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const bind = try self.freshVar(layout);
        const expr_id = try self.output.store.addExpr(expr);
        try stmts.append(self.allocator, try self.output.store.addStmt(.{ .let_ = .{
            .bind = bind,
            .expr = expr_id,
        } }));
        try self.value_env.put(value_ref, bind);
        return bind;
    }

    fn bindAnonymous(
        self: *IrBuilder,
        layout: Ast.LayoutRef,
        expr: Ast.Expr,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const bind = try self.freshVar(layout);
        const expr_id = try self.output.store.addExpr(expr);
        try stmts.append(self.allocator, try self.output.store.addStmt(.{ .let_ = .{
            .bind = bind,
            .expr = expr_id,
        } }));
        return bind;
    }

    fn freshVar(self: *IrBuilder, layout: Ast.LayoutRef) LowerResourceError!Ast.Var {
        return .{
            .layout = layout,
            .symbol = try self.output.symbols.add(base.Ident.Idx.NONE, .synthetic),
        };
    }

    fn blockReturnLayout(self: *const IrBuilder, block_id: Ast.BlockId) Ast.LayoutRef {
        const block = self.output.store.getBlock(block_id);
        return switch (block.term) {
            .value => |value| value.layout,
            .return_ => |value| value.layout,
            .crash, .runtime_error, .@"unreachable" => .{ .canonical = .zst },
        };
    }

    fn structLayout(self: *IrBuilder, fields: []const Ast.Var) LowerResourceError!Ast.LayoutRef {
        if (fields.len == 0) return .{ .canonical = .zst };
        const graph_fields = try self.allocator.alloc(Layout.Field, fields.len);
        defer self.allocator.free(graph_fields);
        for (fields, 0..) |field, i| {
            graph_fields[i] = .{
                .index = @intCast(i),
                .child = field.layout,
            };
        }
        const node = try self.output.layouts.reserveNode(self.allocator);
        self.output.layouts.setNode(node, .{ .struct_ = try self.output.layouts.appendFields(self.allocator, graph_fields) });
        return .{ .local = node };
    }

    fn structLayoutFromTypes(self: *IrBuilder, types: []const Exec.Type.TypeId) LowerResourceError!Ast.LayoutRef {
        if (types.len == 0) return .{ .canonical = .zst };
        const vars = try self.allocator.alloc(Ast.Var, types.len);
        defer self.allocator.free(vars);
        for (types, 0..) |ty, i| {
            vars[i] = .{
                .layout = try self.layoutForType(ty),
                .symbol = symbol_mod.Symbol.none,
            };
        }
        return try self.structLayout(vars);
    }

    fn payloadLayout(self: *IrBuilder, payloads: []const Exec.Type.TagPayloadType) LowerResourceError!Ast.LayoutRef {
        if (payloads.len == 0) return .{ .canonical = .zst };
        if (payloads.len == 1) return try self.layoutForType(payloads[0].ty);

        const payload_types = try self.allocator.alloc(Exec.Type.TypeId, payloads.len);
        defer self.allocator.free(payload_types);
        var seen = try self.allocator.alloc(bool, payloads.len);
        defer self.allocator.free(seen);
        @memset(seen, false);
        for (payloads) |payload| {
            const payload_info = self.input.row_shapes.tagPayload(payload.payload);
            if (payload_info.logical_index >= payloads.len) irInvariant("IR lowering payload type logical index exceeded tag arity");
            if (seen[payload_info.logical_index]) irInvariant("IR lowering payload type saw duplicate payload logical index");
            payload_types[payload_info.logical_index] = payload.ty;
            seen[payload_info.logical_index] = true;
        }
        for (seen) |was_seen| {
            if (!was_seen) irInvariant("IR lowering payload type did not provide every payload slot");
        }
        return try self.structLayoutFromTypes(payload_types);
    }

    fn payloadStructLayoutForTag(
        self: *IrBuilder,
        tag_union_ty: Exec.Type.TypeId,
        tag_id: mir.MonoRow.TagId,
    ) LowerResourceError!Ast.LayoutRef {
        return switch (self.input.types.getType(tag_union_ty)) {
            .link => |next| try self.payloadStructLayoutForTag(next, tag_id),
            .tag_union => |tag_union| blk: {
                for (tag_union.tags) |tag| {
                    if (tag.tag == tag_id) break :blk try self.payloadLayout(tag.payloads);
                }
                irInvariant("IR lowering tag payload projection did not find payload tag in union type");
            },
            else => irInvariant("IR lowering tag payload projection expected tag-union source type"),
        };
    }

    fn recordLayout(self: *IrBuilder, record: Exec.Type.RecordType) LowerResourceError!Ast.LayoutRef {
        if (record.fields.len == 0) return .{ .canonical = .zst };
        const graph_fields = try self.allocator.alloc(Layout.Field, record.fields.len);
        defer self.allocator.free(graph_fields);
        var seen = try self.allocator.alloc(bool, record.fields.len);
        defer self.allocator.free(seen);
        @memset(seen, false);
        for (record.fields) |field| {
            const field_info = self.input.row_shapes.recordField(field.field);
            if (field_info.logical_index >= record.fields.len) irInvariant("IR lowering record field logical index exceeded record arity");
            if (seen[field_info.logical_index]) irInvariant("IR lowering record type saw duplicate field logical index");
            graph_fields[field_info.logical_index] = .{
                .index = @intCast(field_info.logical_index),
                .child = try self.layoutForType(field.ty),
            };
            seen[field_info.logical_index] = true;
        }
        for (seen) |was_seen| {
            if (!was_seen) irInvariant("IR lowering record type did not provide every field");
        }
        const node = try self.output.layouts.reserveNode(self.allocator);
        self.output.layouts.setNode(node, .{ .struct_ = try self.output.layouts.appendFields(self.allocator, graph_fields) });
        return .{ .local = node };
    }

    fn tagUnionLayout(self: *IrBuilder, tag_union: Exec.Type.TagUnionType) LowerResourceError!Ast.LayoutRef {
        if (tag_union.tags.len == 0) return .{ .canonical = .zst };
        const variants = try self.allocator.alloc(Ast.LayoutRef, tag_union.tags.len);
        defer self.allocator.free(variants);
        var seen = try self.allocator.alloc(bool, tag_union.tags.len);
        defer self.allocator.free(seen);
        @memset(seen, false);

        for (tag_union.tags) |tag| {
            const tag_info = self.input.row_shapes.tag(tag.tag);
            if (tag_info.logical_index >= tag_union.tags.len) irInvariant("IR lowering tag logical index exceeded tag-union arity");
            if (seen[tag_info.logical_index]) irInvariant("IR lowering tag-union type saw duplicate tag logical index");
            variants[tag_info.logical_index] = try self.payloadLayout(tag.payloads);
            seen[tag_info.logical_index] = true;
        }
        for (seen) |was_seen| {
            if (!was_seen) irInvariant("IR lowering tag-union type did not provide every tag variant");
        }

        const node = try self.output.layouts.reserveNode(self.allocator);
        self.output.layouts.setNode(node, .{ .tag_union = try self.output.layouts.appendRefs(self.allocator, variants) });
        return .{ .local = node };
    }

    fn layoutForType(self: *IrBuilder, ty: Exec.Type.TypeId) LowerResourceError!Ast.LayoutRef {
        return switch (self.input.types.getType(ty)) {
            .placeholder => irInvariant("IR lowering received executable placeholder type"),
            .link => |next| try self.layoutForType(next),
            .primitive => |prim| .{ .canonical = primitiveLayout(prim) },
            .nominal => |nominal| try self.layoutForType(nominal.backing),
            .list => |elem| blk: {
                const child = try self.layoutForType(elem);
                const node = try self.output.layouts.reserveNode(self.allocator);
                self.output.layouts.setNode(node, .{ .list = child });
                break :blk .{ .local = node };
            },
            .box => |elem| blk: {
                const child = try self.layoutForType(elem);
                const node = try self.output.layouts.reserveNode(self.allocator);
                self.output.layouts.setNode(node, .{ .box = child });
                break :blk .{ .local = node };
            },
            .tuple => |items| blk: {
                const vars = try self.allocator.alloc(Ast.Var, items.len);
                defer self.allocator.free(vars);
                for (items, 0..) |item, i| {
                    vars[i] = .{
                        .layout = try self.layoutForType(item),
                        .symbol = symbol_mod.Symbol.none,
                    };
                }
                break :blk try self.structLayout(vars);
            },
            .record => |record| try self.recordLayout(record),
            .tag_union => |tag_union| try self.tagUnionLayout(tag_union),
            .callable_set,
            .erased_fn,
            => .{ .canonical = .opaque_ptr },
        };
    }
};

fn primitiveLayout(prim: Exec.Type.Prim) layout_mod.Idx {
    return switch (prim) {
        .bool => .bool,
        .str => .str,
        .u8 => .u8,
        .i8 => .i8,
        .u16 => .u16,
        .i16 => .i16,
        .u32 => .u32,
        .i32 => .i32,
        .u64 => .u64,
        .i64 => .i64,
        .u128 => .u128,
        .i128 => .i128,
        .f32 => .f32,
        .f64 => .f64,
        .dec => .dec,
        .erased => .opaque_ptr,
    };
}

fn irInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) std.debug.panic(message, .{});
    unreachable;
}

test "IR lowering consumes executable MIR only" {
    std.testing.refAllDecls(@This());
}
