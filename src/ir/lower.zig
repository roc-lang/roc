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
const repr = mir.LambdaSolved.Representation;

/// Public `LowerResourceError` declaration.
pub const LowerResourceError = Allocator.Error;

/// Public `Program` declaration.
pub const Program = struct {
    allocator: Allocator,
    canonical_names: mir.Hosted.CanonicalNameStore,
    literal_pool: mir.Ids.ProgramLiteralPool,
    symbols: symbol_mod.Store,
    store: Ast.Store,
    layouts: Layout.Graph,
    root_procs: std.ArrayList(Ast.ProcRef),
    root_metadata: std.ArrayList(mir.Ids.RootMetadata),
    requested_layouts: std.ArrayList(RequestedLayout),

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .canonical_names = mir.Hosted.CanonicalNameStore.init(allocator),
            .literal_pool = mir.Ids.ProgramLiteralPool.init(allocator),
            .symbols = symbol_mod.Store.init(allocator),
            .store = Ast.Store.init(allocator),
            .layouts = .{},
            .root_procs = .empty,
            .root_metadata = .empty,
            .requested_layouts = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        self.requested_layouts.deinit(self.allocator);
        self.root_metadata.deinit(self.allocator);
        self.root_procs.deinit(self.allocator);
        self.layouts.deinit(self.allocator);
        self.store.deinit();
        self.symbols.deinit();
        self.literal_pool.deinit();
        self.canonical_names.deinit();
        self.* = Program.init(self.allocator);
    }
};

/// Public `RequestedLayout` declaration.
pub const RequestedLayout = struct {
    key: repr.CanonicalExecValueTypeKey,
    layout: Ast.LayoutRef,
};

/// Public `fromExecutable` function.
pub fn fromExecutable(
    allocator: Allocator,
    executable: mir.Executable.Build.Program,
    layout_request_keys: []const repr.CanonicalExecValueTypeKey,
) LowerResourceError!Program {
    var input = executable;
    errdefer input.deinit();

    var program = Program.init(allocator);
    errdefer program.deinit();
    program.canonical_names = input.canonical_names;
    input.canonical_names = mir.Hosted.CanonicalNameStore.init(allocator);
    program.literal_pool = input.literal_pool;
    input.literal_pool = mir.Ids.ProgramLiteralPool.init(allocator);
    program.symbols = input.symbols;
    input.symbols = symbol_mod.Store.init(allocator);

    var lowerer = IrBuilder{
        .allocator = allocator,
        .input = &input,
        .output = &program,
        .value_env = std.AutoHashMap(Exec.Ast.ExecutableValueRef, Ast.Var).init(allocator),
        .proc_def_index = std.AutoHashMap(Exec.Ast.ExecutableProcId, usize).init(allocator),
        .layout_cache = std.AutoHashMap(Exec.Type.TypeId, Ast.LayoutRef).init(allocator),
        .nominal_layout_cache = std.AutoHashMap([32]u8, Ast.LayoutRef).init(allocator),
        .next_internal_value_ref = input.ast.next_value_ref,
    };
    defer lowerer.deinit();
    try lowerer.lowerAllDefs();
    try lowerer.publishRequestedLayouts(layout_request_keys);

    try program.root_procs.appendSlice(allocator, input.root_procs.items);
    try program.root_metadata.appendSlice(allocator, input.root_metadata.items);

    input.deinit();
    return program;
}

const IrBuilder = struct {
    allocator: Allocator,
    input: *const Exec.Build.Program,
    output: *Program,
    value_env: std.AutoHashMap(Exec.Ast.ExecutableValueRef, Ast.Var),
    proc_def_index: std.AutoHashMap(Exec.Ast.ExecutableProcId, usize),
    layout_cache: std.AutoHashMap(Exec.Type.TypeId, Ast.LayoutRef),
    nominal_layout_cache: std.AutoHashMap([32]u8, Ast.LayoutRef),
    next_internal_value_ref: u32,

    fn deinit(self: *IrBuilder) void {
        self.nominal_layout_cache.deinit();
        self.layout_cache.deinit();
        self.proc_def_index.deinit();
        self.value_env.deinit();
    }

    fn lowerAllDefs(self: *IrBuilder) LowerResourceError!void {
        try self.buildProcDefIndex();
        for (self.input.ast.defs.items) |def| {
            self.value_env.clearRetainingCapacity();
            try self.lowerDef(def);
        }
    }

    fn buildProcDefIndex(self: *IrBuilder) LowerResourceError!void {
        try self.proc_def_index.ensureTotalCapacity(@intCast(self.input.ast.defs.items.len));
        for (self.input.ast.defs.items, 0..) |def, i| {
            self.proc_def_index.putAssumeCapacity(def.proc, i);
        }
    }

    fn lowerDef(self: *IrBuilder, def: Exec.Ast.Def) LowerResourceError!void {
        switch (def.value) {
            .fn_ => |fn_| {
                const args = try self.lowerArgSpan(fn_.args);
                const body = try self.lowerExprToBlock(fn_.body);
                const ret_layout = self.blockReturnLayout(body);
                _ = try self.output.store.addDef(.{
                    .proc = def.proc,
                    .origin = def.origin,
                    .debug_name = null,
                    .args = args,
                    .body = body,
                    .ret_layout = ret_layout,
                    .hosted = null,
                });
            },
            .hosted_fn => |hosted| {
                const args = try self.lowerArgSpan(hosted.args);
                _ = try self.output.store.addDef(.{
                    .proc = def.proc,
                    .origin = def.origin,
                    .debug_name = null,
                    .args = args,
                    .body = null,
                    .ret_layout = try self.layoutForType(hosted.ret_ty),
                    .hosted = hosted.hosted,
                });
            },
        }
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

        const term = try self.lowerExprToTerm(expr_id, &stmts);

        return try self.output.store.addBlock(.{
            .stmts = try self.output.store.addStmtSpan(stmts.items),
            .term = term,
        });
    }

    fn lowerPredicateToBlock(self: *IrBuilder, condition: Exec.Ast.BoolCondition) LowerResourceError!Ast.BlockId {
        var stmts = std.ArrayList(Ast.StmtId).empty;
        defer stmts.deinit(self.allocator);

        const predicate = try self.lowerExprAsPredicate(condition.expr, condition.true_discriminant, &stmts);

        return try self.output.store.addBlock(.{
            .stmts = try self.output.store.addStmtSpan(stmts.items),
            .term = .{ .value = predicate },
        });
    }

    fn lowerExprToTerm(
        self: *IrBuilder,
        expr_id: Exec.Ast.ExprId,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Term {
        const expr = self.input.ast.getExpr(expr_id);
        return switch (expr.data) {
            .crash => |literal| .{ .crash = literal },
            .runtime_error => .runtime_error,
            .@"unreachable" => .@"unreachable",
            .return_ => |child| blk: {
                const value = try self.lowerExpr(child, stmts);
                break :blk .{ .return_ = value };
            },
            .block => |block| blk: {
                try self.lowerStmtSpan(block.stmts, stmts);
                break :blk try self.lowerExprToTerm(block.final_expr, stmts);
            },
            else => blk: {
                const value = try self.lowerExpr(expr_id, stmts);
                break :blk .{ .value = value };
            },
        };
    }

    fn lowerExpr(
        self: *IrBuilder,
        expr_id: Exec.Ast.ExprId,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const expr = self.input.ast.getExpr(expr_id);
        const lowered: Ast.Var = switch (expr.data) {
            .value_ref => |value| blk: {
                const existing = self.value_env.get(value) orelse irInvariant("IR lowering reached executable value_ref before value was bound");
                break :blk existing;
            },
            .int_lit => |literal| try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .lit = .{ .int = literal } }, stmts),
            .frac_f32_lit => |literal| try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .lit = .{ .f32 = literal } }, stmts),
            .frac_f64_lit => |literal| try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .lit = .{ .f64 = literal } }, stmts),
            .dec_lit => |literal| try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .lit = .{ .dec = literal } }, stmts),
            .str_lit => |literal| try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .lit = .{ .str = literal } }, stmts),
            .unit => try self.bindExpr(expr.value, .{ .canonical = .zst }, try self.makeDirectStructExpr(&.{}), stmts),
            .record => |record| blk: {
                const fields = try self.lowerRecordFields(record.fields, stmts);
                defer fields.deinit(self.allocator);
                const layout = try self.layoutForType(expr.ty);
                break :blk try self.bindExpr(expr.value, layout, try self.makeStructExpr(fields.vars, fields.bridges), stmts);
            },
            .nominal_reinterpret => |backing| blk: {
                const lowered_backing = try self.lowerExpr(backing, stmts);
                break :blk try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{
                    .nominal_reinterpret = lowered_backing,
                }, stmts);
            },
            .tag => |tag| blk: {
                const payload = try self.lowerTagPayloadForConstruction(expr.ty, tag.tag, tag.payloads, stmts);
                break :blk try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .make_union = .{
                    .discriminant = @intCast(self.input.row_shapes.tag(tag.tag).logical_index),
                    .payload = payload.var_,
                    .payload_bridge_plan = payload.bridge,
                } }, stmts);
            },
            .access => |access| blk: {
                const record = try self.lowerExpr(access.record, stmts);
                const field = self.input.row_shapes.recordField(access.field);
                break :blk try self.bindExpr(
                    expr.value,
                    try self.layoutForType(expr.ty),
                    .{ .get_struct_field = .{
                        .record = record,
                        .field_index = @intCast(field.logical_index),
                        .field_bridge_plan = try self.constructionSlotBridge(expr.ty, expr.ty),
                    } },
                    stmts,
                );
            },
            .block => |block| blk: {
                try self.lowerStmtSpan(block.stmts, stmts);
                break :blk try self.lowerExpr(block.final_expr, stmts);
            },
            .tuple => |items| blk: {
                const values = try self.lowerTupleItems(items, stmts);
                defer values.deinit(self.allocator);
                const layout = try self.structLayout(values.vars);
                break :blk try self.bindExpr(expr.value, layout, try self.makeStructExpr(values.vars, values.bridges), stmts);
            },
            .list => |items| blk: {
                const values = try self.lowerListItems(items, stmts);
                defer values.deinit(self.allocator);
                break :blk try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{
                    .make_list = .{
                        .elems = try self.output.store.addVarSpan(values.vars),
                        .elem_bridge_plans = try self.output.store.addBridgePlanSpan(values.bridges),
                    },
                }, stmts);
            },
            .tuple_access => |access| blk: {
                const tuple = try self.lowerExpr(access.tuple, stmts);
                break :blk try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .get_struct_field = .{
                    .record = tuple,
                    .field_index = @intCast(access.elem_index),
                    .field_bridge_plan = try self.constructionSlotBridge(expr.ty, expr.ty),
                } }, stmts);
            },
            .tag_payload => |payload| try self.lowerTagPayload(expr, payload, stmts),
            .low_level => |low_level| try self.lowerLowLevelExpr(expr, low_level, stmts),
            .return_ => |child| try self.lowerExpr(child, stmts),
            .if_ => |if_| try self.lowerIfExpr(expr, if_, stmts),
            .call_direct => |call| try self.lowerCallDirect(expr, call, stmts),
            .structural_eq => |eq| try self.lowerStructuralEq(expr, eq, stmts),
            .callable_set_value => |callable| try self.lowerCallableSetValue(expr, callable, stmts),
            .callable_match => |callable_match| try self.lowerCallableMatch(expr, callable_match, stmts),
            .source_match => |source_match| try self.lowerSourceMatch(expr, source_match, stmts),
            .value_transform_tag_union => |tag_transform| try self.lowerValueTransformTagUnion(expr, tag_transform, stmts),
            .value_transform_list => |list_transform| try self.lowerValueTransformList(expr, list_transform, stmts),
            .for_ => |for_| try self.lowerForExpr(expr, for_, stmts),
            .bridge => |bridge_expr| blk: {
                const value = self.value_env.get(bridge_expr.value) orelse irInvariant("IR lowering bridge source value was not bound");
                const bridge_plan = try self.lowerBridgePlan(bridge_expr.bridge);
                break :blk try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .bridge = .{
                    .value = value,
                    .plan = bridge_plan,
                } }, stmts);
            },
            .packed_erased_fn => |packed_fn| try self.lowerPackedErasedFn(expr, packed_fn, stmts),
            .call_erased => |call| try self.lowerCallErased(expr, call, stmts),
            .crash => |literal| blk: {
                try stmts.append(self.allocator, try self.output.store.addStmt(.{ .crash = literal }));
                break :blk try self.freshVar(try self.layoutForType(expr.ty));
            },
            .runtime_error => blk: {
                try stmts.append(self.allocator, try self.output.store.addStmt(.runtime_error));
                break :blk try self.freshVar(try self.layoutForType(expr.ty));
            },
            .@"unreachable" => blk: {
                try stmts.append(self.allocator, try self.output.store.addStmt(.runtime_error));
                break :blk try self.freshVar(try self.layoutForType(expr.ty));
            },
            .const_instance => irInvariant("IR lowering received executable const_instance; executable MIR must materialize constants before IR"),
            .const_ref => irInvariant("IR lowering received non-runnable compile-time dependency const_ref"),
        };

        try self.value_env.put(expr.value, lowered);
        return lowered;
    }

    fn lowerIfExpr(
        self: *IrBuilder,
        expr: Exec.Ast.Expr,
        if_: anytype,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const cond = try self.lowerExprAsPredicate(if_.cond, if_.true_discriminant, stmts);
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
        const direct_call: Ast.Expr = .{ .call_direct = .{
            .proc = call.executable_proc,
            .args = try self.output.store.addVarSpan(args),
        } };
        return try self.bindExpr(expr.value, try self.layoutForType(expr.ty), direct_call, stmts);
    }

    fn lowerCallErased(
        self: *IrBuilder,
        expr: Exec.Ast.Expr,
        call: anytype,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const func = self.value_env.get(call.func) orelse irInvariant("IR lowering call_erased function value was not bound");
        const args = try self.lowerVarSpanFromValueRefSpan(call.args);
        defer if (args.len > 0) self.allocator.free(args);
        return try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .call_erased = .{
            .func = func,
            .args = try self.output.store.addVarSpan(args),
        } }, stmts);
    }

    fn lowerPackedErasedFn(
        self: *IrBuilder,
        expr: Exec.Ast.Expr,
        packed_fn: Exec.Ast.PackedErasedFn,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        var capture_layout: ?Ast.LayoutRef = null;
        var capture: ?Ast.Var = null;
        if (packed_fn.capture_ty) |capture_ty| {
            const capture_ref = packed_fn.capture orelse irInvariant("IR lowering packed erased fn has capture type but no capture value");
            capture = self.value_env.get(capture_ref) orelse irInvariant("IR lowering packed erased fn capture value was not bound");
            capture_layout = try self.layoutForType(capture_ty);
        } else if (packed_fn.capture != null) {
            irInvariant("IR lowering packed erased fn has capture value but no capture type");
        }

        return try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .packed_erased_fn = .{
            .proc = packed_fn.code,
            .capture = capture,
            .capture_layout = capture_layout,
        } }, stmts);
    }

    fn lowerCallableSetValue(
        self: *IrBuilder,
        expr: Exec.Ast.Expr,
        callable: Exec.Ast.CallableSetValue,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        if (!repr.callableSetKeyEql(callable.member.callable_set_key, callable.callable_set_key)) {
            irInvariant("IR lowering callable_set_value member points at a different callable set");
        }
        var payload: ?Ast.Var = null;
        if (callable.capture_record) |record| {
            const capture_refs = self.input.ast.capture_value_refs.items[record.values.start..][0..record.values.len];
            const fields = try self.allocator.alloc(Ast.Var, capture_refs.len);
            defer self.allocator.free(fields);
            var seen = try self.allocator.alloc(bool, capture_refs.len);
            defer self.allocator.free(seen);
            @memset(seen, false);

            for (capture_refs) |capture| {
                const slot: usize = @intCast(capture.slot);
                if (slot >= capture_refs.len) irInvariant("IR lowering captured callable slot exceeded capture record arity");
                if (seen[slot]) irInvariant("IR lowering captured callable record saw duplicate capture slot");
                const value = self.value_env.get(capture.value) orelse irInvariant("IR lowering captured callable value was not bound");
                fields[slot] = value;
                seen[slot] = true;
            }
            for (seen) |was_seen| {
                if (!was_seen) irInvariant("IR lowering captured callable record did not provide every capture slot");
            }

            const layout = try self.structLayout(fields);
            const bind = try self.bindExpr(record.record_tmp, layout, try self.makeDirectStructExpr(fields), stmts);
            payload = bind;
        }

        const payload_bridge = if (payload != null) try self.output.store.addBridgePlan(.direct) else null;
        return try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .make_union = .{
            .discriminant = @intCast(@intFromEnum(callable.member.member_index)),
            .payload = payload,
            .payload_bridge_plan = payload_bridge,
        } }, stmts);
    }

    fn lowerStructuralEq(
        self: *IrBuilder,
        expr: Exec.Ast.Expr,
        eq: anytype,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const lhs = try self.lowerExpr(eq.lhs, stmts);
        const rhs = try self.lowerExpr(eq.rhs, stmts);
        const predicate = try self.boolStructuralEq(lhs, rhs, stmts);
        return try self.lowerPredicateResult(expr.value, expr.ty, predicate, eq.result_bool, stmts);
    }

    fn lowerLowLevelExpr(
        self: *IrBuilder,
        expr: Exec.Ast.Expr,
        low_level: anytype,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        if (lowLevelReturnsPredicate(low_level.op)) {
            const result_bool = low_level.predicate_result orelse
                irInvariant("IR lowering predicate low-level operation omitted Bool discriminants");
            const predicate = try self.lowerLowLevelPredicate(low_level, stmts);
            return try self.lowerPredicateResult(expr.value, expr.ty, predicate, result_bool, stmts);
        }

        const args = try self.lowerVarSpanFromExprSpan(low_level.args, stmts);
        defer if (args.len > 0) self.allocator.free(args);
        return try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .call_low_level = .{
            .op = low_level.op,
            .rc_effect = low_level.rc_effect,
            .args = try self.output.store.addVarSpan(args),
        } }, stmts);
    }

    fn lowerLowLevelPredicate(
        self: *IrBuilder,
        low_level: anytype,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const args = try self.lowerVarSpanFromExprSpan(low_level.args, stmts);
        defer if (args.len > 0) self.allocator.free(args);
        return try self.bindAnonymous(.{ .canonical = .bool }, .{ .call_low_level = .{
            .op = low_level.op,
            .rc_effect = low_level.rc_effect,
            .args = try self.output.store.addVarSpan(args),
        } }, stmts);
    }

    fn lowerExprAsPredicate(
        self: *IrBuilder,
        expr_id: Exec.Ast.ExprId,
        true_discriminant: u16,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const expr = self.input.ast.getExpr(expr_id);
        return switch (expr.data) {
            .low_level => |low_level| if (lowLevelReturnsPredicate(low_level.op))
                try self.lowerLowLevelPredicate(low_level, stmts)
            else
                try self.boolPredicateForValue(try self.lowerExpr(expr_id, stmts), expr.ty, true_discriminant, stmts),
            .structural_eq => |eq| blk: {
                const lhs = try self.lowerExpr(eq.lhs, stmts);
                const rhs = try self.lowerExpr(eq.rhs, stmts);
                break :blk try self.boolStructuralEq(lhs, rhs, stmts);
            },
            else => try self.boolPredicateForValue(try self.lowerExpr(expr_id, stmts), expr.ty, true_discriminant, stmts),
        };
    }

    fn lowerPredicateResult(
        self: *IrBuilder,
        value_ref: Exec.Ast.ExecutableValueRef,
        result_ty: Exec.Type.TypeId,
        predicate: Ast.Var,
        result_bool: Exec.Ast.BoolDiscriminants,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const result_layout = try self.layoutForType(result_ty);
        const true_value = try self.bindAnonymous(result_layout, .{ .make_union = .{
            .discriminant = result_bool.true_discriminant,
            .payload = null,
            .payload_bridge_plan = null,
        } }, stmts);
        const false_value = try self.bindAnonymous(result_layout, .{ .make_union = .{
            .discriminant = result_bool.false_discriminant,
            .payload = null,
            .payload_bridge_plan = null,
        } }, stmts);
        const result = try self.freshVar(result_layout);
        const branches = [_]Ast.Branch{.{
            .value = 1,
            .block = try self.valueBlock(true_value),
        }};
        try stmts.append(self.allocator, try self.output.store.addStmt(.{ .switch_ = .{
            .cond = predicate,
            .branches = try self.output.store.addBranchSpan(&branches),
            .default_block = try self.valueBlock(false_value),
            .join = result,
        } }));
        try self.value_env.put(value_ref, result);
        return result;
    }

    fn boolPredicateForValue(
        self: *IrBuilder,
        value: Ast.Var,
        ty: Exec.Type.TypeId,
        true_discriminant: u16,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        if (self.typeIsPrimitiveBool(ty)) return value;
        const shape = self.boolTagUnionShapeForType(ty);
        const discriminant = try self.bindAnonymous(.{ .canonical = .u16 }, .{ .get_union_id = .{
            .value = value,
            .source = self.discriminantSourceForTagUnionShape(shape),
        } }, stmts);
        const expected = try self.bindAnonymous(.{ .canonical = .u16 }, .{ .lit = .{
            .int = true_discriminant,
        } }, stmts);
        return try self.boolLowLevel(.num_is_eq, discriminant, expected, stmts);
    }

    fn typeIsPrimitiveBool(self: *const IrBuilder, ty: Exec.Type.TypeId) bool {
        return switch (self.input.types.getType(self.resolveLayoutType(ty))) {
            .primitive => |prim| prim == .bool,
            else => false,
        };
    }

    fn boolTagUnionShapeForType(self: *const IrBuilder, ty: Exec.Type.TypeId) mir.MonoRow.TagUnionShapeId {
        return switch (self.input.types.getType(self.resolveLayoutType(ty))) {
            .nominal => |nominal| self.boolTagUnionShapeForType(nominal.backing),
            .tag_union => |tag_union| tag_union.shape,
            else => irInvariant("IR lowering expected Bool condition/result to be an ordinary tag union"),
        };
    }

    fn valueBlock(self: *IrBuilder, value: Ast.Var) LowerResourceError!Ast.BlockId {
        return try self.output.store.addBlock(.{
            .stmts = Ast.Span(Ast.StmtId).empty(),
            .term = .{ .value = value },
        });
    }

    fn lowerCallableMatch(
        self: *IrBuilder,
        expr: Exec.Ast.Expr,
        callable_match: anytype,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const callee = self.value_env.get(callable_match.callee) orelse irInvariant("IR lowering callable_match callee was not bound");
        const branch_ids = self.input.ast.callable_match_branches.items[callable_match.branches.start..][0..callable_match.branches.len];
        if (branch_ids.len == 0) irInvariant("IR lowering callable_match received no branches");

        const subject = try self.bindExpr(
            self.freshInternalValueRef(),
            .{ .canonical = .u16 },
            .{ .get_union_id = .{
                .value = callee,
                .source = if (branch_ids.len == 1)
                    .{ .known_singleton = @intCast(@intFromEnum(branch_ids[0].member.member_index)) }
                else
                    .runtime_callable_set,
            } },
            stmts,
        );
        const result = try self.freshVar(try self.layoutForType(expr.ty));
        const branches = try self.allocator.alloc(Ast.Branch, branch_ids.len);
        defer self.allocator.free(branches);

        for (branch_ids, 0..) |branch_id, i| {
            const branch = branch_id;
            if (!repr.callableSetKeyEql(branch.member.callable_set_key, callable_match.callable_set_key)) {
                irInvariant("IR lowering callable_match branch points at a different callable set");
            }
            branches[i] = .{
                .value = @intCast(@intFromEnum(branch.member.member_index)),
                .block = try self.lowerCallableMatchBranchBlock(branch, callee),
            };
        }

        try stmts.append(self.allocator, try self.output.store.addStmt(.{ .switch_ = .{
            .cond = subject,
            .branches = try self.output.store.addBranchSpan(branches),
            .default_block = try self.output.store.addBlock(.{
                .stmts = Ast.Span(Ast.StmtId).empty(),
                .term = .@"unreachable",
            }),
            .join = result,
        } }));
        try self.value_env.put(expr.value, result);
        return result;
    }

    fn lowerCallableMatchBranchBlock(
        self: *IrBuilder,
        branch: Exec.Ast.CallableMatchBranch,
        callee: Ast.Var,
    ) LowerResourceError!Ast.BlockId {
        var saved = std.ArrayList(SavedValueBinding).empty;
        defer {
            self.restoreValueBindings(saved.items);
            saved.deinit(self.allocator);
        }

        var stmts = std.ArrayList(Ast.StmtId).empty;
        defer stmts.deinit(self.allocator);

        if (branch.capture_payload) |payload_ref| {
            const payload_ty = branch.capture_payload_ty orelse irInvariant("IR lowering callable_match branch has capture payload value without payload type");
            const payload = try self.freshVar(try self.layoutForType(payload_ty));
            try stmts.append(self.allocator, try self.output.store.addStmt(.{ .let_ = .{
                .bind = payload,
                .expr = try self.output.store.addExpr(.{ .get_union_struct = .{
                    .value = callee,
                    .tag_discriminant = @intCast(@intFromEnum(branch.member.member_index)),
                } }),
            } }));
            try self.pushValueBinding(payload_ref, payload, &saved);
        } else if (branch.capture_payload_ty != null) {
            irInvariant("IR lowering callable_match branch has payload type without payload value");
        }

        const body = try self.lowerExprToBlock(branch.body);
        if (stmts.items.len == 0) return body;

        const nested = self.output.store.getBlock(body);
        try stmts.appendSlice(self.allocator, self.output.store.stmt_ids.items[nested.stmts.start..][0..nested.stmts.len]);
        return try self.output.store.addBlock(.{
            .stmts = try self.output.store.addStmtSpan(stmts.items),
            .term = nested.term,
        });
    }

    fn lowerSourceMatch(
        self: *IrBuilder,
        expr: Exec.Ast.Expr,
        source_match: Exec.Ast.SourceMatch,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const scrutinee_exprs = self.input.ast.expr_ids.items[source_match.scrutinee_exprs.start..][0..source_match.scrutinee_exprs.len];
        const scrutinee_values = self.input.ast.value_refs.items[source_match.scrutinees.start..][0..source_match.scrutinees.len];
        if (scrutinee_exprs.len != scrutinee_values.len) {
            irInvariant("IR lowering source_match scrutinee expr/value counts disagreed");
        }
        for (scrutinee_exprs) |scrutinee_expr| {
            _ = try self.lowerExpr(scrutinee_expr, stmts);
        }

        const result = try self.freshVar(try self.layoutForType(expr.ty));
        var path_values = std.ArrayList(SourceMatchPathValue).empty;
        defer path_values.deinit(self.allocator);
        try self.appendSourceMatchDecisionNode(source_match.decision_plan, scrutinee_values, result, &path_values, stmts);
        try self.value_env.put(expr.value, result);
        return result;
    }

    fn lowerValueTransformTagUnion(
        self: *IrBuilder,
        expr: Exec.Ast.Expr,
        tag_transform: anytype,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const source = self.value_env.get(tag_transform.source) orelse
            irInvariant("IR lowering value_transform_tag_union source value was not bound");
        const discriminant = try self.bindExpr(
            self.freshInternalValueRef(),
            .{ .canonical = .u16 },
            .{ .get_union_id = .{
                .value = source,
                .source = self.discriminantSourceForTagUnionShape(tag_transform.source_union_shape),
            } },
            stmts,
        );

        const input_branches = self.input.ast.value_transform_tag_branches.items[tag_transform.branches.start..][0..tag_transform.branches.len];
        if (input_branches.len == 0) irInvariant("IR lowering value_transform_tag_union had no branches");
        const branches = try self.allocator.alloc(Ast.Branch, input_branches.len);
        defer self.allocator.free(branches);
        for (input_branches, 0..) |branch, i| {
            branches[i] = .{
                .value = branch.discriminant,
                .block = try self.lowerExprToBlock(branch.body),
            };
        }

        const result = try self.freshVar(try self.layoutForType(expr.ty));
        try stmts.append(self.allocator, try self.output.store.addStmt(.{ .switch_ = .{
            .cond = discriminant,
            .branches = try self.output.store.addBranchSpan(branches),
            .default_block = try self.output.store.addBlock(.{
                .stmts = Ast.Span(Ast.StmtId).empty(),
                .term = .@"unreachable",
            }),
            .join = result,
        } }));
        try self.value_env.put(expr.value, result);
        return result;
    }

    fn lowerValueTransformList(
        self: *IrBuilder,
        expr: Exec.Ast.Expr,
        list_transform: Exec.Ast.ValueTransformList,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const source = self.value_env.get(list_transform.source) orelse
            irInvariant("IR lowering value_transform_list source value was not bound");
        const source_elem_layout = try self.layoutForType(list_transform.source_elem_ty);
        const result_layout = try self.layoutForType(expr.ty);

        const len = try self.bindAnonymous(.{ .canonical = .u64 }, .{ .call_low_level = .{
            .op = .list_len,
            .rc_effect = base.LowLevel.list_len.rcEffect(),
            .args = try self.output.store.addVarSpan(&[_]Ast.Var{source}),
        } }, stmts);
        const result = try self.bindExpr(expr.value, result_layout, .{ .call_low_level = .{
            .op = .list_with_capacity,
            .rc_effect = base.LowLevel.list_with_capacity.rcEffect(),
            .args = try self.output.store.addVarSpan(&[_]Ast.Var{len}),
        } }, stmts);

        const elem = try self.freshVar(source_elem_layout);
        try stmts.append(self.allocator, try self.output.store.addStmt(.{ .for_list = .{
            .elem = elem,
            .iterable = source,
            .body = try self.lowerValueTransformListBodyBlock(list_transform, elem, result, result_layout),
            .elem_bridge_plan = try self.output.store.addBridgePlan(.direct),
        } }));

        try self.value_env.put(expr.value, result);
        return result;
    }

    fn lowerValueTransformListBodyBlock(
        self: *IrBuilder,
        list_transform: Exec.Ast.ValueTransformList,
        elem: Ast.Var,
        result: Ast.Var,
        result_layout: Ast.LayoutRef,
    ) LowerResourceError!Ast.BlockId {
        var saved = std.ArrayList(SavedValueBinding).empty;
        defer {
            self.restoreValueBindings(saved.items);
            saved.deinit(self.allocator);
        }

        var body_stmts = std.ArrayList(Ast.StmtId).empty;
        defer body_stmts.deinit(self.allocator);

        try self.pushValueBinding(list_transform.source_elem, elem, &saved);
        const transformed = try self.lowerExpr(list_transform.body, &body_stmts);
        const append_args = [_]Ast.Var{ result, transformed };
        const appended = try self.bindAnonymous(result_layout, .{ .call_low_level = .{
            .op = .list_append_unsafe,
            .rc_effect = base.LowLevel.list_append_unsafe.rcEffect(),
            .args = try self.output.store.addVarSpan(&append_args),
        } }, &body_stmts);
        try body_stmts.append(self.allocator, try self.output.store.addStmt(.{ .set = .{
            .target = result,
            .value = appended,
        } }));

        return try self.output.store.addBlock(.{
            .stmts = try self.output.store.addStmtSpan(body_stmts.items),
            .term = .{ .value = result },
        });
    }

    fn lowerForExpr(
        self: *IrBuilder,
        expr: Exec.Ast.Expr,
        for_: anytype,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        try self.appendForList(for_, stmts);
        return try self.bindExpr(
            expr.value,
            .{ .canonical = .zst },
            try self.makeDirectStructExpr(&.{}),
            stmts,
        );
    }

    fn appendWhile(
        self: *IrBuilder,
        while_: anytype,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!void {
        try stmts.append(self.allocator, try self.output.store.addStmt(.{ .while_ = .{
            .cond = try self.lowerPredicateToBlock(while_.cond),
            .body = try self.lowerExprToBlock(while_.body),
        } }));
    }

    fn appendForList(
        self: *IrBuilder,
        for_: anytype,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!void {
        const iterable = try self.lowerExpr(for_.iterable, stmts);
        const elem_ty = self.listElementType(self.input.ast.getExpr(for_.iterable).ty);
        const elem = try self.freshVar(try self.layoutForType(elem_ty));
        const direct = try self.output.store.addBridgePlan(.direct);
        try stmts.append(self.allocator, try self.output.store.addStmt(.{ .for_list = .{
            .elem = elem,
            .iterable = iterable,
            .body = try self.lowerForBodyBlock(for_.patt, elem, for_.body),
            .elem_bridge_plan = direct,
        } }));
    }

    fn lowerForBodyBlock(
        self: *IrBuilder,
        pat_id: Exec.Ast.PatId,
        elem: Ast.Var,
        body: Exec.Ast.ExprId,
    ) LowerResourceError!Ast.BlockId {
        var saved = std.ArrayList(SavedValueBinding).empty;
        defer {
            self.restoreValueBindings(saved.items);
            saved.deinit(self.allocator);
        }

        var body_stmts = std.ArrayList(Ast.StmtId).empty;
        defer body_stmts.deinit(self.allocator);
        const pat = self.input.ast.pats.items[@intFromEnum(pat_id)];
        try self.bindForPatternValues(pat, elem, &body_stmts, &saved);
        const result = try self.lowerExpr(body, &body_stmts);
        return try self.output.store.addBlock(.{
            .stmts = try self.output.store.addStmtSpan(body_stmts.items),
            .term = .{ .value = result },
        });
    }

    const SavedValueBinding = struct {
        value: Exec.Ast.ExecutableValueRef,
        previous: ?Ast.Var,
    };

    const SourceMatchPathValue = struct {
        plan: Exec.Ast.PatternPathValuePlanId,
        value: Ast.Var,
    };

    fn appendSourceMatchDecisionNode(
        self: *IrBuilder,
        decision_plan_id: Exec.Ast.PatternDecisionPlanId,
        scrutinee_values: []const Exec.Ast.ExecutableValueRef,
        result: Ast.Var,
        path_values: *std.ArrayList(SourceMatchPathValue),
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!void {
        const decision_plan = self.input.ast.getPatternDecisionPlan(decision_plan_id);
        try self.appendDecisionNode(decision_plan.root, scrutinee_values, result, path_values, stmts);
    }

    fn appendDecisionNode(
        self: *IrBuilder,
        node_id: Exec.Ast.DecisionNodeId,
        scrutinee_values: []const Exec.Ast.ExecutableValueRef,
        result: Ast.Var,
        path_values: *std.ArrayList(SourceMatchPathValue),
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!void {
        return switch (self.input.ast.getDecisionNode(node_id)) {
            .leaf => |leaf_id| try self.appendDecisionLeaf(leaf_id, scrutinee_values, result, path_values, stmts),
            .decision_test => |test_node| try self.appendDecisionTest(test_node, scrutinee_values, result, path_values, stmts),
        };
    }

    fn appendDecisionTest(
        self: *IrBuilder,
        test_node: Exec.Ast.DecisionTestNode,
        scrutinee_values: []const Exec.Ast.ExecutableValueRef,
        result: Ast.Var,
        path_values: *std.ArrayList(SourceMatchPathValue),
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!void {
        const path_value = try self.materializePatternPathValue(test_node.path_value, scrutinee_values, path_values, stmts);
        const edges = self.input.ast.sliceDecisionEdgeSpan(test_node.edges);
        if (edges.len == 0) irInvariant("IR lowering source_match decision test had no edges");
        try self.appendDecisionEdgeCascade(edges, 0, path_value, test_node.default, scrutinee_values, result, path_values, stmts);
    }

    fn appendDecisionEdgeCascade(
        self: *IrBuilder,
        edges: []const Exec.Ast.DecisionEdge,
        index: usize,
        path_value: Ast.Var,
        default_node: ?Exec.Ast.DecisionNodeId,
        scrutinee_values: []const Exec.Ast.ExecutableValueRef,
        result: Ast.Var,
        path_values: *std.ArrayList(SourceMatchPathValue),
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!void {
        if (index >= edges.len) {
            if (default_node) |node| {
                try self.appendDecisionNode(node, scrutinee_values, result, path_values, stmts);
            } else {
                try stmts.append(self.allocator, try self.output.store.addStmt(.{ .switch_ = .{
                    .cond = try self.u64Literal(0, stmts),
                    .branches = Ast.Span(Ast.BranchId).empty(),
                    .default_block = try self.unreachableBlock(),
                    .join = result,
                } }));
            }
            return;
        }

        const edge = edges[index];
        const condition = try self.lowerPatternTest(path_value, edge.pattern_test, stmts);
        const true_block = try self.decisionNodeBlock(edge.next, scrutinee_values, result, path_values);
        const false_block = try self.decisionEdgeCascadeBlock(edges, index + 1, path_value, default_node, scrutinee_values, result, path_values);
        const true_branches = [_]Ast.Branch{.{
            .value = 1,
            .block = true_block,
        }};
        try stmts.append(self.allocator, try self.output.store.addStmt(.{ .switch_ = .{
            .cond = condition,
            .branches = try self.output.store.addBranchSpan(&true_branches),
            .default_block = false_block,
            .join = result,
        } }));
    }

    fn decisionEdgeCascadeBlock(
        self: *IrBuilder,
        edges: []const Exec.Ast.DecisionEdge,
        index: usize,
        path_value: Ast.Var,
        default_node: ?Exec.Ast.DecisionNodeId,
        scrutinee_values: []const Exec.Ast.ExecutableValueRef,
        result: Ast.Var,
        path_values: *std.ArrayList(SourceMatchPathValue),
    ) LowerResourceError!Ast.BlockId {
        var path_copy = try self.clonePathValues(path_values.items);
        defer path_copy.deinit(self.allocator);
        var block_stmts = std.ArrayList(Ast.StmtId).empty;
        defer block_stmts.deinit(self.allocator);
        try self.appendDecisionEdgeCascade(edges, index, path_value, default_node, scrutinee_values, result, &path_copy, &block_stmts);
        return try self.output.store.addBlock(.{
            .stmts = try self.output.store.addStmtSpan(block_stmts.items),
            .term = .{ .value = result },
        });
    }

    fn decisionNodeBlock(
        self: *IrBuilder,
        node_id: Exec.Ast.DecisionNodeId,
        scrutinee_values: []const Exec.Ast.ExecutableValueRef,
        result: Ast.Var,
        path_values: *std.ArrayList(SourceMatchPathValue),
    ) LowerResourceError!Ast.BlockId {
        var path_copy = try self.clonePathValues(path_values.items);
        defer path_copy.deinit(self.allocator);
        var block_stmts = std.ArrayList(Ast.StmtId).empty;
        defer block_stmts.deinit(self.allocator);
        try self.appendDecisionNode(node_id, scrutinee_values, result, &path_copy, &block_stmts);
        return try self.output.store.addBlock(.{
            .stmts = try self.output.store.addStmtSpan(block_stmts.items),
            .term = .{ .value = result },
        });
    }

    fn appendDecisionLeaf(
        self: *IrBuilder,
        leaf_id: Exec.Ast.DecisionLeafId,
        scrutinee_values: []const Exec.Ast.ExecutableValueRef,
        result: Ast.Var,
        path_values: *std.ArrayList(SourceMatchPathValue),
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!void {
        const leaf = self.input.ast.getDecisionLeaf(leaf_id);
        if (leaf.degenerate) {
            try self.appendExprResultOrTerminator(leaf.body, result, stmts);
            return;
        }

        var saved = std.ArrayList(SavedValueBinding).empty;
        var bindings_restored = false;
        defer {
            if (!bindings_restored) self.restoreValueBindings(saved.items);
            saved.deinit(self.allocator);
        }

        const bindings = self.input.ast.slicePatternBindingSpan(leaf.bindings);
        for (bindings) |binding| {
            const value = try self.materializePatternPathValue(binding.source, scrutinee_values, path_values, stmts);
            try self.bindPatternValue(binding, value, stmts, &saved);
        }

        if (leaf.guard) |guard_expr| {
            const guard = try self.lowerExprAsPredicate(guard_expr.expr, guard_expr.true_discriminant, stmts);
            const true_block = try self.decisionLeafBodyBlock(leaf, result);
            self.restoreValueBindings(saved.items);
            saved.clearRetainingCapacity();
            bindings_restored = true;
            const true_branches = [_]Ast.Branch{.{
                .value = 1,
                .block = true_block,
            }};
            const default_block = if (leaf.guard_miss) |guard_miss|
                try self.decisionNodeBlock(guard_miss, scrutinee_values, result, path_values)
            else
                try self.unreachableBlock();
            try stmts.append(self.allocator, try self.output.store.addStmt(.{ .switch_ = .{
                .cond = guard,
                .branches = try self.output.store.addBranchSpan(&true_branches),
                .default_block = default_block,
                .join = result,
            } }));
            return;
        }

        try self.appendExprResultOrTerminator(leaf.body, result, stmts);
    }

    fn clonePathValues(
        self: *IrBuilder,
        path_values: []const SourceMatchPathValue,
    ) LowerResourceError!std.ArrayList(SourceMatchPathValue) {
        var clone = std.ArrayList(SourceMatchPathValue).empty;
        errdefer clone.deinit(self.allocator);
        try clone.appendSlice(self.allocator, path_values);
        return clone;
    }

    fn decisionLeafBodyBlock(
        self: *IrBuilder,
        leaf: Exec.Ast.DecisionLeaf,
        result: Ast.Var,
    ) LowerResourceError!Ast.BlockId {
        var stmts = std.ArrayList(Ast.StmtId).empty;
        defer stmts.deinit(self.allocator);
        try self.appendExprResultOrTerminator(leaf.body, result, &stmts);
        return try self.output.store.addBlock(.{
            .stmts = try self.output.store.addStmtSpan(stmts.items),
            .term = .{ .value = result },
        });
    }

    fn appendExprResultOrTerminator(
        self: *IrBuilder,
        expr_id: Exec.Ast.ExprId,
        result: Ast.Var,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!void {
        const expr = self.input.ast.getExpr(expr_id);
        switch (expr.data) {
            .return_ => |child| {
                const value = try self.lowerExpr(child, stmts);
                try stmts.append(self.allocator, try self.output.store.addStmt(.{ .return_ = value }));
            },
            .crash => |literal| try stmts.append(self.allocator, try self.output.store.addStmt(.{ .crash = literal })),
            .runtime_error => try stmts.append(self.allocator, try self.output.store.addStmt(.runtime_error)),
            .@"unreachable" => try stmts.append(self.allocator, try self.output.store.addStmt(.runtime_error)),
            .block => |block| {
                try self.lowerStmtSpan(block.stmts, stmts);
                try self.appendExprResultOrTerminator(block.final_expr, result, stmts);
            },
            else => {
                const value = try self.lowerExpr(expr_id, stmts);
                try stmts.append(self.allocator, try self.output.store.addStmt(.{ .set = .{
                    .target = result,
                    .value = value,
                } }));
            },
        }
    }

    fn unreachableBlock(self: *IrBuilder) LowerResourceError!Ast.BlockId {
        return try self.output.store.addBlock(.{
            .stmts = Ast.Span(Ast.StmtId).empty(),
            .term = .@"unreachable",
        });
    }

    fn materializePatternPathValue(
        self: *IrBuilder,
        plan_id: Exec.Ast.PatternPathValuePlanId,
        scrutinee_values: []const Exec.Ast.ExecutableValueRef,
        path_values: *std.ArrayList(SourceMatchPathValue),
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        for (path_values.items) |entry| {
            if (entry.plan == plan_id) return entry.value;
        }

        const plan = self.input.ast.getPatternPathValuePlan(plan_id);
        const value = switch (plan.source) {
            .scrutinee => |index| blk: {
                if (index >= scrutinee_values.len) irInvariant("IR lowering source_match path referenced missing scrutinee");
                break :blk self.value_env.get(scrutinee_values[index]) orelse
                    irInvariant("IR lowering source_match scrutinee value was not bound");
            },
            .tag_payload_record => |payload| blk: {
                const parent = try self.materializePatternPathValue(payload.parent, scrutinee_values, path_values, stmts);
                break :blk try self.bindAnonymous(try self.payloadStructLayoutFromUnionLayout(parent.layout, payload.tag), .{ .get_union_struct = .{
                    .value = parent,
                    .tag_discriminant = @intCast(self.input.row_shapes.tag(payload.tag).logical_index),
                } }, stmts);
            },
            .tag_payload_field => |payload| blk: {
                const parent = try self.materializePatternPathValue(payload.parent_payload_record, scrutinee_values, path_values, stmts);
                const parent_plan = self.input.ast.getPatternPathValuePlan(payload.parent_payload_record);
                if (self.singlePayloadRecordPath(parent_plan)) break :blk parent;
                break :blk try self.bindAnonymous(try self.layoutForType(plan.ty), .{ .get_struct_field = .{
                    .record = parent,
                    .field_index = @intCast(self.input.row_shapes.tagPayload(payload.payload).logical_index),
                    .field_bridge_plan = try self.constructionSlotBridge(plan.ty, plan.ty),
                } }, stmts);
            },
            .record_field => |field| blk: {
                const parent = try self.materializePatternPathValue(field.parent, scrutinee_values, path_values, stmts);
                break :blk try self.bindAnonymous(try self.layoutForType(plan.ty), .{ .get_struct_field = .{
                    .record = parent,
                    .field_index = @intCast(self.input.row_shapes.recordField(field.field).logical_index),
                    .field_bridge_plan = try self.constructionSlotBridge(plan.ty, plan.ty),
                } }, stmts);
            },
            .record_rest => |projection| try self.materializeRecordRestPatternPath(plan.ty, projection, scrutinee_values, path_values, stmts),
            .tuple_field => |field| blk: {
                const parent = try self.materializePatternPathValue(field.parent, scrutinee_values, path_values, stmts);
                break :blk try self.bindAnonymous(try self.layoutForType(plan.ty), .{ .get_struct_field = .{
                    .record = parent,
                    .field_index = @intCast(field.field),
                    .field_bridge_plan = try self.constructionSlotBridge(plan.ty, plan.ty),
                } }, stmts);
            },
            .list_element => |element| try self.materializeListElementPatternPath(plan.ty, element, scrutinee_values, path_values, stmts),
            .list_rest => |rest| try self.materializeListRestPatternPath(plan.ty, rest, scrutinee_values, path_values, stmts),
            .nominal_payload => |parent| try self.materializePatternPathValue(parent, scrutinee_values, path_values, stmts),
        };

        try path_values.append(self.allocator, .{
            .plan = plan_id,
            .value = value,
        });
        return value;
    }

    fn singlePayloadRecordPath(self: *IrBuilder, plan: Exec.Ast.PatternPathValuePlan) bool {
        return switch (plan.source) {
            .tag_payload_record => |payload| self.input.row_shapes.tagPayloads(payload.tag).len == 1,
            else => false,
        };
    }

    fn materializeRecordRestPatternPath(
        self: *IrBuilder,
        ty: Exec.Type.TypeId,
        projection_id: Exec.Ast.RecordRestProjectionId,
        scrutinee_values: []const Exec.Ast.ExecutableValueRef,
        path_values: *std.ArrayList(SourceMatchPathValue),
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const projection = self.input.ast.getRecordRestProjection(projection_id);
        const parent = try self.materializePatternPathValue(projection.parent, scrutinee_values, path_values, stmts);
        const projected = self.input.ast.sliceRecordRestProjectedFieldSpan(projection.projected_fields);
        const fields = try self.allocator.alloc(Ast.Var, projected.len);
        defer self.allocator.free(fields);
        const seen = try self.allocator.alloc(bool, projected.len);
        defer self.allocator.free(seen);
        @memset(seen, false);
        for (projected) |field| {
            const index: usize = @intCast(field.result_logical_index);
            if (index >= fields.len) irInvariant("IR lowering record-rest result field index exceeded projection arity");
            if (seen[index]) irInvariant("IR lowering record-rest projection saw duplicate result field index");
            fields[index] = try self.bindAnonymous(try self.layoutForType(field.ty), .{ .get_struct_field = .{
                .record = parent,
                .field_index = @intCast(self.input.row_shapes.recordField(field.source_field).logical_index),
                .field_bridge_plan = try self.constructionSlotBridge(field.ty, field.ty),
            } }, stmts);
            seen[index] = true;
        }
        for (seen) |was_seen| {
            if (!was_seen) irInvariant("IR lowering record-rest projection did not provide every result field");
        }
        return try self.bindAnonymous(try self.layoutForType(ty), try self.makeDirectStructExpr(fields), stmts);
    }

    fn materializeListElementPatternPath(
        self: *IrBuilder,
        ty: Exec.Type.TypeId,
        element: anytype,
        scrutinee_values: []const Exec.Ast.ExecutableValueRef,
        path_values: *std.ArrayList(SourceMatchPathValue),
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const parent = try self.materializePatternPathValue(element.parent, scrutinee_values, path_values, stmts);
        const index = try self.listProbeIndex(parent, element.probe, stmts);
        const args = [_]Ast.Var{ parent, index };
        const result_layout = try self.layoutForType(ty);
        if (@import("builtin").mode == .Debug) {
            const parent_plan = self.input.ast.getPatternPathValuePlan(element.parent);
            const elem_ty = self.listElementType(parent_plan.ty);
            const elem_layout = try self.layoutForType(elem_ty);
            if (layout_mod.graphRefKey(result_layout) != layout_mod.graphRefKey(elem_layout)) {
                std.debug.panic(
                    "IR lowering invariant violated: list element pattern result layout differs from parent list element layout (result={d}, elem={d})",
                    .{ layout_mod.graphRefKey(result_layout), layout_mod.graphRefKey(elem_layout) },
                );
            }
        }
        return try self.bindAnonymous(result_layout, .{ .call_low_level = .{
            .op = .list_get_unsafe,
            .rc_effect = base.LowLevel.list_get_unsafe.rcEffect(),
            .args = try self.output.store.addVarSpan(&args),
        } }, stmts);
    }

    fn materializeListRestPatternPath(
        self: *IrBuilder,
        ty: Exec.Type.TypeId,
        rest: anytype,
        scrutinee_values: []const Exec.Ast.ExecutableValueRef,
        path_values: *std.ArrayList(SourceMatchPathValue),
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const parent = try self.materializePatternPathValue(rest.parent, scrutinee_values, path_values, stmts);
        const len = try self.listLength(parent, stmts);
        const dropped_tail = try self.u64Literal(@intCast(rest.probe.from_end_count), stmts);
        const len_without_tail = try self.bindAnonymous(.{ .canonical = .u64 }, .{ .call_low_level = .{
            .op = .num_minus,
            .rc_effect = base.LowLevel.num_minus.rcEffect(),
            .args = try self.output.store.addVarSpan(&[_]Ast.Var{ len, dropped_tail }),
        } }, stmts);
        const start = try self.u64Literal(@intCast(rest.probe.start), stmts);
        const rest_len = try self.bindAnonymous(.{ .canonical = .u64 }, .{ .call_low_level = .{
            .op = .num_minus,
            .rc_effect = base.LowLevel.num_minus.rcEffect(),
            .args = try self.output.store.addVarSpan(&[_]Ast.Var{ len_without_tail, start }),
        } }, stmts);
        const slice_record = try self.bindAnonymous(try self.structLayout(&[_]Ast.Var{ rest_len, start }), try self.makeDirectStructExpr(&[_]Ast.Var{ rest_len, start }), stmts);
        return try self.bindAnonymous(try self.layoutForType(ty), .{ .call_low_level = .{
            .op = .list_sublist,
            .rc_effect = base.LowLevel.list_sublist.rcEffect(),
            .args = try self.output.store.addVarSpan(&[_]Ast.Var{ parent, slice_record }),
        } }, stmts);
    }

    fn listProbeIndex(
        self: *IrBuilder,
        list: Ast.Var,
        probe: Exec.Ast.ListElementProbe,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const offset = try self.u64Literal(@intCast(probe.index), stmts);
        if (!probe.from_end) return offset;
        const len = try self.listLength(list, stmts);
        return try self.bindAnonymous(.{ .canonical = .u64 }, .{ .call_low_level = .{
            .op = .num_minus,
            .rc_effect = base.LowLevel.num_minus.rcEffect(),
            .args = try self.output.store.addVarSpan(&[_]Ast.Var{ len, offset }),
        } }, stmts);
    }

    fn listLength(
        self: *IrBuilder,
        list: Ast.Var,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        return try self.bindAnonymous(.{ .canonical = .u64 }, .{ .call_low_level = .{
            .op = .list_len,
            .rc_effect = base.LowLevel.list_len.rcEffect(),
            .args = try self.output.store.addVarSpan(&[_]Ast.Var{list}),
        } }, stmts);
    }

    fn u64Literal(
        self: *IrBuilder,
        value: u64,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        return try self.bindAnonymous(.{ .canonical = .u64 }, .{ .lit = .{ .int = @intCast(value) } }, stmts);
    }

    fn lowerPatternTest(
        self: *IrBuilder,
        path_value: Ast.Var,
        pattern_test: Exec.Ast.PatternTest,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        return switch (pattern_test) {
            .tag => |tag| blk: {
                const discriminant = try self.bindAnonymous(.{ .canonical = .u16 }, .{ .get_union_id = .{
                    .value = path_value,
                    .source = self.discriminantSourceForTagUnionShape(tag.union_shape),
                } }, stmts);
                const literal = try self.bindAnonymous(.{ .canonical = .u16 }, .{ .lit = .{ .int = @intCast(self.input.row_shapes.tag(tag.tag).logical_index) } }, stmts);
                break :blk try self.boolLowLevel(.num_is_eq, discriminant, literal, stmts);
            },
            .int_literal => |literal| blk: {
                const expected = try self.bindAnonymous(path_value.layout, .{ .lit = .{ .int = literal } }, stmts);
                break :blk try self.boolStructuralEq(path_value, expected, stmts);
            },
            .float_f32_literal => |literal| blk: {
                const expected = try self.bindAnonymous(path_value.layout, .{ .lit = .{ .f32 = literal } }, stmts);
                break :blk try self.boolStructuralEq(path_value, expected, stmts);
            },
            .float_f64_literal => |literal| blk: {
                const expected = try self.bindAnonymous(path_value.layout, .{ .lit = .{ .f64 = literal } }, stmts);
                break :blk try self.boolStructuralEq(path_value, expected, stmts);
            },
            .decimal_literal => |literal| blk: {
                const expected = try self.bindAnonymous(path_value.layout, .{ .lit = .{ .dec = literal } }, stmts);
                break :blk try self.boolStructuralEq(path_value, expected, stmts);
            },
            .str_literal => |literal| blk: {
                const expected = try self.bindAnonymous(.{ .canonical = .str }, .{ .lit = .{ .str = literal } }, stmts);
                break :blk try self.boolLowLevel(.str_is_eq, path_value, expected, stmts);
            },
            .list_len_exact => |expected_len| blk: {
                const len = try self.listLength(path_value, stmts);
                const expected = try self.u64Literal(@intCast(expected_len), stmts);
                break :blk try self.boolLowLevel(.num_is_eq, len, expected, stmts);
            },
            .list_len_at_least => |expected_len| blk: {
                const len = try self.listLength(path_value, stmts);
                const expected = try self.u64Literal(@intCast(expected_len), stmts);
                break :blk try self.boolLowLevel(.num_is_gte, len, expected, stmts);
            },
            .guard => |guard_expr| try self.lowerExprAsPredicate(guard_expr.expr, guard_expr.true_discriminant, stmts),
        };
    }

    fn discriminantSourceForTagUnionShape(
        self: *const IrBuilder,
        shape: mir.MonoRow.TagUnionShapeId,
    ) Ast.DiscriminantSource {
        const tags = self.input.row_shapes.tagUnionTags(shape);
        if (tags.len == 1) {
            return .{ .known_singleton = @intCast(self.input.row_shapes.tag(tags[0]).logical_index) };
        }
        return .{ .runtime_tag_union = shape };
    }

    fn boolStructuralEq(
        self: *IrBuilder,
        lhs: Ast.Var,
        rhs: Ast.Var,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        return try self.bindAnonymous(.{ .canonical = .bool }, .{ .structural_eq = .{
            .lhs = lhs,
            .rhs = rhs,
        } }, stmts);
    }

    fn boolLowLevel(
        self: *IrBuilder,
        op: base.LowLevel,
        lhs: Ast.Var,
        rhs: Ast.Var,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        return try self.bindAnonymous(.{ .canonical = .bool }, .{ .call_low_level = .{
            .op = op,
            .rc_effect = op.rcEffect(),
            .args = try self.output.store.addVarSpan(&[_]Ast.Var{ lhs, rhs }),
        } }, stmts);
    }

    fn bindForPatternValues(
        self: *IrBuilder,
        pat: Exec.Ast.Pat,
        value: Ast.Var,
        stmts: *std.ArrayList(Ast.StmtId),
        saved: *std.ArrayList(SavedValueBinding),
    ) LowerResourceError!void {
        switch (pat.data) {
            .wildcard,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            => {},
            .as => |as| {
                try self.pushValueBinding(as.bind.value, value, saved);
                const child_pat = self.input.ast.pats.items[@intFromEnum(as.pattern)];
                try self.bindForPatternValues(child_pat, value, stmts, saved);
            },
            .bind => |bind| try self.pushValueBinding(bind.value, value, saved),
            .nominal => |child| {
                const child_pat = self.input.ast.pats.items[@intFromEnum(child)];
                try self.bindForPatternValues(child_pat, value, stmts, saved);
            },
            .tuple => |items| {
                const child_pats = self.input.ast.pat_ids.items[items.start..][0..items.len];
                for (child_pats, 0..) |child_pat_id, i| {
                    const child_pat = self.input.ast.pats.items[@intFromEnum(child_pat_id)];
                    const child_value = try self.bindExpr(
                        self.freshInternalValueRef(),
                        try self.layoutForType(child_pat.ty),
                        .{ .get_struct_field = .{
                            .record = value,
                            .field_index = @intCast(i),
                            .field_bridge_plan = try self.constructionSlotBridge(child_pat.ty, child_pat.ty),
                        } },
                        stmts,
                    );
                    try self.bindForPatternValues(child_pat, child_value, stmts, saved);
                }
            },
            .record => |record| {
                if (record.rest != null) irInvariant("IR lowering for pattern requires explicit record-rest materialization");
                const field_patterns = self.input.ast.record_field_patterns.items[record.fields.start..][0..record.fields.len];
                for (field_patterns) |field_pattern| {
                    const child_pat = self.input.ast.pats.items[@intFromEnum(field_pattern.pattern)];
                    const field = self.input.row_shapes.recordField(field_pattern.field);
                    const child_value = try self.bindExpr(
                        self.freshInternalValueRef(),
                        try self.layoutForType(child_pat.ty),
                        .{ .get_struct_field = .{
                            .record = value,
                            .field_index = @intCast(field.logical_index),
                            .field_bridge_plan = try self.constructionSlotBridge(child_pat.ty, child_pat.ty),
                        } },
                        stmts,
                    );
                    try self.bindForPatternValues(child_pat, child_value, stmts, saved);
                }
            },
            .list => irInvariant("IR lowering for pattern requires explicit list-pattern materialization"),
            .tag => |tag| {
                const payload_ids = self.input.ast.tag_payload_patterns.items[tag.payloads.start..][0..tag.payloads.len];
                if (payload_ids.len == 0) return;

                const payload_record = try self.bindExpr(
                    self.freshInternalValueRef(),
                    try self.payloadStructLayoutFromUnionLayout(value.layout, tag.tag),
                    .{ .get_union_struct = .{
                        .value = value,
                        .tag_discriminant = @intCast(self.input.row_shapes.tag(tag.tag).logical_index),
                    } },
                    stmts,
                );

                for (payload_ids) |payload_pattern| {
                    const child_pat = self.input.ast.pats.items[@intFromEnum(payload_pattern.pattern)];
                    const payload_value = if (payload_ids.len == 1)
                        payload_record
                    else blk: {
                        const payload = self.input.row_shapes.tagPayload(payload_pattern.payload);
                        break :blk try self.bindExpr(
                            self.freshInternalValueRef(),
                            try self.layoutForType(child_pat.ty),
                            .{ .get_struct_field = .{
                                .record = payload_record,
                                .field_index = @intCast(payload.logical_index),
                                .field_bridge_plan = try self.constructionSlotBridge(child_pat.ty, child_pat.ty),
                            } },
                            stmts,
                        );
                    };
                    try self.bindForPatternValues(child_pat, payload_value, stmts, saved);
                }
            },
        }
    }

    fn pushValueBinding(
        self: *IrBuilder,
        value_ref: Exec.Ast.ExecutableValueRef,
        value: Ast.Var,
        saved: *std.ArrayList(SavedValueBinding),
    ) LowerResourceError!void {
        const previous = try self.value_env.fetchPut(value_ref, value);
        try saved.append(self.allocator, .{
            .value = value_ref,
            .previous = if (previous) |entry| entry.value else null,
        });
    }

    fn bindPatternValue(
        self: *IrBuilder,
        binding: Exec.Ast.PatternBinding,
        source: Ast.Var,
        stmts: *std.ArrayList(Ast.StmtId),
        saved: *std.ArrayList(SavedValueBinding),
    ) LowerResourceError!void {
        const bridge_plan = try self.lowerBridgePlan(binding.bridge);
        const bind = try self.freshVar(try self.layoutForType(binding.ty));
        const expr = try self.output.store.addExpr(.{ .bridge = .{
            .value = source,
            .plan = bridge_plan,
        } });
        try stmts.append(self.allocator, try self.output.store.addStmt(.{ .let_ = .{
            .bind = bind,
            .expr = expr,
        } }));
        const previous = try self.value_env.fetchPut(binding.binder, bind);
        try saved.append(self.allocator, .{
            .value = binding.binder,
            .previous = if (previous) |entry| entry.value else null,
        });
    }

    fn freshInternalValueRef(self: *IrBuilder) Exec.Ast.ExecutableValueRef {
        const value: Exec.Ast.ExecutableValueRef = @enumFromInt(self.next_internal_value_ref);
        self.next_internal_value_ref += 1;
        return value;
    }

    fn restoreValueBindings(self: *IrBuilder, saved: []const SavedValueBinding) void {
        var i = saved.len;
        while (i > 0) {
            i -= 1;
            const entry = saved[i];
            if (entry.previous) |previous| {
                self.value_env.put(entry.value, previous) catch unreachable;
            } else {
                _ = self.value_env.remove(entry.value);
            }
        }
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
                const bind = try self.freshVar(value.layout);
                const expr = try self.output.store.addExpr(.{ .var_ = value });
                try stmts.append(self.allocator, try self.output.store.addStmt(.{ .let_ = .{
                    .bind = bind,
                    .expr = expr,
                } }));
                try self.value_env.put(decl.value, bind);
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
            .expect => |condition| {
                const value = try self.lowerExprAsPredicate(condition.expr, condition.true_discriminant, stmts);
                try stmts.append(self.allocator, try self.output.store.addStmt(.{ .expect = value }));
            },
            .return_ => |expr| {
                const value = try self.lowerExpr(expr, stmts);
                try stmts.append(self.allocator, try self.output.store.addStmt(.{ .return_ = value }));
            },
            .for_ => |for_| try self.appendForList(for_, stmts),
            .while_ => |while_| try self.appendWhile(while_, stmts),
            .break_ => try stmts.append(self.allocator, try self.output.store.addStmt(.break_)),
            .crash => |literal| try stmts.append(self.allocator, try self.output.store.addStmt(.{ .crash = literal })),
        }
    }

    const LoweredConstructionValues = struct {
        vars: []const Ast.Var,
        bridges: []const Ast.BridgePlanId,

        fn deinit(self: LoweredConstructionValues, allocator: Allocator) void {
            if (self.vars.len > 0) allocator.free(self.vars);
            if (self.bridges.len > 0) allocator.free(self.bridges);
        }
    };

    const LoweredPayloadConstruction = struct {
        var_: ?Ast.Var,
        bridge: ?Ast.BridgePlanId,
    };

    fn makeDirectStructExpr(
        self: *IrBuilder,
        fields: []const Ast.Var,
    ) LowerResourceError!Ast.Expr {
        const bridge_plans = try self.directBridgePlanSpan(fields.len);
        return .{ .make_struct = .{
            .fields = try self.output.store.addVarSpan(fields),
            .field_bridge_plans = bridge_plans,
        } };
    }

    fn makeStructExpr(
        self: *IrBuilder,
        fields: []const Ast.Var,
        bridge_plans: []const Ast.BridgePlanId,
    ) LowerResourceError!Ast.Expr {
        if (fields.len != bridge_plans.len) {
            irInvariant("IR lowering struct construction bridge count did not match field count");
        }
        return .{ .make_struct = .{
            .fields = try self.output.store.addVarSpan(fields),
            .field_bridge_plans = try self.output.store.addBridgePlanSpan(bridge_plans),
        } };
    }

    fn directBridgePlanSpan(
        self: *IrBuilder,
        count: usize,
    ) LowerResourceError!Ast.Span(Ast.BridgePlanId) {
        if (count == 0) return Ast.Span(Ast.BridgePlanId).empty();
        const direct = try self.output.store.addBridgePlan(.direct);
        const bridge_plans = try self.allocator.alloc(Ast.BridgePlanId, count);
        defer self.allocator.free(bridge_plans);
        @memset(bridge_plans, direct);
        return try self.output.store.addBridgePlanSpan(bridge_plans);
    }

    fn lowerRecordFields(
        self: *IrBuilder,
        span: Exec.Ast.Span(Exec.Ast.RecordFieldExpr),
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!LoweredConstructionValues {
        if (span.len == 0) return .{ .vars = &.{}, .bridges = &.{} };
        const input_items = self.input.ast.record_field_exprs.items[span.start..][0..span.len];
        const values = try self.allocator.alloc(Ast.Var, input_items.len);
        const bridges = try self.allocator.alloc(Ast.BridgePlanId, input_items.len);
        for (input_items, 0..) |field, i| {
            values[i] = try self.lowerExpr(field.expr, stmts);
            bridges[i] = try self.lowerBridgePlan(field.bridge);
        }
        return .{ .vars = values, .bridges = bridges };
    }

    fn lowerTagPayloadForConstruction(
        self: *IrBuilder,
        union_ty: Exec.Type.TypeId,
        tag_id: mir.MonoRow.TagId,
        span: Exec.Ast.Span(Exec.Ast.TagPayloadExpr),
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!LoweredPayloadConstruction {
        const expected_payloads = self.input.row_shapes.tagPayloads(tag_id);
        if (expected_payloads.len == 0) return .{ .var_ = null, .bridge = null };

        const payload_vars = try self.allocator.alloc(Ast.Var, expected_payloads.len);
        defer self.allocator.free(payload_vars);
        const payload_bridges = try self.allocator.alloc(Ast.BridgePlanId, expected_payloads.len);
        defer self.allocator.free(payload_bridges);
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
            payload_bridges[logical_index] = try self.lowerBridgePlan(payload.bridge);
            seen[logical_index] = true;
        }
        for (seen) |was_seen| {
            if (!was_seen) irInvariant("IR lowering tag construction did not provide every payload slot");
        }

        if (payload_vars.len == 1) return .{ .var_ = payload_vars[0], .bridge = payload_bridges[0] };
        const payload_layout = try self.payloadStructLayoutFromUnionLayout(try self.layoutForType(union_ty), tag_id);
        const payload_record = try self.bindAnonymous(payload_layout, try self.makeStructExpr(payload_vars, payload_bridges), stmts);
        const direct = try self.output.store.addBridgePlan(.direct);
        return .{ .var_ = payload_record, .bridge = direct };
    }

    fn lowerTupleItems(
        self: *IrBuilder,
        span: Exec.Ast.Span(Exec.Ast.TupleItemExpr),
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!LoweredConstructionValues {
        if (span.len == 0) return .{ .vars = &.{}, .bridges = &.{} };
        const input_items = self.input.ast.tuple_item_exprs.items[span.start..][0..span.len];
        const values = try self.allocator.alloc(Ast.Var, input_items.len);
        const bridges = try self.allocator.alloc(Ast.BridgePlanId, input_items.len);
        for (input_items, 0..) |item, i| {
            values[i] = try self.lowerExpr(item.expr, stmts);
            bridges[i] = try self.lowerBridgePlan(item.bridge);
        }
        return .{ .vars = values, .bridges = bridges };
    }

    fn lowerListItems(
        self: *IrBuilder,
        span: Exec.Ast.Span(Exec.Ast.ListItemExpr),
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!LoweredConstructionValues {
        if (span.len == 0) return .{ .vars = &.{}, .bridges = &.{} };
        const input_items = self.input.ast.list_item_exprs.items[span.start..][0..span.len];
        const values = try self.allocator.alloc(Ast.Var, input_items.len);
        const bridges = try self.allocator.alloc(Ast.BridgePlanId, input_items.len);
        for (input_items, 0..) |item, i| {
            values[i] = try self.lowerExpr(item.expr, stmts);
            bridges[i] = try self.lowerBridgePlan(item.bridge);
        }
        return .{ .vars = values, .bridges = bridges };
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

        const payload_struct_layout = try self.payloadStructLayoutFromUnionLayout(tag_union.layout, payload_info.tag);
        if (tag_payloads.len == 1) {
            return try self.bindExpr(expr.value, payload_struct_layout, .{ .get_union_struct = .{
                .value = tag_union,
                .tag_discriminant = @intCast(tag_info.logical_index),
            } }, stmts);
        }

        const payload_struct = try self.bindAnonymous(payload_struct_layout, .{ .get_union_struct = .{
            .value = tag_union,
            .tag_discriminant = @intCast(tag_info.logical_index),
        } }, stmts);
        return try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .get_struct_field = .{
            .record = payload_struct,
            .field_index = @intCast(payload_info.logical_index),
            .field_bridge_plan = try self.constructionSlotBridge(expr.ty, expr.ty),
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

    fn lowerVarSpanFromValueRefSpan(
        self: *IrBuilder,
        span: Exec.Ast.Span(Exec.Ast.ExecutableValueRef),
    ) LowerResourceError![]const Ast.Var {
        if (span.len == 0) return &.{};
        const input_items = self.input.ast.value_refs.items[span.start..][0..span.len];
        const values = try self.allocator.alloc(Ast.Var, input_items.len);
        for (input_items, 0..) |value_ref, i| {
            values[i] = self.value_env.get(value_ref) orelse irInvariant("IR lowering value-ref call argument was not bound");
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
            values[i] = self.value_env.get(arg.value) orelse irInvariant("IR lowering direct call argument value was not bound");
        }
        return values;
    }

    fn lowerBridgePlan(self: *IrBuilder, bridge_id: Exec.Ast.BridgeId) LowerResourceError!Ast.BridgePlanId {
        const bridge = self.input.ast.getBridgePlan(bridge_id);
        const lowered: Ast.BridgePlan = switch (bridge) {
            .direct => .direct,
            .zst => .zst,
            .list_reinterpret => .list_reinterpret,
            .nominal_reinterpret => .nominal_reinterpret,
            .box_unbox => |child| .{ .box_unbox = try self.lowerBridgePlan(child) },
            .box_box => |child| .{ .box_box = try self.lowerBridgePlan(child) },
            .struct_ => |children| .{ .struct_ = try self.lowerBridgePlanSpan(children) },
            .tag_union => |children| .{ .tag_union = try self.lowerBridgePlanSpan(children) },
            .singleton_to_tag_union => |singleton| .{ .singleton_to_tag_union = .{
                .source_payload = try self.layoutForType(singleton.source_payload),
                .target_discriminant = singleton.target_discriminant,
                .payload_plan = if (singleton.payload_plan) |payload| try self.lowerBridgePlan(payload) else null,
            } },
            .tag_union_to_singleton => |singleton| .{ .tag_union_to_singleton = .{
                .target_payload = try self.layoutForType(singleton.target_payload),
                .source_discriminant = singleton.source_discriminant,
                .payload_plan = if (singleton.payload_plan) |payload| try self.lowerBridgePlan(payload) else null,
            } },
        };
        return try self.output.store.addBridgePlan(lowered);
    }

    fn lowerBridgePlanSpan(
        self: *IrBuilder,
        span: Exec.Ast.Span(Exec.Ast.BridgeId),
    ) LowerResourceError!Ast.Span(Ast.BridgePlanId) {
        const input_items = self.input.ast.sliceBridgePlanSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.BridgePlanId).empty();
        const lowered = try self.allocator.alloc(Ast.BridgePlanId, input_items.len);
        defer self.allocator.free(lowered);
        for (input_items, 0..) |bridge_id, i| {
            lowered[i] = try self.lowerBridgePlan(bridge_id);
        }
        return try self.output.store.addBridgePlanSpan(lowered);
    }

    fn constructionSlotBridge(
        self: *IrBuilder,
        source_ty: Exec.Type.TypeId,
        target_ty: Exec.Type.TypeId,
    ) LowerResourceError!Ast.BridgePlanId {
        if (source_ty == target_ty) {
            return switch (self.input.types.getType(source_ty)) {
                .placeholder => irInvariant("IR construction bridge saw placeholder type"),
                .link => irInvariant("IR construction bridge saw unresolved link type"),
                .primitive => try self.output.store.addBridgePlan(.direct),
                .nominal, .box, .callable_set, .erased_fn => try self.output.store.addBridgePlan(.nominal_reinterpret),
                .list => try self.output.store.addBridgePlan(.list_reinterpret),
                .tuple => |items| try self.output.store.addBridgePlan(.{ .struct_ = try self.constructionSlotStructBridge(items, items) }),
                .record => |record| try self.output.store.addBridgePlan(.{ .struct_ = try self.constructionSlotRecordBridge(record, record) }),
                .tag_union => |tag_union| try self.output.store.addBridgePlan(.{ .tag_union = try self.constructionSlotTagUnionBridge(tag_union, tag_union) }),
                .vacant_callable_slot => try self.output.store.addBridgePlan(.zst),
            };
        }

        const source = self.input.types.getType(source_ty);
        const target = self.input.types.getType(target_ty);
        const plan: Ast.BridgePlan = switch (source) {
            .placeholder => irInvariant("IR construction bridge saw placeholder source type"),
            .link => irInvariant("IR construction bridge saw unresolved source link"),
            .primitive => |source_prim| switch (target) {
                .primitive => |target_prim| blk: {
                    if (source_prim != target_prim) irInvariant("IR construction bridge crossed primitive types");
                    break :blk .direct;
                },
                else => irInvariant("IR construction bridge crossed primitive/non-primitive types"),
            },
            .nominal => |source_nominal| switch (target) {
                .nominal => |target_nominal| blk: {
                    if (source_nominal.nominal.module_name == target_nominal.nominal.module_name and
                        source_nominal.nominal.type_name == target_nominal.nominal.type_name)
                    {
                        break :blk .nominal_reinterpret;
                    }
                    irInvariant("IR construction bridge crossed distinct nominal types");
                },
                else => .nominal_reinterpret,
            },
            .list => switch (target) {
                .list => .list_reinterpret,
                .nominal => .nominal_reinterpret,
                else => irInvariant("IR construction bridge crossed list/non-list types"),
            },
            .box => switch (target) {
                .box, .nominal => .nominal_reinterpret,
                else => irInvariant("IR construction bridge crossed box/non-box types"),
            },
            .tuple => |source_items| switch (target) {
                .tuple => |target_items| .{ .struct_ = try self.constructionSlotStructBridge(source_items, target_items) },
                .nominal => .nominal_reinterpret,
                else => irInvariant("IR construction bridge crossed tuple/non-tuple types"),
            },
            .record => |source_record| switch (target) {
                .record => |target_record| .{ .struct_ = try self.constructionSlotRecordBridge(source_record, target_record) },
                .nominal => .nominal_reinterpret,
                else => irInvariant("IR construction bridge crossed record/non-record types"),
            },
            .tag_union => |source_union| switch (target) {
                .tag_union => |target_union| .{ .tag_union = try self.constructionSlotTagUnionBridge(source_union, target_union) },
                .nominal => .nominal_reinterpret,
                else => irInvariant("IR construction bridge crossed tag-union/non-tag-union types"),
            },
            .callable_set => |source_callable| switch (target) {
                .callable_set => |target_callable| blk: {
                    if (!repr.callableSetKeyEql(source_callable.key, target_callable.key)) {
                        irInvariant("IR construction bridge crossed callable-set keys");
                    }
                    break :blk .nominal_reinterpret;
                },
                .nominal => .nominal_reinterpret,
                else => irInvariant("IR construction bridge crossed callable-set/non-callable-set types"),
            },
            .erased_fn => switch (target) {
                .erased_fn => .nominal_reinterpret,
                else => irInvariant("IR construction bridge crossed erased-fn/non-erased-fn types"),
            },
            .vacant_callable_slot => switch (target) {
                .vacant_callable_slot => .zst,
                else => irInvariant("IR construction bridge crossed vacant/non-vacant callable-slot types"),
            },
        };
        return try self.output.store.addBridgePlan(plan);
    }

    fn constructionSlotStructBridge(
        self: *IrBuilder,
        source_items: []const Exec.Type.TypeId,
        target_items: []const Exec.Type.TypeId,
    ) LowerResourceError!Ast.Span(Ast.BridgePlanId) {
        if (source_items.len != target_items.len) irInvariant("IR construction struct bridge arity mismatch");
        if (source_items.len == 0) return Ast.Span(Ast.BridgePlanId).empty();

        const children = try self.allocator.alloc(Ast.BridgePlanId, source_items.len);
        defer self.allocator.free(children);
        for (source_items, target_items, 0..) |source, target, i| {
            children[i] = try self.constructionSlotBridge(source, target);
        }
        return try self.output.store.addBridgePlanSpan(children);
    }

    fn constructionSlotRecordBridge(
        self: *IrBuilder,
        source: Exec.Type.RecordType,
        target: Exec.Type.RecordType,
    ) LowerResourceError!Ast.Span(Ast.BridgePlanId) {
        if (source.fields.len != target.fields.len) irInvariant("IR construction record bridge arity mismatch");
        if (source.fields.len == 0) return Ast.Span(Ast.BridgePlanId).empty();

        const children = try self.allocator.alloc(Ast.BridgePlanId, source.fields.len);
        defer self.allocator.free(children);
        for (target.fields, 0..) |target_field, i| {
            const target_label = self.input.row_shapes.recordField(target_field.field).label;
            const source_field = self.recordFieldForLabel(source, target_label);
            children[i] = try self.constructionSlotBridge(source_field.ty, target_field.ty);
        }
        return try self.output.store.addBridgePlanSpan(children);
    }

    fn constructionSlotTagUnionBridge(
        self: *IrBuilder,
        source: Exec.Type.TagUnionType,
        target: Exec.Type.TagUnionType,
    ) LowerResourceError!Ast.Span(Ast.BridgePlanId) {
        if (source.tags.len != target.tags.len) irInvariant("IR construction tag-union bridge arity mismatch");
        if (source.tags.len == 0) return Ast.Span(Ast.BridgePlanId).empty();

        const children = try self.allocator.alloc(Ast.BridgePlanId, target.tags.len);
        defer self.allocator.free(children);
        for (target.tags, 0..) |target_tag, i| {
            const target_label = self.input.row_shapes.tag(target_tag.tag).label;
            const source_tag = self.tagTypeForLabel(source, target_label);
            children[i] = try self.constructionSlotTagPayloadBridge(source_tag, target_tag);
        }
        return try self.output.store.addBridgePlanSpan(children);
    }

    fn constructionSlotTagPayloadBridge(
        self: *IrBuilder,
        source: Exec.Type.TagType,
        target: Exec.Type.TagType,
    ) LowerResourceError!Ast.BridgePlanId {
        if (source.payloads.len != target.payloads.len) irInvariant("IR construction tag payload bridge arity mismatch");
        if (source.payloads.len == 0) return try self.output.store.addBridgePlan(.zst);
        if (source.payloads.len == 1) {
            return try self.constructionSlotBridge(source.payloads[0].ty, target.payloads[0].ty);
        }

        const source_payloads = try self.allocator.alloc(Exec.Type.TypeId, source.payloads.len);
        defer self.allocator.free(source_payloads);
        const target_payloads = try self.allocator.alloc(Exec.Type.TypeId, target.payloads.len);
        defer self.allocator.free(target_payloads);
        var source_seen = try self.allocator.alloc(bool, source.payloads.len);
        defer self.allocator.free(source_seen);
        var target_seen = try self.allocator.alloc(bool, target.payloads.len);
        defer self.allocator.free(target_seen);
        @memset(source_seen, false);
        @memset(target_seen, false);

        for (source.payloads) |payload| {
            const index: usize = @intCast(self.input.row_shapes.tagPayload(payload.payload).logical_index);
            if (index >= source_payloads.len) irInvariant("IR construction tag payload source index exceeded payload arity");
            if (source_seen[index]) irInvariant("IR construction tag payload bridge saw duplicate source payload");
            source_payloads[index] = payload.ty;
            source_seen[index] = true;
        }
        for (target.payloads) |payload| {
            const index: usize = @intCast(self.input.row_shapes.tagPayload(payload.payload).logical_index);
            if (index >= target_payloads.len) irInvariant("IR construction tag payload target index exceeded payload arity");
            if (target_seen[index]) irInvariant("IR construction tag payload bridge saw duplicate target payload");
            target_payloads[index] = payload.ty;
            target_seen[index] = true;
        }
        for (source_seen) |was_seen| {
            if (!was_seen) irInvariant("IR construction tag payload bridge omitted source payload");
        }
        for (target_seen) |was_seen| {
            if (!was_seen) irInvariant("IR construction tag payload bridge omitted target payload");
        }

        return try self.output.store.addBridgePlan(.{ .struct_ = try self.constructionSlotStructBridge(source_payloads, target_payloads) });
    }

    fn recordFieldForLabel(
        self: *IrBuilder,
        record: Exec.Type.RecordType,
        label: anytype,
    ) Exec.Type.RecordFieldType {
        for (record.fields) |field| {
            if (self.input.row_shapes.recordField(field.field).label == label) return field;
        }
        irInvariant("IR construction record bridge source field label is absent from source type");
    }

    fn tagTypeForLabel(
        self: *IrBuilder,
        tag_union: Exec.Type.TagUnionType,
        label: anytype,
    ) Exec.Type.TagType {
        for (tag_union.tags) |tag| {
            if (self.input.row_shapes.tag(tag.tag).label == label) return tag;
        }
        irInvariant("IR construction tag-union bridge source tag label is absent from source type");
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
        const node = try self.output.layouts.reserveNode(self.allocator);
        try self.fillStructLayoutNodeFromTypes(node, types);
        return .{ .local = node };
    }

    fn fillStructLayoutNodeFromTypes(
        self: *IrBuilder,
        node: Layout.NodeId,
        types: []const Exec.Type.TypeId,
    ) LowerResourceError!void {
        if (types.len == 0) irInvariant("IR lowering tried to fill empty struct layout node");
        const vars = try self.allocator.alloc(Ast.Var, types.len);
        defer self.allocator.free(vars);
        for (types, 0..) |ty, i| {
            vars[i] = .{
                .layout = try self.layoutForType(ty),
                .symbol = symbol_mod.Symbol.none,
            };
        }
        try self.fillStructLayoutNode(node, vars);
    }

    fn fillStructLayoutNode(
        self: *IrBuilder,
        node: Layout.NodeId,
        fields: []const Ast.Var,
    ) LowerResourceError!void {
        if (fields.len == 0) irInvariant("IR lowering tried to fill empty struct layout node");
        const graph_fields = try self.allocator.alloc(Layout.Field, fields.len);
        defer self.allocator.free(graph_fields);
        for (fields, 0..) |field, i| {
            graph_fields[i] = .{
                .index = @intCast(i),
                .child = field.layout,
            };
        }
        self.output.layouts.setNode(node, .{ .struct_ = try self.output.layouts.appendFields(self.allocator, graph_fields) });
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

    fn payloadStructLayoutFromUnionLayout(
        self: *IrBuilder,
        union_layout: Ast.LayoutRef,
        tag_id: mir.MonoRow.TagId,
    ) LowerResourceError!Ast.LayoutRef {
        return switch (union_layout) {
            .canonical => irInvariant("IR lowering tag payload projection expected a local tag-union layout"),
            .local => |node| switch (self.output.layouts.getNode(node)) {
                .tag_union => |span| blk: {
                    const tag_info = self.input.row_shapes.tag(tag_id);
                    const variants = self.output.layouts.getRefs(span);
                    if (tag_info.logical_index >= variants.len) {
                        irInvariant("IR lowering tag payload projection tag index exceeded union layout arity");
                    }
                    break :blk variants[@intCast(tag_info.logical_index)];
                },
                .nominal => |backing| try self.payloadStructLayoutFromUnionLayout(backing, tag_id),
                .pending => irInvariant("IR lowering tag payload projection reached pending union layout"),
                else => irInvariant("IR lowering tag payload projection expected tag-union source layout"),
            },
        };
    }

    fn recordLayout(
        self: *IrBuilder,
        node: Layout.NodeId,
        record: Exec.Type.RecordType,
    ) LowerResourceError!void {
        if (record.fields.len == 0) irInvariant("IR lowering tried to fill empty record layout node");
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
        self.output.layouts.setNode(node, .{ .struct_ = try self.output.layouts.appendFields(self.allocator, graph_fields) });
    }

    fn tagUnionLayout(
        self: *IrBuilder,
        node: Layout.NodeId,
        tag_union: Exec.Type.TagUnionType,
    ) LowerResourceError!void {
        if (tag_union.tags.len == 0) irInvariant("IR lowering tried to fill empty tag-union layout node");
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

        self.output.layouts.setNode(node, .{ .tag_union = try self.output.layouts.appendRefs(self.allocator, variants) });
    }

    fn callableSetLayout(
        self: *IrBuilder,
        node: Layout.NodeId,
        callable_set: Exec.Type.CallableSetType,
    ) LowerResourceError!void {
        if (callable_set.members.len == 0) irInvariant("IR lowering callable-set type had no members");
        const variants = try self.allocator.alloc(Ast.LayoutRef, callable_set.members.len);
        defer self.allocator.free(variants);
        var seen = try self.allocator.alloc(bool, callable_set.members.len);
        defer self.allocator.free(seen);
        @memset(seen, false);

        for (callable_set.members) |member| {
            const index: usize = @intCast(@intFromEnum(member.member));
            if (index >= callable_set.members.len) irInvariant("IR lowering callable-set member index exceeded member count");
            if (seen[index]) irInvariant("IR lowering callable-set type saw duplicate member index");
            variants[index] = if (member.payload_ty) |payload_ty| try self.layoutForType(payload_ty) else .{ .canonical = .zst };
            seen[index] = true;
        }
        for (seen) |was_seen| {
            if (!was_seen) irInvariant("IR lowering callable-set type did not provide every member payload layout");
        }

        self.output.layouts.setNode(node, .{ .tag_union = try self.output.layouts.appendRefs(self.allocator, variants) });
    }

    fn layoutForType(self: *IrBuilder, ty: Exec.Type.TypeId) LowerResourceError!Ast.LayoutRef {
        const resolved = self.resolveLayoutType(ty);
        if (self.layout_cache.get(resolved)) |existing| return existing;

        return switch (self.input.types.getType(resolved)) {
            .placeholder => irInvariant("IR lowering received executable placeholder type"),
            .link => irInvariant("IR lowering type resolver returned a link"),
            .primitive => |prim| .{ .canonical = primitiveLayout(prim) },
            .nominal => |nominal| try self.layoutForNominalType(nominal.source_ty.bytes, nominal.backing),
            .list => |elem| blk: {
                const node = try self.reserveCachedLayoutNode(resolved);
                const child = try self.layoutForType(elem);
                self.output.layouts.setNode(node, .{ .list = child });
                break :blk .{ .local = node };
            },
            .box => |elem| blk: {
                if (self.isErasedFnType(elem)) {
                    const layout_ref = try self.layoutForType(elem);
                    try self.layout_cache.put(resolved, layout_ref);
                    break :blk layout_ref;
                }
                const node = try self.reserveCachedLayoutNode(resolved);
                const child = try self.layoutForType(elem);
                self.output.layouts.setNode(node, .{ .box = child });
                break :blk .{ .local = node };
            },
            .tuple => |items| blk: {
                if (items.len == 0) break :blk .{ .canonical = .zst };
                const node = try self.reserveCachedLayoutNode(resolved);
                try self.fillStructLayoutNodeFromTypes(node, items);
                break :blk .{ .local = node };
            },
            .record => |record| blk: {
                if (record.fields.len == 0) break :blk .{ .canonical = .zst };
                const node = try self.reserveCachedLayoutNode(resolved);
                try self.recordLayout(node, record);
                break :blk .{ .local = node };
            },
            .tag_union => |tag_union| blk: {
                if (tag_union.tags.len == 0) break :blk .{ .canonical = .zst };
                const node = try self.reserveCachedLayoutNode(resolved);
                try self.tagUnionLayout(node, tag_union);
                break :blk .{ .local = node };
            },
            .callable_set => |callable_set| blk: {
                const node = try self.reserveCachedLayoutNode(resolved);
                try self.callableSetLayout(node, callable_set);
                break :blk .{ .local = node };
            },
            .erased_fn => |erased| blk: {
                const node = try self.reserveCachedLayoutNode(resolved);
                try self.erasedFnLayout(node, erased);
                break :blk .{ .local = node };
            },
            .vacant_callable_slot => .{ .canonical = .zst },
        };
    }

    fn layoutForNominalType(
        self: *IrBuilder,
        source_ty_bytes: [32]u8,
        backing: Exec.Type.TypeId,
    ) LowerResourceError!Ast.LayoutRef {
        if (isEmptyCanonicalTypeKeyBytes(source_ty_bytes)) {
            irInvariant("IR lowering nominal executable type was missing canonical source type identity");
        }
        if (self.nominal_layout_cache.get(source_ty_bytes)) |existing| return existing;

        const node = try self.output.layouts.reserveNode(self.allocator);
        const ref: Ast.LayoutRef = .{ .local = node };
        try self.nominal_layout_cache.put(source_ty_bytes, ref);
        errdefer _ = self.nominal_layout_cache.remove(source_ty_bytes);

        const backing_layout = try self.layoutForType(backing);
        self.output.layouts.setNode(node, .{ .nominal = backing_layout });
        return ref;
    }

    fn reserveCachedLayoutNode(
        self: *IrBuilder,
        ty: Exec.Type.TypeId,
    ) LowerResourceError!Layout.NodeId {
        const node = try self.output.layouts.reserveNode(self.allocator);
        try self.layout_cache.put(ty, .{ .local = node });
        return node;
    }

    fn resolveLayoutType(self: *const IrBuilder, ty: Exec.Type.TypeId) Exec.Type.TypeId {
        var current = ty;
        while (true) {
            switch (self.input.types.getType(current)) {
                .link => |next| current = next,
                else => return current,
            }
        }
    }

    fn erasedFnLayout(
        self: *IrBuilder,
        node: Layout.NodeId,
        _: Exec.Type.ErasedFnType,
    ) LowerResourceError!void {
        self.output.layouts.setNode(node, .erased_callable);
    }

    fn isErasedFnType(self: *IrBuilder, ty: Exec.Type.TypeId) bool {
        const resolved = self.resolveLayoutType(ty);
        return switch (self.input.types.getType(resolved)) {
            .link => unreachable,
            .nominal => |nominal| self.isErasedFnType(nominal.backing),
            .erased_fn => true,
            else => false,
        };
    }

    fn listElementType(self: *IrBuilder, ty: Exec.Type.TypeId) Exec.Type.TypeId {
        return switch (self.input.types.getType(ty)) {
            .link => |next| self.listElementType(next),
            .list => |elem| elem,
            else => irInvariant("IR lowering for_list expected iterable expression to have List(T) type"),
        };
    }

    fn publishRequestedLayouts(
        self: *IrBuilder,
        request_keys: []const repr.CanonicalExecValueTypeKey,
    ) LowerResourceError!void {
        if (request_keys.len == 0) return;
        try self.output.requested_layouts.ensureUnusedCapacity(self.allocator, request_keys.len);
        for (request_keys) |key| {
            const ty = self.input.lowered_session_types_by_key.get(key) orelse {
                irInvariant("IR lowering requested a layout for an unpublished executable type key");
            };
            const layout = try self.layoutForType(ty);
            self.output.requested_layouts.appendAssumeCapacity(.{
                .key = key,
                .layout = layout,
            });
        }
    }
};

fn isEmptyCanonicalTypeKeyBytes(bytes: [32]u8) bool {
    return std.mem.allEqual(u8, bytes[0..], 0);
}

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

fn lowLevelReturnsPredicate(op: base.LowLevel) bool {
    return switch (op) {
        .str_is_eq,
        .str_contains,
        .str_caseless_ascii_equals,
        .str_starts_with,
        .str_ends_with,
        .num_is_eq,
        .num_is_gt,
        .num_is_gte,
        .num_is_lt,
        .num_is_lte,
        => true,
        else => false,
    };
}

fn irInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) std.debug.panic(message, .{});
    unreachable;
}

test "IR lowering consumes executable MIR only" {
    std.testing.refAllDecls(@This());
}
