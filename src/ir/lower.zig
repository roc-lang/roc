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

pub const LowerResourceError = Allocator.Error;

pub const Program = struct {
    allocator: Allocator,
    canonical_names: mir.Hosted.CanonicalNameStore,
    literal_pool: mir.Ids.ProgramLiteralPool,
    symbols: symbol_mod.Store,
    store: Ast.Store,
    layouts: Layout.Graph,
    root_procs: std.ArrayList(Ast.ProcRef),

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .canonical_names = mir.Hosted.CanonicalNameStore.init(allocator),
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
        self.canonical_names.deinit();
        self.* = Program.init(self.allocator);
    }
};

pub fn fromExecutable(allocator: Allocator, executable: mir.Executable.Build.Program) LowerResourceError!Program {
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
        .expr_map = std.AutoHashMap(Exec.Ast.ExprId, Ast.Var).init(allocator),
        .proc_def_index = std.AutoHashMap(Exec.Ast.ExecutableProcId, usize).init(allocator),
        .next_internal_value_ref = input.ast.next_value_ref,
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
    proc_def_index: std.AutoHashMap(Exec.Ast.ExecutableProcId, usize),
    next_internal_value_ref: u32,

    fn deinit(self: *IrBuilder) void {
        self.proc_def_index.deinit();
        self.expr_map.deinit();
        self.value_env.deinit();
    }

    fn lowerAllDefs(self: *IrBuilder) LowerResourceError!void {
        try self.buildProcDefIndex();
        for (self.input.ast.defs.items) |def| {
            self.value_env.clearRetainingCapacity();
            self.expr_map.clearRetainingCapacity();
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
            .structural_eq => |eq| try self.lowerStructuralEq(expr, eq, stmts),
            .callable_set_value => |callable| try self.lowerCallableSetValue(expr, callable, stmts),
            .callable_match => |callable_match| try self.lowerCallableMatch(expr, callable_match, stmts),
            .source_match => |source_match| try self.lowerSourceMatch(expr, source_match, stmts),
            .for_ => |for_| try self.lowerForExpr(expr, for_, stmts),
            .while_ => |while_| try self.lowerWhileExpr(expr, while_, stmts),
            .bridge => |bridge_expr| blk: {
                const value = self.value_env.get(bridge_expr.value) orelse irInvariant("IR lowering bridge source value was not bound");
                const bridge_plan = try self.lowerBridgePlan(bridge_expr.bridge);
                break :blk try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .bridge = .{
                    .value = value,
                    .plan = bridge_plan,
                } }, stmts);
            },
            .packed_erased_fn => |packed| try self.lowerPackedErasedFn(expr, packed, stmts),
            .call_erased => |call| try self.lowerCallErased(expr, call, stmts),
            .const_ref,
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
        const args = try self.lowerDirectCallArgSpan(call.executable_proc, call.direct_args, stmts);
        defer if (args.len > 0) self.allocator.free(args);
        const direct_call: Ast.Expr = .{ .call_direct = .{
            .proc = call.executable_proc,
            .args = try self.output.store.addVarSpan(args),
        } };
        if (call.result_bridge) |result_bridge| {
            const raw_result = try self.bindAnonymous(try self.procReturnLayout(call.executable_proc), direct_call, stmts);
            return try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .bridge = .{
                .value = raw_result,
                .plan = try self.lowerBridgePlan(result_bridge),
            } }, stmts);
        }
        return try self.bindExpr(expr.value, try self.layoutForType(expr.ty), direct_call, stmts);
    }

    fn lowerCallErased(
        self: *IrBuilder,
        expr: Exec.Ast.Expr,
        call: anytype,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const sig_has_capture = call.sig_key.capture_ty != null;
        const type_has_capture = call.capture_ty != null;
        if (sig_has_capture != type_has_capture) {
            irInvariant("IR lowering call_erased hidden capture type disagrees with erased signature");
        }
        const func = self.value_env.get(call.func) orelse irInvariant("IR lowering call_erased function value was not bound");
        const args = try self.lowerVarSpanFromValueRefSpan(call.args);
        defer if (args.len > 0) self.allocator.free(args);
        const capture_layout = if (call.capture_ty) |capture_ty| try self.layoutForType(capture_ty) else null;
        return try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .call_erased = .{
            .func = func,
            .args = try self.output.store.addVarSpan(args),
            .capture_layout = capture_layout,
        } }, stmts);
    }

    fn lowerPackedErasedFn(
        self: *IrBuilder,
        expr: Exec.Ast.Expr,
        packed: Exec.Ast.PackedErasedFn,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const field_count: usize = if (packed.capture_ty == null) 1 else 2;
        const fields = try self.allocator.alloc(Ast.Var, field_count);
        defer self.allocator.free(fields);

        const fn_ptr = try self.bindAnonymous(.{ .canonical = .opaque_ptr }, .{ .fn_ptr = packed.code }, stmts);
        fields[0] = fn_ptr;

        if (packed.capture_ty) |capture_ty| {
            const capture_ref = packed.capture orelse irInvariant("IR lowering packed erased fn has capture type but no capture value");
            const capture = self.value_env.get(capture_ref) orelse irInvariant("IR lowering packed erased fn capture value was not bound");
            const capture_layout = try self.layoutForType(capture_ty);
            const capture_box_layout = try self.boxLayout(capture_layout);
            const capture_box = try self.freshVar(capture_box_layout);
            const direct = try self.output.store.addBridgePlan(.direct);
            const box_plan = try self.output.store.addBridgePlan(.{ .box_box = direct });
            try stmts.append(self.allocator, try self.output.store.addStmt(.{ .let_ = .{
                .bind = capture_box,
                .expr = try self.output.store.addExpr(.{ .bridge = .{
                    .value = capture,
                    .plan = box_plan,
                } }),
            } }));
            fields[1] = capture_box;
        } else if (packed.capture != null) {
            irInvariant("IR lowering packed erased fn has capture value but no capture type");
        }

        return try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{
            .make_struct = try self.output.store.addVarSpan(fields),
        }, stmts);
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
            const bind = try self.bindExpr(record.record_tmp, layout, .{
                .make_struct = try self.output.store.addVarSpan(fields),
            }, stmts);
            payload = bind;
        }

        return try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .make_union = .{
            .discriminant = @intCast(@intFromEnum(callable.member.member_index)),
            .payload = payload,
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
        return try self.bindExpr(expr.value, try self.layoutForType(expr.ty), .{ .structural_eq = .{
            .lhs = lhs,
            .rhs = rhs,
        } }, stmts);
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
            .{ .get_union_id = callee },
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
                .block = try self.lowerCallableMatchBranchBlock(branch, callee, callable_match.result_ty),
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
        result_ty: Exec.Type.TypeId,
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

        const args = try self.lowerDirectCallArgSpan(branch.executable_proc, branch.direct_args, &stmts);
        defer if (args.len > 0) self.allocator.free(args);
        const direct_call: Ast.Expr = .{ .call_direct = .{
            .proc = branch.executable_proc,
            .args = try self.output.store.addVarSpan(args),
        } };
        const raw_result = try self.bindAnonymous(try self.procReturnLayout(branch.executable_proc), direct_call, &stmts);
        const result = if (branch.result_bridge) |result_bridge|
            try self.bindAnonymous(try self.layoutForType(result_ty), .{ .bridge = .{
                .value = raw_result,
                .plan = try self.lowerBridgePlan(result_bridge),
            } }, &stmts)
        else
            raw_result;

        return try self.output.store.addBlock(.{
            .stmts = try self.output.store.addStmtSpan(stmts.items),
            .term = .{ .value = result },
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
        if (scrutinee_exprs.len != 1 or scrutinee_values.len != 1) {
            irInvariant("IR lowering source_match requires executable decision-plan lowering for multi-scrutinee matches");
        }

        const scrutinee = try self.lowerExpr(scrutinee_exprs[0], stmts);
        const subject = try self.sourceMatchSwitchSubject(scrutinee, source_match.branches, stmts);
        const branch_ids = self.input.ast.branch_ids.items[source_match.branches.start..][0..source_match.branches.len];
        if (branch_ids.len == 0) irInvariant("IR lowering source_match received no branches");

        const result = try self.freshVar(try self.layoutForType(expr.ty));
        try self.appendSourceMatchBranchSwitch(branch_ids, 0, scrutinee, subject, result, stmts);
        try self.value_env.put(expr.value, result);
        return result;
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
            .{ .make_struct = Ast.Span(Ast.Var).empty() },
            stmts,
        );
    }

    fn lowerWhileExpr(
        self: *IrBuilder,
        expr: Exec.Ast.Expr,
        while_: anytype,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        try self.appendWhile(while_, stmts);
        return try self.bindExpr(
            expr.value,
            .{ .canonical = .zst },
            .{ .make_struct = Ast.Span(Ast.Var).empty() },
            stmts,
        );
    }

    fn appendWhile(
        self: *IrBuilder,
        while_: anytype,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!void {
        try stmts.append(self.allocator, try self.output.store.addStmt(.{ .while_ = .{
            .cond = try self.lowerExprToBlock(while_.cond),
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
        try self.bindSourceMatchPatternValues(pat, elem, &body_stmts, &saved);
        const result = try self.lowerExpr(body, &body_stmts);
        return try self.output.store.addBlock(.{
            .stmts = try self.output.store.addStmtSpan(body_stmts.items),
            .term = .{ .value = result },
        });
    }

    fn sourceMatchSwitchSubject(
        self: *IrBuilder,
        scrutinee: Ast.Var,
        branches: Exec.Ast.Span(Exec.Ast.BranchId),
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!Ast.Var {
        const branch_ids = self.input.ast.branch_ids.items[branches.start..][0..branches.len];
        var needs_discriminant = false;
        for (branch_ids) |branch_id| {
            const branch = self.input.ast.branches.items[@intFromEnum(branch_id)];
            const pat = self.input.ast.pats.items[@intFromEnum(branch.pat)];
            if (self.sourceMatchPatternNeedsDiscriminant(pat)) needs_discriminant = true;
        }
        if (!needs_discriminant) return scrutinee;

        for (branch_ids) |branch_id| {
            const branch = self.input.ast.branches.items[@intFromEnum(branch_id)];
            const pat = self.input.ast.pats.items[@intFromEnum(branch.pat)];
            if (!self.sourceMatchPatternCanUseTagSubject(pat)) irInvariant("IR lowering source_match cannot mix tag tests with non-tag literal tests");
        }

        return try self.bindExpr(
            self.freshInternalValueRef(),
            .{ .canonical = .u16 },
            .{ .get_union_id = scrutinee },
            stmts,
        );
    }

    fn sourceMatchPatternNeedsDiscriminant(self: *IrBuilder, pat: Exec.Ast.Pat) bool {
        return switch (pat.data) {
            .as => |as| self.sourceMatchPatternNeedsDiscriminant(self.input.ast.pats.items[@intFromEnum(as.pattern)]),
            .nominal => |child| self.sourceMatchPatternNeedsDiscriminant(self.input.ast.pats.items[@intFromEnum(child)]),
            .tag => true,
            .wildcard, .bind, .bool_lit, .int_lit => false,
            else => irInvariant("IR lowering source_match needs full pattern-decision lowering for this pattern form"),
        };
    }

    fn sourceMatchPatternCanUseTagSubject(self: *IrBuilder, pat: Exec.Ast.Pat) bool {
        return switch (pat.data) {
            .as => |as| self.sourceMatchPatternCanUseTagSubject(self.input.ast.pats.items[@intFromEnum(as.pattern)]),
            .nominal => |child| self.sourceMatchPatternCanUseTagSubject(self.input.ast.pats.items[@intFromEnum(child)]),
            .tag, .wildcard, .bind => true,
            else => false,
        };
    }

    fn sourceMatchPatternSwitchValue(self: *IrBuilder, pat: Exec.Ast.Pat) ?u64 {
        return switch (pat.data) {
            .as => |as| self.sourceMatchPatternSwitchValue(self.input.ast.pats.items[@intFromEnum(as.pattern)]),
            .nominal => |child| self.sourceMatchPatternSwitchValue(self.input.ast.pats.items[@intFromEnum(child)]),
            .wildcard, .bind => null,
            .tag => |tag| @intCast(self.input.row_shapes.tag(tag.tag).logical_index),
            .bool_lit => |value| @as(u64, if (value) 1 else 0),
            .int_lit => |value| blk: {
                if (value < 0) irInvariant("IR lowering source_match needs signed-int pattern lowering for negative literals");
                if (value > std.math.maxInt(u64)) irInvariant("IR lowering source_match needs large-int pattern lowering");
                break :blk @intCast(value);
            },
            else => irInvariant("IR lowering source_match needs full pattern-decision lowering for this pattern form"),
        };
    }

    const SavedValueBinding = struct {
        value: Exec.Ast.ExecutableValueRef,
        previous: ?Ast.Var,
    };

    fn appendSourceMatchBranchSwitch(
        self: *IrBuilder,
        branch_ids: []const Exec.Ast.BranchId,
        index: usize,
        scrutinee: Ast.Var,
        subject: Ast.Var,
        result: Ast.Var,
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError!void {
        if (index >= branch_ids.len) irInvariant("IR lowering source_match exhausted branches while emitting ordered cascade");

        const branch = self.input.ast.branches.items[@intFromEnum(branch_ids[index])];
        const pat = self.input.ast.pats.items[@intFromEnum(branch.pat)];
        const body = try self.lowerSourceMatchBranchBlock(branch, scrutinee);

        if (self.sourceMatchPatternSwitchValue(pat)) |value| {
            const branches = [_]Ast.Branch{.{
                .value = value,
                .block = body,
            }};
            try stmts.append(self.allocator, try self.output.store.addStmt(.{ .switch_ = .{
                .cond = subject,
                .branches = try self.output.store.addBranchSpan(&branches),
                .default_block = try self.sourceMatchBranchCascadeBlock(branch_ids, index + 1, scrutinee, subject, result),
                .join = result,
            } }));
        } else {
            try stmts.append(self.allocator, try self.output.store.addStmt(.{ .switch_ = .{
                .cond = subject,
                .branches = Ast.Span(Ast.BranchId).empty(),
                .default_block = body,
                .join = result,
            } }));
        }
    }

    fn sourceMatchBranchCascadeBlock(
        self: *IrBuilder,
        branch_ids: []const Exec.Ast.BranchId,
        index: usize,
        scrutinee: Ast.Var,
        subject: Ast.Var,
        result: Ast.Var,
    ) LowerResourceError!Ast.BlockId {
        if (index >= branch_ids.len) {
            return try self.output.store.addBlock(.{
                .stmts = Ast.Span(Ast.StmtId).empty(),
                .term = .@"unreachable",
            });
        }

        var stmts = std.ArrayList(Ast.StmtId).empty;
        defer stmts.deinit(self.allocator);
        try self.appendSourceMatchBranchSwitch(branch_ids, index, scrutinee, subject, result, &stmts);
        return try self.output.store.addBlock(.{
            .stmts = try self.output.store.addStmtSpan(stmts.items),
            .term = .{ .value = result },
        });
    }

    fn lowerSourceMatchBranchBlock(
        self: *IrBuilder,
        branch: Exec.Ast.Branch,
        scrutinee: Ast.Var,
    ) LowerResourceError!Ast.BlockId {
        var saved = std.ArrayList(SavedValueBinding).empty;
        defer {
            self.restoreValueBindings(saved.items);
            saved.deinit(self.allocator);
        }

        var branch_stmts = std.ArrayList(Ast.StmtId).empty;
        defer branch_stmts.deinit(self.allocator);
        const pat = self.input.ast.pats.items[@intFromEnum(branch.pat)];
        try self.bindSourceMatchPatternValues(pat, scrutinee, &branch_stmts, &saved);
        const result = try self.lowerExpr(branch.body, &branch_stmts);
        return try self.output.store.addBlock(.{
            .stmts = try self.output.store.addStmtSpan(branch_stmts.items),
            .term = .{ .value = result },
        });
    }

    fn bindSourceMatchPatternValues(
        self: *IrBuilder,
        pat: Exec.Ast.Pat,
        value: Ast.Var,
        stmts: *std.ArrayList(Ast.StmtId),
        saved: *std.ArrayList(SavedValueBinding),
    ) LowerResourceError!void {
        switch (pat.data) {
            .wildcard,
            .bool_lit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            => {},
            .as => |as| {
                try self.pushValueBinding(as.bind, value, saved);
                const child_pat = self.input.ast.pats.items[@intFromEnum(as.pattern)];
                try self.bindSourceMatchPatternValues(child_pat, value, stmts, saved);
            },
            .bind => |bind| try self.pushValueBinding(bind, value, saved),
            .nominal => |child| {
                const child_pat = self.input.ast.pats.items[@intFromEnum(child)];
                try self.bindSourceMatchPatternValues(child_pat, value, stmts, saved);
            },
            .tuple => |items| {
                const child_pats = self.input.ast.pat_ids.items[items.start..][0..items.len];
                for (child_pats, 0..) |child_pat_id, i| {
                    const child_pat = self.input.ast.pats.items[@intFromEnum(child_pat_id)];
                    const direct = try self.output.store.addBridgePlan(.direct);
                    const child_value = try self.bindExpr(
                        self.freshInternalValueRef(),
                        try self.layoutForType(child_pat.ty),
                        .{ .get_struct_field = .{
                            .record = value,
                            .field_index = @intCast(i),
                            .field_bridge_plan = direct,
                        } },
                        stmts,
                    );
                    try self.bindSourceMatchPatternValues(child_pat, child_value, stmts, saved);
                }
            },
            .record => |record| {
                if (record.rest != null) irInvariant("IR lowering record rest pattern requires executable decision-plan materialization");
                const field_patterns = self.input.ast.record_field_patterns.items[record.fields.start..][0..record.fields.len];
                for (field_patterns) |field_pattern| {
                    const child_pat = self.input.ast.pats.items[@intFromEnum(field_pattern.pattern)];
                    const field = self.input.row_shapes.recordField(field_pattern.field);
                    const direct = try self.output.store.addBridgePlan(.direct);
                    const child_value = try self.bindExpr(
                        self.freshInternalValueRef(),
                        try self.layoutForType(child_pat.ty),
                        .{ .get_struct_field = .{
                            .record = value,
                            .field_index = @intCast(field.logical_index),
                            .field_bridge_plan = direct,
                        } },
                        stmts,
                    );
                    try self.bindSourceMatchPatternValues(child_pat, child_value, stmts, saved);
                }
            },
            .tag => |tag| {
                const payload_ids = self.input.ast.tag_payload_patterns.items[tag.payloads.start..][0..tag.payloads.len];
                if (payload_ids.len == 0) return;

                const payload_record = try self.bindExpr(
                    self.freshInternalValueRef(),
                    try self.payloadStructLayoutForTag(pat.ty, tag.tag),
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
                                .field_bridge_plan = try self.output.store.addBridgePlan(.direct),
                            } },
                            stmts,
                        );
                    };
                    try self.bindSourceMatchPatternValues(child_pat, payload_value, stmts, saved);
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
            .return_ => |value_ref| {
                const value = self.value_env.get(value_ref) orelse irInvariant("IR lowering reached return value before it was bound");
                try stmts.append(self.allocator, try self.output.store.addStmt(.{ .return_ = value }));
            },
            .for_ => |for_| try self.appendForList(for_, stmts),
            .while_ => |while_| try self.appendWhile(while_, stmts),
            .break_ => try stmts.append(self.allocator, try self.output.store.addStmt(.break_)),
            .crash,
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
        proc: Exec.Ast.ExecutableProcId,
        span: Exec.Ast.Span(Exec.Ast.DirectCallArg),
        stmts: *std.ArrayList(Ast.StmtId),
    ) LowerResourceError![]const Ast.Var {
        if (span.len == 0) return &.{};
        const input_items = self.input.ast.direct_call_args.items[span.start..][0..span.len];
        const values = try self.allocator.alloc(Ast.Var, input_items.len);
        for (input_items, 0..) |arg, i| {
            const source = self.value_env.get(arg.value) orelse irInvariant("IR lowering direct call argument value was not bound");
            values[i] = if (arg.bridge) |bridge| blk: {
                break :blk try self.bindAnonymous(
                    try self.procArgLayout(proc, i),
                    .{ .bridge = .{
                        .value = source,
                        .plan = try self.lowerBridgePlan(bridge),
                    } },
                    stmts,
                );
            } else source;
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

    fn procDef(self: *const IrBuilder, proc: Exec.Ast.ExecutableProcId) Exec.Ast.Def {
        const index = self.proc_def_index.get(proc) orelse irInvariant("IR lowering referenced executable proc with no definition");
        return self.input.ast.defs.items[index];
    }

    fn procArgLayout(
        self: *IrBuilder,
        proc: Exec.Ast.ExecutableProcId,
        arg_index: usize,
    ) LowerResourceError!Ast.LayoutRef {
        const def = self.procDef(proc);
        const args = switch (def.value) {
            .fn_ => |fn_| self.input.ast.typed_values.items[fn_.args.start..][0..fn_.args.len],
            .hosted_fn => |hosted| self.input.ast.typed_values.items[hosted.args.start..][0..hosted.args.len],
        };
        if (arg_index >= args.len) irInvariant("IR lowering direct call argument exceeded target procedure arity");
        return try self.layoutForType(args[arg_index].ty);
    }

    fn procReturnLayout(
        self: *IrBuilder,
        proc: Exec.Ast.ExecutableProcId,
    ) LowerResourceError!Ast.LayoutRef {
        const def = self.procDef(proc);
        return switch (def.value) {
            .fn_ => |fn_| try self.layoutForType(self.input.ast.getExpr(fn_.body).ty),
            .hosted_fn => |hosted| try self.layoutForType(hosted.ret_ty),
        };
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

    fn boxLayout(self: *IrBuilder, child: Ast.LayoutRef) LowerResourceError!Ast.LayoutRef {
        const node = try self.output.layouts.reserveNode(self.allocator);
        self.output.layouts.setNode(node, .{ .box = child });
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

    fn callableSetLayout(self: *IrBuilder, callable_set: Exec.Type.CallableSetType) LowerResourceError!Ast.LayoutRef {
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
                break :blk try self.boxLayout(child);
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
            .callable_set => |callable_set| try self.callableSetLayout(callable_set),
            .erased_fn => |erased| try self.erasedFnLayout(erased),
        };
    }

    fn erasedFnLayout(self: *IrBuilder, erased: Exec.Type.ErasedFnType) LowerResourceError!Ast.LayoutRef {
        const field_count: usize = if (erased.capture_ty == null) 1 else 2;
        const fields = try self.allocator.alloc(Ast.Var, field_count);
        defer self.allocator.free(fields);

        fields[0] = .{
            .layout = .{ .canonical = .opaque_ptr },
            .symbol = symbol_mod.Symbol.none,
        };
        if (erased.capture_ty) |capture_ty| {
            fields[1] = .{
                .layout = try self.boxLayout(try self.layoutForType(capture_ty)),
                .symbol = symbol_mod.Symbol.none,
            };
        }
        return try self.structLayout(fields);
    }

    fn listElementType(self: *IrBuilder, ty: Exec.Type.TypeId) Exec.Type.TypeId {
        return switch (self.input.types.getType(ty)) {
            .link => |next| self.listElementType(next),
            .list => |elem| elem,
            else => irInvariant("IR lowering for_list expected iterable expression to have List(T) type"),
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
