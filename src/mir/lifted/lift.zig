//! Lambda lifting state for row-finalized mono MIR.

const std = @import("std");
const check = @import("check");
const symbol_mod = @import("symbol");
const MonoRow = @import("../mono_row/mod.zig");
const ids = @import("../ids.zig");

const Ast = @import("ast.zig");
const Type = @import("type.zig");

const Allocator = std.mem.Allocator;
const canonical = check.CanonicalNames;
const Symbol = symbol_mod.Symbol;

pub const ProcOrderKey = struct {
    ordinal: u32,
};

pub const LiftedGroupMember = struct {
    source_symbol: Symbol,
    lifted_proc: canonical.ProcedureValueRef,
    order_key: ProcOrderKey,
    args: Ast.Span(Ast.TypedSymbol),
    capture_slots: Ast.Span(Ast.CaptureSlot),
};

pub const CaptureValueEdge = struct {
    from_proc: canonical.ProcedureValueRef,
    source_symbol: Symbol,
    source_ty: Type.TypeId,
};

pub const CaptureProcValueEdge = struct {
    from_proc: canonical.ProcedureValueRef,
    referenced_proc: canonical.ProcedureValueRef,
};

pub const LiftedCaptureGraph = struct {
    members: []const LiftedGroupMember = &.{},
    value_edges: []const CaptureValueEdge = &.{},
    proc_value_edges: []const CaptureProcValueEdge = &.{},
};

pub const Proc = struct {
    proc: canonical.MonoSpecializedProcRef,
    order_key: ProcOrderKey,
    body: Ast.DefId,
};

pub const Program = struct {
    allocator: Allocator,
    literal_pool: ids.ProgramLiteralPool,
    symbols: symbol_mod.Store,
    row_shapes: MonoRow.Store,
    types: Type.Store,
    ast: Ast.Store,
    procs: std.ArrayList(Proc),
    root_procs: std.ArrayList(canonical.MonoSpecializedProcRef),

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .literal_pool = ids.ProgramLiteralPool.init(allocator),
            .symbols = symbol_mod.Store.init(allocator),
            .row_shapes = MonoRow.Store.init(allocator),
            .types = Type.Store.init(allocator),
            .ast = Ast.Store.init(allocator),
            .procs = .empty,
            .root_procs = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        self.root_procs.deinit(self.allocator);
        self.procs.deinit(self.allocator);
        self.ast.deinit();
        self.types.deinit();
        self.row_shapes.deinit();
        self.symbols.deinit();
        self.literal_pool.deinit();
        self.* = Program.init(self.allocator);
    }
};

pub fn run(allocator: Allocator, row_result: MonoRow.Result) Allocator.Error!Program {
    var input = row_result;
    errdefer input.deinit();

    var program = Program.init(allocator);
    errdefer program.deinit();
    program.types = input.program.types;
    input.program.types = Type.Store.init(allocator);
    program.literal_pool = input.program.literal_pool;
    input.program.literal_pool = ids.ProgramLiteralPool.init(allocator);
    program.symbols = input.program.symbols;
    input.program.symbols = symbol_mod.Store.init(allocator);
    program.row_shapes = input.shapes;
    input.shapes = MonoRow.Store.init(allocator);

    var lifter = BodyLifter{
        .allocator = allocator,
        .input = &input.program.ast,
        .output = &program.ast,
        .expr_map = std.AutoHashMap(MonoRow.Ast.ExprId, Ast.ExprId).init(allocator),
    };
    defer lifter.deinit();

    try program.procs.ensureTotalCapacity(allocator, input.program.procs.items.len);
    for (input.program.procs.items, 0..) |proc, i| {
        program.procs.appendAssumeCapacity(.{
            .proc = proc.proc,
            .order_key = .{ .ordinal = @intCast(i) },
            .body = try lifter.lowerDef(proc.body),
        });
    }
    try program.root_procs.appendSlice(allocator, input.program.root_procs.items);

    input.deinit();
    return program;
}

const BodyLifter = struct {
    allocator: Allocator,
    input: *const MonoRow.Ast.Store,
    output: *Ast.Store,
    expr_map: std.AutoHashMap(MonoRow.Ast.ExprId, Ast.ExprId),

    fn deinit(self: *BodyLifter) void {
        self.expr_map.deinit();
    }

    fn lowerDef(self: *BodyLifter, def_id: MonoRow.Ast.DefId) Allocator.Error!Ast.DefId {
        const def = self.input.getDef(def_id);
        return try self.output.addDef(.{
            .proc = def.proc,
            .debug_name = def.debug_name,
            .value = switch (def.value) {
                .fn_ => |fn_| .{ .fn_ = .{
                    .args = try self.lowerTypedSymbolSpan(fn_.args),
                    .captures = Ast.Span(Ast.CaptureSlot).empty(),
                    .body = try self.lowerExpr(fn_.body),
                } },
                .hosted_fn => |hosted| .{ .hosted_fn = .{
                    .proc = hosted.proc,
                    .args = try self.lowerTypedSymbolSpan(hosted.args),
                    .hosted = hosted.hosted,
                } },
                .val => |expr| .{ .val = try self.lowerExpr(expr) },
                .run => |run| .{ .run = .{ .body = try self.lowerExpr(run.body) } },
            },
        });
    }

    fn lowerExpr(self: *BodyLifter, expr_id: MonoRow.Ast.ExprId) Allocator.Error!Ast.ExprId {
        if (self.expr_map.get(expr_id)) |existing| return existing;

        const expr = self.input.getExpr(expr_id);
        const lowered = try self.output.addExpr(expr.ty, switch (expr.data) {
            .var_ => |symbol| .{ .var_ = symbol },
            .int_lit => |value| .{ .int_lit = value },
            .frac_f32_lit => |value| .{ .frac_f32_lit = value },
            .frac_f64_lit => |value| .{ .frac_f64_lit = value },
            .dec_lit => |value| .{ .dec_lit = value },
            .bool_lit => |value| .{ .bool_lit = value },
            .str_lit => |literal| .{ .str_lit = literal },
            .const_ref => |const_ref| .{ .const_ref = const_ref },
            .tag => |tag| .{ .tag = .{
                .union_shape = tag.union_shape,
                .tag = tag.tag,
                .eval_order = try self.lowerTagPayloadEvalSpan(tag.eval_order),
                .assembly_order = try self.lowerTagPayloadAssemblySpan(tag.assembly_order),
                .constructor_ty = tag.constructor_ty,
            } },
            .record => |record| .{ .record = .{
                .shape = record.shape,
                .eval_order = try self.lowerRecordFieldEvalSpan(record.eval_order),
                .assembly_order = try self.lowerRecordFieldAssemblySpan(record.assembly_order),
            } },
            .access => |access| .{ .access = .{
                .record = try self.lowerExpr(access.record),
                .field = access.field,
            } },
            .structural_eq => |eq| .{ .structural_eq = .{
                .lhs = try self.lowerExpr(eq.lhs),
                .rhs = try self.lowerExpr(eq.rhs),
            } },
            .bool_not => |child| .{ .bool_not = try self.lowerExpr(child) },
            .clos => liftInvariant("lifted MIR must not receive bare closure expressions after closure lifting is implemented"),
            .call_value => |call| .{ .call_value = .{
                .func = try self.lowerExpr(call.func),
                .args = try self.lowerExprSpan(call.args),
                .requested_fn_ty = call.requested_fn_ty,
            } },
            .call_proc => |call| .{ .call_proc = .{
                .proc = call.proc,
                .args = try self.lowerExprSpan(call.args),
                .requested_fn_ty = call.requested_fn_ty,
            } },
            .proc_value => |proc_value| .{ .proc_value = .{
                .proc = proc_value.proc,
                .captures = try self.lowerCaptureArgSpan(proc_value.captures),
                .fn_ty = proc_value.fn_ty,
            } },
            .inspect => |child| .{ .inspect = try self.lowerExpr(child) },
            .low_level => |low_level| .{ .low_level = .{
                .op = low_level.op,
                .args = try self.lowerExprSpan(low_level.args),
                .source_constraint_ty = low_level.source_constraint_ty,
            } },
            .block => |block| .{ .block = .{
                .stmts = try self.lowerStmtSpan(block.stmts),
                .final_expr = try self.lowerExpr(block.final_expr),
            } },
            .tuple => |items| .{ .tuple = try self.lowerExprSpan(items) },
            .tag_payload => |payload| .{ .tag_payload = .{
                .tag_union = try self.lowerExpr(payload.tag_union),
                .payload = payload.payload,
            } },
            .tuple_access => |access| .{ .tuple_access = .{
                .tuple = try self.lowerExpr(access.tuple),
                .elem_index = access.elem_index,
            } },
            .list => |items| .{ .list = try self.lowerExprSpan(items) },
            .unit => .unit,
            .return_ => |child| .{ .return_ = try self.lowerExpr(child) },
            .crash => |literal| .{ .crash = literal },
            .runtime_error => .runtime_error,
            .match_ => |match_| .{ .match_ = .{
                .cond = try self.lowerExpr(match_.cond),
                .branches = try self.lowerBranchSpan(match_.branches),
                .is_try_suffix = match_.is_try_suffix,
            } },
            .if_ => |if_| .{ .if_ = .{
                .cond = try self.lowerExpr(if_.cond),
                .then_body = try self.lowerExpr(if_.then_body),
                .else_body = try self.lowerExpr(if_.else_body),
            } },
            .for_ => |for_| .{ .for_ = .{
                .patt = try self.lowerPat(for_.patt),
                .iterable = try self.lowerExpr(for_.iterable),
                .body = try self.lowerExpr(for_.body),
            } },
            .let_ => |let_| .{ .let_ = .{
                .bind = .{
                    .ty = let_.bind.ty,
                    .symbol = let_.bind.symbol,
                },
                .body = try self.lowerExpr(let_.body),
                .rest = try self.lowerExpr(let_.rest),
            } },
        });
        try self.expr_map.put(expr_id, lowered);
        return lowered;
    }

    fn lowerPat(self: *BodyLifter, pat_id: MonoRow.Ast.PatId) Allocator.Error!Ast.PatId {
        const pat = self.input.getPat(pat_id);
        return try self.output.addPat(.{ .ty = pat.ty, .data = switch (pat.data) {
            .bool_lit => |value| .{ .bool_lit = value },
            .var_ => |symbol| .{ .var_ = symbol },
            .wildcard => .wildcard,
            .tag => |tag| .{ .tag = .{
                .union_shape = tag.union_shape,
                .tag = tag.tag,
                .payloads = try self.lowerTagPayloadPatternSpan(tag.payloads),
            } },
        } });
    }

    fn lowerBranch(self: *BodyLifter, branch_id: MonoRow.Ast.BranchId) Allocator.Error!Ast.BranchId {
        const branch = self.input.getBranch(branch_id);
        return try self.output.addBranch(.{
            .pat = try self.lowerPat(branch.pat),
            .body = try self.lowerExpr(branch.body),
        });
    }

    fn lowerStmt(self: *BodyLifter, stmt_id: MonoRow.Ast.StmtId) Allocator.Error!Ast.StmtId {
        const stmt = self.input.getStmt(stmt_id);
        return try self.output.addStmt(switch (stmt) {
            .decl => |decl| .{ .decl = .{
                .bind = decl.bind,
                .body = try self.lowerExpr(decl.body),
            } },
            .var_decl => |decl| .{ .var_decl = .{
                .bind = decl.bind,
                .body = try self.lowerExpr(decl.body),
            } },
            .reassign => |reassign| .{ .reassign = .{
                .target = reassign.target,
                .body = try self.lowerExpr(reassign.body),
            } },
            .expr => |expr| .{ .expr = try self.lowerExpr(expr) },
            .debug => |expr| .{ .debug = try self.lowerExpr(expr) },
            .expect => |expr| .{ .expect = try self.lowerExpr(expr) },
            .crash => |literal| .{ .crash = literal },
            .return_ => |expr| .{ .return_ = try self.lowerExpr(expr) },
            .break_ => .break_,
            .for_ => |for_| .{ .for_ = .{
                .patt = try self.lowerPat(for_.patt),
                .iterable = try self.lowerExpr(for_.iterable),
                .body = try self.lowerExpr(for_.body),
            } },
            .while_ => |while_| .{ .while_ = .{
                .cond = try self.lowerExpr(while_.cond),
                .body = try self.lowerExpr(while_.body),
            } },
        });
    }

    fn lowerExprSpan(self: *BodyLifter, span: MonoRow.Ast.Span(MonoRow.Ast.ExprId)) Allocator.Error!Ast.Span(Ast.ExprId) {
        const input_items = self.input.sliceExprSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.ExprId).empty();
        const output_items = try self.allocator.alloc(Ast.ExprId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |expr, i| {
            output_items[i] = try self.lowerExpr(expr);
        }
        return try self.output.addExprSpan(output_items);
    }

    fn lowerStmtSpan(self: *BodyLifter, span: MonoRow.Ast.Span(MonoRow.Ast.StmtId)) Allocator.Error!Ast.Span(Ast.StmtId) {
        const input_items = self.input.sliceStmtSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.StmtId).empty();
        const output_items = try self.allocator.alloc(Ast.StmtId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |stmt, i| {
            output_items[i] = try self.lowerStmt(stmt);
        }
        return try self.output.addStmtSpan(output_items);
    }

    fn lowerBranchSpan(self: *BodyLifter, span: MonoRow.Ast.Span(MonoRow.Ast.BranchId)) Allocator.Error!Ast.Span(Ast.BranchId) {
        const input_items = self.input.sliceBranchSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.BranchId).empty();
        const output_items = try self.allocator.alloc(Ast.BranchId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |branch, i| {
            output_items[i] = try self.lowerBranch(branch);
        }
        return try self.output.addBranchSpan(output_items);
    }

    fn lowerTagPayloadPatternSpan(self: *BodyLifter, span: MonoRow.Ast.Span(MonoRow.Ast.TagPayloadPattern)) Allocator.Error!Ast.Span(Ast.TagPayloadPattern) {
        const input_items = self.input.sliceTagPayloadPatternSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.TagPayloadPattern).empty();
        const output_items = try self.allocator.alloc(Ast.TagPayloadPattern, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |payload, i| {
            output_items[i] = .{
                .payload = payload.payload,
                .pattern = try self.lowerPat(payload.pattern),
            };
        }
        return try self.output.addTagPayloadPatternSpan(output_items);
    }

    fn lowerTypedSymbolSpan(self: *BodyLifter, span: MonoRow.Ast.Span(MonoRow.Ast.TypedSymbol)) Allocator.Error!Ast.Span(Ast.TypedSymbol) {
        const input_items = self.input.sliceTypedSymbolSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.TypedSymbol).empty();
        const output_items = try self.allocator.alloc(Ast.TypedSymbol, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |symbol, i| {
            output_items[i] = .{ .ty = symbol.ty, .symbol = symbol.symbol };
        }
        return try self.output.addTypedSymbolSpan(output_items);
    }

    fn lowerCaptureArgSpan(self: *BodyLifter, span: MonoRow.Ast.Span(MonoRow.Ast.CaptureArg)) Allocator.Error!Ast.Span(Ast.CaptureArg) {
        const input_items = self.input.sliceCaptureArgSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.CaptureArg).empty();
        const output_items = try self.allocator.alloc(Ast.CaptureArg, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |capture, i| {
            output_items[i] = .{
                .slot = capture.slot,
                .symbol = capture.symbol,
                .expr = try self.lowerExpr(capture.expr),
            };
        }
        return try self.output.addCaptureArgSpan(output_items);
    }

    fn lowerRecordFieldEvalSpan(self: *BodyLifter, span: MonoRow.Ast.Span(MonoRow.Ast.RecordFieldEval)) Allocator.Error!Ast.Span(Ast.RecordFieldEval) {
        const input_items = self.input.sliceRecordFieldEvalSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.RecordFieldEval).empty();
        const output_items = try self.allocator.alloc(Ast.RecordFieldEval, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |field, i| {
            output_items[i] = .{
                .field = field.field,
                .value = try self.lowerExpr(field.value),
            };
        }
        return try self.output.addRecordFieldEvalSpan(output_items);
    }

    fn lowerRecordFieldAssemblySpan(self: *BodyLifter, span: MonoRow.Ast.Span(MonoRow.Ast.RecordFieldAssembly)) Allocator.Error!Ast.Span(Ast.RecordFieldAssembly) {
        const input_items = self.input.sliceRecordFieldAssemblySpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.RecordFieldAssembly).empty();
        const output_items = try self.allocator.alloc(Ast.RecordFieldAssembly, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |field, i| {
            output_items[i] = .{
                .field = field.field,
                .value = try self.lowerExpr(field.value),
            };
        }
        return try self.output.addRecordFieldAssemblySpan(output_items);
    }

    fn lowerTagPayloadEvalSpan(self: *BodyLifter, span: MonoRow.Ast.Span(MonoRow.Ast.TagPayloadEval)) Allocator.Error!Ast.Span(Ast.TagPayloadEval) {
        const input_items = self.input.sliceTagPayloadEvalSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.TagPayloadEval).empty();
        const output_items = try self.allocator.alloc(Ast.TagPayloadEval, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |payload, i| {
            output_items[i] = .{
                .payload = payload.payload,
                .value = try self.lowerExpr(payload.value),
            };
        }
        return try self.output.addTagPayloadEvalSpan(output_items);
    }

    fn lowerTagPayloadAssemblySpan(self: *BodyLifter, span: MonoRow.Ast.Span(MonoRow.Ast.TagPayloadAssembly)) Allocator.Error!Ast.Span(Ast.TagPayloadAssembly) {
        const input_items = self.input.sliceTagPayloadAssemblySpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.TagPayloadAssembly).empty();
        const output_items = try self.allocator.alloc(Ast.TagPayloadAssembly, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |payload, i| {
            output_items[i] = .{
                .payload = payload.payload,
                .value = try self.lowerExpr(payload.value),
            };
        }
        return try self.output.addTagPayloadAssemblySpan(output_items);
    }
};

fn liftInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) std.debug.panic(message, .{});
    unreachable;
}

test "lifted capture graph has explicit edge records" {
    std.testing.refAllDecls(@This());
}
