//! Executable MIR construction state.

const std = @import("std");
const check = @import("check");
const symbol_mod = @import("symbol");
const LambdaSolved = @import("../lambda_solved/mod.zig");
const MonoRow = @import("../mono_row/mod.zig");
const debug = @import("../debug_verify.zig");
const ids = @import("../ids.zig");

const Ast = @import("ast.zig");
const Type = @import("type.zig");
const Layouts = @import("layouts.zig");

const Allocator = std.mem.Allocator;
const canonical = check.CanonicalNames;
const repr = LambdaSolved.Representation;

pub const Proc = struct {
    executable_proc: Ast.ExecutableProcId,
    source_proc: canonical.MonoSpecializedProcRef,
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
    root_procs: std.ArrayList(Ast.ExecutableProcId),
    layouts: ?Layouts.Layouts = null,

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
        if (self.layouts) |*layouts| layouts.deinit();
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

pub fn run(allocator: Allocator, solved: LambdaSolved.Solve.Program) Allocator.Error!Program {
    var input = solved;
    errdefer input.deinit();

    var program = Program.init(allocator);
    errdefer program.deinit();
    program.literal_pool = input.literal_pool;
    input.literal_pool = ids.ProgramLiteralPool.init(allocator);
    program.symbols = input.symbols;
    input.symbols = symbol_mod.Store.init(allocator);
    program.row_shapes = input.row_shapes;
    input.row_shapes = MonoRow.Store.init(allocator);

    try program.procs.ensureTotalCapacity(allocator, input.procs.items.len);
    var type_lowerer = TypeLowerer.init(allocator, &input.types, &program.types);
    defer type_lowerer.deinit();

    for (input.procs.items, 0..) |proc, i| {
        const executable_proc: Ast.ExecutableProcId = @enumFromInt(@as(u32, @intCast(i)));
        const value_store = &input.value_stores.items[@intFromEnum(proc.representation_instance)];
        var builder = BodyBuilder{
            .allocator = allocator,
            .input = &input.ast,
            .output = &program.ast,
            .type_lowerer = &type_lowerer,
            .value_store = value_store,
            .env = std.AutoHashMap(repr.BindingInfoId, Ast.ExecutableValueRef).init(allocator),
            .expr_map = std.AutoHashMap(LambdaSolved.Ast.ExprId, Ast.ExprId).init(allocator),
            .executable_proc = executable_proc,
            .source_proc = proc.proc,
            .representation_instance = proc.representation_instance,
        };
        defer builder.deinit();

        program.procs.appendAssumeCapacity(.{
            .executable_proc = executable_proc,
            .source_proc = proc.proc,
            .body = try builder.lowerDef(proc.body),
        });
    }

    for (input.root_procs.items) |root| {
        const executable_root = executableProcForSource(&program, root) orelse {
            debug.invariant(false, "executable build invariant violated: root source proc has no executable proc");
            unreachable;
        };
        try program.root_procs.append(allocator, executable_root);
    }

    input.deinit();
    return program;
}

const TypeLowerer = struct {
    allocator: Allocator,
    input: *const LambdaSolved.Type.Store,
    output: *Type.Store,
    active: std.AutoHashMap(LambdaSolved.Type.TypeVarId, Type.TypeId),

    fn init(allocator: Allocator, input: *const LambdaSolved.Type.Store, output: *Type.Store) TypeLowerer {
        return .{
            .allocator = allocator,
            .input = input,
            .output = output,
            .active = std.AutoHashMap(LambdaSolved.Type.TypeVarId, Type.TypeId).init(allocator),
        };
    }

    fn deinit(self: *TypeLowerer) void {
        self.active.deinit();
    }

    fn lowerType(self: *TypeLowerer, source: LambdaSolved.Type.TypeVarId) Allocator.Error!Type.TypeId {
        const root = self.input.unlinkConst(source);
        if (self.active.get(root)) |existing| return existing;

        const target = try self.output.addType(.placeholder);
        try self.active.put(root, target);
        errdefer _ = self.active.remove(root);

        const lowered: Type.Content = switch (self.input.getNode(root)) {
            .link => unreachable,
            .unbd,
            .for_a,
            .flex_for_a,
            => executableInvariant("executable type lowering received unresolved lambda-solved type"),
            .nominal => |nominal| .{ .nominal = .{
                .nominal = nominal.nominal,
                .backing = try self.lowerType(nominal.backing),
            } },
            .content => |content| switch (content) {
                .primitive => |prim| .{ .primitive = prim },
                .list => |elem| .{ .list = try self.lowerType(elem) },
                .box => |elem| .{ .box = try self.lowerType(elem) },
                .tuple => |span| blk: {
                    const source_items = self.input.sliceTypeVarSpan(span);
                    const items = try self.allocator.alloc(Type.TypeId, source_items.len);
                    defer self.allocator.free(items);
                    for (source_items, 0..) |item, i| {
                        items[i] = try self.lowerType(item);
                    }
                    break :blk .{ .tuple = try self.allocator.dupe(Type.TypeId, items) };
                },
                .func => executableInvariant("executable type lowering requires solved callable representation for function type"),
                .record => executableInvariant("executable type lowering requires row-finalized record shape metadata"),
                .tag_union => executableInvariant("executable type lowering requires row-finalized tag-union shape metadata"),
            },
        };

        self.output.types.items[@intFromEnum(target)] = lowered;
        _ = self.active.remove(root);
        return target;
    }

    fn lowerRecordType(self: *TypeLowerer, shape: MonoRow.RecordShapeId) Allocator.Error!Type.TypeId {
        return try self.output.addType(.{ .record = shape });
    }

    fn lowerTagUnionType(self: *TypeLowerer, shape: MonoRow.TagUnionShapeId) Allocator.Error!Type.TypeId {
        return try self.output.addType(.{ .tag_union = shape });
    }
};

const BodyBuilder = struct {
    allocator: Allocator,
    input: *const LambdaSolved.Ast.Store,
    output: *Ast.Store,
    type_lowerer: *TypeLowerer,
    value_store: *const repr.ValueInfoStore,
    env: std.AutoHashMap(repr.BindingInfoId, Ast.ExecutableValueRef),
    expr_map: std.AutoHashMap(LambdaSolved.Ast.ExprId, Ast.ExprId),
    executable_proc: Ast.ExecutableProcId,
    source_proc: canonical.MonoSpecializedProcRef,
    representation_instance: repr.ProcRepresentationInstanceId,

    fn deinit(self: *BodyBuilder) void {
        self.expr_map.deinit();
        self.env.deinit();
    }

    fn lowerDef(self: *BodyBuilder, def_id: LambdaSolved.Ast.DefId) Allocator.Error!Ast.DefId {
        const def = self.input.defs.items[@intFromEnum(def_id)];
        return try self.output.addDef(switch (def.value) {
            .fn_ => |fn_| blk: {
                const args = try self.lowerParamSpan(fn_.args);
                const body = try self.lowerExpr(fn_.body);
                break :blk .{
                    .proc = self.executable_proc,
                    .source_proc = def.proc,
                    .specialization_key = self.executableSpecializationKey(),
                    .value = .{
                        .args = args,
                        .body = body,
                    },
                };
            },
            .hosted_fn => |hosted| blk: {
                const args = try self.lowerParamSpan(hosted.args);
                const body = try self.output.addExpr(
                    try self.type_lowerer.output.addType(.placeholder),
                    self.output.freshValueRef(),
                    .runtime_error,
                );
                break :blk .{
                    .proc = self.executable_proc,
                    .source_proc = def.proc,
                    .specialization_key = self.executableSpecializationKey(),
                    .value = .{
                        .args = args,
                        .body = body,
                    },
                };
            },
            .val => |expr| blk: {
                const body = try self.lowerExpr(expr);
                break :blk .{
                    .proc = self.executable_proc,
                    .source_proc = def.proc,
                    .specialization_key = self.executableSpecializationKey(),
                    .value = .{
                        .args = Ast.Span(Ast.TypedValue).empty(),
                        .body = body,
                    },
                };
            },
            .run => |run| blk: {
                const body = try self.lowerExpr(run.body);
                break :blk .{
                    .proc = self.executable_proc,
                    .source_proc = def.proc,
                    .specialization_key = self.executableSpecializationKey(),
                    .value = .{
                        .args = Ast.Span(Ast.TypedValue).empty(),
                        .body = body,
                    },
                };
            },
        });
    }

    fn executableSpecializationKey(self: *const BodyBuilder) ?repr.ExecutableSpecializationKey {
        _ = self;
        return null;
    }

    fn lowerParamSpan(self: *BodyBuilder, span: LambdaSolved.Ast.Span(LambdaSolved.Ast.TypedSymbol)) Allocator.Error!Ast.Span(Ast.TypedValue) {
        if (span.len == 0) return Ast.Span(Ast.TypedValue).empty();
        const input_items = self.input.typed_symbols.items[span.start..][0..span.len];
        const output_items = try self.allocator.alloc(Ast.TypedValue, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |param, i| {
            const value = self.output.freshValueRef();
            try self.env.put(param.binding_info, value);
            output_items[i] = .{
                .ty = try self.type_lowerer.lowerType(param.ty),
                .value = value,
            };
        }
        return try self.output.addTypedValueSpan(output_items);
    }

    fn lowerExpr(self: *BodyBuilder, expr_id: LambdaSolved.Ast.ExprId) Allocator.Error!Ast.ExprId {
        if (self.expr_map.get(expr_id)) |existing| return existing;

        const expr = self.input.exprs.items[@intFromEnum(expr_id)];
        const lowered = switch (expr.data) {
            .var_ => |var_| blk: {
                const value = self.env.get(var_.binding_info) orelse executableInvariant("executable variable occurrence has no lowered binding value");
                break :blk try self.output.addExpr(
                    try self.type_lowerer.lowerType(expr.ty),
                    value,
                    .{ .value_ref = value },
                );
            },
            .int_lit => |literal| try self.addValueExpr(expr.ty, .{ .int_lit = literal }),
            .frac_f32_lit => |literal| try self.addValueExpr(expr.ty, .{ .frac_f32_lit = literal }),
            .frac_f64_lit => |literal| try self.addValueExpr(expr.ty, .{ .frac_f64_lit = literal }),
            .dec_lit => |literal| try self.addValueExpr(expr.ty, .{ .dec_lit = literal }),
            .str_lit => |literal| try self.addValueExpr(expr.ty, .{ .str_lit = literal }),
            .bool_lit => |literal| try self.addValueExpr(expr.ty, .{ .bool_lit = literal }),
            .unit => try self.addValueExpr(expr.ty, .unit),
            .const_ref => |const_ref| try self.addValueExpr(expr.ty, .{ .const_ref = const_ref }),
            .record => |record| blk: {
                const fields = try self.lowerRecordFields(record.assembly_order);
                break :blk try self.output.addExpr(
                    try self.type_lowerer.lowerRecordType(record.shape),
                    self.output.freshValueRef(),
                    .{ .record = .{
                        .shape = record.shape,
                        .fields = fields,
                    } },
                );
            },
            .tag => |tag| blk: {
                const payloads = try self.lowerTagPayloadValues(tag.assembly_order);
                break :blk try self.output.addExpr(
                    try self.type_lowerer.lowerTagUnionType(tag.union_shape),
                    self.output.freshValueRef(),
                    .{ .tag = .{
                        .union_shape = tag.union_shape,
                        .tag = tag.tag,
                        .payloads = payloads,
                    } },
                );
            },
            .access => |access| blk: {
                const record = try self.lowerExpr(access.record);
                break :blk try self.output.addExpr(
                    try self.type_lowerer.lowerType(expr.ty),
                    self.output.freshValueRef(),
                    .{ .access = .{
                        .record = record,
                        .field = access.field,
                    } },
                );
            },
            .let_ => |let_| blk: {
                const body = try self.lowerExpr(let_.body);
                const previous = try self.env.fetchPut(let_.bind.binding_info, self.exprValue(body));
                defer {
                    if (previous) |entry| {
                        self.env.put(let_.bind.binding_info, entry.value) catch unreachable;
                    } else {
                        _ = self.env.remove(let_.bind.binding_info);
                    }
                }
                const rest = try self.lowerExpr(let_.rest);
                const stmt = try self.output.addStmt(.{ .decl = .{
                    .value = self.exprValue(body),
                    .body = body,
                } });
                const stmt_span = try self.output.addStmtSpan(&.{stmt});
                break :blk try self.output.addExpr(
                    self.output.getExpr(rest).ty,
                    self.exprValue(rest),
                    .{ .block = .{
                        .stmts = stmt_span,
                        .final_expr = rest,
                    } },
                );
            },
            .block => |block| blk: {
                const stmts = try self.lowerStmtSpan(block.stmts);
                const final_expr = try self.lowerExpr(block.final_expr);
                break :blk try self.output.addExpr(
                    self.output.getExpr(final_expr).ty,
                    self.exprValue(final_expr),
                    .{ .block = .{
                        .stmts = stmts,
                        .final_expr = final_expr,
                    } },
                );
            },
            .tuple => |items| blk: {
                const items_span = try self.lowerExprIds(items);
                break :blk try self.output.addExpr(
                    try self.type_lowerer.lowerType(expr.ty),
                    self.output.freshValueRef(),
                    .{ .tuple = items_span },
                );
            },
            .list => |items| blk: {
                const items_span = try self.lowerExprIds(items);
                break :blk try self.output.addExpr(
                    try self.type_lowerer.lowerType(expr.ty),
                    self.output.freshValueRef(),
                    .{ .list = items_span },
                );
            },
            .tag_payload => |payload| blk: {
                const tag_union = try self.lowerExpr(payload.tag_union);
                break :blk try self.output.addExpr(
                    try self.type_lowerer.lowerType(expr.ty),
                    self.output.freshValueRef(),
                    .{ .tag_payload = .{
                        .tag_union = tag_union,
                        .payload = payload.payload,
                    } },
                );
            },
            .tuple_access => |access| blk: {
                const tuple = try self.lowerExpr(access.tuple);
                break :blk try self.output.addExpr(
                    try self.type_lowerer.lowerType(expr.ty),
                    self.output.freshValueRef(),
                    .{ .tuple_access = .{
                        .tuple = tuple,
                        .elem_index = access.elem_index,
                    } },
                );
            },
            .low_level => |low_level| blk: {
                const args = try self.lowerExprIds(low_level.args);
                break :blk try self.output.addExpr(
                    try self.type_lowerer.lowerType(expr.ty),
                    self.output.freshValueRef(),
                    .{ .low_level = .{
                        .op = low_level.op,
                        .args = args,
                    } },
                );
            },
            .return_ => |child| blk: {
                const lowered_child = try self.lowerExpr(child);
                break :blk try self.output.addExpr(
                    self.output.getExpr(lowered_child).ty,
                    self.exprValue(lowered_child),
                    .{ .return_ = lowered_child },
                );
            },
            .crash => |literal| try self.addValueExpr(expr.ty, .{ .crash = literal }),
            .runtime_error => try self.addValueExpr(expr.ty, .runtime_error),
            .match_ => |match_| blk: {
                _ = match_.join_info;
                const cond = try self.lowerExpr(match_.cond);
                const scrutinees = [_]Ast.ExecutableValueRef{self.exprValue(cond)};
                const lowered_branches = try self.lowerBranchSpan(match_.branches);
                const scrutinee_span = try self.output.addValueRefSpan(&scrutinees);
                const decision_plan = try self.output.addPatternDecisionPlan(.{
                    .scrutinees = scrutinee_span,
                    .branches = lowered_branches,
                });
                break :blk try self.output.addExpr(
                    try self.type_lowerer.lowerType(expr.ty),
                    self.output.freshValueRef(),
                    .{ .source_match = .{
                        .scrutinees = scrutinee_span,
                        .decision_plan = decision_plan,
                        .branches = lowered_branches,
                    } },
                );
            },
            .if_ => |if_| blk: {
                _ = if_.join_info;
                const cond = try self.lowerExpr(if_.cond);
                const then_body = try self.lowerExpr(if_.then_body);
                const else_body = try self.lowerExpr(if_.else_body);
                break :blk try self.output.addExpr(
                    try self.type_lowerer.lowerType(expr.ty),
                    self.output.freshValueRef(),
                    .{ .if_ = .{
                        .cond = cond,
                        .then_body = then_body,
                        .else_body = else_body,
                    } },
                );
            },
            .for_ => |for_| try self.lowerForExpr(expr.ty, for_),
            .capture_ref,
            .structural_eq,
            .bool_not,
            .call_value,
            .call_proc,
            .proc_value,
            .inspect,
            => executableInvariant("executable MIR reached lambda-solved expression form whose executable lowering is still missing"),
        };
        try self.expr_map.put(expr_id, lowered);
        return lowered;
    }

    const SavedBinding = struct {
        binding: repr.BindingInfoId,
        previous: ?Ast.ExecutableValueRef,
    };

    fn lowerPatScoped(
        self: *BodyBuilder,
        pat_id: LambdaSolved.Ast.PatId,
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.PatId {
        const pat = self.input.pats.items[@intFromEnum(pat_id)];
        return try self.output.addPat(.{ .ty = try self.type_lowerer.lowerType(pat.ty), .data = switch (pat.data) {
            .bool_lit => |literal| .{ .bool_lit = literal },
            .wildcard => .wildcard,
            .var_ => |var_| blk: {
                const value = self.output.freshValueRef();
                const previous = try self.env.fetchPut(var_.binding_info, value);
                try saved.append(self.allocator, .{
                    .binding = var_.binding_info,
                    .previous = if (previous) |entry| entry.value else null,
                });
                break :blk .{ .bind = value };
            },
            .tag => |tag| .{ .tag = .{
                .union_shape = tag.union_shape,
                .tag = tag.tag,
                .payloads = try self.lowerTagPayloadPatternSpan(tag.payloads, saved),
            } },
        } });
    }

    fn restoreBindings(self: *BodyBuilder, saved: *std.ArrayList(SavedBinding), start: usize) void {
        while (saved.items.len > start) {
            const binding = saved.pop().?;
            if (binding.previous) |previous| {
                self.env.put(binding.binding, previous) catch unreachable;
            } else {
                _ = self.env.remove(binding.binding);
            }
        }
    }

    fn lowerBranch(self: *BodyBuilder, branch_id: LambdaSolved.Ast.BranchId) Allocator.Error!Ast.BranchId {
        const branch = self.input.branches.items[@intFromEnum(branch_id)];
        var saved = std.ArrayList(SavedBinding).empty;
        defer saved.deinit(self.allocator);
        const pat = try self.lowerPatScoped(branch.pat, &saved);
        defer self.restoreBindings(&saved, 0);
        const body = try self.lowerExpr(branch.body);
        return try self.output.addBranch(.{
            .pat = pat,
            .body = body,
        });
    }

    fn lowerForExpr(self: *BodyBuilder, source_ty: LambdaSolved.Type.TypeVarId, for_: anytype) Allocator.Error!Ast.ExprId {
        var saved = std.ArrayList(SavedBinding).empty;
        defer saved.deinit(self.allocator);
        const patt = try self.lowerPatScoped(for_.patt, &saved);
        defer self.restoreBindings(&saved, 0);
        return try self.output.addExpr(
            try self.type_lowerer.lowerType(source_ty),
            self.output.freshValueRef(),
            .{ .for_ = .{
                .patt = patt,
                .iterable = try self.lowerExpr(for_.iterable),
                .body = try self.lowerExpr(for_.body),
            } },
        );
    }

    fn lowerStmt(self: *BodyBuilder, stmt_id: LambdaSolved.Ast.StmtId) Allocator.Error!Ast.StmtId {
        const stmt = self.input.stmts.items[@intFromEnum(stmt_id)];
        return try self.output.addStmt(switch (stmt) {
            .decl => |decl| blk: {
                const body = try self.lowerExpr(decl.body);
                try self.env.put(decl.bind.binding_info, self.exprValue(body));
                break :blk .{ .decl = .{
                    .value = self.exprValue(body),
                    .body = body,
                } };
            },
            .var_decl => |decl| blk: {
                const body = try self.lowerExpr(decl.body);
                try self.env.put(decl.bind.binding_info, self.exprValue(body));
                break :blk .{ .decl = .{
                    .value = self.exprValue(body),
                    .body = body,
                } };
            },
            .reassign => |reassign| blk: {
                const body = try self.lowerExpr(reassign.body);
                const target = self.env.get(reassign.version) orelse executableInvariant("executable reassignment target has no lowered binding value");
                break :blk .{ .reassign = .{
                    .target = target,
                    .body = body,
                } };
            },
            .expr => |expr| .{ .expr = try self.lowerExpr(expr) },
            .debug => |expr| .{ .debug = try self.lowerExpr(expr) },
            .expect => |expr| .{ .expect = try self.lowerExpr(expr) },
            .crash => |literal| .{ .crash = literal },
            .return_ => |expr| blk: {
                const body = try self.lowerExpr(expr);
                break :blk .{ .return_ = self.exprValue(body) };
            },
            .break_ => .break_,
            .for_ => |for_| blk: {
                var saved = std.ArrayList(SavedBinding).empty;
                defer saved.deinit(self.allocator);
                const patt = try self.lowerPatScoped(for_.patt, &saved);
                defer self.restoreBindings(&saved, 0);
                break :blk .{ .for_ = .{
                    .patt = patt,
                    .iterable = try self.lowerExpr(for_.iterable),
                    .body = try self.lowerExpr(for_.body),
                } };
            },
            .while_ => |while_| .{ .while_ = .{
                .cond = try self.lowerExpr(while_.cond),
                .body = try self.lowerExpr(while_.body),
            } },
        });
    }

    fn lowerStmtSpan(self: *BodyBuilder, span: LambdaSolved.Ast.Span(LambdaSolved.Ast.StmtId)) Allocator.Error!Ast.Span(Ast.StmtId) {
        if (span.len == 0) return Ast.Span(Ast.StmtId).empty();
        const input_items = self.input.stmt_ids.items[span.start..][0..span.len];
        const output_items = try self.allocator.alloc(Ast.StmtId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |stmt, i| {
            output_items[i] = try self.lowerStmt(stmt);
        }
        return try self.output.addStmtSpan(output_items);
    }

    fn lowerExprIds(self: *BodyBuilder, span: LambdaSolved.Ast.Span(LambdaSolved.Ast.ExprId)) Allocator.Error!Ast.Span(Ast.ExprId) {
        if (span.len == 0) return Ast.Span(Ast.ExprId).empty();
        const input_items = self.input.expr_ids.items[span.start..][0..span.len];
        const exprs = try self.allocator.alloc(Ast.ExprId, input_items.len);
        defer self.allocator.free(exprs);
        for (input_items, 0..) |expr, i| {
            exprs[i] = try self.lowerExpr(expr);
        }
        return try self.output.addExprSpan(exprs);
    }

    fn lowerBranchSpan(self: *BodyBuilder, span: LambdaSolved.Ast.Span(LambdaSolved.Ast.BranchId)) Allocator.Error!Ast.Span(Ast.BranchId) {
        if (span.len == 0) return Ast.Span(Ast.BranchId).empty();
        const input_items = self.input.branch_ids.items[span.start..][0..span.len];
        const branches = try self.allocator.alloc(Ast.BranchId, input_items.len);
        defer self.allocator.free(branches);
        for (input_items, 0..) |branch, i| {
            branches[i] = try self.lowerBranch(branch);
        }
        return try self.output.addBranchSpan(branches);
    }

    fn lowerTagPayloadPatternSpan(
        self: *BodyBuilder,
        span: LambdaSolved.Ast.Span(LambdaSolved.Ast.TagPayloadPattern),
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.Span(Ast.TagPayloadPattern) {
        if (span.len == 0) return Ast.Span(Ast.TagPayloadPattern).empty();
        const input_items = self.input.tag_payload_patterns.items[span.start..][0..span.len];
        const payloads = try self.allocator.alloc(Ast.TagPayloadPattern, input_items.len);
        defer self.allocator.free(payloads);
        for (input_items, 0..) |payload, i| {
            payloads[i] = .{
                .payload = payload.payload,
                .pattern = try self.lowerPatScoped(payload.pattern, saved),
            };
        }
        return try self.output.addTagPayloadPatternSpan(payloads);
    }

    fn lowerRecordFields(self: *BodyBuilder, span: LambdaSolved.Ast.Span(LambdaSolved.Ast.RecordFieldAssembly)) Allocator.Error!Ast.Span(Ast.RecordFieldExpr) {
        if (span.len == 0) return Ast.Span(Ast.RecordFieldExpr).empty();
        const input_items = self.input.record_field_assemblies.items[span.start..][0..span.len];
        const values = try self.allocator.alloc(Ast.RecordFieldExpr, input_items.len);
        defer self.allocator.free(values);
        for (input_items, 0..) |field, i| {
            const lowered = try self.lowerExpr(field.value);
            values[i] = .{
                .field = field.field,
                .expr = lowered,
                .ty = self.output.getExpr(lowered).ty,
                .value = self.exprValue(lowered),
            };
        }
        return try self.output.addRecordFieldExprSpan(values);
    }

    fn lowerTagPayloadValues(self: *BodyBuilder, span: LambdaSolved.Ast.Span(LambdaSolved.Ast.TagPayloadAssembly)) Allocator.Error!Ast.Span(Ast.TagPayloadExpr) {
        if (span.len == 0) return Ast.Span(Ast.TagPayloadExpr).empty();
        const input_items = self.input.tag_payload_assemblies.items[span.start..][0..span.len];
        const values = try self.allocator.alloc(Ast.TagPayloadExpr, input_items.len);
        defer self.allocator.free(values);
        for (input_items, 0..) |payload, i| {
            const lowered = try self.lowerExpr(payload.value);
            values[i] = .{
                .payload = payload.payload,
                .expr = lowered,
                .value = self.exprValue(lowered),
            };
        }
        return try self.output.addTagPayloadExprSpan(values);
    }

    fn addValueExpr(self: *BodyBuilder, source_ty: LambdaSolved.Type.TypeVarId, data: Ast.Expr.Data) Allocator.Error!Ast.ExprId {
        return try self.output.addExpr(
            try self.type_lowerer.lowerType(source_ty),
            self.output.freshValueRef(),
            data,
        );
    }

    fn exprValue(self: *const BodyBuilder, expr: Ast.ExprId) Ast.ExecutableValueRef {
        return self.output.getExpr(expr).value;
    }
};

fn executableInvariant(comptime message: []const u8) noreturn {
    debug.invariant(false, message);
    unreachable;
}

fn executableProcForSource(program: *const Program, source_proc: canonical.MonoSpecializedProcRef) ?Ast.ExecutableProcId {
    for (program.procs.items) |proc| {
        if (canonical.monoSpecializedProcRefEql(proc.source_proc, source_proc))
        {
            return proc.executable_proc;
        }
    }
    return null;
}

pub fn verifyCallableMatchBranch(
    representation_store: *const repr.RepresentationStore,
    callable_set_key: repr.CanonicalCallableSetKey,
    requested_source_fn_ty: canonical.CanonicalTypeKey,
    branch: Ast.CallableMatchBranch,
) void {
    debug.invariant(
        repr.callableSetKeyEql(branch.member.callable_set_key, callable_set_key),
        "executable invariant violated: callable_match branch points at a different callable set",
    );
    const member = representation_store.callableSetMember(callable_set_key, branch.member.member_index) orelse {
        debug.invariant(false, "executable invariant violated: callable_match branch points at missing callable member");
        return;
    };
    debug.invariant(
        repr.canonicalTypeKeyEql(member.proc_value.source_fn_ty, requested_source_fn_ty),
        "executable invariant violated: callable_match member source function type differs from call site",
    );
    debug.invariant(
        repr.canonicalTypeKeyEql(branch.source_fn_ty, requested_source_fn_ty),
        "executable invariant violated: callable_match branch source function type differs from call site",
    );
    debug.invariant(
        repr.canonicalTypeKeyEql(branch.executable_specialization_key.requested_fn_ty, requested_source_fn_ty),
        "executable invariant violated: callable_match executable specialization requested type differs from call site",
    );
}

test "executable build owns final program state" {
    std.testing.refAllDecls(@This());
}
