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
    canonical_names: canonical.CanonicalNameStore,
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
            .canonical_names = canonical.CanonicalNameStore.init(allocator),
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
        self.canonical_names.deinit();
        self.* = Program.init(self.allocator);
    }
};

pub fn run(allocator: Allocator, solved: LambdaSolved.Solve.Program) Allocator.Error!Program {
    var input = solved;
    errdefer input.deinit();

    var program = Program.init(allocator);
    errdefer program.deinit();
    program.canonical_names = input.canonical_names;
    input.canonical_names = canonical.CanonicalNameStore.init(allocator);
    program.literal_pool = input.literal_pool;
    input.literal_pool = ids.ProgramLiteralPool.init(allocator);
    program.symbols = input.symbols;
    input.symbols = symbol_mod.Store.init(allocator);
    program.row_shapes = input.row_shapes;
    input.row_shapes = MonoRow.Store.init(allocator);

    var proc_map = std.AutoHashMap(canonical.MonoSpecializedProcRef, Ast.ExecutableProcId).init(allocator);
    defer proc_map.deinit();
    var proc_instance_map = std.AutoHashMap(canonical.MonoSpecializedProcRef, repr.ProcRepresentationInstanceId).init(allocator);
    defer proc_instance_map.deinit();
    try proc_map.ensureTotalCapacity(@intCast(input.procs.items.len));
    try proc_instance_map.ensureTotalCapacity(@intCast(input.procs.items.len));
    for (input.procs.items, 0..) |proc, i| {
        const executable_proc: Ast.ExecutableProcId = @enumFromInt(@as(u32, @intCast(i)));
        proc_map.putAssumeCapacity(proc.proc, executable_proc);
        proc_instance_map.putAssumeCapacity(proc.proc, proc.representation_instance);
    }

    try program.procs.ensureTotalCapacity(allocator, input.procs.items.len);
    var type_lowerer = TypeLowerer.init(allocator, &input.types, &program.types, &program.row_shapes);
    defer type_lowerer.deinit();

    for (input.procs.items, 0..) |proc, i| {
        const executable_proc: Ast.ExecutableProcId = @enumFromInt(@as(u32, @intCast(i)));
        const value_store = &input.value_stores.items[@intFromEnum(proc.representation_instance)];
        const proc_instance = &input.proc_instances.items[@intFromEnum(proc.representation_instance)];
        var builder = BodyBuilder{
            .allocator = allocator,
            .input = &input.ast,
            .output = &program.ast,
            .type_lowerer = &type_lowerer,
            .value_store = value_store,
            .representation_store = &input.solve_sessions.items[@intFromEnum(proc_instance.solve_session)].representation_store,
            .env = std.AutoHashMap(repr.BindingInfoId, Ast.ExecutableValueRef).init(allocator),
            .expr_map = std.AutoHashMap(LambdaSolved.Ast.ExprId, Ast.ExprId).init(allocator),
            .executable_proc = executable_proc,
            .source_proc = proc.proc,
            .representation_instance = proc.representation_instance,
            .proc_instance = proc_instance,
            .proc_instances = input.proc_instances.items,
            .proc_map = &proc_map,
            .proc_instance_map = &proc_instance_map,
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
    row_shapes: *MonoRow.Store,
    active: std.AutoHashMap(LambdaSolved.Type.TypeVarId, Type.TypeId),

    fn init(
        allocator: Allocator,
        input: *const LambdaSolved.Type.Store,
        output: *Type.Store,
        row_shapes: *MonoRow.Store,
    ) TypeLowerer {
        return .{
            .allocator = allocator,
            .input = input,
            .output = output,
            .row_shapes = row_shapes,
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
                .record => |record| try self.lowerRecordType(record.fields),
                .tag_union => |tag_union| try self.lowerTagUnionType(tag_union.tags),
            },
        };

        self.output.types.items[@intFromEnum(target)] = lowered;
        _ = self.active.remove(root);
        return target;
    }

    fn lowerRecordType(self: *TypeLowerer, span: LambdaSolved.Type.Span(LambdaSolved.Type.Field)) Allocator.Error!Type.Content {
        const source_fields = self.input.sliceFields(span);
        const labels = try self.allocator.alloc(canonical.RecordFieldLabelId, source_fields.len);
        defer self.allocator.free(labels);
        for (source_fields, 0..) |field, i| {
            labels[i] = field.name;
        }

        const shape = try self.row_shapes.internRecordShapeFromLabels(labels);
        const shape_fields = self.row_shapes.recordShapeFields(shape);
        if (shape_fields.len != source_fields.len) executableInvariant("executable type lowering record shape arity mismatch");

        const fields = try self.allocator.alloc(Type.RecordFieldType, source_fields.len);
        errdefer self.allocator.free(fields);
        for (source_fields, 0..) |field, i| {
            fields[i] = .{
                .field = shape_fields[i],
                .ty = try self.lowerType(field.ty),
            };
        }

        return .{ .record = .{
            .shape = shape,
            .fields = fields,
        } };
    }

    fn lowerTagUnionType(self: *TypeLowerer, span: LambdaSolved.Type.Span(LambdaSolved.Type.Tag)) Allocator.Error!Type.Content {
        const source_tags = self.input.sliceTags(span);
        const descriptors = try self.allocator.alloc(MonoRow.TagShapeDescriptor, source_tags.len);
        defer self.allocator.free(descriptors);
        for (source_tags, 0..) |tag, i| {
            descriptors[i] = .{
                .name = tag.name,
                .payload_arity = tag.args.len,
            };
        }

        const shape = try self.row_shapes.internTagUnionShapeFromDescriptors(descriptors);
        const shape_tags = self.row_shapes.tagUnionTags(shape);
        if (shape_tags.len != source_tags.len) executableInvariant("executable type lowering tag-union shape arity mismatch");

        const tags = try self.allocator.alloc(Type.TagType, source_tags.len);
        for (tags) |*tag| tag.* = .{ .tag = @enumFromInt(0), .payloads = &.{} };
        errdefer {
            for (tags[0..source_tags.len]) |tag| {
                if (tag.payloads.len > 0) self.allocator.free(tag.payloads);
            }
            self.allocator.free(tags);
        }
        for (source_tags, 0..) |source_tag, i| {
            const source_payload_tys = self.input.sliceTypeVarSpan(source_tag.args);
            const shape_payloads = self.row_shapes.tagPayloads(shape_tags[i]);
            if (shape_payloads.len != source_payload_tys.len) executableInvariant("executable type lowering tag payload arity mismatch");

            const payloads = try self.allocator.alloc(Type.TagPayloadType, source_payload_tys.len);
            errdefer self.allocator.free(payloads);
            for (source_payload_tys, 0..) |payload_ty, payload_i| {
                payloads[payload_i] = .{
                    .payload = shape_payloads[payload_i],
                    .ty = try self.lowerType(payload_ty),
                };
            }

            tags[i] = .{
                .tag = shape_tags[i],
                .payloads = payloads,
            };
        }

        return .{ .tag_union = .{
            .shape = shape,
            .tags = tags,
        } };
    }
};

const BodyBuilder = struct {
    allocator: Allocator,
    input: *const LambdaSolved.Ast.Store,
    output: *Ast.Store,
    type_lowerer: *TypeLowerer,
    value_store: *const repr.ValueInfoStore,
    representation_store: *const repr.RepresentationStore,
    env: std.AutoHashMap(repr.BindingInfoId, Ast.ExecutableValueRef),
    expr_map: std.AutoHashMap(LambdaSolved.Ast.ExprId, Ast.ExprId),
    executable_proc: Ast.ExecutableProcId,
    source_proc: canonical.MonoSpecializedProcRef,
    representation_instance: repr.ProcRepresentationInstanceId,
    proc_instance: *const repr.ProcRepresentationInstance,
    proc_instances: []const repr.ProcRepresentationInstance,
    proc_map: *const std.AutoHashMap(canonical.MonoSpecializedProcRef, Ast.ExecutableProcId),
    proc_instance_map: *const std.AutoHashMap(canonical.MonoSpecializedProcRef, repr.ProcRepresentationInstanceId),

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
                    .specialization_key = try self.executableSpecializationKey(),
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
                    .specialization_key = try self.executableSpecializationKey(),
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
                    .specialization_key = try self.executableSpecializationKey(),
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
                    .specialization_key = try self.executableSpecializationKey(),
                    .value = .{
                        .args = Ast.Span(Ast.TypedValue).empty(),
                        .body = body,
                    },
                };
            },
        });
    }

    fn executableSpecializationKey(self: *const BodyBuilder) Allocator.Error!repr.ExecutableSpecializationKey {
        if (!canonical.monoSpecializedProcRefEql(self.proc_instance.proc, self.source_proc)) {
            executableInvariant("executable build procedure instance does not match the source procedure being lowered");
        }
        return try repr.cloneExecutableSpecializationKey(self.allocator, self.proc_instance.executable_specialization_key);
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
                    try self.type_lowerer.lowerType(expr.ty),
                    self.output.freshValueRef(),
                    .{ .record = .{
                        .shape = record.shape,
                        .fields = fields,
                    } },
                );
            },
            .nominal_reinterpret => |backing| blk: {
                const lowered_backing = try self.lowerExpr(backing);
                break :blk try self.output.addExpr(
                    try self.type_lowerer.lowerType(expr.ty),
                    self.output.freshValueRef(),
                    .{ .nominal_reinterpret = lowered_backing },
                );
            },
            .tag => |tag| blk: {
                const payloads = try self.lowerTagPayloadValues(tag.assembly_order);
                break :blk try self.output.addExpr(
                    try self.type_lowerer.lowerType(expr.ty),
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
            .bool_not => |child| blk: {
                const lowered_child = try self.lowerExpr(child);
                break :blk try self.output.addExpr(
                    try self.type_lowerer.lowerType(expr.ty),
                    self.output.freshValueRef(),
                    .{ .bool_not = lowered_child },
                );
            },
            .crash => |literal| try self.addValueExpr(expr.ty, .{ .crash = literal }),
            .runtime_error => try self.addValueExpr(expr.ty, .runtime_error),
            .match_ => |match_| blk: {
                _ = match_.join_info;
                const cond = try self.lowerExpr(match_.cond);
                const scrutinee_exprs = [_]Ast.ExprId{cond};
                const scrutinees = [_]Ast.ExecutableValueRef{self.exprValue(cond)};
                const lowered_branches = try self.lowerBranchSpan(match_.branches);
                const scrutinee_expr_span = try self.output.addExprSpan(&scrutinee_exprs);
                const scrutinee_span = try self.output.addValueRefSpan(&scrutinees);
                const decision_plan = try self.output.addPatternDecisionPlan(.{
                    .scrutinees = scrutinee_span,
                    .branches = lowered_branches,
                });
                break :blk try self.output.addExpr(
                    try self.type_lowerer.lowerType(expr.ty),
                    self.output.freshValueRef(),
                    .{ .source_match = .{
                        .scrutinee_exprs = scrutinee_expr_span,
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
            => executableInvariant("executable MIR reached lambda-solved expression form whose executable lowering is still missing"),
            .call_value => |call| try self.lowerCallValue(expr.ty, call),
            .call_proc => |call| try self.lowerCallProc(expr.ty, call),
            .proc_value => |proc_value| try self.lowerProcValue(expr.ty, expr.value_info, proc_value),
            .inspect => |child| blk: {
                const value = try self.lowerExpr(child);
                const debug_stmt = try self.output.addStmt(.{ .debug = value });
                const stmts = try self.output.addStmtSpan(&.{debug_stmt});
                break :blk try self.output.addExpr(
                    self.output.getExpr(value).ty,
                    self.exprValue(value),
                    .{ .block = .{
                        .stmts = stmts,
                        .final_expr = value,
                    } },
                );
            },
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
            .int_lit => |literal| .{ .int_lit = literal },
            .frac_f32_lit => |literal| .{ .frac_f32_lit = literal },
            .frac_f64_lit => |literal| .{ .frac_f64_lit = literal },
            .dec_lit => |literal| .{ .dec_lit = literal },
            .str_lit => |literal| .{ .str_lit = literal },
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
                .ty = self.output.getExpr(lowered).ty,
                .value = self.exprValue(lowered),
            };
        }
        return try self.output.addTagPayloadExprSpan(values);
    }

    fn lowerCallProc(
        self: *BodyBuilder,
        source_ty: LambdaSolved.Type.TypeVarId,
        call: anytype,
    ) Allocator.Error!Ast.ExprId {
        const target_proc = self.proc_map.get(call.proc) orelse executableInvariant("executable call_proc target was not reserved before body lowering");
        const target_instance_id = self.proc_instance_map.get(call.proc) orelse executableInvariant("executable call_proc target has no representation instance");
        const target_instance = self.proc_instances[@intFromEnum(target_instance_id)];

        const arg_items = self.input.expr_ids.items[call.args.start..][0..call.args.len];
        const direct_args = try self.allocator.alloc(Ast.DirectCallArg, arg_items.len);
        defer self.allocator.free(direct_args);
        const stmt_ids = try self.allocator.alloc(Ast.StmtId, arg_items.len);
        defer self.allocator.free(stmt_ids);

        for (arg_items, 0..) |arg, i| {
            const lowered = try self.lowerExpr(arg);
            const value = self.exprValue(lowered);
            direct_args[i] = .{ .value = value };
            stmt_ids[i] = try self.output.addStmt(.{ .decl = .{
                .value = value,
                .body = lowered,
            } });
        }

        const result_ty = try self.type_lowerer.lowerType(source_ty);
        const result_value = self.output.freshValueRef();
        const final_call = try self.output.addExpr(result_ty, result_value, .{ .call_direct = .{
            .source = call.proc.proc,
            .executable_specialization_key = try repr.cloneExecutableSpecializationKey(self.allocator, target_instance.executable_specialization_key),
            .executable_proc = target_proc,
            .direct_args = try self.output.addDirectCallArgSpan(direct_args),
            .result_bridge = null,
        } });

        if (stmt_ids.len == 0) return final_call;

        return try self.output.addExpr(result_ty, result_value, .{ .block = .{
            .stmts = try self.output.addStmtSpan(stmt_ids),
            .final_expr = final_call,
        } });
    }

    fn lowerProcValue(
        self: *BodyBuilder,
        source_ty: LambdaSolved.Type.TypeVarId,
        value_info_id: repr.ValueInfoId,
        proc_value: anytype,
    ) Allocator.Error!Ast.ExprId {
        const value_info = self.value_store.values.items[@intFromEnum(value_info_id)];
        const callable = value_info.callable orelse executableInvariant("executable proc_value reached value without callable metadata");
        const construction_id = callable.construction_plan orelse executableInvariant("executable proc_value reached finite callable value without construction metadata");
        const construction = self.representation_store.callableConstructionPlan(construction_id);
        const member = self.representation_store.callableSetMember(construction.callable_set_key, construction.selected_member) orelse {
            executableInvariant("executable proc_value construction selected a missing callable-set member");
        };

        const capture_items = self.input.capture_args.items[proc_value.captures.start..][0..proc_value.captures.len];
        if (capture_items.len != construction.capture_values.len) {
            executableInvariant("executable proc_value capture arity does not match construction plan");
        }
        const capture_refs = try self.allocator.alloc(Ast.CaptureValueRef, capture_items.len);
        defer self.allocator.free(capture_refs);
        const stmt_ids = try self.allocator.alloc(Ast.StmtId, capture_items.len);
        defer self.allocator.free(stmt_ids);

        for (capture_items, 0..) |capture, i| {
            if (capture.value_info != construction.capture_values[i]) {
                executableInvariant("executable proc_value capture value differs from construction plan");
            }
            const lowered = try self.lowerExpr(capture.expr);
            const value = self.exprValue(lowered);
            capture_refs[i] = .{
                .slot = capture.slot,
                .value = value,
                .exec_ty = self.output.getExpr(lowered).ty,
            };
            stmt_ids[i] = try self.output.addStmt(.{ .decl = .{
                .value = value,
                .body = lowered,
            } });
        }

        const result_ty = try self.type_lowerer.lowerType(source_ty);
        const result_value = self.output.freshValueRef();
        const final_value = try self.output.addExpr(result_ty, result_value, .{ .callable_set_value = .{
            .construction_plan = construction_id,
            .callable_set_key = construction.callable_set_key,
            .member = .{
                .callable_set_key = construction.callable_set_key,
                .member_index = construction.selected_member,
            },
            .capture_record = if (capture_refs.len == 0) null else .{
                .capture_shape_key = member.capture_shape_key,
                .values = try self.output.addCaptureValueRefSpan(capture_refs),
                .record_tmp = self.output.freshValueRef(),
            },
        } });

        if (stmt_ids.len == 0) return final_value;

        return try self.output.addExpr(result_ty, result_value, .{ .block = .{
            .stmts = try self.output.addStmtSpan(stmt_ids),
            .final_expr = final_value,
        } });
    }

    fn lowerCallValue(
        self: *BodyBuilder,
        source_ty: LambdaSolved.Type.TypeVarId,
        call: anytype,
    ) Allocator.Error!Ast.ExprId {
        const func = try self.lowerExpr(call.func);
        const func_value = self.exprValue(func);
        const func_value_info_id = self.input.exprs.items[@intFromEnum(call.func)].value_info;
        const func_value_info = self.value_store.values.items[@intFromEnum(func_value_info_id)];
        const callable = func_value_info.callable orelse executableInvariant("executable call_value callee has no callable metadata");
        const emission = self.representation_store.callableEmissionPlan(callable.emission_plan);
        const callable_set_key = switch (emission) {
            .finite => |key| key,
            .already_erased,
            .erase_proc_value,
            .erase_finite_set,
            => executableInvariant("executable call_value erased callable lowering is not implemented yet"),
        };
        const descriptor = self.representation_store.callableSetDescriptor(callable_set_key) orelse {
            executableInvariant("executable call_value finite callable set has no descriptor");
        };
        if (descriptor.members.len == 0) executableInvariant("executable call_value finite callable set has no members");

        const arg_items = self.input.expr_ids.items[call.args.start..][0..call.args.len];
        const arg_values = try self.allocator.alloc(Ast.ExecutableValueRef, arg_items.len);
        defer self.allocator.free(arg_values);
        const stmt_ids = try self.allocator.alloc(Ast.StmtId, arg_items.len + 1);
        defer self.allocator.free(stmt_ids);
        stmt_ids[0] = try self.output.addStmt(.{ .decl = .{
            .value = func_value,
            .body = func,
        } });
        for (arg_items, 0..) |arg, i| {
            const lowered = try self.lowerExpr(arg);
            const value = self.exprValue(lowered);
            arg_values[i] = value;
            stmt_ids[i + 1] = try self.output.addStmt(.{ .decl = .{
                .value = value,
                .body = lowered,
            } });
        }

        const requested_source_fn_ty = descriptor.members[0].proc_value.source_fn_ty;
        const branches = try self.allocator.alloc(Ast.CallableMatchBranch, descriptor.members.len);
        defer self.allocator.free(branches);
        for (descriptor.members, 0..) |member, i| {
            if (!repr.canonicalTypeKeyEql(member.proc_value.source_fn_ty, requested_source_fn_ty)) {
                executableInvariant("executable call_value callable-set member source type differs from call site");
            }
            const target = sourceProcForCallable(member.proc_value);
            const executable_proc = self.proc_map.get(target) orelse executableInvariant("executable call_value member target was not reserved");
            const target_instance_id = self.proc_instance_map.get(target) orelse executableInvariant("executable call_value member target has no representation instance");
            const target_instance = self.proc_instances[@intFromEnum(target_instance_id)];
            const direct_args = try self.allocator.alloc(Ast.DirectCallArg, arg_values.len);
            defer self.allocator.free(direct_args);
            for (arg_values, 0..) |arg_value, arg_i| {
                direct_args[arg_i] = .{ .value = arg_value };
            }
            branches[i] = .{
                .member = .{
                    .callable_set_key = callable_set_key,
                    .member_index = member.member,
                },
                .source_fn_ty = member.proc_value.source_fn_ty,
                .executable_specialization_key = try repr.cloneExecutableSpecializationKey(self.allocator, target_instance.executable_specialization_key),
                .executable_proc = executable_proc,
                .direct_args = try self.output.addDirectCallArgSpan(direct_args),
                .result_bridge = null,
            };
        }

        const result_ty = try self.type_lowerer.lowerType(source_ty);
        const result_value = self.output.freshValueRef();
        const final_call = try self.output.addExpr(result_ty, result_value, .{ .callable_match = .{
            .callable_set_key = callable_set_key,
            .requested_source_fn_ty = requested_source_fn_ty,
            .callee = func_value,
            .args = try self.output.addValueRefSpan(arg_values),
            .branches = try self.output.addCallableMatchBranchSpan(branches),
            .result_ty = result_ty,
            .result_value = result_value,
        } });

        return try self.output.addExpr(result_ty, result_value, .{ .block = .{
            .stmts = try self.output.addStmtSpan(stmt_ids),
            .final_expr = final_call,
        } });
    }

    fn sourceProcForCallable(proc_callable: canonical.ProcedureCallableRef) canonical.MonoSpecializedProcRef {
        const template = switch (proc_callable.template) {
            .checked => |checked| checked,
            .lifted,
            .synthetic,
            => executableInvariant("executable callable member target is not a checked mono specialization"),
        };
        return .{
            .proc = .{
                .artifact = template.artifact,
                .proc_base = template.proc_base,
            },
            .specialization = .{
                .template = template,
                .requested_mono_fn_ty = proc_callable.source_fn_ty,
            },
        };
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
