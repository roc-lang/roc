//! Lambda-solved MIR construction state.

const std = @import("std");
const check = @import("check");
const symbol_mod = @import("symbol");
const Lifted = @import("../lifted/mod.zig");
const MonoRow = @import("../mono_row/mod.zig");
const ids = @import("../ids.zig");

const Ast = @import("ast.zig");
const Type = @import("type.zig");
const repr = @import("representation.zig");

const Allocator = std.mem.Allocator;
const canonical = check.CanonicalNames;

pub const Proc = struct {
    proc: canonical.MirProcedureRef,
    body: Ast.DefId,
    representation_instance: repr.ProcRepresentationInstanceId,
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
    root_procs: std.ArrayList(canonical.MirProcedureRef),
    solve_sessions: std.ArrayList(repr.RepresentationSolveSession),
    proc_instances: std.ArrayList(repr.ProcRepresentationInstance),
    value_stores: std.ArrayList(repr.ValueInfoStore),

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
            .solve_sessions = .empty,
            .proc_instances = .empty,
            .value_stores = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        for (self.value_stores.items) |*store| {
            store.deinit();
        }
        self.value_stores.deinit(self.allocator);
        for (self.proc_instances.items) |*instance| {
            repr.deinitProcRepresentationInstance(self.allocator, instance);
        }
        self.proc_instances.deinit(self.allocator);
        for (self.solve_sessions.items) |*session| {
            session.deinit();
        }
        self.solve_sessions.deinit(self.allocator);
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

pub fn run(allocator: Allocator, lifted: Lifted.Lift.Program) Allocator.Error!Program {
    var input = lifted;
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

    try program.procs.ensureTotalCapacity(allocator, input.procs.items.len);
    try program.solve_sessions.ensureTotalCapacity(allocator, input.procs.items.len);
    try program.proc_instances.ensureTotalCapacity(allocator, input.procs.items.len);
    try program.value_stores.ensureTotalCapacity(allocator, input.procs.items.len);

    var type_importer = TypeImporter.init(allocator, &input.types, &program.types);
    defer type_importer.deinit();

    for (input.procs.items, 0..) |proc, i| {
        const instance: repr.ProcRepresentationInstanceId = @enumFromInt(@as(u32, @intCast(i)));
        const session_id: repr.RepresentationSolveSessionId = @enumFromInt(@as(u32, @intCast(i)));
        const value_store_id: repr.ValueInfoStoreId = @enumFromInt(@as(u32, @intCast(i)));

        program.solve_sessions.appendAssumeCapacity(.{
            .members = &.{},
            .representation_store = repr.RepresentationStore.init(allocator),
            .state = .building,
        });
        program.value_stores.appendAssumeCapacity(repr.ValueInfoStore.init(allocator));

        var solver = BodySolver{
            .allocator = allocator,
            .input = &input.ast,
            .output = &program.ast,
            .canonical_names = &program.canonical_names,
            .type_importer = &type_importer,
            .representation_store = &program.solve_sessions.items[i].representation_store,
            .value_store = &program.value_stores.items[i],
            .env = std.AutoHashMap(Ast.Symbol, repr.BindingInfoId).init(allocator),
            .expr_map = std.AutoHashMap(Lifted.Ast.ExprId, Ast.ExprId).init(allocator),
            .instance = instance,
        };
        defer solver.deinit();

        const body = try solver.lowerDef(proc.body);
        const roots = solver.public_roots orelse lambdaInvariant("lambda-solved MIR built a procedure without public roots");
        const executable_key = try repr.executableSpecializationKeyForProc(
            allocator,
            &program.canonical_names,
            &program.types,
            &program.value_stores.items[i],
            proc.proc,
            roots,
        );

        program.solve_sessions.items[i].state = .sealed;
        program.proc_instances.appendAssumeCapacity(.{
            .proc = proc.proc,
            .executable_specialization_key = executable_key,
            .solve_session = session_id,
            .value_store = value_store_id,
            .public_roots = roots,
        });
        program.procs.appendAssumeCapacity(.{
            .proc = proc.proc,
            .body = body,
            .representation_instance = instance,
        });
    }
    try program.root_procs.appendSlice(allocator, input.root_procs.items);

    input.deinit();
    return program;
}

const TypeImporter = struct {
    allocator: Allocator,
    input: *const Lifted.Type.Store,
    output: *Type.Store,
    active: std.AutoHashMap(Lifted.Type.TypeId, Type.TypeVarId),

    fn init(allocator: Allocator, input: *const Lifted.Type.Store, output: *Type.Store) TypeImporter {
        return .{
            .allocator = allocator,
            .input = input,
            .output = output,
            .active = std.AutoHashMap(Lifted.Type.TypeId, Type.TypeVarId).init(allocator),
        };
    }

    fn deinit(self: *TypeImporter) void {
        self.active.deinit();
    }

    fn importType(self: *TypeImporter, source: Lifted.Type.TypeId) Allocator.Error!Type.TypeVarId {
        switch (self.input.getTypePreservingNominal(source)) {
            .link => |next| return try self.importType(next),
            else => {},
        }

        if (self.active.get(source)) |existing| return existing;

        const target = try self.output.freshUnbd();
        try self.active.put(source, target);
        errdefer _ = self.active.remove(source);

        const node: Type.Node = switch (self.input.getTypePreservingNominal(source)) {
            .placeholder,
            .unbd,
            => lambdaInvariant("lambda-solved type import received unresolved lifted type"),
            .link => unreachable,
            .primitive => |prim| .{ .content = .{ .primitive = prim } },
            .func => |func| blk: {
                const args = try self.allocator.alloc(Type.TypeVarId, func.args.len);
                defer self.allocator.free(args);
                for (func.args, 0..) |arg, i| {
                    args[i] = try self.importType(arg);
                }
                const ret = try self.importType(func.ret);
                break :blk .{ .content = .{ .func = .{
                    .fixed_arity = @intCast(func.args.len),
                    .args = try self.output.addTypeVarSpan(args),
                    .ret = ret,
                    .callable = self.output.freshCallableVar(),
                } } };
            },
            .nominal => |nominal| blk: {
                const args = try self.allocator.alloc(Type.TypeVarId, nominal.args.len);
                defer self.allocator.free(args);
                for (nominal.args, 0..) |arg, i| {
                    args[i] = try self.importType(arg);
                }
                break :blk .{ .nominal = .{
                    .nominal = nominal.nominal,
                    .is_opaque = nominal.is_opaque,
                    .args = try self.output.addTypeVarSpan(args),
                    .backing = try self.importType(nominal.backing),
                } };
            },
            .list => |elem| .{ .content = .{ .list = try self.importType(elem) } },
            .box => |elem| .{ .content = .{ .box = try self.importType(elem) } },
            .tuple => |elems| blk: {
                const items = try self.allocator.alloc(Type.TypeVarId, elems.len);
                defer self.allocator.free(items);
                for (elems, 0..) |elem, i| {
                    items[i] = try self.importType(elem);
                }
                break :blk .{ .content = .{ .tuple = try self.output.addTypeVarSpan(items) } };
            },
            .tag_union => |tag_union| blk: {
                const tags = try self.allocator.alloc(Type.Tag, tag_union.tags.len);
                defer self.allocator.free(tags);
                for (tag_union.tags, 0..) |tag, i| {
                    const args = try self.allocator.alloc(Type.TypeVarId, tag.args.len);
                    defer self.allocator.free(args);
                    for (tag.args, 0..) |arg, j| {
                        args[j] = try self.importType(arg);
                    }
                    tags[i] = .{
                        .name = tag.name,
                        .args = try self.output.addTypeVarSpan(args),
                    };
                }
                break :blk .{ .content = .{ .tag_union = .{ .tags = try self.output.addTags(tags) } } };
            },
            .record => |record| blk: {
                const fields = try self.allocator.alloc(Type.Field, record.fields.len);
                defer self.allocator.free(fields);
                for (record.fields, 0..) |field, i| {
                    fields[i] = .{
                        .name = field.name,
                        .ty = try self.importType(field.ty),
                    };
                }
                break :blk .{ .content = .{ .record = .{ .fields = try self.output.addFields(fields) } } };
            },
        };

        self.output.setNode(target, node);
        _ = self.active.remove(source);
        return target;
    }
};

const BodySolver = struct {
    allocator: Allocator,
    input: *const Lifted.Ast.Store,
    output: *Ast.Store,
    canonical_names: *const canonical.CanonicalNameStore,
    type_importer: *TypeImporter,
    representation_store: *repr.RepresentationStore,
    value_store: *repr.ValueInfoStore,
    env: std.AutoHashMap(Ast.Symbol, repr.BindingInfoId),
    expr_map: std.AutoHashMap(Lifted.Ast.ExprId, Ast.ExprId),
    instance: repr.ProcRepresentationInstanceId,
    public_roots: ?repr.ProcPublicValueRoots = null,

    fn deinit(self: *BodySolver) void {
        self.expr_map.deinit();
        self.env.deinit();
    }

    fn lowerDef(self: *BodySolver, def_id: Lifted.Ast.DefId) Allocator.Error!Ast.DefId {
        const def = self.input.getDef(def_id);
        return try self.output.addDef(.{
            .proc = def.proc,
            .value = switch (def.value) {
                .fn_ => |fn_| blk: {
                    const lowered_args = try self.lowerParamSpan(fn_.args);
                    const capture_values = try self.lowerCaptureSlotRoots(fn_.captures);
                    const body = try self.lowerExpr(fn_.body);
                    const body_value = self.exprValue(body);
                    const function_root = self.representation_store.reserveRoot();
                    self.public_roots = .{
                        .params = lowered_args.values,
                        .ret = body_value,
                        .captures = capture_values,
                        .function_root = function_root,
                    };
                    break :blk .{ .fn_ = .{
                        .args = lowered_args.symbols,
                        .body = body,
                        .representation_instance = self.instance,
                    } };
                },
                .hosted_fn => |hosted| blk: {
                    const lowered_args = try self.lowerParamSpan(hosted.args);
                    const ret_ty = try self.type_importer.importType(hosted.ret_ty);
                    const ret = try self.newValue(ret_ty);
                    self.public_roots = .{
                        .params = lowered_args.values,
                        .ret = ret,
                        .captures = repr.Span(repr.ValueInfoId).empty(),
                        .function_root = self.representation_store.reserveRoot(),
                    };
                    break :blk .{ .hosted_fn = .{
                        .proc = hosted.proc,
                        .args = lowered_args.symbols,
                        .ret_ty = ret_ty,
                        .hosted = hosted.hosted,
                    } };
                },
                .val => |expr| blk: {
                    const body = try self.lowerExpr(expr);
                    self.public_roots = .{
                        .params = repr.Span(repr.ValueInfoId).empty(),
                        .ret = self.exprValue(body),
                        .captures = repr.Span(repr.ValueInfoId).empty(),
                        .function_root = self.representation_store.reserveRoot(),
                    };
                    break :blk .{ .val = body };
                },
                .run => |run| blk: {
                    const body = try self.lowerExpr(run.body);
                    self.public_roots = .{
                        .params = repr.Span(repr.ValueInfoId).empty(),
                        .ret = self.exprValue(body),
                        .captures = repr.Span(repr.ValueInfoId).empty(),
                        .function_root = self.representation_store.reserveRoot(),
                    };
                    break :blk .{ .run = .{ .body = body } };
                },
            },
        });
    }

    const LoweredParams = struct {
        symbols: Ast.Span(Ast.TypedSymbol),
        values: repr.Span(repr.ValueInfoId),
    };

    fn lowerParamSpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.TypedSymbol)) Allocator.Error!LoweredParams {
        const input_items = self.input.sliceTypedSymbolSpan(span);
        if (input_items.len == 0) return .{
            .symbols = Ast.Span(Ast.TypedSymbol).empty(),
            .values = repr.Span(repr.ValueInfoId).empty(),
        };
        const symbols = try self.allocator.alloc(Ast.TypedSymbol, input_items.len);
        defer self.allocator.free(symbols);
        const values = try self.allocator.alloc(repr.ValueInfoId, input_items.len);
        defer self.allocator.free(values);
        for (input_items, 0..) |param, i| {
            const ty = try self.type_importer.importType(param.ty);
            const value = try self.newValue(ty);
            const binding = try self.value_store.addBinding(.{
                .symbol = param.symbol,
                .value = value,
                .root = self.valueRoot(value),
            });
            try self.env.put(param.symbol, binding);
            symbols[i] = .{
                .ty = ty,
                .symbol = param.symbol,
                .binding_info = binding,
            };
            values[i] = value;
        }
        return .{
            .symbols = try self.output.addTypedSymbolSpan(symbols),
            .values = try self.value_store.addValueSpan(values),
        };
    }

    fn lowerCaptureSlotRoots(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.CaptureSlot)) Allocator.Error!repr.Span(repr.ValueInfoId) {
        const input_items = self.input.sliceCaptureSlotSpan(span);
        if (input_items.len == 0) return repr.Span(repr.ValueInfoId).empty();
        const values = try self.allocator.alloc(repr.ValueInfoId, input_items.len);
        defer self.allocator.free(values);
        for (input_items, 0..) |slot, i| {
            const ty = try self.type_importer.importType(slot.ty);
            const value = try self.newValue(ty);
            const binding = try self.value_store.addBinding(.{
                .symbol = slot.source_symbol,
                .value = value,
                .root = self.valueRoot(value),
            });
            try self.env.put(slot.source_symbol, binding);
            values[i] = value;
        }
        return try self.value_store.addValueSpan(values);
    }

    fn lowerExpr(self: *BodySolver, expr_id: Lifted.Ast.ExprId) Allocator.Error!Ast.ExprId {
        if (self.expr_map.get(expr_id)) |existing| return existing;

        const expr = self.input.getExpr(expr_id);
        const ty = try self.type_importer.importType(expr.ty);
        const value = try self.newValue(ty);
        const lowered = try self.output.addExpr(ty, value, switch (expr.data) {
            .var_ => |symbol| .{ .var_ = .{
                .symbol = symbol,
                .binding_info = self.env.get(symbol) orelse lambdaInvariant("lambda-solved variable occurrence has no published binding info"),
            } },
            .capture_ref => |slot| .{ .capture_ref = slot },
            .int_lit => |literal| .{ .int_lit = literal },
            .frac_f32_lit => |literal| .{ .frac_f32_lit = literal },
            .frac_f64_lit => |literal| .{ .frac_f64_lit = literal },
            .dec_lit => |literal| .{ .dec_lit = literal },
            .bool_lit => |literal| .{ .bool_lit = literal },
            .str_lit => |literal| .{ .str_lit = literal },
            .const_ref => |const_ref| .{ .const_ref = const_ref },
            .tag => |tag| blk: {
                const eval_order = try self.lowerTagPayloadEvalSpan(tag.eval_order);
                const assembly_order = try self.lowerTagPayloadAssemblySpan(tag.assembly_order);
                try self.publishTagAggregate(value, tag.union_shape, tag.tag, assembly_order);
                break :blk .{ .tag = .{
                    .union_shape = tag.union_shape,
                    .tag = tag.tag,
                    .eval_order = eval_order,
                    .assembly_order = assembly_order,
                    .constructor_ty = try self.type_importer.importType(tag.constructor_ty),
                } };
            },
            .record => |record| blk: {
                const eval_order = try self.lowerRecordFieldEvalSpan(record.eval_order);
                const assembly_order = try self.lowerRecordFieldAssemblySpan(record.assembly_order);
                try self.publishRecordAggregate(value, assembly_order);
                break :blk .{ .record = .{
                    .shape = record.shape,
                    .eval_order = eval_order,
                    .assembly_order = assembly_order,
                } };
            },
            .nominal_reinterpret => |backing| .{ .nominal_reinterpret = try self.lowerExpr(backing) },
            .access => |access| blk: {
                const record = try self.lowerExpr(access.record);
                const projection = try self.value_store.addProjection(.{
                    .source = self.exprValue(record),
                    .result = value,
                    .root = self.valueRoot(value),
                    .kind = .{ .record_field = access.field },
                });
                break :blk .{ .access = .{
                    .record = record,
                    .field = access.field,
                    .projection_info = projection,
                } };
            },
            .structural_eq => |eq| .{ .structural_eq = .{
                .lhs = try self.lowerExpr(eq.lhs),
                .rhs = try self.lowerExpr(eq.rhs),
            } },
            .bool_not => |child| .{ .bool_not = try self.lowerExpr(child) },
            .let_ => |let_| blk: {
                const body = try self.lowerExpr(let_.body);
                const bind_ty = try self.type_importer.importType(let_.bind.ty);
                const binding = try self.value_store.addBinding(.{
                    .symbol = let_.bind.symbol,
                    .value = self.exprValue(body),
                    .root = self.valueRoot(self.exprValue(body)),
                });
                const previous = try self.env.fetchPut(let_.bind.symbol, binding);
                defer {
                    if (previous) |entry| {
                        self.env.put(let_.bind.symbol, entry.value) catch unreachable;
                    } else {
                        _ = self.env.remove(let_.bind.symbol);
                    }
                }
                break :blk .{ .let_ = .{
                    .bind = .{
                        .ty = bind_ty,
                        .symbol = let_.bind.symbol,
                        .binding_info = binding,
                    },
                    .body = body,
                    .rest = try self.lowerExpr(let_.rest),
                } };
            },
            .call_value => |call| blk: {
                const func = try self.lowerExpr(call.func);
                const lowered_args = try self.lowerExprSpanWithValues(call.args);
                const requested_fn_ty = try self.type_importer.importType(call.requested_fn_ty);
                const call_site = try self.value_store.addCallSite(.{
                    .callee = self.exprValue(func),
                    .args = lowered_args.values,
                    .result = value,
                    .requested_fn_root = self.representation_store.reserveRoot(),
                });
                break :blk .{ .call_value = .{
                    .func = func,
                    .args = lowered_args.exprs,
                    .requested_fn_ty = requested_fn_ty,
                    .call_site = call_site,
                } };
            },
            .call_proc => |call| blk: {
                const lowered_args = try self.lowerExprSpanWithValues(call.args);
                const requested_fn_ty = try self.type_importer.importType(call.requested_fn_ty);
                const call_site = try self.value_store.addCallSite(.{
                    .callee = null,
                    .args = lowered_args.values,
                    .result = value,
                    .requested_fn_root = self.representation_store.reserveRoot(),
                });
                break :blk .{ .call_proc = .{
                    .proc = call.proc,
                    .args = lowered_args.exprs,
                    .requested_fn_ty = requested_fn_ty,
                    .call_site = call_site,
                } };
            },
            .proc_value => |proc_value| blk: {
                const captures = try self.lowerCaptureArgSpanWithValues(proc_value.captures);
                const callable = try self.representation_store.addSingletonProcValueCallable(
                    self.canonical_names,
                    self.type_importer.output,
                    self.value_store,
                    value,
                    self.valueRoot(value),
                    proc_value.proc,
                    self.value_store.sliceValueSpan(captures.values),
                );
                self.value_store.values.items[@intFromEnum(value)].callable = callable;
                break :blk .{ .proc_value = .{
                    .proc = proc_value.proc,
                    .captures = captures.args,
                    .fn_ty = try self.type_importer.importType(proc_value.fn_ty),
                } };
            },
            .inspect => |child| .{ .inspect = try self.lowerExpr(child) },
            .low_level => |low_level| .{ .low_level = .{
                .op = low_level.op,
                .args = try self.lowerExprSpan(low_level.args),
                .source_constraint_ty = try self.type_importer.importType(low_level.source_constraint_ty),
            } },
            .block => |block| .{ .block = .{
                .stmts = try self.lowerStmtSpan(block.stmts),
                .final_expr = try self.lowerExpr(block.final_expr),
            } },
            .tuple => |items| blk: {
                const lowered_items = try self.lowerExprSpanWithValues(items);
                try self.publishTupleAggregate(value, lowered_items.values);
                break :blk .{ .tuple = lowered_items.exprs };
            },
            .tag_payload => |payload| blk: {
                const tag_union = try self.lowerExpr(payload.tag_union);
                const projection = try self.value_store.addProjection(.{
                    .source = self.exprValue(tag_union),
                    .result = value,
                    .root = self.valueRoot(value),
                    .kind = .{ .tag_payload = payload.payload },
                });
                break :blk .{ .tag_payload = .{
                    .tag_union = tag_union,
                    .payload = payload.payload,
                    .projection_info = projection,
                } };
            },
            .tuple_access => |access| blk: {
                const tuple = try self.lowerExpr(access.tuple);
                const projection = try self.value_store.addProjection(.{
                    .source = self.exprValue(tuple),
                    .result = value,
                    .root = self.valueRoot(value),
                    .kind = .{ .tuple_elem = access.elem_index },
                });
                break :blk .{ .tuple_access = .{
                    .tuple = tuple,
                    .elem_index = access.elem_index,
                    .projection_info = projection,
                } };
            },
            .list => |items| blk: {
                const lowered_items = try self.lowerExprSpanWithValues(items);
                try self.publishListAggregate(value, lowered_items.values);
                break :blk .{ .list = lowered_items.exprs };
            },
            .unit => .unit,
            .return_ => |child| .{ .return_ = try self.lowerExpr(child) },
            .crash => |literal| .{ .crash = literal },
            .runtime_error => .runtime_error,
            .match_ => |match_| blk: {
                const cond = try self.lowerExpr(match_.cond);
                const lowered_branches = try self.lowerBranchSpan(match_.branches);
                const branch_values = try self.valuesForBranches(lowered_branches);
                const join_info = try self.value_store.addJoin(.{
                    .result = value,
                    .inputs = branch_values,
                    .root = self.valueRoot(value),
                    .kind = .match_expr,
                });
                break :blk .{ .match_ = .{
                    .cond = cond,
                    .branches = lowered_branches,
                    .is_try_suffix = match_.is_try_suffix,
                    .join_info = join_info,
                } };
            },
            .if_ => |if_| blk: {
                const cond = try self.lowerExpr(if_.cond);
                const then_body = try self.lowerExpr(if_.then_body);
                const else_body = try self.lowerExpr(if_.else_body);
                const inputs = [_]repr.ValueInfoId{ self.exprValue(then_body), self.exprValue(else_body) };
                const join_info = try self.value_store.addJoin(.{
                    .result = value,
                    .inputs = try self.value_store.addValueSpan(&inputs),
                    .root = self.valueRoot(value),
                    .kind = .if_expr,
                });
                break :blk .{ .if_ = .{
                    .cond = cond,
                    .then_body = then_body,
                    .else_body = else_body,
                    .join_info = join_info,
                } };
            },
            .for_ => |for_| try self.lowerForExpr(value, for_),
        });
        try self.expr_map.put(expr_id, lowered);
        return lowered;
    }

    const SavedBinding = struct {
        symbol: Ast.Symbol,
        previous: ?repr.BindingInfoId,
    };

    fn lowerPatScoped(
        self: *BodySolver,
        pat_id: Lifted.Ast.PatId,
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.PatId {
        const pat = self.input.getPat(pat_id);
        const ty = try self.type_importer.importType(pat.ty);
        const value = try self.newValue(ty);
        return try self.output.addPat(.{ .ty = ty, .value_info = value, .data = switch (pat.data) {
            .bool_lit => |literal| .{ .bool_lit = literal },
            .int_lit => |literal| .{ .int_lit = literal },
            .frac_f32_lit => |literal| .{ .frac_f32_lit = literal },
            .frac_f64_lit => |literal| .{ .frac_f64_lit = literal },
            .dec_lit => |literal| .{ .dec_lit = literal },
            .str_lit => |literal| .{ .str_lit = literal },
            .wildcard => .wildcard,
            .var_ => |symbol| blk: {
                const binding = try self.value_store.addBinding(.{
                    .symbol = symbol,
                    .value = value,
                    .root = self.valueRoot(value),
                });
                const previous = try self.env.fetchPut(symbol, binding);
                try saved.append(self.allocator, .{
                    .symbol = symbol,
                    .previous = if (previous) |entry| entry.value else null,
                });
                break :blk .{ .var_ = .{
                    .symbol = symbol,
                    .binding_info = binding,
                } };
            },
            .tag => |tag| .{ .tag = .{
                .union_shape = tag.union_shape,
                .tag = tag.tag,
                .payloads = try self.lowerTagPayloadPatternSpan(tag.payloads, saved),
            } },
        } });
    }

    fn restoreBindings(self: *BodySolver, saved: *std.ArrayList(SavedBinding), start: usize) void {
        while (saved.items.len > start) {
            const binding = saved.pop().?;
            if (binding.previous) |previous| {
                self.env.put(binding.symbol, previous) catch unreachable;
            } else {
                _ = self.env.remove(binding.symbol);
            }
        }
    }

    fn lowerBranch(self: *BodySolver, branch_id: Lifted.Ast.BranchId) Allocator.Error!Ast.BranchId {
        const branch = self.input.getBranch(branch_id);
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

    fn lowerForExpr(self: *BodySolver, value: repr.ValueInfoId, for_: anytype) Allocator.Error!Ast.Expr.Data {
        _ = value;
        var saved = std.ArrayList(SavedBinding).empty;
        defer saved.deinit(self.allocator);
        const patt = try self.lowerPatScoped(for_.patt, &saved);
        defer self.restoreBindings(&saved, 0);
        return .{ .for_ = .{
            .patt = patt,
            .iterable = try self.lowerExpr(for_.iterable),
            .body = try self.lowerExpr(for_.body),
        } };
    }

    fn lowerStmt(self: *BodySolver, stmt_id: Lifted.Ast.StmtId) Allocator.Error!Ast.StmtId {
        const stmt = self.input.getStmt(stmt_id);
        return try self.output.addStmt(switch (stmt) {
            .decl => |decl| blk: {
                const body = try self.lowerExpr(decl.body);
                const bind_ty = try self.type_importer.importType(decl.bind.ty);
                const binding = try self.value_store.addBinding(.{
                    .symbol = decl.bind.symbol,
                    .value = self.exprValue(body),
                    .root = self.valueRoot(self.exprValue(body)),
                });
                try self.env.put(decl.bind.symbol, binding);
                break :blk .{ .decl = .{
                    .bind = .{
                        .ty = bind_ty,
                        .symbol = decl.bind.symbol,
                        .binding_info = binding,
                    },
                    .body = body,
                } };
            },
            .var_decl => |decl| blk: {
                const body = try self.lowerExpr(decl.body);
                const bind_ty = try self.type_importer.importType(decl.bind.ty);
                const binding = try self.value_store.addBinding(.{
                    .symbol = decl.bind.symbol,
                    .value = self.exprValue(body),
                    .root = self.valueRoot(self.exprValue(body)),
                });
                try self.env.put(decl.bind.symbol, binding);
                break :blk .{ .var_decl = .{
                    .bind = .{
                        .ty = bind_ty,
                        .symbol = decl.bind.symbol,
                        .binding_info = binding,
                    },
                    .body = body,
                } };
            },
            .reassign => |reassign| blk: {
                const body = try self.lowerExpr(reassign.body);
                const binding = self.env.get(reassign.target) orelse lambdaInvariant("lambda-solved reassignment target has no binding info");
                break :blk .{ .reassign = .{
                    .target = reassign.target,
                    .version = binding,
                    .body = body,
                } };
            },
            .expr => |expr| .{ .expr = try self.lowerExpr(expr) },
            .debug => |expr| .{ .debug = try self.lowerExpr(expr) },
            .expect => |expr| .{ .expect = try self.lowerExpr(expr) },
            .crash => |literal| .{ .crash = literal },
            .return_ => |expr| .{ .return_ = try self.lowerExpr(expr) },
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

    const LoweredExprSpan = struct {
        exprs: Ast.Span(Ast.ExprId),
        values: repr.Span(repr.ValueInfoId),
    };

    fn lowerExprSpanWithValues(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.ExprId)) Allocator.Error!LoweredExprSpan {
        const input_items = self.input.sliceExprSpan(span);
        if (input_items.len == 0) return .{
            .exprs = Ast.Span(Ast.ExprId).empty(),
            .values = repr.Span(repr.ValueInfoId).empty(),
        };
        const exprs = try self.allocator.alloc(Ast.ExprId, input_items.len);
        defer self.allocator.free(exprs);
        const values = try self.allocator.alloc(repr.ValueInfoId, input_items.len);
        defer self.allocator.free(values);
        for (input_items, 0..) |expr, i| {
            exprs[i] = try self.lowerExpr(expr);
            values[i] = self.exprValue(exprs[i]);
        }
        return .{
            .exprs = try self.output.addExprSpan(exprs),
            .values = try self.value_store.addValueSpan(values),
        };
    }

    fn lowerExprSpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.ExprId)) Allocator.Error!Ast.Span(Ast.ExprId) {
        return (try self.lowerExprSpanWithValues(span)).exprs;
    }

    fn lowerStmtSpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.StmtId)) Allocator.Error!Ast.Span(Ast.StmtId) {
        const input_items = self.input.sliceStmtSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.StmtId).empty();
        const output_items = try self.allocator.alloc(Ast.StmtId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |stmt, i| {
            output_items[i] = try self.lowerStmt(stmt);
        }
        return try self.output.addStmtSpan(output_items);
    }

    fn lowerBranchSpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.BranchId)) Allocator.Error!Ast.Span(Ast.BranchId) {
        const input_items = self.input.sliceBranchSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.BranchId).empty();
        const output_items = try self.allocator.alloc(Ast.BranchId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |branch, i| {
            output_items[i] = try self.lowerBranch(branch);
        }
        return try self.output.addBranchSpan(output_items);
    }

    fn valuesForBranches(self: *BodySolver, span: Ast.Span(Ast.BranchId)) Allocator.Error!repr.Span(repr.ValueInfoId) {
        if (span.len == 0) return repr.Span(repr.ValueInfoId).empty();
        const branch_ids = self.output.branch_ids.items[span.start..][0..span.len];
        const values = try self.allocator.alloc(repr.ValueInfoId, branch_ids.len);
        defer self.allocator.free(values);
        for (branch_ids, 0..) |branch_id, i| {
            values[i] = self.exprValue(self.output.branches.items[@intFromEnum(branch_id)].body);
        }
        return try self.value_store.addValueSpan(values);
    }

    const LoweredCaptureArgs = struct {
        args: Ast.Span(Ast.CaptureArg),
        values: repr.Span(repr.ValueInfoId),
    };

    fn lowerCaptureArgSpanWithValues(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.CaptureArg)) Allocator.Error!LoweredCaptureArgs {
        const input_items = self.input.sliceCaptureArgSpan(span);
        if (input_items.len == 0) return .{
            .args = Ast.Span(Ast.CaptureArg).empty(),
            .values = repr.Span(repr.ValueInfoId).empty(),
        };
        const output_items = try self.allocator.alloc(Ast.CaptureArg, input_items.len);
        defer self.allocator.free(output_items);
        const values = try self.allocator.alloc(repr.ValueInfoId, input_items.len);
        defer self.allocator.free(values);
        for (input_items, 0..) |capture, i| {
            const expr = try self.lowerExpr(capture.expr);
            const value = self.exprValue(expr);
            output_items[i] = .{
                .slot = capture.slot,
                .value_info = value,
                .expr = expr,
            };
            values[i] = value;
        }
        return .{
            .args = try self.output.addCaptureArgSpan(output_items),
            .values = try self.value_store.addValueSpan(values),
        };
    }

    fn lowerRecordFieldEvalSpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.RecordFieldEval)) Allocator.Error!Ast.Span(Ast.RecordFieldEval) {
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

    fn lowerRecordFieldAssemblySpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.RecordFieldAssembly)) Allocator.Error!Ast.Span(Ast.RecordFieldAssembly) {
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

    fn lowerTagPayloadEvalSpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.TagPayloadEval)) Allocator.Error!Ast.Span(Ast.TagPayloadEval) {
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

    fn lowerTagPayloadAssemblySpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.TagPayloadAssembly)) Allocator.Error!Ast.Span(Ast.TagPayloadAssembly) {
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

    fn publishRecordAggregate(
        self: *BodySolver,
        value: repr.ValueInfoId,
        assembly_order: Ast.Span(Ast.RecordFieldAssembly),
    ) Allocator.Error!void {
        const assemblies = self.output.record_field_assemblies.items[assembly_order.start..][0..assembly_order.len];
        const fields = try self.allocator.alloc(repr.FieldValueInfo, assemblies.len);
        errdefer if (fields.len > 0) self.allocator.free(fields);

        for (assemblies, 0..) |field, i| {
            fields[i] = .{
                .field = field.field,
                .value = self.exprValue(field.value),
            };
        }
        self.publishAggregate(value, .{ .record = fields });
    }

    fn publishTupleAggregate(
        self: *BodySolver,
        value: repr.ValueInfoId,
        elems: repr.Span(repr.ValueInfoId),
    ) Allocator.Error!void {
        const elem_values = self.value_store.sliceValueSpan(elems);
        const infos = try self.allocator.alloc(repr.ElemValueInfo, elem_values.len);
        errdefer if (infos.len > 0) self.allocator.free(infos);

        for (elem_values, 0..) |elem, i| {
            infos[i] = .{
                .index = @intCast(i),
                .value = elem,
            };
        }
        self.publishAggregate(value, .{ .tuple = infos });
    }

    fn publishTagAggregate(
        self: *BodySolver,
        value: repr.ValueInfoId,
        union_shape: MonoRow.TagUnionShapeId,
        tag_id: MonoRow.TagId,
        assembly_order: Ast.Span(Ast.TagPayloadAssembly),
    ) Allocator.Error!void {
        const assemblies = self.output.tag_payload_assemblies.items[assembly_order.start..][0..assembly_order.len];
        const payloads = try self.allocator.alloc(repr.TagPayloadValueInfo, assemblies.len);
        errdefer if (payloads.len > 0) self.allocator.free(payloads);

        for (assemblies, 0..) |payload, i| {
            payloads[i] = .{
                .payload = payload.payload,
                .value = self.exprValue(payload.value),
            };
        }
        self.publishAggregate(value, .{ .tag = .{
            .union_shape = union_shape,
            .tag = tag_id,
            .payloads = payloads,
        } });
    }

    fn publishListAggregate(
        self: *BodySolver,
        value: repr.ValueInfoId,
        elems: repr.Span(repr.ValueInfoId),
    ) Allocator.Error!void {
        const elem_values = self.value_store.sliceValueSpan(elems);
        const owned_elems = try self.allocator.dupe(repr.ValueInfoId, elem_values);
        errdefer if (owned_elems.len > 0) self.allocator.free(owned_elems);

        self.publishAggregate(value, .{ .list = .{
            .elem_root = self.representation_store.reserveRoot(),
            .elems = owned_elems,
        } });
    }

    fn publishAggregate(
        self: *BodySolver,
        value: repr.ValueInfoId,
        aggregate: repr.AggregateValueInfo,
    ) void {
        const value_info = &self.value_store.values.items[@intFromEnum(value)];
        if (value_info.aggregate != null) lambdaInvariant("lambda-solved value published aggregate metadata twice");
        value_info.aggregate = aggregate;
    }

    fn lowerTagPayloadPatternSpan(
        self: *BodySolver,
        span: Lifted.Ast.Span(Lifted.Ast.TagPayloadPattern),
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.Span(Ast.TagPayloadPattern) {
        const input_items = self.input.sliceTagPayloadPatternSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.TagPayloadPattern).empty();
        const output_items = try self.allocator.alloc(Ast.TagPayloadPattern, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |payload, i| {
            output_items[i] = .{
                .payload = payload.payload,
                .pattern = try self.lowerPatScoped(payload.pattern, saved),
            };
        }
        return try self.output.addTagPayloadPatternSpan(output_items);
    }

    fn newValue(self: *BodySolver, ty: Type.TypeVarId) Allocator.Error!repr.ValueInfoId {
        const root = self.representation_store.reserveRoot();
        const class = self.representation_store.reserveClass();
        return try self.value_store.addValue(.{
            .logical_ty = ty,
            .root = root,
            .solved_class = class,
        });
    }

    fn exprValue(self: *const BodySolver, expr: Ast.ExprId) repr.ValueInfoId {
        return self.output.exprs.items[@intFromEnum(expr)].value_info;
    }

    fn valueRoot(self: *const BodySolver, value: repr.ValueInfoId) repr.RepRootId {
        return self.value_store.values.items[@intFromEnum(value)].root;
    }
};

fn lambdaInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) std.debug.panic(message, .{});
    unreachable;
}

test "lambda-solved program owns representation tables" {
    std.testing.refAllDecls(@This());
}
