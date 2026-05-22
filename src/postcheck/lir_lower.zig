//! Direct Lambda Mono IR to LIR lowering.

const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
const can = @import("can");
const check = @import("check");
const layout = @import("layout");
const Common = @import("common.zig");
const Mono = @import("monotype/ast.zig");
const LambdaMono = @import("lambda_mono/ast.zig");
const MonoType = @import("monotype/type.zig");
const Type = @import("lambda_mono/type.zig");
const lir_core = @import("lir_core");
const LIR = lir_core.LIR;
const LirProgram = lir_core.Program;
const RootMetadata = lir_core.RootMetadata.RootMetadata;
const checked = check.CheckedModule;

pub const RuntimeRecordFieldSchema = struct {
    name: []const u8,
    logical_index: u32,
};

pub const RuntimeRecordSchema = struct {
    type_name: []const u8,
    fields: []const RuntimeRecordFieldSchema,

    pub fn fieldLogicalIndex(self: RuntimeRecordSchema, field_name: []const u8) ?u32 {
        for (self.fields) |field| {
            if (std.mem.eql(u8, field.name, field_name)) return field.logical_index;
        }
        return null;
    }
};

pub const RuntimeTagSchema = struct {
    name: []const u8,
    discriminant: u16,
};

pub const RuntimeTagUnionSchema = struct {
    type_name: []const u8,
    tags: []const RuntimeTagSchema,

    pub fn tagDiscriminant(self: RuntimeTagUnionSchema, tag_name: []const u8) ?u16 {
        for (self.tags) |tag| {
            if (std.mem.eql(u8, tag.name, tag_name)) return tag.discriminant;
        }
        return null;
    }
};

pub const RuntimeSchemaStore = struct {
    allocator: std.mem.Allocator,
    records: std.ArrayList(RuntimeRecordSchema),
    tag_unions: std.ArrayList(RuntimeTagUnionSchema),

    pub fn init(allocator: std.mem.Allocator) RuntimeSchemaStore {
        return .{
            .allocator = allocator,
            .records = .empty,
            .tag_unions = .empty,
        };
    }

    pub fn deinit(self: *RuntimeSchemaStore) void {
        for (self.records.items) |schema| {
            for (schema.fields) |field| self.allocator.free(field.name);
            self.allocator.free(schema.fields);
            self.allocator.free(schema.type_name);
        }
        for (self.tag_unions.items) |schema| {
            for (schema.tags) |tag| self.allocator.free(tag.name);
            self.allocator.free(schema.tags);
            self.allocator.free(schema.type_name);
        }
        self.tag_unions.deinit(self.allocator);
        self.records.deinit(self.allocator);
    }
};

pub const Output = struct {
    lir_result: LirProgram.Result,
    runtime_schemas: RuntimeSchemaStore,

    pub fn deinit(self: *Output) void {
        self.runtime_schemas.deinit();
        self.lir_result.deinit();
    }
};

pub fn run(
    allocator: std.mem.Allocator,
    target_usize: base.target.TargetUsize,
    program: LambdaMono.Program,
) Common.LowerError!Output {
    var owned = program;
    errdefer owned.deinit();

    var lowerer = try Lowerer.init(allocator, target_usize, &owned);
    errdefer lowerer.deinit();

    try lowerer.registerProcPlaceholders();
    try lowerer.lowerAllFns();
    try lowerer.bindRoots();
    try lowerer.publishRuntimeSchemas();

    owned.deinit();
    return lowerer.finish();
}

const Lowerer = struct {
    allocator: std.mem.Allocator,
    program: *const LambdaMono.Program,
    result: LirProgram.Result,
    runtime_schemas: RuntimeSchemaStore,
    fn_map: []LIR.LirProcSpecId,
    local_map: []?LIR.LocalId,
    type_layouts: []?layout.Idx,
    const_plan_map: []?LirProgram.ConstPlanId,
    next_join_point: u32 = 0,
    loop_stack: std.ArrayList(LoopContext),

    const LoopContext = struct {
        join_id: LIR.JoinPointId,
        params: LIR.LocalSpan,
        result_target: LIR.LocalId,
        after_loop: LIR.CFStmtId,
    };

    fn init(
        allocator: std.mem.Allocator,
        target_usize: base.target.TargetUsize,
        program: *const LambdaMono.Program,
    ) Common.LowerError!Lowerer {
        const fn_map = try allocator.alloc(LIR.LirProcSpecId, program.fns.items.len);
        errdefer allocator.free(fn_map);

        const local_map = try allocator.alloc(?LIR.LocalId, program.locals.items.len);
        errdefer allocator.free(local_map);
        @memset(local_map, null);

        const type_layouts = try allocator.alloc(?layout.Idx, program.types.types.items.len);
        errdefer allocator.free(type_layouts);
        @memset(type_layouts, null);

        const const_plan_map = try allocator.alloc(?LirProgram.ConstPlanId, program.types.types.items.len);
        errdefer allocator.free(const_plan_map);
        @memset(const_plan_map, null);

        return .{
            .allocator = allocator,
            .program = program,
            .result = try LirProgram.Result.init(allocator, target_usize),
            .runtime_schemas = RuntimeSchemaStore.init(allocator),
            .fn_map = fn_map,
            .local_map = local_map,
            .type_layouts = type_layouts,
            .const_plan_map = const_plan_map,
            .loop_stack = .empty,
        };
    }

    fn deinit(self: *Lowerer) void {
        self.loop_stack.deinit(self.allocator);
        self.allocator.free(self.const_plan_map);
        self.allocator.free(self.type_layouts);
        self.allocator.free(self.local_map);
        self.allocator.free(self.fn_map);
        self.runtime_schemas.deinit();
        self.result.deinit();
    }

    fn finish(self: *Lowerer) Output {
        const output = Output{
            .lir_result = self.result,
            .runtime_schemas = self.runtime_schemas,
        };
        self.loop_stack.deinit(self.allocator);
        self.allocator.free(self.const_plan_map);
        self.allocator.free(self.type_layouts);
        self.allocator.free(self.local_map);
        self.allocator.free(self.fn_map);
        self.result = undefined;
        self.runtime_schemas = RuntimeSchemaStore.init(self.allocator);
        self.fn_map = &.{};
        self.local_map = &.{};
        self.type_layouts = &.{};
        self.const_plan_map = &.{};
        self.loop_stack = .empty;
        return output;
    }

    fn registerProcPlaceholders(self: *Lowerer) Common.LowerError!void {
        for (self.program.fns.items, 0..) |fn_, index| {
            const args = self.program.typedLocalSpan(fn_.args);
            const arg_locals = try self.allocator.alloc(LIR.LocalId, args.len);
            defer self.allocator.free(arg_locals);

            for (args, 0..) |arg, i| {
                arg_locals[i] = try self.localFor(arg.local);
            }

            const proc_id = try self.result.store.addProcSpec(.{
                .name = lirSymbol(fn_.symbol),
                .args = try self.result.store.addLocalSpan(arg_locals),
                .body = null,
                .ret_layout = try self.layoutOfType(fn_.ret),
            });
            self.fn_map[index] = proc_id;
        }
    }

    fn lowerAllFns(self: *Lowerer) Common.LowerError!void {
        for (self.program.fns.items, 0..) |fn_, index| {
            const proc_id = self.fn_map[index];
            const body = try self.lowerExprReturn(fn_.body);
            self.result.store.getProcSpecPtr(proc_id).body = body;
        }
    }

    fn bindRoots(self: *Lowerer) Common.LowerError!void {
        for (self.program.roots.items) |root| {
            try self.result.root_procs.append(self.allocator, self.fn_map[@intFromEnum(root.fn_id)]);
            try self.result.root_metadata.append(self.allocator, RootMetadata.fromCheckedRoot(root.request));
            if (root.request.abi == .compile_time) {
                const fn_ = self.program.fns.items[@intFromEnum(root.fn_id)];
                try self.result.const_roots.append(self.allocator, .{
                    .root_order = root.request.order,
                    .request = root.request,
                    .proc = self.fn_map[@intFromEnum(root.fn_id)],
                    .ret_layout = try self.layoutOfType(fn_.ret),
                    .plan = try self.constPlanOfType(fn_.ret),
                });
            }
        }
    }

    fn constPlanOfType(self: *Lowerer, ty: Type.TypeId) Common.LowerError!LirProgram.ConstPlanId {
        const index = @intFromEnum(ty);
        if (self.const_plan_map[index]) |existing| return existing;

        const id: LirProgram.ConstPlanId = @enumFromInt(@as(u32, @intCast(self.result.const_plans.items.len)));
        try self.result.const_plans.append(self.allocator, .pending);
        self.const_plan_map[index] = id;
        errdefer {
            if (self.const_plan_map[index] == id) self.const_plan_map[index] = null;
        }

        const plan = try self.buildConstPlan(ty);
        self.result.const_plans.items[@intFromEnum(id)] = plan;
        return id;
    }

    fn buildConstPlan(self: *Lowerer, ty: Type.TypeId) Common.LowerError!LirProgram.ConstPlan {
        return switch (self.program.types.get(ty)) {
            .primitive => |primitive| switch (primitive) {
                .bool => Common.invariant("primitive Bool reached ConstStore plan publication; Bool must be a checked named tag union"),
                .str => .str,
                .u8,
                .i8,
                .u16,
                .i16,
                .u32,
                .i32,
                .u64,
                .i64,
                .u128,
                .i128,
                .f32,
                .f64,
                .dec,
                => .scalar,
            },
            .zst => .zst,
            .list => |elem| .{ .list = try self.constPlanOfType(elem) },
            .box => |elem| .{ .box = try self.constPlanOfType(elem) },
            .tuple => |items| blk: {
                const source = self.program.types.span(items);
                const plans = try self.allocator.alloc(LirProgram.ConstPlanId, source.len);
                errdefer self.allocator.free(plans);
                for (source, 0..) |item, i| plans[i] = try self.constPlanOfType(item);
                break :blk .{ .tuple = plans };
            },
            .record => |fields| blk: {
                const source = self.program.types.fieldSpan(fields);
                const plans = try self.allocator.alloc(LirProgram.ConstPlanId, source.len);
                errdefer self.allocator.free(plans);
                for (source, 0..) |field, i| plans[i] = try self.constPlanOfType(field.ty);
                break :blk .{ .record = plans };
            },
            .tag_union => |tags| blk: {
                const source = self.program.types.tagSpan(tags);
                const variants = try self.allocator.alloc(LirProgram.ConstTagVariant, source.len);
                var initialized: usize = 0;
                errdefer {
                    for (variants[0..initialized]) |variant| self.allocator.free(variant.payloads);
                    self.allocator.free(variants);
                }
                for (source, 0..) |tag, i| {
                    const payload_tys = self.program.types.span(tag.payloads);
                    const payloads = try self.allocator.alloc(LirProgram.ConstPlanId, payload_tys.len);
                    var payloads_owned = true;
                    errdefer if (payloads_owned) self.allocator.free(payloads);
                    for (payload_tys, 0..) |payload_ty, j| payloads[j] = try self.constPlanOfType(payload_ty);
                    variants[i] = .{
                        .name = tag.name,
                        .discriminant = @intCast(i),
                        .payloads = payloads,
                    };
                    payloads_owned = false;
                    initialized += 1;
                }
                break :blk .{ .tag_union = variants };
            },
            .named => |named| blk: {
                const backing = named.backing orelse Common.invariant("named type without backing reached ConstStore plan publication");
                break :blk .{ .named = .{
                    .named_type = .{
                        .module = named.named_type.module,
                        .ty = named.named_type.ty,
                    },
                    .backing = try self.constPlanOfType(backing.ty),
                } };
            },
            .callable => |variants| .{ .fn_value = try self.fnSetForType(ty, variants) },
            .erased_fn => Common.invariant("erased function ConstStore publication requires explicit erased function entries"),
            .capture_record => Common.invariant("capture record reached root ConstStore plan publication"),
        };
    }

    fn fnSetForType(self: *Lowerer, ty: Type.TypeId, variants_span: Type.Span) Common.LowerError!LirProgram.FnSetId {
        const type_variants = self.program.types.fnVariantSpan(variants_span);
        const value_layout = try self.layoutOfType(ty);
        const variants = try self.allocator.alloc(LirProgram.FnVariant, type_variants.len);
        var initialized: usize = 0;
        errdefer {
            for (variants[0..initialized]) |variant| {
                if (variant.captures.len > 0) self.allocator.free(variant.captures);
            }
            self.allocator.free(variants);
        }

        for (type_variants, 0..) |variant, index| {
            const captures = if (variant.capture_ty) |capture_ty|
                try self.captureSlotsForType(capture_ty)
            else
                &.{};
            var captures_owned = captures.len > 0;
            errdefer if (captures_owned) self.allocator.free(captures);

            variants[index] = .{
                .id = @enumFromInt(@as(u32, @intCast(index))),
                .discriminant = @intCast(index),
                .variant_index = @intCast(index),
                .payload_layout = if (variant.capture_ty) |capture_ty|
                    try self.callablePayloadLayout(value_layout, @intCast(index), capture_ty)
                else
                    .zst,
                .template = constFnTemplateFromMono(self.fnTemplateForSymbol(variant.lambda)),
                .captures = captures,
            };
            captures_owned = false;
            initialized += 1;
        }

        const id: LirProgram.FnSetId = @enumFromInt(@as(u32, @intCast(self.result.fn_sets.items.len)));
        try self.result.fn_sets.append(self.allocator, .{
            .layout = value_layout,
            .variants = variants,
        });
        return id;
    }

    fn callablePayloadLayout(
        self: *Lowerer,
        value_layout: layout.Idx,
        variant_index: u16,
        capture_ty: Type.TypeId,
    ) Common.LowerError!layout.Idx {
        const capture_layout = try self.layoutOfType(capture_ty);
        const value_layout_data = self.result.layouts.getLayout(value_layout);
        if (self.result.layouts.isZeroSized(value_layout_data)) {
            if (!self.result.layouts.isZeroSized(self.result.layouts.getLayout(capture_layout))) {
                Common.invariant("callable value layout is zero-sized but capture payload is not");
            }
            return .zst;
        }
        return self.tagUnionPayloadLayout(value_layout, variant_index);
    }

    fn captureSlotsForType(self: *Lowerer, ty: Type.TypeId) Common.LowerError![]const LirProgram.CaptureSlot {
        const fields = switch (self.program.types.get(ty)) {
            .capture_record => |fields| self.program.types.captureFieldSpan(fields),
            else => Common.invariant("function result capture slot publication expected capture record type"),
        };
        const slots = try self.allocator.alloc(LirProgram.CaptureSlot, fields.len);
        errdefer self.allocator.free(slots);
        for (fields, 0..) |field, index| {
            slots[index] = .{
                .binder = field.binder orelse Common.invariant("function result capture had no checked binder"),
                .slot = @intCast(index),
                .plan = try self.constPlanOfType(field.ty),
            };
        }
        return slots;
    }

    fn fnTemplateForSymbol(self: *Lowerer, symbol: Common.Symbol) Mono.FnTemplate {
        for (self.program.fns.items) |fn_| {
            if (fn_.symbol != symbol) continue;
            return fn_.source orelse Common.invariant("function result referenced a generated function without checked source identity");
        }
        Common.invariant("function result referenced a missing function symbol");
    }

    fn publishRuntimeSchemas(self: *Lowerer) Common.LowerError!void {
        for (self.program.types.types.items) |content| {
            switch (content) {
                .named => |named| {
                    const backing = named.backing orelse continue;
                    const type_name = self.program.names.typeNameText(named.def.type_name);
                    switch (self.program.types.get(backing.ty)) {
                        .record => |fields| try self.publishRecordSchema(type_name, fields),
                        .tag_union => |tags| try self.publishTagUnionSchema(type_name, tags),
                        else => {},
                    }
                },
                else => {},
            }
        }
    }

    fn publishRecordSchema(self: *Lowerer, type_name: []const u8, fields: Type.Span) Common.LowerError!void {
        const source = self.program.types.fieldSpan(fields);
        const schema_fields = try self.allocator.alloc(RuntimeRecordFieldSchema, source.len);
        errdefer {
            for (schema_fields) |field| self.allocator.free(field.name);
            self.allocator.free(schema_fields);
        }
        for (source, 0..) |field, index| {
            schema_fields[index] = .{
                .name = try self.allocator.dupe(u8, self.program.names.recordFieldLabelText(field.name)),
                .logical_index = @intCast(index),
            };
        }
        try self.runtime_schemas.records.append(self.allocator, .{
            .type_name = try self.allocator.dupe(u8, type_name),
            .fields = schema_fields,
        });
    }

    fn publishTagUnionSchema(self: *Lowerer, type_name: []const u8, tags: Type.Span) Common.LowerError!void {
        const source = self.program.types.tagSpan(tags);
        const schema_tags = try self.allocator.alloc(RuntimeTagSchema, source.len);
        errdefer {
            for (schema_tags) |tag| self.allocator.free(tag.name);
            self.allocator.free(schema_tags);
        }
        for (source, 0..) |tag, index| {
            schema_tags[index] = .{
                .name = try self.allocator.dupe(u8, self.program.names.tagLabelText(tag.name)),
                .discriminant = @intCast(index),
            };
        }
        try self.runtime_schemas.tag_unions.append(self.allocator, .{
            .type_name = try self.allocator.dupe(u8, type_name),
            .tags = schema_tags,
        });
    }

    fn lowerExprReturn(self: *Lowerer, expr_id: LambdaMono.ExprId) Common.LowerError!LIR.CFStmtId {
        const expr_data = self.expr(expr_id);
        const ret_local = try self.addTemp(expr_data.ty);
        const ret_stmt = try self.result.store.addCFStmt(.{ .ret = .{ .value = ret_local } });
        return try self.lowerExprInto(ret_local, expr_id, ret_stmt);
    }

    fn lowerExprInto(
        self: *Lowerer,
        target: LIR.LocalId,
        expr_id: LambdaMono.ExprId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const expr_data = self.expr(expr_id);
        return switch (expr_data.data) {
            .local => |local| try self.assignLocal(target, try self.localFor(local), next),
            .unit => try self.assignZst(target, next),
            .int_lit => |value| try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .i128_literal = .{
                    .value = value.toI128(),
                    .layout_idx = self.result.store.getLocal(target).layout_idx,
                } },
                .next = next,
            } }),
            .frac_f32_lit => |value| try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .f32_literal = value },
                .next = next,
            } }),
            .frac_f64_lit => |value| try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .f64_literal = value },
                .next = next,
            } }),
            .dec_lit => |value| try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .dec_literal = value.num },
                .next = next,
            } }),
            .str_lit => |literal| try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .str_literal = try self.result.store.insertString(self.program.stringLiteralText(literal)) },
                .next = next,
            } }),
            .list => |items| try self.lowerListInto(target, items, next),
            .tuple => |items| try self.lowerTupleInto(target, items, next),
            .record => |fields| try self.lowerRecordInto(target, expr_data.ty, fields, next),
            .capture_record => |items| try self.lowerCaptureRecordInto(target, items, next),
            .tag => |tag| try self.lowerTagInto(target, expr_data.ty, tag.name, tag.payloads, next),
            .callable => |callable| try self.lowerCallableInto(target, callable, next),
            .nominal => |backing| try self.lowerNominalInto(target, backing, next),
            .let_ => |let_| try self.lowerLetInto(target, let_, next),
            .direct_call => |call| try self.lowerDirectCallInto(target, call, next),
            .indirect_erased_call => |call| try self.lowerErasedCallInto(target, call, next),
            .packed_erased_fn => |packed_fn| try self.lowerPackedErasedFnInto(target, packed_fn, next),
            .low_level => |call| try self.lowerLowLevelInto(target, call.op, call.args, next),
            .field_access => |field| try self.lowerFieldAccessInto(target, field.receiver, field.field, next),
            .capture_access => |slot| try self.lowerCaptureAccessInto(target, slot, next),
            .tuple_access => |access| try self.lowerTupleAccessInto(target, access.tuple, access.elem_index, next),
            .structural_eq => |eq| try self.lowerStructuralEqInto(target, eq.lhs, eq.rhs, eq.negated, next),
            .match_ => |match_| try self.lowerMatchInto(target, match_.scrutinee, match_.branches, next),
            .if_ => |if_| try self.lowerIfInto(target, if_.branches, if_.final_else, next),
            .block => |block| try self.lowerBlockInto(target, block.statements, block.final_expr, next),
            .loop_ => |loop| try self.lowerLoopInto(target, loop, next),
            .break_ => |value| try self.lowerBreak(value),
            .continue_ => |continue_| try self.lowerContinue(continue_.values),
            .return_ => |value| try self.lowerReturn(value),
            .crash => |msg| try self.result.store.addCFStmt(.{ .crash = .{
                .msg = try self.result.store.insertString(self.program.stringLiteralText(msg)),
            } }),
            .dbg => |child| blk: {
                const debug_stmt = try self.result.store.addCFStmt(.{ .debug = .{ .message = target, .next = next } });
                break :blk try self.lowerExprInto(target, child, debug_stmt);
            },
            .expect => |child| try self.lowerExpectExprInto(target, child, next),
        };
    }

    fn lowerListInto(self: *Lowerer, target: LIR.LocalId, span: LambdaMono.Span(LambdaMono.ExprId), next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const items = self.program.exprSpan(span);
        const locals = try self.lowerExprsToTemps(items);
        defer self.allocator.free(locals.ids);
        var current = try self.result.store.addCFStmt(.{ .assign_list = .{
            .target = target,
            .elems = try self.result.store.addLocalSpan(locals.ids),
            .next = next,
        } });
        current = try self.prependExprs(locals, current);
        return current;
    }

    fn lowerTupleInto(self: *Lowerer, target: LIR.LocalId, span: LambdaMono.Span(LambdaMono.ExprId), next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const items = self.program.exprSpan(span);
        return try self.lowerStructExprsInto(target, items, next);
    }

    fn lowerCaptureRecordInto(self: *Lowerer, target: LIR.LocalId, span: LambdaMono.Span(LambdaMono.ExprId), next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const items = self.program.exprSpan(span);
        return try self.lowerStructExprsInto(target, items, next);
    }

    fn lowerStructExprsInto(self: *Lowerer, target: LIR.LocalId, items: []const LambdaMono.ExprId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const locals = try self.lowerExprsToTemps(items);
        defer self.allocator.free(locals.ids);
        var current = if (self.isZstLocal(target))
            try self.assignZst(target, next)
        else
            try self.result.store.addCFStmt(.{ .assign_struct = .{
                .target = target,
                .fields = try self.result.store.addLocalSpan(locals.ids),
                .next = next,
            } });
        current = try self.prependExprs(locals, current);
        return current;
    }

    fn lowerRecordInto(
        self: *Lowerer,
        target: LIR.LocalId,
        ty: Type.TypeId,
        span: LambdaMono.Span(LambdaMono.FieldExpr),
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const type_fields = self.recordFields(ty);
        const expr_fields = self.program.fieldExprSpan(span);
        const ordered = try self.allocator.alloc(LambdaMono.ExprId, type_fields.len);
        defer self.allocator.free(ordered);

        for (type_fields, 0..) |field, i| {
            ordered[i] = for (expr_fields) |expr_field| {
                if (expr_field.name == field.name) break expr_field.value;
            } else Common.invariant("record expression missing field published by Lambda Mono type");
        }

        return try self.lowerStructExprsInto(target, ordered, next);
    }

    fn lowerTagInto(
        self: *Lowerer,
        target: LIR.LocalId,
        ty: Type.TypeId,
        name: Type.names.TagNameId,
        payload_span: LambdaMono.Span(LambdaMono.ExprId),
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const variant_index = self.tagIndex(ty, name);
        return try self.lowerTagPayloadInto(target, variant_index, payload_span, next);
    }

    fn lowerCallableInto(self: *Lowerer, target: LIR.LocalId, callable: LambdaMono.CallableValue, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const variant_index = self.callableVariantIndex(callable.ty, callable.variant);
        if (callable.payload) |payload_expr| {
            const payload = try self.addTemp(self.expr(payload_expr).ty);
            if (self.isZstLocal(target) and !self.isZstLocal(payload)) {
                Common.invariant("zero-sized callable layout had a non-zero-sized payload");
            }
            const assign = if (self.isZstLocal(target))
                try self.assignZst(target, next)
            else
                try self.result.store.addCFStmt(.{ .assign_tag = .{
                    .target = target,
                    .variant_index = variant_index,
                    .discriminant = variant_index,
                    .payload = payload,
                    .next = next,
                } });
            return try self.lowerExprInto(payload, payload_expr, assign);
        }
        if (self.isZstLocal(target)) return try self.assignZst(target, next);
        return try self.result.store.addCFStmt(.{ .assign_tag = .{
            .target = target,
            .variant_index = variant_index,
            .discriminant = variant_index,
            .payload = null,
            .next = next,
        } });
    }

    fn lowerTagPayloadInto(
        self: *Lowerer,
        target: LIR.LocalId,
        variant_index: u16,
        payload_span: LambdaMono.Span(LambdaMono.ExprId),
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const payloads = self.program.exprSpan(payload_span);
        if (payloads.len == 0) {
            if (self.isZstLocal(target)) return try self.assignZst(target, next);
            return try self.result.store.addCFStmt(.{ .assign_tag = .{
                .target = target,
                .variant_index = variant_index,
                .discriminant = variant_index,
                .payload = null,
                .next = next,
            } });
        }

        const target_layout = self.result.store.getLocal(target).layout_idx;
        const payload_local = try self.addLocalForLayout(if (self.isZstLocal(target))
            .zst
        else
            self.tagUnionPayloadLayout(target_layout, variant_index));
        if (self.isZstLocal(target) and !self.isZstLocal(payload_local)) {
            Common.invariant("zero-sized tag layout had a non-zero-sized payload");
        }
        const assign_tag = if (self.isZstLocal(target))
            try self.assignZst(target, next)
        else
            try self.result.store.addCFStmt(.{ .assign_tag = .{
                .target = target,
                .variant_index = variant_index,
                .discriminant = variant_index,
                .payload = payload_local,
                .next = next,
            } });

        if (payloads.len == 1) {
            return try self.lowerExprInto(payload_local, payloads[0], assign_tag);
        }
        return try self.lowerStructExprsInto(payload_local, payloads, assign_tag);
    }

    fn lowerNominalInto(self: *Lowerer, target: LIR.LocalId, backing: LambdaMono.ExprId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const backing_local = try self.addTemp(self.expr(backing).ty);
        const assign = if (self.isZstLocal(target))
            try self.assignZst(target, next)
        else
            try self.result.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = .{ .nominal = .{ .backing_ref = backing_local } },
                .next = next,
            } });
        return try self.lowerExprInto(backing_local, backing, assign);
    }

    fn lowerLetInto(self: *Lowerer, target: LIR.LocalId, let_: anytype, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const rest = try self.lowerExprInto(target, let_.rest, next);
        const value_expr = self.expr(let_.value);
        const value_local = try self.addTemp(value_expr.ty);
        const bind = try self.bindPattern(let_.bind, value_local, rest);
        return try self.lowerExprInto(value_local, let_.value, bind);
    }

    fn lowerDirectCallInto(self: *Lowerer, target: LIR.LocalId, call: LambdaMono.DirectCall, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const args = self.program.exprSpan(call.args);
        const lowered = try self.lowerExprsToTemps(args);
        defer self.allocator.free(lowered.ids);
        var current = try self.result.store.addCFStmt(.{ .assign_call = .{
            .target = target,
            .proc = self.fn_map[@intFromEnum(call.target)],
            .args = try self.result.store.addLocalSpan(lowered.ids),
            .next = next,
        } });
        current = try self.prependExprs(lowered, current);
        return current;
    }

    fn lowerErasedCallInto(self: *Lowerer, target: LIR.LocalId, call: LambdaMono.ErasedCall, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const arg_exprs = self.program.exprSpan(call.args);
        const args = try self.lowerExprsToTemps(arg_exprs);
        defer self.allocator.free(args.ids);
        const callee = try self.addTemp(self.expr(call.callee).ty);
        var current = try self.result.store.addCFStmt(.{ .assign_call_erased = .{
            .target = target,
            .closure = callee,
            .args = try self.result.store.addLocalSpan(args.ids),
            .next = next,
        } });
        current = try self.prependExprs(args, current);
        return try self.lowerExprInto(callee, call.callee, current);
    }

    fn lowerPackedErasedFnInto(self: *Lowerer, target: LIR.LocalId, packed_fn: LambdaMono.PackedErasedFn, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const capture = if (packed_fn.capture) |capture_expr| try self.addTemp(self.expr(capture_expr).ty) else null;
        const capture_layout = if (capture) |local| self.result.store.getLocal(local).layout_idx else null;
        const on_drop: LIR.ErasedCallableOnDrop = if (capture_layout) |layout_idx| blk: {
            const helper_key = layout.RcHelper{ .op = .decref, .layout_idx = layout_idx };
            break :blk if (self.result.layouts.rcHelperPlan(helper_key) == .noop)
                .none
            else
                .{ .rc_helper = helper_key };
        } else .none;

        const assign = try self.result.store.addCFStmt(.{ .assign_packed_erased_fn = .{
            .target = target,
            .proc = self.fn_map[@intFromEnum(packed_fn.target)],
            .capture = capture,
            .capture_layout = capture_layout,
            .on_drop = on_drop,
            .next = next,
        } });
        if (packed_fn.capture) |capture_expr| return try self.lowerExprInto(capture.?, capture_expr, assign);
        return assign;
    }

    fn lowerLowLevelInto(self: *Lowerer, target: LIR.LocalId, op: can.CIR.Expr.LowLevel, span: LambdaMono.Span(LambdaMono.ExprId), next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const args = self.program.exprSpan(span);
        const lowered = try self.lowerExprsToTemps(args);
        defer self.allocator.free(lowered.ids);
        var current = try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = op.rcEffect(),
            .args = try self.result.store.addLocalSpan(lowered.ids),
            .next = next,
        } });
        current = try self.prependExprs(lowered, current);
        return current;
    }

    fn lowerFieldAccessInto(self: *Lowerer, target: LIR.LocalId, receiver: LambdaMono.ExprId, field_name: Type.names.RecordFieldNameId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const receiver_expr = self.expr(receiver);
        const receiver_local = try self.addTemp(receiver_expr.ty);
        const field_index = self.recordFieldIndex(receiver_expr.ty, field_name);
        const assign = if (self.isZstLocal(target))
            try self.assignZst(target, next)
        else
            try self.result.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = .{ .field = .{ .source = receiver_local, .field_idx = field_index } },
                .next = next,
            } });
        return try self.lowerExprInto(receiver_local, receiver, assign);
    }

    fn lowerCaptureAccessInto(self: *Lowerer, target: LIR.LocalId, slot: LambdaMono.CaptureSlot, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const record_expr = self.expr(slot.record);
        const record_local = try self.addTemp(record_expr.ty);
        const field_index = self.captureFieldIndex(record_expr.ty, slot.symbol);
        const assign = if (self.isZstLocal(target))
            try self.assignZst(target, next)
        else
            try self.result.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = .{ .field = .{ .source = record_local, .field_idx = field_index } },
                .next = next,
            } });
        return try self.lowerExprInto(record_local, slot.record, assign);
    }

    fn lowerTupleAccessInto(self: *Lowerer, target: LIR.LocalId, tuple: LambdaMono.ExprId, elem_index: u32, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const tuple_expr = self.expr(tuple);
        const tuple_local = try self.addTemp(tuple_expr.ty);
        const assign = if (self.isZstLocal(target))
            try self.assignZst(target, next)
        else
            try self.result.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = .{ .field = .{ .source = tuple_local, .field_idx = @intCast(elem_index) } },
                .next = next,
            } });
        return try self.lowerExprInto(tuple_local, tuple, assign);
    }

    fn lowerStructuralEqInto(self: *Lowerer, target: LIR.LocalId, lhs: LambdaMono.ExprId, rhs: LambdaMono.ExprId, negated: bool, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const lhs_local = try self.addTemp(self.expr(lhs).ty);
        const rhs_local = try self.addTemp(self.expr(rhs).ty);
        var current = try self.lowerEqLocalsInto(target, lhs_local, rhs_local, self.expr(lhs).ty, negated, next);
        current = try self.lowerExprInto(rhs_local, rhs, current);
        return try self.lowerExprInto(lhs_local, lhs, current);
    }

    fn lowerMatchInto(
        self: *Lowerer,
        target: LIR.LocalId,
        scrutinee: LambdaMono.ExprId,
        branches_span: LambdaMono.Span(LambdaMono.Branch),
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const scrutinee_local = try self.addTemp(self.expr(scrutinee).ty);
        const branches = self.program.branchSpan(branches_span);
        const branch_chain = try self.lowerBranchChain(scrutinee_local, branches, target, next);
        return try self.lowerExprInto(scrutinee_local, scrutinee, branch_chain);
    }

    fn lowerIfInto(self: *Lowerer, target: LIR.LocalId, branches_span: LambdaMono.Span(LambdaMono.IfBranch), final_else: LambdaMono.ExprId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const branches = self.program.ifBranchSpan(branches_span);
        var current = try self.lowerExprInto(target, final_else, next);
        var i = branches.len;
        while (i > 0) {
            i -= 1;
            const body = try self.lowerExprInto(target, branches[i].body, next);
            const cond_local = try self.addTemp(self.expr(branches[i].cond).ty);
            const switch_stmt = try self.boolSwitch(cond_local, body, current, next);
            current = try self.lowerExprInto(cond_local, branches[i].cond, switch_stmt);
        }
        return current;
    }

    fn lowerBlockInto(self: *Lowerer, target: LIR.LocalId, stmts_span: LambdaMono.Span(LambdaMono.StmtId), final_expr: LambdaMono.ExprId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        var current = try self.lowerExprInto(target, final_expr, next);
        const stmts = self.program.stmtSpan(stmts_span);
        var i = stmts.len;
        while (i > 0) {
            i -= 1;
            current = try self.lowerStmt(stmts[i], current);
        }
        return current;
    }

    fn lowerStmt(self: *Lowerer, stmt_id: LambdaMono.StmtId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        return switch (self.program.stmts.items[@intFromEnum(stmt_id)]) {
            .let_ => |let_| blk: {
                const value = try self.addTemp(self.expr(let_.value).ty);
                const bind = try self.bindPattern(let_.pat, value, next);
                break :blk try self.lowerExprInto(value, let_.value, bind);
            },
            .expr => |expr_id| blk: {
                const temp = try self.addTemp(self.expr(expr_id).ty);
                break :blk try self.lowerExprInto(temp, expr_id, next);
            },
            .expect => |expr_id| try self.lowerExpectStmt(expr_id, next),
            .dbg => |expr_id| blk: {
                const temp = try self.addTemp(self.expr(expr_id).ty);
                const debug_stmt = try self.result.store.addCFStmt(.{ .debug = .{ .message = temp, .next = next } });
                break :blk try self.lowerExprInto(temp, expr_id, debug_stmt);
            },
            .return_ => |expr_id| try self.lowerReturn(expr_id),
            .crash => |msg| try self.result.store.addCFStmt(.{ .crash = .{
                .msg = try self.result.store.insertString(self.program.stringLiteralText(msg)),
            } }),
        };
    }

    fn lowerLoopInto(self: *Lowerer, target: LIR.LocalId, loop: anytype, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const params = self.program.typedLocalSpan(loop.params);
        const initial_values = self.program.exprSpan(loop.initial_values);
        if (params.len != initial_values.len) Common.invariant("Lambda Mono loop parameter count differs from initial value count");

        const param_locals = try self.allocator.alloc(LIR.LocalId, params.len);
        defer self.allocator.free(param_locals);
        for (params, 0..) |param, i| param_locals[i] = try self.localFor(param.local);
        const param_span = try self.result.store.addLocalSpan(param_locals);
        const join_id = self.freshJoinPointId();

        try self.loop_stack.append(self.allocator, .{
            .join_id = join_id,
            .params = param_span,
            .result_target = target,
            .after_loop = next,
        });
        defer _ = self.loop_stack.pop();

        const body = try self.lowerExprInto(target, loop.body, next);
        const jump_args = try self.lowerExprsToTemps(initial_values);
        defer self.allocator.free(jump_args.ids);
        var initial_jump = try self.result.store.addCFStmt(.{ .jump = .{
            .target = join_id,
            .args = try self.result.store.addLocalSpan(jump_args.ids),
        } });
        initial_jump = try self.prependExprs(jump_args, initial_jump);

        return try self.result.store.addCFStmt(.{ .join = .{
            .id = join_id,
            .params = param_span,
            .body = body,
            .remainder = initial_jump,
        } });
    }

    fn lowerBreak(self: *Lowerer, value: ?LambdaMono.ExprId) Common.LowerError!LIR.CFStmtId {
        const loop = self.currentLoop();
        if (value) |expr_id| {
            return try self.lowerExprInto(loop.result_target, expr_id, loop.after_loop);
        }
        return try self.assignZst(loop.result_target, loop.after_loop);
    }

    fn lowerContinue(self: *Lowerer, values_span: LambdaMono.Span(LambdaMono.ExprId)) Common.LowerError!LIR.CFStmtId {
        const loop = self.currentLoop();
        const values = self.program.exprSpan(values_span);
        if (self.result.store.getLocalSpan(loop.params).len != values.len) {
            Common.invariant("continue value count differed from loop parameter count during direct LIR lowering");
        }
        const locals = try self.lowerExprsToTemps(values);
        defer self.allocator.free(locals.ids);
        var jump = try self.result.store.addCFStmt(.{ .jump = .{
            .target = loop.join_id,
            .args = try self.result.store.addLocalSpan(locals.ids),
        } });
        jump = try self.prependExprs(locals, jump);
        return jump;
    }

    fn lowerReturn(self: *Lowerer, expr_id: LambdaMono.ExprId) Common.LowerError!LIR.CFStmtId {
        const ret_local = try self.addTemp(self.expr(expr_id).ty);
        const ret_stmt = try self.result.store.addCFStmt(.{ .ret = .{ .value = ret_local } });
        return try self.lowerExprInto(ret_local, expr_id, ret_stmt);
    }

    fn lowerExpectExprInto(self: *Lowerer, target: LIR.LocalId, child: LambdaMono.ExprId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const after = try self.assignZst(target, next);
        return try self.lowerExpectStmt(child, after);
    }

    fn lowerExpectStmt(self: *Lowerer, child: LambdaMono.ExprId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const cond = try self.addTemp(self.expr(child).ty);
        const expect_stmt = try self.result.store.addCFStmt(.{ .expect = .{ .condition = cond, .next = next } });
        return try self.lowerExprInto(cond, child, expect_stmt);
    }

    fn lowerBranchChain(self: *Lowerer, scrutinee: LIR.LocalId, branches: []const LambdaMono.Branch, target: LIR.LocalId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        var current = try self.result.store.addCFStmt(.{ .runtime_error = {} });
        var i = branches.len;
        while (i > 0) {
            i -= 1;
            const branch_body = try self.lowerExprInto(target, branches[i].body, next);
            const guarded = if (branches[i].guard) |guard| blk: {
                const guard_local = try self.addTemp(self.expr(guard).ty);
                const guard_switch = try self.boolSwitch(guard_local, branch_body, current, next);
                break :blk try self.lowerExprInto(guard_local, guard, guard_switch);
            } else branch_body;
            current = try self.lowerPatternThen(branches[i].pat, scrutinee, guarded, current, next);
        }
        return current;
    }

    fn lowerPatternThen(self: *Lowerer, pat_id: LambdaMono.PatId, source: LIR.LocalId, on_match: LIR.CFStmtId, on_miss: LIR.CFStmtId, continuation: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const pat_data = self.pat(pat_id);
        return switch (pat_data.data) {
            .bind, .wildcard => try self.bindPattern(pat_id, source, on_match),
            .as => |as| blk: {
                const tested = try self.lowerPatternThen(as.pattern, source, on_match, on_miss, continuation);
                break :blk try self.assignLocal(try self.localFor(as.local), source, tested);
            },
            .record => |fields| try self.lowerRecordPatternThen(pat_data.ty, fields, source, on_match, on_miss, continuation),
            .tuple => |items| try self.lowerTuplePatternThen(items, source, on_match, on_miss, continuation),
            .nominal => |inner| try self.lowerPatternThen(inner, source, on_match, on_miss, continuation),
            .tag => |tag| try self.lowerTagPatternThen(pat_data.ty, tag.name, tag.payloads, source, on_match, on_miss, continuation),
            .callable => |callable| try self.lowerCallablePatternThen(pat_data.ty, callable.variant, callable.payload, source, on_match, on_miss, continuation),
            .int_lit => |value| try self.lowerLiteralPatternThen(source, pat_data.ty, .{ .int_lit = value }, on_match, on_miss, continuation),
            .dec_lit => |value| try self.lowerLiteralPatternThen(source, pat_data.ty, .{ .dec_lit = value }, on_match, on_miss, continuation),
            .frac_f32_lit => |value| try self.lowerLiteralPatternThen(source, pat_data.ty, .{ .frac_f32_lit = value }, on_match, on_miss, continuation),
            .frac_f64_lit => |value| try self.lowerLiteralPatternThen(source, pat_data.ty, .{ .frac_f64_lit = value }, on_match, on_miss, continuation),
            .str_lit => |value| try self.lowerLiteralPatternThen(source, pat_data.ty, .{ .str_lit = value }, on_match, on_miss, continuation),
        };
    }

    const LiteralPattern = union(enum) {
        int_lit: can.CIR.IntValue,
        dec_lit: builtins.dec.RocDec,
        frac_f32_lit: f32,
        frac_f64_lit: f64,
        str_lit: LambdaMono.StringLiteralId,
    };

    fn lowerLiteralPatternThen(
        self: *Lowerer,
        source: LIR.LocalId,
        ty: Type.TypeId,
        literal: LiteralPattern,
        on_match: LIR.CFStmtId,
        on_miss: LIR.CFStmtId,
        continuation: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const lit_local = try self.addTemp(ty);
        const eq_local = try self.addLocalForLayout(.bool);
        const switch_stmt = try self.boolSwitch(eq_local, on_match, on_miss, continuation);
        const compare = try self.lowerEqLocalsInto(eq_local, source, lit_local, ty, false, switch_stmt);
        return try self.lowerLiteralInto(lit_local, literal, compare);
    }

    fn lowerLiteralInto(self: *Lowerer, target: LIR.LocalId, literal: LiteralPattern, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        return switch (literal) {
            .int_lit => |value| try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .i128_literal = .{ .value = value.toI128(), .layout_idx = self.result.store.getLocal(target).layout_idx } },
                .next = next,
            } }),
            .dec_lit => |value| try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .dec_literal = value.num },
                .next = next,
            } }),
            .frac_f32_lit => |value| try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .f32_literal = value },
                .next = next,
            } }),
            .frac_f64_lit => |value| try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .f64_literal = value },
                .next = next,
            } }),
            .str_lit => |value| try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .str_literal = try self.result.store.insertString(self.program.stringLiteralText(value)) },
                .next = next,
            } }),
        };
    }

    fn lowerTagPatternThen(
        self: *Lowerer,
        ty: Type.TypeId,
        name: Type.names.TagNameId,
        payloads: LambdaMono.Span(LambdaMono.PatId),
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        on_miss: LIR.CFStmtId,
        continuation: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const variant_index = self.tagIndex(ty, name);
        const bind_payloads = try self.matchTagPayloadPatterns(variant_index, payloads, source, on_match, on_miss, continuation);
        return try self.discriminantSwitch(source, variant_index, bind_payloads, on_miss, continuation);
    }

    fn lowerCallablePatternThen(
        self: *Lowerer,
        ty: Type.TypeId,
        variant: Type.FnVariantId,
        payload: ?LambdaMono.PatId,
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        on_miss: LIR.CFStmtId,
        continuation: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const variant_index = self.callableVariantIndex(ty, variant);
        const bind_payload = if (payload) |payload_pat| blk: {
            const payload_local = try self.addLocalForLayout(self.tagUnionPayloadLayout(self.result.store.getLocal(source).layout_idx, variant_index));
            const bound = try self.lowerPatternThen(payload_pat, payload_local, on_match, on_miss, continuation);
            break :blk if (self.isZstLocal(payload_local))
                bound
            else
                try self.result.store.addCFStmt(.{ .assign_ref = .{
                    .target = payload_local,
                    .op = .{ .tag_payload_struct = .{
                        .source = source,
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } },
                    .next = bound,
                } });
        } else on_match;
        return try self.discriminantSwitch(source, variant_index, bind_payload, on_miss, continuation);
    }

    fn lowerRecordPatternThen(
        self: *Lowerer,
        ty: Type.TypeId,
        span: LambdaMono.Span(LambdaMono.RecordDestruct),
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        on_miss: LIR.CFStmtId,
        continuation: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        var current = on_match;
        const destructs = self.program.recordDestructSpan(span);
        var i = destructs.len;
        while (i > 0) {
            i -= 1;
            const field_index = self.recordFieldIndex(ty, destructs[i].name);
            const field_ty = self.recordFields(ty)[@as(usize, @intCast(field_index))].ty;
            const field_local = try self.addTemp(field_ty);
            current = try self.lowerPatternThen(destructs[i].pattern, field_local, current, on_miss, continuation);
            if (!self.isZstLocal(field_local)) {
                current = try self.result.store.addCFStmt(.{ .assign_ref = .{
                    .target = field_local,
                    .op = .{ .field = .{ .source = source, .field_idx = field_index } },
                    .next = current,
                } });
            }
        }
        return current;
    }

    fn lowerTuplePatternThen(
        self: *Lowerer,
        span: LambdaMono.Span(LambdaMono.PatId),
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        on_miss: LIR.CFStmtId,
        continuation: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        var current = on_match;
        const items = self.program.patSpan(span);
        var i = items.len;
        while (i > 0) {
            i -= 1;
            const item_pat = self.pat(items[i]);
            const item_local = try self.addTemp(item_pat.ty);
            current = try self.lowerPatternThen(items[i], item_local, current, on_miss, continuation);
            if (!self.isZstLocal(item_local)) {
                current = try self.result.store.addCFStmt(.{ .assign_ref = .{
                    .target = item_local,
                    .op = .{ .field = .{ .source = source, .field_idx = @intCast(i) } },
                    .next = current,
                } });
            }
        }
        return current;
    }

    fn bindPattern(self: *Lowerer, pat_id: LambdaMono.PatId, source: LIR.LocalId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const pat_data = self.pat(pat_id);
        return switch (pat_data.data) {
            .bind => |local| try self.assignLocal(try self.localFor(local), source, next),
            .wildcard => next,
            .as => |as| blk: {
                const bound = try self.bindPattern(as.pattern, source, next);
                break :blk try self.assignLocal(try self.localFor(as.local), source, bound);
            },
            .record => |fields| try self.bindRecordPattern(pat_data.ty, fields, source, next),
            .tuple => |items| try self.bindTuplePattern(items, source, next),
            .tag => |tag| try self.bindTagPayloadPatterns(self.tagIndex(pat_data.ty, tag.name), tag.payloads, source, next),
            .callable => |callable| if (callable.payload) |payload_pat| blk: {
                const variant_index = self.callableVariantIndex(pat_data.ty, callable.variant);
                const payload_local = try self.addLocalForLayout(self.tagUnionPayloadLayout(self.result.store.getLocal(source).layout_idx, variant_index));
                const bound = try self.bindPattern(payload_pat, payload_local, next);
                break :blk if (self.isZstLocal(payload_local))
                    bound
                else
                    try self.result.store.addCFStmt(.{ .assign_ref = .{
                        .target = payload_local,
                        .op = .{ .tag_payload_struct = .{
                            .source = source,
                            .variant_index = variant_index,
                            .tag_discriminant = variant_index,
                        } },
                        .next = bound,
                    } });
            } else next,
            .nominal => |inner| try self.bindPattern(inner, source, next),
            .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit => next,
        };
    }

    fn bindRecordPattern(self: *Lowerer, ty: Type.TypeId, span: LambdaMono.Span(LambdaMono.RecordDestruct), source: LIR.LocalId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        var current = next;
        const destructs = self.program.recordDestructSpan(span);
        var i = destructs.len;
        while (i > 0) {
            i -= 1;
            const field_index = self.recordFieldIndex(ty, destructs[i].name);
            const field_ty = self.recordFields(ty)[@as(usize, @intCast(field_index))].ty;
            const field_local = try self.addTemp(field_ty);
            current = try self.bindPattern(destructs[i].pattern, field_local, current);
            if (!self.isZstLocal(field_local)) {
                current = try self.result.store.addCFStmt(.{ .assign_ref = .{
                    .target = field_local,
                    .op = .{ .field = .{ .source = source, .field_idx = field_index } },
                    .next = current,
                } });
            }
        }
        return current;
    }

    fn matchTagPayloadPatterns(
        self: *Lowerer,
        variant_index: u16,
        payload_span: LambdaMono.Span(LambdaMono.PatId),
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        on_miss: LIR.CFStmtId,
        continuation: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        var current = on_match;
        const payloads = self.program.patSpan(payload_span);
        var i = payloads.len;
        while (i > 0) {
            i -= 1;
            const payload_pat = self.pat(payloads[i]);
            const payload_local = try self.addTemp(payload_pat.ty);
            current = try self.lowerPatternThen(payloads[i], payload_local, current, on_miss, continuation);
            if (!self.isZstLocal(payload_local)) {
                current = try self.result.store.addCFStmt(.{ .assign_ref = .{
                    .target = payload_local,
                    .op = .{ .tag_payload = .{
                        .source = source,
                        .payload_idx = @intCast(i),
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } },
                    .next = current,
                } });
            }
        }
        return current;
    }

    fn bindTuplePattern(self: *Lowerer, span: LambdaMono.Span(LambdaMono.PatId), source: LIR.LocalId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        var current = next;
        const items = self.program.patSpan(span);
        var i = items.len;
        while (i > 0) {
            i -= 1;
            const item_pat = self.pat(items[i]);
            const item_local = try self.addTemp(item_pat.ty);
            current = try self.bindPattern(items[i], item_local, current);
            if (!self.isZstLocal(item_local)) {
                current = try self.result.store.addCFStmt(.{ .assign_ref = .{
                    .target = item_local,
                    .op = .{ .field = .{ .source = source, .field_idx = @intCast(i) } },
                    .next = current,
                } });
            }
        }
        return current;
    }

    fn bindTagPayloadPatterns(
        self: *Lowerer,
        variant_index: u16,
        payload_span: LambdaMono.Span(LambdaMono.PatId),
        source: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        var current = next;
        const payloads = self.program.patSpan(payload_span);
        var i = payloads.len;
        while (i > 0) {
            i -= 1;
            const payload_pat = self.pat(payloads[i]);
            const payload_local = try self.addTemp(payload_pat.ty);
            current = try self.bindPattern(payloads[i], payload_local, current);
            if (!self.isZstLocal(payload_local)) {
                current = try self.result.store.addCFStmt(.{ .assign_ref = .{
                    .target = payload_local,
                    .op = .{ .tag_payload = .{
                        .source = source,
                        .payload_idx = @intCast(i),
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } },
                    .next = current,
                } });
            }
        }
        return current;
    }

    fn lowerEqLocalsInto(self: *Lowerer, target: LIR.LocalId, lhs: LIR.LocalId, rhs: LIR.LocalId, ty: Type.TypeId, negated: bool, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        return switch (self.program.types.get(ty)) {
            .primitive => |primitive| try self.lowerPrimitiveEqLocalsInto(target, lhs, rhs, primitive, negated, next),
            .zst => try self.assignBool(target, !negated, next),
            .named => |named| blk: {
                const backing = named.backing orelse Common.invariant("named equality reached direct LIR without runtime backing");
                break :blk try self.lowerEqLocalsInto(target, lhs, rhs, backing.ty, negated, next);
            },
            .record => |fields| try self.lowerRecordEqLocalsInto(target, lhs, rhs, self.program.types.fieldSpan(fields), negated, next),
            .capture_record => |fields| try self.lowerCaptureRecordEqLocalsInto(target, lhs, rhs, self.program.types.captureFieldSpan(fields), negated, next),
            .tuple => |items| try self.lowerTupleEqLocalsInto(target, lhs, rhs, self.program.types.span(items), negated, next),
            .tag_union => |tags| try self.lowerTagUnionEqLocalsInto(target, lhs, rhs, self.program.types.tagSpan(tags), negated, next),
            .list, .box, .callable, .erased_fn => Common.invariant("non-structural equality reached direct LIR structural equality lowering"),
        };
    }

    fn lowerPrimitiveEqLocalsInto(
        self: *Lowerer,
        target: LIR.LocalId,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        primitive: MonoType.Primitive,
        negated: bool,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (primitive == .bool) {
            return try self.lowerBoolEqLocalsInto(target, lhs, rhs, negated, next);
        }
        const eq_op: LIR.LowLevel = switch (primitive) {
            .str => .str_is_eq,
            .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .f32, .f64, .dec => .num_is_eq,
            .bool => unreachable,
        };
        const args = [_]LIR.LocalId{ lhs, rhs };
        const not_op: LIR.LowLevel = .bool_not;
        if (!negated) {
            return try self.result.store.addCFStmt(.{ .assign_low_level = .{
                .target = target,
                .op = eq_op,
                .rc_effect = eq_op.rcEffect(),
                .args = try self.result.store.addLocalSpan(&args),
                .next = next,
            } });
        }
        const raw = try self.addLocalForLayout(.bool);
        const not_stmt = try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = not_op,
            .rc_effect = not_op.rcEffect(),
            .args = try self.result.store.addLocalSpan(&[_]LIR.LocalId{raw}),
            .next = next,
        } });
        return try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = raw,
            .op = eq_op,
            .rc_effect = eq_op.rcEffect(),
            .args = try self.result.store.addLocalSpan(&args),
            .next = not_stmt,
        } });
    }

    fn lowerBoolEqLocalsInto(self: *Lowerer, target: LIR.LocalId, lhs: LIR.LocalId, rhs: LIR.LocalId, negated: bool, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const lhs_disc = try self.addLocalForLayout(.u16);
        const rhs_disc = try self.addLocalForLayout(.u16);
        const compare = try self.lowerPrimitiveEqLocalsInto(target, lhs_disc, rhs_disc, .u16, negated, next);
        const read_rhs = try self.result.store.addCFStmt(.{ .assign_ref = .{
            .target = rhs_disc,
            .op = .{ .discriminant = .{ .source = rhs } },
            .next = compare,
        } });
        return try self.result.store.addCFStmt(.{ .assign_ref = .{
            .target = lhs_disc,
            .op = .{ .discriminant = .{ .source = lhs } },
            .next = read_rhs,
        } });
    }

    fn lowerRecordEqLocalsInto(
        self: *Lowerer,
        target: LIR.LocalId,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        fields: []const Type.Field,
        negated: bool,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        var current = try self.assignBool(target, !negated, next);
        const failed = try self.assignBool(target, negated, next);
        var i = fields.len;
        while (i > 0) {
            i -= 1;
            current = try self.lowerFieldEqStep(lhs, rhs, fields[i].ty, @intCast(i), current, failed, next);
        }
        return current;
    }

    fn lowerCaptureRecordEqLocalsInto(
        self: *Lowerer,
        target: LIR.LocalId,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        fields: []const Type.CaptureField,
        negated: bool,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        var current = try self.assignBool(target, !negated, next);
        const failed = try self.assignBool(target, negated, next);
        var i = fields.len;
        while (i > 0) {
            i -= 1;
            current = try self.lowerFieldEqStep(lhs, rhs, fields[i].ty, @intCast(i), current, failed, next);
        }
        return current;
    }

    fn lowerTupleEqLocalsInto(
        self: *Lowerer,
        target: LIR.LocalId,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        items: []const Type.TypeId,
        negated: bool,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        var current = try self.assignBool(target, !negated, next);
        const failed = try self.assignBool(target, negated, next);
        var i = items.len;
        while (i > 0) {
            i -= 1;
            current = try self.lowerFieldEqStep(lhs, rhs, items[i], @intCast(i), current, failed, next);
        }
        return current;
    }

    fn lowerFieldEqStep(
        self: *Lowerer,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        field_ty: Type.TypeId,
        field_index: u16,
        on_equal: LIR.CFStmtId,
        on_not_equal: LIR.CFStmtId,
        continuation: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const lhs_field = try self.addTemp(field_ty);
        const rhs_field = try self.addTemp(field_ty);
        const eq = try self.addLocalForLayout(.bool);
        var current = try self.boolSwitch(eq, on_equal, on_not_equal, continuation);
        current = try self.lowerEqLocalsInto(eq, lhs_field, rhs_field, field_ty, false, current);
        if (!self.isZstLocal(rhs_field)) {
            current = try self.result.store.addCFStmt(.{ .assign_ref = .{
                .target = rhs_field,
                .op = .{ .field = .{ .source = rhs, .field_idx = field_index } },
                .next = current,
            } });
        }
        if (!self.isZstLocal(lhs_field)) {
            current = try self.result.store.addCFStmt(.{ .assign_ref = .{
                .target = lhs_field,
                .op = .{ .field = .{ .source = lhs, .field_idx = field_index } },
                .next = current,
            } });
        }
        return current;
    }

    fn lowerTagUnionEqLocalsInto(
        self: *Lowerer,
        target: LIR.LocalId,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        tags: []const Type.Tag,
        negated: bool,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (self.isZstLocal(lhs) and self.isZstLocal(rhs)) {
            return try self.assignBool(target, !negated, next);
        }

        const success = try self.assignBool(target, !negated, next);
        const failed = try self.assignBool(target, negated, next);

        const lhs_disc = try self.addLocalForLayout(.u16);
        const rhs_disc = try self.addLocalForLayout(.u16);
        const same_disc = try self.addLocalForLayout(.bool);

        const branches = try self.allocator.alloc(LIR.CFSwitchBranch, tags.len);
        defer self.allocator.free(branches);
        for (tags, 0..) |tag, index| {
            branches[index] = .{
                .value = @intCast(index),
                .body = try self.lowerTagPayloadEqVariant(lhs, rhs, tag, @intCast(index), success, failed, next),
            };
        }

        const payload_switch = try self.result.store.addCFStmt(.{ .switch_stmt = .{
            .cond = lhs_disc,
            .branches = try self.result.store.addCFSwitchBranches(branches),
            .default_branch = failed,
            .continuation = next,
        } });
        const disc_switch = try self.boolSwitch(same_disc, payload_switch, failed, next);
        const eq_op: LIR.LowLevel = .num_is_eq;
        const compare_disc = try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = same_disc,
            .op = eq_op,
            .rc_effect = eq_op.rcEffect(),
            .args = try self.result.store.addLocalSpan(&[_]LIR.LocalId{ lhs_disc, rhs_disc }),
            .next = disc_switch,
        } });
        const read_rhs = try self.result.store.addCFStmt(.{ .assign_ref = .{
            .target = rhs_disc,
            .op = .{ .discriminant = .{ .source = rhs } },
            .next = compare_disc,
        } });
        return try self.result.store.addCFStmt(.{ .assign_ref = .{
            .target = lhs_disc,
            .op = .{ .discriminant = .{ .source = lhs } },
            .next = read_rhs,
        } });
    }

    fn lowerTagPayloadEqVariant(
        self: *Lowerer,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        tag: Type.Tag,
        variant_index: u16,
        on_equal: LIR.CFStmtId,
        on_not_equal: LIR.CFStmtId,
        continuation: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        var current = on_equal;
        const payloads = self.program.types.span(tag.payloads);
        var i = payloads.len;
        while (i > 0) {
            i -= 1;
            const payload_ty = payloads[i];
            const lhs_payload = try self.addTemp(payload_ty);
            const rhs_payload = try self.addTemp(payload_ty);
            const eq = try self.addLocalForLayout(.bool);
            current = try self.boolSwitch(eq, current, on_not_equal, continuation);
            current = try self.lowerEqLocalsInto(eq, lhs_payload, rhs_payload, payload_ty, false, current);
            if (!self.isZstLocal(rhs_payload)) {
                current = try self.result.store.addCFStmt(.{ .assign_ref = .{
                    .target = rhs_payload,
                    .op = .{ .tag_payload = .{
                        .source = rhs,
                        .payload_idx = @intCast(i),
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } },
                    .next = current,
                } });
            }
            if (!self.isZstLocal(lhs_payload)) {
                current = try self.result.store.addCFStmt(.{ .assign_ref = .{
                    .target = lhs_payload,
                    .op = .{ .tag_payload = .{
                        .source = lhs,
                        .payload_idx = @intCast(i),
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } },
                    .next = current,
                } });
            }
        }
        return current;
    }

    fn assignBool(self: *Lowerer, target: LIR.LocalId, value: bool, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        if (self.result.store.getLocal(target).layout_idx != .bool) {
            Common.invariant("boolean assignment target was not Bool layout");
        }
        const discriminant: u16 = if (value) 1 else 0;
        return try self.result.store.addCFStmt(.{ .assign_tag = .{
            .target = target,
            .variant_index = discriminant,
            .discriminant = discriminant,
            .payload = null,
            .next = next,
        } });
    }

    fn boolSwitch(self: *Lowerer, cond: LIR.LocalId, true_body: LIR.CFStmtId, false_body: LIR.CFStmtId, continuation: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const branches = [_]LIR.CFSwitchBranch{.{ .value = 1, .body = true_body }};
        return try self.result.store.addCFStmt(.{ .switch_stmt = .{
            .cond = cond,
            .branches = try self.result.store.addCFSwitchBranches(&branches),
            .default_branch = false_body,
            .continuation = continuation,
        } });
    }

    fn discriminantSwitch(self: *Lowerer, source: LIR.LocalId, discriminant: u16, body: LIR.CFStmtId, default: LIR.CFStmtId, continuation: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        if (self.isZstLocal(source)) return body;
        const disc_local = try self.addLocalForLayout(.u16);
        const branches = [_]LIR.CFSwitchBranch{.{ .value = discriminant, .body = body }};
        const switch_stmt = try self.result.store.addCFStmt(.{ .switch_stmt = .{
            .cond = disc_local,
            .branches = try self.result.store.addCFSwitchBranches(&branches),
            .default_branch = default,
            .continuation = continuation,
        } });
        return try self.result.store.addCFStmt(.{ .assign_ref = .{
            .target = disc_local,
            .op = .{ .discriminant = .{ .source = source } },
            .next = switch_stmt,
        } });
    }

    const LoweredExprLocals = struct {
        exprs: []const LambdaMono.ExprId,
        ids: []LIR.LocalId,
    };

    fn lowerExprsToTemps(self: *Lowerer, exprs: []const LambdaMono.ExprId) Common.LowerError!LoweredExprLocals {
        const ids = try self.allocator.alloc(LIR.LocalId, exprs.len);
        errdefer self.allocator.free(ids);
        for (exprs, 0..) |expr_id, i| ids[i] = try self.addTemp(self.expr(expr_id).ty);
        return .{ .exprs = exprs, .ids = ids };
    }

    fn prependExprs(self: *Lowerer, lowered: LoweredExprLocals, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        var current = next;
        var i = lowered.ids.len;
        while (i > 0) {
            i -= 1;
            current = try self.lowerExprInto(lowered.ids[i], lowered.exprs[i], current);
        }
        return current;
    }

    fn addTemp(self: *Lowerer, ty: Type.TypeId) Common.LowerError!LIR.LocalId {
        return try self.addLocalForLayout(try self.layoutOfType(ty));
    }

    fn addLocalForLayout(self: *Lowerer, layout_idx: layout.Idx) Common.LowerError!LIR.LocalId {
        return try self.result.store.addLocal(.{ .layout_idx = layout_idx });
    }

    fn localFor(self: *Lowerer, local: LambdaMono.LocalId) Common.LowerError!LIR.LocalId {
        const index = @intFromEnum(local);
        if (self.local_map[index]) |existing| return existing;
        const program_local = self.program.locals.items[index];
        const lir_local = try self.addTemp(program_local.ty);
        self.local_map[index] = lir_local;
        return lir_local;
    }

    fn assignLocal(self: *Lowerer, target: LIR.LocalId, source: LIR.LocalId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        if (self.isZstLocal(target)) {
            if (!self.isZstLocal(source)) {
                Common.invariant("local assignment from non-zero-sized source into zero-sized target");
            }
            return try self.assignZst(target, next);
        }
        return try self.result.store.addCFStmt(.{ .assign_ref = .{
            .target = target,
            .op = .{ .local = source },
            .next = next,
        } });
    }

    fn assignZst(self: *Lowerer, target: LIR.LocalId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        if (!self.isZstLocal(target)) {
            Common.invariant("zero-sized assignment target was not a zero-sized layout");
        }
        return try self.result.store.addCFStmt(.{ .assign_struct = .{
            .target = target,
            .fields = LIR.LocalSpan.empty(),
            .next = next,
        } });
    }

    fn isZstLocal(self: *Lowerer, local: LIR.LocalId) bool {
        const layout_idx = self.result.store.getLocal(local).layout_idx;
        return self.result.layouts.isZeroSized(self.result.layouts.getLayout(layout_idx));
    }

    fn layoutOfType(self: *Lowerer, ty: Type.TypeId) Common.LowerError!layout.Idx {
        const index = @intFromEnum(ty);
        if (self.type_layouts[index]) |existing| return existing;

        var graph = layout.Graph{};
        defer graph.deinit(self.allocator);

        const local_nodes = try self.allocator.alloc(?layout.GraphNodeId, self.program.types.types.items.len);
        defer self.allocator.free(local_nodes);
        @memset(local_nodes, null);

        var builder = LayoutGraphBuilder{
            .lowerer = self,
            .graph = &graph,
            .local_nodes = local_nodes,
        };
        const root = try builder.inputForType(ty);
        var commit = try self.result.layouts.commitGraph(&graph, root);
        defer commit.deinit(self.allocator);

        if (layout.graphInputCommitted(root)) |layout_idx| {
            self.type_layouts[index] = layout_idx;
            return layout_idx;
        }

        const node = layout.graphInputLocal(root) orelse Common.invariant("layout graph root was neither committed nor local");
        for (local_nodes, 0..) |maybe_node, type_index| {
            if (maybe_node) |mapped_node| {
                self.type_layouts[type_index] = commit.value_layouts[@intFromEnum(mapped_node)];
            }
        }
        return commit.value_layouts[@intFromEnum(node)];
    }

    const LayoutGraphBuilder = struct {
        lowerer: *Lowerer,
        graph: *layout.Graph,
        local_nodes: []?layout.GraphNodeId,

        fn inputForType(self: *LayoutGraphBuilder, ty: Type.TypeId) Common.LowerError!layout.GraphInput {
            const index = @intFromEnum(ty);
            if (self.lowerer.type_layouts[index]) |layout_idx| return layout.committedGraphInput(layout_idx);
            if (self.local_nodes[index]) |node| return layout.localGraphInput(node);

            switch (self.lowerer.program.types.get(ty)) {
                .primitive => |primitive| return layout.committedGraphInput(primitiveLayout(primitive)),
                .zst => return layout.committedGraphInput(.zst),
                else => {},
            }

            const node = try self.graph.reserveNode(self.lowerer.allocator);
            self.local_nodes[index] = node;
            self.graph.setNode(node, try self.nodeForType(ty));
            return layout.localGraphInput(node);
        }

        fn nodeForType(self: *LayoutGraphBuilder, ty: Type.TypeId) Common.LowerError!layout.GraphNode {
            return switch (self.lowerer.program.types.get(ty)) {
                .primitive, .zst => unreachable,
                .named => |named| blk: {
                    const backing = named.backing orelse Common.invariant("named type without runtime backing reached layout selection");
                    break :blk .{ .nominal = try self.inputForType(backing.ty) };
                },
                .record => |fields| .{ .struct_ = try self.appendStructFields(self.lowerer.program.types.fieldSpan(fields)) },
                .capture_record => |fields| .{ .struct_ = try self.appendCaptureFields(self.lowerer.program.types.captureFieldSpan(fields)) },
                .tuple => |items| .{ .struct_ = try self.appendTupleFields(self.lowerer.program.types.span(items)) },
                .tag_union => |tags| .{ .tag_union = try self.appendTagPayloadInputs(self.lowerer.program.types.tagSpan(tags)) },
                .callable => |variants| .{ .tag_union = try self.appendCallablePayloadInputs(self.lowerer.program.types.fnVariantSpan(variants)) },
                .list => |elem| .{ .list = try self.inputForType(elem) },
                .box => |elem| .{ .box = try self.inputForType(elem) },
                .erased_fn => .erased_callable,
            };
        }

        fn appendTupleFields(self: *LayoutGraphBuilder, items: []const Type.TypeId) Common.LowerError!layout.GraphFieldSpan {
            const fields = try self.lowerer.allocator.alloc(layout.GraphField, items.len);
            defer self.lowerer.allocator.free(fields);
            for (items, 0..) |item, i| {
                fields[i] = .{ .index = @intCast(i), .child = try self.inputForType(item) };
            }
            return try self.graph.appendFields(self.lowerer.allocator, fields);
        }

        fn appendStructFields(self: *LayoutGraphBuilder, items: []const Type.Field) Common.LowerError!layout.GraphFieldSpan {
            const fields = try self.lowerer.allocator.alloc(layout.GraphField, items.len);
            defer self.lowerer.allocator.free(fields);
            for (items, 0..) |item, i| {
                fields[i] = .{ .index = @intCast(i), .child = try self.inputForType(item.ty) };
            }
            return try self.graph.appendFields(self.lowerer.allocator, fields);
        }

        fn appendCaptureFields(self: *LayoutGraphBuilder, items: []const Type.CaptureField) Common.LowerError!layout.GraphFieldSpan {
            const fields = try self.lowerer.allocator.alloc(layout.GraphField, items.len);
            defer self.lowerer.allocator.free(fields);
            for (items, 0..) |item, i| {
                fields[i] = .{ .index = @intCast(i), .child = try self.inputForType(item.ty) };
            }
            return try self.graph.appendFields(self.lowerer.allocator, fields);
        }

        fn appendTagPayloadInputs(self: *LayoutGraphBuilder, tags: []const Type.Tag) Common.LowerError!layout.GraphInputSpan {
            const refs = try self.lowerer.allocator.alloc(layout.GraphInput, tags.len);
            defer self.lowerer.allocator.free(refs);
            for (tags, 0..) |tag, i| {
                refs[i] = try self.payloadInput(self.lowerer.program.types.span(tag.payloads));
            }
            return try self.graph.appendRefs(self.lowerer.allocator, refs);
        }

        fn appendCallablePayloadInputs(self: *LayoutGraphBuilder, variants: []const Type.FnVariant) Common.LowerError!layout.GraphInputSpan {
            const refs = try self.lowerer.allocator.alloc(layout.GraphInput, variants.len);
            defer self.lowerer.allocator.free(refs);
            for (variants, 0..) |variant, i| {
                refs[i] = if (variant.capture_ty) |capture_ty| try self.inputForType(capture_ty) else layout.committedGraphInput(.zst);
            }
            return try self.graph.appendRefs(self.lowerer.allocator, refs);
        }

        fn payloadInput(self: *LayoutGraphBuilder, payloads: []const Type.TypeId) Common.LowerError!layout.GraphInput {
            return switch (payloads.len) {
                0 => layout.committedGraphInput(.zst),
                1 => try self.inputForType(payloads[0]),
                else => blk: {
                    const node = try self.graph.reserveNode(self.lowerer.allocator);
                    self.graph.setNode(node, .{ .struct_ = try self.appendTupleFields(payloads) });
                    break :blk layout.localGraphInput(node);
                },
            };
        }
    };

    fn primitiveLayout(primitive: MonoType.Primitive) layout.Idx {
        return switch (primitive) {
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
        };
    }

    fn tagIndex(self: *Lowerer, ty: Type.TypeId, name: Type.names.TagNameId) u16 {
        const tags = switch (self.program.types.get(ty)) {
            .tag_union => |tags| self.program.types.tagSpan(tags),
            .named => |named| if (named.backing) |backing| return self.tagIndex(backing.ty, name) else Common.invariant("named tag has no backing"),
            else => Common.invariant("tag operation expected tag-union type"),
        };
        for (tags, 0..) |tag, index| {
            if (tag.name == name) return @intCast(index);
        }
        Common.invariant("tag operation referenced tag absent from Lambda Mono type");
    }

    fn callableVariantIndex(self: *Lowerer, ty: Type.TypeId, variant: Type.FnVariantId) u16 {
        const variants = switch (self.program.types.get(ty)) {
            .callable => |variants| self.program.types.fnVariantSpan(variants),
            else => Common.invariant("callable operation expected callable type"),
        };
        for (variants, 0..) |item, index| {
            if (item.id == variant) return @intCast(index);
        }
        Common.invariant("callable operation referenced variant absent from Lambda Mono type");
    }

    fn recordFields(self: *Lowerer, ty: Type.TypeId) []const Type.Field {
        return switch (self.program.types.get(ty)) {
            .record => |fields| self.program.types.fieldSpan(fields),
            .named => |named| if (named.backing) |backing| return self.recordFields(backing.ty) else Common.invariant("named record has no backing"),
            else => Common.invariant("record operation expected record type"),
        };
    }

    fn recordFieldIndex(self: *Lowerer, ty: Type.TypeId, name: Type.names.RecordFieldNameId) u16 {
        for (self.recordFields(ty), 0..) |field, index| {
            if (field.name == name) return @intCast(index);
        }
        Common.invariant("record operation referenced field absent from Lambda Mono type");
    }

    fn captureFieldIndex(self: *Lowerer, ty: Type.TypeId, symbol: Common.Symbol) u16 {
        const fields = switch (self.program.types.get(ty)) {
            .capture_record => |fields| self.program.types.captureFieldSpan(fields),
            else => Common.invariant("capture access expected capture record type"),
        };
        for (fields, 0..) |field, index| {
            if (field.symbol == symbol) return @intCast(index);
        }
        Common.invariant("capture access referenced symbol absent from capture record");
    }

    fn tagUnionPayloadLayout(self: *Lowerer, tag_union_layout_idx: layout.Idx, variant_index: u16) layout.Idx {
        const tag_union_layout = self.result.layouts.getLayout(tag_union_layout_idx);
        return switch (tag_union_layout.tag) {
            .tag_union => blk: {
                const data = self.result.layouts.getTagUnionData(tag_union_layout.data.tag_union.idx);
                const variants = self.result.layouts.getTagUnionVariants(data);
                if (variant_index >= variants.len) Common.invariant("tag payload variant exceeded committed tag-union layout");
                break :blk variants.get(@intCast(variant_index)).payload_layout;
            },
            .zst, .scalar => .zst,
            else => Common.invariant("tag payload operation expected tag-union layout"),
        };
    }

    fn currentLoop(self: *Lowerer) LoopContext {
        if (self.loop_stack.items.len == 0) Common.invariant("loop control expression reached LIR outside a loop");
        return self.loop_stack.items[self.loop_stack.items.len - 1];
    }

    fn freshJoinPointId(self: *Lowerer) LIR.JoinPointId {
        const id: LIR.JoinPointId = @enumFromInt(self.next_join_point);
        self.next_join_point += 1;
        return id;
    }

    fn expr(self: *const Lowerer, id: LambdaMono.ExprId) LambdaMono.Expr {
        return self.program.exprs.items[@intFromEnum(id)];
    }

    fn pat(self: *const Lowerer, id: LambdaMono.PatId) LambdaMono.Pat {
        return self.program.pats.items[@intFromEnum(id)];
    }
};

fn lirSymbol(symbol: Common.Symbol) LIR.Symbol {
    return LIR.Symbol.fromRaw(@intCast(@intFromEnum(symbol)));
}

fn constFnTemplateFromMono(template: Mono.FnTemplate) LirProgram.FnTemplate {
    return .{
        .fn_def = constFnDefFromMono(template.fn_def),
        .source_fn_ty = template.source_fn_ty,
    };
}

fn constFnDefFromMono(fn_def: Mono.FnDef) check.ConstStore.FnDef {
    return switch (fn_def) {
        .local_template => |template| .{ .local_template = template },
        .imported_template => |template| .{ .imported_template = template },
        .nested => |nested| .{ .nested = .{
            .owner = nested.owner,
            .site = nested.site,
        } },
        .local_hosted => |template| .{ .local_hosted = template },
        .imported_hosted => |template| .{ .imported_hosted = template },
        .checked_generated => |template| .{ .checked_generated = template },
    };
}

test "direct LIR lower declarations are referenced" {
    std.testing.refAllDecls(@This());
}
