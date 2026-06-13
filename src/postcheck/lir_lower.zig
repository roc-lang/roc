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

/// Runtime field order for a named record field.
pub const RuntimeRecordFieldSchema = struct {
    name: []const u8,
    logical_index: u32,
};

/// Runtime field order for a named record.
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

/// Runtime discriminant for a named tag.
pub const RuntimeTagSchema = struct {
    name: []const u8,
    discriminant: u16,
};

/// Runtime tag order for a named tag union.
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

/// Runtime schemas emitted while lowering Lambda Mono to LIR.
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

/// LIR result plus runtime schemas emitted by direct LIR lowering.
pub const Output = struct {
    lir_result: LirProgram.Result,
    runtime_schemas: RuntimeSchemaStore,

    pub fn deinit(self: *Output) void {
        self.runtime_schemas.deinit();
        self.lir_result.deinit();
    }
};

/// Lower Lambda Mono directly into LIR.
pub fn run(
    allocator: std.mem.Allocator,
    target_usize: base.target.TargetUsize,
    program: LambdaMono.Program,
) Common.LowerError!Output {
    var owned = program;
    errdefer owned.deinit();

    var lowerer = try Lowerer.init(allocator, target_usize, &owned);
    errdefer lowerer.deinit();

    try lowerer.result.store.setSourceFiles(owned.source_files.items);
    try lowerer.registerProcPlaceholders();
    try lowerer.lowerAllFns();
    try lowerer.bindRoots();
    try lowerer.writeRuntimeSchemas();

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
    current_ret_ty: ?Type.TypeId = null,
    current_proc_locals: ?*ProcLocalSet = null,

    const ProcLocalSet = std.AutoArrayHashMapUnmanaged(u32, void);

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

            self.result.store.current_loc = switch (fn_.body) {
                .roc => |body_id| self.program.exprLoc(body_id),
                .hosted => base.SourceLoc.none,
            };
            const proc_id = try self.result.store.addProcSpec(.{
                .name = lirSymbol(fn_.symbol),
                .args = try self.result.store.addLocalSpan(arg_locals),
                .body = null,
                .ret_layout = try self.layoutOfType(fn_.ret),
                .abi = if (self.usesErasedCallableAbi(fn_)) .erased_callable else .roc,
                .hosted = try self.hostedProcForFn(fn_),
            });
            if (self.program.procDebugName(fn_.symbol)) |name| {
                try self.result.store.setProcDebugName(proc_id, self.program.names.exportNameText(name));
            }
            self.result.store.current_loc = base.SourceLoc.none;
            self.fn_map[index] = proc_id;
        }
    }

    fn hostedProcForFn(self: *Lowerer, fn_: LambdaMono.Fn) Common.LowerError!?LIR.HostedProc {
        const source = fn_.source orelse return null;
        return switch (source.fn_def) {
            .local_hosted,
            .imported_hosted,
            => |hosted| .{
                .symbol = try self.result.store.insertString(
                    self.program.names.externalSymbolNameText(hosted.external_symbol_name),
                ),
                .dispatch_index = hosted.dispatch_index,
            },
            else => null,
        };
    }

    fn usesErasedCallableAbi(self: *Lowerer, fn_: LambdaMono.Fn) bool {
        const args = self.program.typedLocalSpan(fn_.args);
        if (args.len == 0) return false;
        return switch (self.program.types.get(args[args.len - 1].ty)) {
            .erased_capture_ptr => true,
            else => false,
        };
    }

    fn noteLocal(self: *Lowerer, local: LIR.LocalId) Common.LowerError!void {
        if (self.current_proc_locals) |locals| {
            try locals.put(self.allocator, @intFromEnum(local), {});
        }
    }

    fn noteLocalSpan(self: *Lowerer, span: LIR.LocalSpan) Common.LowerError!void {
        for (self.result.store.getLocalSpan(span)) |local| {
            try self.noteLocal(local);
        }
    }

    fn writeFrameLocals(self: *Lowerer, locals: *ProcLocalSet) Common.LowerError!LIR.LocalSpan {
        const raw_ids = locals.keys();
        const sorted = try self.allocator.alloc(LIR.LocalId, raw_ids.len);
        defer self.allocator.free(sorted);
        for (raw_ids, 0..) |raw_id, i| {
            sorted[i] = @enumFromInt(raw_id);
        }
        std.mem.sort(LIR.LocalId, sorted, {}, localIdLessThan);
        return try self.result.store.addLocalSpan(sorted);
    }

    fn localIdLessThan(_: void, a: LIR.LocalId, b: LIR.LocalId) bool {
        return @intFromEnum(a) < @intFromEnum(b);
    }

    fn lowerAllFns(self: *Lowerer) Common.LowerError!void {
        for (self.program.fns.items, 0..) |fn_, index| {
            const proc_id = self.fn_map[index];
            switch (fn_.body) {
                .roc => |body_expr| {
                    const saved_ret_ty = self.current_ret_ty;
                    const saved_proc_locals = self.current_proc_locals;
                    var proc_locals: ProcLocalSet = .{};
                    defer proc_locals.deinit(self.allocator);

                    self.current_proc_locals = &proc_locals;
                    self.current_ret_ty = fn_.ret;
                    defer {
                        self.current_ret_ty = saved_ret_ty;
                        self.current_proc_locals = saved_proc_locals;
                    }

                    try self.noteLocalSpan(self.result.store.getProcSpec(proc_id).args);
                    const body = try self.lowerExprReturn(body_expr, fn_.ret);

                    const frame_locals = try self.writeFrameLocals(&proc_locals);
                    const proc = self.result.store.getProcSpecPtr(proc_id);
                    proc.body = body;
                    proc.frame_locals = frame_locals;
                },
                .hosted => {
                    if (self.result.store.getProcSpec(proc_id).hosted == null) {
                        Common.invariant("hosted Lambda Mono function reached LIR without hosted metadata");
                    }
                },
            }
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

        for (self.program.layout_requests.items) |request| {
            try self.result.requested_layouts.append(self.allocator, .{
                .ty = self.program.types.typeDigest(&self.program.names, request.ty),
                .checked_type = request.checked_type,
                .layout_idx = try self.layoutOfType(request.ty),
                .plan = try self.constPlanOfType(request.ty),
            });
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
                .bool => Common.invariant("primitive Bool reached ConstStore plan output; Bool must be a checked named tag union"),
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
                    for (variants[0..initialized]) |variant| {
                        self.allocator.free(variant.name);
                        self.allocator.free(variant.payloads);
                    }
                    self.allocator.free(variants);
                }
                for (source, 0..) |tag, i| {
                    const name = try self.allocator.dupe(u8, self.program.names.tagLabelText(tag.name));
                    errdefer self.allocator.free(name);
                    const payload_tys = self.program.types.span(tag.payloads);
                    const payloads = try self.allocator.alloc(LirProgram.ConstPlanId, payload_tys.len);
                    var payloads_owned = true;
                    errdefer if (payloads_owned) self.allocator.free(payloads);
                    for (payload_tys, 0..) |payload_ty, j| payloads[j] = try self.constPlanOfType(payload_ty);
                    variants[i] = .{
                        .name = name,
                        .checked_name = tag.checked_name,
                        .discriminant = @intCast(i),
                        .payloads = payloads,
                    };
                    payloads_owned = false;
                    initialized += 1;
                }
                break :blk .{ .tag_union = variants };
            },
            .named => |named| blk: {
                const backing = named.backing orelse Common.invariant("named type without backing reached ConstStore plan output");
                break :blk .{ .named = .{
                    .named_type = .{
                        .module = named.named_type.module,
                        .ty = named.named_type.ty,
                    },
                    .backing = try self.constPlanOfType(backing.ty),
                } };
            },
            .callable => |variants| .{ .fn_value = try self.fnSetForType(ty, variants) },
            .erased_fn => |erased| .{ .erased_fn = try self.erasedFnsForType(ty, erased) },
            .capture_record => Common.invariant("capture record reached root ConstStore plan output"),
            .erased_capture_ptr => Common.invariant("erased capture pointer reached root ConstStore plan output"),
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
                    try self.callablePayloadLayout(value_layout, type_variants.len, @intCast(index), capture_ty)
                else
                    .zst,
                .template = constFnTemplateFromMono(self.fnTemplateForFn(variant.target)),
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

    fn erasedFnsForType(self: *Lowerer, ty: Type.TypeId, erased: anytype) Common.LowerError!LirProgram.ErasedFnsId {
        const members = self.program.types.fnVariantSpan(erased.members);
        if (members.len == 0) {
            Common.invariant("erased function ConstStore output requires explicit erased function entries");
        }

        const entries = try self.allocator.alloc(LirProgram.ErasedFn, members.len);
        var initialized: usize = 0;
        errdefer {
            for (entries[0..initialized]) |entry| {
                if (entry.captures.len > 0) self.allocator.free(entry.captures);
            }
            self.allocator.free(entries);
        }

        for (members, 0..) |member, index| {
            const captures = if (member.capture_ty) |capture_ty|
                try self.captureSlotsForType(capture_ty)
            else
                &.{};
            var captures_owned = captures.len > 0;
            errdefer if (captures_owned) self.allocator.free(captures);

            entries[index] = .{
                .entry = self.fn_map[@intFromEnum(member.target)],
                .capture_layout = if (member.capture_ty) |capture_ty| try self.layoutOfType(capture_ty) else .zst,
                .template = constFnTemplateFromMono(self.fnTemplateForFn(member.target)),
                .captures = captures,
            };
            captures_owned = false;
            initialized += 1;
        }

        const id: LirProgram.ErasedFnsId = @enumFromInt(@as(u32, @intCast(self.result.erased_fns.items.len)));
        try self.result.erased_fns.append(self.allocator, .{
            .layout = try self.layoutOfType(ty),
            .entries = entries,
        });
        return id;
    }

    fn callablePayloadLayout(
        self: *Lowerer,
        value_layout: layout.Idx,
        variant_count: usize,
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
        if (value_layout_data.tag == .tag_union) {
            return self.tagUnionPayloadLayout(value_layout, variant_index);
        }
        if (variant_count != 1) {
            Common.invariant("multi-variant callable with captures did not commit a tag-union layout");
        }
        return capture_layout;
    }

    fn captureSlotsForType(self: *Lowerer, ty: Type.TypeId) Common.LowerError![]const LirProgram.CaptureSlot {
        const fields = switch (self.program.types.get(ty)) {
            .capture_record => |fields| self.program.types.captureFieldSpan(fields),
            else => Common.invariant("function result capture slot output expected capture record type"),
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

    fn fnTemplateForFn(self: *Lowerer, fn_id: LambdaMono.FnId) Mono.FnTemplate {
        const raw = @intFromEnum(fn_id);
        if (raw >= self.program.fns.items.len) Common.invariant("function result referenced a missing function");
        return self.program.fns.items[raw].source orelse
            Common.invariant("function result referenced a generated function without checked source identity");
    }

    fn writeRuntimeSchemas(self: *Lowerer) Common.LowerError!void {
        for (self.program.runtime_schema_requests.items) |request| {
            try self.writeRuntimeSchema(request);
        }
    }

    fn writeRuntimeSchema(self: *Lowerer, request: LambdaMono.RuntimeSchemaRequest) Common.LowerError!void {
        const content = self.program.types.get(request.ty);
        const named = switch (content) {
            .named => |named| named,
            else => Common.invariant("runtime schema request did not reference a named Lambda Mono type"),
        };
        if (named.def.module_name != request.def.module_name or named.def.type_name != request.def.type_name) {
            Common.invariant("runtime schema request named type identity changed before LIR lowering");
        }

        const backing = named.backing orelse Common.invariant("runtime schema request referenced a named type without runtime backing");
        if (backing.use != .inspectable) {
            Common.invariant("runtime schema request referenced a named type whose backing is not inspectable");
        }
        _ = try self.layoutOfType(request.ty);

        const type_name = self.program.names.typeNameText(request.def.type_name);
        switch (self.runtimeSchemaShape(backing.ty)) {
            .record => try self.writeRecordSchema(type_name, request.ty),
            .tag_union => try self.writeTagUnionSchema(type_name, request.ty),
        }
    }

    const RuntimeSchemaShape = enum {
        record,
        tag_union,
    };

    fn runtimeSchemaShape(self: *Lowerer, ty: Type.TypeId) RuntimeSchemaShape {
        return switch (self.program.types.get(ty)) {
            .record => .record,
            .tag_union => .tag_union,
            .named => |named| if (named.backing) |backing| blk: {
                if (backing.use != .inspectable) {
                    Common.invariant("runtime schema request crossed a non-inspectable named backing");
                }
                break :blk self.runtimeSchemaShape(backing.ty);
            } else Common.invariant("runtime schema request crossed a named type without backing"),
            else => Common.invariant("runtime schema request backing was not a record or tag union"),
        };
    }

    fn writeRecordSchema(self: *Lowerer, type_name: []const u8, ty: Type.TypeId) Common.LowerError!void {
        const source = self.recordFields(ty);
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

    fn writeTagUnionSchema(self: *Lowerer, type_name: []const u8, ty: Type.TypeId) Common.LowerError!void {
        const source = self.tagUnionTags(ty);
        const schema_tags = try self.allocator.alloc(RuntimeTagSchema, source.len);
        errdefer {
            for (schema_tags) |tag| self.allocator.free(tag.name);
            self.allocator.free(schema_tags);
        }
        for (source, 0..) |tag, index| {
            schema_tags[index] = .{
                .name = try self.allocator.dupe(u8, self.program.names.tagLabelText(tag.name)),
                .discriminant = self.tagIndex(ty, tag.name),
            };
        }
        try self.runtime_schemas.tag_unions.append(self.allocator, .{
            .type_name = try self.allocator.dupe(u8, type_name),
            .tags = schema_tags,
        });
    }

    fn lowerExprReturn(self: *Lowerer, expr_id: LambdaMono.ExprId, ret_ty: Type.TypeId) Common.LowerError!LIR.CFStmtId {
        const ret_local = try self.addTemp(ret_ty);
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
        const saved_loc = self.result.store.current_loc;
        defer self.result.store.current_loc = saved_loc;
        self.result.store.current_loc = self.program.exprLoc(expr_id);
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
            .str_lit => |literal| blk: {
                const str_lit = self.program.stringLiteral(literal);
                break :blk try self.result.store.addCFStmt(.{ .assign_literal = .{
                    .target = target,
                    .value = .{ .str_literal = try self.result.store.insertStringView(str_lit.backing, str_lit.offset, str_lit.len) },
                    .next = next,
                } });
            },
            .list => |items| try self.lowerListInto(target, items, next),
            .tuple => |items| try self.lowerTupleInto(target, items, next),
            .record => |fields| try self.lowerRecordInto(target, expr_data.ty, fields, next),
            .capture_record => |items| try self.lowerCaptureRecordInto(target, items, next),
            .tag => |tag| try self.lowerTagInto(target, expr_data.ty, tag.name, tag.payloads, next),
            .callable => |callable| try self.lowerCallableInto(target, callable, next),
            .nominal => |backing| try self.lowerNominalInto(target, expr_data.ty, backing, next),
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
                const after_dbg = try self.assignZst(target, next);
                const message = try self.addTemp(self.expr(child).ty);
                const debug_stmt = try self.result.store.addCFStmt(.{ .debug = .{ .message = message, .next = after_dbg } });
                break :blk try self.lowerExprInto(message, child, debug_stmt);
            },
            .expect_err => |expect_err| blk: {
                const message = try self.addTemp(self.expr(expect_err.msg).ty);
                const expect_err_stmt = try self.result.store.addCFStmt(.{ .expect_err = .{
                    .message = message,
                    .region = expect_err.region,
                } });
                break :blk try self.lowerExprInto(message, expect_err.msg, expect_err_stmt);
            },
            .expect => |child| try self.lowerExpectExprInto(target, child, next),
        };
    }

    fn lowerListInto(self: *Lowerer, target: LIR.LocalId, span: LambdaMono.Span(LambdaMono.ExprId), next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const items = self.program.exprSpan(span);
        const expr_locals = try self.allocator.alloc(LIR.LocalId, items.len);
        defer self.allocator.free(expr_locals);
        const elem_locals = try self.allocator.alloc(LIR.LocalId, items.len);
        defer self.allocator.free(elem_locals);

        const elem_layout = self.localListElemLayout(target);
        for (items, 0..) |expr_id, i| {
            expr_locals[i] = try self.addTemp(self.expr(expr_id).ty);
            const expr_layout = self.result.store.getLocal(expr_locals[i]).layout_idx;
            elem_locals[i] = if (expr_layout == elem_layout)
                expr_locals[i]
            else
                try self.addLocalForLayout(elem_layout);
        }

        var current = try self.result.store.addCFStmt(.{ .assign_list = .{
            .target = target,
            .elems = try self.result.store.addLocalSpan(elem_locals),
            .next = next,
        } });
        var i = items.len;
        while (i > 0) {
            i -= 1;
            if (elem_locals[i] != expr_locals[i]) {
                current = try self.assignBoxBoundary(
                    elem_locals[i],
                    expr_locals[i],
                    self.result.store.getLocal(expr_locals[i]).layout_idx,
                    current,
                );
            }
            current = try self.lowerExprInto(expr_locals[i], items[i], current);
        }
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
        const expr_locals = try self.allocator.alloc(LIR.LocalId, items.len);
        defer self.allocator.free(expr_locals);
        const field_locals = try self.allocator.alloc(LIR.LocalId, items.len);
        defer self.allocator.free(field_locals);

        const target_is_zst = self.isZstLocal(target);
        for (items, 0..) |expr_id, i| {
            expr_locals[i] = try self.addTemp(self.expr(expr_id).ty);
            if (target_is_zst) {
                field_locals[i] = expr_locals[i];
                continue;
            }
            const field_layout = self.localFieldLayout(target, @intCast(i));
            const expr_layout = self.result.store.getLocal(expr_locals[i]).layout_idx;
            field_locals[i] = if (expr_layout == field_layout)
                expr_locals[i]
            else
                try self.addLocalForLayout(field_layout);
        }

        var current = if (target_is_zst)
            try self.assignZst(target, next)
        else
            try self.result.store.addCFStmt(.{ .assign_struct = .{
                .target = target,
                .fields = try self.result.store.addLocalSpan(field_locals),
                .next = next,
            } });
        var i = items.len;
        while (i > 0) {
            i -= 1;
            if (field_locals[i] != expr_locals[i]) {
                current = try self.assignBoxBoundary(
                    field_locals[i],
                    expr_locals[i],
                    self.result.store.getLocal(expr_locals[i]).layout_idx,
                    current,
                );
            }
            current = try self.lowerExprInto(expr_locals[i], items[i], current);
        }
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
            } else Common.invariant("record expression missing field output by Lambda Mono type");
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

    fn lowerNominalInto(self: *Lowerer, target: LIR.LocalId, nominal_ty: Type.TypeId, backing: LambdaMono.ExprId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const backing_ty = self.nominalBackingType(nominal_ty, backing);
        const backing_layout = try self.layoutOfType(backing_ty);
        const backing_local = try self.addLocalForLayout(backing_layout);
        const assign = try self.assignNominalBoundary(target, backing_local, backing_layout, next);
        return try self.lowerExprInto(backing_local, backing, assign);
    }

    fn nominalBackingType(self: *Lowerer, nominal_ty: Type.TypeId, backing: LambdaMono.ExprId) Type.TypeId {
        return switch (self.program.types.get(nominal_ty)) {
            .named => |named| (named.backing orelse Common.invariant("nominal expression target had no runtime backing")).ty,
            .box => |elem| elem,
            else => self.expr(backing).ty,
        };
    }

    fn assignNominalBoundary(
        self: *Lowerer,
        target: LIR.LocalId,
        backing_local: LIR.LocalId,
        backing_layout: layout.Idx,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        return try self.assignBoxBoundary(target, backing_local, backing_layout, next);
    }

    fn assignBoxBoundary(
        self: *Lowerer,
        target: LIR.LocalId,
        source: LIR.LocalId,
        source_layout: layout.Idx,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const target_layout = self.result.store.getLocal(target).layout_idx;
        if (target_layout == source_layout) return try self.assignLocal(target, source, next);
        const target_content = self.result.layouts.getLayout(target_layout);
        const source_content = self.result.layouts.getLayout(source_layout);
        if (target_content.eql(source_content)) return try self.assignLocal(target, source, next);
        if (target_content.tag == .box and self.result.layouts.getLayout(target_content.getIdx()).eql(source_content)) {
            return try self.assignUnaryLowLevel(target, .box_box, source, next);
        }
        if (target_content.tag == .box_of_zst and self.result.layouts.isZeroSized(source_content)) {
            return try self.assignUnaryLowLevel(target, .box_box, source, next);
        }
        if (source_content.tag == .box and self.result.layouts.getLayout(source_content.getIdx()).eql(target_content)) {
            return try self.assignUnaryLowLevel(target, .box_unbox, source, next);
        }
        if (source_content.tag == .box_of_zst and self.result.layouts.isZeroSized(target_content)) {
            return try self.assignUnaryLowLevel(target, .box_unbox, source, next);
        }

        if (self.isZstLocal(target)) {
            if (!self.isZstLocal(source)) {
                Common.invariant("box boundary tried to store non-zero-sized source into zero-sized target");
            }
            return try self.assignZst(target, next);
        }

        if (@import("builtin").mode == .Debug) {
            if (target_content.tag == .list and source_content.tag == .list) {
                const target_elem = target_content.getIdx();
                const source_elem = source_content.getIdx();
                std.debug.panic(
                    "postcheck invariant violated: LIR lowering expected matching list layouts, target={d} elem={d} ({s}) source={d} elem={d} ({s})",
                    .{
                        @intFromEnum(target_layout),
                        @intFromEnum(target_elem),
                        @tagName(self.result.layouts.getLayout(target_elem).tag),
                        @intFromEnum(source_layout),
                        @intFromEnum(source_elem),
                        @tagName(self.result.layouts.getLayout(source_elem).tag),
                    },
                );
            }
            std.debug.panic(
                "postcheck invariant violated: LIR lowering expected layouts to match or differ by an explicit Box edge, target={d} ({s}) source={d} ({s})",
                .{
                    @intFromEnum(target_layout),
                    @tagName(target_content.tag),
                    @intFromEnum(source_layout),
                    @tagName(source_content.tag),
                },
            );
        }
        unreachable;
    }

    fn assignRefRead(
        self: *Lowerer,
        target: LIR.LocalId,
        storage_layout: layout.Idx,
        op: LIR.RefOp,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const target_layout = self.result.store.getLocal(target).layout_idx;
        if (target_layout == storage_layout) {
            if (self.isZstLocal(target)) return try self.assignZst(target, next);
            return try self.result.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = op,
                .next = next,
            } });
        }

        const storage_local = try self.addLocalForLayout(storage_layout);
        const after_read = try self.assignBoxBoundary(target, storage_local, storage_layout, next);
        if (self.isZstLocal(storage_local)) return try self.assignZst(storage_local, after_read);
        return try self.result.store.addCFStmt(.{ .assign_ref = .{
            .target = storage_local,
            .op = op,
            .next = after_read,
        } });
    }

    fn assignUnaryLowLevel(
        self: *Lowerer,
        target: LIR.LocalId,
        op: LIR.LowLevel,
        arg: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const args = [_]LIR.LocalId{arg};
        return try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = op.rcEffect(),
            .args = try self.result.store.addLocalSpan(&args),
            .next = next,
        } });
    }

    fn lowerLetInto(self: *Lowerer, target: LIR.LocalId, let_: anytype, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const rest = try self.lowerExprInto(target, let_.rest, next);
        const value_expr = self.expr(let_.value);
        const value_local = try self.addTemp(value_expr.ty);
        const bind = try self.bindPatternOrCrash(let_.bind, value_local, rest);
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
        switch (op) {
            .box_box,
            .box_unbox,
            => return try self.lowerBoxBoundaryLowLevelInto(target, op, args, next),
            else => {},
        }
        if (lowLevelNeedsIntegerZeroDenominatorCheck(op)) {
            return try self.lowerIntegerZeroDenominatorCheckedLowLevelInto(target, op, args, next);
        }

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

    fn lowLevelNeedsIntegerZeroDenominatorCheck(op: LIR.LowLevel) bool {
        return switch (op) {
            .num_div_by,
            .num_div_trunc_by,
            .num_rem_by,
            .num_mod_by,
            => true,
            else => false,
        };
    }

    fn integerDivisionByZeroMessage(layout_idx: layout.Idx) ?[]const u8 {
        return switch (layout_idx) {
            .u8 => "U8 division by zero",
            .i8 => "I8 division by zero",
            .u16 => "U16 division by zero",
            .i16 => "I16 division by zero",
            .u32 => "U32 division by zero",
            .i32 => "I32 division by zero",
            .u64 => "U64 division by zero",
            .i64 => "I64 division by zero",
            .u128 => "U128 division by zero",
            .i128 => "I128 division by zero",
            else => null,
        };
    }

    fn lowerIntegerZeroDenominatorCheckedLowLevelInto(
        self: *Lowerer,
        target: LIR.LocalId,
        op: LIR.LowLevel,
        args: []const LambdaMono.ExprId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (args.len != 2) {
            Common.invariant("integer division/remainder reached LIR lowering with the wrong arity");
        }

        const lowered = try self.lowerExprsToTemps(args);
        defer self.allocator.free(lowered.ids);

        const lhs = lowered.ids[0];
        const rhs = lowered.ids[1];
        const rhs_layout = self.result.store.getLocal(rhs).layout_idx;
        const crash_message = integerDivisionByZeroMessage(rhs_layout) orelse {
            var current = try self.result.store.addCFStmt(.{ .assign_low_level = .{
                .target = target,
                .op = op,
                .rc_effect = op.rcEffect(),
                .args = try self.result.store.addLocalSpan(lowered.ids),
                .next = next,
            } });
            current = try self.prependExprs(lowered, current);
            return current;
        };

        const zero = try self.addLocalForLayout(rhs_layout);
        const is_zero = try self.addLocalForLayout(.bool);

        const div_args = [_]LIR.LocalId{ lhs, rhs };
        const div_stmt = try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = op.rcEffect(),
            .args = try self.result.store.addLocalSpan(&div_args),
            .next = next,
        } });

        const crash_stmt = try self.result.store.addCFStmt(.{ .crash = .{
            .msg = try self.result.store.insertString(crash_message),
        } });

        const switch_stmt = try self.boolSwitchNoContinuation(is_zero, crash_stmt, div_stmt);

        const eq_args = [_]LIR.LocalId{ rhs, zero };
        const eq_stmt = try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = is_zero,
            .op = .num_is_eq,
            .rc_effect = LIR.LowLevel.num_is_eq.rcEffect(),
            .args = try self.result.store.addLocalSpan(&eq_args),
            .next = switch_stmt,
        } });

        var current = try self.result.store.addCFStmt(.{ .assign_literal = .{
            .target = zero,
            .value = .{ .i128_literal = .{
                .value = 0,
                .layout_idx = rhs_layout,
            } },
            .next = eq_stmt,
        } });
        current = try self.prependExprs(lowered, current);
        return current;
    }

    fn lowerBoxBoundaryLowLevelInto(self: *Lowerer, target: LIR.LocalId, op: can.CIR.Expr.LowLevel, args: []const LambdaMono.ExprId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        if (args.len != 1) Common.invariant("Box low-level operation reached LIR lowering with the wrong arity");
        const source = try self.addTemp(self.expr(args[0]).ty);
        const source_layout = self.result.store.getLocal(source).layout_idx;
        const assign = switch (op) {
            .box_box,
            .box_unbox,
            => try self.assignBoxBoundary(target, source, source_layout, next),
            else => unreachable,
        };
        return try self.lowerExprInto(source, args[0], assign);
    }

    fn lowerFieldAccessInto(self: *Lowerer, target: LIR.LocalId, receiver: LambdaMono.ExprId, field_name: Type.names.RecordFieldNameId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const receiver_expr = self.expr(receiver);
        const receiver_local = try self.addTemp(receiver_expr.ty);
        const field_index = self.recordFieldIndex(receiver_expr.ty, field_name);
        if (self.isZstLocal(target)) {
            return try self.lowerExprInto(receiver_local, receiver, try self.assignZst(target, next));
        }
        const assign = try self.assignRefRead(
            target,
            self.localFieldLayout(receiver_local, field_index),
            .{ .field = .{ .source = receiver_local, .field_idx = field_index } },
            next,
        );
        return try self.lowerExprInto(receiver_local, receiver, assign);
    }

    fn lowerCaptureAccessInto(self: *Lowerer, target: LIR.LocalId, slot: LambdaMono.CaptureSlot, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const record_expr = self.expr(slot.record);
        const record_local = try self.addTemp(record_expr.ty);
        const field_index = self.captureFieldIndex(record_expr.ty, slot.symbol);
        if (self.isZstLocal(target)) {
            return try self.lowerExprInto(record_local, slot.record, try self.assignZst(target, next));
        }
        const assign = try self.assignRefRead(
            target,
            self.localFieldLayout(record_local, field_index),
            .{ .field = .{ .source = record_local, .field_idx = field_index } },
            next,
        );
        return try self.lowerExprInto(record_local, slot.record, assign);
    }

    fn lowerTupleAccessInto(self: *Lowerer, target: LIR.LocalId, tuple: LambdaMono.ExprId, elem_index: u32, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const tuple_expr = self.expr(tuple);
        const tuple_local = try self.addTemp(tuple_expr.ty);
        const field_index: u16 = @intCast(elem_index);
        if (self.isZstLocal(target)) {
            return try self.lowerExprInto(tuple_local, tuple, try self.assignZst(target, next));
        }
        const assign = try self.assignRefRead(
            target,
            self.localFieldLayout(tuple_local, field_index),
            .{ .field = .{ .source = tuple_local, .field_idx = field_index } },
            next,
        );
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
        const done = self.freshJoinPointId();
        const branch_chain = try self.lowerBranchChain(scrutinee_local, branches, target, done);
        const remainder = try self.lowerExprInto(scrutinee_local, scrutinee, branch_chain);
        return try self.result.store.addCFStmt(.{ .join = .{
            .id = done,
            .params = try self.result.store.addLocalSpan(&[_]LIR.LocalId{target}),
            .body = next,
            .remainder = remainder,
        } });
    }

    fn lowerIfInto(self: *Lowerer, target: LIR.LocalId, branches_span: LambdaMono.Span(LambdaMono.IfBranch), final_else: LambdaMono.ExprId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const branches = self.program.ifBranchSpan(branches_span);
        const done = self.freshJoinPointId();
        var current = try self.lowerExprInto(target, final_else, try self.joinJump(done));
        var i = branches.len;
        while (i > 0) {
            i -= 1;
            const body = try self.lowerExprInto(target, branches[i].body, try self.joinJump(done));
            const cond_local = try self.addTemp(self.expr(branches[i].cond).ty);
            const switch_stmt = try self.boolSwitchNoContinuation(cond_local, body, current);
            current = try self.lowerExprInto(cond_local, branches[i].cond, switch_stmt);
        }
        return try self.result.store.addCFStmt(.{ .join = .{
            .id = done,
            .params = try self.result.store.addLocalSpan(&[_]LIR.LocalId{target}),
            .body = next,
            .remainder = current,
        } });
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
        const saved_loc = self.result.store.current_loc;
        defer self.result.store.current_loc = saved_loc;
        self.result.store.current_loc = self.program.stmtLoc(stmt_id);
        return switch (self.program.stmts.items[@intFromEnum(stmt_id)]) {
            .let_ => |let_| blk: {
                const value = try self.addTemp(self.expr(let_.value).ty);
                const bind = try self.bindPatternOrCrash(let_.pat, value, next);
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
        } });
        initial_jump = try self.prependJoinParamInitializers(param_span, jump_args.ids, initial_jump);
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
        } });
        jump = try self.prependJoinParamInitializers(loop.params, locals.ids, jump);
        jump = try self.prependExprs(locals, jump);
        return jump;
    }

    fn lowerReturn(self: *Lowerer, expr_id: LambdaMono.ExprId) Common.LowerError!LIR.CFStmtId {
        const ret_ty = self.current_ret_ty orelse Common.invariant("return expression reached LIR lowering outside a function");
        const ret_local = try self.addTemp(ret_ty);
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

    const PatternMiss = struct {
        join_id: LIR.JoinPointId,
    };

    fn lowerBranchChain(self: *Lowerer, scrutinee: LIR.LocalId, branches: []const LambdaMono.Branch, target: LIR.LocalId, done: LIR.JoinPointId) Common.LowerError!LIR.CFStmtId {
        var current = try self.result.store.addCFStmt(.{ .runtime_error = {} });
        var i = branches.len;
        while (i > 0) {
            i -= 1;
            const next_branch = current;
            const needs_miss_join = branches[i].guard != null or self.patternCanMiss(branches[i].pat);
            const miss = if (needs_miss_join)
                PatternMiss{ .join_id = self.freshJoinPointId() }
            else
                null;
            const branch_done = try self.joinJump(done);
            const branch_body = try self.lowerExprInto(target, branches[i].body, branch_done);
            const guarded = if (branches[i].guard) |guard| blk: {
                const guard_local = try self.addTemp(self.expr(guard).ty);
                const guard_switch = try self.boolSwitchNoContinuation(guard_local, branch_body, try self.patternMissJump(miss));
                break :blk try self.lowerExprInto(guard_local, guard, guard_switch);
            } else branch_body;
            const branch_start = try self.lowerPatternThen(branches[i].pat, scrutinee, guarded, miss, branch_done);
            current = if (miss) |miss_info|
                try self.result.store.addCFStmt(.{ .join = .{
                    .id = miss_info.join_id,
                    .params = LIR.LocalSpan.empty(),
                    .body = next_branch,
                    .remainder = branch_start,
                } })
            else
                branch_start;
        }
        return current;
    }

    fn joinJump(self: *Lowerer, join_id: LIR.JoinPointId) Common.LowerError!LIR.CFStmtId {
        return try self.result.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    }

    fn patternMissJump(self: *Lowerer, miss: ?PatternMiss) Common.LowerError!LIR.CFStmtId {
        const miss_info = miss orelse Common.invariant("refutable pattern lowering had no miss target");
        return try self.result.store.addCFStmt(.{ .jump = .{ .target = miss_info.join_id } });
    }

    fn patternCanMiss(self: *Lowerer, pat_id: LambdaMono.PatId) bool {
        const pat_data = self.pat(pat_id);
        return switch (pat_data.data) {
            .bind, .wildcard => false,
            .as => |as| self.patternCanMiss(as.pattern),
            .record => |fields| blk: {
                for (self.program.recordDestructSpan(fields)) |field| {
                    if (self.patternCanMiss(field.pattern)) break :blk true;
                }
                break :blk false;
            },
            .tuple => |items| blk: {
                for (self.program.patSpan(items)) |item| {
                    if (self.patternCanMiss(item)) break :blk true;
                }
                break :blk false;
            },
            .nominal => |inner| self.patternCanMiss(inner),
            .tag, .callable, .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit => true,
        };
    }

    fn lowerPatternThen(self: *Lowerer, pat_id: LambdaMono.PatId, source: LIR.LocalId, on_match: LIR.CFStmtId, miss: ?PatternMiss, continuation: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const pat_data = self.pat(pat_id);
        return switch (pat_data.data) {
            .bind, .wildcard => try self.bindPattern(pat_id, source, on_match),
            .as => |as| blk: {
                const tested = try self.lowerPatternThen(as.pattern, source, on_match, miss, continuation);
                break :blk try self.assignLocal(try self.localFor(as.local), source, tested);
            },
            .record => |fields| try self.lowerRecordPatternThen(pat_data.ty, fields, source, on_match, miss, continuation),
            .tuple => |items| try self.lowerTuplePatternThen(items, source, on_match, miss, continuation),
            .nominal => |inner| try self.lowerPatternThen(inner, source, on_match, miss, continuation),
            .tag => |tag| try self.lowerTagPatternThen(pat_data.ty, tag.name, tag.payloads, source, on_match, miss, continuation),
            .callable => |callable| try self.lowerCallablePatternThen(pat_data.ty, callable.variant, callable.payload, source, on_match, miss, continuation),
            .int_lit => |value| try self.lowerLiteralPatternThen(source, pat_data.ty, .{ .int_lit = value }, on_match, miss),
            .dec_lit => |value| try self.lowerLiteralPatternThen(source, pat_data.ty, .{ .dec_lit = value }, on_match, miss),
            .frac_f32_lit => |value| try self.lowerLiteralPatternThen(source, pat_data.ty, .{ .frac_f32_lit = value }, on_match, miss),
            .frac_f64_lit => |value| try self.lowerLiteralPatternThen(source, pat_data.ty, .{ .frac_f64_lit = value }, on_match, miss),
            .str_lit => |value| try self.lowerLiteralPatternThen(source, pat_data.ty, .{ .str_lit = value }, on_match, miss),
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
        miss: ?PatternMiss,
    ) Common.LowerError!LIR.CFStmtId {
        const lit_local = try self.addTemp(ty);
        const eq_local = try self.addLocalForLayout(.bool);
        const switch_stmt = try self.boolSwitchNoContinuation(eq_local, on_match, try self.patternMissJump(miss));
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
            .str_lit => |value| blk: {
                const str_lit = self.program.stringLiteral(value);
                break :blk try self.result.store.addCFStmt(.{ .assign_literal = .{
                    .target = target,
                    .value = .{ .str_literal = try self.result.store.insertStringView(str_lit.backing, str_lit.offset, str_lit.len) },
                    .next = next,
                } });
            },
        };
    }

    fn lowerTagPatternThen(
        self: *Lowerer,
        ty: Type.TypeId,
        name: Type.names.TagNameId,
        payloads: LambdaMono.Span(LambdaMono.PatId),
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
        continuation: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const variant_index = self.tagIndex(ty, name);
        const bind_payloads = try self.matchTagPayloadPatterns(variant_index, payloads, source, on_match, miss, continuation);
        return try self.discriminantSwitch(source, variant_index, bind_payloads, try self.patternMissJump(miss));
    }

    fn lowerCallablePatternThen(
        self: *Lowerer,
        ty: Type.TypeId,
        variant: Type.FnVariantId,
        payload: ?LambdaMono.PatId,
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
        continuation: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const variant_index = self.callableVariantIndex(ty, variant);
        const bind_payload = if (payload) |payload_pat| blk: {
            const payload_local = try self.addLocalForLayout(self.tagUnionPayloadLayout(self.result.store.getLocal(source).layout_idx, variant_index));
            const bound = try self.lowerPatternThen(payload_pat, payload_local, on_match, miss, continuation);
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
        return try self.discriminantSwitch(source, variant_index, bind_payload, try self.patternMissJump(miss));
    }

    fn lowerRecordPatternThen(
        self: *Lowerer,
        ty: Type.TypeId,
        span: LambdaMono.Span(LambdaMono.RecordDestruct),
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
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
            current = try self.lowerPatternThen(destructs[i].pattern, field_local, current, miss, continuation);
            if (!self.isZstLocal(field_local)) {
                current = try self.assignRefRead(
                    field_local,
                    self.localFieldLayout(source, field_index),
                    .{ .field = .{ .source = source, .field_idx = field_index } },
                    current,
                );
            }
        }
        return current;
    }

    fn lowerTuplePatternThen(
        self: *Lowerer,
        span: LambdaMono.Span(LambdaMono.PatId),
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
        continuation: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        var current = on_match;
        const items = self.program.patSpan(span);
        var i = items.len;
        while (i > 0) {
            i -= 1;
            const item_pat = self.pat(items[i]);
            const item_local = try self.addTemp(item_pat.ty);
            current = try self.lowerPatternThen(items[i], item_local, current, miss, continuation);
            if (!self.isZstLocal(item_local)) {
                const field_index: u16 = @intCast(i);
                current = try self.assignRefRead(
                    item_local,
                    self.localFieldLayout(source, field_index),
                    .{ .field = .{ .source = source, .field_idx = field_index } },
                    current,
                );
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

    fn bindPatternOrCrash(self: *Lowerer, pat_id: LambdaMono.PatId, source: LIR.LocalId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        if (!self.patternCanMiss(pat_id)) return try self.bindPattern(pat_id, source, next);

        const miss = PatternMiss{ .join_id = self.freshJoinPointId() };
        const crash = try self.result.store.addCFStmt(.{ .runtime_error = {} });
        const matched = try self.lowerPatternThen(pat_id, source, next, miss, next);
        return try self.result.store.addCFStmt(.{ .join = .{
            .id = miss.join_id,
            .params = LIR.LocalSpan.empty(),
            .body = crash,
            .remainder = matched,
        } });
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
                current = try self.assignRefRead(
                    field_local,
                    self.localFieldLayout(source, field_index),
                    .{ .field = .{ .source = source, .field_idx = field_index } },
                    current,
                );
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
        miss: ?PatternMiss,
        continuation: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        var current = on_match;
        const payloads = self.program.patSpan(payload_span);
        var i = payloads.len;
        while (i > 0) {
            i -= 1;
            const payload_pat = self.pat(payloads[i]);
            const payload_local = try self.addTemp(payload_pat.ty);
            current = try self.lowerPatternThen(payloads[i], payload_local, current, miss, continuation);
            if (!self.isZstLocal(payload_local)) {
                if (payloads.len == 1) {
                    current = try self.assignRefRead(
                        payload_local,
                        self.localTagPayloadLayout(source, variant_index, null),
                        .{ .tag_payload_struct = .{
                            .source = source,
                            .variant_index = variant_index,
                            .tag_discriminant = variant_index,
                        } },
                        current,
                    );
                    continue;
                }
                const payload_index: u16 = @intCast(i);
                current = try self.assignRefRead(
                    payload_local,
                    self.localTagPayloadLayout(source, variant_index, payload_index),
                    .{ .tag_payload = .{
                        .source = source,
                        .payload_idx = payload_index,
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } },
                    current,
                );
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
                const field_index: u16 = @intCast(i);
                current = try self.assignRefRead(
                    item_local,
                    self.localFieldLayout(source, field_index),
                    .{ .field = .{ .source = source, .field_idx = field_index } },
                    current,
                );
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
                if (payloads.len == 1) {
                    current = try self.assignRefRead(
                        payload_local,
                        self.localTagPayloadLayout(source, variant_index, null),
                        .{ .tag_payload_struct = .{
                            .source = source,
                            .variant_index = variant_index,
                            .tag_discriminant = variant_index,
                        } },
                        current,
                    );
                    continue;
                }
                const payload_index: u16 = @intCast(i);
                current = try self.assignRefRead(
                    payload_local,
                    self.localTagPayloadLayout(source, variant_index, payload_index),
                    .{ .tag_payload = .{
                        .source = source,
                        .payload_idx = payload_index,
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } },
                    current,
                );
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
            .list, .box, .callable, .erased_fn, .erased_capture_ptr => Common.invariant("non-structural equality reached direct LIR structural equality lowering"),
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
            current = try self.lowerFieldEqStep(lhs, rhs, fields[i].ty, @intCast(i), current, failed);
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
            current = try self.lowerFieldEqStep(lhs, rhs, fields[i].ty, @intCast(i), current, failed);
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
            current = try self.lowerFieldEqStep(lhs, rhs, items[i], @intCast(i), current, failed);
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
    ) Common.LowerError!LIR.CFStmtId {
        const lhs_field = try self.addTemp(field_ty);
        const rhs_field = try self.addTemp(field_ty);
        const eq = try self.addLocalForLayout(.bool);
        var current = try self.boolSwitchNoContinuation(eq, on_equal, on_not_equal);
        current = try self.lowerEqLocalsInto(eq, lhs_field, rhs_field, field_ty, false, current);
        if (!self.isZstLocal(rhs_field)) {
            current = try self.assignRefRead(
                rhs_field,
                self.localFieldLayout(rhs, field_index),
                .{ .field = .{ .source = rhs, .field_idx = field_index } },
                current,
            );
        }
        if (!self.isZstLocal(lhs_field)) {
            current = try self.assignRefRead(
                lhs_field,
                self.localFieldLayout(lhs, field_index),
                .{ .field = .{ .source = lhs, .field_idx = field_index } },
                current,
            );
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
                .body = try self.lowerTagPayloadEqVariant(lhs, rhs, tag, @intCast(index), success, failed),
            };
        }

        const payload_switch = try self.result.store.addCFStmt(.{ .switch_stmt = .{
            .cond = lhs_disc,
            .branches = try self.result.store.addCFSwitchBranches(branches),
            .default_branch = failed,
            .continuation = null,
        } });
        const disc_switch = try self.boolSwitchNoContinuation(same_disc, payload_switch, failed);
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
            current = try self.boolSwitchNoContinuation(eq, current, on_not_equal);
            current = try self.lowerEqLocalsInto(eq, lhs_payload, rhs_payload, payload_ty, false, current);
            const payload_idx: ?u16 = if (payloads.len == 1) null else @as(u16, @intCast(i));
            if (!self.isZstLocal(rhs_payload)) {
                current = try self.assignRefRead(
                    rhs_payload,
                    self.localTagPayloadLayout(rhs, variant_index, payload_idx),
                    if (payload_idx) |index| .{ .tag_payload = .{
                        .source = rhs,
                        .payload_idx = index,
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } } else .{ .tag_payload_struct = .{
                        .source = rhs,
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } },
                    current,
                );
            }
            if (!self.isZstLocal(lhs_payload)) {
                current = try self.assignRefRead(
                    lhs_payload,
                    self.localTagPayloadLayout(lhs, variant_index, payload_idx),
                    if (payload_idx) |index| .{ .tag_payload = .{
                        .source = lhs,
                        .payload_idx = index,
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } } else .{ .tag_payload_struct = .{
                        .source = lhs,
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } },
                    current,
                );
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

    fn boolSwitchNoContinuation(self: *Lowerer, cond: LIR.LocalId, true_body: LIR.CFStmtId, false_body: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const branches = [_]LIR.CFSwitchBranch{.{ .value = 1, .body = true_body }};
        return try self.result.store.addCFStmt(.{ .switch_stmt = .{
            .cond = cond,
            .branches = try self.result.store.addCFSwitchBranches(&branches),
            .default_branch = false_body,
            .continuation = null,
        } });
    }

    fn discriminantSwitch(self: *Lowerer, source: LIR.LocalId, discriminant: u16, body: LIR.CFStmtId, default: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        if (self.isZstLocal(source)) return body;
        const disc_local = try self.addLocalForLayout(.u16);
        const branches = [_]LIR.CFSwitchBranch{.{ .value = discriminant, .body = body }};
        const switch_stmt = try self.result.store.addCFStmt(.{ .switch_stmt = .{
            .cond = disc_local,
            .branches = try self.result.store.addCFSwitchBranches(&branches),
            .default_branch = default,
            .continuation = null,
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

    fn prependJoinParamInitializers(
        self: *Lowerer,
        params: LIR.LocalSpan,
        args: []const LIR.LocalId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const param_locals = self.result.store.getLocalSpan(params);
        if (param_locals.len != args.len) {
            Common.invariant("LIR join parameter initializer arity differed from jump argument arity");
        }

        var current = next;
        var i = args.len;
        while (i > 0) {
            i -= 1;
            current = try self.result.store.addCFStmt(.{ .set_local = .{
                .target = param_locals[i],
                .value = args[i],
                .mode = .initialize_join_param,
                .next = current,
            } });
        }
        return current;
    }

    fn addTemp(self: *Lowerer, ty: Type.TypeId) Common.LowerError!LIR.LocalId {
        return try self.addLocalForLayout(try self.layoutOfType(ty));
    }

    fn addLocalForLayout(self: *Lowerer, layout_idx: layout.Idx) Common.LowerError!LIR.LocalId {
        const local = try self.result.store.addLocal(.{ .layout_idx = layout_idx });
        try self.noteLocal(local);
        return local;
    }

    fn localFor(self: *Lowerer, local: LambdaMono.LocalId) Common.LowerError!LIR.LocalId {
        const index = @intFromEnum(local);
        if (self.local_map[index]) |existing| {
            try self.noteLocal(existing);
            return existing;
        }
        const program_local = self.program.locals.items[index];
        const lir_local = try self.addTemp(program_local.ty);
        try self.result.store.setLocalName(lir_local, self.program.localName(local));
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

    fn knownLayoutForType(self: *Lowerer, ty: Type.TypeId) ?layout.Idx {
        const index = @intFromEnum(ty);
        if (self.type_layouts[index]) |existing| {
            return existing;
        }
        return null;
    }

    fn rememberLayoutForType(self: *Lowerer, ty: Type.TypeId, layout_idx: layout.Idx) Common.LowerError!void {
        const index = @intFromEnum(ty);
        self.type_layouts[index] = layout_idx;
    }

    fn layoutOfType(self: *Lowerer, ty: Type.TypeId) Common.LowerError!layout.Idx {
        if (self.knownLayoutForType(ty)) |existing| return existing;

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
            try self.rememberLayoutForType(ty, layout_idx);
            return layout_idx;
        }

        const node = layout.graphInputLocal(root) orelse Common.invariant("layout graph root was neither committed nor local");
        for (local_nodes, 0..) |maybe_node, type_index| {
            if (maybe_node) |mapped_node| {
                try self.rememberLayoutForType(@enumFromInt(type_index), commit.value_layouts[@intFromEnum(mapped_node)]);
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
            if (self.lowerer.knownLayoutForType(ty)) |layout_idx| return layout.committedGraphInput(layout_idx);
            if (self.local_nodes[index]) |node| return layout.localGraphInput(node);

            switch (self.lowerer.program.types.get(ty)) {
                .primitive => |primitive| return layout.committedGraphInput(primitiveLayout(primitive)),
                .zst => return layout.committedGraphInput(.zst),
                .erased_capture_ptr => return layout.committedGraphInput(.opaque_ptr),
                .named => |named| if (named.builtin_owner) |owner| {
                    if (builtinOwnerLayout(owner)) |layout_idx| return layout.committedGraphInput(layout_idx);
                },
                else => {},
            }

            switch (self.lowerer.program.types.get(ty)) {
                .named => |named| if (named.backing) |backing| {
                    if (self.lowerer.knownLayoutForType(backing.ty)) |layout_idx| return layout.committedGraphInput(layout_idx);

                    const backing_index = @intFromEnum(backing.ty);
                    if (self.local_nodes[backing_index]) |node| {
                        self.local_nodes[index] = node;
                        return layout.localGraphInput(node);
                    }

                    const node = try self.graph.reserveNode(self.lowerer.allocator);
                    self.local_nodes[index] = node;
                    self.local_nodes[backing_index] = node;
                    self.graph.setNode(node, try self.nodeForType(backing.ty));
                    return layout.localGraphInput(node);
                },
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
                .box => |elem| if (self.isErasedCallableValueType(elem))
                    .erased_callable
                else
                    .{ .box = try self.inputForType(elem) },
                .erased_fn => .erased_callable,
                .erased_capture_ptr => unreachable,
            };
        }

        fn isErasedCallableValueType(self: *LayoutGraphBuilder, ty: Type.TypeId) bool {
            var current = ty;
            var depth: u8 = 0;
            while (true) {
                if (depth == 32) Common.invariant("transparent alias chain exceeded layout lowering limit");
                depth += 1;
                switch (self.lowerer.program.types.get(current)) {
                    .erased_fn => return true,
                    .named => |named| {
                        if (named.kind != .alias) return false;
                        const backing = named.backing orelse Common.invariant("transparent alias reached layout lowering without a backing type");
                        current = backing.ty;
                    },
                    else => return false,
                }
            }
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

    fn builtinOwnerLayout(owner: check.StaticDispatchRegistry.BuiltinOwner) ?layout.Idx {
        return switch (owner) {
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
            .list, .box => null,
        };
    }

    fn tagIndex(self: *Lowerer, ty: Type.TypeId, name: Type.names.TagNameId) u16 {
        for (self.tagUnionTags(ty), 0..) |tag, index| {
            if (tag.name == name) return @intCast(index);
        }
        Common.invariant("tag operation referenced tag absent from Lambda Mono type");
    }

    fn tagUnionTags(self: *Lowerer, ty: Type.TypeId) []const Type.Tag {
        return switch (self.program.types.get(ty)) {
            .tag_union => |tags| self.program.types.tagSpan(tags),
            .named => |named| if (named.backing) |backing| return self.tagUnionTags(backing.ty) else Common.invariant("named tag has no backing"),
            else => Common.invariant("tag operation expected tag-union type"),
        };
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
                const data = self.result.layouts.getTagUnionData(tag_union_layout.getTagUnion().idx);
                const variants = self.result.layouts.getTagUnionVariants(data);
                if (variant_index >= variants.len) Common.invariant("tag payload variant exceeded committed tag-union layout");
                break :blk variants.get(@intCast(variant_index)).payload_layout;
            },
            .zst, .scalar => .zst,
            else => Common.invariant("tag payload operation expected tag-union layout"),
        };
    }

    fn localFieldLayout(self: *Lowerer, source: LIR.LocalId, field_index: u16) layout.Idx {
        const source_layout_idx = self.result.store.getLocal(source).layout_idx;
        const source_layout = self.result.layouts.getLayout(source_layout_idx);
        const struct_layout_idx = switch (source_layout.tag) {
            .box => source_layout.getIdx(),
            else => source_layout_idx,
        };
        const struct_layout = self.result.layouts.getLayout(struct_layout_idx);
        if (struct_layout.tag != .struct_) {
            Common.invariant("field read expected a struct layout");
        }
        return self.result.layouts.getStructFieldLayoutByOriginalIndex(struct_layout.getStruct().idx, field_index);
    }

    fn localListElemLayout(self: *Lowerer, source: LIR.LocalId) layout.Idx {
        const list_layout_idx = self.result.store.getLocal(source).layout_idx;
        const list_layout = self.result.layouts.getLayout(list_layout_idx);
        return switch (list_layout.tag) {
            .list => list_layout.getIdx(),
            .list_of_zst => .zst,
            else => Common.invariant("list expression target was not a list layout"),
        };
    }

    fn localTagPayloadLayout(self: *Lowerer, source: LIR.LocalId, variant_index: u16, payload_idx: ?u16) layout.Idx {
        const payload_layout_idx = self.tagUnionPayloadLayout(self.result.store.getLocal(source).layout_idx, variant_index);
        const index = payload_idx orelse return payload_layout_idx;
        const payload_layout = self.result.layouts.getLayout(payload_layout_idx);
        return switch (payload_layout.tag) {
            .struct_ => self.result.layouts.getStructFieldLayoutByOriginalIndex(payload_layout.getStruct().idx, index),
            else => blk: {
                if (index != 0) Common.invariant("tag payload field read indexed a non-struct payload");
                break :blk payload_layout_idx;
            },
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
        .source_fn_key = template.source_fn_key,
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
        .local_hosted => |hosted| .{ .local_hosted = hosted.template },
        .imported_hosted => |hosted| .{ .imported_hosted = hosted.template },
        .checked_generated => |template| .{ .checked_generated = template },
    };
}

test "direct LIR lower declarations are referenced" {
    std.testing.refAllDecls(@This());
}
