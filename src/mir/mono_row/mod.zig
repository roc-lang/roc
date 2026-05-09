//! Row-finalized mono MIR shape store.
//!
//! This module interns record and tag-union row shapes immediately after mono
//! MIR lowering. Later stages should consume these ids instead of performing
//! field-name, tag-name, payload-position, or display-name lookup.

const std = @import("std");
const check = @import("check");
const symbol_mod = @import("symbol");
const Mono = @import("../mono/mod.zig");
const ConcreteSourceType = @import("../concrete_source_type.zig");
pub const Ast = @import("ast.zig");
const ids = @import("../ids.zig");
const verify = @import("../debug_verify.zig");

const Allocator = std.mem.Allocator;
const canonical = check.CanonicalNames;
const MonoType = Mono.Type;
const TypeId = MonoType.TypeId;

pub const RecordShapeId = ids.RecordShapeId;
pub const RecordFieldId = ids.RecordFieldId;
pub const TagUnionShapeId = ids.TagUnionShapeId;
pub const TagId = ids.TagId;
pub const TagPayloadId = ids.TagPayloadId;

/// Public `RecordField` declaration.
pub const RecordField = struct {
    label: canonical.RecordFieldLabelId,
    logical_index: u32,
};

/// Public `RecordShape` declaration.
pub const RecordShape = struct {
    fields: ids.Span(RecordFieldId),
};

/// Public `TagPayload` declaration.
pub const TagPayload = struct {
    tag: TagId,
    logical_index: u32,
};

/// Public `Tag` declaration.
pub const Tag = struct {
    label: canonical.TagLabelId,
    logical_index: u32,
    payloads: ids.Span(TagPayloadId),
};

/// Public `TagUnionShape` declaration.
pub const TagUnionShape = struct {
    tags: ids.Span(TagId),
};

/// Public `Store` declaration.
pub const Store = struct {
    allocator: Allocator,
    record_shapes: std.ArrayList(RecordShape),
    record_fields: std.ArrayList(RecordField),
    record_shape_fields: std.ArrayList(RecordFieldId),
    tag_union_shapes: std.ArrayList(TagUnionShape),
    tags: std.ArrayList(Tag),
    tag_union_tags: std.ArrayList(TagId),
    tag_payloads: std.ArrayList(TagPayload),
    tag_payload_ids: std.ArrayList(TagPayloadId),
    record_shape_by_key: std.StringHashMap(RecordShapeId),
    tag_union_shape_by_key: std.StringHashMap(TagUnionShapeId),
    scratch_key: std.ArrayList(u8),

    pub fn init(allocator: Allocator) Store {
        return .{
            .allocator = allocator,
            .record_shapes = .empty,
            .record_fields = .empty,
            .record_shape_fields = .empty,
            .tag_union_shapes = .empty,
            .tags = .empty,
            .tag_union_tags = .empty,
            .tag_payloads = .empty,
            .tag_payload_ids = .empty,
            .record_shape_by_key = std.StringHashMap(RecordShapeId).init(allocator),
            .tag_union_shape_by_key = std.StringHashMap(TagUnionShapeId).init(allocator),
            .scratch_key = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        freeStringHashMapKeys(&self.record_shape_by_key, self.allocator);
        self.record_shape_by_key.deinit();
        freeStringHashMapKeysTag(&self.tag_union_shape_by_key, self.allocator);
        self.tag_union_shape_by_key.deinit();
        self.scratch_key.deinit(self.allocator);
        self.tag_payload_ids.deinit(self.allocator);
        self.tag_payloads.deinit(self.allocator);
        self.tag_union_tags.deinit(self.allocator);
        self.tags.deinit(self.allocator);
        self.tag_union_shapes.deinit(self.allocator);
        self.record_shape_fields.deinit(self.allocator);
        self.record_fields.deinit(self.allocator);
        self.record_shapes.deinit(self.allocator);
        self.* = Store.init(self.allocator);
    }

    pub fn internRecordShapeFromType(self: *Store, types: *const MonoType.Store, ty: TypeId) Allocator.Error!RecordShapeId {
        return switch (types.getType(ty)) {
            .record => |record| try self.internRecordShape(record.fields),
            else => {
                verify.invariant(false, "mono_row expected record type for row finalization");
                unreachable;
            },
        };
    }

    pub fn internTagUnionShapeFromType(self: *Store, types: *const MonoType.Store, ty: TypeId) Allocator.Error!TagUnionShapeId {
        return switch (types.getType(ty)) {
            .tag_union => |tag_union| try self.internTagUnionShape(tag_union.tags),
            else => {
                verify.invariant(false, "mono_row expected tag-union type for row finalization");
                unreachable;
            },
        };
    }

    pub const TagShapeDescriptor = struct {
        name: canonical.TagLabelId,
        payload_arity: u32,
    };

    pub fn internRecordShape(self: *Store, fields: MonoType.Fields) Allocator.Error!RecordShapeId {
        try self.buildRecordKeyFromMonoFields(fields);
        if (self.record_shape_by_key.get(self.scratch_key.items)) |shape_id| return shape_id;

        const shape_id: RecordShapeId = @enumFromInt(@as(u32, @intCast(self.record_shapes.items.len)));
        const key = try self.allocator.dupe(u8, self.scratch_key.items);
        errdefer self.allocator.free(key);

        const start: u32 = @intCast(self.record_shape_fields.items.len);
        for (fields, 0..) |field, i| {
            const field_id: RecordFieldId = @enumFromInt(@as(u32, @intCast(self.record_fields.items.len)));
            try self.record_fields.append(self.allocator, .{
                .label = field.name,
                .logical_index = @intCast(i),
            });
            try self.record_shape_fields.append(self.allocator, field_id);
        }

        try self.record_shapes.append(self.allocator, .{
            .fields = .{ .start = start, .len = @intCast(fields.len) },
        });
        try self.record_shape_by_key.put(key, shape_id);
        return shape_id;
    }

    pub fn internRecordShapeFromLabels(self: *Store, labels: []const canonical.RecordFieldLabelId) Allocator.Error!RecordShapeId {
        try self.buildRecordKeyFromLabels(labels);
        if (self.record_shape_by_key.get(self.scratch_key.items)) |shape_id| return shape_id;

        const shape_id: RecordShapeId = @enumFromInt(@as(u32, @intCast(self.record_shapes.items.len)));
        const key = try self.allocator.dupe(u8, self.scratch_key.items);
        errdefer self.allocator.free(key);

        const start: u32 = @intCast(self.record_shape_fields.items.len);
        for (labels, 0..) |label, i| {
            const field_id: RecordFieldId = @enumFromInt(@as(u32, @intCast(self.record_fields.items.len)));
            try self.record_fields.append(self.allocator, .{
                .label = label,
                .logical_index = @intCast(i),
            });
            try self.record_shape_fields.append(self.allocator, field_id);
        }

        try self.record_shapes.append(self.allocator, .{
            .fields = .{ .start = start, .len = @intCast(labels.len) },
        });
        try self.record_shape_by_key.put(key, shape_id);
        return shape_id;
    }

    pub fn internTagUnionShape(self: *Store, source_tags: MonoType.Tags) Allocator.Error!TagUnionShapeId {
        try self.buildTagUnionKeyFromMonoTags(source_tags);
        if (self.tag_union_shape_by_key.get(self.scratch_key.items)) |shape_id| return shape_id;

        const shape_id: TagUnionShapeId = @enumFromInt(@as(u32, @intCast(self.tag_union_shapes.items.len)));
        const key = try self.allocator.dupe(u8, self.scratch_key.items);
        errdefer self.allocator.free(key);

        const tag_start: u32 = @intCast(self.tag_union_tags.items.len);
        for (source_tags, 0..) |source_tag, tag_i| {
            const payload_start: u32 = @intCast(self.tag_payload_ids.items.len);
            const tag_id: TagId = @enumFromInt(@as(u32, @intCast(self.tags.items.len)));
            for (0..source_tag.args.len) |payload_i| {
                const payload_id: TagPayloadId = @enumFromInt(@as(u32, @intCast(self.tag_payloads.items.len)));
                try self.tag_payloads.append(self.allocator, .{
                    .tag = tag_id,
                    .logical_index = @intCast(payload_i),
                });
                try self.tag_payload_ids.append(self.allocator, payload_id);
            }

            try self.tags.append(self.allocator, .{
                .label = source_tag.name,
                .logical_index = @intCast(tag_i),
                .payloads = .{ .start = payload_start, .len = @intCast(source_tag.args.len) },
            });
            try self.tag_union_tags.append(self.allocator, tag_id);
        }

        try self.tag_union_shapes.append(self.allocator, .{
            .tags = .{ .start = tag_start, .len = @intCast(source_tags.len) },
        });
        try self.tag_union_shape_by_key.put(key, shape_id);
        return shape_id;
    }

    pub fn internTagUnionShapeFromDescriptors(self: *Store, source_tags: []const TagShapeDescriptor) Allocator.Error!TagUnionShapeId {
        try self.buildTagUnionKeyFromDescriptors(source_tags);
        if (self.tag_union_shape_by_key.get(self.scratch_key.items)) |shape_id| return shape_id;

        const shape_id: TagUnionShapeId = @enumFromInt(@as(u32, @intCast(self.tag_union_shapes.items.len)));
        const key = try self.allocator.dupe(u8, self.scratch_key.items);
        errdefer self.allocator.free(key);

        const tag_start: u32 = @intCast(self.tag_union_tags.items.len);
        for (source_tags, 0..) |source_tag, tag_i| {
            const payload_start: u32 = @intCast(self.tag_payload_ids.items.len);
            const tag_id: TagId = @enumFromInt(@as(u32, @intCast(self.tags.items.len)));
            for (0..source_tag.payload_arity) |payload_i| {
                const payload_id: TagPayloadId = @enumFromInt(@as(u32, @intCast(self.tag_payloads.items.len)));
                try self.tag_payloads.append(self.allocator, .{
                    .tag = tag_id,
                    .logical_index = @intCast(payload_i),
                });
                try self.tag_payload_ids.append(self.allocator, payload_id);
            }

            try self.tags.append(self.allocator, .{
                .label = source_tag.name,
                .logical_index = @intCast(tag_i),
                .payloads = .{ .start = payload_start, .len = @intCast(source_tag.payload_arity) },
            });
            try self.tag_union_tags.append(self.allocator, tag_id);
        }

        try self.tag_union_shapes.append(self.allocator, .{
            .tags = .{ .start = tag_start, .len = @intCast(source_tags.len) },
        });
        try self.tag_union_shape_by_key.put(key, shape_id);
        return shape_id;
    }

    pub fn recordShape(self: *const Store, id: RecordShapeId) RecordShape {
        return self.record_shapes.items[@intFromEnum(id)];
    }

    pub fn recordShapeFields(self: *const Store, id: RecordShapeId) []const RecordFieldId {
        return self.recordShape(id).fields.get(self.record_shape_fields.items);
    }

    pub fn recordField(self: *const Store, id: RecordFieldId) RecordField {
        return self.record_fields.items[@intFromEnum(id)];
    }

    pub fn tagUnionShape(self: *const Store, id: TagUnionShapeId) TagUnionShape {
        return self.tag_union_shapes.items[@intFromEnum(id)];
    }

    pub fn tagUnionTags(self: *const Store, id: TagUnionShapeId) []const TagId {
        return self.tagUnionShape(id).tags.get(self.tag_union_tags.items);
    }

    pub fn tag(self: *const Store, id: TagId) Tag {
        return self.tags.items[@intFromEnum(id)];
    }

    pub fn tagPayloads(self: *const Store, id: TagId) []const TagPayloadId {
        return self.tag(id).payloads.get(self.tag_payload_ids.items);
    }

    pub fn tagPayload(self: *const Store, id: TagPayloadId) TagPayload {
        return self.tag_payloads.items[@intFromEnum(id)];
    }

    fn buildRecordKeyFromMonoFields(self: *Store, fields: MonoType.Fields) Allocator.Error!void {
        self.scratch_key.clearRetainingCapacity();
        try self.scratch_key.writer(self.allocator).print("record:{d}|", .{fields.len});
        for (fields) |field| {
            try self.scratch_key.writer(self.allocator).print("{d}|", .{@intFromEnum(field.name)});
        }
    }

    fn buildRecordKeyFromLabels(self: *Store, labels: []const canonical.RecordFieldLabelId) Allocator.Error!void {
        self.scratch_key.clearRetainingCapacity();
        try self.scratch_key.writer(self.allocator).print("record:{d}|", .{labels.len});
        for (labels) |label| {
            try self.scratch_key.writer(self.allocator).print("{d}|", .{@intFromEnum(label)});
        }
    }

    fn buildTagUnionKeyFromMonoTags(self: *Store, source_tags: MonoType.Tags) Allocator.Error!void {
        self.scratch_key.clearRetainingCapacity();
        try self.scratch_key.writer(self.allocator).print("tag_union:{d}|", .{source_tags.len});
        for (source_tags) |source_tag| {
            try self.scratch_key.writer(self.allocator).print("{d}:{d}|", .{
                @intFromEnum(source_tag.name),
                source_tag.args.len,
            });
        }
    }

    fn buildTagUnionKeyFromDescriptors(self: *Store, source_tags: []const TagShapeDescriptor) Allocator.Error!void {
        self.scratch_key.clearRetainingCapacity();
        try self.scratch_key.writer(self.allocator).print("tag_union:{d}|", .{source_tags.len});
        for (source_tags) |source_tag| {
            try self.scratch_key.writer(self.allocator).print("{d}:{d}|", .{
                @intFromEnum(source_tag.name),
                source_tag.payload_arity,
            });
        }
    }
};

/// Public `Proc` declaration.
pub const Proc = struct {
    key: canonical.MonoSpecializationKey,
    proc: canonical.MirProcedureRef,
    local_handle: Mono.Specialize.MonoProcHandle,
    fn_ty: TypeId,
    body: Ast.DefId,
};

/// Public `Program` declaration.
pub const Program = struct {
    allocator: Allocator,
    canonical_names: canonical.CanonicalNameStore,
    concrete_source_types: ConcreteSourceType.Store,
    literal_pool: ids.ProgramLiteralPool,
    symbols: symbol_mod.Store,
    types: Mono.Type.Store,
    ast: Ast.Store,
    procs: std.ArrayList(Proc),
    executable_synthetic_procs: std.ArrayList(ids.ExecutableSyntheticProc),
    root_procs: std.ArrayList(canonical.MirProcedureRef),
    root_metadata: std.ArrayList(ids.RootMetadata),

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .canonical_names = canonical.CanonicalNameStore.init(allocator),
            .concrete_source_types = ConcreteSourceType.Store.init(allocator),
            .literal_pool = ids.ProgramLiteralPool.init(allocator),
            .symbols = symbol_mod.Store.init(allocator),
            .types = Mono.Type.Store.init(allocator),
            .ast = Ast.Store.init(allocator),
            .procs = .empty,
            .executable_synthetic_procs = .empty,
            .root_procs = .empty,
            .root_metadata = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        self.root_metadata.deinit(self.allocator);
        self.root_procs.deinit(self.allocator);
        self.executable_synthetic_procs.deinit(self.allocator);
        self.procs.deinit(self.allocator);
        self.ast.deinit();
        self.types.deinit();
        self.symbols.deinit();
        self.literal_pool.deinit();
        self.concrete_source_types.deinit();
        self.canonical_names.deinit();
        self.* = Program.init(self.allocator);
    }
};

/// Public `Result` declaration.
pub const Result = struct {
    program: Program,
    shapes: Store,

    pub fn deinit(self: *Result) void {
        self.program.deinit();
        self.shapes.deinit();
    }
};

/// Public `run` function.
pub fn run(allocator: Allocator, mono: Mono.Specialize.Program) Allocator.Error!Result {
    var owned_mono = mono;
    errdefer owned_mono.deinit();
    var shapes = Store.init(allocator);
    errdefer shapes.deinit();

    for (owned_mono.types.types.items, 0..) |_, raw_i| {
        const ty: TypeId = @enumFromInt(@as(u32, @intCast(raw_i)));
        switch (owned_mono.types.getType(ty)) {
            .record => |record| _ = try shapes.internRecordShape(record.fields),
            .tag_union => |tag_union| _ = try shapes.internTagUnionShape(tag_union.tags),
            else => {},
        }
    }

    var program = Program.init(allocator);
    errdefer program.deinit();
    program.canonical_names = owned_mono.canonical_names;
    owned_mono.canonical_names = canonical.CanonicalNameStore.init(allocator);
    program.concrete_source_types = owned_mono.concrete_source_types;
    owned_mono.concrete_source_types = ConcreteSourceType.Store.init(allocator);
    program.types = owned_mono.types;
    owned_mono.types = Mono.Type.Store.init(allocator);
    program.literal_pool = owned_mono.literal_pool;
    owned_mono.literal_pool = ids.ProgramLiteralPool.init(allocator);
    program.symbols = owned_mono.symbols;
    owned_mono.symbols = symbol_mod.Store.init(allocator);

    var finalizer = BodyFinalizer{
        .allocator = allocator,
        .input = &owned_mono.ast,
        .output = &program.ast,
        .types = &program.types,
        .shapes = &shapes,
    };

    try program.procs.ensureTotalCapacity(allocator, owned_mono.procs.items.len);
    for (owned_mono.procs.items) |proc| {
        program.procs.appendAssumeCapacity(.{
            .key = proc.key,
            .proc = proc.proc,
            .local_handle = proc.local_handle,
            .fn_ty = proc.fn_ty,
            .body = try finalizer.lowerDef(proc.body),
        });
    }
    try program.executable_synthetic_procs.appendSlice(allocator, owned_mono.executable_synthetic_procs.items);
    owned_mono.executable_synthetic_procs.clearRetainingCapacity();
    try program.root_procs.appendSlice(allocator, owned_mono.root_procs.items);
    try program.root_metadata.appendSlice(allocator, owned_mono.root_metadata.items);
    owned_mono.root_procs.clearRetainingCapacity();
    owned_mono.root_metadata.clearRetainingCapacity();
    owned_mono.ast.deinit();
    owned_mono.root_metadata.deinit(allocator);
    owned_mono.root_procs.deinit(allocator);
    owned_mono.executable_synthetic_procs.deinit(allocator);
    owned_mono.procs.deinit(allocator);
    var nominal_keys = owned_mono.nominal_backing_instantiations.keyIterator();
    while (nominal_keys.next()) |stored_key| allocator.free(stored_key.*);
    owned_mono.nominal_backing_instantiations.deinit();

    const result = Result{
        .program = program,
        .shapes = shapes,
    };
    if (verify.enabled()) verifyResult(&result);
    return result;
}

/// Public `verifyResult` function.
pub fn verifyResult(result: *const Result) void {
    if (!verify.enabled()) return;

    verify.assertFmt(
        result.program.root_procs.items.len == result.program.root_metadata.items.len,
        "root proc metadata mismatch: procs={d} metadata={d}",
        .{ result.program.root_procs.items.len, result.program.root_metadata.items.len },
    );

    for (result.shapes.record_shapes.items) |shape| {
        const fields = shape.fields.get(result.shapes.record_shape_fields.items);
        for (fields) |field_id| {
            verify.assertFmt(@intFromEnum(field_id) < result.shapes.record_fields.items.len, "invalid record field id {d}", .{@intFromEnum(field_id)});
        }
    }

    for (result.shapes.tag_union_shapes.items) |shape| {
        const tag_ids = shape.tags.get(result.shapes.tag_union_tags.items);
        for (tag_ids) |tag_id| {
            verify.assertFmt(@intFromEnum(tag_id) < result.shapes.tags.items.len, "invalid tag id {d}", .{@intFromEnum(tag_id)});
            const payload_ids = result.shapes.tag(tag_id).payloads.get(result.shapes.tag_payload_ids.items);
            for (payload_ids) |payload_id| {
                verify.assertFmt(@intFromEnum(payload_id) < result.shapes.tag_payloads.items.len, "invalid tag payload id {d}", .{@intFromEnum(payload_id)});
            }
        }
    }
}

const BodyFinalizer = struct {
    allocator: Allocator,
    input: *const Mono.Ast.Store,
    output: *Ast.Store,
    types: *const Mono.Type.Store,
    shapes: *Store,

    fn lowerDef(self: *BodyFinalizer, def_id: Mono.Ast.DefId) Allocator.Error!Ast.DefId {
        const def = self.input.getDef(def_id);
        return try self.output.addDef(.{
            .proc = def.proc,
            .debug_name = def.debug_name,
            .value = switch (def.value) {
                .fn_ => |fn_| .{ .fn_ = .{
                    .args = try self.lowerTypedSymbolSpan(fn_.args),
                    .captures = Ast.Span(Ast.TypedSymbol).empty(),
                    .body = try self.lowerExpr(fn_.body),
                } },
                .hosted_fn => |hosted| .{ .hosted_fn = .{
                    .proc = hosted.proc,
                    .args = try self.lowerTypedSymbolSpan(hosted.args),
                    .ret_ty = hosted.ret_ty,
                    .hosted = hosted.hosted,
                } },
                .val => |expr| .{ .val = try self.lowerExpr(expr) },
                .run => |run_def| .{ .run = .{ .body = try self.lowerExpr(run_def.body) } },
            },
        });
    }

    fn lowerExpr(self: *BodyFinalizer, expr_id: Mono.Ast.ExprId) Allocator.Error!Ast.ExprId {
        const expr = self.input.getExpr(expr_id);
        return try self.output.addExpr(expr.ty, expr.source_ty, switch (expr.data) {
            .var_ => |symbol| .{ .var_ = symbol },
            .int_lit => |value| .{ .int_lit = value },
            .frac_f32_lit => |value| .{ .frac_f32_lit = value },
            .frac_f64_lit => |value| .{ .frac_f64_lit = value },
            .dec_lit => |value| .{ .dec_lit = value },
            .bool_lit => |value| .{ .bool_lit = value },
            .str_lit => |literal| .{ .str_lit = literal },
            .const_instance => |const_instance| .{ .const_instance = const_instance },
            .const_ref => |key| .{ .const_ref = key },
            .pending_local_root => |root| .{ .pending_local_root = root },
            .structural_eq => |eq| .{ .structural_eq = .{
                .lhs = try self.lowerExpr(eq.lhs),
                .rhs = try self.lowerExpr(eq.rhs),
            } },
            .bool_not => |child| .{ .bool_not = try self.lowerExpr(child) },
            .clos => |clos| .{ .clos = .{
                .site = clos.site,
                .source_fn_ty = clos.source_fn_ty,
                .args = try self.lowerTypedSymbolSpan(clos.args),
                .body = try self.lowerExpr(clos.body),
            } },
            .call_value => |call| .{ .call_value = .{
                .func = try self.lowerExpr(call.func),
                .args = try self.lowerExprSpan(call.args),
                .requested_fn_ty = call.requested_fn_ty,
                .requested_source_fn_ty = call.requested_source_fn_ty,
            } },
            .call_proc => |call| .{ .call_proc = .{
                .proc = call.proc,
                .args = try self.lowerExprSpan(call.args),
                .requested_fn_ty = call.requested_fn_ty,
                .requested_source_fn_ty = call.requested_source_fn_ty,
            } },
            .proc_value => |proc_value| .{ .proc_value = .{
                .proc = proc_value.proc,
                .published_proc = proc_value.published_proc,
                .captures = try self.lowerCaptureArgSpan(proc_value.captures),
                .fn_ty = proc_value.fn_ty,
                .forced_target = try ids.cloneProcValueExecutableTargetOptional(self.allocator, proc_value.forced_target),
            } },
            .low_level => |low_level| .{ .low_level = .{
                .op = low_level.op,
                .rc_effect = low_level.rc_effect,
                .args = try self.lowerExprSpan(low_level.args),
                .source_constraint_ty = low_level.source_constraint_ty,
            } },
            .block => |block| .{ .block = .{
                .stmts = try self.lowerStmtSpan(block.stmts),
                .final_expr = try self.lowerExpr(block.final_expr),
            } },
            .tuple => |items| .{ .tuple = try self.lowerExprSpan(items) },
            .list => |items| .{ .list = try self.lowerExprSpan(items) },
            .unit => .unit,
            .return_ => |child| .{ .return_ = try self.lowerExpr(child) },
            .crash => |literal| .{ .crash = literal },
            .runtime_error => .runtime_error,
            .record => |fields| try self.lowerRecord(expr.ty, fields),
            .nominal_reinterpret => |backing| .{ .nominal_reinterpret = try self.lowerExpr(backing) },
            .access => |access| try self.lowerAccess(access),
            .tag => |tag| try self.lowerTag(expr.ty, tag),
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
            .tag_payload => |payload| try self.lowerTagPayload(payload),
            .tuple_access => |access| .{ .tuple_access = .{
                .tuple = try self.lowerExpr(access.tuple),
                .elem_index = access.elem_index,
            } },
            .for_ => |for_| .{ .for_ = .{
                .patt = try self.lowerPat(for_.patt),
                .iterable = try self.lowerExpr(for_.iterable),
                .body = try self.lowerExpr(for_.body),
            } },
            .let_ => |let_| try self.lowerLet(let_),
        });
    }

    fn lowerLet(self: *BodyFinalizer, let_: anytype) Allocator.Error!Ast.Expr.Data {
        return switch (let_.def) {
            .let_val => |let_val| .{ .let_ = .{
                .bind = .{
                    .ty = let_val.bind.ty,
                    .source_ty = let_val.bind.source_ty,
                    .symbol = let_val.bind.symbol,
                },
                .body = try self.lowerExpr(let_val.body),
                .rest = try self.lowerExpr(let_.rest),
            } },
            .let_fn => |let_fn| blk: {
                const stmt = try self.output.addStmt(.{ .local_fn = try self.lowerLetFn(let_fn) });
                const stmts = try self.output.addStmtSpan(&.{stmt});
                break :blk .{ .block = .{
                    .stmts = stmts,
                    .final_expr = try self.lowerExpr(let_.rest),
                } };
            },
        };
    }

    fn lowerLetFn(self: *BodyFinalizer, let_fn: Mono.Ast.LetFn) Allocator.Error!Ast.LetFn {
        return .{
            .site = let_fn.site orelse rowInvariant("row finalization received local function without a nested procedure site"),
            .source_fn_ty = let_fn.source_fn_ty,
            .recursive = let_fn.recursive,
            .bind = self.lowerTypedSymbol(let_fn.bind),
            .args = try self.lowerTypedSymbolSpan(let_fn.args),
            .body = try self.lowerExpr(let_fn.body),
        };
    }

    fn lowerRecord(
        self: *BodyFinalizer,
        record_ty: TypeId,
        fields: Mono.Ast.Span(Mono.Ast.FieldExpr),
    ) Allocator.Error!Ast.Expr.Data {
        const shape = try self.shapes.internRecordShapeFromType(self.types, record_ty);
        const mono_fields = self.input.sliceFieldExprSpan(fields);
        const evals = try self.allocator.alloc(Ast.RecordFieldEval, mono_fields.len);
        defer self.allocator.free(evals);
        const assemblies = try self.allocator.alloc(Ast.RecordFieldAssembly, mono_fields.len);
        defer self.allocator.free(assemblies);

        for (mono_fields, 0..) |field, i| {
            const field_id = self.recordFieldId(shape, field.field);
            const value = try self.lowerExpr(field.value);
            evals[i] = .{ .field = field_id, .value = value };
        }

        const shape_fields = self.shapes.recordShapeFields(shape);
        if (shape_fields.len != mono_fields.len) rowInvariant("row finalization record field count did not match finalized shape");
        for (shape_fields, 0..) |field_id, i| {
            assemblies[i] = .{
                .field = field_id,
                .eval_index = self.recordEvalIndex(evals, field_id),
            };
        }

        return .{ .record = .{
            .shape = shape,
            .eval_order = try self.output.addRecordFieldEvalSpan(evals),
            .assembly_order = try self.output.addRecordFieldAssemblySpan(assemblies),
        } };
    }

    fn lowerAccess(self: *BodyFinalizer, access: anytype) Allocator.Error!Ast.Expr.Data {
        const record_expr = self.input.getExpr(access.record);
        const shape = try self.shapes.internRecordShapeFromType(self.types, record_expr.ty);
        return .{ .access = .{
            .record = try self.lowerExpr(access.record),
            .field = self.recordFieldId(shape, access.field),
        } };
    }

    fn lowerTag(self: *BodyFinalizer, tag_ty: TypeId, tag: anytype) Allocator.Error!Ast.Expr.Data {
        const shape = try self.shapes.internTagUnionShapeFromType(self.types, tag_ty);
        const tag_id = self.tagId(shape, tag.name, tag.discriminant);
        const payload_ids = self.shapes.tagPayloads(tag_id);
        const mono_args = self.input.sliceExprSpan(tag.args);
        if (payload_ids.len != mono_args.len) rowInvariant("row finalization tag payload count did not match finalized shape");

        const evals = try self.allocator.alloc(Ast.TagPayloadEval, mono_args.len);
        defer self.allocator.free(evals);
        const assemblies = try self.allocator.alloc(Ast.TagPayloadAssembly, mono_args.len);
        defer self.allocator.free(assemblies);
        for (mono_args, 0..) |arg, i| {
            const value = try self.lowerExpr(arg);
            evals[i] = .{ .payload = payload_ids[i], .value = value };
            assemblies[i] = .{ .payload = payload_ids[i], .eval_index = @intCast(i) };
        }
        return .{ .tag = .{
            .union_shape = shape,
            .tag = tag_id,
            .eval_order = try self.output.addTagPayloadEvalSpan(evals),
            .assembly_order = try self.output.addTagPayloadAssemblySpan(assemblies),
            .constructor_ty = tag.constructor_ty,
        } };
    }

    fn lowerTagPayload(self: *BodyFinalizer, payload: anytype) Allocator.Error!Ast.Expr.Data {
        const tag_union_expr = self.input.getExpr(payload.tag_union);
        const shape = try self.shapes.internTagUnionShapeFromType(self.types, tag_union_expr.ty);
        const tag_id = self.tagId(shape, payload.tag_name, payload.tag_discriminant);
        const payload_id = self.tagPayloadId(tag_id, payload.payload_index);
        return .{ .tag_payload = .{
            .tag_union = try self.lowerExpr(payload.tag_union),
            .payload = payload_id,
        } };
    }

    fn lowerPat(self: *BodyFinalizer, pat_id: Mono.Ast.PatId) Allocator.Error!Ast.PatId {
        const pat = self.input.getPat(pat_id);
        return try self.output.addPat(.{ .ty = pat.ty, .source_ty = pat.source_ty, .data = switch (pat.data) {
            .bool_lit => |value| .{ .bool_lit = value },
            .int_lit => |value| .{ .int_lit = value },
            .frac_f32_lit => |value| .{ .frac_f32_lit = value },
            .frac_f64_lit => |value| .{ .frac_f64_lit = value },
            .dec_lit => |value| .{ .dec_lit = value },
            .str_lit => |value| .{ .str_lit = value },
            .record => |record| blk: {
                const shape = try self.shapes.internRecordShapeFromType(self.types, pat.ty);
                break :blk .{ .record = .{
                    .shape = shape,
                    .fields = try self.lowerRecordFieldPatternSpan(shape, record.fields),
                    .rest = if (record.rest) |rest| try self.lowerPat(rest) else null,
                } };
            },
            .nominal => |child| .{ .nominal = try self.lowerPat(child) },
            .tuple => |items| .{ .tuple = try self.lowerPatSpan(items) },
            .list => |list| .{ .list = .{
                .items = try self.lowerPatSpan(list.items),
                .rest = if (list.rest) |rest| .{
                    .index = rest.index,
                    .pattern = if (rest.pattern) |pattern| try self.lowerPat(pattern) else null,
                } else null,
            } },
            .as => |as| .{ .as = .{
                .pattern = try self.lowerPat(as.pattern),
                .symbol = as.symbol,
            } },
            .var_ => |symbol| .{ .var_ = symbol },
            .wildcard => .wildcard,
            .tag => |tag| blk: {
                const shape = try self.shapes.internTagUnionShapeFromType(self.types, pat.ty);
                const tag_id = self.tagId(shape, tag.name, tag.discriminant);
                const payload_ids = self.shapes.tagPayloads(tag_id);
                const mono_args = self.input.slicePatSpan(tag.args);
                if (payload_ids.len != mono_args.len) rowInvariant("row finalization tag pattern payload count did not match finalized shape");
                const payloads = try self.allocator.alloc(Ast.TagPayloadPattern, mono_args.len);
                defer self.allocator.free(payloads);
                for (mono_args, 0..) |arg, i| {
                    payloads[i] = .{
                        .payload = payload_ids[i],
                        .pattern = try self.lowerPat(arg),
                    };
                }
                break :blk .{ .tag = .{
                    .union_shape = shape,
                    .tag = tag_id,
                    .payloads = try self.output.addTagPayloadPatternSpan(payloads),
                } };
            },
        } });
    }

    fn lowerRecordFieldPatternSpan(
        self: *BodyFinalizer,
        shape: Ast.RecordShapeId,
        span: Mono.Ast.Span(Mono.Ast.RecordFieldPattern),
    ) Allocator.Error!Ast.Span(Ast.RecordFieldPattern) {
        const input_items = self.input.sliceRecordFieldPatternSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.RecordFieldPattern).empty();
        const output_items = try self.allocator.alloc(Ast.RecordFieldPattern, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |field, i| {
            output_items[i] = .{
                .field = self.recordFieldId(shape, field.field),
                .pattern = try self.lowerPat(field.pattern),
            };
        }
        return try self.output.addRecordFieldPatternSpan(output_items);
    }

    fn lowerPatSpan(self: *BodyFinalizer, span: Mono.Ast.Span(Mono.Ast.PatId)) Allocator.Error!Ast.Span(Ast.PatId) {
        const input_items = self.input.slicePatSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.PatId).empty();
        const output_items = try self.allocator.alloc(Ast.PatId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |item, i| {
            output_items[i] = try self.lowerPat(item);
        }
        return try self.output.addPatSpan(output_items);
    }

    fn lowerBranch(self: *BodyFinalizer, branch_id: Mono.Ast.BranchId) Allocator.Error!Ast.BranchId {
        const branch = self.input.getBranch(branch_id);
        return try self.output.addBranch(.{
            .pat = try self.lowerPat(branch.pat),
            .guard = if (branch.guard) |guard| try self.lowerExpr(guard) else null,
            .body = try self.lowerExpr(branch.body),
            .degenerate = branch.degenerate,
        });
    }

    fn lowerStmt(self: *BodyFinalizer, stmt_id: Mono.Ast.StmtId) Allocator.Error!Ast.StmtId {
        const stmt = self.input.getStmt(stmt_id);
        return try self.output.addStmt(switch (stmt) {
            .local_fn => |local_fn| .{ .local_fn = try self.lowerLetFn(local_fn) },
            .decl => |decl| .{ .decl = .{
                .bind = self.lowerTypedSymbol(decl.bind),
                .body = try self.lowerExpr(decl.body),
            } },
            .var_decl => |decl| .{ .var_decl = .{
                .bind = self.lowerTypedSymbol(decl.bind),
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

    fn lowerExprSpan(
        self: *BodyFinalizer,
        span: Mono.Ast.Span(Mono.Ast.ExprId),
    ) Allocator.Error!Ast.Span(Ast.ExprId) {
        const input_items = self.input.sliceExprSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.ExprId).empty();
        const output_items = try self.allocator.alloc(Ast.ExprId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |expr, i| {
            output_items[i] = try self.lowerExpr(expr);
        }
        return try self.output.addExprSpan(output_items);
    }

    fn lowerStmtSpan(
        self: *BodyFinalizer,
        span: Mono.Ast.Span(Mono.Ast.StmtId),
    ) Allocator.Error!Ast.Span(Ast.StmtId) {
        const input_items = self.input.sliceStmtSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.StmtId).empty();
        const output_items = try self.allocator.alloc(Ast.StmtId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |stmt, i| {
            output_items[i] = try self.lowerStmt(stmt);
        }
        return try self.output.addStmtSpan(output_items);
    }

    fn lowerBranchSpan(
        self: *BodyFinalizer,
        span: Mono.Ast.Span(Mono.Ast.BranchId),
    ) Allocator.Error!Ast.Span(Ast.BranchId) {
        const input_items = self.input.sliceBranchSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.BranchId).empty();
        const output_items = try self.allocator.alloc(Ast.BranchId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |branch, i| {
            output_items[i] = try self.lowerBranch(branch);
        }
        return try self.output.addBranchSpan(output_items);
    }

    fn lowerTypedSymbolSpan(
        self: *BodyFinalizer,
        span: Mono.Ast.Span(Mono.Ast.TypedSymbol),
    ) Allocator.Error!Ast.Span(Ast.TypedSymbol) {
        const input_items = self.input.sliceTypedSymbolSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.TypedSymbol).empty();
        const output_items = try self.allocator.alloc(Ast.TypedSymbol, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |symbol, i| {
            output_items[i] = .{ .ty = symbol.ty, .source_ty = symbol.source_ty, .symbol = symbol.symbol };
        }
        return try self.output.addTypedSymbolSpan(output_items);
    }

    fn lowerTypedSymbol(self: *BodyFinalizer, symbol: Mono.Ast.TypedSymbol) Ast.TypedSymbol {
        _ = self;
        return .{ .ty = symbol.ty, .source_ty = symbol.source_ty, .symbol = symbol.symbol };
    }

    fn lowerCaptureArgSpan(
        self: *BodyFinalizer,
        span: Mono.Ast.Span(Mono.Ast.CaptureArg),
    ) Allocator.Error!Ast.Span(Ast.CaptureArg) {
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

    fn recordFieldId(
        self: *const BodyFinalizer,
        shape: RecordShapeId,
        label: canonical.RecordFieldLabelId,
    ) RecordFieldId {
        for (self.shapes.recordShapeFields(shape)) |field_id| {
            if (self.shapes.recordField(field_id).label == label) return field_id;
        }
        rowInvariant("row finalization could not find record field label in finalized shape");
    }

    fn recordEvalIndex(
        self: *const BodyFinalizer,
        evals: []const Ast.RecordFieldEval,
        field_id: RecordFieldId,
    ) u32 {
        _ = self;
        for (evals, 0..) |eval, i| {
            if (eval.field == field_id) return @intCast(i);
        }
        rowInvariant("row finalization record assembly field was missing from eval order");
    }

    fn tagId(
        self: *const BodyFinalizer,
        shape: TagUnionShapeId,
        label: canonical.TagLabelId,
        discriminant: u16,
    ) TagId {
        for (self.shapes.tagUnionTags(shape)) |tag_id| {
            const tag = self.shapes.tag(tag_id);
            if (tag.label == label) {
                if (tag.logical_index != @as(u32, discriminant)) rowInvariant("row finalization tag discriminant disagreed with finalized shape");
                return tag_id;
            }
        }
        rowInvariant("row finalization could not find tag label in finalized shape");
    }

    fn tagPayloadId(
        self: *const BodyFinalizer,
        tag_id: TagId,
        payload_index: u16,
    ) TagPayloadId {
        for (self.shapes.tagPayloads(tag_id)) |payload_id| {
            const payload = self.shapes.tagPayload(payload_id);
            if (payload.logical_index == @as(u32, payload_index)) return payload_id;
        }
        rowInvariant("row finalization could not find payload index in finalized tag");
    }
};

fn rowInvariant(comptime message: []const u8) noreturn {
    verify.invariant(false, message);
    unreachable;
}

fn freeStringHashMapKeys(map: *std.StringHashMap(RecordShapeId), allocator: Allocator) void {
    var keys = map.keyIterator();
    while (keys.next()) |key| allocator.free(key.*);
}

fn freeStringHashMapKeysTag(map: *std.StringHashMap(TagUnionShapeId), allocator: Allocator) void {
    var keys = map.keyIterator();
    while (keys.next()) |key| allocator.free(key.*);
}

test "mono_row store interns empty record shape once" {
    std.testing.refAllDecls(Ast);

    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const first = try store.internRecordShape(&.{});
    const second = try store.internRecordShape(&.{});

    try std.testing.expectEqual(first, second);
    try std.testing.expectEqual(@as(usize, 1), store.record_shapes.items.len);
}
