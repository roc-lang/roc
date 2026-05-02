//! Compile-time evaluation finalization for checked artifacts.
//!
//! This module owns the post-check work that cannot live in `check` because it
//! must run the MIR-family pipeline, ARC insertion, and the LIR interpreter.

const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
const check = @import("check");
const layout_mod = @import("layout");
const lir = @import("lir");

const Interpreter = @import("interpreter.zig").Interpreter;
const RuntimeHostEnv = @import("test/RuntimeHostEnv.zig");
const Value = @import("value.zig").Value;

const Allocator = std.mem.Allocator;
const RocList = builtins.list.RocList;
const RocStr = builtins.str.RocStr;
const checked_artifact = check.CheckedArtifact;

pub fn finalizer() checked_artifact.CompileTimeFinalizer {
    return .{ .finalize = finalize };
}

fn finalize(
    _: ?*anyopaque,
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    imports: []const checked_artifact.PublishImportArtifact,
) anyerror!void {
    const compile_time_roots = try compileTimeRootRequests(allocator, artifact.root_requests.requests);
    defer if (compile_time_roots.len > 0) allocator.free(compile_time_roots);

    if (compile_time_roots.len == 0) {
        try artifact.comptime_values.sealBindings();
        return;
    }

    const import_views = try allocator.alloc(checked_artifact.ImportedModuleView, imports.len);
    defer allocator.free(import_views);
    for (imports, 0..) |import, i| {
        import_views[i] = import.view;
    }

    var lowered = try lir.CheckedPipeline.lowerArtifactsToLir(
        allocator,
        .{
            .root = checked_artifact.loweringView(artifact),
            .imports = import_views,
        },
        .{
            .requests = compile_time_roots,
            .purpose = .compile_time,
        },
        .{
            .target_usize = base.target.TargetUsize.native,
            .artifact_state = .checking_finalization,
        },
    );
    defer lowered.deinit();

    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    var interpreter = try Interpreter.init(
        allocator,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        runtime_env.get_ops(),
    );
    defer interpreter.deinit();

    if (compile_time_roots.len != lowered.lir_result.root_procs.items.len) {
        compileTimeFinalizationInvariant("compile-time lowering did not preserve root cardinality");
    }

    for (compile_time_roots, lowered.lir_result.root_procs.items) |root_request, lir_root| {
        const root = compileTimeRootForRequest(artifact, root_request);
        switch (root.kind) {
            .constant => try evaluateConstantRoot(
                allocator,
                artifact,
                &interpreter,
                &lowered,
                root,
                lir_root,
            ),
            .callable_binding => compileTimeFinalizationInvariant("compile-time callable binding promotion is not sealed yet"),
            .expect => {},
        }
    }

    try artifact.comptime_values.sealBindings();
}

fn compileTimeRootRequests(
    allocator: Allocator,
    roots: []const checked_artifact.RootRequest,
) Allocator.Error![]checked_artifact.RootRequest {
    var out = std.ArrayList(checked_artifact.RootRequest).empty;
    errdefer out.deinit(allocator);

    for (roots) |root| {
        if (root.abi != .compile_time) continue;
        try out.append(allocator, root);
    }

    return try out.toOwnedSlice(allocator);
}

fn evaluateConstantRoot(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    interpreter: *Interpreter,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    root: checked_artifact.CompileTimeRoot,
    lir_root: lir.LIR.LirProcSpecId,
) anyerror!void {
    const pattern = root.pattern orelse compileTimeFinalizationInvariant("constant root had no top-level pattern");
    const top_level = artifact.top_level_values.lookupByPattern(pattern) orelse {
        compileTimeFinalizationInvariant("constant root had no top-level value entry");
    };
    const const_ref = switch (top_level.value) {
        .const_ref => |ref| ref,
        .procedure_binding => compileTimeFinalizationInvariant("constant root top-level value was a procedure binding"),
    };

    const result = try interpreter.eval(.{
        .proc_id = lir_root,
        .arg_layouts = &.{},
    });
    const ret_layout = lowered.lir_result.store.getProcSpec(lir_root).ret_layout;
    defer interpreter.dropValue(result.value, ret_layout);

    var reifier = ComptimeReifier{
        .allocator = allocator,
        .values = &artifact.comptime_values,
        .checked_types = &artifact.checked_types,
        .layouts = &lowered.lir_result.layouts,
    };
    const reified = try reifier.reify(root.checked_type, ret_layout, result.value);

    artifact.const_templates.fillValueGraph(const_ref, .{
        .schema = reified.schema,
        .value = reified.value,
    });
    try artifact.comptime_values.bind(pattern, reified.schema, reified.value);

    const requested_source_ty = artifact.checked_types.roots[@intFromEnum(root.checked_type)].key;
    const instance_ref = try artifact.const_instances.reserve(allocator, .{
        .const_ref = const_ref,
        .requested_source_ty = requested_source_ty,
    });
    artifact.const_instances.fill(instance_ref, .{
        .schema = reified.schema,
        .value = reified.value,
    });
}

fn compileTimeRootForRequest(
    artifact: *const checked_artifact.CheckedModuleArtifact,
    request: checked_artifact.RootRequest,
) checked_artifact.CompileTimeRoot {
    for (artifact.compile_time_roots.roots) |root| {
        if (!rootMatchesRequest(root, request)) continue;
        return root;
    }
    compileTimeFinalizationInvariant("compile-time root request had no matching root record");
}

fn rootMatchesRequest(
    root: checked_artifact.CompileTimeRoot,
    request: checked_artifact.RootRequest,
) bool {
    const kind_matches = switch (root.kind) {
        .constant => request.kind == .compile_time_constant,
        .callable_binding => request.kind == .compile_time_callable,
        .expect => request.kind == .test_expect,
    };
    return kind_matches and rootSourceEql(root.source, request.source);
}

fn rootSourceEql(a: checked_artifact.RootSource, b: checked_artifact.RootSource) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .def => |def| def == b.def,
        .expr => |expr| expr == b.expr,
        .statement => |statement| statement == b.statement,
        .required_binding => |binding| binding == b.required_binding,
    };
}

const ReifiedValue = struct {
    schema: checked_artifact.ComptimeSchemaId,
    value: checked_artifact.ComptimeValueId,
};

const PhysicalValue = struct {
    layout_idx: layout_mod.Idx,
    value: Value,
};

const ComptimeReifier = struct {
    allocator: Allocator,
    values: *checked_artifact.CompileTimeValueStore,
    checked_types: *const checked_artifact.CheckedTypeStore,
    layouts: *const layout_mod.Store,

    fn reify(
        self: *ComptimeReifier,
        checked_ty: checked_artifact.CheckedTypeId,
        layout_idx: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!ReifiedValue {
        const payload = self.checkedPayload(checked_ty);
        return switch (payload) {
            .empty_record => self.reifyZst(),
            .record => |record| self.reifyRecord(record.fields, layout_idx, value),
            .record_unbound => |fields| self.reifyRecord(fields, layout_idx, value),
            .tuple => |items| self.reifyTuple(items, layout_idx, value),
            .tag_union => |tag_union| self.reifyTagUnion(tag_union, layout_idx, value),
            .empty_tag_union => reifierInvariant("attempted to reify empty tag union value"),
            .alias => |alias| self.reifyAlias(alias, layout_idx, value),
            .nominal => |nominal| self.reifyNominal(nominal, layout_idx, value),
            .function => reifierInvariant("callable constant leaf reification requires callable promotion plans"),
            .flex, .rigid => reifierInvariant("compile-time reification reached unresolved type variable"),
            .pending => reifierInvariant("compile-time reification reached pending checked type"),
        };
    }

    fn schemaFor(
        self: *ComptimeReifier,
        checked_ty: checked_artifact.CheckedTypeId,
    ) Allocator.Error!checked_artifact.ComptimeSchemaId {
        const payload = self.checkedPayload(checked_ty);
        return switch (payload) {
            .empty_record => self.values.addSchema(.zst),
            .record => |record| self.schemaForRecord(record.fields),
            .record_unbound => |fields| self.schemaForRecord(fields),
            .tuple => |items| self.schemaForTuple(items),
            .tag_union => |tag_union| self.schemaForTagUnion(tag_union),
            .empty_tag_union => reifierInvariant("attempted to build schema for empty tag union value"),
            .alias => |alias| self.schemaForAlias(alias),
            .nominal => |nominal| self.schemaForNominal(nominal),
            .function => reifierInvariant("callable constant leaf schema requires callable promotion plans"),
            .flex, .rigid => reifierInvariant("compile-time schema construction reached unresolved type variable"),
            .pending => reifierInvariant("compile-time schema construction reached pending checked type"),
        };
    }

    fn reifyZst(self: *ComptimeReifier) Allocator.Error!ReifiedValue {
        return .{
            .schema = try self.values.addSchema(.zst),
            .value = try self.values.addValue(.zst),
        };
    }

    fn reifyAlias(
        self: *ComptimeReifier,
        alias: checked_artifact.CheckedAliasType,
        layout_idx: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!ReifiedValue {
        const backing = try self.reify(alias.backing, layout_idx, value);
        const schema = try self.values.addSchema(.{ .alias = .{
            .type_name = .{
                .module_name = alias.origin_module,
                .type_name = alias.name,
            },
            .backing = backing.schema,
        } });
        return .{
            .schema = schema,
            .value = try self.values.addValue(.{ .alias = backing.value }),
        };
    }

    fn schemaForAlias(
        self: *ComptimeReifier,
        alias: checked_artifact.CheckedAliasType,
    ) Allocator.Error!checked_artifact.ComptimeSchemaId {
        return self.values.addSchema(.{ .alias = .{
            .type_name = .{
                .module_name = alias.origin_module,
                .type_name = alias.name,
            },
            .backing = try self.schemaFor(alias.backing),
        } });
    }

    fn reifyNominal(
        self: *ComptimeReifier,
        nominal: checked_artifact.CheckedNominalType,
        layout_idx: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!ReifiedValue {
        if (nominal.builtin) |builtin_nominal| {
            return switch (builtin_nominal) {
                .str => self.reifyStr(layout_idx, value),
                .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .f32, .f64, .dec => self.reifyScalar(layout_idx, value),
                .list => self.reifyList(nominalArg(nominal, 0), layout_idx, value),
                .box => self.reifyBox(nominalArg(nominal, 0), layout_idx, value),
                .bool => self.reify(nominal.backing, layout_idx, value),
            };
        }

        const backing = try self.reify(nominal.backing, layout_idx, value);
        const schema = try self.values.addSchema(.{ .nominal = .{
            .type_name = .{
                .module_name = nominal.origin_module,
                .type_name = nominal.name,
            },
            .backing = backing.schema,
            .is_opaque = nominal.is_opaque,
        } });
        return .{
            .schema = schema,
            .value = try self.values.addValue(.{ .nominal = backing.value }),
        };
    }

    fn schemaForNominal(
        self: *ComptimeReifier,
        nominal: checked_artifact.CheckedNominalType,
    ) Allocator.Error!checked_artifact.ComptimeSchemaId {
        if (nominal.builtin) |builtin_nominal| {
            return switch (builtin_nominal) {
                .str => self.values.addSchema(.str),
                .u8 => self.values.addSchema(.{ .int = .u8 }),
                .i8 => self.values.addSchema(.{ .int = .i8 }),
                .u16 => self.values.addSchema(.{ .int = .u16 }),
                .i16 => self.values.addSchema(.{ .int = .i16 }),
                .u32 => self.values.addSchema(.{ .int = .u32 }),
                .i32 => self.values.addSchema(.{ .int = .i32 }),
                .u64 => self.values.addSchema(.{ .int = .u64 }),
                .i64 => self.values.addSchema(.{ .int = .i64 }),
                .u128 => self.values.addSchema(.{ .int = .u128 }),
                .i128 => self.values.addSchema(.{ .int = .i128 }),
                .f32 => self.values.addSchema(.{ .frac = .f32 }),
                .f64 => self.values.addSchema(.{ .frac = .f64 }),
                .dec => self.values.addSchema(.{ .frac = .dec }),
                .list => self.values.addSchema(.{ .list = try self.schemaFor(nominalArg(nominal, 0)) }),
                .box => self.values.addSchema(.{ .box = try self.schemaFor(nominalArg(nominal, 0)) }),
                .bool => self.schemaFor(nominal.backing),
            };
        }

        return self.values.addSchema(.{ .nominal = .{
            .type_name = .{
                .module_name = nominal.origin_module,
                .type_name = nominal.name,
            },
            .backing = try self.schemaFor(nominal.backing),
            .is_opaque = nominal.is_opaque,
        } });
    }

    fn reifyScalar(
        self: *ComptimeReifier,
        layout_idx: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!ReifiedValue {
        const layout = self.layouts.getLayout(layout_idx);
        if (layout.tag != .scalar) reifierInvariant("scalar checked type did not lower to scalar layout");
        const scalar = layout.data.scalar;
        return switch (scalar.tag) {
            .str => self.reifyStr(layout_idx, value),
            .int => blk: {
                var bytes = [_]u8{0} ** 16;
                const size: usize = @intCast(self.layouts.layoutSize(layout));
                @memcpy(bytes[0..size], value.readBytes(size));
                break :blk .{
                    .schema = try self.values.addSchema(.{ .int = scalar.data.int }),
                    .value = try self.values.addValue(.{ .int_bytes = bytes }),
                };
            },
            .frac => switch (scalar.data.frac) {
                .f32 => .{
                    .schema = try self.values.addSchema(.{ .frac = .f32 }),
                    .value = try self.values.addValue(.{ .f32 = value.read(f32) }),
                },
                .f64 => .{
                    .schema = try self.values.addSchema(.{ .frac = .f64 }),
                    .value = try self.values.addValue(.{ .f64 = value.read(f64) }),
                },
                .dec => blk: {
                    var bytes = [_]u8{0} ** 16;
                    @memcpy(bytes[0..16], value.readBytes(16));
                    break :blk .{
                        .schema = try self.values.addSchema(.{ .frac = .dec }),
                        .value = try self.values.addValue(.{ .dec = bytes }),
                    };
                },
            },
            .opaque_ptr => reifierInvariant("compile-time constants cannot reify opaque pointers"),
        };
    }

    fn reifyStr(
        self: *ComptimeReifier,
        _: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!ReifiedValue {
        const roc_str: *const RocStr = @ptrCast(@alignCast(value.ptr));
        const owned = try self.allocator.dupe(u8, roc_str.asSlice());
        errdefer self.allocator.free(owned);
        return .{
            .schema = try self.values.addSchema(.str),
            .value = try self.values.addValue(.{ .str = owned }),
        };
    }

    fn reifyList(
        self: *ComptimeReifier,
        elem_ty: checked_artifact.CheckedTypeId,
        layout_idx: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!ReifiedValue {
        const layout = self.layouts.getLayout(layout_idx);
        const elem_layout_idx = switch (layout.tag) {
            .list => layout.data.list,
            .list_of_zst => layout_mod.Idx.zst,
            else => reifierInvariant("List(T) checked type did not lower to list layout"),
        };
        const elem_layout = self.layouts.getLayout(elem_layout_idx);
        const elem_size: usize = @intCast(self.layouts.layoutSize(elem_layout));
        const elem_schema = try self.schemaFor(elem_ty);

        const roc_list: *const RocList = @ptrCast(@alignCast(value.ptr));
        const items = try self.allocator.alloc(checked_artifact.ComptimeValueId, roc_list.len());
        errdefer self.allocator.free(items);

        var i: usize = 0;
        while (i < roc_list.len()) : (i += 1) {
            const elem_value = if (elem_size == 0)
                Value.zst
            else
                Value{ .ptr = (roc_list.bytes orelse reifierInvariant("non-empty list had null bytes pointer")) + i * elem_size };
            items[i] = (try self.reify(elem_ty, elem_layout_idx, elem_value)).value;
        }

        return .{
            .schema = try self.values.addSchema(.{ .list = elem_schema }),
            .value = try self.values.addValue(.{ .list = items }),
        };
    }

    fn reifyBox(
        self: *ComptimeReifier,
        elem_ty: checked_artifact.CheckedTypeId,
        layout_idx: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!ReifiedValue {
        const layout = self.layouts.getLayout(layout_idx);
        const elem_layout_idx = switch (layout.tag) {
            .box => layout.data.box,
            .box_of_zst => layout_mod.Idx.zst,
            else => reifierInvariant("Box(T) checked type did not lower to box layout"),
        };
        const child = if (layout.tag == .box_of_zst)
            try self.reify(elem_ty, elem_layout_idx, Value.zst)
        else blk: {
            const payload = value.read(?[*]u8) orelse reifierInvariant("Box(T) value had null payload pointer");
            break :blk try self.reify(elem_ty, elem_layout_idx, .{ .ptr = payload });
        };
        return .{
            .schema = try self.values.addSchema(.{ .box = child.schema }),
            .value = try self.values.addValue(.{ .box = child.value }),
        };
    }

    fn reifyRecord(
        self: *ComptimeReifier,
        fields: []const checked_artifact.CheckedRecordField,
        layout_idx: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!ReifiedValue {
        if (fields.len == 0) return self.reifyZst();
        const physical = self.logicalAggregateValue(layout_idx, value, .struct_);
        const layout = self.layouts.getLayout(physical.layout_idx);
        if (layout.tag != .struct_) reifierInvariant("record checked type did not lower to struct layout");

        const schema_fields = try self.allocator.alloc(checked_artifact.ComptimeFieldSchema, fields.len);
        errdefer self.allocator.free(schema_fields);
        const value_fields = try self.allocator.alloc(checked_artifact.ComptimeValueId, fields.len);
        errdefer self.allocator.free(value_fields);

        for (fields, 0..) |field, i| {
            const field_layout_idx = self.layouts.getStructFieldLayoutByOriginalIndex(layout.data.struct_.idx, @intCast(i));
            const field_layout = self.layouts.getLayout(field_layout_idx);
            const offset = self.layouts.getStructFieldOffsetByOriginalIndex(layout.data.struct_.idx, @intCast(i));
            const field_value = if (self.layouts.layoutSize(field_layout) == 0) Value.zst else physical.value.offset(offset);
            const reified = try self.reify(field.ty, field_layout_idx, field_value);
            schema_fields[i] = .{ .name = field.name, .schema = reified.schema };
            value_fields[i] = reified.value;
        }

        return .{
            .schema = try self.values.addSchema(.{ .record = schema_fields }),
            .value = try self.values.addValue(.{ .record = value_fields }),
        };
    }

    fn schemaForRecord(
        self: *ComptimeReifier,
        fields: []const checked_artifact.CheckedRecordField,
    ) Allocator.Error!checked_artifact.ComptimeSchemaId {
        if (fields.len == 0) return self.values.addSchema(.zst);

        const schema_fields = try self.allocator.alloc(checked_artifact.ComptimeFieldSchema, fields.len);
        errdefer self.allocator.free(schema_fields);
        for (fields, 0..) |field, i| {
            schema_fields[i] = .{
                .name = field.name,
                .schema = try self.schemaFor(field.ty),
            };
        }
        return self.values.addSchema(.{ .record = schema_fields });
    }

    fn reifyTuple(
        self: *ComptimeReifier,
        items: []const checked_artifact.CheckedTypeId,
        layout_idx: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!ReifiedValue {
        if (items.len == 0) return self.reifyZst();
        const physical = self.logicalAggregateValue(layout_idx, value, .struct_);
        const layout = self.layouts.getLayout(physical.layout_idx);
        if (layout.tag != .struct_) reifierInvariant("tuple checked type did not lower to struct layout");

        const schemas = try self.allocator.alloc(checked_artifact.ComptimeSchemaId, items.len);
        errdefer self.allocator.free(schemas);
        const values = try self.allocator.alloc(checked_artifact.ComptimeValueId, items.len);
        errdefer self.allocator.free(values);

        for (items, 0..) |item_ty, i| {
            const item_layout_idx = self.layouts.getStructFieldLayoutByOriginalIndex(layout.data.struct_.idx, @intCast(i));
            const item_layout = self.layouts.getLayout(item_layout_idx);
            const offset = self.layouts.getStructFieldOffsetByOriginalIndex(layout.data.struct_.idx, @intCast(i));
            const item_value = if (self.layouts.layoutSize(item_layout) == 0) Value.zst else physical.value.offset(offset);
            const reified = try self.reify(item_ty, item_layout_idx, item_value);
            schemas[i] = reified.schema;
            values[i] = reified.value;
        }

        return .{
            .schema = try self.values.addSchema(.{ .tuple = schemas }),
            .value = try self.values.addValue(.{ .tuple = values }),
        };
    }

    fn schemaForTuple(
        self: *ComptimeReifier,
        items: []const checked_artifact.CheckedTypeId,
    ) Allocator.Error!checked_artifact.ComptimeSchemaId {
        if (items.len == 0) return self.values.addSchema(.zst);

        const schemas = try self.allocator.alloc(checked_artifact.ComptimeSchemaId, items.len);
        errdefer self.allocator.free(schemas);
        for (items, 0..) |item_ty, i| {
            schemas[i] = try self.schemaFor(item_ty);
        }
        return self.values.addSchema(.{ .tuple = schemas });
    }

    fn reifyTagUnion(
        self: *ComptimeReifier,
        tag_union: checked_artifact.CheckedTagUnionType,
        layout_idx: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!ReifiedValue {
        const physical = self.logicalAggregateValue(layout_idx, value, .tag_union);
        const layout = self.layouts.getLayout(physical.layout_idx);
        if (layout.tag != .tag_union) reifierInvariant("tag union checked type did not lower to tag-union layout");
        const info = self.layouts.getTagUnionInfo(layout);
        const discriminant = info.data.readDiscriminant(physical.value.ptr);
        if (discriminant >= tag_union.tags.len) reifierInvariant("tag union discriminant was outside checked tag set");

        const variants = try self.allocator.alloc(checked_artifact.ComptimeVariantSchema, tag_union.tags.len);
        errdefer {
            for (variants) |variant| self.allocator.free(variant.payloads);
            self.allocator.free(variants);
        }

        for (tag_union.tags, 0..) |tag, i| {
            const payloads = try self.allocator.alloc(checked_artifact.ComptimeSchemaId, tag.args.len);
            errdefer self.allocator.free(payloads);
            for (tag.args, 0..) |arg_ty, arg_i| {
                payloads[arg_i] = try self.schemaFor(arg_ty);
            }
            variants[i] = .{ .name = tag.name, .payloads = payloads };
        }

        const active_tag = tag_union.tags[discriminant];
        const active_payload_layout = info.variants.get(@intCast(discriminant)).payload_layout;
        const payload_values = try self.allocator.alloc(checked_artifact.ComptimeValueId, active_tag.args.len);
        errdefer self.allocator.free(payload_values);
        for (active_tag.args, 0..) |arg_ty, arg_i| {
            const arg_layout_idx = payloadLayoutForTagArg(self.layouts, active_payload_layout, active_tag.args.len, @intCast(arg_i));
            const arg_layout = self.layouts.getLayout(arg_layout_idx);
            const offset = payloadOffsetForTagArg(self.layouts, active_payload_layout, active_tag.args.len, @intCast(arg_i));
            const arg_value = if (self.layouts.layoutSize(arg_layout) == 0) Value.zst else physical.value.offset(offset);
            payload_values[arg_i] = (try self.reify(arg_ty, arg_layout_idx, arg_value)).value;
        }

        return .{
            .schema = try self.values.addSchema(.{ .tag_union = variants }),
            .value = try self.values.addValue(.{ .tag_union = .{
                .variant_index = discriminant,
                .payloads = payload_values,
            } }),
        };
    }

    fn schemaForTagUnion(
        self: *ComptimeReifier,
        tag_union: checked_artifact.CheckedTagUnionType,
    ) Allocator.Error!checked_artifact.ComptimeSchemaId {
        const variants = try self.allocator.alloc(checked_artifact.ComptimeVariantSchema, tag_union.tags.len);
        errdefer {
            for (variants) |variant| self.allocator.free(variant.payloads);
            self.allocator.free(variants);
        }

        for (tag_union.tags, 0..) |tag, i| {
            const payloads = try self.allocator.alloc(checked_artifact.ComptimeSchemaId, tag.args.len);
            errdefer self.allocator.free(payloads);
            for (tag.args, 0..) |arg_ty, arg_i| {
                payloads[arg_i] = try self.schemaFor(arg_ty);
            }
            variants[i] = .{ .name = tag.name, .payloads = payloads };
        }

        return self.values.addSchema(.{ .tag_union = variants });
    }

    fn checkedPayload(self: *const ComptimeReifier, ty: checked_artifact.CheckedTypeId) checked_artifact.CheckedTypePayload {
        return self.checked_types.payloads[@intFromEnum(ty)];
    }

    fn logicalAggregateValue(
        self: *const ComptimeReifier,
        layout_idx: layout_mod.Idx,
        value: Value,
        expected_tag: layout_mod.LayoutTag,
    ) PhysicalValue {
        const layout = self.layouts.getLayout(layout_idx);
        switch (layout.tag) {
            .box => {
                const payload = value.read(?[*]u8) orelse reifierInvariant("logical aggregate used boxed physical layout with null payload");
                const inner_layout = self.layouts.getLayout(layout.data.box);
                if (inner_layout.tag != expected_tag) {
                    reifierInvariant("logical aggregate physical box did not contain expected layout tag");
                }
                return .{ .layout_idx = layout.data.box, .value = .{ .ptr = payload } };
            },
            .box_of_zst => {
                if (expected_tag != .zst) reifierInvariant("logical aggregate used box_of_zst for non-ZST layout");
                return .{ .layout_idx = .zst, .value = Value.zst };
            },
            else => return .{ .layout_idx = layout_idx, .value = value },
        }
    }
};

fn nominalArg(
    nominal: checked_artifact.CheckedNominalType,
    index: usize,
) checked_artifact.CheckedTypeId {
    if (index >= nominal.args.len) reifierInvariant("builtin nominal type was missing an argument");
    return nominal.args[index];
}

fn payloadLayoutForTagArg(
    layouts: *const layout_mod.Store,
    variant_layout_idx: layout_mod.Idx,
    arg_count: usize,
    arg_index: u32,
) layout_mod.Idx {
    if (arg_count == 0) return layout_mod.Idx.zst;
    if (arg_count == 1) return variant_layout_idx;
    const variant_layout = layouts.getLayout(variant_layout_idx);
    if (variant_layout.tag != .struct_) reifierInvariant("multi-payload tag did not use struct payload layout");
    return layouts.getStructFieldLayoutByOriginalIndex(variant_layout.data.struct_.idx, arg_index);
}

fn payloadOffsetForTagArg(
    layouts: *const layout_mod.Store,
    variant_layout_idx: layout_mod.Idx,
    arg_count: usize,
    arg_index: u32,
) u32 {
    if (arg_count <= 1) return 0;
    const variant_layout = layouts.getLayout(variant_layout_idx);
    if (variant_layout.tag != .struct_) reifierInvariant("multi-payload tag did not use struct payload layout");
    return layouts.getStructFieldOffsetByOriginalIndex(variant_layout.data.struct_.idx, arg_index);
}

fn reifierInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("compile-time reifier invariant violated: " ++ message, .{});
    }
    unreachable;
}

fn compileTimeFinalizationInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("compile-time finalization invariant violated: " ++ message, .{});
    }
    unreachable;
}

test "compile-time finalization tests" {
    std.testing.refAllDecls(@This());
}
