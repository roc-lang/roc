//! Target-layout readonly data symbols for provided non-function constants.
//!
//! This module turns explicit checked-artifact constant data into object-file
//! readonly symbols and relocations. It does not inspect source syntax and does
//! not create runtime initializer procedures.

const std = @import("std");
const Allocator = std.mem.Allocator;

const backend = @import("backend");
const base = @import("base");
const builtins = @import("builtins");
const check = @import("check");
const layout_mod = @import("layout");
const lir = @import("lir");
const mir = @import("mir");
const roc_target = @import("roc_target");
const types = @import("types");

const CheckedArtifact = check.CheckedArtifact;
const canonical = check.CanonicalNames;
const repr = mir.LambdaSolved.Representation;
const ArtifactViews = mir.Executable.Build.ArtifactViews;

const StaticDataExport = backend.StaticDataExport;
const StaticDataRelocation = backend.StaticDataRelocation;

const MaterializationError = Allocator.Error || error{
    UnsupportedTarget,
};

const ValueLayout = struct {
    idx: layout_mod.Idx,
    size: u32,
    alignment: u32,
    contains_refcounted: bool,
};

const MaterializedValue = struct {
    bytes: []u8,
    alignment: u32,
    relocations: []StaticDataRelocation,
};

const PointerTarget = struct {
    symbol_name: []const u8,
    addend: i64,
};

const MaterializationContext = struct {
    owner: CheckedArtifact.CheckedModuleArtifactKey,
    canonical_names: *const canonical.CanonicalNameStore,
    plans: *const CheckedArtifact.CompileTimePlanStore,
    values: *const CheckedArtifact.CompileTimeValueStore,
    executable_type_payloads: *const CheckedArtifact.ExecutableTypePayloadStore,
    callable_set_descriptors: *const CheckedArtifact.CallableSetDescriptorStore,
    const_instances: CheckedArtifact.ConstInstantiationStoreView,
};

const ExecutablePayloadContext = struct {
    materialization: MaterializationContext,
    payload: CheckedArtifact.ExecutableTypePayload,
};

/// Build every host-visible provided data export for a target.
pub fn buildProvidedDataExports(
    allocator: Allocator,
    artifact_views: ArtifactViews,
    lowered: ?*const lir.CheckedPipeline.LoweredProgram,
    target: roc_target.RocTarget,
) MaterializationError![]StaticDataExport {
    var builder = try StaticDataBuilder.init(allocator, artifact_views, lowered, target);
    defer builder.deinit();
    return try builder.build();
}

/// Free a static-data graph returned by `buildProvidedDataExports`.
pub fn deinitProvidedDataExports(allocator: Allocator, exports: []StaticDataExport) void {
    for (exports) |static_export| {
        allocator.free(static_export.symbol_name);
        allocator.free(static_export.bytes);
        deinitRelocationSlice(allocator, static_export.relocations);
        allocator.free(static_export.relocations);
    }
    allocator.free(exports);
}

fn deinitRelocationSlice(allocator: Allocator, relocations: []const StaticDataRelocation) void {
    for (relocations) |relocation| {
        if (relocation.owns_target_symbol_name) allocator.free(relocation.target_symbol_name);
    }
}

fn deinitRelocationList(allocator: Allocator, relocations: *std.ArrayList(StaticDataRelocation)) void {
    deinitRelocationSlice(allocator, relocations.items);
    relocations.deinit(allocator);
}

const StaticDataBuilder = struct {
    allocator: Allocator,
    artifact_views: ArtifactViews,
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    lowered: ?*const lir.CheckedPipeline.LoweredProgram,
    target: roc_target.RocTarget,
    target_usize: base.target.TargetUsize,
    word_size: u32,
    layout_store: layout_mod.Store,
    nodes: std.ArrayList(StaticDataExport),
    local_symbol_ordinal: u32,

    fn init(
        allocator: Allocator,
        artifact_views: ArtifactViews,
        lowered: ?*const lir.CheckedPipeline.LoweredProgram,
        target: roc_target.RocTarget,
    ) MaterializationError!StaticDataBuilder {
        const root = artifact_views.root orelse {
            staticDataInvariant("static data export requires a root lowering artifact view");
        };
        const target_usize = targetUsizeForTarget(target) orelse return error.UnsupportedTarget;
        return .{
            .allocator = allocator,
            .artifact_views = artifact_views,
            .artifact = root.artifact,
            .lowered = lowered,
            .target = target,
            .target_usize = target_usize,
            .word_size = target_usize.size(),
            .layout_store = try layout_mod.Store.init(allocator, target_usize),
            .nodes = .empty,
            .local_symbol_ordinal = 0,
        };
    }

    fn deinit(self: *StaticDataBuilder) void {
        self.layout_store.deinit();
    }

    fn build(self: *StaticDataBuilder) MaterializationError![]StaticDataExport {
        errdefer self.deinitNodes();

        for (self.artifact.provided_exports.exports) |provided| {
            const data = switch (provided) {
                .data => |data| data,
                .procedure => continue,
            };

            const binding = self.artifact.comptime_values.lookupBinding(data.pattern) orelse {
                staticDataInvariant("provided data export has no published compile-time value");
            };

            const entrypoint_name = self.artifact.canonical_names.externalSymbolNameText(data.ffi_symbol);
            const symbol_name = try std.fmt.allocPrint(self.allocator, "roc__{s}", .{entrypoint_name});
            errdefer self.allocator.free(symbol_name);

            const materialized = try self.materializeValue(binding.schema, binding.value);
            errdefer self.deinitMaterialized(materialized);

            try self.nodes.append(self.allocator, .{
                .symbol_name = symbol_name,
                .bytes = materialized.bytes,
                .alignment = materialized.alignment,
                .is_global = true,
                .relocations = materialized.relocations,
            });
        }

        return try self.nodes.toOwnedSlice(self.allocator);
    }

    fn deinitNodes(self: *StaticDataBuilder) void {
        for (self.nodes.items) |node| {
            self.allocator.free(node.symbol_name);
            self.allocator.free(node.bytes);
            deinitRelocationSlice(self.allocator, node.relocations);
            self.allocator.free(node.relocations);
        }
        self.nodes.deinit(self.allocator);
    }

    fn deinitMaterialized(self: *StaticDataBuilder, value: MaterializedValue) void {
        self.allocator.free(value.bytes);
        deinitRelocationSlice(self.allocator, value.relocations);
        self.allocator.free(value.relocations);
    }

    fn materializeValue(
        self: *StaticDataBuilder,
        schema_id: CheckedArtifact.ComptimeSchemaId,
        value_id: CheckedArtifact.ComptimeValueId,
    ) MaterializationError!MaterializedValue {
        const value_layout = try self.layoutForSchema(schema_id);
        const bytes = try self.allocator.alloc(u8, value_layout.size);
        @memset(bytes, 0);

        var relocations = std.ArrayList(StaticDataRelocation).empty;
        errdefer {
            deinitRelocationList(self.allocator, &relocations);
            self.allocator.free(bytes);
        }

        try self.writeValue(bytes, &relocations, 0, schema_id, value_id, value_layout.idx);

        return .{
            .bytes = bytes,
            .alignment = value_layout.alignment,
            .relocations = try relocations.toOwnedSlice(self.allocator),
        };
    }

    fn writeValue(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        schema_id: CheckedArtifact.ComptimeSchemaId,
        value_id: CheckedArtifact.ComptimeValueId,
        layout_idx: layout_mod.Idx,
    ) MaterializationError!void {
        const schema = self.comptimeSchema(schema_id);
        const value = self.comptimeValue(value_id);
        switch (schema) {
            .pending => staticDataInvariant("static data export has pending schema"),
            .zst => switch (value) {
                .zst => {},
                else => staticDataInvariant("static ZST export has non-ZST value"),
            },
            .int => |precision| self.writeInt(bytes, base_offset, precision, value),
            .frac => |precision| self.writeFrac(bytes, base_offset, precision, value),
            .str => try self.writeStr(bytes, relocations, base_offset, value),
            .list => |elem_schema| try self.writeList(bytes, relocations, base_offset, elem_schema, value, layout_idx),
            .box => |payload_schema| try self.writeBox(bytes, relocations, base_offset, payload_schema, value, layout_idx),
            .tuple => |items| try self.writeTuple(bytes, relocations, base_offset, items, value, layout_idx),
            .record => |fields| try self.writeRecord(bytes, relocations, base_offset, fields, value, layout_idx),
            .tag_union => |variants| try self.writeTagUnion(bytes, relocations, base_offset, variants, value, layout_idx),
            .alias => |wrapped| {
                const inner = switch (value) {
                    .alias => |inner| inner,
                    else => staticDataInvariant("static alias export has non-alias value"),
                };
                try self.writeValue(bytes, relocations, base_offset, wrapped.backing, inner, layout_idx);
            },
            .nominal => |wrapped| {
                const inner = switch (value) {
                    .nominal => |inner| inner,
                    else => staticDataInvariant("static nominal export has non-nominal value"),
                };
                try self.writeValue(bytes, relocations, base_offset, wrapped.backing, inner, layout_idx);
            },
            .callable => staticDataInvariant("provided callable data export requires executable callable materialization metadata"),
        }
    }

    fn writeInt(
        self: *StaticDataBuilder,
        bytes: []u8,
        base_offset: u32,
        precision: types.Int.Precision,
        value: CheckedArtifact.ComptimeValue,
    ) void {
        const int_bytes = switch (value) {
            .int_bytes => |int_bytes| int_bytes,
            else => staticDataInvariant("static integer export has non-integer value"),
        };
        const size = precision.size();
        self.writeBytes(bytes, base_offset, int_bytes[0..size]);
    }

    fn writeFrac(
        self: *StaticDataBuilder,
        bytes: []u8,
        base_offset: u32,
        precision: types.Frac.Precision,
        value: CheckedArtifact.ComptimeValue,
    ) void {
        switch (precision) {
            .f32 => {
                const f = switch (value) {
                    .f32 => |f| f,
                    else => staticDataInvariant("static F32 export has non-F32 value"),
                };
                var out: [4]u8 = undefined;
                std.mem.writeInt(u32, &out, @bitCast(f), .little);
                self.writeBytes(bytes, base_offset, &out);
            },
            .f64 => {
                const f = switch (value) {
                    .f64 => |f| f,
                    else => staticDataInvariant("static F64 export has non-F64 value"),
                };
                var out: [8]u8 = undefined;
                std.mem.writeInt(u64, &out, @bitCast(f), .little);
                self.writeBytes(bytes, base_offset, &out);
            },
            .dec => {
                const dec = switch (value) {
                    .dec => |dec| dec,
                    else => staticDataInvariant("static Dec export has non-Dec value"),
                };
                self.writeBytes(bytes, base_offset, &dec);
            },
        }
    }

    fn writeStr(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        value: CheckedArtifact.ComptimeValue,
    ) MaterializationError!void {
        const str_bytes = switch (value) {
            .str => |str_bytes| str_bytes,
            else => staticDataInvariant("static Str export has non-Str value"),
        };
        const roc_str_size = self.word_size * 3;
        if (str_bytes.len < roc_str_size) {
            self.writeBytes(bytes, base_offset, str_bytes);
            bytes[base_offset + roc_str_size - 1] = @as(u8, @intCast(str_bytes.len)) | 0x80;
            return;
        }

        const payload = try self.allocator.dupe(u8, str_bytes);
        const target = try self.addStaticAllocation(payload, self.word_size, false, null);
        try self.writePointerRelocation(bytes, relocations, base_offset, target.symbol_name, target.addend);
        self.writeWord(bytes, base_offset + self.word_size, str_bytes.len);
        self.writeWord(bytes, base_offset + self.word_size * 2, str_bytes.len);
    }

    fn writeList(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        elem_schema: CheckedArtifact.ComptimeSchemaId,
        value: CheckedArtifact.ComptimeValue,
        list_layout_idx: layout_mod.Idx,
    ) MaterializationError!void {
        const items = switch (value) {
            .list => |items| items,
            else => staticDataInvariant("static List export has non-list value"),
        };
        if (items.len == 0) {
            self.writeWord(bytes, base_offset, 0);
            self.writeWord(bytes, base_offset + self.word_size, 0);
            self.writeWord(bytes, base_offset + self.word_size * 2, 0);
            return;
        }

        const list_layout = self.layout_store.getLayout(list_layout_idx);
        const abi = self.layout_store.builtinListAbi(list_layout_idx);
        const payload_size = @as(usize, abi.elem_size) * items.len;
        const payload = try self.allocator.alloc(u8, payload_size);
        @memset(payload, 0);

        var payload_relocs = std.ArrayList(StaticDataRelocation).empty;
        var payload_consumed = false;
        errdefer {
            if (!payload_consumed) {
                deinitRelocationList(self.allocator, &payload_relocs);
                self.allocator.free(payload);
            }
        }

        if (abi.elem_size != 0) {
            const elem_layout_idx = switch (list_layout.tag) {
                .list => list_layout.data.list,
                .list_of_zst => layout_mod.Idx.zst,
                else => staticDataInvariant("static List schema did not lower to list layout"),
            };
            for (items, 0..) |item, i| {
                try self.writeValue(
                    payload,
                    &payload_relocs,
                    @as(u32, @intCast(i * abi.elem_size)),
                    elem_schema,
                    item,
                    elem_layout_idx,
                );
            }
        }

        const payload_relocations = try payload_relocs.toOwnedSlice(self.allocator);
        errdefer if (!payload_consumed) self.allocator.free(payload_relocations);
        const target = try self.addStaticAllocationWithRelocs(
            payload,
            abi.elem_alignment,
            abi.contains_refcounted,
            if (abi.contains_refcounted) items.len else null,
            payload_relocations,
        );
        payload_consumed = true;
        try self.writePointerRelocation(bytes, relocations, base_offset, target.symbol_name, target.addend);
        self.writeWord(bytes, base_offset + self.word_size, items.len);
        self.writeWord(bytes, base_offset + self.word_size * 2, items.len);
    }

    fn writeBox(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        payload_schema: CheckedArtifact.ComptimeSchemaId,
        value: CheckedArtifact.ComptimeValue,
        box_layout_idx: layout_mod.Idx,
    ) MaterializationError!void {
        const payload_value = switch (value) {
            .box => |payload| payload,
            else => staticDataInvariant("static Box export has non-box value"),
        };
        const box_layout = self.layout_store.getLayout(box_layout_idx);
        if (box_layout.tag == .box_of_zst) {
            self.writeWord(bytes, base_offset, 0);
            return;
        }
        if (box_layout.tag == .erased_callable) {
            try self.writeBoxedErasedCallable(bytes, relocations, base_offset, payload_schema, payload_value);
            return;
        }
        if (box_layout.tag != .box) staticDataInvariant("static Box schema did not lower to box layout");

        const abi = self.layout_store.builtinBoxAbi(box_layout_idx);
        const payload = try self.materializeValueWithLayout(payload_schema, payload_value, abi.elem_layout_idx orelse layout_mod.Idx.zst);
        var payload_consumed = false;
        errdefer if (!payload_consumed) self.deinitMaterialized(payload);

        const target = try self.addStaticAllocationWithRelocs(
            payload.bytes,
            abi.elem_alignment,
            abi.contains_refcounted,
            null,
            payload.relocations,
        );
        payload_consumed = true;
        try self.writePointerRelocation(bytes, relocations, base_offset, target.symbol_name, target.addend);
    }

    fn writeBoxedErasedCallable(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        payload_schema: CheckedArtifact.ComptimeSchemaId,
        payload_value: CheckedArtifact.ComptimeValueId,
    ) MaterializationError!void {
        switch (self.comptimeSchema(payload_schema)) {
            .callable => {},
            else => staticDataInvariant("static erased Box payload schema is not callable"),
        }
        const leaf = switch (self.comptimeValue(payload_value)) {
            .callable => |callable| callable,
            else => staticDataInvariant("static erased Box payload value is not callable"),
        };
        const erased = switch (leaf) {
            .erased_boxed => |erased| erased,
            .finite => staticDataInvariant("static Box(function) export reached non-erased finite callable leaf"),
        };
        try self.writeSealedErasedCallablePointer(
            self.rootMaterializationContext(),
            bytes,
            relocations,
            base_offset,
            erased.sealed,
        );
    }

    fn writeTuple(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        schemas: []const CheckedArtifact.ComptimeSchemaId,
        value: CheckedArtifact.ComptimeValue,
        tuple_layout_idx: layout_mod.Idx,
    ) MaterializationError!void {
        const values = switch (value) {
            .tuple => |values| values,
            else => staticDataInvariant("static tuple export has non-tuple value"),
        };
        if (schemas.len != values.len) staticDataInvariant("static tuple schema/value length mismatch");
        if (schemas.len == 0) return;

        const tuple_layout = self.layout_store.getLayout(tuple_layout_idx);
        if (tuple_layout.tag == .zst) return;
        if (tuple_layout.tag != .struct_) staticDataInvariant("static tuple schema did not lower to struct layout");

        for (schemas, 0..) |schema, i| {
            const field_layout_idx = self.layout_store.getStructFieldLayoutByOriginalIndex(tuple_layout.data.struct_.idx, @intCast(i));
            const field_layout = self.layout_store.getLayout(field_layout_idx);
            const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(tuple_layout.data.struct_.idx, @intCast(i));
            if (self.layout_store.layoutSize(field_layout) == 0) continue;
            try self.writeValue(bytes, relocations, base_offset + field_offset, schema, values[i], field_layout_idx);
        }
    }

    fn writeRecord(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        fields_schema: []const CheckedArtifact.ComptimeFieldSchema,
        value: CheckedArtifact.ComptimeValue,
        record_layout_idx: layout_mod.Idx,
    ) MaterializationError!void {
        const values = switch (value) {
            .record => |values| values,
            else => staticDataInvariant("static record export has non-record value"),
        };
        if (fields_schema.len != values.len) staticDataInvariant("static record schema/value length mismatch");
        if (fields_schema.len == 0) return;

        const record_layout = self.layout_store.getLayout(record_layout_idx);
        if (record_layout.tag == .zst) return;
        if (record_layout.tag != .struct_) staticDataInvariant("static record schema did not lower to struct layout");

        for (fields_schema, 0..) |field_schema, i| {
            const field_layout_idx = self.layout_store.getStructFieldLayoutByOriginalIndex(record_layout.data.struct_.idx, @intCast(i));
            const field_layout = self.layout_store.getLayout(field_layout_idx);
            const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(record_layout.data.struct_.idx, @intCast(i));
            if (self.layout_store.layoutSize(field_layout) == 0) continue;
            try self.writeValue(bytes, relocations, base_offset + field_offset, field_schema.schema, values[i], field_layout_idx);
        }
    }

    fn writeTagUnion(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        variants_schema: []const CheckedArtifact.ComptimeVariantSchema,
        value: CheckedArtifact.ComptimeValue,
        tag_union_layout_idx: layout_mod.Idx,
    ) MaterializationError!void {
        const tag_value = switch (value) {
            .tag_union => |tag| tag,
            else => staticDataInvariant("static tag-union export has non-tag value"),
        };
        if (tag_value.variant_index >= variants_schema.len) staticDataInvariant("static tag-union variant index out of range");

        const sorted = try self.sortedTagVariants(variants_schema);
        defer self.allocator.free(sorted);

        const active_sorted_index = sortedIndexForOriginal(sorted, tag_value.variant_index);
        const active_variant = variants_schema[tag_value.variant_index];
        if (active_variant.payloads.len != tag_value.payloads.len) staticDataInvariant("static tag payload length mismatch");

        const tag_layout = self.layout_store.getLayout(tag_union_layout_idx);
        if (tag_layout.tag == .zst) return;
        if (tag_layout.tag != .tag_union) staticDataInvariant("static tag union schema did not lower to tag-union layout");

        const tag_info = self.layout_store.getTagUnionInfo(tag_layout);
        const active_payload_layout_idx = tag_info.variants.get(@intCast(active_sorted_index)).payload_layout;
        for (active_variant.payloads, 0..) |payload_schema, payload_i| {
            const payload_layout_idx = payloadLayoutForTagArg(
                &self.layout_store,
                active_payload_layout_idx,
                active_variant.payloads.len,
                @intCast(payload_i),
            );
            const payload_layout = self.layout_store.getLayout(payload_layout_idx);
            if (self.layout_store.layoutSize(payload_layout) == 0) continue;
            const payload_offset = payloadOffsetForTagArg(
                &self.layout_store,
                active_payload_layout_idx,
                active_variant.payloads.len,
                @intCast(payload_i),
            );
            try self.writeValue(
                bytes,
                relocations,
                base_offset + payload_offset,
                payload_schema,
                tag_value.payloads[payload_i],
                payload_layout_idx,
            );
        }

        const tag_data = self.layout_store.getTagUnionData(tag_layout.data.tag_union.idx);
        if (tag_data.discriminant_size != 0) {
            self.writeDiscriminant(
                bytes,
                base_offset + tag_data.discriminant_offset,
                tag_data.discriminant_size,
                active_sorted_index,
            );
        }
    }

    fn materializeValueWithLayout(
        self: *StaticDataBuilder,
        schema_id: CheckedArtifact.ComptimeSchemaId,
        value_id: CheckedArtifact.ComptimeValueId,
        layout_idx: layout_mod.Idx,
    ) MaterializationError!MaterializedValue {
        const layout_info = self.layoutInfo(layout_idx);
        const bytes = try self.allocator.alloc(u8, layout_info.size);
        @memset(bytes, 0);

        var relocations = std.ArrayList(StaticDataRelocation).empty;
        errdefer {
            deinitRelocationList(self.allocator, &relocations);
            self.allocator.free(bytes);
        }

        try self.writeValue(bytes, &relocations, 0, schema_id, value_id, layout_idx);

        return .{
            .bytes = bytes,
            .alignment = layout_info.alignment,
            .relocations = try relocations.toOwnedSlice(self.allocator),
        };
    }

    fn addStaticAllocation(
        self: *StaticDataBuilder,
        payload: []u8,
        payload_alignment: u32,
        contains_refcounted: bool,
        list_element_count: ?usize,
    ) MaterializationError!PointerTarget {
        const relocations = try self.allocator.alloc(StaticDataRelocation, 0);
        return try self.addStaticAllocationWithRelocs(payload, payload_alignment, contains_refcounted, list_element_count, relocations);
    }

    fn addStaticAllocationWithRelocs(
        self: *StaticDataBuilder,
        payload: []u8,
        payload_alignment: u32,
        contains_refcounted: bool,
        list_element_count: ?usize,
        payload_relocations: []StaticDataRelocation,
    ) MaterializationError!PointerTarget {
        var payload_owned = true;
        var payload_relocations_owned = true;
        errdefer {
            if (payload_owned) self.allocator.free(payload);
            if (payload_relocations_owned) {
                deinitRelocationSlice(self.allocator, payload_relocations);
                self.allocator.free(payload_relocations);
            }
        }

        const symbol_name = try std.fmt.allocPrint(
            self.allocator,
            "roc__static_const_{d}",
            .{self.local_symbol_ordinal},
        );
        self.local_symbol_ordinal += 1;
        errdefer self.allocator.free(symbol_name);

        const data_offset = staticDataPtrOffset(self.word_size, payload_alignment, contains_refcounted);
        const total_size = data_offset + payload.len;
        const bytes = try self.allocator.alloc(u8, total_size);
        errdefer self.allocator.free(bytes);
        @memset(bytes, 0);
        @memcpy(bytes[data_offset..][0..payload.len], payload);
        self.allocator.free(payload);
        payload_owned = false;

        if (contains_refcounted) {
            self.writeWord(bytes, data_offset - self.word_size * 2, list_element_count orelse 0);
        }
        self.writeSignedWord(bytes, data_offset - self.word_size, 0);

        const relocations = try self.allocator.alloc(StaticDataRelocation, payload_relocations.len);
        errdefer {
            deinitRelocationSlice(self.allocator, relocations);
            self.allocator.free(relocations);
        }
        for (payload_relocations, 0..) |rel, i| {
            relocations[i] = .{
                .offset = data_offset + rel.offset,
                .target_symbol_name = rel.target_symbol_name,
                .addend = rel.addend,
                .owns_target_symbol_name = rel.owns_target_symbol_name,
            };
        }
        self.allocator.free(payload_relocations);
        payload_relocations_owned = false;

        try self.nodes.append(self.allocator, .{
            .symbol_name = symbol_name,
            .bytes = bytes,
            .alignment = @max(payload_alignment, self.word_size),
            .is_global = false,
            .relocations = relocations,
        });

        return .{
            .symbol_name = symbol_name,
            .addend = @intCast(data_offset),
        };
    }

    fn layoutForSchema(self: *StaticDataBuilder, schema_id: CheckedArtifact.ComptimeSchemaId) MaterializationError!ValueLayout {
        const layout_idx = try self.layoutIdxForSchema(schema_id);
        return self.layoutInfo(layout_idx);
    }

    fn layoutInfo(self: *StaticDataBuilder, layout_idx: layout_mod.Idx) ValueLayout {
        const layout = self.layout_store.getLayout(layout_idx);
        return .{
            .idx = layout_idx,
            .size = self.layout_store.layoutSize(layout),
            .alignment = @intCast(layout.alignment(self.target_usize).toByteUnits()),
            .contains_refcounted = self.layout_store.layoutContainsRefcounted(layout),
        };
    }

    fn layoutIdxForSchema(
        self: *StaticDataBuilder,
        schema_id: CheckedArtifact.ComptimeSchemaId,
    ) MaterializationError!layout_mod.Idx {
        const schema = self.comptimeSchema(schema_id);
        return switch (schema) {
            .pending => staticDataInvariant("static data layout requested for pending schema"),
            .zst => self.layout_store.ensureZstLayout(),
            .int => |precision| self.layout_store.insertLayout(layout_mod.Layout.int(precision)),
            .frac => |precision| self.layout_store.insertLayout(layout_mod.Layout.frac(precision)),
            .str => self.layout_store.insertLayout(layout_mod.Layout.str()),
            .callable => self.layout_store.insertErasedCallable(),
            .list => |elem_schema| blk: {
                const elem_idx = try self.layoutIdxForSchema(elem_schema);
                const elem_layout = self.layout_store.getLayout(elem_idx);
                if (self.layout_store.layoutSize(elem_layout) == 0) {
                    break :blk self.layout_store.insertLayout(layout_mod.Layout.listOfZst());
                }
                break :blk self.layout_store.insertList(elem_idx);
            },
            .box => |payload_schema| blk: {
                const payload_idx = try self.layoutIdxForSchema(payload_schema);
                const payload_layout = self.layout_store.getLayout(payload_idx);
                if (self.layout_store.layoutSize(payload_layout) == 0) {
                    break :blk self.layout_store.insertLayout(layout_mod.Layout.boxOfZst());
                }
                if (payload_layout.tag == .erased_callable) break :blk payload_idx;
                break :blk self.layout_store.insertBox(payload_idx);
            },
            .tuple => |items| self.layoutIdxForTuple(items),
            .record => |fields| self.layoutIdxForRecord(fields),
            .tag_union => |variants| self.layoutIdxForTagUnion(variants),
            .alias => |wrapped| self.layoutIdxForSchema(wrapped.backing),
            .nominal => |wrapped| self.layoutIdxForSchema(wrapped.backing),
        };
    }

    fn layoutIdxForTuple(
        self: *StaticDataBuilder,
        items: []const CheckedArtifact.ComptimeSchemaId,
    ) MaterializationError!layout_mod.Idx {
        if (items.len == 0) return self.layout_store.ensureZstLayout();
        const fields = try self.allocator.alloc(layout_mod.StructField, items.len);
        defer self.allocator.free(fields);
        for (items, 0..) |item, i| {
            fields[i] = .{
                .index = @intCast(i),
                .layout = try self.layoutIdxForSchema(item),
            };
        }
        return self.layout_store.putStructFields(fields);
    }

    fn layoutIdxForRecord(
        self: *StaticDataBuilder,
        fields_schema: []const CheckedArtifact.ComptimeFieldSchema,
    ) MaterializationError!layout_mod.Idx {
        if (fields_schema.len == 0) return self.layout_store.ensureZstLayout();
        const fields = try self.allocator.alloc(layout_mod.StructField, fields_schema.len);
        defer self.allocator.free(fields);
        for (fields_schema, 0..) |field_schema, i| {
            fields[i] = .{
                .index = @intCast(i),
                .layout = try self.layoutIdxForSchema(field_schema.schema),
            };
        }
        return self.layout_store.putStructFields(fields);
    }

    fn layoutIdxForTagUnion(
        self: *StaticDataBuilder,
        variants_schema: []const CheckedArtifact.ComptimeVariantSchema,
    ) MaterializationError!layout_mod.Idx {
        if (variants_schema.len == 0) staticDataInvariant("static tag union has no variants");
        const sorted = try self.sortedTagVariants(variants_schema);
        defer self.allocator.free(sorted);

        const payload_layouts = try self.allocator.alloc(layout_mod.Idx, sorted.len);
        defer self.allocator.free(payload_layouts);
        for (sorted, 0..) |variant, i| {
            payload_layouts[i] = try self.layoutIdxForVariantPayload(variant.payloads);
        }

        return self.layout_store.putTagUnion(payload_layouts);
    }

    fn layoutIdxForVariantPayload(
        self: *StaticDataBuilder,
        payloads: []const CheckedArtifact.ComptimeSchemaId,
    ) MaterializationError!layout_mod.Idx {
        return switch (payloads.len) {
            0 => self.layout_store.ensureZstLayout(),
            1 => self.layoutIdxForSchema(payloads[0]),
            else => self.layoutIdxForTuple(payloads),
        };
    }

    const SortedVariant = struct {
        original_index: u32,
        name: canonical.TagLabelId,
        payloads: []const CheckedArtifact.ComptimeSchemaId,
    };

    fn sortedTagVariants(
        self: *StaticDataBuilder,
        variants_schema: []const CheckedArtifact.ComptimeVariantSchema,
    ) MaterializationError![]SortedVariant {
        const sorted = try self.allocator.alloc(SortedVariant, variants_schema.len);
        errdefer self.allocator.free(sorted);
        for (variants_schema, 0..) |variant, i| {
            sorted[i] = .{
                .original_index = @intCast(i),
                .name = variant.name,
                .payloads = variant.payloads,
            };
        }
        std.mem.sort(SortedVariant, sorted, self, tagVariantLessThan);
        return sorted;
    }

    fn tagVariantLessThan(self: *StaticDataBuilder, lhs: SortedVariant, rhs: SortedVariant) bool {
        return self.artifact.canonical_names.tagLabelTextLessThan(lhs.name, rhs.name);
    }

    fn comptimeSchema(self: *const StaticDataBuilder, id: CheckedArtifact.ComptimeSchemaId) CheckedArtifact.ComptimeSchema {
        const idx = @intFromEnum(id);
        if (idx >= self.artifact.comptime_values.schemas.items.len) staticDataInvariant("static data schema id out of range");
        return self.artifact.comptime_values.schemas.items[idx];
    }

    fn comptimeValue(self: *const StaticDataBuilder, id: CheckedArtifact.ComptimeValueId) CheckedArtifact.ComptimeValue {
        const idx = @intFromEnum(id);
        if (idx >= self.artifact.comptime_values.values.items.len) staticDataInvariant("static data value id out of range");
        return self.artifact.comptime_values.values.items[idx];
    }

    fn rootMaterializationContext(self: *const StaticDataBuilder) MaterializationContext {
        return .{
            .owner = self.artifact.key,
            .canonical_names = &self.artifact.canonical_names,
            .plans = &self.artifact.comptime_plans,
            .values = &self.artifact.comptime_values,
            .executable_type_payloads = &self.artifact.executable_type_payloads,
            .callable_set_descriptors = &self.artifact.callable_set_descriptors,
            .const_instances = self.artifact.const_instances.view(),
        };
    }

    fn materializationContextForArtifact(
        self: *const StaticDataBuilder,
        owner: CheckedArtifact.CheckedModuleArtifactKey,
    ) MaterializationContext {
        if (artifactKeyEql(self.artifact.key, owner)) return self.rootMaterializationContext();
        if (self.artifact_views.root) |root| {
            for (root.relation_artifacts) |related| {
                if (!artifactKeyEql(related.key, owner)) continue;
                return materializationContextFromImportedView(related);
            }
        }
        for (self.artifact_views.imports) |imported| {
            if (!artifactKeyEql(imported.key, owner)) continue;
            return materializationContextFromImportedView(imported);
        }
        staticDataInvariant("static data materialization referenced an unavailable artifact view");
    }

    fn materializationContextForArtifactRef(
        self: *const StaticDataBuilder,
        ref: canonical.ArtifactRef,
    ) MaterializationContext {
        return self.materializationContextForArtifact(.{ .bytes = ref.bytes });
    }

    fn materializeErasedCapturePlan(
        self: *StaticDataBuilder,
        materialization: MaterializationContext,
        plan: CheckedArtifact.ErasedCaptureExecutableMaterializationPlan,
        expected_key: canonical.CanonicalExecValueTypeKey,
    ) MaterializationError!MaterializedValue {
        const lowered = self.lowered orelse {
            staticDataInvariant("static boxed erased callable capture requires lowered requested layout table");
        };
        const layout_idx = lowered.lir_result.requestedLayoutForKey(expected_key) orelse {
            staticDataInvariant("static boxed erased callable capture type has no requested target layout");
        };
        const layout_info = layoutInfoFromStore(&lowered.lir_result.layouts, self.target_usize, layout_idx);
        if (layout_info.alignment > builtins.erased_callable.capture_alignment) {
            staticDataInvariant("static boxed erased callable capture alignment exceeds erased callable ABI alignment");
        }

        const bytes = try self.allocator.alloc(u8, layout_info.size);
        @memset(bytes, 0);

        var relocations = std.ArrayList(StaticDataRelocation).empty;
        errdefer {
            deinitRelocationList(self.allocator, &relocations);
            self.allocator.free(bytes);
        }

        try self.writeErasedCapturePlan(
            materialization,
            bytes,
            &relocations,
            0,
            plan,
            expected_key,
            &lowered.lir_result.layouts,
            layout_idx,
        );

        return .{
            .bytes = bytes,
            .alignment = layout_info.alignment,
            .relocations = try relocations.toOwnedSlice(self.allocator),
        };
    }

    fn writeErasedCapturePlan(
        self: *StaticDataBuilder,
        materialization: MaterializationContext,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        plan: CheckedArtifact.ErasedCaptureExecutableMaterializationPlan,
        expected_key: canonical.CanonicalExecValueTypeKey,
        layouts: *const layout_mod.Store,
        layout_idx: layout_mod.Idx,
    ) MaterializationError!void {
        switch (plan) {
            .none => staticDataInvariant("static erased capture materialization expected value but got none"),
            .zero_sized_typed => |zero_key| {
                if (!repr.canonicalExecValueTypeKeyEql(expected_key, zero_key)) {
                    staticDataInvariant("static erased capture zero-sized key differs from expected type");
                }
                const layout = layouts.getLayout(layout_idx);
                if (layouts.layoutSize(layout) != 0) {
                    staticDataInvariant("static erased capture zero-sized materialization has nonzero layout");
                }
            },
            .node => |node| try self.writeErasedCaptureNode(
                materialization,
                bytes,
                relocations,
                base_offset,
                node,
                expected_key,
                layouts,
                layout_idx,
            ),
        }
    }

    fn writeErasedCaptureNode(
        self: *StaticDataBuilder,
        materialization: MaterializationContext,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        node_id: CheckedArtifact.ErasedCaptureExecutableMaterializationNodeId,
        expected_key: canonical.CanonicalExecValueTypeKey,
        layouts: *const layout_mod.Store,
        layout_idx: layout_mod.Idx,
    ) MaterializationError!void {
        const node = materialization.plans.erasedCaptureExecutableMaterializationNode(node_id);
        switch (node) {
            .pending => staticDataInvariant("static erased capture materialization reached pending node"),
            .const_instance => |const_instance| try self.writeConstInstanceCapture(
                bytes,
                relocations,
                base_offset,
                const_instance,
                layouts,
                layout_idx,
            ),
            .pure_const => |pure_const| try self.writeConstInstanceCapture(
                bytes,
                relocations,
                base_offset,
                pure_const.const_instance,
                layouts,
                layout_idx,
            ),
            .pure_value => |pure_value| try self.writePureValueCapture(
                materialization,
                bytes,
                relocations,
                base_offset,
                pure_value.schema,
                pure_value.value,
                layouts,
                layout_idx,
            ),
            .finite_callable_set => |finite| try self.writeFiniteCallableSetCapture(
                materialization,
                bytes,
                relocations,
                base_offset,
                finite,
                expected_key,
                layouts,
                layout_idx,
            ),
            .erased_callable => |erased| try self.writeMaterializedErasedCallableCapture(
                materialization,
                bytes,
                relocations,
                base_offset,
                erased,
                expected_key,
                layouts,
                layout_idx,
            ),
            .record => |fields| try self.writeRecordCapture(
                materialization,
                bytes,
                relocations,
                base_offset,
                fields,
                expected_key,
                layouts,
                layout_idx,
            ),
            .tuple => |items| try self.writeTupleCapture(
                materialization,
                bytes,
                relocations,
                base_offset,
                items,
                expected_key,
                layouts,
                layout_idx,
            ),
            .tag_union => |tag| try self.writeTagCapture(
                materialization,
                bytes,
                relocations,
                base_offset,
                tag,
                expected_key,
                layouts,
                layout_idx,
            ),
            .list => |items| try self.writeListCapture(
                materialization,
                bytes,
                relocations,
                base_offset,
                items,
                expected_key,
                layouts,
                layout_idx,
            ),
            .box => |payload| try self.writeBoxCapture(
                materialization,
                bytes,
                relocations,
                base_offset,
                payload,
                expected_key,
                layouts,
                layout_idx,
            ),
            .nominal => |nominal| try self.writeNominalCapture(
                materialization,
                bytes,
                relocations,
                base_offset,
                nominal,
                expected_key,
                layouts,
                layout_idx,
            ),
            .recursive_ref => |ref| try self.writeErasedCaptureNode(
                materialization,
                bytes,
                relocations,
                base_offset,
                ref,
                expected_key,
                layouts,
                layout_idx,
            ),
        }
    }

    fn writeConstInstanceCapture(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        const_instance: CheckedArtifact.ConstInstanceRef,
        layouts: *const layout_mod.Store,
        layout_idx: layout_mod.Idx,
    ) MaterializationError!void {
        const resolved = self.resolveConstInstance(const_instance);
        try self.writePureValueCapture(
            resolved.materialization,
            bytes,
            relocations,
            base_offset,
            resolved.instance.schema,
            resolved.instance.value,
            layouts,
            layout_idx,
        );
    }

    fn writePureValueCapture(
        self: *StaticDataBuilder,
        materialization: MaterializationContext,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        schema: CheckedArtifact.ComptimeSchemaId,
        value: CheckedArtifact.ComptimeValueId,
        layouts: *const layout_mod.Store,
        layout_idx: layout_mod.Idx,
    ) MaterializationError!void {
        const expected = layoutInfoFromStore(layouts, self.target_usize, layout_idx);
        if (!artifactKeyEql(materialization.owner, self.artifact.key)) {
            staticDataInvariant("static erased capture pure value currently requires root-owned materialization values");
        }
        const materialized = try self.materializeValueWithLayout(schema, value, try self.layoutIdxForSchema(schema));
        defer self.deinitMaterialized(materialized);
        if (materialized.bytes.len != expected.size or materialized.alignment != expected.alignment) {
            staticDataInvariant("static erased capture pure value layout differs from expected executable capture layout");
        }
        self.writeBytes(bytes, base_offset, materialized.bytes);
        try self.appendRelocationsWithOffset(relocations, materialized.relocations, base_offset);
    }

    fn writeFiniteCallableSetCapture(
        self: *StaticDataBuilder,
        materialization: MaterializationContext,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        finite: CheckedArtifact.MaterializedFiniteCallableSetValue,
        expected_key: canonical.CanonicalExecValueTypeKey,
        layouts: *const layout_mod.Store,
        layout_idx: layout_mod.Idx,
    ) MaterializationError!void {
        const payload_context = self.executablePayloadForKey(materialization, expected_key);
        const callable_set = switch (payload_context.payload) {
            .callable_set => |callable_set| callable_set,
            else => staticDataInvariant("static erased capture finite callable set expected callable-set executable type"),
        };
        if (!repr.callableSetKeyEql(callable_set.key, finite.callable_set_key)) {
            staticDataInvariant("static erased capture finite callable set key differs from expected type");
        }

        const member_index: usize = @intFromEnum(finite.selected_member);
        const selected_member = findCallableSetMemberPayload(callable_set.members, finite.selected_member) orelse {
            staticDataInvariant("static erased capture finite callable set selected member missing from expected type");
        };
        const layout = layouts.getLayout(layout_idx);
        if (layout.tag != .tag_union) {
            if (layouts.layoutSize(layout) == 0 and callable_set.members.len == 1 and finite.captures.len == 0) return;
            staticDataInvariant("static erased capture finite callable set did not lower to tag union layout");
        }
        const tag_info = layouts.getTagUnionInfo(layout);
        if (member_index >= tag_info.variants.len) {
            staticDataInvariant("static erased capture finite callable set selected member exceeds layout variants");
        }
        const payload_layout_idx = tag_info.variants.get(@intCast(member_index)).payload_layout;
        if (finite.captures.len == 0) {
            if (selected_member.payload_ty != null) {
                staticDataInvariant("static erased capture finite callable set has no captures but member expects payload");
            }
        } else {
            const payload_ref = selected_member.payload_ty orelse {
                staticDataInvariant("static erased capture finite callable set has captures but member has no payload type");
            };
            const payload_key = selected_member.payload_ty_key orelse {
                staticDataInvariant("static erased capture finite callable set member payload has no key");
            };
            const tuple_context = self.executablePayloadForRef(payload_ref);
            const tuple = switch (tuple_context.payload) {
                .tuple => |tuple| tuple,
                else => staticDataInvariant("static erased capture finite callable set payload was not a tuple"),
            };
            if (!repr.canonicalExecValueTypeKeyEql(tuple_context.materialization.executable_type_payloads.keyFor(payload_ref.payload), payload_key)) {
                staticDataInvariant("static erased capture finite callable set payload key differs from expected member payload key");
            }
            try self.writeCaptureTupleItems(
                materialization,
                bytes,
                relocations,
                base_offset,
                finite.captures,
                tuple,
                layouts,
                payload_layout_idx,
            );
        }

        const tag_data = layouts.getTagUnionData(layout.data.tag_union.idx);
        if (tag_data.discriminant_size != 0) {
            self.writeDiscriminant(
                bytes,
                base_offset + tag_data.discriminant_offset,
                tag_data.discriminant_size,
                @intCast(member_index),
            );
        }
    }

    fn writeMaterializedErasedCallableCapture(
        self: *StaticDataBuilder,
        materialization: MaterializationContext,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        erased: CheckedArtifact.MaterializedErasedCallableValue,
        expected_key: canonical.CanonicalExecValueTypeKey,
        layouts: *const layout_mod.Store,
        layout_idx: layout_mod.Idx,
    ) MaterializationError!void {
        const payload_context = self.executablePayloadForKey(materialization, expected_key);
        const erased_fn = switch (payload_context.payload) {
            .erased_fn => |erased_fn| erased_fn,
            else => staticDataInvariant("static erased callable materialization expected erased-fn executable type"),
        };
        if (!repr.erasedFnSigKeyEql(erased_fn.sig_key, erased.sealed.boundary.sig_key)) {
            staticDataInvariant("static erased callable materialization signature differs from expected type");
        }
        if (layouts.getLayout(layout_idx).tag != .erased_callable) {
            staticDataInvariant("static erased callable materialization did not lower to erased callable layout");
        }
        try self.writeSealedErasedCallablePointer(materialization, bytes, relocations, base_offset, erased.sealed);
    }

    fn writeRecordCapture(
        self: *StaticDataBuilder,
        materialization: MaterializationContext,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        fields: []const CheckedArtifact.ErasedCaptureExecutableMaterializationRecordField,
        expected_key: canonical.CanonicalExecValueTypeKey,
        layouts: *const layout_mod.Store,
        layout_idx: layout_mod.Idx,
    ) MaterializationError!void {
        const payload_context = self.executablePayloadForKey(materialization, expected_key);
        const expected_fields = switch (payload_context.payload) {
            .record => |record| record,
            else => staticDataInvariant("static erased capture record expected record executable type"),
        };
        if (fields.len != expected_fields.len) {
            staticDataInvariant("static erased capture record field count differs from expected type");
        }
        const layout = layouts.getLayout(layout_idx);
        if (layout.tag == .zst) return;
        if (layout.tag != .struct_) staticDataInvariant("static erased capture record did not lower to struct layout");

        const seen = try self.allocator.alloc(bool, fields.len);
        defer self.allocator.free(seen);
        @memset(seen, false);

        for (expected_fields, 0..) |field, logical_i| {
            const materialized = findMaterializedRecordField(materialization, fields, payload_context.materialization, field.field, seen) orelse {
                staticDataInvariant("static erased capture record missing expected field");
            };
            const field_layout_idx = layouts.getStructFieldLayoutByOriginalIndex(layout.data.struct_.idx, @intCast(logical_i));
            const field_layout = layouts.getLayout(field_layout_idx);
            if (layouts.layoutSize(field_layout) == 0) continue;
            const field_offset = layouts.getStructFieldOffsetByOriginalIndex(layout.data.struct_.idx, @intCast(logical_i));
            try self.writeErasedCapturePlan(
                materialization,
                bytes,
                relocations,
                base_offset + field_offset,
                materialized.value,
                field.key,
                layouts,
                field_layout_idx,
            );
        }
        verifyAllSeen(seen, "static erased capture record had extra field");
    }

    fn writeTupleCapture(
        self: *StaticDataBuilder,
        materialization: MaterializationContext,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        items: []const CheckedArtifact.ErasedCaptureExecutableMaterializationPlan,
        expected_key: canonical.CanonicalExecValueTypeKey,
        layouts: *const layout_mod.Store,
        layout_idx: layout_mod.Idx,
    ) MaterializationError!void {
        const payload_context = self.executablePayloadForKey(materialization, expected_key);
        const tuple = switch (payload_context.payload) {
            .tuple => |tuple| tuple,
            else => staticDataInvariant("static erased capture tuple expected tuple executable type"),
        };
        try self.writeCaptureTupleItems(materialization, bytes, relocations, base_offset, items, tuple, layouts, layout_idx);
    }

    fn writeCaptureTupleItems(
        self: *StaticDataBuilder,
        materialization: MaterializationContext,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        items: []const CheckedArtifact.ErasedCaptureExecutableMaterializationPlan,
        tuple: []const CheckedArtifact.ExecutableTupleElemPayload,
        layouts: *const layout_mod.Store,
        layout_idx: layout_mod.Idx,
    ) MaterializationError!void {
        if (items.len != tuple.len) {
            staticDataInvariant("static erased capture tuple arity differs from expected type");
        }
        const layout = layouts.getLayout(layout_idx);
        if (layout.tag == .zst) return;
        if (layout.tag != .struct_) staticDataInvariant("static erased capture tuple did not lower to struct layout");

        var seen = try self.allocator.alloc(bool, items.len);
        defer self.allocator.free(seen);
        @memset(seen, false);

        for (tuple) |item| {
            const index: usize = @intCast(item.index);
            if (index >= items.len) staticDataInvariant("static erased capture tuple item index out of range");
            if (seen[index]) staticDataInvariant("static erased capture tuple duplicated item index");
            seen[index] = true;
            const field_layout_idx = layouts.getStructFieldLayoutByOriginalIndex(layout.data.struct_.idx, @intCast(index));
            const field_layout = layouts.getLayout(field_layout_idx);
            if (layouts.layoutSize(field_layout) == 0) continue;
            const field_offset = layouts.getStructFieldOffsetByOriginalIndex(layout.data.struct_.idx, @intCast(index));
            try self.writeErasedCapturePlan(
                materialization,
                bytes,
                relocations,
                base_offset + field_offset,
                items[index],
                item.key,
                layouts,
                field_layout_idx,
            );
        }
        verifyAllSeen(seen, "static erased capture tuple missing item");
    }

    fn writeTagCapture(
        self: *StaticDataBuilder,
        materialization: MaterializationContext,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        tag: CheckedArtifact.ErasedCaptureExecutableMaterializationTagNode,
        expected_key: canonical.CanonicalExecValueTypeKey,
        layouts: *const layout_mod.Store,
        layout_idx: layout_mod.Idx,
    ) MaterializationError!void {
        const payload_context = self.executablePayloadForKey(materialization, expected_key);
        const variants = switch (payload_context.payload) {
            .tag_union => |variants| variants,
            else => staticDataInvariant("static erased capture tag expected tag-union executable type"),
        };
        const selected = findExecutableTagVariant(materialization, tag.tag, payload_context.materialization, variants) orelse {
            staticDataInvariant("static erased capture tag missing from expected type");
        };
        if (tag.payloads.len != selected.variant.payloads.len) {
            staticDataInvariant("static erased capture tag payload count differs from expected type");
        }

        const layout = layouts.getLayout(layout_idx);
        if (layout.tag == .zst) return;
        if (layout.tag != .tag_union) staticDataInvariant("static erased capture tag did not lower to tag-union layout");

        const tag_info = layouts.getTagUnionInfo(layout);
        if (selected.index >= tag_info.variants.len) {
            staticDataInvariant("static erased capture tag index exceeds layout variants");
        }
        const active_payload_layout_idx = tag_info.variants.get(@intCast(selected.index)).payload_layout;

        const seen = try self.allocator.alloc(bool, tag.payloads.len);
        defer self.allocator.free(seen);
        @memset(seen, false);

        for (selected.variant.payloads) |expected_payload| {
            const materialized = findTagPayload(tag.payloads, expected_payload.index, seen) orelse {
                staticDataInvariant("static erased capture tag missing expected payload");
            };
            const payload_layout_idx = payloadLayoutForTagArg(layouts, active_payload_layout_idx, selected.variant.payloads.len, expected_payload.index);
            const payload_layout = layouts.getLayout(payload_layout_idx);
            if (layouts.layoutSize(payload_layout) == 0) continue;
            const payload_offset = payloadOffsetForTagArg(layouts, active_payload_layout_idx, selected.variant.payloads.len, expected_payload.index);
            try self.writeErasedCapturePlan(
                materialization,
                bytes,
                relocations,
                base_offset + payload_offset,
                materialized.value,
                expected_payload.key,
                layouts,
                payload_layout_idx,
            );
        }
        verifyAllSeen(seen, "static erased capture tag had extra payload");

        const tag_data = layouts.getTagUnionData(layout.data.tag_union.idx);
        if (tag_data.discriminant_size != 0) {
            self.writeDiscriminant(bytes, base_offset + tag_data.discriminant_offset, tag_data.discriminant_size, selected.index);
        }
    }

    fn writeListCapture(
        self: *StaticDataBuilder,
        materialization: MaterializationContext,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        items: []const CheckedArtifact.ErasedCaptureExecutableMaterializationPlan,
        expected_key: canonical.CanonicalExecValueTypeKey,
        layouts: *const layout_mod.Store,
        layout_idx: layout_mod.Idx,
    ) MaterializationError!void {
        const payload_context = self.executablePayloadForKey(materialization, expected_key);
        const list_child = switch (payload_context.payload) {
            .list => |list| list,
            else => staticDataInvariant("static erased capture list expected list executable type"),
        };
        if (items.len == 0) {
            self.writeWord(bytes, base_offset, 0);
            self.writeWord(bytes, base_offset + self.word_size, 0);
            self.writeWord(bytes, base_offset + self.word_size * 2, 0);
            return;
        }

        const list_layout = layouts.getLayout(layout_idx);
        const abi = layouts.builtinListAbi(layout_idx);
        const payload_size = @as(usize, abi.elem_size) * items.len;
        const payload = try self.allocator.alloc(u8, payload_size);
        @memset(payload, 0);

        var payload_relocs = std.ArrayList(StaticDataRelocation).empty;
        var payload_consumed = false;
        errdefer {
            if (!payload_consumed) {
                deinitRelocationList(self.allocator, &payload_relocs);
                self.allocator.free(payload);
            }
        }

        if (abi.elem_size != 0) {
            const elem_layout_idx = switch (list_layout.tag) {
                .list => list_layout.data.list,
                .list_of_zst => layout_mod.Idx.zst,
                else => staticDataInvariant("static erased capture list did not lower to list layout"),
            };
            for (items, 0..) |item, i| {
                try self.writeErasedCapturePlan(
                    materialization,
                    payload,
                    &payload_relocs,
                    @as(u32, @intCast(i * abi.elem_size)),
                    item,
                    list_child.key,
                    layouts,
                    elem_layout_idx,
                );
            }
        }

        const payload_relocations = try payload_relocs.toOwnedSlice(self.allocator);
        errdefer if (!payload_consumed) self.allocator.free(payload_relocations);
        const target = try self.addStaticAllocationWithRelocs(
            payload,
            abi.elem_alignment,
            abi.contains_refcounted,
            if (abi.contains_refcounted) items.len else null,
            payload_relocations,
        );
        payload_consumed = true;
        try self.writePointerRelocation(bytes, relocations, base_offset, target.symbol_name, target.addend);
        self.writeWord(bytes, base_offset + self.word_size, items.len);
        self.writeWord(bytes, base_offset + self.word_size * 2, items.len);
    }

    fn writeBoxCapture(
        self: *StaticDataBuilder,
        materialization: MaterializationContext,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        payload: CheckedArtifact.ErasedCaptureExecutableMaterializationPlan,
        expected_key: canonical.CanonicalExecValueTypeKey,
        layouts: *const layout_mod.Store,
        layout_idx: layout_mod.Idx,
    ) MaterializationError!void {
        const payload_context = self.executablePayloadForKey(materialization, expected_key);
        const box_child = switch (payload_context.payload) {
            .box => |box| box,
            else => staticDataInvariant("static erased capture box expected box executable type"),
        };
        const layout = layouts.getLayout(layout_idx);
        if (layout.tag == .box_of_zst) {
            self.writeWord(bytes, base_offset, 0);
            return;
        }
        if (layout.tag == .erased_callable) {
            try self.writeErasedCapturePlan(materialization, bytes, relocations, base_offset, payload, box_child.key, layouts, layout_idx);
            return;
        }
        if (layout.tag != .box) staticDataInvariant("static erased capture box did not lower to box layout");

        const abi = layouts.builtinBoxAbi(layout_idx);
        const payload_value = try self.materializeErasedCapturePlan(materialization, payload, box_child.key);
        defer self.deinitMaterialized(payload_value);
        if (payload_value.bytes.len != abi.elem_size or payload_value.alignment != abi.elem_alignment) {
            staticDataInvariant("static erased capture box payload layout differs from expected box ABI");
        }

        const target = try self.addStaticAllocationWithRelocs(
            try self.allocator.dupe(u8, payload_value.bytes),
            abi.elem_alignment,
            abi.contains_refcounted,
            null,
            try self.cloneRelocations(payload_value.relocations),
        );
        try self.writePointerRelocation(bytes, relocations, base_offset, target.symbol_name, target.addend);
    }

    fn writeNominalCapture(
        self: *StaticDataBuilder,
        materialization: MaterializationContext,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        nominal: anytype,
        expected_key: canonical.CanonicalExecValueTypeKey,
        layouts: *const layout_mod.Store,
        layout_idx: layout_mod.Idx,
    ) MaterializationError!void {
        const payload_context = self.executablePayloadForKey(materialization, expected_key);
        const expected_nominal = switch (payload_context.payload) {
            .nominal => |nominal_payload| nominal_payload,
            else => staticDataInvariant("static erased capture nominal expected nominal executable type"),
        };
        if (!nominalKeyTextEql(materialization, nominal.nominal, payload_context.materialization, expected_nominal.nominal)) {
            staticDataInvariant("static erased capture nominal identity differs from expected type");
        }
        try self.writeErasedCapturePlan(materialization, bytes, relocations, base_offset, nominal.backing, expected_nominal.backing_key, layouts, layout_idx);
    }

    fn writeSealedErasedCallablePointer(
        self: *StaticDataBuilder,
        materialization: MaterializationContext,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        sealed: CheckedArtifact.SealedErasedCallableValue,
    ) MaterializationError!void {
        const capture: ?MaterializedValue = switch (sealed.capture) {
            .none => blk: {
                if (sealed.boundary.sig_key.capture_ty != null) {
                    staticDataInvariant("static boxed erased callable has hidden capture type but no capture materialization");
                }
                break :blk null;
            },
            .zero_sized_typed => |capture_key| blk: {
                const expected = sealed.boundary.sig_key.capture_ty orelse {
                    staticDataInvariant("static boxed erased callable has zero-sized capture but no hidden capture type");
                };
                if (!repr.canonicalExecValueTypeKeyEql(expected, capture_key)) {
                    staticDataInvariant("static boxed erased callable zero-sized capture type differs from hidden capture type");
                }
                break :blk try self.materializeErasedCapturePlan(materialization, sealed.capture, expected);
            },
            .node => blk: {
                const expected = sealed.boundary.sig_key.capture_ty orelse {
                    staticDataInvariant("static boxed erased callable has capture materialization node but no hidden capture type");
                };
                break :blk try self.materializeErasedCapturePlan(materialization, sealed.capture, expected);
            },
        };
        defer if (capture) |materialized| self.deinitMaterialized(materialized);

        const proc_symbol_name = try self.procSymbolNameForSealedErasedCallableCode(sealed.code);
        var proc_symbol_owned = true;
        errdefer if (proc_symbol_owned) self.allocator.free(proc_symbol_name);

        const capture_size = if (capture) |materialized| materialized.bytes.len else 0;
        const payload_size = erasedCallablePayloadSize(self.word_size, capture_size);
        const payload = try self.allocator.alloc(u8, payload_size);
        @memset(payload, 0);
        var payload_owned = true;
        var payload_relocs = std.ArrayList(StaticDataRelocation).empty;
        var payload_relocs_owned = true;
        errdefer {
            if (payload_owned) self.allocator.free(payload);
            if (payload_relocs_owned) deinitRelocationList(self.allocator, &payload_relocs);
        }

        try self.writePointerRelocationWithOwnership(payload, &payload_relocs, 0, proc_symbol_name, 0, true);
        proc_symbol_owned = false;
        self.writeWord(payload, self.word_size, 0);
        if (capture) |materialized| {
            const capture_offset = erasedCallableCaptureOffset(self.word_size);
            self.writeBytes(payload, capture_offset, materialized.bytes);
            try self.appendRelocationsWithOffset(&payload_relocs, materialized.relocations, capture_offset);
        }

        const payload_relocations = try payload_relocs.toOwnedSlice(self.allocator);
        payload_relocs_owned = false;
        var payload_relocations_owned = true;
        errdefer if (payload_relocations_owned) {
            deinitRelocationSlice(self.allocator, payload_relocations);
            self.allocator.free(payload_relocations);
        };

        const target = try self.addStaticAllocationWithRelocs(
            payload,
            builtins.erased_callable.payload_alignment,
            builtins.erased_callable.allocation_has_refcounted_children,
            null,
            payload_relocations,
        );
        payload_owned = false;
        payload_relocations_owned = false;
        try self.writePointerRelocation(bytes, relocations, base_offset, target.symbol_name, target.addend);
    }

    const ResolvedConstInstance = struct {
        materialization: MaterializationContext,
        instance: CheckedArtifact.ConstInstance,
    };

    fn resolveConstInstance(
        self: *const StaticDataBuilder,
        ref: CheckedArtifact.ConstInstanceRef,
    ) ResolvedConstInstance {
        const materialization = self.materializationContextForArtifact(ref.owner);
        if (!artifactKeyEql(materialization.const_instances.owner, ref.owner)) {
            staticDataInvariant("static const instance view has wrong owning artifact");
        }
        const index: usize = @intFromEnum(ref.instance);
        if (index >= materialization.const_instances.instances.len) {
            staticDataInvariant("static const instance ref out of range");
        }
        const record = materialization.const_instances.instances[index];
        return .{
            .materialization = materialization,
            .instance = switch (record.state) {
                .evaluated => |instance| instance,
                .reserved, .evaluating => staticDataInvariant("static const instance was consumed before being sealed"),
            },
        };
    }

    fn executablePayloadForKey(
        self: *const StaticDataBuilder,
        preferred: MaterializationContext,
        key: canonical.CanonicalExecValueTypeKey,
    ) ExecutablePayloadContext {
        const preferred_ref = preferred.executable_type_payloads.refForKey(artifactRefFromKey(preferred.owner), key);
        if (preferred_ref) |ref| return self.executablePayloadForRef(ref);
        if (self.artifact.executable_type_payloads.refForKey(artifactRefFromKey(self.artifact.key), key)) |ref| {
            return self.executablePayloadForRef(ref);
        }
        if (self.artifact_views.root) |root| {
            for (root.relation_artifacts) |related| {
                if (related.executable_type_payloads.refForKey(artifactRefFromKey(related.key), key)) |ref| {
                    return self.executablePayloadForRef(ref);
                }
            }
        }
        for (self.artifact_views.imports) |imported| {
            if (imported.executable_type_payloads.refForKey(artifactRefFromKey(imported.key), key)) |ref| {
                return self.executablePayloadForRef(ref);
            }
        }
        staticDataInvariant("static data materialization could not find executable payload for key");
    }

    fn executablePayloadForRef(
        self: *const StaticDataBuilder,
        ref: CheckedArtifact.ExecutableTypePayloadRef,
    ) ExecutablePayloadContext {
        const materialization = self.materializationContextForArtifactRef(ref.artifact);
        return .{
            .materialization = materialization,
            .payload = materialization.executable_type_payloads.get(ref.payload),
        };
    }

    fn findMaterializedRecordField(
        materialization: MaterializationContext,
        fields: []const CheckedArtifact.ErasedCaptureExecutableMaterializationRecordField,
        expected_context: MaterializationContext,
        expected_label: canonical.RecordFieldLabelId,
        seen: []bool,
    ) ?CheckedArtifact.ErasedCaptureExecutableMaterializationRecordField {
        const expected_text = expected_context.canonical_names.recordFieldLabelText(expected_label);
        for (fields, 0..) |field, i| {
            const actual_text = materialization.canonical_names.recordFieldLabelText(field.field);
            if (!std.mem.eql(u8, actual_text, expected_text)) continue;
            if (seen[i]) staticDataInvariant("static erased capture record duplicated field");
            seen[i] = true;
            return field;
        }
        return null;
    }

    const SelectedTagVariant = struct {
        index: u32,
        variant: CheckedArtifact.ExecutableTagVariantPayload,
    };

    fn findExecutableTagVariant(
        materialization: MaterializationContext,
        actual_tag: canonical.TagLabelId,
        expected_context: MaterializationContext,
        variants: []const CheckedArtifact.ExecutableTagVariantPayload,
    ) ?SelectedTagVariant {
        const actual_text = materialization.canonical_names.tagLabelText(actual_tag);
        for (variants, 0..) |variant, i| {
            const expected_text = expected_context.canonical_names.tagLabelText(variant.tag);
            if (!std.mem.eql(u8, actual_text, expected_text)) continue;
            return .{
                .index = @intCast(i),
                .variant = variant,
            };
        }
        return null;
    }

    fn appendRelocationsWithOffset(
        self: *StaticDataBuilder,
        dest: *std.ArrayList(StaticDataRelocation),
        source: []const StaticDataRelocation,
        base_offset: u32,
    ) Allocator.Error!void {
        try dest.ensureUnusedCapacity(self.allocator, source.len);
        for (source) |relocation| {
            var target_name = relocation.target_symbol_name;
            var owns_target_symbol_name = false;
            if (relocation.owns_target_symbol_name) {
                target_name = try self.allocator.dupe(u8, relocation.target_symbol_name);
                owns_target_symbol_name = true;
            }
            errdefer if (owns_target_symbol_name) self.allocator.free(target_name);
            dest.appendAssumeCapacity(.{
                .offset = base_offset + relocation.offset,
                .target_symbol_name = target_name,
                .addend = relocation.addend,
                .owns_target_symbol_name = owns_target_symbol_name,
            });
        }
    }

    fn cloneRelocations(
        self: *StaticDataBuilder,
        source: []const StaticDataRelocation,
    ) Allocator.Error![]StaticDataRelocation {
        const cloned = try self.allocator.alloc(StaticDataRelocation, source.len);
        var initialized: usize = 0;
        errdefer {
            deinitRelocationSlice(self.allocator, cloned[0..initialized]);
            self.allocator.free(cloned);
        }
        for (source, 0..) |relocation, i| {
            var target_name = relocation.target_symbol_name;
            var owns_target_symbol_name = false;
            if (relocation.owns_target_symbol_name) {
                target_name = try self.allocator.dupe(u8, relocation.target_symbol_name);
                owns_target_symbol_name = true;
            }
            cloned[i] = .{
                .offset = relocation.offset,
                .target_symbol_name = target_name,
                .addend = relocation.addend,
                .owns_target_symbol_name = owns_target_symbol_name,
            };
            initialized += 1;
        }
        return cloned;
    }

    fn procSymbolNameForSealedErasedCallableCode(
        self: *StaticDataBuilder,
        code: CheckedArtifact.SealedErasedCallableCode,
    ) MaterializationError![]u8 {
        const lowered = self.lowered orelse {
            staticDataInvariant("static boxed erased callable export requires lowered erased-callable code map");
        };
        for (lowered.erased_callable_code_map) |entry| {
            if (!sealedErasedCallableCodeMatchesLowered(code, entry.code)) continue;
            const proc_spec = lowered.lir_result.store.getProcSpec(entry.lir_proc);
            return try backend.procSymbolName(self.allocator, proc_spec.name);
        }
        staticDataInvariant("static boxed erased callable export referenced erased callable code that was not lowered");
    }

    fn sealedErasedCallableCodeMatchesLowered(
        sealed: CheckedArtifact.SealedErasedCallableCode,
        lowered: canonical.ErasedCallableCodeRef,
    ) bool {
        return switch (sealed) {
            .direct_proc => |sealed_direct| switch (lowered) {
                .direct_proc_value => |lowered_direct| erasedDirectProcCodeRefEql(sealed_direct.code, lowered_direct),
                .finite_set_adapter => false,
            },
            .finite_adapter => |sealed_finite| switch (lowered) {
                .direct_proc_value => false,
                .finite_set_adapter => |lowered_adapter| repr.erasedAdapterKeyEql(sealed_finite.adapter_key, lowered_adapter),
            },
        };
    }

    fn erasedDirectProcCodeRefEql(
        a: canonical.ErasedDirectProcCodeRef,
        b: canonical.ErasedDirectProcCodeRef,
    ) bool {
        return canonical.procedureCallableRefEql(a.proc_value, b.proc_value) and
            repr.captureShapeKeyEql(a.capture_shape_key, b.capture_shape_key);
    }

    fn writePointerRelocation(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        offset: u32,
        target_symbol_name: []const u8,
        addend: i64,
    ) Allocator.Error!void {
        try self.writePointerRelocationWithOwnership(bytes, relocations, offset, target_symbol_name, addend, false);
    }

    fn writePointerRelocationWithOwnership(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        offset: u32,
        target_symbol_name: []const u8,
        addend: i64,
        owns_target_symbol_name: bool,
    ) Allocator.Error!void {
        self.writeWord(bytes, offset, 0);
        try relocations.append(self.allocator, .{
            .offset = offset,
            .target_symbol_name = target_symbol_name,
            .addend = addend,
            .owns_target_symbol_name = owns_target_symbol_name,
        });
    }

    fn writeBytes(_: *StaticDataBuilder, bytes: []u8, offset: u32, source: []const u8) void {
        @memcpy(bytes[offset..][0..source.len], source);
    }

    fn writeWord(self: *StaticDataBuilder, bytes: []u8, offset: u32, value: usize) void {
        switch (self.word_size) {
            4 => std.mem.writeInt(u32, bytes[offset..][0..4], @intCast(value), .little),
            8 => std.mem.writeInt(u64, bytes[offset..][0..8], @intCast(value), .little),
            else => unreachable,
        }
    }

    fn writeSignedWord(self: *StaticDataBuilder, bytes: []u8, offset: u32, value: isize) void {
        switch (self.word_size) {
            4 => std.mem.writeInt(i32, bytes[offset..][0..4], @intCast(value), .little),
            8 => std.mem.writeInt(i64, bytes[offset..][0..8], @intCast(value), .little),
            else => unreachable,
        }
    }

    fn writeDiscriminant(_: *StaticDataBuilder, bytes: []u8, offset: u32, size: u8, value: u32) void {
        switch (size) {
            0 => {},
            1 => bytes[offset] = @intCast(value),
            2 => std.mem.writeInt(u16, bytes[offset..][0..2], @intCast(value), .little),
            4 => std.mem.writeInt(u32, bytes[offset..][0..4], value, .little),
            8 => std.mem.writeInt(u64, bytes[offset..][0..8], value, .little),
            else => unreachable,
        }
    }
};

fn materializationContextFromImportedView(view: CheckedArtifact.ImportedModuleView) MaterializationContext {
    return .{
        .owner = view.key,
        .canonical_names = view.canonical_names,
        .plans = view.comptime_plans,
        .values = view.comptime_values,
        .executable_type_payloads = view.executable_type_payloads,
        .callable_set_descriptors = view.callable_set_descriptors,
        .const_instances = view.const_instances,
    };
}

fn artifactRefFromKey(key: CheckedArtifact.CheckedModuleArtifactKey) canonical.ArtifactRef {
    return .{ .bytes = key.bytes };
}

fn artifactKeyEql(a: CheckedArtifact.CheckedModuleArtifactKey, b: CheckedArtifact.CheckedModuleArtifactKey) bool {
    return std.mem.eql(u8, &a.bytes, &b.bytes);
}

fn findCallableSetMemberPayload(
    members: []const CheckedArtifact.ExecutableCallableSetMemberPayload,
    selected: canonical.CallableSetMemberId,
) ?CheckedArtifact.ExecutableCallableSetMemberPayload {
    for (members) |member| {
        if (member.member == selected) return member;
    }
    return null;
}

fn findTagPayload(
    payloads: []const CheckedArtifact.ErasedCaptureExecutableMaterializationTagPayload,
    expected_index: u32,
    seen: []bool,
) ?CheckedArtifact.ErasedCaptureExecutableMaterializationTagPayload {
    for (payloads, 0..) |payload, i| {
        if (payload.index != expected_index) continue;
        if (seen[i]) staticDataInvariant("static erased capture tag duplicated payload");
        seen[i] = true;
        return payload;
    }
    return null;
}

fn nominalKeyTextEql(
    actual_context: MaterializationContext,
    actual: canonical.NominalTypeKey,
    expected_context: MaterializationContext,
    expected: canonical.NominalTypeKey,
) bool {
    return std.mem.eql(
        u8,
        actual_context.canonical_names.moduleNameText(actual.module_name),
        expected_context.canonical_names.moduleNameText(expected.module_name),
    ) and std.mem.eql(
        u8,
        actual_context.canonical_names.typeNameText(actual.type_name),
        expected_context.canonical_names.typeNameText(expected.type_name),
    );
}

fn verifyAllSeen(seen: []const bool, comptime message: []const u8) void {
    for (seen) |was_seen| {
        if (!was_seen) staticDataInvariant(message);
    }
}

fn layoutInfoFromStore(
    layouts: *const layout_mod.Store,
    target_usize: base.target.TargetUsize,
    layout_idx: layout_mod.Idx,
) ValueLayout {
    const layout = layouts.getLayout(layout_idx);
    return .{
        .idx = layout_idx,
        .size = layouts.layoutSize(layout),
        .alignment = @intCast(layout.alignment(target_usize).toByteUnits()),
        .contains_refcounted = layouts.layoutContainsRefcounted(layout),
    };
}

fn payloadLayoutForTagArg(
    layouts: *const layout_mod.Store,
    variant_layout_idx: layout_mod.Idx,
    arg_count: usize,
    arg_index: u32,
) layout_mod.Idx {
    if (arg_count == 0) return layout_mod.Idx.zst;
    const variant_layout = layouts.getLayout(variant_layout_idx);
    if (arg_count == 1) {
        if (variant_layout.tag == .struct_ and layouts.getStructInfo(variant_layout).fields.len == 1) {
            return layouts.getStructFieldLayoutByOriginalIndex(variant_layout.data.struct_.idx, 0);
        }
        return variant_layout_idx;
    }
    if (variant_layout.tag != .struct_) staticDataInvariant("multi-payload tag did not use struct payload layout");
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
    if (variant_layout.tag != .struct_) staticDataInvariant("multi-payload tag did not use struct payload layout");
    return layouts.getStructFieldOffsetByOriginalIndex(variant_layout.data.struct_.idx, arg_index);
}

fn sortedIndexForOriginal(sorted: []const StaticDataBuilder.SortedVariant, original: u32) u32 {
    for (sorted, 0..) |variant, i| {
        if (variant.original_index == original) return @intCast(i);
    }
    staticDataInvariant("tag-union variant missing from sorted static layout");
}

fn staticDataPtrOffset(word_size: u32, element_alignment: u32, contains_refcounted: bool) u32 {
    const required_space = if (contains_refcounted) word_size * 2 else word_size;
    return alignForwardU32(required_space, element_alignment);
}

fn erasedCallablePayloadSize(word_size: u32, capture_size: usize) usize {
    return @as(usize, erasedCallableCaptureOffset(word_size)) + capture_size;
}

fn erasedCallableCaptureOffset(word_size: u32) u32 {
    return alignForwardU32(word_size * 2, builtins.erased_callable.capture_alignment);
}

fn alignForwardU32(value: u32, alignment: u32) u32 {
    std.debug.assert(alignment != 0);
    return @intCast(std.mem.alignForward(usize, value, alignment));
}

fn targetUsizeForTarget(target: roc_target.RocTarget) ?base.target.TargetUsize {
    return switch (target.ptrBitWidth()) {
        32 => .u32,
        64 => .u64,
        else => null,
    };
}

fn staticDataInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("static data export invariant violated: " ++ message, .{});
    }
    unreachable;
}
