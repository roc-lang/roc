//! Target-layout readonly data symbols for provided non-function constants.
//!
//! This module turns explicit checked-artifact constant data into object-file
//! readonly symbols and relocations. It does not inspect source syntax and does
//! not create runtime initializer procedures.

const std = @import("std");
const Allocator = std.mem.Allocator;

const backend = @import("backend");
const base = @import("base");
const check = @import("check");
const layout_mod = @import("layout");
const roc_target = @import("roc_target");
const types = @import("types");

const CheckedArtifact = check.CheckedArtifact;
const canonical = check.CanonicalNames;

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

/// Build every host-visible provided data export for a target.
pub fn buildProvidedDataExports(
    allocator: Allocator,
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    target: roc_target.RocTarget,
) MaterializationError![]StaticDataExport {
    var builder = try StaticDataBuilder.init(allocator, artifact, target);
    defer builder.deinit();
    return try builder.build();
}

/// Free a static-data graph returned by `buildProvidedDataExports`.
pub fn deinitProvidedDataExports(allocator: Allocator, exports: []StaticDataExport) void {
    for (exports) |static_export| {
        allocator.free(static_export.symbol_name);
        allocator.free(static_export.bytes);
        allocator.free(static_export.relocations);
    }
    allocator.free(exports);
}

const StaticDataBuilder = struct {
    allocator: Allocator,
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    target: roc_target.RocTarget,
    target_usize: base.target.TargetUsize,
    word_size: u32,
    layout_store: layout_mod.Store,
    nodes: std.ArrayList(StaticDataExport),
    local_symbol_ordinal: u32,

    fn init(
        allocator: Allocator,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        target: roc_target.RocTarget,
    ) MaterializationError!StaticDataBuilder {
        const target_usize = targetUsizeForTarget(target) orelse return error.UnsupportedTarget;
        return .{
            .allocator = allocator,
            .artifact = artifact,
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
            self.allocator.free(node.relocations);
        }
        self.nodes.deinit(self.allocator);
    }

    fn deinitMaterialized(self: *StaticDataBuilder, value: MaterializedValue) void {
        self.allocator.free(value.bytes);
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
            relocations.deinit(self.allocator);
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
                payload_relocs.deinit(self.allocator);
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
            relocations.deinit(self.allocator);
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
            if (payload_relocations_owned) self.allocator.free(payload_relocations);
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
        errdefer self.allocator.free(relocations);
        for (payload_relocations, 0..) |rel, i| {
            relocations[i] = .{
                .offset = data_offset + rel.offset,
                .target_symbol_name = rel.target_symbol_name,
                .addend = rel.addend,
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

    fn writePointerRelocation(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        offset: u32,
        target_symbol_name: []const u8,
        addend: i64,
    ) Allocator.Error!void {
        self.writeWord(bytes, offset, 0);
        try relocations.append(self.allocator, .{
            .offset = offset,
            .target_symbol_name = target_symbol_name,
            .addend = addend,
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
