//! Target-layout readonly data symbols for provided non-function constants.
//!
//! Static data exports are materialized from checked `ConstStore` nodes and the
//! layout/const plans output by direct LIR lowering.

const std = @import("std");

const backend = @import("backend");
const builtins = @import("builtins");
const check = @import("check");
const layout = @import("layout");
const lir = @import("lir");
const roc_target = @import("roc_target");

const Allocator = std.mem.Allocator;
const CanonicalNameStore = check.CanonicalNames.CanonicalNameStore;
const Checked = check.CheckedArtifact;
const CheckedModule = check.CheckedModule;
const ConstFn = check.ConstStore.ConstFn;
const ConstStrDataId = check.ConstStore.ConstStrDataId;
const ConstValue = CheckedModule.ConstValue;
const StaticDataExport = backend.StaticDataExport;
const StaticDataRelocation = backend.StaticDataRelocation;

/// Checked modules whose provided data exports can become static data.
pub const ModuleViews = struct {
    root: ?Checked.LoweringModuleView = null,
    imports: []const Checked.ImportedModuleView = &.{},
};

const MaterializationError = Allocator.Error || error{
    UnsupportedTarget,
};

const PointerTarget = struct {
    symbol_name: []const u8,
    addend: i64,
};

const MaterializedValue = struct {
    bytes: []u8,
    alignment: u32,
    relocations: []StaticDataRelocation,
};

const ConstModule = struct {
    key: CheckedModule.CheckedModuleArtifactKey,
    names: *const CanonicalNameStore,
    templates: *const CheckedModule.ConstTemplateTable,
    store: *const CheckedModule.ConstStore,
};

const ConstNode = struct {
    module: ConstModule,
    id: CheckedModule.ConstNodeId,
};

const ConstStrDataSite = struct {
    store_address: usize,
    data: u32,
};

/// Build readonly data exports for provided constants.
pub fn buildProvidedDataExports(
    allocator: Allocator,
    modules: ModuleViews,
    lowered: ?*const lir.CheckedPipeline.LoweredProgram,
    target: roc_target.RocTarget,
) MaterializationError![]StaticDataExport {
    const root = modules.root orelse {
        if (hasProvidedData(modules)) staticDataInvariant("provided data exports require a root checked module");
        return try allocator.alloc(StaticDataExport, 0);
    };
    const lowered_program = lowered orelse {
        if (moduleHasProvidedData(root.module)) staticDataInvariant("provided data exports require LIR layout output");
        return try allocator.alloc(StaticDataExport, 0);
    };

    var builder = StaticDataBuilder.init(allocator, root, modules.imports, lowered_program, target) orelse
        return error.UnsupportedTarget;
    defer builder.deinitScratch();
    return try builder.build();
}

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
    root: Checked.LoweringModuleView,
    imports: []const Checked.ImportedModuleView,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    target_usize: @import("base").target.TargetUsize,
    word_size: u32,
    nodes: std.ArrayList(StaticDataExport),
    str_allocations: std.AutoHashMap(ConstStrDataSite, PointerTarget),
    local_symbol_ordinal: u32,

    fn init(
        allocator: Allocator,
        root: Checked.LoweringModuleView,
        imports: []const Checked.ImportedModuleView,
        lowered: *const lir.CheckedPipeline.LoweredProgram,
        target: roc_target.RocTarget,
    ) ?StaticDataBuilder {
        const target_usize = switch (target.ptrBitWidth()) {
            32 => @import("base").target.TargetUsize.u32,
            64 => @import("base").target.TargetUsize.u64,
            else => return null,
        };
        return .{
            .allocator = allocator,
            .root = root,
            .imports = imports,
            .lowered = lowered,
            .target_usize = target_usize,
            .word_size = target_usize.size(),
            .nodes = .empty,
            .str_allocations = std.AutoHashMap(ConstStrDataSite, PointerTarget).init(allocator),
            .local_symbol_ordinal = 0,
        };
    }

    fn deinitScratch(self: *StaticDataBuilder) void {
        self.str_allocations.deinit();
    }

    fn build(self: *StaticDataBuilder) MaterializationError![]StaticDataExport {
        errdefer self.deinitNodes();

        for (self.root.module.provided_exports.exports) |provided| {
            const data = switch (provided) {
                .data => |data| data,
                .procedure => continue,
            };

            const const_node = self.constNode(data.const_ref);
            const request = self.requestedLayout(data.checked_type);
            const entrypoint_name = self.root.module.canonical_names.externalSymbolNameText(data.ffi_symbol);
            const symbol_name = try self.allocator.dupe(u8, entrypoint_name);
            errdefer self.allocator.free(symbol_name);

            const materialized = try self.materializeValue(const_node, request.plan, request.layout_idx);
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

    fn constNode(self: *StaticDataBuilder, ref: CheckedModule.ConstRef) ConstNode {
        const module = self.moduleForConst(ref);
        const template = module.templates.get(ref);
        return switch (template.state) {
            .stored_const => |stored| .{ .module = module, .id = stored.node },
            .reserved,
            .eval_template,
            => staticDataInvariant("provided data export const was not stored before static materialization"),
        };
    }

    fn moduleForConst(self: *StaticDataBuilder, ref: CheckedModule.ConstRef) ConstModule {
        if (moduleBytesEqual(self.root.module.key.bytes, ref.artifact.bytes)) return .{
            .key = self.root.module.key,
            .names = &self.root.module.canonical_names,
            .templates = &self.root.module.const_templates,
            .store = &self.root.module.const_store,
        };
        for (self.imports) |imported| {
            if (moduleBytesEqual(imported.key.bytes, ref.artifact.bytes)) return .{
                .key = imported.key,
                .names = imported.canonical_names,
                .templates = imported.const_templates,
                .store = imported.const_store,
            };
        }
        staticDataInvariant("provided data export referenced a const outside the lowering module set");
    }

    fn requestedLayout(self: *StaticDataBuilder, checked_type: CheckedModule.CheckedTypeId) lir.Program.RequestedLayout {
        for (self.lowered.lir_result.requested_layouts.items) |request| {
            if (request.checked_type == checked_type) return request;
        }
        staticDataInvariant("provided data export had no LIR layout request");
    }

    fn materializeValue(
        self: *StaticDataBuilder,
        node: ConstNode,
        plan: lir.Program.ConstPlanId,
        layout_idx: layout.Idx,
    ) MaterializationError!MaterializedValue {
        return try self.materializeRawValue(node.module, node.module.store.get(node.id), plan, layout_idx);
    }

    fn materializeRawValue(
        self: *StaticDataBuilder,
        source: ConstModule,
        value: ConstValue,
        plan: lir.Program.ConstPlanId,
        layout_idx: layout.Idx,
    ) MaterializationError!MaterializedValue {
        const layout_value = self.layoutValue(layout_idx);
        const bytes = try self.allocator.alloc(u8, self.layouts().layoutSize(layout_value));
        @memset(bytes, 0);

        var relocations = std.ArrayList(StaticDataRelocation).empty;
        errdefer {
            deinitRelocationList(self.allocator, &relocations);
            self.allocator.free(bytes);
        }

        try self.writeValue(bytes, &relocations, 0, source, value, plan, layout_idx);

        return .{
            .bytes = bytes,
            .alignment = @intCast(layout_value.alignment(self.target_usize).toByteUnits()),
            .relocations = try relocations.toOwnedSlice(self.allocator),
        };
    }

    fn writeValue(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        source: ConstModule,
        value: ConstValue,
        plan_id: lir.Program.ConstPlanId,
        layout_idx: layout.Idx,
    ) MaterializationError!void {
        const plan = self.constPlan(plan_id);
        switch (plan) {
            .pending => staticDataInvariant("pending const plan reached static data export"),
            .zst => switch (value) {
                .zst => {},
                else => staticDataInvariant("ZST const plan received non-ZST ConstStore node"),
            },
            .scalar => self.writeScalar(bytes, base_offset, value, layout_idx),
            .str => try self.writeStr(bytes, relocations, base_offset, source, value),
            .list => |elem_plan| try self.writeList(bytes, relocations, base_offset, source, value, elem_plan, layout_idx),
            .box => |payload_plan| try self.writeBox(bytes, relocations, base_offset, source, value, payload_plan, layout_idx),
            .tuple => |items| try self.writeTuple(bytes, relocations, base_offset, source, value, plan_id, items, layout_idx),
            .record => |fields| try self.writeRecord(bytes, relocations, base_offset, source, value, plan_id, fields, layout_idx),
            .tag_union => |variants| try self.writeTagUnion(bytes, relocations, base_offset, source, value, plan_id, variants, layout_idx),
            .named => |named| {
                const backing = switch (value) {
                    .nominal => |nominal| nominal.backing,
                    else => staticDataInvariant("named const plan received non-nominal ConstStore node"),
                };
                try self.writeValue(bytes, relocations, base_offset, source, source.store.get(backing), named.backing, layout_idx);
            },
            .fn_value => staticDataInvariant("provided function-valued data export reached finite callable static materialization"),
            .erased_fn => |set| try self.writeErasedFn(bytes, relocations, base_offset, source, value, set, layout_idx),
        }
    }

    fn writeBoxedSemanticValue(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        source: ConstModule,
        value: ConstValue,
        plan_id: lir.Program.ConstPlanId,
        layout_idx: layout.Idx,
    ) MaterializationError!bool {
        const layout_value = self.layoutValue(layout_idx);
        if (layout_value.tag == .box_of_zst) {
            self.writeTargetWord(bytes, base_offset, 0);
            return true;
        }
        if (layout_value.tag != .box) return false;

        const abi = self.layouts().builtinBoxAbi(layout_idx);
        const payload = try self.materializeRawValue(source, value, plan_id, abi.elem_layout_idx orelse layout.Idx.zst);
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
        return true;
    }

    fn writeScalar(self: *StaticDataBuilder, bytes: []u8, base_offset: u32, value: ConstValue, layout_idx: layout.Idx) void {
        const scalar = switch (value) {
            .scalar => |scalar| scalar,
            else => staticDataInvariant("scalar const plan received non-scalar ConstStore node"),
        };
        const layout_value = self.layoutValue(layout_idx);
        if (layout_value.tag != .scalar) staticDataInvariant("scalar const plan had non-scalar layout");
        const info = self.layouts().getScalarInfo(layout_value);
        switch (info.tag) {
            .str,
            .opaque_ptr,
            => staticDataInvariant("unsupported scalar layout reached scalar static data export"),
            .int => switch (info.int_precision orelse unreachable) {
                .u8 => writeInt(u8, bytes, base_offset, scalar, .u8),
                .i8 => writeInt(i8, bytes, base_offset, scalar, .i8),
                .u16 => writeInt(u16, bytes, base_offset, scalar, .u16),
                .i16 => writeInt(i16, bytes, base_offset, scalar, .i16),
                .u32 => writeInt(u32, bytes, base_offset, scalar, .u32),
                .i32 => writeInt(i32, bytes, base_offset, scalar, .i32),
                .u64 => writeInt(u64, bytes, base_offset, scalar, .u64),
                .i64 => writeInt(i64, bytes, base_offset, scalar, .i64),
                .u128 => writeInt(u128, bytes, base_offset, scalar, .u128),
                .i128 => writeInt(i128, bytes, base_offset, scalar, .i128),
            },
            .frac => switch (info.frac_precision orelse unreachable) {
                .f32 => writeInt(u32, bytes, base_offset, scalar, .f32_bits),
                .f64 => writeInt(u64, bytes, base_offset, scalar, .f64_bits),
                .dec => writeInt(i128, bytes, base_offset, scalar, .dec_bits),
            },
        }
    }

    fn writeInt(
        comptime T: type,
        bytes: []u8,
        base_offset: u32,
        scalar: CheckedModule.ConstScalar,
        comptime field: std.meta.FieldEnum(CheckedModule.ConstScalar),
    ) void {
        const value = switch (field) {
            inline else => |tag| switch (scalar) {
                tag => |value| value,
                else => staticDataInvariant("scalar ConstStore node precision differed from scalar layout"),
            },
        };
        std.mem.writeInt(T, bytes[base_offset..][0..@sizeOf(T)], value, .little);
    }

    fn writeStr(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        source: ConstModule,
        value: ConstValue,
    ) MaterializationError!void {
        const str = switch (value) {
            .str => |str| str,
            else => staticDataInvariant("Str const plan received non-Str ConstStore node"),
        };
        const str_bytes = source.store.strBytes(str);
        const backing = source.store.strData(str.data);
        const whole_backing = str.offset == 0 and @as(usize, str.len) == backing.len;
        const roc_str_size = self.word_size * 3;
        if (backing.len < roc_str_size and str_bytes.len < roc_str_size) {
            self.writeBytes(bytes, base_offset, str_bytes);
            bytes[base_offset + roc_str_size - 1] = @as(u8, @intCast(str_bytes.len)) | 0x80;
            return;
        }

        const target = try self.staticStrAllocation(source, str.data);
        try self.writePointerRelocation(
            bytes,
            relocations,
            base_offset,
            target.symbol_name,
            target.addend + @as(i64, str.offset),
        );

        if (whole_backing) {
            self.writeTargetWord(bytes, base_offset + self.word_size, self.encodeRocStrCapacity(str_bytes.len));
        } else {
            try self.writePointerRelocation(
                bytes,
                relocations,
                base_offset + self.word_size,
                target.symbol_name,
                target.addend + 1,
            );
        }
        self.writeTargetWord(bytes, base_offset + self.word_size * 2, @intCast(str_bytes.len));
    }

    fn staticStrAllocation(
        self: *StaticDataBuilder,
        source: ConstModule,
        data: ConstStrDataId,
    ) MaterializationError!PointerTarget {
        const site: ConstStrDataSite = .{
            .store_address = @intFromPtr(source.store),
            .data = @intFromEnum(data),
        };
        if (self.str_allocations.get(site)) |target| return target;

        const str_bytes = source.store.strData(data);
        const payload = try self.allocator.dupe(u8, str_bytes);
        const target = try self.addStaticAllocation(payload, self.word_size, false, null);
        try self.str_allocations.put(site, target);
        return target;
    }

    fn writeList(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        source: ConstModule,
        value: ConstValue,
        elem_plan: lir.Program.ConstPlanId,
        list_layout_idx: layout.Idx,
    ) MaterializationError!void {
        const items = switch (value) {
            .list => |items| items,
            else => staticDataInvariant("List const plan received non-list ConstStore node"),
        };
        self.writeTargetWord(bytes, base_offset, 0);
        self.writeTargetWord(bytes, base_offset + self.word_size, @intCast(items.len));
        self.writeTargetWord(bytes, base_offset + self.word_size * 2, self.encodeRocListCapacity(items.len));
        if (items.len == 0) return;

        const list_layout = self.layoutValue(list_layout_idx);
        const abi = self.layouts().builtinListAbi(list_layout_idx);
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
                .list => list_layout.getIdx(),
                .list_of_zst => layout.Idx.zst,
                else => staticDataInvariant("List const plan had non-list layout"),
            };
            for (items, 0..) |item, i| {
                try self.writeValue(
                    payload,
                    &payload_relocs,
                    @as(u32, @intCast(i * abi.elem_size)),
                    source,
                    source.store.get(item),
                    elem_plan,
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
    }

    fn writeBox(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        source: ConstModule,
        value: ConstValue,
        payload_plan: lir.Program.ConstPlanId,
        box_layout_idx: layout.Idx,
    ) MaterializationError!void {
        const payload_node = switch (value) {
            .box => |payload| payload,
            else => staticDataInvariant("Box const plan received non-box ConstStore node"),
        };
        const box_layout = self.layoutValue(box_layout_idx);
        if (box_layout.tag == .box_of_zst) {
            self.writeTargetWord(bytes, base_offset, 0);
            return;
        }
        if (box_layout.tag == .erased_callable) {
            try self.writeValue(bytes, relocations, base_offset, source, source.store.get(payload_node), payload_plan, box_layout_idx);
            return;
        }
        if (box_layout.tag != .box) staticDataInvariant("Box const plan had non-box layout");

        const abi = self.layouts().builtinBoxAbi(box_layout_idx);
        const payload = try self.materializeValue(.{ .module = source, .id = payload_node }, payload_plan, abi.elem_layout_idx orelse layout.Idx.zst);
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
        source: ConstModule,
        value: ConstValue,
        plan_id: lir.Program.ConstPlanId,
        item_plans: []const lir.Program.ConstPlanId,
        tuple_layout_idx: layout.Idx,
    ) MaterializationError!void {
        const items = switch (value) {
            .tuple => |items| items,
            else => staticDataInvariant("tuple const plan received non-tuple ConstStore node"),
        };
        if (item_plans.len != items.len) staticDataInvariant("tuple const plan length differed from ConstStore node");
        if (try self.writeBoxedSemanticValue(bytes, relocations, base_offset, source, value, plan_id, tuple_layout_idx)) return;
        const tuple_layout = self.layoutValue(tuple_layout_idx);
        if (tuple_layout.tag == .zst) return;
        if (tuple_layout.tag != .struct_) staticDataInvariant("tuple const plan had non-struct layout");

        for (item_plans, 0..) |item_plan, i| {
            const field_layout_idx = self.layouts().getStructFieldLayoutByOriginalIndex(tuple_layout.getStruct().idx, @intCast(i));
            const field_layout = self.layoutValue(field_layout_idx);
            if (self.layouts().layoutSize(field_layout) == 0) continue;
            const field_offset = self.layouts().getStructFieldOffsetByOriginalIndex(tuple_layout.getStruct().idx, @intCast(i));
            try self.writeValue(bytes, relocations, base_offset + field_offset, source, source.store.get(items[i]), item_plan, field_layout_idx);
        }
    }

    fn writeRecord(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        source: ConstModule,
        value: ConstValue,
        plan_id: lir.Program.ConstPlanId,
        field_plans: []const lir.Program.ConstPlanId,
        record_layout_idx: layout.Idx,
    ) MaterializationError!void {
        const fields = switch (value) {
            .record => |fields| fields,
            else => staticDataInvariant("record const plan received non-record ConstStore node"),
        };
        if (field_plans.len != fields.len) staticDataInvariant("record const plan length differed from ConstStore node");
        if (try self.writeBoxedSemanticValue(bytes, relocations, base_offset, source, value, plan_id, record_layout_idx)) return;
        const record_layout = self.layoutValue(record_layout_idx);
        if (record_layout.tag == .zst) return;
        if (record_layout.tag != .struct_) staticDataInvariant("record const plan had non-struct layout");

        for (field_plans, 0..) |field_plan, i| {
            const field_layout_idx = self.layouts().getStructFieldLayoutByOriginalIndex(record_layout.getStruct().idx, @intCast(i));
            const field_layout = self.layoutValue(field_layout_idx);
            if (self.layouts().layoutSize(field_layout) == 0) continue;
            const field_offset = self.layouts().getStructFieldOffsetByOriginalIndex(record_layout.getStruct().idx, @intCast(i));
            try self.writeValue(bytes, relocations, base_offset + field_offset, source, source.store.get(fields[i]), field_plan, field_layout_idx);
        }
    }

    fn writeTagUnion(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        source: ConstModule,
        value: ConstValue,
        plan_id: lir.Program.ConstPlanId,
        variants: []const lir.Program.ConstTagVariant,
        tag_union_layout_idx: layout.Idx,
    ) MaterializationError!void {
        const tag = switch (value) {
            .tag => |tag| tag,
            else => staticDataInvariant("tag-union const plan received non-tag ConstStore node"),
        };
        const variant_index = variantIndexForTag(variants, tag.tag_name);
        const variant = variants[variant_index];
        if (variant.payloads.len != tag.payloads.len) staticDataInvariant("tag const plan payload count differed from ConstStore node");
        if (try self.writeBoxedSemanticValue(bytes, relocations, base_offset, source, value, plan_id, tag_union_layout_idx)) return;

        const tag_layout = self.layoutValue(tag_union_layout_idx);
        if (tag_layout.tag == .zst) return;
        if (tag_layout.tag != .tag_union) staticDataInvariant("tag-union const plan had non-tag-union layout");

        const tag_info = self.layouts().getTagUnionInfo(tag_layout);
        const active_payload_layout_idx = tag_info.variants.get(@intCast(variant.discriminant)).payload_layout;
        for (variant.payloads, 0..) |payload_plan, payload_index| {
            const payload_layout_idx = payloadLayoutForTagArg(
                self.layouts(),
                active_payload_layout_idx,
                variant.payloads.len,
                @intCast(payload_index),
            );
            const payload_layout = self.layoutValue(payload_layout_idx);
            if (self.layouts().layoutSize(payload_layout) == 0) continue;
            const payload_offset = payloadOffsetForTagArg(
                self.layouts(),
                active_payload_layout_idx,
                variant.payloads.len,
                @intCast(payload_index),
            );
            try self.writeValue(
                bytes,
                relocations,
                base_offset + payload_offset,
                source,
                source.store.get(tag.payloads[payload_index]),
                payload_plan,
                payload_layout_idx,
            );
        }

        const tag_data = self.layouts().getTagUnionData(tag_layout.getTagUnion().idx);
        tag_data.writeDiscriminant(bytes[base_offset..].ptr, variant.discriminant, self.layouts().targetUsize());
    }

    fn writeErasedFn(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        source: ConstModule,
        value: ConstValue,
        set_id: lir.Program.ErasedFnsId,
        erased_layout_idx: layout.Idx,
    ) MaterializationError!void {
        if (self.layoutValue(erased_layout_idx).tag != .erased_callable) {
            staticDataInvariant("erased callable const plan had non-erased-callable layout");
        }
        const fn_id = switch (value) {
            .fn_value => |fn_id| fn_id,
            else => staticDataInvariant("erased callable const plan received non-function ConstStore node"),
        };
        const raw = @intFromEnum(fn_id);
        if (raw >= source.store.fns.items.len) staticDataInvariant("ConstStore function id is out of range");
        const fn_value = source.store.getFn(@enumFromInt(raw));
        const entry = self.erasedFnEntry(set_id, fn_value);
        const target = try self.materializeErasedFn(source, fn_value, entry);
        try self.writePointerRelocation(bytes, relocations, base_offset, target.symbol_name, target.addend);
    }

    fn erasedFnEntry(
        self: *StaticDataBuilder,
        set_id: lir.Program.ErasedFnsId,
        fn_value: ConstFn,
    ) lir.Program.ErasedFn {
        const set = self.lowered.lir_result.erased_fns.items[@intFromEnum(set_id)];
        for (set.entries) |entry| {
            if (sameFnTemplate(fn_value, entry.template)) return entry;
        }
        staticDataInvariant("erased callable ConstStore function was absent from LIR erased callable entries");
    }

    fn materializeErasedFn(
        self: *StaticDataBuilder,
        source: ConstModule,
        fn_value: ConstFn,
        entry: lir.Program.ErasedFn,
    ) MaterializationError!PointerTarget {
        const capture_layout = self.layoutValue(entry.capture_layout);
        const capture_size = self.layouts().layoutSize(capture_layout);
        const capture_align = capture_layout.alignment(self.target_usize).toByteUnits();
        if (capture_align > builtins.erased_callable.capture_alignment) {
            staticDataInvariant("erased callable capture layout alignment exceeded erased callable capture alignment");
        }

        const payload_size = builtins.erased_callable.payloadSize(capture_size);
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

        const proc = self.lowered.lir_result.store.getProcSpec(entry.entry);
        const proc_symbol = try backend.procSymbolName(self.allocator, proc.name);
        var proc_symbol_owned = true;
        errdefer if (proc_symbol_owned) self.allocator.free(proc_symbol);
        try self.writeOwnedPointerRelocation(payload, &payload_relocs, 0, proc_symbol, 0, .function_pointer);
        proc_symbol_owned = false;

        try self.writeCaptures(
            payload,
            &payload_relocs,
            builtins.erased_callable.capture_offset,
            source,
            fn_value,
            entry.captures,
            entry.capture_layout,
        );

        const relocations = try payload_relocs.toOwnedSlice(self.allocator);
        errdefer if (!payload_consumed) self.allocator.free(relocations);
        const target = try self.addStaticAllocationWithRelocs(
            payload,
            builtins.erased_callable.payload_alignment,
            builtins.erased_callable.allocation_has_refcounted_children,
            null,
            relocations,
        );
        payload_consumed = true;
        return target;
    }

    fn writeCaptures(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        base_offset: u32,
        source: ConstModule,
        fn_value: ConstFn,
        slots: []const lir.Program.CaptureSlot,
        capture_layout_idx: layout.Idx,
    ) MaterializationError!void {
        if (slots.len != fn_value.captures.len) {
            staticDataInvariant("erased callable capture slot count differed from ConstStore captures");
        }
        if (slots.len == 0) return;

        const capture_layout = self.layoutValue(capture_layout_idx);
        if (capture_layout.tag == .zst) return;
        if (capture_layout.tag == .struct_) {
            for (slots) |slot| {
                const field_layout = self.layouts().getStructFieldLayoutByOriginalIndex(capture_layout.getStruct().idx, slot.slot);
                const field_offset = self.layouts().getStructFieldOffsetByOriginalIndex(capture_layout.getStruct().idx, slot.slot);
                const capture_node = self.captureNode(fn_value, slot.id);
                try self.writeValue(bytes, relocations, base_offset + field_offset, source, source.store.get(capture_node), slot.plan, field_layout);
            }
            return;
        }
        if (slots.len == 1) {
            const capture_node = self.captureNode(fn_value, slots[0].id);
            try self.writeValue(bytes, relocations, base_offset, source, source.store.get(capture_node), slots[0].plan, capture_layout_idx);
            return;
        }
        staticDataInvariant("multi-capture erased callable did not use a struct capture layout");
    }

    fn captureNode(
        _: *StaticDataBuilder,
        fn_value: ConstFn,
        id: check.ConstStore.CaptureId,
    ) CheckedModule.ConstNodeId {
        for (fn_value.captures) |capture| {
            if (std.meta.eql(capture.id, id)) return capture.value;
        }
        staticDataInvariant("erased callable capture slot had no ConstStore capture");
    }

    fn variantIndexForTag(
        variants: []const lir.Program.ConstTagVariant,
        tag_name: []const u8,
    ) usize {
        for (variants, 0..) |variant, index| {
            if (std.mem.eql(u8, tag_name, variant.name)) return index;
        }
        staticDataInvariant("ConstStore tag name was absent from requested static data plan");
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

        const symbol_name = try std.fmt.allocPrint(self.allocator, "roc__static_const_{d}", .{self.local_symbol_ordinal});
        self.local_symbol_ordinal += 1;
        errdefer self.allocator.free(symbol_name);

        const data_offset = staticDataPtrOffset(self.word_size, payload_alignment, contains_refcounted);
        const bytes = try self.allocator.alloc(u8, data_offset + payload.len);
        errdefer self.allocator.free(bytes);
        @memset(bytes, 0);
        @memcpy(bytes[data_offset..][0..payload.len], payload);
        self.allocator.free(payload);
        payload_owned = false;

        if (contains_refcounted) {
            self.writeTargetWord(bytes, data_offset - self.word_size * 2, if (list_element_count) |count| @intCast(count) else 0);
        }
        self.writeTargetSignedWord(bytes, data_offset - self.word_size, 0);

        const relocations = try self.allocator.alloc(StaticDataRelocation, payload_relocations.len);
        errdefer {
            deinitRelocationSlice(self.allocator, relocations);
            self.allocator.free(relocations);
        }
        for (payload_relocations, 0..) |relocation, i| {
            relocations[i] = .{
                .offset = data_offset + relocation.offset,
                .target_symbol_name = relocation.target_symbol_name,
                .addend = relocation.addend,
                .kind = relocation.kind,
                .owns_target_symbol_name = relocation.owns_target_symbol_name,
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

    fn constPlan(self: *StaticDataBuilder, plan: lir.Program.ConstPlanId) lir.Program.ConstPlan {
        return self.lowered.lir_result.const_plans.items[@intFromEnum(plan)];
    }

    fn layouts(self: *StaticDataBuilder) *const layout.Store {
        return &self.lowered.lir_result.layouts;
    }

    fn layoutValue(self: *StaticDataBuilder, layout_idx: layout.Idx) layout.Layout {
        return self.layouts().getLayout(layout_idx);
    }

    fn writePointerRelocation(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        offset: u32,
        target_symbol_name: []const u8,
        addend: i64,
    ) Allocator.Error!void {
        self.writeTargetWord(bytes, offset, 0);
        try relocations.append(self.allocator, .{
            .offset = offset,
            .target_symbol_name = target_symbol_name,
            .addend = addend,
        });
    }

    fn writeOwnedPointerRelocation(
        self: *StaticDataBuilder,
        bytes: []u8,
        relocations: *std.ArrayList(StaticDataRelocation),
        offset: u32,
        target_symbol_name: []const u8,
        addend: i64,
        kind: StaticDataRelocation.Kind,
    ) Allocator.Error!void {
        self.writeTargetWord(bytes, offset, 0);
        try relocations.append(self.allocator, .{
            .offset = offset,
            .target_symbol_name = target_symbol_name,
            .addend = addend,
            .kind = kind,
            .owns_target_symbol_name = true,
        });
    }

    fn writeBytes(_: *StaticDataBuilder, bytes: []u8, offset: u32, source: []const u8) void {
        @memcpy(bytes[offset..][0..source.len], source);
    }

    fn writeTargetWord(self: *StaticDataBuilder, bytes: []u8, offset: u32, value: u64) void {
        if (value > self.targetWordMax()) staticDataInvariant("static data word exceeds target usize");
        switch (self.word_size) {
            4 => std.mem.writeInt(u32, bytes[offset..][0..4], @intCast(value), .little),
            8 => std.mem.writeInt(u64, bytes[offset..][0..8], value, .little),
            else => unreachable,
        }
    }

    fn encodeRocStrCapacity(self: *StaticDataBuilder, capacity: usize) u64 {
        const target_capacity: u64 = @intCast(capacity);
        const max_capacity = self.targetWordMax() >> 1;
        if (target_capacity > max_capacity) staticDataInvariant("static string exceeds RocStr capacity limit for target");
        return target_capacity << 1;
    }

    fn encodeRocListCapacity(self: *StaticDataBuilder, capacity: usize) u64 {
        const target_capacity: u64 = @intCast(capacity);
        const max_capacity = self.targetWordMax() >> 1;
        if (target_capacity > max_capacity) staticDataInvariant("static list exceeds RocList capacity limit for target");
        return target_capacity << 1;
    }

    fn writeTargetSignedWord(self: *StaticDataBuilder, bytes: []u8, offset: u32, value: i64) void {
        switch (self.word_size) {
            4 => std.mem.writeInt(i32, bytes[offset..][0..4], @intCast(value), .little),
            8 => std.mem.writeInt(i64, bytes[offset..][0..8], value, .little),
            else => unreachable,
        }
    }

    fn targetWordMax(self: *StaticDataBuilder) u64 {
        const unused_bits: std.math.Log2Int(u64) = @intCast((8 - self.word_size) * 8);
        return @as(u64, std.math.maxInt(u64)) >> unused_bits;
    }
};

fn moduleHasProvidedData(module: *const Checked.CheckedModuleArtifact) bool {
    for (module.provided_exports.exports) |provided| {
        switch (provided) {
            .data => return true,
            .procedure => {},
        }
    }
    return false;
}

fn hasProvidedData(modules: ModuleViews) bool {
    if (modules.root) |root| {
        if (moduleHasProvidedData(root.module)) return true;
    }
    return false;
}

fn moduleBytesEqual(a: [32]u8, b: [32]u8) bool {
    return std.mem.eql(u8, a[0..], b[0..]);
}

fn payloadLayoutForTagArg(
    layouts: *const layout.Store,
    variant_layout_idx: layout.Idx,
    arg_count: usize,
    arg_index: u32,
) layout.Idx {
    if (arg_count == 0) return layout.Idx.zst;
    const variant_layout = layouts.getLayout(variant_layout_idx);
    if (arg_count == 1) {
        if (variant_layout.tag == .struct_ and layouts.getStructInfo(variant_layout).fields.len == 1) {
            return layouts.getStructFieldLayoutByOriginalIndex(variant_layout.getStruct().idx, 0);
        }
        return variant_layout_idx;
    }
    if (variant_layout.tag != .struct_) staticDataInvariant("multi-payload tag did not use struct payload layout");
    return layouts.getStructFieldLayoutByOriginalIndex(variant_layout.getStruct().idx, arg_index);
}

fn payloadOffsetForTagArg(
    layouts: *const layout.Store,
    variant_layout_idx: layout.Idx,
    arg_count: usize,
    arg_index: u32,
) u32 {
    if (arg_count <= 1) return 0;
    const variant_layout = layouts.getLayout(variant_layout_idx);
    if (variant_layout.tag != .struct_) staticDataInvariant("multi-payload tag did not use struct payload layout");
    return layouts.getStructFieldOffsetByOriginalIndex(variant_layout.getStruct().idx, arg_index);
}

fn staticDataPtrOffset(word_size: u32, element_alignment: u32, contains_refcounted: bool) u32 {
    const required_space = if (contains_refcounted) word_size * 2 else word_size;
    return alignForwardU32(required_space, element_alignment);
}

fn sameFnTemplate(fn_value: ConstFn, template: lir.Program.FnTemplate) bool {
    return std.meta.eql(fn_value.fn_def, template.fn_def) and
        fn_value.source_fn_ty == template.source_fn_ty and
        std.mem.eql(u8, fn_value.source_fn_key.bytes[0..], template.source_fn_key.bytes[0..]);
}

fn alignForwardU32(value: u32, alignment: u32) u32 {
    std.debug.assert(alignment != 0);
    return @intCast(std.mem.alignForward(usize, value, alignment));
}

fn staticDataInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("static data invariant violated: {s}", .{message});
    }
    unreachable;
}

test "static data declarations are referenced" {
    std.testing.refAllDecls(@This());
}
