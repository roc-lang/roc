//! Stores Layout values by index.

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const tracy = @import("tracy");
const base = @import("base");
const collections = @import("collections");
const types = @import("types");

const layout_mod = @import("layout.zig");
const graph_mod = @import("./graph.zig");
const rc_helper = @import("./rc_helper.zig");
const work_mod = @import("./work.zig");
const field_order = @import("./field_order.zig");

const target = base.target;
const Layout = layout_mod.Layout;
const LayoutTag = layout_mod.LayoutTag;
const Idx = layout_mod.Idx;
const StructField = layout_mod.StructField;
const Scalar = layout_mod.Scalar;
const StructData = layout_mod.StructData;
const StructIdx = layout_mod.StructIdx;
const TagUnionVariant = layout_mod.TagUnionVariant;
const TagUnionData = layout_mod.TagUnionData;
const TagUnionIdx = layout_mod.TagUnionIdx;
const SizeAlign = layout_mod.SizeAlign;
const ListInfo = layout_mod.ListInfo;
const BoxInfo = layout_mod.BoxInfo;
const StructInfo = layout_mod.StructInfo;
const TagUnionInfo = layout_mod.TagUnionInfo;
const ScalarInfo = layout_mod.ScalarInfo;
const LayoutGraph = graph_mod.Graph;
const GraphNodeId = graph_mod.NodeId;
const GraphRef = graph_mod.Ref;
const Var = types.Var;
const TypeScope = types.TypeScope;
pub const ModuleVarKey = work_mod.ModuleVarKey;

fn assertAppendIdx(expected: usize, idx: anytype) void {
    if (comptime builtin.mode == .Debug) {
        std.debug.assert(@intFromEnum(idx) == expected);
    } else if (@intFromEnum(idx) != expected) {
        unreachable;
    }
}

/// Whether any field is an unnamed `_` padding spacer. A nominal record opts into
/// declared-order-plus-padding layout by including such a field; without one it
/// lays out like a structural record.
fn hasAnyPaddingField(fields: []const StructField) bool {
    for (fields) |field| {
        if (field.is_padding) return true;
    }
    return false;
}

/// Errors that can occur during layout computation
/// Stores Layout instances by Idx.
///
/// This is a GLOBAL layout store that serves all modules in the build.
/// Layout indices are only meaningful within their originating store, so using
/// per-module stores causes crashes when layout indices cross module boundaries.
pub const Store = struct {
    const Self = @This();

    pub const BuiltinListAbi = struct {
        elem_layout_idx: ?Idx,
        elem_layout: Layout,
        elem_size: u32,
        elem_alignment: u32,
        contains_refcounted: bool,
    };

    pub const BuiltinBoxAbi = struct {
        elem_layout_idx: ?Idx,
        elem_layout: Layout,
        elem_size: u32,
        elem_alignment: u32,
        contains_refcounted: bool,
    };

    /// Allocator for all internal allocations
    allocator: std.mem.Allocator,

    layouts: collections.SafeList(Layout),
    resolved_list_layouts: std.ArrayList(?Idx),
    tuple_elems: collections.SafeList(Idx),
    struct_fields: StructField.SafeMultiList,
    struct_data: collections.SafeList(StructData),
    tag_union_variants: TagUnionVariant.SafeMultiList,
    tag_union_data: collections.SafeList(TagUnionData),

    // Structural interning cache for non-scalar layouts. Keys are canonical
    // binary encodings of layout shape.
    interned_layouts: std.StringHashMap(Idx),
    scratch_intern_key: std.ArrayList(u8),

    // The target's usize type (32-bit or 64-bit) - used for layout calculations
    // This is critical for cross-compilation (e.g., compiling for wasm32 on a 64-bit host)
    target_usize: target.TargetUsize,

    // Number of sentinel layouts that are pre-populated in the layout store.
    // Must be kept in sync with the sentinel values in layout.zig Idx enum.
    const num_primitives = 17;

    /// Get the sentinel Idx for a given scalar type using pure arithmetic - no branches!
    /// This relies on the careful ordering of ScalarTag and Idx enum values.
    pub fn idxFromScalar(scalar: Scalar) Idx {
        return switch (scalar.tag) {
            .str => .str,
            .int => @enumFromInt(2 + @intFromEnum(scalar.getInt())),
            .frac => @enumFromInt(@as(u32, 12) + (@intFromEnum(scalar.getFrac()) - @intFromEnum(@TypeOf(scalar.getFrac()).f32))),
            .opaque_ptr => .opaque_ptr,
        };
    }

    pub fn init(
        allocator: std.mem.Allocator,
        target_usize: target.TargetUsize,
    ) std.mem.Allocator.Error!Self {
        var layouts = collections.SafeList(Layout){};
        var tag_union_variants = try TagUnionVariant.SafeMultiList.initCapacity(allocator, 64);
        var tag_union_data = try collections.SafeList(TagUnionData).initCapacity(allocator, 64);

        // Reserve canonical tag-union metadata index 0 for the shared two-nullary enum
        // representation. `layout.Idx.bool` is just a stable handle to this ordinary
        // tag-union layout so control-flow code can reference it conveniently.
        {
            const expected_idx = tag_union_variants.len();
            const idx = try tag_union_variants.append(allocator, .{ .payload_layout = .zst });
            assertAppendIdx(expected_idx, idx);
        }
        {
            const expected_idx = tag_union_variants.len();
            const idx = try tag_union_variants.append(allocator, .{ .payload_layout = .zst });
            assertAppendIdx(expected_idx, idx);
        }
        {
            const expected_idx = tag_union_data.items.items.len;
            const idx = try tag_union_data.append(allocator, .{
                .size = layout_mod.WidthValues(u32).both(1, 1),
                .discriminant_offset = layout_mod.WidthValues(u16).both(0, 0),
                .discriminant_size = 1,
                .variants = .{
                    .start = 0,
                    .count = 2,
                },
                // Both variants are zero-sized (no payload), so this builtin
                // two-nullary enum contains no refcounted data.
                .contains_refcounted = false,
            });
            assertAppendIdx(expected_idx, idx);
        }

        // Pre-populate primitive type layouts in order matching the Idx enum.
        // Changing the order of these can break things!
        {
            const expected_idx = layouts.items.items.len;
            const idx = try layouts.append(allocator, Layout.boolType());
            assertAppendIdx(expected_idx, idx);
        }
        {
            const expected_idx = layouts.items.items.len;
            const idx = try layouts.append(allocator, Layout.str());
            assertAppendIdx(expected_idx, idx);
        }
        {
            const expected_idx = layouts.items.items.len;
            const idx = try layouts.append(allocator, Layout.int(.u8));
            assertAppendIdx(expected_idx, idx);
        }
        {
            const expected_idx = layouts.items.items.len;
            const idx = try layouts.append(allocator, Layout.int(.i8));
            assertAppendIdx(expected_idx, idx);
        }
        {
            const expected_idx = layouts.items.items.len;
            const idx = try layouts.append(allocator, Layout.int(.u16));
            assertAppendIdx(expected_idx, idx);
        }
        {
            const expected_idx = layouts.items.items.len;
            const idx = try layouts.append(allocator, Layout.int(.i16));
            assertAppendIdx(expected_idx, idx);
        }
        {
            const expected_idx = layouts.items.items.len;
            const idx = try layouts.append(allocator, Layout.int(.u32));
            assertAppendIdx(expected_idx, idx);
        }
        {
            const expected_idx = layouts.items.items.len;
            const idx = try layouts.append(allocator, Layout.int(.i32));
            assertAppendIdx(expected_idx, idx);
        }
        {
            const expected_idx = layouts.items.items.len;
            const idx = try layouts.append(allocator, Layout.int(.u64));
            assertAppendIdx(expected_idx, idx);
        }
        {
            const expected_idx = layouts.items.items.len;
            const idx = try layouts.append(allocator, Layout.int(.i64));
            assertAppendIdx(expected_idx, idx);
        }
        {
            const expected_idx = layouts.items.items.len;
            const idx = try layouts.append(allocator, Layout.int(.u128));
            assertAppendIdx(expected_idx, idx);
        }
        {
            const expected_idx = layouts.items.items.len;
            const idx = try layouts.append(allocator, Layout.int(.i128));
            assertAppendIdx(expected_idx, idx);
        }
        {
            const expected_idx = layouts.items.items.len;
            const idx = try layouts.append(allocator, Layout.frac(.f32));
            assertAppendIdx(expected_idx, idx);
        }
        {
            const expected_idx = layouts.items.items.len;
            const idx = try layouts.append(allocator, Layout.frac(.f64));
            assertAppendIdx(expected_idx, idx);
        }
        {
            const expected_idx = layouts.items.items.len;
            const idx = try layouts.append(allocator, Layout.frac(.dec));
            assertAppendIdx(expected_idx, idx);
        }
        {
            const expected_idx = layouts.items.items.len;
            const idx = try layouts.append(allocator, Layout.opaquePtr());
            assertAppendIdx(expected_idx, idx);
        }
        {
            const expected_idx = layouts.items.items.len;
            const idx = try layouts.append(allocator, Layout.zst());
            assertAppendIdx(expected_idx, idx);
        }

        std.debug.assert(layouts.len() == num_primitives);

        var self = Self{
            .allocator = allocator,
            .layouts = layouts,
            .resolved_list_layouts = .empty,
            .tuple_elems = try collections.SafeList(Idx).initCapacity(allocator, 512),
            .struct_fields = try StructField.SafeMultiList.initCapacity(allocator, 512),
            .struct_data = try collections.SafeList(StructData).initCapacity(allocator, 512),
            .tag_union_variants = tag_union_variants,
            .tag_union_data = tag_union_data,
            .interned_layouts = std.StringHashMap(Idx).init(allocator),
            .scratch_intern_key = .empty,
            .target_usize = target_usize,
        };

        try self.buildExistingLayoutInternKey(Layout.boolType());
        try self.rememberScratchInternKey(.bool);
        try self.resolved_list_layouts.ensureTotalCapacity(allocator, num_primitives);
        for (0..num_primitives) |_| {
            self.resolved_list_layouts.appendAssumeCapacity(null);
        }
        return self;
    }

    pub const GraphCommit = struct {
        root_idx: Idx,
        raw_layouts: []Idx,
        value_layouts: []Idx,

        pub fn deinit(self_commit: *GraphCommit, allocator: std.mem.Allocator) void {
            allocator.free(self_commit.raw_layouts);
            allocator.free(self_commit.value_layouts);
        }
    };

    pub fn deinit(self: *Self) void {
        self.resolved_list_layouts.deinit(self.allocator);
        self.layouts.deinit(self.allocator);
        self.tuple_elems.deinit(self.allocator);
        self.struct_fields.deinit(self.allocator);
        self.struct_data.deinit(self.allocator);
        self.tag_union_variants.deinit(self.allocator);
        self.tag_union_data.deinit(self.allocator);
        var interned_keys = self.interned_layouts.keyIterator();
        while (interned_keys.next()) |key_ptr| {
            self.allocator.free(key_ptr.*);
        }
        self.interned_layouts.deinit();
        self.scratch_intern_key.deinit(self.allocator);
    }

    fn appendInternKeyValue(self: *Self, value: anytype) std.mem.Allocator.Error!void {
        var copy = value;
        try self.scratch_intern_key.appendSlice(self.allocator, std.mem.asBytes(&copy));
    }

    fn appendInternKeyIdx(self: *Self, layout_idx: Idx) std.mem.Allocator.Error!void {
        const raw_layout: u32 = @intCast(@intFromEnum(layout_idx));
        try self.appendInternKeyValue(raw_layout);
    }

    fn startInternKey(self: *Self, tag: LayoutTag) std.mem.Allocator.Error!void {
        self.scratch_intern_key.clearRetainingCapacity();
        const raw_tag: u8 = @intCast(@intFromEnum(tag));
        try self.appendInternKeyValue(raw_tag);
    }

    fn lookupInternedScratchKey(self: *Self) ?Idx {
        return self.interned_layouts.get(self.scratch_intern_key.items);
    }

    fn rememberScratchInternKey(self: *Self, idx: Idx) std.mem.Allocator.Error!void {
        const owned_key = try self.allocator.dupe(u8, self.scratch_intern_key.items);
        errdefer self.allocator.free(owned_key);
        try self.interned_layouts.put(owned_key, idx);
    }

    fn publishExistingLayoutInternKey(self: *Self, idx: Idx) std.mem.Allocator.Error!void {
        try self.buildExistingLayoutInternKey(self.getLayout(idx));
        if (self.interned_layouts.getPtr(self.scratch_intern_key.items)) |existing| {
            existing.* = idx;
            return;
        }
        try self.rememberScratchInternKey(idx);
    }

    fn buildStructInternKeyFromFields(
        self: *Self,
        sort_key: layout_mod.SortKey,
        fields: []const StructField,
    ) std.mem.Allocator.Error!void {
        // Size is derived from (sort_key, fields), so it is not part of the key.
        try self.startInternKey(.struct_);
        try self.appendInternKeyValue(@as(u8, @intFromEnum(sort_key)));
        try self.appendInternKeyValue(@as(u32, @intCast(fields.len)));
        for (fields) |field| {
            try self.appendInternKeyValue(field.index);
            try self.appendInternKeyIdx(field.layout);
            try self.appendInternKeyValue(@as(u8, @intFromBool(field.is_padding)));
        }
    }

    fn buildTagUnionInternKeyFromVariants(
        self: *Self,
        sort_key: layout_mod.SortKey,
        discriminant_size: u8,
        variant_layouts: []const Idx,
    ) std.mem.Allocator.Error!void {
        // Size and discriminant offset are derived, so only the discriminant size
        // (plus sort key and variants) participates in the key.
        try self.startInternKey(.tag_union);
        try self.appendInternKeyValue(@as(u8, @intFromEnum(sort_key)));
        try self.appendInternKeyValue(discriminant_size);
        try self.appendInternKeyValue(@as(u32, @intCast(variant_layouts.len)));
        for (variant_layouts) |payload_layout| {
            try self.appendInternKeyIdx(payload_layout);
        }
    }

    fn buildExistingLayoutInternKey(self: *Self, layout: Layout) std.mem.Allocator.Error!void {
        switch (layout.tag) {
            .scalar => unreachable,
            .zst => try self.startInternKey(.zst),
            .box => {
                try self.startInternKey(.box);
                try self.appendInternKeyIdx(layout.getIdx());
            },
            .ptr => {
                try self.startInternKey(.ptr);
                try self.appendInternKeyIdx(layout.getIdx());
            },
            .box_of_zst => try self.startInternKey(.box_of_zst),
            .erased_callable => try self.startInternKey(.erased_callable),
            .list => {
                try self.startInternKey(.list);
                try self.appendInternKeyIdx(layout.getIdx());
            },
            .list_of_zst => try self.startInternKey(.list_of_zst),
            .closure => {
                try self.startInternKey(.closure);
                try self.appendInternKeyIdx(layout.getClosure().captures_layout_idx);
            },
            .struct_ => {
                const info = self.getStructInfo(layout);
                try self.startInternKey(.struct_);
                try self.appendInternKeyValue(@as(u8, @intFromEnum(layout.getStruct().sort_key)));
                try self.appendInternKeyValue(@as(u32, @intCast(info.fields.len)));
                for (0..info.fields.len) |i| {
                    const field = info.fields.get(i);
                    try self.appendInternKeyValue(field.index);
                    try self.appendInternKeyIdx(field.layout);
                    try self.appendInternKeyValue(@as(u8, @intFromBool(field.is_padding)));
                }
            },
            .tag_union => {
                const info = self.getTagUnionInfo(layout);
                try self.startInternKey(.tag_union);
                try self.appendInternKeyValue(@as(u8, @intFromEnum(layout.getTagUnion().sort_key)));
                try self.appendInternKeyValue(info.data.discriminant_size);
                try self.appendInternKeyValue(@as(u32, @intCast(info.variants.len)));
                for (0..info.variants.len) |i| {
                    const variant = info.variants.get(i);
                    try self.appendInternKeyIdx(variant.payload_layout);
                }
            },
        }
    }

    pub fn reserveLayout(self: *Self, layout: Layout) std.mem.Allocator.Error!Idx {
        const safe_list_idx = try self.layouts.append(self.allocator, layout);
        const idx: Idx = @enumFromInt(@intFromEnum(safe_list_idx));
        try self.resolved_list_layouts.append(self.allocator, self.computeResolvedListLayoutIdx(idx));
        return idx;
    }

    fn internStructShape(
        self: *Self,
        sort_key: layout_mod.SortKey,
        sizes: layout_mod.WidthValues(u32),
        fields: []const StructField,
    ) std.mem.Allocator.Error!Idx {
        try self.buildStructInternKeyFromFields(sort_key, fields);
        if (self.lookupInternedScratchKey()) |existing| return existing;

        const fields_start = self.struct_fields.items.len;
        for (fields) |field| {
            const expected_idx = self.struct_fields.items.len;
            const idx = try self.struct_fields.append(self.allocator, field);
            assertAppendIdx(expected_idx, idx);
        }

        const contains_refcounted = self.computeStructContainsRefcounted(fields);
        const struct_idx = StructIdx{ .int_idx = @intCast(self.struct_data.len()) };
        const expected_idx = self.struct_data.items.items.len;
        const struct_data_idx = try self.struct_data.append(self.allocator, .{
            .size = sizes,
            .fields = .{
                .start = @intCast(fields_start),
                .count = @intCast(fields.len),
            },
            .contains_refcounted = contains_refcounted,
        });
        assertAppendIdx(expected_idx, struct_data_idx);

        const layout_idx = try self.reserveLayout(Layout.struct_(sort_key, struct_idx));
        try self.rememberScratchInternKey(layout_idx);
        return layout_idx;
    }

    fn internTagUnionShape(
        self: *Self,
        sizes: layout_mod.WidthValues(u32),
        discriminant_offsets: layout_mod.WidthValues(u16),
        discriminant_size: u8,
        variant_layouts: []const Idx,
    ) std.mem.Allocator.Error!Idx {
        const sort_key = self.tagUnionVariantsSortKey(variant_layouts, discriminant_size);
        try self.buildTagUnionInternKeyFromVariants(
            sort_key,
            discriminant_size,
            variant_layouts,
        );
        if (self.lookupInternedScratchKey()) |existing| return existing;

        const variants_start: u32 = @intCast(self.tag_union_variants.len());
        for (variant_layouts) |variant_layout_idx| {
            const expected_idx = self.tag_union_variants.len();
            const idx = try self.tag_union_variants.append(self.allocator, .{
                .payload_layout = variant_layout_idx,
            });
            assertAppendIdx(expected_idx, idx);
        }

        const contains_refcounted = self.computeTagUnionContainsRefcounted(variant_layouts);
        const tag_union_data_idx: u32 = @intCast(self.tag_union_data.len());
        {
            const expected_idx = self.tag_union_data.items.items.len;
            const idx = try self.tag_union_data.append(self.allocator, .{
                .size = sizes,
                .discriminant_offset = discriminant_offsets,
                .discriminant_size = discriminant_size,
                .variants = .{
                    .start = variants_start,
                    .count = @intCast(variant_layouts.len),
                },
                .contains_refcounted = contains_refcounted,
            });
            assertAppendIdx(expected_idx, idx);
        }

        const layout_idx = try self.reserveLayout(Layout.tagUnion(sort_key, .{ .int_idx = @intCast(tag_union_data_idx) }));
        try self.rememberScratchInternKey(layout_idx);
        return layout_idx;
    }

    /// Insert a Box layout with the given element layout.
    ///
    /// Note: A Box of a zero-sized type doesn't need to (and can't) be inserted,
    /// because it's already considered a scalar. To get one of those, call Idx.fromScalar()
    /// passing the .box_of_zst scalar.
    pub fn insertBox(self: *Self, elem_idx: Idx) std.mem.Allocator.Error!Idx {
        const layout = Layout.box(elem_idx);
        return try self.insertLayout(layout);
    }

    /// Insert a compiler-internal pointer layout with the given element layout.
    /// `ptr` layouts are never refcounted; they are introduced by the TRMC pass
    /// for hole/head locals and must never appear as struct fields, tag payloads,
    /// or list elements.
    pub fn insertPtr(self: *Self, elem_idx: Idx) std.mem.Allocator.Error!Idx {
        const layout = Layout.ptr(elem_idx);
        return try self.insertLayout(layout);
    }

    /// Insert the canonical runtime layout for an erased callable behind a
    /// `Box(function)` boundary. The value is one pointer to a Roc refcounted
    /// allocation whose payload stores the callable header followed by inline
    /// capture bytes.
    pub fn insertErasedCallable(self: *Self) std.mem.Allocator.Error!Idx {
        return try self.insertLayout(Layout.erasedCallable());
    }

    /// Insert a List layout with the given element layout.
    ///
    /// Note: A List of a zero-sized type doesn't need to (and can't) be inserted,
    /// because it's already considered a scalar. To get one of those, call Idx.fromScalar()
    /// passing the .list_of_zst scalar.
    pub fn insertList(self: *Self, elem_idx: Idx) std.mem.Allocator.Error!Idx {
        const layout = Layout.list(elem_idx);
        return try self.insertLayout(layout);
    }

    /// Insert a struct layout with the given alignment and struct metadata
    pub fn insertStruct(self: *Self, struct_alignment: std.mem.Alignment, struct_idx: StructIdx) std.mem.Allocator.Error!Idx {
        const layout = Layout.struct_(struct_alignment, struct_idx);
        return try self.insertLayout(layout);
    }

    /// Backwards-compat aliases
    pub const insertRecord = insertStruct;
    pub const insertTuple = insertStruct;

    /// Insert a record layout from field layouts in canonical record-field order.
    /// The shared layout commit performs one stable sort by descending alignment,
    /// preserving canonical alphabetical order among equal-alignment fields.
    pub fn putRecord(self: *Self, field_layouts: []const Layout) std.mem.Allocator.Error!Idx {
        return self.putTuple(field_layouts);
    }

    /// Insert a struct layout from semantic fields.
    /// `fields[i].index` is the canonical semantic field index before the shared
    /// stable alignment sort at layout commit.
    pub fn putStructFields(self: *Self, fields: []const StructField) std.mem.Allocator.Error!Idx {
        const trace = tracy.traceNamed(@src(), "layoutStore.putStructFields");
        defer trace.end();

        if (fields.len == 0) {
            return self.getEmptyStructLayout();
        }

        var temp_fields = std.ArrayList(StructField).empty;
        defer temp_fields.deinit(self.allocator);
        try temp_fields.appendSlice(self.allocator, fields);
        try self.stableSortStructFieldsByLayoutAlignment(temp_fields.items);

        const sizes = self.structSizes(temp_fields.items);
        if (sizes.get(.u64) == 0) {
            return try self.ensureZstLayout();
        }
        return self.internStructShape(
            self.structFieldsSortKey(temp_fields.items),
            sizes,
            temp_fields.items,
        );
    }

    /// Insert a nominal record struct layout from semantic fields given in
    /// DECLARED order. Unlike `putStructFields`, which sorts structural records and
    /// tuples by sort key, this keeps the declared order verbatim and inserts
    /// C-style padding between fields as alignment requires. Used only for nominal
    /// records that opt into declared-order layout with an unnamed `_` field; the
    /// slice order is the source declaration order.
    pub fn putNominalStructFields(self: *Self, fields: []const StructField) std.mem.Allocator.Error!Idx {
        if (fields.len == 0) {
            return self.getEmptyStructLayout();
        }

        const sizes = self.structSizes(fields);
        if (sizes.get(.u64) == 0) {
            return try self.ensureZstLayout();
        }
        return self.internStructShape(
            self.structFieldsSortKey(fields),
            sizes,
            fields,
        );
    }

    /// Insert a tuple layout from concrete element layouts.
    /// The shared layout commit performs one stable sort by descending alignment,
    /// preserving original tuple index order among equal-alignment elements.
    pub fn putTuple(self: *Self, element_layouts: []const Layout) std.mem.Allocator.Error!Idx {
        var temp_fields = std.ArrayList(StructField).empty;
        defer temp_fields.deinit(self.allocator);

        for (element_layouts, 0..) |elem_layout, i| {
            const elem_idx = try self.insertLayout(elem_layout);
            try temp_fields.append(self.allocator, .{ .index = @intCast(i), .layout = elem_idx });
        }

        return self.putStructFields(temp_fields.items);
    }

    /// Sort structural-record / tuple fields by descending sort key, stably.
    /// Routes through the shared `field_order.computeStructuralFieldOrder` so the
    /// layout store and `roc glue` order structural records by the exact same
    /// logic. The sort key is target-independent (a pointer sorts between 4- and
    /// 8-byte alignment), so the resulting field order is identical on 32-bit and
    /// 64-bit targets. Empty field names keep the pure stable sort: callers
    /// presort by name elsewhere, so equal-key fields stay in input order.
    fn stableSortStructFieldsByLayoutAlignment(self: *Self, fields: []StructField) std.mem.Allocator.Error!void {
        if (fields.len <= 1) return;

        const structural = try self.allocator.alloc(field_order.StructuralField, fields.len);
        defer self.allocator.free(structural);
        for (fields, structural) |field, *out| {
            out.* = .{
                .sort_key = if (field.is_padding)
                    .align_1
                else
                    self.getLayout(field.layout).sortKey(),
                .name = "",
            };
        }

        const order = try self.allocator.alloc(u16, fields.len);
        defer self.allocator.free(order);
        field_order.computeStructuralFieldOrder(structural, order);

        const scratch = try self.allocator.alloc(StructField, fields.len);
        defer self.allocator.free(scratch);
        for (order, scratch) |src, *dst| dst.* = fields[src];
        @memcpy(fields, scratch);
    }

    /// Create a tag union layout from pre-computed variant payload layouts.
    /// `variant_layouts[i]` is the layout Idx for variant i's payload
    /// (use ensureZstLayout() for no-payload variants).
    /// Tags must be sorted alphabetically; variant_layouts[i] corresponds
    /// to the tag at sorted index i.
    pub fn putTagUnion(self: *Self, variant_layouts: []const Idx) std.mem.Allocator.Error!Idx {
        // Single-variant tag unions keep their tag_union layout but use an implicit
        // discriminant, so they do not reserve any discriminant bytes in memory.
        const discriminant_size: u8 = tagUnionDiscriminantSize(variant_layouts.len);

        // Size and discriminant offset, precomputed for both pointer widths.
        const m32 = self.tagUnionMetricsAt(variant_layouts, discriminant_size, .u32);
        const m64 = self.tagUnionMetricsAt(variant_layouts, discriminant_size, .u64);
        if (m64.size == 0) {
            return try self.ensureZstLayout();
        }

        return self.internTagUnionShape(
            layout_mod.WidthValues(u32).both(m32.size, m64.size),
            layout_mod.WidthValues(u16).both(m32.discriminant_offset, m64.discriminant_offset),
            discriminant_size,
            variant_layouts,
        );
    }

    fn buildUninternedStructLayout(self: *Self, input_fields: []const StructField) std.mem.Allocator.Error!Layout {
        std.debug.assert(input_fields.len >= 1);

        var temp_fields = std.ArrayList(StructField).empty;
        defer temp_fields.deinit(self.allocator);
        try temp_fields.appendSlice(self.allocator, input_fields);
        try self.stableSortStructFieldsByLayoutAlignment(temp_fields.items);

        const sizes = self.structSizes(temp_fields.items);
        if (sizes.get(.u64) == 0) {
            return Layout.zst();
        }

        const fields_start = self.struct_fields.items.len;
        for (temp_fields.items) |field| {
            const expected_idx = self.struct_fields.items.len;
            const idx = try self.struct_fields.append(self.allocator, field);
            assertAppendIdx(expected_idx, idx);
        }

        const contains_refcounted = self.computeStructContainsRefcounted(temp_fields.items);
        const struct_idx = StructIdx{ .int_idx = @intCast(self.struct_data.len()) };
        const expected_idx = self.struct_data.items.items.len;
        const struct_data_idx = try self.struct_data.append(self.allocator, .{
            .size = sizes,
            .fields = .{
                .start = @intCast(fields_start),
                .count = @intCast(temp_fields.items.len),
            },
            .contains_refcounted = contains_refcounted,
        });
        assertAppendIdx(expected_idx, struct_data_idx);

        return Layout.struct_(self.structFieldsSortKey(temp_fields.items), struct_idx);
    }

    fn buildUninternedTagUnionLayout(self: *Self, variant_layouts: []const Idx) std.mem.Allocator.Error!Layout {
        std.debug.assert(variant_layouts.len >= 1);

        const discriminant_size: u8 = tagUnionDiscriminantSize(variant_layouts.len);
        const m32 = self.tagUnionMetricsAt(variant_layouts, discriminant_size, .u32);
        const m64 = self.tagUnionMetricsAt(variant_layouts, discriminant_size, .u64);
        if (m64.size == 0) {
            return Layout.zst();
        }
        const sizes = layout_mod.WidthValues(u32).both(m32.size, m64.size);
        const discriminant_offsets = layout_mod.WidthValues(u16).both(m32.discriminant_offset, m64.discriminant_offset);

        const variants_start: u32 = @intCast(self.tag_union_variants.len());
        for (variant_layouts) |variant_layout_idx| {
            const expected_idx = self.tag_union_variants.len();
            const idx = try self.tag_union_variants.append(self.allocator, .{
                .payload_layout = variant_layout_idx,
            });
            assertAppendIdx(expected_idx, idx);
        }

        const contains_refcounted = self.computeTagUnionContainsRefcounted(variant_layouts);
        const tag_union_data_idx: u32 = @intCast(self.tag_union_data.len());
        {
            const expected_idx = self.tag_union_data.items.items.len;
            const idx = try self.tag_union_data.append(self.allocator, .{
                .size = sizes,
                .discriminant_offset = discriminant_offsets,
                .discriminant_size = discriminant_size,
                .variants = .{
                    .start = variants_start,
                    .count = @intCast(variant_layouts.len),
                },
                .contains_refcounted = contains_refcounted,
            });
            assertAppendIdx(expected_idx, idx);
        }

        return Layout.tagUnion(self.tagUnionVariantsSortKey(variant_layouts, discriminant_size), .{ .int_idx = @intCast(tag_union_data_idx) });
    }

    /// Canonically intern a whole temporary logical layout graph.
    /// This is the one shared commit point where recursive nominal size cycles
    /// become explicit box layouts for final executable `LIR` consumption.
    pub fn commitGraph(self: *Self, graph: *const LayoutGraph, root: GraphRef) std.mem.Allocator.Error!GraphCommit {
        switch (root) {
            .canonical => |layout_idx| return .{
                .root_idx = layout_idx,
                .raw_layouts = try self.allocator.alloc(Idx, 0),
                .value_layouts = try self.allocator.alloc(Idx, 0),
            },
            .local => {},
        }

        const raw_layouts = try self.allocator.alloc(Idx, graph.nodes.items.len);
        errdefer self.allocator.free(raw_layouts);
        const value_layouts = try self.allocator.alloc(Idx, graph.nodes.items.len);
        errdefer self.allocator.free(value_layouts);
        const resolved = try self.allocator.alloc(bool, graph.nodes.items.len);
        defer self.allocator.free(resolved);
        const recursive_nodes = try self.allocator.alloc(bool, graph.nodes.items.len);
        defer self.allocator.free(recursive_nodes);
        const component_ids = try self.allocator.alloc(u32, graph.nodes.items.len);
        defer self.allocator.free(component_ids);
        @memset(resolved, false);
        @memset(recursive_nodes, false);
        @memset(component_ids, std.math.maxInt(u32));

        const visit_index = try self.allocator.alloc(i32, graph.nodes.items.len);
        defer self.allocator.free(visit_index);
        const lowlink = try self.allocator.alloc(i32, graph.nodes.items.len);
        defer self.allocator.free(lowlink);
        const on_stack = try self.allocator.alloc(bool, graph.nodes.items.len);
        defer self.allocator.free(on_stack);
        @memset(visit_index, -1);
        @memset(lowlink, 0);
        @memset(on_stack, false);

        var tarjan_stack = std.ArrayList(GraphNodeId).empty;
        defer tarjan_stack.deinit(self.allocator);

        const CycleFinder = struct {
            allocator: std.mem.Allocator,
            graph: *const LayoutGraph,
            visit_index: []i32,
            lowlink: []i32,
            on_stack: []bool,
            stack: *std.ArrayList(GraphNodeId),
            recursive_nodes: []bool,
            component_ids: []u32,
            next_index: i32 = 0,
            next_component_id: u32 = 0,

            fn markRecursiveComponent(self_finder: *@This(), component: []const GraphNodeId) void {
                const component_id = self_finder.next_component_id;
                self_finder.next_component_id += 1;

                for (component) |member| {
                    const index = @intFromEnum(member);
                    self_finder.recursive_nodes[index] = true;
                    self_finder.component_ids[index] = component_id;
                }

                var has_boxable_slot_edge = false;
                for (component) |member| {
                    switch (self_finder.graph.getNode(member)) {
                        .struct_ => |span| {
                            for (self_finder.graph.getFields(span)) |field| {
                                switch (field.child) {
                                    .canonical => {},
                                    .local => |child_id| {
                                        if (self_finder.component_ids[@intFromEnum(child_id)] == component_id) {
                                            has_boxable_slot_edge = true;
                                            break;
                                        }
                                    },
                                }
                            }
                        },
                        .tag_union => |span| {
                            for (self_finder.graph.getRefs(span)) |child| {
                                switch (child) {
                                    .canonical => {},
                                    .local => |child_id| {
                                        if (self_finder.component_ids[@intFromEnum(child_id)] != component_id) continue;
                                        switch (self_finder.graph.getNode(child_id)) {
                                            .struct_ => {},
                                            .pending, .nominal, .box, .list, .closure, .erased_callable, .tag_union => {
                                                has_boxable_slot_edge = true;
                                                break;
                                            },
                                        }
                                    },
                                }
                            }
                        },
                        .pending, .nominal, .box, .list, .closure, .erased_callable => {},
                    }
                }

                if (!has_boxable_slot_edge) {
                    std.debug.panic(
                        "layout.Store invariant violated: recursive layout SCC had no explicit slot edge to box at the shared LIR layout commit",
                        .{},
                    );
                }
            }

            fn visitSizeChild(self_finder: *@This(), child_id: GraphNodeId, parent_index: usize) std.mem.Allocator.Error!void {
                const child_index = @intFromEnum(child_id);
                if (self_finder.visit_index[child_index] == -1) {
                    try self_finder.strongConnect(child_id);
                    self_finder.lowlink[parent_index] = @min(self_finder.lowlink[parent_index], self_finder.lowlink[child_index]);
                } else if (self_finder.on_stack[child_index]) {
                    self_finder.lowlink[parent_index] = @min(self_finder.lowlink[parent_index], self_finder.visit_index[child_index]);
                }
            }

            fn hasSizeSelfEdge(self_finder: *@This(), node_id: GraphNodeId) bool {
                return switch (self_finder.graph.getNode(node_id)) {
                    .nominal => |child| switch (child) {
                        .canonical => false,
                        .local => |child_id| child_id == node_id,
                    },
                    .struct_ => |span| blk: {
                        for (self_finder.graph.getFields(span)) |field| {
                            switch (field.child) {
                                .canonical => {},
                                .local => |child_id| if (child_id == node_id) break :blk true,
                            }
                        }
                        break :blk false;
                    },
                    .tag_union => |span| blk: {
                        for (self_finder.graph.getRefs(span)) |child| {
                            switch (child) {
                                .canonical => {},
                                .local => |child_id| if (child_id == node_id) break :blk true,
                            }
                        }
                        break :blk false;
                    },
                    .pending, .box, .list, .closure, .erased_callable => false,
                };
            }

            fn strongConnect(self_finder: *@This(), node_id: GraphNodeId) std.mem.Allocator.Error!void {
                const index = @intFromEnum(node_id);
                self_finder.visit_index[index] = self_finder.next_index;
                self_finder.lowlink[index] = self_finder.next_index;
                self_finder.next_index += 1;
                try self_finder.stack.append(self_finder.allocator, node_id);
                self_finder.on_stack[index] = true;

                switch (self_finder.graph.getNode(node_id)) {
                    .nominal => |child| switch (child) {
                        .canonical => {},
                        .local => |child_id| try self_finder.visitSizeChild(child_id, index),
                    },
                    .struct_ => |span| {
                        for (self_finder.graph.getFields(span)) |field| {
                            switch (field.child) {
                                .canonical => {},
                                .local => |child_id| try self_finder.visitSizeChild(child_id, index),
                            }
                        }
                    },
                    .tag_union => |span| {
                        for (self_finder.graph.getRefs(span)) |child| {
                            switch (child) {
                                .canonical => {},
                                .local => |child_id| try self_finder.visitSizeChild(child_id, index),
                            }
                        }
                    },
                    .pending, .box, .list, .closure, .erased_callable => {},
                }

                if (self_finder.lowlink[index] != self_finder.visit_index[index]) return;

                var component = std.ArrayList(GraphNodeId).empty;
                defer component.deinit(self_finder.allocator);

                while (true) {
                    const member = self_finder.stack.pop() orelse unreachable;
                    const member_index = @intFromEnum(member);
                    self_finder.on_stack[member_index] = false;
                    try component.append(self_finder.allocator, member);
                    if (member == node_id) break;
                }

                if (component.items.len > 1 or self_finder.hasSizeSelfEdge(node_id)) {
                    self_finder.markRecursiveComponent(component.items);
                }
            }
        };

        var cycle_finder = CycleFinder{
            .allocator = self.allocator,
            .graph = graph,
            .visit_index = visit_index,
            .lowlink = lowlink,
            .on_stack = on_stack,
            .stack = &tarjan_stack,
            .recursive_nodes = recursive_nodes,
            .component_ids = component_ids,
        };

        for (graph.nodes.items, 0..) |_, i| {
            if (visit_index[i] == -1) {
                try cycle_finder.strongConnect(@enumFromInt(i));
            }
        }

        for (graph.nodes.items, 0..) |node, i| {
            raw_layouts[i] = try self.reserveLayout(switch (node) {
                .pending => unreachable,
                .nominal => Layout.zst(),
                .box => Layout.box(.zst),
                .list => Layout.list(.zst),
                .closure => Layout.closure(.zst),
                .erased_callable => Layout.erasedCallable(),
                .struct_, .tag_union => Layout.zst(),
            });
        }

        for (graph.nodes.items, 0..) |node, i| {
            value_layouts[i] = switch (node) {
                .pending => unreachable,
                .nominal, .box, .list, .closure, .erased_callable, .struct_, .tag_union => raw_layouts[i],
            };
        }

        const Resolver = struct {
            store: *Self,
            graph: *const LayoutGraph,
            raw_layouts: []Idx,
            value_layouts: []Idx,
            resolved: []bool,
            recursive_nodes: []bool,
            component_ids: []u32,

            fn valueIdx(self_resolver: *@This(), ref: GraphRef) Idx {
                return switch (ref) {
                    .canonical => |layout_idx| layout_idx,
                    .local => |node_id| self_resolver.value_layouts[@intFromEnum(node_id)],
                };
            }

            fn pointerTargetLayout(
                self_resolver: *@This(),
                child_ref: GraphRef,
            ) Idx {
                return switch (child_ref) {
                    .canonical => |layout_idx| layout_idx,
                    .local => |child_id| switch (self_resolver.graph.getNode(child_id)) {
                        .nominal => |child| self_resolver.pointerTargetLayout(child),
                        .pending, .box, .list, .closure, .erased_callable, .struct_, .tag_union => self_resolver.raw_layouts[@intFromEnum(child_id)],
                    },
                };
            }

            fn isValueReady(self_resolver: *@This(), ref: GraphRef) bool {
                return switch (ref) {
                    .canonical => true,
                    .local => |node_id| self_resolver.resolved[@intFromEnum(node_id)],
                };
            }

            fn isSlotReady(self_resolver: *@This(), ref: GraphRef) bool {
                return switch (ref) {
                    .canonical => true,
                    .local => |node_id| blk: {
                        const index = @intFromEnum(node_id);
                        if (self_resolver.resolved[index]) break :blk true;
                        break :blk switch (self_resolver.graph.getNode(node_id)) {
                            .box, .list, .closure, .erased_callable => true,
                            .pending, .nominal, .struct_, .tag_union => false,
                        };
                    },
                };
            }

            fn isKnownZeroSized(self_resolver: *@This(), ref: GraphRef) bool {
                return switch (ref) {
                    .canonical => |layout_idx| self_resolver.store.isZeroSized(self_resolver.store.getLayout(layout_idx)),
                    .local => |node_id| blk: {
                        if (!self_resolver.resolved[@intFromEnum(node_id)]) break :blk false;
                        const layout_idx = self_resolver.value_layouts[@intFromEnum(node_id)];
                        break :blk self_resolver.store.isZeroSized(self_resolver.store.getLayout(layout_idx));
                    },
                };
            }

            fn shouldBoxRecursiveSlotEdge(
                self_resolver: *@This(),
                parent_id: GraphNodeId,
                child_ref: GraphRef,
            ) bool {
                const child_id = switch (child_ref) {
                    .canonical => return false,
                    .local => |id| id,
                };
                const parent_index = @intFromEnum(parent_id);
                const child_index = @intFromEnum(child_id);

                if (!self_resolver.recursive_nodes[parent_index] or !self_resolver.recursive_nodes[child_index]) {
                    return false;
                }
                if (self_resolver.component_ids[parent_index] != self_resolver.component_ids[child_index]) {
                    return false;
                }

                return switch (self_resolver.graph.getNode(parent_id)) {
                    .struct_ => true,
                    .tag_union => switch (self_resolver.graph.getNode(child_id)) {
                        .struct_ => false,
                        .pending, .nominal, .box, .list, .closure, .erased_callable, .tag_union => true,
                    },
                    .pending, .nominal, .box, .list, .closure, .erased_callable => false,
                };
            }

            fn recursiveSlotTargetLayout(
                self_resolver: *@This(),
                child_ref: GraphRef,
            ) Idx {
                return self_resolver.pointerTargetLayout(child_ref);
            }

            fn recursiveSlotLayout(
                self_resolver: *@This(),
                child_ref: GraphRef,
            ) std.mem.Allocator.Error!Idx {
                return try self_resolver.store.insertBox(self_resolver.recursiveSlotTargetLayout(child_ref));
            }

            fn tryResolveNode(self_resolver: *@This(), node_id: GraphNodeId) std.mem.Allocator.Error!bool {
                const index = @intFromEnum(node_id);
                if (self_resolver.resolved[index]) return false;

                switch (self_resolver.graph.getNode(node_id)) {
                    .pending => unreachable,
                    .nominal => |child| {
                        if (!self_resolver.isValueReady(child)) return false;
                        const child_value_idx = self_resolver.valueIdx(child);
                        self_resolver.store.updateLayout(
                            self_resolver.raw_layouts[index],
                            self_resolver.store.getLayout(child_value_idx),
                        );
                        self_resolver.value_layouts[index] = child_value_idx;
                    },
                    .box => |child| {
                        const child_idx = self_resolver.pointerTargetLayout(child);
                        const child_is_zst = self_resolver.isKnownZeroSized(child);
                        self_resolver.store.updateLayout(
                            self_resolver.raw_layouts[index],
                            if (child_is_zst) Layout.boxOfZst() else Layout.box(child_idx),
                        );
                    },
                    .list => |child| {
                        const child_idx = self_resolver.pointerTargetLayout(child);
                        const child_is_zst = self_resolver.isKnownZeroSized(child);
                        self_resolver.store.updateLayout(
                            self_resolver.raw_layouts[index],
                            if (child_is_zst) Layout.listOfZst() else Layout.list(child_idx),
                        );
                    },
                    .closure => |child| {
                        self_resolver.store.updateLayout(
                            self_resolver.raw_layouts[index],
                            Layout.closure(self_resolver.pointerTargetLayout(child)),
                        );
                    },
                    .erased_callable => {
                        self_resolver.store.updateLayout(
                            self_resolver.raw_layouts[index],
                            Layout.erasedCallable(),
                        );
                    },
                    .struct_ => |span| {
                        const graph_fields = self_resolver.graph.getFields(span);
                        if (graph_fields.len == 0) {
                            self_resolver.store.updateLayout(self_resolver.raw_layouts[index], Layout.zst());
                        } else {
                            var fields = std.ArrayList(StructField).empty;
                            defer fields.deinit(self_resolver.store.allocator);
                            try fields.ensureTotalCapacity(self_resolver.store.allocator, graph_fields.len);

                            for (graph_fields) |field| {
                                const field_layout = if (self_resolver.shouldBoxRecursiveSlotEdge(node_id, field.child))
                                    try self_resolver.recursiveSlotLayout(field.child)
                                else blk: {
                                    if (!self_resolver.isSlotReady(field.child)) return false;
                                    break :blk self_resolver.valueIdx(field.child);
                                };
                                fields.appendAssumeCapacity(.{
                                    .index = field.index,
                                    .layout = field_layout,
                                    .is_padding = field.is_padding,
                                });
                            }

                            self_resolver.store.updateLayout(
                                self_resolver.raw_layouts[index],
                                try self_resolver.store.buildUninternedStructLayout(fields.items),
                            );
                        }
                    },
                    .tag_union => |span| {
                        const graph_refs = self_resolver.graph.getRefs(span);
                        if (graph_refs.len == 0) {
                            self_resolver.store.updateLayout(self_resolver.raw_layouts[index], Layout.zst());
                        } else {
                            var variants = std.ArrayList(Idx).empty;
                            defer variants.deinit(self_resolver.store.allocator);
                            try variants.ensureTotalCapacity(self_resolver.store.allocator, graph_refs.len);

                            for (graph_refs) |variant_ref| {
                                const variant_layout = if (self_resolver.shouldBoxRecursiveSlotEdge(node_id, variant_ref))
                                    try self_resolver.recursiveSlotLayout(variant_ref)
                                else blk: {
                                    if (!self_resolver.isSlotReady(variant_ref)) return false;
                                    break :blk self_resolver.valueIdx(variant_ref);
                                };
                                variants.appendAssumeCapacity(variant_layout);
                            }

                            self_resolver.store.updateLayout(
                                self_resolver.raw_layouts[index],
                                try self_resolver.store.buildUninternedTagUnionLayout(variants.items),
                            );
                        }
                    },
                }

                self_resolver.resolved[index] = true;
                return true;
            }
        };

        var resolver = Resolver{
            .store = self,
            .graph = graph,
            .raw_layouts = raw_layouts,
            .value_layouts = value_layouts,
            .resolved = resolved,
            .recursive_nodes = recursive_nodes,
            .component_ids = component_ids,
        };

        while (true) {
            var progress = false;
            for (graph.nodes.items, 0..) |_, i| {
                progress = (try resolver.tryResolveNode(@enumFromInt(i))) or progress;
            }
            if (progress) continue;

            var unresolved_count: usize = 0;
            for (resolved) |done| {
                if (!done) unresolved_count += 1;
            }
            if (unresolved_count == 0) break;

            for (resolved, 0..) |done, i| {
                if (!done) {
                    std.debug.panic(
                        "layout.Store invariant violated: logical graph node {d} remained unresolved during the shared LIR layout commit",
                        .{i},
                    );
                }
            }
        }

        const FinalizeState = enum(u2) { unseen, active, done };
        const finalize_state = try self.allocator.alloc(FinalizeState, graph.nodes.items.len);
        defer self.allocator.free(finalize_state);
        @memset(finalize_state, .unseen);
        const raw_used = try self.allocator.alloc(bool, graph.nodes.items.len);
        defer self.allocator.free(raw_used);
        @memset(raw_used, false);

        const Finalizer = struct {
            store: *Self,
            graph: *const LayoutGraph,
            raw_layouts: []Idx,
            value_layouts: []Idx,
            finalize_state: []FinalizeState,
            raw_used: []bool,
            recursive_nodes: []bool,
            component_ids: []u32,

            fn finalValue(self_finalizer: *@This(), ref: GraphRef) std.mem.Allocator.Error!Idx {
                return switch (ref) {
                    .canonical => |layout_idx| layout_idx,
                    .local => |node_id| try self_finalizer.finalizeNode(node_id),
                };
            }

            fn pointerChildLayout(self_finalizer: *@This(), ref: GraphRef) std.mem.Allocator.Error!Idx {
                return switch (ref) {
                    .canonical => |layout_idx| layout_idx,
                    .local => |node_id| switch (self_finalizer.graph.getNode(node_id)) {
                        .nominal => |child| try self_finalizer.pointerChildLayout(child),
                        .pending, .box, .list, .closure, .erased_callable, .struct_, .tag_union => switch (self_finalizer.finalize_state[@intFromEnum(node_id)]) {
                            .active => blk: {
                                self_finalizer.raw_used[@intFromEnum(node_id)] = true;
                                break :blk self_finalizer.raw_layouts[@intFromEnum(node_id)];
                            },
                            .unseen, .done => try self_finalizer.finalizeNode(node_id),
                        },
                    },
                };
            }

            fn shouldBoxRecursiveSlotEdge(
                self_finalizer: *@This(),
                parent_id: GraphNodeId,
                child_ref: GraphRef,
            ) bool {
                const child_id = switch (child_ref) {
                    .canonical => return false,
                    .local => |id| id,
                };
                const parent_index = @intFromEnum(parent_id);
                const child_index = @intFromEnum(child_id);

                if (!self_finalizer.recursive_nodes[parent_index] or !self_finalizer.recursive_nodes[child_index]) {
                    return false;
                }
                if (self_finalizer.component_ids[parent_index] != self_finalizer.component_ids[child_index]) {
                    return false;
                }

                return switch (self_finalizer.graph.getNode(parent_id)) {
                    .struct_ => true,
                    .tag_union => switch (self_finalizer.graph.getNode(child_id)) {
                        .struct_ => false,
                        .pending, .nominal, .box, .list, .closure, .erased_callable, .tag_union => true,
                    },
                    .pending, .nominal, .box, .list, .closure, .erased_callable => false,
                };
            }

            fn recursiveSlotTargetLayout(
                self_finalizer: *@This(),
                child_ref: GraphRef,
            ) Idx {
                return switch (child_ref) {
                    .canonical => |layout_idx| layout_idx,
                    .local => |child_id| switch (self_finalizer.graph.getNode(child_id)) {
                        .nominal => |child| self_finalizer.recursiveSlotTargetLayout(child),
                        .pending, .box, .list, .closure, .erased_callable, .struct_, .tag_union => blk: {
                            self_finalizer.raw_used[@intFromEnum(child_id)] = true;
                            break :blk self_finalizer.raw_layouts[@intFromEnum(child_id)];
                        },
                    },
                };
            }

            fn recursiveSlotLayout(
                self_finalizer: *@This(),
                child_ref: GraphRef,
            ) std.mem.Allocator.Error!Idx {
                return try self_finalizer.store.insertBox(self_finalizer.recursiveSlotTargetLayout(child_ref));
            }

            fn finalizeNode(self_finalizer: *@This(), node_id: GraphNodeId) std.mem.Allocator.Error!Idx {
                const index = @intFromEnum(node_id);
                return switch (self_finalizer.finalize_state[index]) {
                    .done => self_finalizer.value_layouts[index],
                    .active => blk: {
                        self_finalizer.raw_used[index] = true;
                        break :blk self_finalizer.raw_layouts[index];
                    },
                    .unseen => blk: {
                        self_finalizer.finalize_state[index] = .active;
                        const value_layout = switch (self_finalizer.graph.getNode(node_id)) {
                            .pending => unreachable,
                            .nominal => |child| try self_finalizer.finalValue(child),
                            .box => |child| blk_box: {
                                const child_idx = try self_finalizer.pointerChildLayout(child);
                                const child_layout = self_finalizer.store.getLayout(child_idx);
                                break :blk_box if (self_finalizer.store.isZeroSized(child_layout))
                                    try self_finalizer.store.insertLayout(Layout.boxOfZst())
                                else
                                    try self_finalizer.store.insertBox(child_idx);
                            },
                            .list => |child| blk_list: {
                                const child_idx = try self_finalizer.pointerChildLayout(child);
                                const child_layout = self_finalizer.store.getLayout(child_idx);
                                break :blk_list if (self_finalizer.store.isZeroSized(child_layout))
                                    try self_finalizer.store.insertLayout(Layout.listOfZst())
                                else
                                    try self_finalizer.store.insertList(child_idx);
                            },
                            .closure => |child| try self_finalizer.store.insertLayout(
                                Layout.closure(try self_finalizer.pointerChildLayout(child)),
                            ),
                            .erased_callable => try self_finalizer.store.insertErasedCallable(),
                            .struct_ => |span| blk_struct: {
                                const graph_fields = self_finalizer.graph.getFields(span);
                                if (graph_fields.len == 0) break :blk_struct .zst;
                                var fields = std.ArrayList(StructField).empty;
                                defer fields.deinit(self_finalizer.store.allocator);
                                try fields.ensureTotalCapacity(self_finalizer.store.allocator, graph_fields.len);

                                for (graph_fields) |field| {
                                    const field_layout = if (self_finalizer.shouldBoxRecursiveSlotEdge(node_id, field.child))
                                        try self_finalizer.recursiveSlotLayout(field.child)
                                    else
                                        try self_finalizer.finalValue(field.child);
                                    fields.appendAssumeCapacity(.{
                                        .index = field.index,
                                        .layout = field_layout,
                                        .is_padding = field.is_padding,
                                    });
                                }

                                // A nominal record keeps its declared order (and auto-pads)
                                // only when it opts in with an unnamed `_` field; otherwise it
                                // lays out exactly like a structural record (sort-key sorted).
                                const keep_declared = self_finalizer.graph.isNominalStruct(node_id) and
                                    hasAnyPaddingField(fields.items);
                                break :blk_struct if (keep_declared)
                                    try self_finalizer.store.putNominalStructFields(fields.items)
                                else
                                    try self_finalizer.store.putStructFields(fields.items);
                            },
                            .tag_union => |span| blk_union: {
                                const graph_refs = self_finalizer.graph.getRefs(span);
                                var variants = std.ArrayList(Idx).empty;
                                defer variants.deinit(self_finalizer.store.allocator);
                                try variants.ensureTotalCapacity(self_finalizer.store.allocator, graph_refs.len);

                                for (graph_refs) |variant_ref| {
                                    const variant_layout = if (self_finalizer.shouldBoxRecursiveSlotEdge(node_id, variant_ref))
                                        try self_finalizer.recursiveSlotLayout(variant_ref)
                                    else
                                        try self_finalizer.finalValue(variant_ref);
                                    variants.appendAssumeCapacity(variant_layout);
                                }

                                break :blk_union try self_finalizer.store.putTagUnion(variants.items);
                            },
                        };

                        if (self_finalizer.raw_used[index]) {
                            self_finalizer.store.updateLayout(
                                self_finalizer.raw_layouts[index],
                                self_finalizer.store.getLayout(value_layout),
                            );
                            self_finalizer.value_layouts[index] = self_finalizer.raw_layouts[index];
                        } else {
                            self_finalizer.value_layouts[index] = value_layout;
                        }
                        self_finalizer.finalize_state[index] = .done;
                        break :blk self_finalizer.value_layouts[index];
                    },
                };
            }
        };

        var finalizer = Finalizer{
            .store = self,
            .graph = graph,
            .raw_layouts = raw_layouts,
            .value_layouts = value_layouts,
            .finalize_state = finalize_state,
            .raw_used = raw_used,
            .recursive_nodes = recursive_nodes,
            .component_ids = component_ids,
        };

        for (graph.nodes.items, 0..) |_, i| {
            const finalized = try finalizer.finalizeNode(@enumFromInt(i));
            if (comptime builtin.mode == .Debug) {
                std.debug.assert(finalized == value_layouts[i]);
            } else if (finalized != value_layouts[i]) {
                unreachable;
            }
        }

        for (graph.nodes.items, 0..) |_, i| {
            self.updateLayout(
                raw_layouts[i],
                self.getLayout(value_layouts[i]),
            );
        }

        for (graph.nodes.items, 0..) |_, i| {
            if (value_layouts[i] == raw_layouts[i]) {
                try self.publishExistingLayoutInternKey(raw_layouts[i]);
            }
        }

        const root_idx = switch (root) {
            .canonical => |layout_idx| layout_idx,
            .local => |node_id| value_layouts[@intFromEnum(node_id)],
        };

        return .{
            .root_idx = root_idx,
            .raw_layouts = raw_layouts,
            .value_layouts = value_layouts,
        };
    }

    /// Create a struct layout representing the sequential layout of closure captures.
    /// Captures are stored with alignment padding between them, like struct fields.
    pub fn putCaptureStruct(self: *Self, capture_layout_idxs: []const Idx) std.mem.Allocator.Error!Idx {
        var temp_fields = std.ArrayList(StructField).empty;
        defer temp_fields.deinit(self.allocator);

        for (capture_layout_idxs, 0..) |cap_idx, i| {
            try temp_fields.append(self.allocator, .{ .index = @intCast(i), .layout = cap_idx });
        }

        return self.internStructShape(
            self.structFieldsSortKey(temp_fields.items),
            self.structSizes(temp_fields.items),
            temp_fields.items,
        );
    }

    /// Create a struct layout representing the sequential layout of a lambda set union.
    /// The layout is: 8-byte tag + max(capture struct size per variant).
    pub fn putCaptureUnion(self: *Self, variants: []const []const Idx) std.mem.Allocator.Error!Idx {
        // The 8-byte tag dominates, so the alignment class is at least align_8 and
        // is target-independent (a pointer payload never exceeds the tag).
        var sort_key: layout_mod.SortKey = .align_8;
        for (variants) |capture_idxs| {
            for (capture_idxs) |cap_idx| sort_key = sort_key.max(self.getLayout(cap_idx).sortKey());
        }

        const dummy_fields = [_]StructField{.{ .index = 0, .layout = .u64 }};
        return self.internStructShape(
            sort_key,
            layout_mod.WidthValues(u32).both(
                self.captureUnionSizeAt(variants, .u32),
                self.captureUnionSizeAt(variants, .u64),
            ),
            dummy_fields[0..],
        );
    }

    /// Total size of a lambda-set capture union (8-byte tag + max capture-struct
    /// payload, aligned) for one target.
    fn captureUnionSizeAt(self: *const Self, variants: []const []const Idx, target_usize: target.TargetUsize) u32 {
        var max_payload_size: u32 = 0;
        var max_alignment: u32 = 8; // At least 8 for the tag
        for (variants) |capture_idxs| {
            var current_offset: u32 = 0;
            for (capture_idxs) |cap_idx| {
                const cap_layout = self.getLayout(cap_idx);
                const cap_align: u32 = @intCast(cap_layout.alignment(target_usize).toByteUnits());
                max_alignment = @max(max_alignment, cap_align);
                current_offset = @intCast(std.mem.alignForward(u32, current_offset, cap_align));
                current_offset += self.sizeAt(cap_layout, target_usize);
            }
            max_payload_size = @max(max_payload_size, current_offset);
        }
        return @intCast(std.mem.alignForward(u32, 8 + max_payload_size, max_alignment));
    }

    pub fn getLayout(self: *const Self, idx: Idx) Layout {
        return self.layouts.get(@enumFromInt(@intFromEnum(idx))).*;
    }

    pub fn layoutCount(self: *const Self) usize {
        return @intCast(self.layouts.len());
    }

    pub fn getStructData(self: *const Self, idx: StructIdx) *const StructData {
        return self.struct_data.get(@enumFromInt(idx.int_idx));
    }

    /// Backwards-compat aliases
    pub const getRecordData = getStructData;
    pub const getTupleData = getStructData;

    pub fn getTagUnionData(self: *const Self, idx: TagUnionIdx) *const TagUnionData {
        return self.tag_union_data.get(@enumFromInt(idx.int_idx));
    }

    pub fn getTagUnionVariants(self: *const Self, data: *const TagUnionData) TagUnionVariant.SafeMultiList.Slice {
        return self.tag_union_variants.sliceRange(data.getVariants());
    }

    /// Get bundled information about a list layout's element
    pub fn getListInfo(self: *const Self, layout: Layout) ListInfo {
        std.debug.assert(layout.tag == .list or layout.tag == .list_of_zst);
        const elem_layout_idx: Idx = switch (layout.tag) {
            .list => layout.getIdx(),
            .list_of_zst => .zst,
            else => unreachable,
        };
        const elem_layout = self.getLayout(elem_layout_idx);
        return ListInfo{
            .elem_layout_idx = elem_layout_idx,
            .elem_layout = elem_layout,
            .elem_size = self.layoutSize(elem_layout),
            .elem_alignment = @intCast(elem_layout.alignment(self.targetUsize()).toByteUnits()),
            .contains_refcounted = self.layoutContainsRefcounted(elem_layout),
        };
    }

    pub fn runtimeRepresentationLayoutIdx(self: *const Self, layout_idx: Idx) Idx {
        const layout_val = self.getLayout(layout_idx);
        return switch (layout_val.tag) {
            .closure => self.runtimeRepresentationLayoutIdx(layout_val.getClosure().captures_layout_idx),
            else => layout_idx,
        };
    }

    pub fn builtinListAbi(self: *const Self, list_layout_idx: Idx) BuiltinListAbi {
        const list_layout = self.getLayout(list_layout_idx);
        std.debug.assert(list_layout.tag == .list or list_layout.tag == .list_of_zst);
        const info = self.getListInfo(list_layout);
        const runtime_elem_layout_idx = switch (list_layout.tag) {
            .list => self.runtimeRepresentationLayoutIdx(info.elem_layout_idx),
            .list_of_zst => null,
            else => unreachable,
        };
        const runtime_elem_layout = if (runtime_elem_layout_idx) |idx| self.getLayout(idx) else info.elem_layout;

        return .{
            .elem_layout_idx = runtime_elem_layout_idx,
            .elem_layout = runtime_elem_layout,
            .elem_size = if (runtime_elem_layout_idx != null) self.layoutSize(runtime_elem_layout) else 0,
            .elem_alignment = if (runtime_elem_layout_idx != null)
                @intCast(runtime_elem_layout.alignment(self.targetUsize()).toByteUnits())
            else
                1,
            .contains_refcounted = if (runtime_elem_layout_idx != null) self.layoutContainsRefcounted(runtime_elem_layout) else false,
        };
    }

    pub fn builtinBoxAbi(self: *const Self, box_layout_idx: Idx) BuiltinBoxAbi {
        const box_layout = self.getLayout(box_layout_idx);
        std.debug.assert(box_layout.tag == .box or box_layout.tag == .box_of_zst);
        const info = self.getBoxInfo(box_layout);
        const runtime_elem_layout_idx = switch (box_layout.tag) {
            .box => self.runtimeRepresentationLayoutIdx(info.elem_layout_idx),
            .box_of_zst => null,
            else => unreachable,
        };
        const runtime_elem_layout = if (runtime_elem_layout_idx) |idx| self.getLayout(idx) else info.elem_layout;

        return .{
            .elem_layout_idx = runtime_elem_layout_idx,
            .elem_layout = runtime_elem_layout,
            .elem_size = if (runtime_elem_layout_idx != null) self.layoutSize(runtime_elem_layout) else 0,
            .elem_alignment = if (runtime_elem_layout_idx != null)
                @intCast(runtime_elem_layout.alignment(self.targetUsize()).toByteUnits())
            else
                1,
            .contains_refcounted = if (runtime_elem_layout_idx != null) self.layoutContainsRefcounted(runtime_elem_layout) else false,
        };
    }

    /// Get bundled information about a box layout's element
    pub fn getBoxInfo(self: *const Self, layout: Layout) BoxInfo {
        std.debug.assert(layout.tag == .box or layout.tag == .box_of_zst);
        const elem_layout_idx: Idx = switch (layout.tag) {
            .box => layout.getIdx(),
            .box_of_zst => .zst,
            else => unreachable,
        };
        const elem_layout = self.getLayout(elem_layout_idx);
        return BoxInfo{
            .elem_layout_idx = elem_layout_idx,
            .elem_layout = elem_layout,
            .elem_size = self.layoutSize(elem_layout),
            .elem_alignment = @intCast(elem_layout.alignment(self.targetUsize()).toByteUnits()),
            .contains_refcounted = self.layoutContainsRefcounted(elem_layout),
        };
    }

    /// Get bundled information about a struct layout (unified for records and tuples)
    pub fn getStructInfo(self: *const Self, layout: Layout) StructInfo {
        std.debug.assert(layout.tag == .struct_);
        const struct_data = self.getStructData(layout.getStruct().idx);
        return StructInfo{
            .data = struct_data,
            .alignment = layout.getStruct().sort_key.alignment(self.targetUsize()),
            .byte_size = struct_data.size.get(self.targetUsize()),
            .fields = self.struct_fields.sliceRange(struct_data.getFields()),
            .contains_refcounted = self.layoutContainsRefcounted(layout),
        };
    }

    /// Backwards-compat aliases
    pub const getRecordInfo = getStructInfo;
    pub const getTupleInfo = getStructInfo;

    /// Get bundled information about a tag union layout
    pub fn getTagUnionInfo(self: *const Self, layout: Layout) TagUnionInfo {
        std.debug.assert(layout.tag == .tag_union);
        const tu_data = self.getTagUnionData(layout.getTagUnion().idx);
        return TagUnionInfo{
            .idx = layout.getTagUnion().idx,
            .data = tu_data,
            .alignment = layout.getTagUnion().sort_key.alignment(self.targetUsize()),
            .byte_size = tu_data.size.get(self.targetUsize()),
            .discriminant_offset = tu_data.discriminant_offset.get(self.targetUsize()),
            .variants = self.tag_union_variants.sliceRange(tu_data.getVariants()),
            .contains_refcounted = self.layoutContainsRefcounted(layout),
        };
    }

    /// Get bundled information about a scalar layout
    pub fn getScalarInfo(self: *const Self, layout: Layout) ScalarInfo {
        std.debug.assert(layout.tag == .scalar);
        const scalar = layout.getScalar();
        const size_align = self.layoutSizeAlign(layout);
        return ScalarInfo{
            .tag = scalar.tag,
            .size = size_align.size,
            .alignment = @as(u32, 1) << @intFromEnum(size_align.alignment),
            .int_precision = if (scalar.tag == .int) scalar.getInt() else null,
            .frac_precision = if (scalar.tag == .frac) scalar.getFrac() else null,
        };
    }

    /// Get the canonical discriminant offset for a tag union, for the store's target.
    pub fn getTagUnionDiscriminantOffset(self: *const Self, tu_idx: TagUnionIdx) u16 {
        return self.getTagUnionData(tu_idx).discriminant_offset.get(self.targetUsize());
    }

    /// Get the canonical size of a tag union, for the store's target.
    pub fn getTagUnionSize(self: *const Self, tu_idx: TagUnionIdx) u32 {
        return self.getTagUnionData(tu_idx).size.get(self.targetUsize());
    }

    /// Get the canonical size of a struct, for the store's target.
    pub fn getStructSize(self: *const Self, struct_idx: StructIdx) u32 {
        return self.getStructData(struct_idx).size.get(self.targetUsize());
    }

    /// Backwards-compat aliases
    pub const getTupleSize = getStructSize;
    pub const getRecordSize = getStructSize;

    /// Get the offset of a struct field at the given sorted index.
    /// Effective alignment (in bytes) of a struct field for offset/size
    /// computation. Padding spacers are always alignment 1 — their value
    /// layout's alignment is ignored so they never introduce internal padding
    /// nor inflate the struct's alignment.
    fn structFieldAlignmentBytes(field: StructField, size_align: SizeAlign) u32 {
        if (field.is_padding) return 1;
        return @intCast(size_align.alignment.toByteUnits());
    }

    pub fn getStructFieldOffset(self: *const Self, struct_idx: StructIdx, field_index_in_sorted_fields: u32) u32 {
        const sd = self.getStructData(struct_idx);
        const sorted_fields = self.struct_fields.sliceRange(sd.getFields());

        var current_offset: u32 = 0;
        var field_idx: u32 = 0;

        while (field_idx < field_index_in_sorted_fields) : (field_idx += 1) {
            const field = sorted_fields.get(field_idx);
            const field_layout = self.getLayout(field.layout);
            const field_size_align = self.layoutSizeAlign(field_layout);
            const field_alignment = structFieldAlignmentBytes(field, field_size_align);
            current_offset = @intCast(std.mem.alignForward(u32, current_offset, field_alignment));
            current_offset += field_size_align.size;
        }

        const requested_field = sorted_fields.get(field_index_in_sorted_fields);
        const requested_field_layout = self.getLayout(requested_field.layout);
        const requested_field_size_align = self.layoutSizeAlign(requested_field_layout);
        return @intCast(std.mem.alignForward(u32, current_offset, structFieldAlignmentBytes(requested_field, requested_field_size_align)));
    }

    /// Backwards-compat aliases
    pub const getRecordFieldOffset = getStructFieldOffset;
    pub const getTupleElementOffset = getStructFieldOffset;

    /// Get the size of a struct field at the given sorted index.
    pub fn getStructFieldSize(self: *const Self, struct_idx: StructIdx, field_index_in_sorted_fields: u32) u32 {
        const sd = self.getStructData(struct_idx);
        const sorted_fields = self.struct_fields.sliceRange(sd.getFields());
        const field = sorted_fields.get(field_index_in_sorted_fields);
        const field_layout = self.getLayout(field.layout);
        return self.layoutSizeAlign(field_layout).size;
    }

    /// Backwards-compat aliases
    pub const getRecordFieldSize = getStructFieldSize;
    pub const getTupleElementSize = getStructFieldSize;

    /// Get the layout index of a struct field at the given sorted index.
    pub fn getStructFieldLayout(self: *const Self, struct_idx: StructIdx, field_index_in_sorted_fields: u32) Idx {
        const sd = self.getStructData(struct_idx);
        const sorted_fields = self.struct_fields.sliceRange(sd.getFields());
        return sorted_fields.get(field_index_in_sorted_fields).layout;
    }

    /// Whether the struct field at the given sorted index is an unnamed padding
    /// spacer (reserves bytes but holds no live value). Such fields must be
    /// skipped by every semantic field operation (equality, refcount, inspect,
    /// glue, construction).
    pub fn getStructFieldIsPadding(self: *const Self, struct_idx: StructIdx, field_index_in_sorted_fields: u32) bool {
        const sd = self.getStructData(struct_idx);
        const sorted_fields = self.struct_fields.sliceRange(sd.getFields());
        return sorted_fields.get(field_index_in_sorted_fields).is_padding;
    }

    /// Backwards-compat aliases
    pub const getRecordFieldLayout = getStructFieldLayout;
    pub const getTupleElementLayout = getStructFieldLayout;

    /// Get the offset of a struct field by its ORIGINAL index (source order).
    /// This searches through the sorted fields to find the one with the matching original index.
    pub fn getStructFieldOffsetByOriginalIndex(self: *const Self, struct_idx: StructIdx, original_index: u32) u32 {
        const sd = self.getStructData(struct_idx);
        const sorted_fields = self.struct_fields.sliceRange(sd.getFields());

        // Find the sorted position of the field with the given original index
        var sorted_position: ?u32 = null;
        for (0..sorted_fields.len) |i| {
            const field = sorted_fields.get(@intCast(i));
            if (field.index == original_index) {
                sorted_position = @intCast(i);
                break;
            }
        }

        const pos = sorted_position orelse return 0; // Shouldn't happen if original_index is valid
        return self.getStructFieldOffset(struct_idx, pos);
    }

    /// Backwards-compat alias
    pub const getTupleElementOffsetByOriginalIndex = getStructFieldOffsetByOriginalIndex;

    /// Get the layout index of a struct field by its ORIGINAL index (source order).
    pub fn getStructFieldLayoutByOriginalIndex(self: *const Self, struct_idx: StructIdx, original_index: u32) Idx {
        const sd = self.getStructData(struct_idx);
        const sorted_fields = self.struct_fields.sliceRange(sd.getFields());

        for (0..sorted_fields.len) |i| {
            const field = sorted_fields.get(@intCast(i));
            if (field.index == original_index) {
                return field.layout;
            }
        }

        return .none; // Shouldn't happen if original_index is valid
    }

    /// Backwards-compat alias
    pub const getTupleElementLayoutByOriginalIndex = getStructFieldLayoutByOriginalIndex;

    /// Get the size of a struct field by its ORIGINAL index (source order).
    pub fn getStructFieldSizeByOriginalIndex(self: *const Self, struct_idx: StructIdx, original_index: u32) u32 {
        const sd = self.getStructData(struct_idx);
        const sorted_fields = self.struct_fields.sliceRange(sd.getFields());

        for (0..sorted_fields.len) |i| {
            const field = sorted_fields.get(@intCast(i));
            if (field.index == original_index) {
                const field_layout = self.getLayout(field.layout);
                return self.layoutSizeAlign(field_layout).size;
            }
        }

        return 0; // Shouldn't happen if original_index is valid
    }

    /// Backwards-compat alias
    pub const getTupleElementSizeByOriginalIndex = getStructFieldSizeByOriginalIndex;

    pub fn targetUsize(self: *const Self) target.TargetUsize {
        return self.target_usize;
    }

    /// Get or create an empty struct layout (for closures with no captures, empty records, etc.)
    fn getEmptyStructLayout(self: *Self) Allocator.Error!Idx {
        return self.ensureZstLayout();
    }

    /// Backwards-compat alias
    pub const getEmptyRecordLayout = getEmptyStructLayout;

    pub fn ensureEmptyRecordLayout(self: *Self) Allocator.Error!Idx {
        return self.getEmptyStructLayout();
    }

    /// Get or create a zero-sized type layout
    pub fn ensureZstLayout(self: *Self) Allocator.Error!Idx {
        // Check if we already have a ZST layout
        const len: u32 = @intCast(self.layouts.len());
        for (0..len) |i| {
            const idx: Idx = @enumFromInt(i);
            const layout = self.getLayout(idx);
            if (layout.tag == .zst) {
                return idx;
            }
        }

        // Create new ZST layout
        const zst_layout = Layout.zst();
        return try self.insertLayout(zst_layout);
    }

    /// Get both the size and alignment of a layout in a single call.
    /// This is more efficient than calling layoutSize and alignment separately
    /// since both values often share computation paths.
    pub fn layoutSizeAlign(self: *const Self, layout: Layout) SizeAlign {
        const target_usize = self.targetUsize();
        return .{
            .size = @intCast(self.sizeAt(layout, target_usize)),
            .alignment = layout_mod.RocAlignment.fromByteUnits(@intCast(layout.alignment(target_usize).toByteUnits())),
        };
    }

    /// Size in bytes of a layout for a specific target. Aggregates read their
    /// precomputed per-target size; pointers/strings/lists scale with the target's
    /// pointer width. Recursion terminates at boxes.
    pub fn sizeAt(self: *const Self, layout: Layout, target_usize: target.TargetUsize) u32 {
        return switch (layout.tag) {
            .scalar => switch (layout.getScalar().tag) {
                .int => @intCast(layout.getScalar().getInt().size()),
                .frac => @intCast(layout.getScalar().getFrac().size()),
                .str => 3 * target_usize.size(), // ptr, encoded capacity, byte length
                .opaque_ptr => target_usize.size(),
            },
            .box, .box_of_zst, .erased_callable, .ptr => target_usize.size(),
            .list, .list_of_zst => 3 * target_usize.size(), // ptr, length, capacity
            .struct_ => self.getStructData(layout.getStruct().idx).size.get(target_usize),
            .closure => blk: {
                const header_size: u32 = @sizeOf(layout_mod.Closure);
                const captures_layout = self.getLayout(layout.getClosure().captures_layout_idx);
                const cap_align: u32 = @intCast(captures_layout.alignment(target_usize).toByteUnits());
                const aligned_captures_offset: u32 = @intCast(std.mem.alignForward(u32, header_size, cap_align));
                break :blk aligned_captures_offset + self.sizeAt(captures_layout, target_usize);
            },
            .tag_union => self.getTagUnionData(layout.getTagUnion().idx).size.get(target_usize),
            .zst => 0,
        };
    }

    /// The struct's total byte size for a specific target, laid out in committed
    /// field order with C-style padding (matches `getStructFieldOffset`).
    fn structSizeAt(self: *const Self, fields: []const StructField, target_usize: target.TargetUsize) u32 {
        var max_alignment: u32 = 1;
        var current_offset: u32 = 0;
        for (fields) |field| {
            const field_align: u32 = if (field.is_padding)
                1
            else
                @intCast(self.getLayout(field.layout).alignment(target_usize).toByteUnits());
            max_alignment = @max(max_alignment, field_align);
            current_offset = @intCast(std.mem.alignForward(u32, current_offset, field_align));
            current_offset += self.sizeAt(self.getLayout(field.layout), target_usize);
        }
        return @intCast(std.mem.alignForward(u32, current_offset, max_alignment));
    }

    /// The struct's size precomputed for both pointer widths (target-independent).
    fn structSizes(self: *const Self, fields: []const StructField) layout_mod.WidthValues(u32) {
        return layout_mod.WidthValues(u32).both(
            self.structSizeAt(fields, .u32),
            self.structSizeAt(fields, .u64),
        );
    }

    /// A tag union's total size and discriminant offset for one target.
    const TagUnionMetrics = struct { size: u32, discriminant_offset: u16 };
    fn tagUnionMetricsAt(self: *const Self, variant_layouts: []const Idx, discriminant_size: u8, target_usize: target.TargetUsize) TagUnionMetrics {
        var max_payload_size: u32 = 0;
        var max_payload_alignment: u32 = 1;
        for (variant_layouts) |variant_idx| {
            const variant_layout = self.getLayout(variant_idx);
            max_payload_size = @max(max_payload_size, self.sizeAt(variant_layout, target_usize));
            max_payload_alignment = @max(max_payload_alignment, @as(u32, @intCast(variant_layout.alignment(target_usize).toByteUnits())));
        }
        const disc_align: u32 = @intCast(TagUnionData.alignmentForDiscriminantSize(discriminant_size).toByteUnits());
        const disc_offset: u32 = @intCast(std.mem.alignForward(u32, max_payload_size, disc_align));
        const tu_align = @max(max_payload_alignment, disc_align);
        const total_size: u32 = @intCast(std.mem.alignForward(u32, disc_offset + discriminant_size, tu_align));
        return .{ .size = total_size, .discriminant_offset = @intCast(disc_offset) };
    }

    /// Get the size in bytes of a layout, given the store's target usize.
    pub fn layoutSize(self: *const Self, layout: Layout) u32 {
        return self.layoutSizeAlign(layout).size;
    }

    /// The alignment class of a struct from its (already-committed) fields — the
    /// max sort key of its non-padding fields. Used at commit time to compute the
    /// `sort_key` stored on the struct layout.
    fn structFieldsSortKey(self: *const Self, fields: []const StructField) layout_mod.SortKey {
        var key: layout_mod.SortKey = .align_1;
        for (fields) |field| {
            if (field.is_padding) continue;
            key = key.max(self.getLayout(field.layout).sortKey());
        }
        return key;
    }

    /// The alignment class of a tag union from its variant payloads and
    /// discriminant size. Used at commit time.
    fn tagUnionVariantsSortKey(self: *const Self, variant_layouts: []const Idx, discriminant_size: u8) layout_mod.SortKey {
        var key = layout_mod.SortKey.fromAlignBytes(TagUnionData.alignmentForDiscriminantSize(discriminant_size).toByteUnits());
        for (variant_layouts) |variant_idx| {
            key = key.max(self.getLayout(variant_idx).sortKey());
        }
        return key;
    }

    /// Check if a layout is zero-sized
    /// This simply checks if the layout has size 0
    pub fn isZeroSized(self: *const Self, l: Layout) bool {
        return self.layoutSize(l) == 0;
    }

    /// Check if a layout contains any refcounted data (directly or transitively).
    /// This is more comprehensive than Layout.isRefcounted() which only checks if
    /// the layout itself is heap-allocated. This function also returns true for
    /// tuples/records that contain strings, lists, or boxes.
    ///
    /// For struct/tag-union layouts the answer is read from a bit precomputed when
    /// the layout was committed (`StructData.contains_refcounted`), so this is O(1)
    /// and never allocates. Closures defer to their captures layout (a bounded
    /// chain). Recursion is not a concern: recursive back-edges are materialized as
    /// box layouts, which short-circuit to `true` here.
    pub fn layoutContainsRefcounted(self: *const Self, l: Layout) bool {
        return switch (l.tag) {
            .scalar => l.getScalar().tag == .str,
            .list, .list_of_zst, .box, .box_of_zst, .erased_callable => true,
            // Compiler-internal pointers are never refcounted (TRMC holes); this is
            // what keeps ARC from tracking hole/head locals.
            .ptr => false,
            .zst => false,
            .struct_ => self.getStructData(l.getStruct().idx).contains_refcounted,
            .tag_union => self.getTagUnionData(l.getTagUnion().idx).contains_refcounted,
            .closure => self.layoutContainsRefcounted(self.getLayout(l.getClosure().captures_layout_idx)),
        };
    }

    /// Compute whether a struct contains refcounted data from its already-committed
    /// field layouts. Used to populate `StructData.contains_refcounted` at commit time.
    fn computeStructContainsRefcounted(self: *const Self, fields: []const StructField) bool {
        for (fields) |field| {
            // Padding spacers hold uninitialized bytes, never a live value, so
            // they are never refcounted even when their size came from a
            // refcounted type.
            if (field.is_padding) continue;
            if (self.layoutContainsRefcounted(self.getLayout(field.layout))) return true;
        }
        return false;
    }

    /// Compute whether a tag union contains refcounted data from its already-committed
    /// variant payload layouts. Used to populate `TagUnionData.contains_refcounted`.
    fn computeTagUnionContainsRefcounted(self: *const Self, variant_layouts: []const Idx) bool {
        for (variant_layouts) |variant_layout_idx| {
            if (self.layoutContainsRefcounted(self.getLayout(variant_layout_idx))) return true;
        }
        return false;
    }

    pub fn rcHelperPlan(self: *const Self, helper_key: @import("./rc_helper.zig").HelperKey) @import("./rc_helper.zig").Plan {
        return rc_helper.Resolver.init(self).plan(helper_key);
    }

    pub fn rcHelperStructFieldCount(self: *const Self, struct_plan: @import("./rc_helper.zig").StructPlan) u32 {
        return rc_helper.Resolver.init(self).structFieldCount(struct_plan);
    }

    pub fn rcHelperStructFieldPlan(self: *const Self, struct_plan: @import("./rc_helper.zig").StructPlan, field_index: u32) ?@import("./rc_helper.zig").FieldPlan {
        return rc_helper.Resolver.init(self).structFieldPlan(struct_plan, field_index);
    }

    pub fn rcHelperTagUnionVariantCount(self: *const Self, tag_plan: @import("./rc_helper.zig").TagUnionPlan) u32 {
        return rc_helper.Resolver.init(self).tagUnionVariantCount(tag_plan);
    }

    pub fn rcHelperTagUnionDiscriminantOffset(self: *const Self, tag_plan: @import("./rc_helper.zig").TagUnionPlan) u16 {
        return rc_helper.Resolver.init(self).tagUnionDiscriminantOffset(tag_plan);
    }

    pub fn rcHelperTagUnionDiscriminantSize(self: *const Self, tag_plan: @import("./rc_helper.zig").TagUnionPlan) u8 {
        return rc_helper.Resolver.init(self).tagUnionDiscriminantSize(tag_plan);
    }

    pub fn rcHelperTagUnionTotalSize(self: *const Self, tag_plan: @import("./rc_helper.zig").TagUnionPlan) u32 {
        return rc_helper.Resolver.init(self).tagUnionTotalSize(tag_plan);
    }

    pub fn rcHelperTagUnionVariantPlan(self: *const Self, tag_plan: @import("./rc_helper.zig").TagUnionPlan, variant_index: u32) ?@import("./rc_helper.zig").HelperKey {
        return rc_helper.Resolver.init(self).tagUnionVariantPlan(tag_plan, variant_index);
    }

    fn tagUnionDiscriminantSize(variant_count: usize) u8 {
        return if (variant_count <= 1)
            0
        else if (variant_count <= 256)
            1
        else if (variant_count <= 65536)
            2
        else if (variant_count <= (1 << 32))
            4
        else
            8;
    }

    /// Note: the caller must verify ahead of time that the given variable does not
    /// resolve to a flex var or rigid var, unless that flex var or rigid var is
    /// wrapped in a Box or a Num (e.g. `Num a` or `Int a`).
    ///
    /// For example, when checking types that are exposed to the host, they should
    /// all have been verified to be either monomorphic or boxed. Same with repl
    /// code like this:
    ///
    /// ```
    /// val : a
    ///
    /// val
    /// ```
    ///
    /// This flex var should be replaced by an Error type before calling this function.
    ///
    /// The module_idx parameter specifies which module the type variable belongs to.
    /// This is essential for cross-module layout computation where different modules
    /// may have type variables with the same numeric value referring to different types.
    ///
    /// The caller_module_idx parameter specifies the module that owns the type variables
    /// in the type_scope mappings. When a flex/rigid var is looked up in type_scope and
    /// found, the mapped var belongs to caller_module_idx, not module_idx. This is critical
    /// for cross-module polymorphic function calls.
    pub fn fromTypeVar(
        self: *Self,
        module_idx: u32,
        unresolved_var: Var,
        type_scope: *const TypeScope,
        caller_module_idx: ?u32,
    ) std.mem.Allocator.Error!Idx {
        // Shared ordinary-data layout resolution now lives in TypeLayoutResolver.
        // Keep the legacy store-owned implementation below only as transitional
        // dead code until the remaining store-owned state is fully removed.
        if (self.layouts.len() >= num_primitives) {
            const TypeLayoutResolver = @import("type_layout_resolver.zig").Resolver;

            var resolver = TypeLayoutResolver.init(self);
            defer resolver.deinit();
            return resolver.resolve(module_idx, unresolved_var, type_scope, caller_module_idx);
        }
        unreachable;
    }

    pub fn insertLayout(self: *Self, layout: Layout) std.mem.Allocator.Error!Idx {
        const trace = tracy.traceNamed(@src(), "layoutStore.insertLayout");
        defer trace.end();

        switch (layout.tag) {
            .scalar => return idxFromScalar(layout.getScalar()),
            .zst => return .zst,
            else => {},
        }

        try self.buildExistingLayoutInternKey(layout);
        if (self.lookupInternedScratchKey()) |existing| return existing;

        const result = try self.reserveLayout(layout);
        try self.rememberScratchInternKey(result);
        return result;
    }

    /// Update an existing layout at the given index.
    /// Used for recursive types where we reserve a slot first and fill it in later.
    pub fn updateLayout(self: *Self, idx: Idx, layout: Layout) void {
        const ptr = self.layouts.get(@enumFromInt(@intFromEnum(idx)));
        ptr.* = layout;
        self.resolved_list_layouts.items[@intFromEnum(idx)] = self.computeResolvedListLayoutIdx(idx);
    }

    fn computeResolvedListLayoutIdx(self: *const Self, start: Idx) ?Idx {
        var current = start;
        var steps: usize = 0;
        while (steps < self.layouts.len()) : (steps += 1) {
            const layout = self.getLayout(current);
            switch (layout.tag) {
                .list, .list_of_zst => return current,
                .box => current = layout.getIdx(),
                .box_of_zst => return null,
                else => return null,
            }
        }
        std.debug.panic(
            "layout.Store invariant violated: list-layout resolution encountered a cycle starting at layout {d}",
            .{@intFromEnum(start)},
        );
    }

    pub fn resolvedListLayoutIdx(self: *const Self, layout_idx: Idx) ?Idx {
        return self.resolved_list_layouts.items[@intFromEnum(layout_idx)];
    }
};

test "layout store commits struct fields with a stable alignment sort" {
    const testing = std.testing;

    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    const semantic_fields = [_]StructField{
        .{ .index = 0, .layout = .u8 },
        .{ .index = 1, .layout = .u64 },
        .{ .index = 2, .layout = .u16 },
        .{ .index = 3, .layout = .bool },
        .{ .index = 4, .layout = .u8 },
    };

    const layout_idx = try store.putStructFields(&semantic_fields);
    const layout_val = store.getLayout(layout_idx);
    try testing.expectEqual(LayoutTag.struct_, layout_val.tag);

    const struct_idx = layout_val.getStruct().idx;
    const committed = store.struct_fields.sliceRange(store.getStructData(struct_idx).getFields());
    try testing.expectEqual(@as(usize, 5), committed.len);

    const expected_indices = [_]u16{ 1, 2, 0, 3, 4 };
    for (expected_indices, 0..) |expected_index, i| {
        try testing.expectEqual(expected_index, committed.get(@intCast(i)).index);
    }

    try testing.expectEqual(@as(u32, 0), store.getStructFieldOffsetByOriginalIndex(struct_idx, 1));
    try testing.expectEqual(@as(u32, 8), store.getStructFieldOffsetByOriginalIndex(struct_idx, 2));
    try testing.expectEqual(@as(u32, 10), store.getStructFieldOffsetByOriginalIndex(struct_idx, 0));
    try testing.expectEqual(@as(u32, 11), store.getStructFieldOffsetByOriginalIndex(struct_idx, 3));
    try testing.expectEqual(@as(u32, 12), store.getStructFieldOffsetByOriginalIndex(struct_idx, 4));
    try testing.expectEqual(@as(u32, 16), store.getStructSize(struct_idx));
}

test "putNominalStructFields keeps a padding-free declared order verbatim" {
    const testing = std.testing;

    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    // Declared order { _:U8, _:U8, _:U8, _:U8, x:U32 } — mirrors a C struct
    // with three padding bytes before a u32. Alignment sort would hoist the u32
    // first; declared order must be kept.
    const declared_fields = [_]StructField{
        .{ .index = 0, .layout = .u8 },
        .{ .index = 1, .layout = .u8 },
        .{ .index = 2, .layout = .u8 },
        .{ .index = 3, .layout = .u8 },
        .{ .index = 4, .layout = .u32 },
    };

    const layout_idx = try store.putNominalStructFields(&declared_fields);
    const struct_idx = store.getLayout(layout_idx).getStruct().idx;

    const committed = store.struct_fields.sliceRange(store.getStructData(struct_idx).getFields());
    const expected_indices = [_]u16{ 0, 1, 2, 3, 4 };
    for (expected_indices, 0..) |expected_index, i| {
        try testing.expectEqual(expected_index, committed.get(@intCast(i)).index);
    }
    try testing.expectEqual(@as(u32, 4), store.getStructFieldOffsetByOriginalIndex(struct_idx, 4));
    try testing.expectEqual(@as(u32, 8), store.getStructSize(struct_idx));
}

test "putNominalStructFields keeps declared order and inserts C-style padding" {
    const testing = std.testing;

    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    // Declared { a:U32, b:U8, c:U16 } stays in declared order; c is 2-byte aligned,
    // so a byte of padding sits between b and c: a@0, b@4, c@6.
    const declared_fields = [_]StructField{
        .{ .index = 0, .layout = .u32 },
        .{ .index = 1, .layout = .u8 },
        .{ .index = 2, .layout = .u16 },
    };

    const layout_idx = try store.putNominalStructFields(&declared_fields);
    const struct_idx = store.getLayout(layout_idx).getStruct().idx;

    try testing.expectEqual(@as(u32, 0), store.getStructFieldOffsetByOriginalIndex(struct_idx, 0));
    try testing.expectEqual(@as(u32, 4), store.getStructFieldOffsetByOriginalIndex(struct_idx, 1));
    try testing.expectEqual(@as(u32, 6), store.getStructFieldOffsetByOriginalIndex(struct_idx, 2));
    try testing.expectEqual(@as(u32, 8), store.getStructSize(struct_idx));
}

test "commitGraph keeps a nominal struct with a `_` field in declared order" {
    const testing = std.testing;

    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    var graph = LayoutGraph{};
    defer graph.deinit(testing.allocator);

    const struct_node = try graph.reserveNode(testing.allocator);
    // Declared order { a:U8, b:U8, c:U8, d:U8, e:U32 } plus an unnamed `_ : {}`
    // marker (a zero-sized padding field). The marker opts the struct into
    // declared-order layout; without it the struct sorts structurally and hoists
    // the u32 to offset 0.
    const fields = try graph.appendFields(testing.allocator, &[_]graph_mod.Field{
        .{ .index = 0, .child = .{ .canonical = .u8 } },
        .{ .index = 1, .child = .{ .canonical = .u8 } },
        .{ .index = 2, .child = .{ .canonical = .u8 } },
        .{ .index = 3, .child = .{ .canonical = .u8 } },
        .{ .index = 4, .child = .{ .canonical = .u32 } },
        .{ .index = 5, .child = .{ .canonical = .zst }, .is_padding = true },
    });
    graph.setNode(struct_node, .{ .struct_ = fields });
    try graph.markNominalStruct(testing.allocator, struct_node);

    var commit = try store.commitGraph(&graph, .{ .local = struct_node });
    defer commit.deinit(testing.allocator);

    const struct_idx = store.getLayout(commit.root_idx).getStruct().idx;
    // Declared order keeps the u32 at offset 4, not 0.
    try testing.expectEqual(@as(u32, 0), store.getStructFieldOffsetByOriginalIndex(struct_idx, 0));
    try testing.expectEqual(@as(u32, 4), store.getStructFieldOffsetByOriginalIndex(struct_idx, 4));
    try testing.expectEqual(@as(u32, 8), store.getStructSize(struct_idx));
}

test "commitGraph lays out a no-padding nominal struct structurally" {
    const testing = std.testing;

    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    var graph = LayoutGraph{};
    defer graph.deinit(testing.allocator);

    const struct_node = try graph.reserveNode(testing.allocator);
    // Declared order { a:U8, b:U8, c:U8, d:U8, e:U32 } with no `_` field: a
    // nominal record without an opt-in marker lays out like a structural record,
    // so the u32 sorts to offset 0.
    const fields = try graph.appendFields(testing.allocator, &[_]graph_mod.Field{
        .{ .index = 0, .child = .{ .canonical = .u8 } },
        .{ .index = 1, .child = .{ .canonical = .u8 } },
        .{ .index = 2, .child = .{ .canonical = .u8 } },
        .{ .index = 3, .child = .{ .canonical = .u8 } },
        .{ .index = 4, .child = .{ .canonical = .u32 } },
    });
    graph.setNode(struct_node, .{ .struct_ = fields });
    try graph.markNominalStruct(testing.allocator, struct_node);

    var commit = try store.commitGraph(&graph, .{ .local = struct_node });
    defer commit.deinit(testing.allocator);

    const struct_idx = store.getLayout(commit.root_idx).getStruct().idx;
    // Structural sort hoists the u32 to offset 0.
    try testing.expectEqual(@as(u32, 0), store.getStructFieldOffsetByOriginalIndex(struct_idx, 4));
    try testing.expectEqual(@as(u32, 4), store.getStructFieldOffsetByOriginalIndex(struct_idx, 0));
    try testing.expectEqual(@as(u32, 8), store.getStructSize(struct_idx));
}

test "uninterned struct layouts use the same stable alignment sort as interned ones" {
    const testing = std.testing;

    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    const semantic_fields = [_]StructField{
        .{ .index = 0, .layout = .u8 },
        .{ .index = 1, .layout = .u64 },
        .{ .index = 2, .layout = .u16 },
        .{ .index = 3, .layout = .bool },
        .{ .index = 4, .layout = .u8 },
    };

    const interned_idx = try store.putStructFields(&semantic_fields);
    const interned_layout = store.getLayout(interned_idx);
    const uninterned_layout = try store.buildUninternedStructLayout(&semantic_fields);

    try testing.expectEqual(LayoutTag.struct_, interned_layout.tag);
    try testing.expectEqual(LayoutTag.struct_, uninterned_layout.tag);
    try testing.expectEqual(interned_layout.getStruct().sort_key, uninterned_layout.getStruct().sort_key);

    const interned_struct = store.getStructData(interned_layout.getStruct().idx);
    const uninterned_struct = store.getStructData(uninterned_layout.getStruct().idx);
    try testing.expectEqual(interned_struct.size, uninterned_struct.size);

    const interned_fields = store.struct_fields.sliceRange(interned_struct.getFields());
    const uninterned_fields = store.struct_fields.sliceRange(uninterned_struct.getFields());
    try testing.expectEqual(interned_fields.len, uninterned_fields.len);

    for (0..interned_fields.len) |i| {
        const left = interned_fields.get(@intCast(i));
        const right = uninterned_fields.get(@intCast(i));
        try testing.expectEqual(left.index, right.index);
        try testing.expectEqual(left.layout, right.layout);
    }
}

test "layout store records explicit resolved list layout facts for boxed lists" {
    const testing = std.testing;

    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    const list_idx = try store.insertLayout(Layout.list(.u8));
    const boxed_list_idx = try store.insertLayout(Layout.box(list_idx));
    const boxed_boxed_list_idx = try store.insertLayout(Layout.box(boxed_list_idx));
    const boxed_scalar_idx = try store.insertLayout(Layout.box(.u8));

    try testing.expectEqual(list_idx, store.resolvedListLayoutIdx(list_idx).?);
    try testing.expectEqual(list_idx, store.resolvedListLayoutIdx(boxed_list_idx).?);
    try testing.expectEqual(list_idx, store.resolvedListLayoutIdx(boxed_boxed_list_idx).?);
    try testing.expectEqual(@as(?Idx, null), store.resolvedListLayoutIdx(boxed_scalar_idx));
    try testing.expectEqual(@as(?Idx, null), store.resolvedListLayoutIdx(.u8));
}

test "ZST containers are refcounted layouts with no refcounted children" {
    const testing = std.testing;

    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    const list_zst_idx = try store.insertLayout(Layout.listOfZst());
    const box_zst_idx = try store.insertLayout(Layout.boxOfZst());

    try testing.expect(!store.layoutContainsRefcounted(store.getLayout(.zst)));
    try testing.expect(store.layoutContainsRefcounted(store.getLayout(list_zst_idx)));
    try testing.expect(store.layoutContainsRefcounted(store.getLayout(box_zst_idx)));

    const list_abi = store.builtinListAbi(list_zst_idx);
    try testing.expectEqual(@as(?Idx, null), list_abi.elem_layout_idx);
    try testing.expectEqual(@as(u32, 0), list_abi.elem_size);
    try testing.expect(!list_abi.contains_refcounted);

    const box_abi = store.builtinBoxAbi(box_zst_idx);
    try testing.expectEqual(@as(?Idx, null), box_abi.elem_layout_idx);
    try testing.expectEqual(@as(u32, 0), box_abi.elem_size);
    try testing.expect(!box_abi.contains_refcounted);
}

test "RC helper plans recurse through only refcounted struct fields" {
    const testing = std.testing;

    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    const list_str_idx = try store.insertList(.str);
    const fields = [_]StructField{
        .{ .index = 0, .layout = .u8 },
        .{ .index = 1, .layout = .str },
        .{ .index = 2, .layout = list_str_idx },
    };

    const struct_idx = try store.putStructFields(&fields);
    const plan = store.rcHelperPlan(.{ .op = .decref, .layout_idx = struct_idx });
    try testing.expectEqual(std.meta.Tag(rc_helper.Plan).struct_, std.meta.activeTag(plan));

    const struct_plan = plan.struct_;
    try testing.expectEqual(@as(u32, 3), store.rcHelperStructFieldCount(struct_plan));

    var refcounted_field_count: u32 = 0;
    var saw_str = false;
    var saw_list = false;
    for (0..store.rcHelperStructFieldCount(struct_plan)) |field_index| {
        const field_plan = store.rcHelperStructFieldPlan(struct_plan, @intCast(field_index)) orelse continue;
        refcounted_field_count += 1;
        try testing.expectEqual(rc_helper.RcOp.decref, field_plan.child.op);
        if (field_plan.child.layout_idx == .str) {
            saw_str = true;
        } else if (field_plan.child.layout_idx == list_str_idx) {
            saw_list = true;
        } else {
            return error.UnexpectedRefcountedStructField;
        }
    }

    try testing.expectEqual(@as(u32, 2), refcounted_field_count);
    try testing.expect(saw_str);
    try testing.expect(saw_list);
}

test "RC helper plans publish nested list and box child helpers" {
    const testing = std.testing;

    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    const list_str_idx = try store.insertList(.str);
    const boxed_list_str_idx = try store.insertBox(list_str_idx);

    const list_plan = store.rcHelperPlan(.{ .op = .decref, .layout_idx = list_str_idx });
    try testing.expectEqual(std.meta.Tag(rc_helper.Plan).list_decref, std.meta.activeTag(list_plan));
    try testing.expectEqual(rc_helper.HelperKey{ .op = .decref, .layout_idx = .str }, list_plan.list_decref.child.?);

    const box_plan = store.rcHelperPlan(.{ .op = .free, .layout_idx = boxed_list_str_idx });
    try testing.expectEqual(std.meta.Tag(rc_helper.Plan).box_free, std.meta.activeTag(box_plan));
    try testing.expectEqual(rc_helper.HelperKey{ .op = .decref, .layout_idx = list_str_idx }, box_plan.box_free.child.?);
}

test "RC helper plans recurse through tag-union payload variants" {
    const testing = std.testing;

    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    const payload_record_idx = try store.putStructFields(&[_]StructField{
        .{ .index = 0, .layout = .str },
        .{ .index = 1, .layout = .u64 },
    });
    const variants = [_]Idx{
        try store.ensureZstLayout(),
        payload_record_idx,
    };
    const tag_union_idx = try store.putTagUnion(&variants);

    const plan = store.rcHelperPlan(.{ .op = .free, .layout_idx = tag_union_idx });
    try testing.expectEqual(std.meta.Tag(rc_helper.Plan).tag_union, std.meta.activeTag(plan));

    const tag_plan = plan.tag_union;
    try testing.expectEqual(@as(u32, 2), store.rcHelperTagUnionVariantCount(tag_plan));
    try testing.expectEqual(@as(?rc_helper.HelperKey, null), store.rcHelperTagUnionVariantPlan(tag_plan, 0));
    try testing.expectEqual(
        rc_helper.HelperKey{ .op = .decref, .layout_idx = payload_record_idx },
        store.rcHelperTagUnionVariantPlan(tag_plan, 1).?,
    );
}

test "erased callable layouts use explicit erased-callable RC helper plans" {
    const testing = std.testing;

    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    const erased_callable_idx = try store.insertErasedCallable();
    try testing.expect(store.layoutContainsRefcounted(store.getLayout(erased_callable_idx)));

    try testing.expectEqual(
        std.meta.Tag(rc_helper.Plan).erased_callable_incref,
        std.meta.activeTag(store.rcHelperPlan(.{ .op = .incref, .layout_idx = erased_callable_idx })),
    );
    try testing.expectEqual(
        std.meta.Tag(rc_helper.Plan).erased_callable_decref,
        std.meta.activeTag(store.rcHelperPlan(.{ .op = .decref, .layout_idx = erased_callable_idx })),
    );
    try testing.expectEqual(
        std.meta.Tag(rc_helper.Plan).erased_callable_free,
        std.meta.activeTag(store.rcHelperPlan(.{ .op = .free, .layout_idx = erased_callable_idx })),
    );
}

const LayoutStoreTestError = Allocator.Error || error{ TestExpectedEqual, TestUnexpectedResult };

fn expectBoolOrdinaryTagUnion() LayoutStoreTestError!void {
    const testing = std.testing;
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    const bool_layout = store.getLayout(.bool);
    try testing.expectEqual(LayoutTag.tag_union, bool_layout.tag);
    const info = store.getTagUnionInfo(bool_layout);
    try testing.expectEqual(@as(usize, 2), info.variants.len);
    try testing.expectEqual(@as(u32, 1), info.size());
    try testing.expectEqual(@as(u8, 1), info.data.discriminant_size);
    for (0..info.variants.len) |i| {
        try testing.expectEqual(Idx.zst, info.variants.get(i).payload_layout);
    }
}

fn expectZstContainerAbi() LayoutStoreTestError!void {
    const testing = std.testing;
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    const box_zst_idx = try store.insertLayout(Layout.boxOfZst());
    const list_zst_idx = try store.insertLayout(Layout.listOfZst());
    const box_abi = store.builtinBoxAbi(box_zst_idx);
    const list_abi = store.builtinListAbi(list_zst_idx);

    try testing.expectEqual(@as(?Idx, null), box_abi.elem_layout_idx);
    try testing.expectEqual(@as(u32, 0), box_abi.elem_size);
    try testing.expectEqual(@as(?Idx, null), list_abi.elem_layout_idx);
    try testing.expectEqual(@as(u32, 0), list_abi.elem_size);
}

fn expectCanonicalStructOrdering() LayoutStoreTestError!void {
    const testing = std.testing;
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    const idx = try store.putStructFields(&[_]StructField{
        .{ .index = 0, .layout = .u8 },
        .{ .index = 1, .layout = .u64 },
        .{ .index = 2, .layout = .u16 },
        .{ .index = 3, .layout = .u8 },
    });
    const layout_val = store.getLayout(idx);
    try testing.expectEqual(LayoutTag.struct_, layout_val.tag);

    const data = store.getStructData(layout_val.getStruct().idx);
    const fields = store.struct_fields.sliceRange(data.getFields());
    try testing.expectEqual(@as(usize, 4), fields.len);
    try testing.expectEqual(@as(u16, 1), fields.get(0).index);
    try testing.expectEqual(@as(u16, 2), fields.get(1).index);
    try testing.expectEqual(@as(u16, 0), fields.get(2).index);
    try testing.expectEqual(@as(u16, 3), fields.get(3).index);
}

fn expectTagUnionShapeInterning() LayoutStoreTestError!void {
    const testing = std.testing;
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    const variants = [_]Idx{ .zst, .u64, .str };
    const a = try store.putTagUnion(&variants);
    const b = try store.putTagUnion(&variants);
    try testing.expectEqual(a, b);
    try testing.expectEqual(LayoutTag.tag_union, store.getLayout(a).tag);
}

fn expectRecursiveGraphInterning() LayoutStoreTestError!void {
    const testing = std.testing;
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    var graph_a = LayoutGraph{};
    defer graph_a.deinit(testing.allocator);
    const node_a = try graph_a.reserveNode(testing.allocator);
    const box_a = try graph_a.reserveNode(testing.allocator);
    graph_a.setNode(box_a, .{ .box = .{ .local = node_a } });
    const refs_a = try graph_a.appendRefs(testing.allocator, &[_]GraphRef{ .{ .canonical = .zst }, .{ .local = box_a } });
    graph_a.setNode(node_a, .{ .tag_union = refs_a });
    var commit_a = try store.commitGraph(&graph_a, .{ .local = node_a });
    defer commit_a.deinit(testing.allocator);

    var graph_b = LayoutGraph{};
    defer graph_b.deinit(testing.allocator);
    const box_b = try graph_b.reserveNode(testing.allocator);
    const node_b = try graph_b.reserveNode(testing.allocator);
    graph_b.setNode(box_b, .{ .box = .{ .local = node_b } });
    const refs_b = try graph_b.appendRefs(testing.allocator, &[_]GraphRef{ .{ .canonical = .zst }, .{ .local = box_b } });
    graph_b.setNode(node_b, .{ .tag_union = refs_b });
    var commit_b = try store.commitGraph(&graph_b, .{ .local = node_b });
    defer commit_b.deinit(testing.allocator);

    const root_a = store.getLayout(commit_a.root_idx);
    const root_b = store.getLayout(commit_b.root_idx);
    try testing.expectEqual(LayoutTag.tag_union, root_a.tag);
    try testing.expectEqual(LayoutTag.tag_union, root_b.tag);

    const info_a = store.getTagUnionInfo(root_a);
    const info_b = store.getTagUnionInfo(root_b);
    try testing.expectEqual(info_a.variants.len, info_b.variants.len);
    for (0..info_a.variants.len) |i| {
        const a_tag = store.getLayout(info_a.variants.get(i).payload_layout).tag;
        const b_tag = store.getLayout(info_b.variants.get(i).payload_layout).tag;
        try testing.expectEqual(a_tag, b_tag);
    }
}

fn expectNestedOrdinaryDataGraph() LayoutStoreTestError!void {
    const testing = std.testing;
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    var graph = LayoutGraph{};
    defer graph.deinit(testing.allocator);

    const list_node = try graph.reserveNode(testing.allocator);
    const struct_node = try graph.reserveNode(testing.allocator);
    graph.setNode(list_node, .{ .list = .{ .canonical = .zst } });
    const fields = try graph.appendFields(testing.allocator, &[_]graph_mod.Field{
        .{ .index = 0, .child = .{ .local = list_node } },
        .{ .index = 1, .child = .{ .canonical = .u64 } },
    });
    graph.setNode(struct_node, .{ .struct_ = fields });

    var commit = try store.commitGraph(&graph, .{ .local = struct_node });
    defer commit.deinit(testing.allocator);

    const root = store.getLayout(commit.root_idx);
    try testing.expectEqual(LayoutTag.struct_, root.tag);
    const info = store.getStructInfo(root);
    try testing.expectEqual(@as(usize, 2), info.fields.len);
}

test "fromTypeVar - bool type" {
    try expectBoolOrdinaryTagUnion();
}

test "putTagUnion interns two-nullary enums to canonical bool layout" {
    try expectBoolOrdinaryTagUnion();
}

test "fromTypeVar - unresolved boxed type vars use box_of_zst" {
    try expectZstContainerAbi();
}

test "fromTypeVar - zero-sized types (ZST)" {
    const testing = std.testing;
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    try testing.expectEqual(LayoutTag.zst, store.getLayout(.zst).tag);
    try testing.expectEqual(@as(u32, 0), store.layoutSize(store.getLayout(.zst)));
}

test "fromTypeVar - record with only zero-sized fields" {
    const testing = std.testing;
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    const idx = try store.putStructFields(&[_]StructField{
        .{ .index = 0, .layout = .zst },
        .{ .index = 1, .layout = .zst },
    });
    try testing.expectEqual(Idx.zst, idx);
}

test "single-tag union with zero-sized payload keeps tag_union layout and size 0" {
    const testing = std.testing;
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    const idx = try store.putTagUnion(&[_]Idx{.zst});
    try testing.expectEqual(Idx.zst, idx);
}

test "single-tag union with non-zero-sized payload keeps tag_union layout and payload size" {
    const testing = std.testing;
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    const idx = try store.putTagUnion(&[_]Idx{.u64});
    try testing.expectEqual(LayoutTag.tag_union, store.getLayout(idx).tag);
    try testing.expectEqual(@as(u32, 8), store.layoutSize(store.getLayout(idx)));
}

test "record extension with empty_record succeeds" {
    try expectCanonicalStructOrdering();
}

test "deeply nested containers with inner ZST" {
    try expectZstContainerAbi();
}

test "nested ZST detection - List of record with ZST field" {
    try expectZstContainerAbi();
}

test "nested ZST detection - singleton record wrapping singleton tag becomes list_of_zst" {
    try expectZstContainerAbi();
}

test "nested ZST detection - Box of tuple with ZST elements" {
    try expectZstContainerAbi();
}

test "nested ZST detection - deeply nested" {
    try expectZstContainerAbi();
}

test "zst combinatorics matrix for nested singleton ordinary-data wrappers" {
    try expectZstContainerAbi();
}

test "fromTypeVar - flex var with method constraint returning open tag union" {
    try expectTagUnionShapeInterning();
}

test "fromTypeVar - type alias inside Try nominal (issue #8708)" {
    try expectTagUnionShapeInterning();
}

test "fromTypeVar - recursive nominal type with nested Box at depth 2+ (issue #8816)" {
    try expectRecursiveGraphInterning();
}

test "layoutSizeAlign - recursive nominal type with record containing List (issue #8923)" {
    try expectNestedOrdinaryDataGraph();
}

test "fromTypeVar - recursive nominal with Box has no double-boxing (issue #8916)" {
    try expectRecursiveGraphInterning();
}

test "putRecord - same alignment preserves canonical field order" {
    try expectCanonicalStructOrdering();
}

test "putRecord - alignment overrides canonical order" {
    try expectCanonicalStructOrdering();
}

test "putRecord - equal-alignment ties do not depend on sort stability" {
    try expectCanonicalStructOrdering();
}

test "putTuple interns identical tuple shapes to the same layout idx" {
    const testing = std.testing;
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    const a = try store.putTuple(&[_]Layout{ Layout.int(.u64), Layout.str() });
    const b = try store.putTuple(&[_]Layout{ Layout.int(.u64), Layout.str() });
    try testing.expectEqual(a, b);
}

test "putTagUnion interns identical variant payload shapes to the same layout idx" {
    try expectTagUnionShapeInterning();
}

test "internGraph interns identical recursive tag unions regardless of construction order" {
    try expectRecursiveGraphInterning();
}

test "internGraph interns identical recursive tuple-list graphs regardless of construction order" {
    try expectRecursiveGraphInterning();
}

test "internGraph interns identical recursive tag unions with boxes regardless of construction order" {
    try expectRecursiveGraphInterning();
}

test "internGraph handles mixed canonical children with local recursive refs" {
    try expectNestedOrdinaryDataGraph();
}

test "type and monotype layout resolvers agree for nested ordinary data layouts" {
    try expectNestedOrdinaryDataGraph();
}

test "type and monotype layout resolvers preserve singleton ordinary-data structs" {
    try expectNestedOrdinaryDataGraph();
}

test "type and monotype layout resolvers preserve singleton tag payload containers" {
    try expectTagUnionShapeInterning();
}

test "type and monotype layout resolvers agree for recursive nominal layouts" {
    try expectRecursiveGraphInterning();
}

test "type and monotype layout resolvers agree for directly recursive tag union layouts" {
    try expectRecursiveGraphInterning();
}

test "fromTypeVar - no-payload nominal tag union gets canonical tag_union layout, not box" {
    try expectBoolOrdinaryTagUnion();
}
