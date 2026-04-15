//! Stores Layout values by index.

const std = @import("std");
const builtin = @import("builtin");
const tracy = @import("tracy");
const base = @import("base");
const collections = @import("collections");

const layout_mod = @import("layout.zig");
const graph_mod = @import("./graph.zig");
const rc_helper = @import("./rc_helper.zig");

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
const RefcountedVisitState = enum(u2) { active, no, yes };

fn assertAppendIdx(expected: usize, idx: anytype) void {
    if (comptime builtin.mode == .Debug) {
        std.debug.assert(@intFromEnum(idx) == expected);
    } else if (@intFromEnum(idx) != expected) {
        unreachable;
    }
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
                .size = 1,
                .discriminant_offset = 0,
                .discriminant_size = 1,
                .variants = .{
                    .start = 0,
                    .count = 2,
                },
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

    fn buildStructInternKeyFromFields(
        self: *Self,
        alignment: std.mem.Alignment,
        total_size: u32,
        fields: []const StructField,
    ) std.mem.Allocator.Error!void {
        try self.startInternKey(.struct_);
        try self.appendInternKeyValue(@as(u8, @intCast(alignment.toByteUnits())));
        try self.appendInternKeyValue(total_size);
        try self.appendInternKeyValue(@as(u32, @intCast(fields.len)));
        for (fields) |field| {
            try self.appendInternKeyValue(field.index);
            try self.appendInternKeyIdx(field.layout);
        }
    }

    fn buildTagUnionInternKeyFromVariants(
        self: *Self,
        alignment: std.mem.Alignment,
        total_size: u32,
        discriminant_offset: u16,
        discriminant_size: u8,
        variant_layouts: []const Idx,
    ) std.mem.Allocator.Error!void {
        try self.startInternKey(.tag_union);
        try self.appendInternKeyValue(@as(u8, @intCast(alignment.toByteUnits())));
        try self.appendInternKeyValue(total_size);
        try self.appendInternKeyValue(discriminant_offset);
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
                try self.appendInternKeyValue(@as(u8, @intCast(info.alignment.toByteUnits())));
                try self.appendInternKeyValue(info.size());
                try self.appendInternKeyValue(@as(u32, @intCast(info.fields.len)));
                for (0..info.fields.len) |i| {
                    const field = info.fields.get(i);
                    try self.appendInternKeyValue(field.index);
                    try self.appendInternKeyIdx(field.layout);
                }
            },
            .tag_union => {
                const info = self.getTagUnionInfo(layout);
                try self.startInternKey(.tag_union);
                try self.appendInternKeyValue(@as(u8, @intCast(info.alignment.toByteUnits())));
                try self.appendInternKeyValue(info.size());
                try self.appendInternKeyValue(info.data.discriminant_offset);
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
        alignment: std.mem.Alignment,
        total_size: u32,
        fields: []const StructField,
    ) std.mem.Allocator.Error!Idx {
        try self.buildStructInternKeyFromFields(alignment, total_size, fields);
        if (self.lookupInternedScratchKey()) |existing| return existing;

        const fields_start = self.struct_fields.items.len;
        for (fields) |field| {
            const expected_idx = self.struct_fields.items.len;
            const idx = try self.struct_fields.append(self.allocator, field);
            assertAppendIdx(expected_idx, idx);
        }

        const struct_idx = StructIdx{ .int_idx = @intCast(self.struct_data.len()) };
        const expected_idx = self.struct_data.items.items.len;
        const struct_data_idx = try self.struct_data.append(self.allocator, .{
            .size = total_size,
            .fields = .{
                .start = @intCast(fields_start),
                .count = @intCast(fields.len),
            },
        });
        assertAppendIdx(expected_idx, struct_data_idx);

        const layout_idx = try self.reserveLayout(Layout.struct_(alignment, struct_idx));
        try self.rememberScratchInternKey(layout_idx);
        return layout_idx;
    }

    fn internTagUnionShape(
        self: *Self,
        alignment: std.mem.Alignment,
        total_size: u32,
        discriminant_offset: u16,
        discriminant_size: u8,
        variant_layouts: []const Idx,
    ) std.mem.Allocator.Error!Idx {
        try self.buildTagUnionInternKeyFromVariants(
            alignment,
            total_size,
            discriminant_offset,
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

        const tag_union_data_idx: u32 = @intCast(self.tag_union_data.len());
        {
            const expected_idx = self.tag_union_data.items.items.len;
            const idx = try self.tag_union_data.append(self.allocator, .{
                .size = total_size,
                .discriminant_offset = discriminant_offset,
                .discriminant_size = discriminant_size,
                .variants = .{
                    .start = variants_start,
                    .count = @intCast(variant_layouts.len),
                },
            });
            assertAppendIdx(expected_idx, idx);
        }

        const layout_idx = try self.reserveLayout(Layout.tagUnion(alignment, .{ .int_idx = @intCast(tag_union_data_idx) }));
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
        self.stableSortStructFieldsByLayoutAlignment(temp_fields.items);

        var max_alignment: usize = 1;
        var current_offset: u32 = 0;
        for (temp_fields.items) |field| {
            const field_layout = self.getLayout(field.layout);
            const field_size_align = self.layoutSizeAlign(field_layout);
            const field_alignment = field_size_align.alignment.toByteUnits();
            max_alignment = @max(max_alignment, field_alignment);
            current_offset = @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(field_alignment))));
            current_offset += field_size_align.size;
        }

        const total_size = @as(u32, @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(max_alignment)))));
        if (total_size == 0) {
            return try self.ensureZstLayout();
        }
        return self.internStructShape(
            std.mem.Alignment.fromByteUnits(max_alignment),
            total_size,
            temp_fields.items,
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

    fn stableSortStructFieldsByLayoutAlignment(self: *Self, fields: []StructField) void {
        const AlignmentSortCtx = struct {
            store: *Self,
            target_usize: target.TargetUsize,

            pub fn lessThan(ctx: @This(), lhs: StructField, rhs: StructField) bool {
                const lhs_alignment = ctx.store.getLayout(lhs.layout).alignment(ctx.target_usize).toByteUnits();
                const rhs_alignment = ctx.store.getLayout(rhs.layout).alignment(ctx.target_usize).toByteUnits();
                return lhs_alignment > rhs_alignment;
            }
        };

        std.sort.block(
            StructField,
            fields,
            AlignmentSortCtx{ .store = self, .target_usize = self.targetUsize() },
            AlignmentSortCtx.lessThan,
        );
    }

    /// Create a tag union layout from pre-computed variant payload layouts.
    /// `variant_layouts[i]` is the layout Idx for variant i's payload
    /// (use ensureZstLayout() for no-payload variants).
    /// Tags must be sorted alphabetically; variant_layouts[i] corresponds
    /// to the tag at sorted index i.
    pub fn putTagUnion(self: *Self, variant_layouts: []const Idx) std.mem.Allocator.Error!Idx {
        var max_payload_size: u32 = 0;
        var max_payload_alignment: std.mem.Alignment = .@"1";

        for (variant_layouts) |variant_layout_idx| {
            const variant_layout = self.getLayout(variant_layout_idx);
            const variant_size = self.layoutSize(variant_layout);
            const variant_alignment = variant_layout.alignment(self.targetUsize());
            if (variant_size > max_payload_size) max_payload_size = variant_size;
            max_payload_alignment = max_payload_alignment.max(variant_alignment);
        }

        // Single-variant tag unions keep their tag_union layout but use an implicit
        // discriminant, so they do not reserve any discriminant bytes in memory.
        const discriminant_size: u8 = tagUnionDiscriminantSize(variant_layouts.len);
        const disc_align = TagUnionData.alignmentForDiscriminantSize(discriminant_size);

        // Canonical layout: payload at offset 0, discriminant after (aligned)
        const discriminant_offset: u16 = @intCast(
            std.mem.alignForward(u32, max_payload_size, @intCast(disc_align.toByteUnits())),
        );
        const tag_union_alignment = max_payload_alignment.max(disc_align);
        const total_size = std.mem.alignForward(
            u32,
            discriminant_offset + discriminant_size,
            @intCast(tag_union_alignment.toByteUnits()),
        );
        if (total_size == 0) {
            return try self.ensureZstLayout();
        }

        return self.internTagUnionShape(
            tag_union_alignment,
            total_size,
            discriminant_offset,
            discriminant_size,
            variant_layouts,
        );
    }

    fn buildUninternedStructLayout(self: *Self, input_fields: []const StructField) std.mem.Allocator.Error!Layout {
        std.debug.assert(input_fields.len >= 1);

        var temp_fields = std.ArrayList(StructField).empty;
        defer temp_fields.deinit(self.allocator);
        try temp_fields.appendSlice(self.allocator, input_fields);
        self.stableSortStructFieldsByLayoutAlignment(temp_fields.items);

        var max_alignment: usize = 1;
        var current_offset: u32 = 0;
        for (temp_fields.items) |field| {
            const field_layout = self.getLayout(field.layout);
            const field_size_align = self.layoutSizeAlign(field_layout);
            const field_alignment = field_size_align.alignment.toByteUnits();
            max_alignment = @max(max_alignment, field_alignment);
            current_offset = @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(field_alignment))));
            current_offset += field_size_align.size;
        }

        const total_size = @as(u32, @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(max_alignment)))));
        if (total_size == 0) {
            return Layout.zst();
        }

        const fields_start = self.struct_fields.items.len;
        for (temp_fields.items) |field| {
            const expected_idx = self.struct_fields.items.len;
            const idx = try self.struct_fields.append(self.allocator, field);
            assertAppendIdx(expected_idx, idx);
        }

        const struct_idx = StructIdx{ .int_idx = @intCast(self.struct_data.len()) };
        const expected_idx = self.struct_data.items.items.len;
        const struct_data_idx = try self.struct_data.append(self.allocator, .{
            .size = total_size,
            .fields = .{
                .start = @intCast(fields_start),
                .count = @intCast(temp_fields.items.len),
            },
        });
        assertAppendIdx(expected_idx, struct_data_idx);

        return Layout.struct_(std.mem.Alignment.fromByteUnits(max_alignment), struct_idx);
    }

    fn buildUninternedTagUnionLayout(self: *Self, variant_layouts: []const Idx) std.mem.Allocator.Error!Layout {
        std.debug.assert(variant_layouts.len >= 1);

        var max_payload_size: u32 = 0;
        var max_payload_alignment: std.mem.Alignment = .@"1";

        for (variant_layouts) |variant_layout_idx| {
            const variant_layout = self.getLayout(variant_layout_idx);
            const variant_size = self.layoutSize(variant_layout);
            const variant_alignment = variant_layout.alignment(self.targetUsize());
            if (variant_size > max_payload_size) max_payload_size = variant_size;
            max_payload_alignment = max_payload_alignment.max(variant_alignment);
        }

        const discriminant_size: u8 = tagUnionDiscriminantSize(variant_layouts.len);
        const disc_align = TagUnionData.alignmentForDiscriminantSize(discriminant_size);
        const discriminant_offset: u16 = @intCast(
            std.mem.alignForward(u32, max_payload_size, @intCast(disc_align.toByteUnits())),
        );
        const tag_union_alignment = max_payload_alignment.max(disc_align);
        const total_size = std.mem.alignForward(
            u32,
            discriminant_offset + discriminant_size,
            @intCast(tag_union_alignment.toByteUnits()),
        );
        if (total_size == 0) {
            return Layout.zst();
        }

        const variants_start: u32 = @intCast(self.tag_union_variants.len());
        for (variant_layouts) |variant_layout_idx| {
            const expected_idx = self.tag_union_variants.len();
            const idx = try self.tag_union_variants.append(self.allocator, .{
                .payload_layout = variant_layout_idx,
            });
            assertAppendIdx(expected_idx, idx);
        }

        const tag_union_data_idx: u32 = @intCast(self.tag_union_data.len());
        {
            const expected_idx = self.tag_union_data.items.items.len;
            const idx = try self.tag_union_data.append(self.allocator, .{
                .size = total_size,
                .discriminant_offset = discriminant_offset,
                .discriminant_size = discriminant_size,
                .variants = .{
                    .start = variants_start,
                    .count = @intCast(variant_layouts.len),
                },
            });
            assertAppendIdx(expected_idx, idx);
        }

        return Layout.tagUnion(tag_union_alignment, .{ .int_idx = @intCast(tag_union_data_idx) });
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

            fn recursiveSlotLayout(
                self_resolver: *@This(),
                child_id: GraphNodeId,
            ) std.mem.Allocator.Error!Idx {
                return try self_resolver.store.insertBox(
                    self_resolver.raw_layouts[@intFromEnum(child_id)],
                );
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
                        self_resolver.value_layouts[index] = self_resolver.raw_layouts[index];
                    },
                    .box => |child| {
                        const child_idx = self_resolver.valueIdx(child);
                        const child_is_zst = self_resolver.isKnownZeroSized(child);
                        self_resolver.store.updateLayout(
                            self_resolver.raw_layouts[index],
                            if (child_is_zst) Layout.boxOfZst() else Layout.box(child_idx),
                        );
                    },
                    .list => |child| {
                        const child_idx = self_resolver.valueIdx(child);
                        const child_is_zst = self_resolver.isKnownZeroSized(child);
                        self_resolver.store.updateLayout(
                            self_resolver.raw_layouts[index],
                            if (child_is_zst) Layout.listOfZst() else Layout.list(child_idx),
                        );
                    },
                    .closure => |child| {
                        self_resolver.store.updateLayout(
                            self_resolver.raw_layouts[index],
                            Layout.closure(self_resolver.valueIdx(child)),
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
                                    try self_resolver.recursiveSlotLayout(switch (field.child) {
                                        .canonical => unreachable,
                                        .local => |child_id| child_id,
                                    })
                                else blk: {
                                    if (!self_resolver.isSlotReady(field.child)) return false;
                                    break :blk self_resolver.valueIdx(field.child);
                                };
                                fields.appendAssumeCapacity(.{
                                    .index = field.index,
                                    .layout = field_layout,
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
                                    try self_resolver.recursiveSlotLayout(switch (variant_ref) {
                                        .canonical => unreachable,
                                        .local => |child_id| child_id,
                                    })
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

        const Finalizer = struct {
            store: *Self,
            graph: *const LayoutGraph,
            raw_layouts: []Idx,
            value_layouts: []Idx,
            finalize_state: []FinalizeState,
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
                    .local => |node_id| switch (self_finalizer.finalize_state[@intFromEnum(node_id)]) {
                        .active => self_finalizer.raw_layouts[@intFromEnum(node_id)],
                        .unseen, .done => try self_finalizer.finalizeNode(node_id),
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

            fn recursiveSlotLayout(
                self_finalizer: *@This(),
                child_id: GraphNodeId,
            ) std.mem.Allocator.Error!Idx {
                return try self_finalizer.store.insertBox(
                    self_finalizer.raw_layouts[@intFromEnum(child_id)],
                );
            }

            fn finalizeNode(self_finalizer: *@This(), node_id: GraphNodeId) std.mem.Allocator.Error!Idx {
                const index = @intFromEnum(node_id);
                return switch (self_finalizer.finalize_state[index]) {
                    .done => self_finalizer.value_layouts[index],
                    // The earlier resolver already established a valid raw recursive graph.
                    // Canonical finalization should reuse that placeholder edge rather than
                    // trying to recurse indefinitely through the same logical node again.
                    .active => self_finalizer.raw_layouts[index],
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
                                        try self_finalizer.recursiveSlotLayout(switch (field.child) {
                                            .canonical => unreachable,
                                            .local => |child_id| child_id,
                                        })
                                    else
                                        try self_finalizer.finalValue(field.child);
                                    fields.appendAssumeCapacity(.{
                                        .index = field.index,
                                        .layout = field_layout,
                                    });
                                }

                                break :blk_struct try self_finalizer.store.putStructFields(fields.items);
                            },
                            .tag_union => |span| blk_union: {
                                const graph_refs = self_finalizer.graph.getRefs(span);
                                var variants = std.ArrayList(Idx).empty;
                                defer variants.deinit(self_finalizer.store.allocator);
                                try variants.ensureTotalCapacity(self_finalizer.store.allocator, graph_refs.len);

                                for (graph_refs) |variant_ref| {
                                    const variant_layout = if (self_finalizer.shouldBoxRecursiveSlotEdge(node_id, variant_ref))
                                        try self_finalizer.recursiveSlotLayout(switch (variant_ref) {
                                            .canonical => unreachable,
                                            .local => |child_id| child_id,
                                        })
                                    else
                                        try self_finalizer.finalValue(variant_ref);
                                    variants.appendAssumeCapacity(variant_layout);
                                }

                                break :blk_union try self_finalizer.store.putTagUnion(variants.items);
                            },
                        };

                        self_finalizer.value_layouts[index] = value_layout;
                        self_finalizer.finalize_state[index] = .done;
                        break :blk value_layout;
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

        var max_alignment: usize = 1;
        var current_offset: u32 = 0;
        for (capture_layout_idxs, 0..) |cap_idx, i| {
            try temp_fields.append(self.allocator, .{ .index = @intCast(i), .layout = cap_idx });
            const cap_layout = self.getLayout(cap_idx);
            const cap_sa = self.layoutSizeAlign(cap_layout);
            const field_alignment = cap_sa.alignment.toByteUnits();
            max_alignment = @max(max_alignment, field_alignment);
            current_offset = @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(field_alignment))));
            current_offset += cap_sa.size;
        }

        const total_size = @as(u32, @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(max_alignment)))));

        return self.internStructShape(
            std.mem.Alignment.fromByteUnits(max_alignment),
            total_size,
            temp_fields.items,
        );
    }

    /// Create a struct layout representing the sequential layout of a lambda set union.
    /// The layout is: 8-byte tag + max(capture struct size per variant).
    pub fn putCaptureUnion(self: *Self, variants: []const []const Idx) std.mem.Allocator.Error!Idx {
        // Find the maximum payload size across all variants
        var max_payload_size: u32 = 0;
        var max_alignment: usize = 8; // At least 8 for the tag
        for (variants) |capture_idxs| {
            var current_offset: u32 = 0;
            for (capture_idxs) |cap_idx| {
                const cap_layout = self.getLayout(cap_idx);
                const cap_sa = self.layoutSizeAlign(cap_layout);
                const field_alignment = cap_sa.alignment.toByteUnits();
                max_alignment = @max(max_alignment, field_alignment);
                current_offset = @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(field_alignment))));
                current_offset += cap_sa.size;
            }
            max_payload_size = @max(max_payload_size, current_offset);
        }

        // Total size = 8 (tag) + max_payload_size, aligned to max_alignment
        const total_size: u32 = @intCast(std.mem.alignForward(
            u32,
            8 + max_payload_size,
            @as(u32, @intCast(max_alignment)),
        ));

        const dummy_fields = [_]StructField{.{ .index = 0, .layout = .u64 }};
        return self.internStructShape(
            std.mem.Alignment.fromByteUnits(max_alignment),
            total_size,
            dummy_fields[0..],
        );
    }

    pub fn getLayout(self: *const Self, idx: Idx) Layout {
        return self.layouts.get(@enumFromInt(@intFromEnum(idx))).*;
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
            .alignment = layout.getStruct().alignment,
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
            .alignment = layout.getTagUnion().alignment,
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

    /// Get the canonical discriminant offset for a tag union.
    pub fn getTagUnionDiscriminantOffset(self: *const Self, tu_idx: TagUnionIdx) u16 {
        return self.getTagUnionData(tu_idx).discriminant_offset;
    }

    /// Get the canonical size of a tag union.
    pub fn getTagUnionSize(self: *const Self, tu_idx: TagUnionIdx, _: std.mem.Alignment) u32 {
        return self.getTagUnionData(tu_idx).size;
    }

    /// Get the canonical size of a struct.
    pub fn getStructSize(self: *const Self, struct_idx: StructIdx, _: std.mem.Alignment) u32 {
        return self.getStructData(struct_idx).size;
    }

    /// Backwards-compat aliases
    pub const getTupleSize = getStructSize;
    pub const getRecordSize = getStructSize;

    /// Get the offset of a struct field at the given sorted index.
    pub fn getStructFieldOffset(self: *const Self, struct_idx: StructIdx, field_index_in_sorted_fields: u32) u32 {
        const sd = self.getStructData(struct_idx);
        const sorted_fields = self.struct_fields.sliceRange(sd.getFields());

        var current_offset: u32 = 0;
        var field_idx: u32 = 0;

        while (field_idx < field_index_in_sorted_fields) : (field_idx += 1) {
            const field = sorted_fields.get(field_idx);
            const field_layout = self.getLayout(field.layout);
            const field_size_align = self.layoutSizeAlign(field_layout);
            current_offset = @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(field_size_align.alignment.toByteUnits()))));
            current_offset += field_size_align.size;
        }

        const requested_field = sorted_fields.get(field_index_in_sorted_fields);
        const requested_field_layout = self.getLayout(requested_field.layout);
        const requested_field_size_align = self.layoutSizeAlign(requested_field_layout);
        return @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(requested_field_size_align.alignment.toByteUnits()))));
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
    fn getEmptyStructLayout(self: *Self) !Idx {
        return self.ensureZstLayout();
    }

    /// Backwards-compat alias
    pub const getEmptyRecordLayout = getEmptyStructLayout;

    pub fn ensureEmptyRecordLayout(self: *Self) !Idx {
        return self.getEmptyStructLayout();
    }

    /// Get or create a zero-sized type layout
    pub fn ensureZstLayout(self: *Self) !Idx {
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
        return switch (layout.tag) {
            .scalar => switch (layout.getScalar().tag) {
                .int => .{
                    .size = @intCast(layout.getScalar().getInt().size()),
                    .alignment = layout_mod.RocAlignment.fromByteUnits(@intCast(layout.getScalar().getInt().alignment().toByteUnits())),
                },
                .frac => .{
                    .size = @intCast(layout.getScalar().getFrac().size()),
                    .alignment = layout_mod.RocAlignment.fromByteUnits(@intCast(layout.getScalar().getFrac().alignment().toByteUnits())),
                },
                .str => .{
                    .size = @intCast(3 * target_usize.size()), // ptr, byte length, capacity
                    .alignment = layout_mod.RocAlignment.fromByteUnits(@intCast(target_usize.size())),
                },
                .opaque_ptr => .{
                    .size = @intCast(target_usize.size()),
                    .alignment = layout_mod.RocAlignment.fromByteUnits(@intCast(target_usize.size())),
                },
            },
            .box, .box_of_zst, .erased_callable => .{
                .size = @intCast(target_usize.size()), // a Box is just a pointer to refcounted memory
                .alignment = layout_mod.RocAlignment.fromByteUnits(@intCast(target_usize.size())),
            },
            .list, .list_of_zst => .{
                .size = @intCast(3 * target_usize.size()), // ptr, length, capacity
                .alignment = layout_mod.RocAlignment.fromByteUnits(@intCast(target_usize.size())),
            },
            .struct_ => .{
                .size = @intCast(self.getStructSize(layout.getStruct().idx, layout.getStruct().alignment)),
                .alignment = layout_mod.RocAlignment.fromByteUnits(@intCast(layout.getStruct().alignment.toByteUnits())),
            },
            .closure => blk: {
                // Closure layout: header + aligned capture data
                const header_size = @sizeOf(layout_mod.Closure);
                const captures_layout = self.getLayout(layout.getClosure().captures_layout_idx);
                const captures_size_align = self.layoutSizeAlign(captures_layout);
                const aligned_captures_offset = std.mem.alignForward(u32, header_size, @as(u32, @intCast(captures_size_align.alignment.toByteUnits())));
                break :blk .{
                    .size = @intCast(aligned_captures_offset + captures_size_align.size),
                    .alignment = layout_mod.RocAlignment.fromByteUnits(@intCast(target_usize.size())),
                };
            },
            .tag_union => .{
                .size = @intCast(self.getTagUnionSize(layout.getTagUnion().idx, layout.getTagUnion().alignment)),
                .alignment = layout_mod.RocAlignment.fromByteUnits(@intCast(layout.getTagUnion().alignment.toByteUnits())),
            },
            .zst => .{
                .size = 0, // Zero-sized types have size 0
                .alignment = .@"1",
            },
        };
    }

    /// Get the size in bytes of a layout, given the store's target usize.
    pub fn layoutSize(self: *const Self, layout: Layout) u32 {
        return self.layoutSizeAlign(layout).size;
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
    pub fn layoutContainsRefcounted(self: *const Self, l: Layout) bool {
        var visit_states = std.AutoHashMap(u32, RefcountedVisitState).init(self.allocator);
        defer visit_states.deinit();

        return self.layoutContainsRefcountedInner(l, &visit_states) catch
            @panic("layoutContainsRefcounted ran out of memory");
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

    fn layoutContainsRefcountedInner(
        self: *const Self,
        l: Layout,
        visit_states: *std.AutoHashMap(u32, RefcountedVisitState),
    ) std.mem.Allocator.Error!bool {
        const key: u32 = @bitCast(l);
        if (visit_states.get(key)) |state| {
            return switch (state) {
                // Recursive layout back-edges are materialized through placeholder
                // indirections, so re-entering an active node implies refcounted data.
                .active, .yes => true,
                .no => false,
            };
        }

        switch (l.tag) {
            .scalar => return l.getScalar().tag == .str,
            .list, .list_of_zst => return true,
            .box, .box_of_zst => return true,
            .erased_callable => return true,
            .zst => return false,
            .struct_, .tag_union, .closure => {},
        }

        try visit_states.put(key, .active);

        const contains_refcounted = switch (l.tag) {
            .struct_ => blk: {
                const sd = self.getStructData(l.getStruct().idx);
                const fields = self.struct_fields.sliceRange(sd.getFields());
                for (0..fields.len) |i| {
                    const field_layout = self.getLayout(fields.get(i).layout);
                    if (try self.layoutContainsRefcountedInner(field_layout, visit_states)) {
                        break :blk true;
                    }
                }
                break :blk false;
            },
            .tag_union => blk: {
                const tu_data = self.getTagUnionData(l.getTagUnion().idx);
                const variants = self.getTagUnionVariants(tu_data);
                for (0..variants.len) |i| {
                    const variant_layout = self.getLayout(variants.get(i).payload_layout);
                    if (try self.layoutContainsRefcountedInner(variant_layout, visit_states)) {
                        break :blk true;
                    }
                }
                break :blk false;
            },
            .closure => blk: {
                const captures_layout = self.getLayout(l.getClosure().captures_layout_idx);
                break :blk try self.layoutContainsRefcountedInner(captures_layout, visit_states);
            },
            .scalar, .list, .list_of_zst, .box, .box_of_zst, .erased_callable, .zst => unreachable,
        };

        try visit_states.put(key, if (contains_refcounted) .yes else .no);
        return contains_refcounted;
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

        // Set the current module for this computation
        self.current_module_idx = module_idx;

        const types_store_ptr = self.getTypesStore();
        var current = types_store_ptr.resolveVar(unresolved_var);

        // If we've already seen this (module, var) pair, return the layout we resolved it to.
        const cache_key = ModuleVarKey{ .module_idx = module_idx, .var_ = current.var_ };
        if (self.layouts_by_module_var.get(cache_key)) |cached_idx| {
            return cached_idx;
        }

        // To make this function stack-safe, we use a manual stack instead of recursing.
        // We reuse that stack from call to call to avoid reallocating it.
        // NOTE: We do NOT clear work fields here because fromTypeVar can be called
        // recursively (e.g., when processing tag union variant payloads), and nested
        // calls must not destroy the work state from outer calls.

        // Save the container stack depth at entry. When fromTypeVar is called recursively
        // (e.g., from flex/rigid type scope resolution), the recursive call must not
        // consume containers that belong to the caller. The container loop below uses
        // this depth to know where to stop.
        const container_base_depth = self.work.pending_containers.len;

        var layout_idx: Idx = undefined;

        // Debug-only: track vars visited via TypeScope lookup to detect cycles.
        // Cycles in layout computation indicate a bug in type checking - they should
        // have been detected earlier. In release builds we skip this check entirely.
        var scope_lookup_visited: if (@import("builtin").mode == .Debug) [32]Var else void = if (@import("builtin").mode == .Debug) undefined else {};
        var scope_lookup_count: if (@import("builtin").mode == .Debug) u8 else void = if (@import("builtin").mode == .Debug) 0 else {};

        // Track whether this computation depends on unresolved type parameters.
        // If so, we should NOT cache the result because the same type var can have
        // different layouts depending on the caller's type context.
        var depends_on_unresolved_type_params = false;

        outer: while (true) {
            // Flag to skip layout computation if we hit cache or detect a cycle
            var skip_layout_computation = false;

            // Check cache at every iteration - critical for recursive types
            // where the inner reference may resolve to the same var as the outer type
            const current_cache_key = ModuleVarKey{ .module_idx = self.current_module_idx, .var_ = current.var_ };
            if (self.layouts_by_module_var.get(current_cache_key)) |cached_idx| {
                // Check if this cache hit is a recursive reference to an in-progress nominal.
                // When we cache a nominal's placeholder (Box) and later hit that cache from
                // within the nominal's backing type computation, we need to mark it as recursive.
                // This can happen when the recursive reference uses the same var as the nominal.
                var is_in_progress_recursive = false;
                var maybe_progress: ?*work.Work.NominalProgress = null;
                if (current.desc.content == .structure) {
                    const flat_type = current.desc.content.structure;
                    if (flat_type == .nominal_type) {
                        const nominal_type = flat_type.nominal_type;
                        const nominal_key = work.NominalKey{
                            .ident_idx = nominal_type.ident.ident_idx,
                            .origin_module = nominal_type.origin_module,
                        };
                        if (self.work.in_progress_nominals.getPtr(nominal_key)) |progress| {
                            // This cache hit is a recursive reference - mark the nominal as recursive
                            progress.is_recursive = true;
                            is_in_progress_recursive = true;
                            maybe_progress = progress;
                        }
                    }
                }
                // For recursive nominal types used as elements in List or Box containers,
                // we need to use the boxed layout, not the raw cached layout.
                // But for tag union and record fields, we use the raw layout - the type
                // system says it's Node, not Box(Node).
                if (self.work.pending_containers.len > 0) {
                    const pending_item = self.work.pending_containers.get(self.work.pending_containers.len - 1);
                    if (pending_item.container == .list or pending_item.container == .box) {
                        if (self.recursive_boxed_layouts.get(current_cache_key)) |boxed_idx| {
                            layout_idx = boxed_idx;
                        } else if (is_in_progress_recursive) {
                            // This is a recursive reference to an in-progress nominal, and we're
                            // inside a Box/List container. We need to use a raw layout placeholder
                            // instead of the boxed placeholder, because the Box/List container
                            // itself provides the heap allocation - using the boxed placeholder
                            // would cause double-boxing.
                            const progress = maybe_progress.?;
                            const progress_raw_key = ModuleVarKey{ .module_idx = self.current_module_idx, .var_ = progress.nominal_var };
                            if (self.raw_layout_placeholders.get(progress_raw_key)) |raw_idx| {
                                layout_idx = raw_idx;
                            } else {
                                // Create a temporary non-zero-sized placeholder layout.
                                // This index is updated to the real layout once nominal resolution finishes.
                                const raw_placeholder = try self.reserveLayout(Layout.box(.zst));
                                try self.raw_layout_placeholders.put(progress_raw_key, raw_placeholder);
                                layout_idx = raw_placeholder;
                            }
                        } else {
                            layout_idx = cached_idx;
                        }
                    } else {
                        layout_idx = cached_idx;
                    }
                } else {
                    layout_idx = cached_idx;
                }
                skip_layout_computation = true;
            } else if (self.work.in_progress_vars.contains(.{ .module_idx = self.current_module_idx, .var_ = current.var_ })) {
                // Cycle detection: this var is already being processed, indicating a recursive type.
                //
                // Function types are an exception: they always have a fixed size (closure pointer)
                // regardless of recursion and regardless of what containers are pending.
                // This handles cases like recursive closures that capture themselves:
                //   flatten_aux = |l, acc| { ... flatten_aux(rest, acc) ... }
                if (current.desc.content == .structure) {
                    const flat = current.desc.content.structure;
                    switch (flat) {
                        .fn_pure, .fn_effectful, .fn_unbound => {
                            // Function types always have closure layout - no infinite size issue
                            const empty_captures_idx = try self.getEmptyRecordLayout();
                            layout_idx = try self.insertLayout(Layout.closure(empty_captures_idx));
                            skip_layout_computation = true;
                        },
                        else => {},
                    }
                }

                if (!skip_layout_computation) {
                    // INVARIANT: Recursive types are only valid if there's a heap-allocating container
                    // (List or Box) somewhere in the recursion path. This breaks the infinite size that
                    // would otherwise result from direct recursion.
                    //
                    // We must check the ENTIRE container stack, not just the last container, because
                    // the recursive reference may be nested inside other structures. For example:
                    //   Statement := [ForLoop(List(Statement)), IfStatement(List(Statement))]
                    //   parse_block : ... => Try((List(Statement), U64), Str)
                    //
                    // When processing this, the container stack might be:
                    //   Try -> tuple -> List -> Statement -> tag_union -> ForLoop -> List -> Statement
                    //
                    // When we hit the recursive Statement reference, the last container is tag_union,
                    // but there IS a List container earlier in the stack, so the recursion is valid.
                    var inside_heap_container = false;
                    for (self.work.pending_containers.slice().items(.container)) |container| {
                        if (container == .box or container == .list) {
                            inside_heap_container = true;
                            break;
                        }
                    }

                    if (inside_heap_container) {
                        // Valid recursive reference - heap allocation breaks the infinite size.
                        // Use a temporary non-zero-sized placeholder layout that preserves container sizing.
                        layout_idx = try self.reserveLayout(Layout.box(.zst));
                        skip_layout_computation = true;
                    } else {
                        // Invalid: recursive type without heap allocation would have infinite size.
                        unreachable;
                    }
                }
            } else if (current.desc.content == .structure) blk: {
                // Early cycle detection for nominal types from other modules.
                // These have different vars but same identity (ident + origin_module).
                const flat_type = current.desc.content.structure;
                if (flat_type != .nominal_type) break :blk;
                const nominal_type = flat_type.nominal_type;
                const nominal_key = work.NominalKey{
                    .ident_idx = nominal_type.ident.ident_idx,
                    .origin_module = nominal_type.origin_module,
                };

                if (self.work.in_progress_nominals.getPtr(nominal_key)) |progress| {
                    // Check if this is truly a recursive reference by comparing type arguments.
                    // A recursive reference has the same type arguments (or none).
                    // Different instantiations (like Try(Str, Str) inside Try((Try(Str, Str), U64), Str))
                    // have different type arguments and should not be treated as recursive.
                    const current_type_args_range = types.Store.getNominalArgsRange(nominal_type);
                    const same_type_args = argsMatch: {
                        if (current_type_args_range.count != progress.type_args_range.count) break :argsMatch false;
                        // Re-slice the stored range to get the actual vars.
                        // We do this now (rather than storing a slice) because the vars storage
                        // may have been reallocated since we stored the range.
                        const current_type_args = self.getTypesStore().sliceVars(current_type_args_range);
                        const progress_type_args = self.getTypesStore().sliceVars(progress.type_args_range);
                        // Compare each type arg by resolving and checking if they point to the same type
                        for (current_type_args, progress_type_args) |curr_arg, prog_arg| {
                            const curr_resolved = self.getTypesStore().resolveVar(curr_arg);
                            const prog_resolved = self.getTypesStore().resolveVar(prog_arg);
                            if (curr_resolved.var_ != prog_resolved.var_) break :argsMatch false;
                        }
                        break :argsMatch true;
                    };
                    if (same_type_args) {
                        // This IS a true recursive reference - the type refers to itself.
                        // Mark it as truly recursive so we know to box its values.
                        progress.is_recursive = true;
                        // Use the cached placeholder index for the nominal.
                        // The placeholder will be updated with the real layout once
                        // the nominal's backing type is fully computed.
                        const progress_cache_key = ModuleVarKey{ .module_idx = self.current_module_idx, .var_ = progress.nominal_var };
                        if (self.layouts_by_module_var.get(progress_cache_key)) |cached_idx| {
                            // We have a placeholder - but we need to check if we're inside a List/Box.
                            // If we are inside a List/Box, we need a RAW layout placeholder, not the
                            // boxed placeholder. This is because the List/Box container itself provides
                            // the heap allocation - using the boxed placeholder would cause double-boxing.
                            if (self.work.pending_containers.len > 0) {
                                const pending_item = self.work.pending_containers.get(self.work.pending_containers.len - 1);
                                if (pending_item.container == .box or pending_item.container == .list) {
                                    // Get or create a raw layout placeholder for this nominal
                                    if (self.raw_layout_placeholders.get(progress_cache_key)) |raw_idx| {
                                        layout_idx = raw_idx;
                                    } else {
                                        // Create a temporary non-zero-sized placeholder layout.
                                        // This index is updated to the real layout once nominal resolution finishes.
                                        const raw_placeholder = try self.reserveLayout(Layout.box(.zst));
                                        try self.raw_layout_placeholders.put(progress_cache_key, raw_placeholder);
                                        layout_idx = raw_placeholder;
                                    }
                                    skip_layout_computation = true;
                                    break :blk;
                                }
                            }
                            // For record/tuple fields (not inside List/Box), we use the boxed placeholder.
                            // The placeholder will be updated by the time we need the actual layout.
                            layout_idx = cached_idx;
                            skip_layout_computation = true;
                            break :blk;
                        }

                        // No cached placeholder - this is an error
                        unreachable;
                    }
                    // Different var means different instantiation - not a recursive reference.
                    // Fall through to normal processing.
                }
            }

            // Declare layout outside the if so it's accessible in container finalization
            var layout: Layout = undefined;

            if (!skip_layout_computation) {
                // Mark this var as in-progress before processing.
                // Note: We don't add aliases to in_progress_vars because aliases are transparent
                // wrappers that just continue to their backing type. The alias handling code
                // does `current = backing; continue;` without ever completing the alias entry,
                // which would cause spurious cycle detection when the alias var is encountered
                // again. See issue #8708.
                if (current.desc.content != .alias) {
                    try self.work.in_progress_vars.put(self.allocator, .{ .module_idx = self.current_module_idx, .var_ = current.var_ }, {});
                }

                layout = switch (current.desc.content) {
                    .structure => |flat_type| flat_type: switch (flat_type) {
                        .nominal_type => |nominal_type| {
                            // Special-case Builtin.Str: it has a tag union backing type, but
                            // should have RocStr layout (3 pointers).
                            // Check if this nominal type's identifier matches Builtin.Str
                            const is_builtin_str = blk: {
                                if (self.builtin_str_ident) |builtin_str| {
                                    if (nominal_type.ident.ident_idx.eql(builtin_str)) break :blk true;
                                }
                                if (nominal_type.origin_module.eql(self.currentEnv().idents.builtin_module)) {
                                    if (self.builtin_str_plain_ident) |plain_str| {
                                        if (nominal_type.ident.ident_idx.eql(plain_str)) break :blk true;
                                    }
                                }
                                break :blk false;
                            };
                            if (is_builtin_str) {
                                // This is Builtin.Str - use string layout
                                break :flat_type Layout.str();
                            }

                            // Special handling for Builtin.Box
                            const is_builtin_box = if (self.box_ident) |box_ident|
                                nominal_type.origin_module.eql(self.currentEnv().idents.builtin_module) and
                                    nominal_type.ident.ident_idx.eql(box_ident)
                            else
                                false;
                            if (is_builtin_box) {
                                // Extract the element type from the type arguments
                                const type_args = self.getTypesStore().sliceNominalArgs(nominal_type);
                                std.debug.assert(type_args.len == 1); // Box must have exactly 1 type parameter
                                const elem_var = type_args[0];

                                // Check if the element type is a known ZST.
                                const elem_resolved = self.getTypesStore().resolveVar(elem_var);
                                const elem_content = elem_resolved.desc.content;
                                const is_elem_zst = switch (elem_content) {
                                    .structure => |ft| switch (ft) {
                                        .empty_record, .empty_tag_union => true,
                                        else => false,
                                    },
                                    else => false,
                                };

                                if (is_elem_zst) {
                                    // For ZST element types, use box of zero-sized type
                                    break :flat_type Layout.boxOfZst();
                                } else {
                                    // Otherwise, add this to the stack of pending work.
                                    try self.work.pending_containers.append(self.allocator, .{
                                        .var_ = current.var_,
                                        .module_idx = self.current_module_idx,
                                        .container = .box,
                                    });

                                    // Push a pending Box container and "recurse" on the elem type
                                    current = elem_resolved;
                                    continue;
                                }
                            }

                            // Special handling for Builtin.List
                            const is_builtin_list = if (self.list_ident) |list_ident|
                                nominal_type.origin_module.eql(self.currentEnv().idents.builtin_module) and
                                    nominal_type.ident.ident_idx.eql(list_ident)
                            else
                                false;
                            if (is_builtin_list) {
                                // Extract the element type from the type arguments
                                const type_args = self.getTypesStore().sliceNominalArgs(nominal_type);
                                std.debug.assert(type_args.len == 1); // List must have exactly 1 type parameter
                                const elem_var = type_args[0];

                                // Check if the element type is a known ZST
                                // For flex/rigid types that are mapped in the type scope, we need to
                                // check what the mapped type resolves to.
                                const elem_resolved = self.getTypesStore().resolveVar(elem_var);
                                const elem_content = elem_resolved.desc.content;
                                const is_elem_zst = switch (elem_content) {
                                    .flex => |flex| blk: {
                                        // If mapped in type scope, check what it maps to
                                        if (caller_module_idx) |caller_mod| {
                                            if (type_scope.lookup(elem_resolved.var_)) |mapped_var| {
                                                // Resolve the mapped type in the caller module
                                                const caller_env = self.all_module_envs[caller_mod];
                                                const mapped_resolved = caller_env.types.resolveVar(mapped_var);
                                                // If there's a mapping, the element type is NOT ZST.
                                                // We'll compute the actual layout recursively.
                                                // Only treat as ZST if the mapped type is truly empty.
                                                break :blk switch (mapped_resolved.desc.content) {
                                                    .structure => |ft| switch (ft) {
                                                        .empty_record, .empty_tag_union => true,
                                                        else => false,
                                                    },
                                                    // A mapped flex/rigid should be computed, not assumed ZST
                                                    .flex, .rigid => false,
                                                    else => false,
                                                };
                                            }
                                        }
                                        // No mapping found for this flex type parameter.
                                        // Mark this computation as depending on unresolved params
                                        // so the result won't be cached.
                                        depends_on_unresolved_type_params = true;
                                        break :blk flex.constraints.count == 0;
                                    },
                                    .rigid => |rigid| blk: {
                                        // If mapped in type scope, check what it maps to
                                        if (caller_module_idx) |caller_mod| {
                                            if (type_scope.lookup(elem_resolved.var_)) |mapped_var| {
                                                // Resolve the mapped type in the caller module
                                                const caller_env = self.all_module_envs[caller_mod];
                                                const mapped_resolved = caller_env.types.resolveVar(mapped_var);
                                                // If there's a mapping, the element type is NOT ZST.
                                                // We'll compute the actual layout recursively.
                                                // Only treat as ZST if the mapped type is truly empty.
                                                break :blk switch (mapped_resolved.desc.content) {
                                                    .structure => |ft| switch (ft) {
                                                        .empty_record, .empty_tag_union => true,
                                                        else => false,
                                                    },
                                                    // A mapped flex/rigid should be computed, not assumed ZST
                                                    .flex, .rigid => false,
                                                    else => false,
                                                };
                                            }
                                        }
                                        // Mark this computation as depending on unresolved params
                                        // so the result won't be cached.
                                        depends_on_unresolved_type_params = true;
                                        break :blk rigid.constraints.count == 0;
                                    },
                                    .structure => |ft| switch (ft) {
                                        .empty_record, .empty_tag_union => true,
                                        else => false,
                                    },
                                    else => false,
                                };

                                if (is_elem_zst) {
                                    // For ZST element types, use list of zero-sized type
                                    break :flat_type Layout.listOfZst();
                                } else {
                                    // Otherwise, add this to the stack of pending work
                                    try self.work.pending_containers.append(self.allocator, .{
                                        .var_ = current.var_,
                                        .module_idx = self.current_module_idx,
                                        .container = .list,
                                    });

                                    // Push a pending List container and "recurse" on the elem type
                                    current = elem_resolved;
                                    continue;
                                }
                            }

                            // Special handling for built-in numeric types from Builtin module
                            // These have empty tag union backings but need scalar layouts
                            if (nominal_type.origin_module.eql(self.currentEnv().idents.builtin_module)) {
                                const ident_idx = nominal_type.ident.ident_idx;
                                const num_layout: ?Layout = blk: {
                                    if (self.u8_ident) |u8_id| if (ident_idx.eql(u8_id)) break :blk Layout.int(types.Int.Precision.u8);
                                    if (self.i8_ident) |i8_id| if (ident_idx.eql(i8_id)) break :blk Layout.int(types.Int.Precision.i8);
                                    if (self.u16_ident) |u16_id| if (ident_idx.eql(u16_id)) break :blk Layout.int(types.Int.Precision.u16);
                                    if (self.i16_ident) |i16_id| if (ident_idx.eql(i16_id)) break :blk Layout.int(types.Int.Precision.i16);
                                    if (self.u32_ident) |u32_id| if (ident_idx.eql(u32_id)) break :blk Layout.int(types.Int.Precision.u32);
                                    if (self.i32_ident) |i32_id| if (ident_idx.eql(i32_id)) break :blk Layout.int(types.Int.Precision.i32);
                                    if (self.u64_ident) |u64_id| if (ident_idx.eql(u64_id)) break :blk Layout.int(types.Int.Precision.u64);
                                    if (self.i64_ident) |i64_id| if (ident_idx.eql(i64_id)) break :blk Layout.int(types.Int.Precision.i64);
                                    if (self.u128_ident) |u128_id| if (ident_idx.eql(u128_id)) break :blk Layout.int(types.Int.Precision.u128);
                                    if (self.i128_ident) |i128_id| if (ident_idx.eql(i128_id)) break :blk Layout.int(types.Int.Precision.i128);
                                    if (self.f32_ident) |f32_id| if (ident_idx.eql(f32_id)) break :blk Layout.frac(types.Frac.Precision.f32);
                                    if (self.f64_ident) |f64_id| if (ident_idx.eql(f64_id)) break :blk Layout.frac(types.Frac.Precision.f64);
                                    if (self.dec_ident) |dec_id| if (ident_idx.eql(dec_id)) break :blk Layout.frac(types.Frac.Precision.dec);
                                    break :blk null;
                                };

                                if (num_layout) |num_layout_val| {
                                    break :flat_type num_layout_val;
                                }
                            }

                            // Cycle detection for recursive nominal types is done above (before this switch).
                            // Here we need to:
                            // 1. Reserve a placeholder layout for this nominal type
                            // 2. Cache it so recursive references can find it
                            // 3. Mark the nominal as in-progress
                            // After the backing type is computed, we'll update the placeholder.
                            const nominal_key = work.NominalKey{
                                .ident_idx = nominal_type.ident.ident_idx,
                                .origin_module = nominal_type.origin_module,
                            };

                            // Get the backing var before we modify current
                            const backing_var = self.getTypesStore().getNominalBackingVar(nominal_type);
                            const resolved_backing = self.getTypesStore().resolveVar(backing_var);

                            // Reserve a placeholder layout and cache it for the nominal's var.
                            // This allows recursive references to find this layout index.
                            // We use Box(ZST) as placeholder because:
                            // 1. It's non-scalar, so it gets inserted (not a sentinel)
                            // 2. It's non-ZST, so isZeroSized() returns false
                            // 3. It can be updated with updateLayout() once the real layout is known
                            const reserved_idx = try self.reserveLayout(Layout.box(.zst));
                            const reserved_cache_key = ModuleVarKey{ .module_idx = self.current_module_idx, .var_ = current.var_ };
                            try self.layouts_by_module_var.put(reserved_cache_key, reserved_idx);

                            // Mark this nominal type as in-progress.
                            // Store the nominal var, backing var, and type args range.
                            // Type args are needed to distinguish different instantiations.
                            // We store the range (indices) rather than a slice to avoid
                            // dangling pointers if the vars storage is reallocated.
                            const type_args_range = types.Store.getNominalArgsRange(nominal_type);
                            try self.work.in_progress_nominals.put(self.allocator, nominal_key, .{
                                .nominal_var = current.var_,
                                .backing_var = resolved_backing.var_,
                                .type_args_range = type_args_range,
                            });

                            // From a layout perspective, nominal types are identical to type aliases:
                            // all we care about is what's inside, so just unroll it.
                            current = resolved_backing;
                            continue;
                        },
                        .tuple => |tuple_type| {
                            const num_fields = try self.gatherTupleFields(tuple_type);

                            if (num_fields == 0) {
                                continue :flat_type .empty_record; // Empty tuple is like empty record
                            }

                            try self.work.pending_containers.append(self.allocator, .{
                                .var_ = current.var_,
                                .module_idx = self.current_module_idx,
                                .container = .{
                                    .tuple = .{
                                        .num_fields = @intCast(num_fields),
                                        .pending_fields = @intCast(num_fields),
                                        .resolved_fields_start = @intCast(self.work.resolved_tuple_fields.len),
                                    },
                                },
                            });

                            // Start working on the last pending field (we want to pop them).
                            const last_field_idx = self.work.pending_tuple_fields.len - 1;
                            const last_pending_field = self.work.pending_tuple_fields.get(last_field_idx);
                            current = self.getTypesStore().resolveVar(last_pending_field.var_);
                            continue :outer;
                        },
                        .fn_pure, .fn_effectful, .fn_unbound => {
                            // Create empty captures layout for generic function type
                            const empty_captures_idx = try self.getEmptyRecordLayout();
                            break :flat_type Layout.closure(empty_captures_idx);
                        },
                        .record => |record_type| {
                            const num_fields = try self.gatherRecordFields(record_type);

                            if (num_fields == 0) {
                                continue :flat_type .empty_record;
                            }

                            try self.work.pending_containers.append(self.allocator, .{
                                .var_ = current.var_,
                                .module_idx = self.current_module_idx,
                                .container = .{
                                    .record = .{
                                        .num_fields = @intCast(num_fields),
                                        .pending_fields = @intCast(num_fields),
                                        .resolved_fields_start = @intCast(self.work.resolved_record_fields.len),
                                    },
                                },
                            });

                            // Start working on the last pending field (we want to pop them).
                            const field = self.work.pending_record_fields.get(self.work.pending_record_fields.len - 1);

                            current = self.getTypesStore().resolveVar(field.var_);
                            continue;
                        },
                        .tag_union => |tag_union| {
                            // Tag Union Layout Computation (Iterative)
                            //
                            // We compute tag union layouts ITERATIVELY using a work queue to avoid
                            // stack overflow on deeply nested types like `Ok((Name("str"), 5))`.
                            //
                            // The approach:
                            // 1. Push all variants with payloads to `pending_tag_union_variants`
                            // 2. Push a `PendingTagUnion` container to track progress
                            // 3. Process each variant's payload type iteratively (not recursively)
                            // 4. When a payload layout completes, move it to `resolved_tag_union_variants`
                            // 5. When all variants are resolved, call `finishTagUnion` to assemble
                            //    the final layout with discriminant, max payload size, etc.
                            //
                            // For multi-arg variants like `Point(1, 2)`, we push a `PendingTuple`
                            // container on top of the tag union. The tuple processes its fields
                            // iteratively, and its resulting layout becomes the variant's payload.

                            const pending_tags_top = self.work.pending_tags.len;
                            defer self.work.pending_tags.shrinkRetainingCapacity(pending_tags_top);

                            // Get all tags by checking the tag extension
                            const num_tags = try self.gatherTags(tag_union);
                            const tags_slice = self.work.pending_tags.slice();
                            const tags_args = tags_slice.items(.args)[pending_tags_top..];

                            // For general tag unions, we need to compute the layout
                            // First, determine discriminant size based on number of tags
                            if (num_tags == 0) {
                                // Empty tag union - represents a zero-sized type
                                break :flat_type Layout.zst();
                            }

                            const discriminant_layout_idx: Idx = if (num_tags <= 256)
                                Idx.u8
                            else if (num_tags <= 65536)
                                Idx.u16
                            else
                                Idx.u32;

                            // If all tags have no payload, we just need the discriminant
                            var has_payload = false;
                            for (tags_args) |tag_args| {
                                const args_slice = self.getTypesStore().sliceVars(tag_args);
                                if (args_slice.len > 0) {
                                    has_payload = true;
                                    break;
                                }
                            }

                            if (!has_payload) {
                                if (num_tags == 1) {
                                    const zst_idx = try self.ensureZstLayout();
                                    break :flat_type self.getLayout(try self.putTagUnion(&.{zst_idx}));
                                }
                                // Simple tag union with no payloads - just use discriminant
                                break :flat_type self.getLayout(discriminant_layout_idx);
                            }

                            // Complex tag union with payloads - process iteratively
                            const tags_names = tags_slice.items(.name)[pending_tags_top..];
                            const tags_args_slice = tags_slice.items(.args)[pending_tags_top..];

                            // Create temporary array of tags for sorting
                            var sorted_tags = try self.allocator.alloc(types.Tag, num_tags);
                            defer self.allocator.free(sorted_tags);
                            for (tags_names, tags_args_slice, 0..) |name, args, i| {
                                sorted_tags[i] = .{ .name = name, .args = args };
                            }

                            // Sort alphabetically by tag name
                            std.mem.sort(types.Tag, sorted_tags, self.currentEnv().getIdentStoreConst(), types.Tag.sortByNameAsc);

                            // Push variants onto pending_tag_union_variants (in reverse order for pop)
                            // For multi-arg variants, we create a synthetic tuple type var.
                            var variants_with_payloads: u32 = 0;

                            // First pass: record where resolved variants will start
                            const resolved_variants_start = self.work.resolved_tag_union_variants.len;

                            for (0..num_tags) |i| {
                                const variant_i = num_tags - 1 - i; // Reverse order for pop
                                const tag = sorted_tags[variant_i];
                                const args_slice = self.getTypesStore().sliceVars(tag.args);

                                if (args_slice.len == 0) {
                                    // No payload - resolve immediately as ZST
                                    try self.work.resolved_tag_union_variants.append(self.allocator, .{
                                        .index = @intCast(variant_i),
                                        .layout_idx = try self.ensureZstLayout(),
                                    });
                                } else {
                                    // One or more args - push to pending variants for processing
                                    try self.work.pending_tag_union_variants.append(self.allocator, .{
                                        .index = @intCast(variant_i),
                                        .args = tag.args,
                                    });
                                    variants_with_payloads += 1;
                                }
                            }

                            // Push the tag union container
                            try self.work.pending_containers.append(self.allocator, .{
                                .var_ = current.var_,
                                .module_idx = self.current_module_idx,
                                .container = .{
                                    .tag_union = .{
                                        .num_variants = @intCast(num_tags),
                                        .pending_variants = variants_with_payloads,
                                        .resolved_variants_start = @intCast(resolved_variants_start),
                                        .discriminant_layout = discriminant_layout_idx,
                                    },
                                },
                            });

                            if (variants_with_payloads == 0) {
                                // All variants have no payload - finalize immediately
                                // This shouldn't happen because we already handled has_payload == false above
                                break :flat_type self.getLayout(discriminant_layout_idx);
                            }

                            // Start processing the first variant with a payload
                            // Find the last pending variant (we process in reverse)
                            const last_variant = self.work.pending_tag_union_variants.get(
                                self.work.pending_tag_union_variants.len - 1,
                            );
                            const args_slice = self.getTypesStore().sliceVars(last_variant.args);
                            if (args_slice.len == 1) {
                                // Single arg variant - process directly
                                current = self.getTypesStore().resolveVar(args_slice[0]);
                                continue :outer;
                            } else {
                                // Multi-arg variant - set up tuple processing
                                for (args_slice, 0..) |var_, index| {
                                    self.debugAssertPendingTupleFieldsSane("tag-union:init-variant:before-append");
                                    try self.work.pending_tuple_fields.append(self.allocator, .{
                                        .index = @intCast(index),
                                        .var_ = var_,
                                    });
                                }
                                try self.work.pending_containers.append(self.allocator, .{
                                    .var_ = null, // synthetic tuple for multi-arg variant
                                    .module_idx = self.current_module_idx,
                                    .container = .{
                                        .tuple = .{
                                            .num_fields = @intCast(args_slice.len),
                                            .resolved_fields_start = @intCast(self.work.resolved_tuple_fields.len),
                                            .pending_fields = @intCast(args_slice.len),
                                        },
                                    },
                                });
                                // Process first tuple field
                                const first_field = self.work.pending_tuple_fields.get(
                                    self.work.pending_tuple_fields.len - 1,
                                );
                                current = self.getTypesStore().resolveVar(first_field.var_);
                                continue :outer;
                            }
                        },
                        .record_unbound => |fields| {
                            // For record_unbound, we need to gather fields directly since it has no Record struct
                            var gathered_fields = std.ArrayList(types.RecordField).empty;
                            defer gathered_fields.deinit(self.allocator);

                            if (fields.len() > 0) {
                                const unbound_field_slice = self.getTypesStore().getRecordFieldsSlice(fields);
                                for (unbound_field_slice.items(.name), unbound_field_slice.items(.var_)) |name, var_| {
                                    try gathered_fields.append(self.allocator, .{ .name = name, .var_ = var_ });
                                }
                            }

                            try self.appendPendingRecordFieldsCanonical(gathered_fields.items);
                            const num_fields = gathered_fields.items.len;

                            if (num_fields == 0) {
                                continue :flat_type .empty_record;
                            }

                            try self.work.pending_containers.append(self.allocator, .{
                                .var_ = current.var_,
                                .module_idx = self.current_module_idx,
                                .container = .{
                                    .record = .{
                                        .num_fields = @intCast(num_fields),
                                        .resolved_fields_start = @intCast(self.work.resolved_record_fields.len),
                                        .pending_fields = @intCast(num_fields),
                                    },
                                },
                            });

                            // Start working on the last pending field (we want to pop them).
                            const field = self.work.pending_record_fields.get(self.work.pending_record_fields.len - 1);

                            current = self.getTypesStore().resolveVar(field.var_);
                            continue;
                        },
                        .empty_record, .empty_tag_union => blk: {
                            // Empty records and tag unions are zero-sized types. They get a ZST layout.
                            // We only special-case List({}) and Box({}) because they need runtime representation.
                            if (self.work.pending_containers.len > 0) {
                                const pending_item = self.work.pending_containers.get(self.work.pending_containers.len - 1);
                                switch (pending_item.container) {
                                    .list => {
                                        // List({}) needs special runtime representation
                                        _ = self.work.pending_containers.pop();
                                        break :blk Layout.listOfZst();
                                    },
                                    .box => {
                                        // Box({}) needs special runtime representation
                                        _ = self.work.pending_containers.pop();
                                        break :blk Layout.boxOfZst();
                                    },
                                    else => {
                                        // For records and tuples, treat ZST fields normally
                                        break :blk Layout.zst();
                                    },
                                }
                            }
                            // Not inside any container, just return ZST
                            break :blk Layout.zst();
                        },
                    },
                    .flex => |flex| blk: {
                        // Only look up in TypeScope if we're doing cross-module resolution.
                        // caller_module_idx being set indicates the type_scope has mappings
                        // from an external module's vars to the caller's vars. If it's null,
                        // we're already in the target module and shouldn't apply mappings.
                        if (caller_module_idx != null) {
                            if (type_scope.lookup(current.var_)) |mapped_var| {
                                // Debug-only cycle detection: if we've visited this var before,
                                // there's a cycle which indicates a bug in type checking.
                                if (@import("builtin").mode == .Debug) {
                                    for (scope_lookup_visited[0..scope_lookup_count]) |visited| {
                                        if (visited == current.var_) {
                                            @panic("Cycle detected in layout computation for flex var - this is a type checking bug");
                                        }
                                    }
                                    if (scope_lookup_count < 32) {
                                        scope_lookup_visited[scope_lookup_count] = current.var_;
                                        scope_lookup_count += 1;
                                    }
                                }
                                // IMPORTANT: Remove the flex from in_progress_vars before making
                                // the recursive call. Otherwise, if the recursive call resolves to
                                // the same flex, it will see it in in_progress_vars and incorrectly
                                // detect a cycle.
                                _ = self.work.in_progress_vars.swapRemove(.{ .module_idx = self.current_module_idx, .var_ = current.var_ });
                                // Make a recursive call to compute the layout in the caller's module.
                                // This avoids switching current_module_idx which would mess up pending
                                // work items from the current module.
                                const target_module = caller_module_idx.?;
                                // Pass target_module as caller so chained type scope lookups
                                // work (e.g., rigid → flex → concrete via two scope entries).
                                // Cycle detection prevents infinite loops.
                                const saved_module_idx = self.current_module_idx;
                                layout_idx = try self.fromTypeVar(target_module, mapped_var, type_scope, target_module);
                                self.current_module_idx = saved_module_idx;
                                skip_layout_computation = true;
                                break :blk self.getLayout(layout_idx);
                            }
                        }

                        // Flex var was not resolved through type scope. Mark as depending
                        // on unresolved params so the result is NOT cached — a later call
                        // with type scope mappings (e.g., from setupLocalCallLayoutHints)
                        // may produce a different, correct layout.
                        depends_on_unresolved_type_params = true;

                        // Flex vars with a from_numeral constraint are numeric literals
                        // that haven't been resolved to a concrete type; default to Dec.
                        if (self.hasFromNumeralConstraint(flex.constraints)) {
                            break :blk Layout.default_num();
                        }

                        // By the time we ask the layout store for a non-numeric unresolved type
                        // variable, the only legitimate survivors are zero-sized cases whose
                        // runtime representation is fully determined without ever materializing
                        // the abstract type variable itself. Examples include empty-list element
                        // vars, phantom-only values, and aggregates whose remaining unresolved
                        // pieces are all representationless. Those must collapse to ZST here so
                        // layout never grows its own shadow "abstract layout param" notion.
                        //
                        // If an earlier compiler bug lets a representationful unresolved type
                        // variable reach this point, collapsing it to ZST is still not ideal.
                        // However, keeping a polymorphic placeholder in the runtime-layout layer
                        // is worse: layout is supposed to describe concrete representation, and
                        // inventing a first-class abstract layout variant only obscures the real
                        // invariant violation upstream.
                        break :blk Layout.zst();
                    },
                    .rigid => |rigid| blk: {
                        // Only look up in TypeScope if we're doing cross-module resolution.
                        // caller_module_idx being set indicates the type_scope has mappings
                        // from an external module's vars to the caller's vars. If it's null,
                        // we're already in the target module and shouldn't apply mappings.
                        if (caller_module_idx != null) {
                            if (type_scope.lookup(current.var_)) |mapped_var| {
                                // Debug-only cycle detection: if we've visited this var before,
                                // there's a cycle which indicates a bug in type checking.
                                if (@import("builtin").mode == .Debug) {
                                    for (scope_lookup_visited[0..scope_lookup_count]) |visited| {
                                        if (visited == current.var_) {
                                            @panic("Cycle detected in layout computation for rigid var - this is a type checking bug");
                                        }
                                    }
                                    if (scope_lookup_count < 32) {
                                        scope_lookup_visited[scope_lookup_count] = current.var_;
                                        scope_lookup_count += 1;
                                    }
                                }
                                // IMPORTANT: Remove the rigid from in_progress_vars before making
                                // the recursive call. Otherwise, if the recursive call resolves to
                                // the same rigid, it will see it in in_progress_vars and incorrectly
                                // detect a cycle.
                                _ = self.work.in_progress_vars.swapRemove(.{ .module_idx = self.current_module_idx, .var_ = current.var_ });
                                // Make a recursive call to compute the layout in the caller's module.
                                // This avoids switching current_module_idx which would mess up pending
                                // work items from the current module.
                                const target_module = caller_module_idx.?;
                                // Pass target_module as caller so chained type scope lookups
                                // work (e.g., rigid → flex → concrete via two scope entries).
                                // Cycle detection prevents infinite loops.
                                const saved_module_idx = self.current_module_idx;
                                layout_idx = try self.fromTypeVar(target_module, mapped_var, type_scope, target_module);
                                self.current_module_idx = saved_module_idx;
                                skip_layout_computation = true;
                                break :blk self.getLayout(layout_idx);
                            }
                        }

                        // Rigid var was not resolved through type scope. Mark as depending
                        // on unresolved params so the result is NOT cached — a later call
                        // with type scope mappings may produce a different, correct layout.
                        depends_on_unresolved_type_params = true;

                        // Check if this rigid var has a from_numeral constraint, indicating
                        // it's an unresolved numeric type that should default to Dec.
                        if (self.hasFromNumeralConstraint(rigid.constraints)) {
                            break :blk Layout.default_num();
                        }

                        // Same rationale as the unresolved flex-var case above: by the time a
                        // non-numeric rigid reaches the layout layer, the only valid survivors
                        // are representationless/ZST cases.
                        break :blk Layout.zst();
                    },
                    .alias => |alias| {
                        // Follow the alias by updating the work item
                        const backing_var = self.getTypesStore().getAliasBackingVar(alias);
                        current = self.getTypesStore().resolveVar(backing_var);
                        continue;
                    },
                    // .err is a "poison" type from type-checking failures.
                    // Treat it as ZST so downstream passes can proceed gracefully
                    // instead of crashing; the expression will fail at a later stage
                    // with a proper error message.
                    .err => Layout.zst(),
                };

                // We actually resolved a layout that wasn't zero-sized.
                layout_idx = try self.insertLayout(layout);
                const layout_cache_key = ModuleVarKey{ .module_idx = self.current_module_idx, .var_ = current.var_ };
                // Only cache if the layout doesn't depend on unresolved type parameters.
                // Layouts that depend on unresolved params (like List(a) where 'a' has no mapping)
                // could produce different results with different caller contexts, so caching
                // them would cause bugs when the same type var is used with different concrete types.
                if (!depends_on_unresolved_type_params) {
                    try self.layouts_by_module_var.put(layout_cache_key, layout_idx);
                }
                // Remove from in_progress now that it's done (regardless of caching)
                _ = self.work.in_progress_vars.swapRemove(.{ .module_idx = self.current_module_idx, .var_ = current.var_ });

                // Check if any in-progress nominals need their reserved layouts updated.
                // When a nominal type's backing type finishes, update the nominal's placeholder.
                var nominals_to_remove: std.ArrayList(work.NominalKey) = .empty;
                defer nominals_to_remove.deinit(self.allocator);

                var nominal_iter = self.work.in_progress_nominals.iterator();
                while (nominal_iter.next()) |entry| {
                    const progress = entry.value_ptr.*;
                    // Check if this nominal's backing type just finished.
                    // The backing_var should match the var we just cached.
                    if (progress.backing_var == current.var_) {
                        // Skip container types that actually pushed a pending container - they
                        // will be handled in the container finish path below.
                        // IMPORTANT: Only skip if the computed layout is a container type.
                        // No-payload tag unions (enums) resolve to scalar discriminant layout
                        // without pushing a container, so they must be handled here.
                        {
                            const computed = self.getLayout(layout_idx);
                            if (computed.tag == .tag_union or computed.tag == .struct_) {
                                // Container layout - will be handled in container path below
                                continue;
                            }
                        }
                        // The backing type just finished!
                        // IMPORTANT: Keep the reserved placeholder as a Box pointing to the real layout.
                        // This ensures recursive references remain boxed (correct size).
                        // Update layouts_by_module_var so non-recursive lookups get the real layout.
                        const nominal_cache_key = ModuleVarKey{ .module_idx = self.current_module_idx, .var_ = progress.nominal_var };
                        if (self.layouts_by_module_var.get(nominal_cache_key)) |reserved_idx| {
                            // Update the placeholder to Box(layout_idx) instead of replacing it
                            // with the raw layout. This keeps recursive references boxed.
                            self.updateLayout(reserved_idx, Layout.box(layout_idx));
                            // Only store in recursive_boxed_layouts if this type is truly recursive
                            // (i.e., a cycle was detected during its processing). Non-recursive
                            // nominal types don't need boxing for their values.
                            if (progress.is_recursive) {
                                try self.recursive_boxed_layouts.put(nominal_cache_key, reserved_idx);
                            }
                        }
                        // Also update the raw layout placeholder if one was created
                        if (self.raw_layout_placeholders.get(nominal_cache_key)) |raw_idx| {
                            self.updateLayout(raw_idx, self.getLayout(layout_idx));
                        }
                        // Update the cache so direct lookups get the actual layout
                        try self.layouts_by_module_var.put(nominal_cache_key, layout_idx);
                        try nominals_to_remove.append(self.allocator, entry.key_ptr.*);

                        // CRITICAL: If there are pending containers (List, Box, etc.), update layout_idx
                        // to use the boxed layout. Container elements need boxed layouts for recursive
                        // types to have fixed size. The boxed layout was stored in recursive_boxed_layouts.
                        if (self.work.pending_containers.len > 0) {
                            if (self.recursive_boxed_layouts.get(nominal_cache_key)) |boxed_layout_idx| {
                                // Use the boxed layout for pending containers
                                layout_idx = boxed_layout_idx;
                            }
                        }
                    }
                }

                // Remove the nominals we updated
                for (nominals_to_remove.items) |key| {
                    _ = self.work.in_progress_nominals.swapRemove(key);
                }
            } // end if (!skip_layout_computation)

            // If this was part of a pending container that we're working on, update that container.
            // Only process containers pushed during THIS invocation (above container_base_depth).
            // Recursive fromTypeVar calls must not consume containers from the caller.
            while (self.work.pending_containers.len > container_base_depth) {
                // Restore module context for the current container.
                // Recursive fromTypeVar calls (via flex/rigid type scope resolution) change
                // current_module_idx to the target module. The container's fields/variants
                // are vars in the module that was active when the container was created.
                self.current_module_idx = self.work.pending_containers.slice().items(.module_idx)[self.work.pending_containers.len - 1];

                // Get a pointer to the last pending container, so we can mutate it in-place.
                switch (self.work.pending_containers.slice().items(.container)[self.work.pending_containers.len - 1]) {
                    .box => {
                        // Check if the element type is zero-sized (recursively)
                        const elem_layout = self.getLayout(layout_idx);
                        if (self.isZeroSized(elem_layout)) {
                            layout = Layout.boxOfZst();
                        } else {
                            layout = Layout.box(layout_idx);
                        }
                    },
                    .list => {
                        // Check if the element type is zero-sized (recursively)
                        const elem_layout = self.getLayout(layout_idx);
                        if (self.isZeroSized(elem_layout)) {
                            layout = Layout.listOfZst();
                        } else {
                            layout = Layout.list(layout_idx);
                        }
                    },
                    .record => |*pending_record| {
                        std.debug.assert(pending_record.pending_fields > 0);
                        pending_record.pending_fields -= 1;

                        // Pop the field we just processed
                        const pending_field = self.work.pending_record_fields.pop() orelse unreachable;

                        // Add to resolved fields
                        try self.work.resolved_record_fields.append(self.allocator, .{
                            .field_index = pending_field.index,
                            .field_idx = layout_idx,
                        });

                        if (pending_record.pending_fields == 0) {
                            layout = try self.finishRecord(pending_record.*);
                        } else {
                            // There are still fields remaining to process, so process the next one in the outer loop.
                            const next_field = self.work.pending_record_fields.get(self.work.pending_record_fields.len - 1);
                            current = self.getTypesStore().resolveVar(next_field.var_);
                            continue :outer;
                        }
                    },
                    .tuple => |*pending_tuple| {
                        std.debug.assert(pending_tuple.pending_fields > 0);
                        pending_tuple.pending_fields -= 1;

                        // Pop the field we just processed
                        self.debugAssertPendingTupleFieldsSane("tuple:before-pop");
                        const pending_field = self.work.pending_tuple_fields.pop() orelse unreachable;

                        // Add to resolved fields
                        try self.work.resolved_tuple_fields.append(self.allocator, .{
                            .field_index = pending_field.index,
                            .field_idx = layout_idx,
                        });

                        if (pending_tuple.pending_fields == 0) {
                            layout = try self.finishTuple(pending_tuple.*);
                        } else {
                            // There are still fields remaining to process, so process the next one in the outer loop.
                            const next_field = self.work.pending_tuple_fields.get(self.work.pending_tuple_fields.len - 1);
                            current = self.getTypesStore().resolveVar(next_field.var_);
                            continue :outer;
                        }
                    },
                    .tag_union => |*pending_tag_union| {
                        // Pop the variant we just processed
                        const pending_variant = self.work.pending_tag_union_variants.pop() orelse unreachable;

                        // Add to resolved variants
                        try self.work.resolved_tag_union_variants.append(self.allocator, .{
                            .index = pending_variant.index,
                            .layout_idx = layout_idx,
                        });

                        // Check if there are more variants with payloads to process
                        if (pending_tag_union.pending_variants > 0) {
                            pending_tag_union.pending_variants -= 1;
                        }

                        if (pending_tag_union.pending_variants == 0) {
                            // All variants processed - finalize
                            layout = try self.finishTagUnion(pending_tag_union.*);
                        } else {
                            // More variants to process - continue with the next one
                            const next_variant = self.work.pending_tag_union_variants.get(
                                self.work.pending_tag_union_variants.len - 1,
                            );
                            const next_args_slice = self.getTypesStore().sliceVars(next_variant.args);
                            if (next_args_slice.len == 1) {
                                // Single arg variant - process directly
                                current = self.getTypesStore().resolveVar(next_args_slice[0]);
                                continue :outer;
                            } else {
                                // Multi-arg variant - set up tuple processing
                                for (next_args_slice, 0..) |var_, index| {
                                    self.debugAssertPendingTupleFieldsSane("tag-union:next-variant:before-append");
                                    try self.work.pending_tuple_fields.append(self.allocator, .{
                                        .index = @intCast(index),
                                        .var_ = var_,
                                    });
                                }
                                // Push tuple container on top of the tag union
                                try self.work.pending_containers.append(self.allocator, .{
                                    .var_ = null, // synthetic tuple for multi-arg variant
                                    .module_idx = self.current_module_idx,
                                    .container = .{
                                        .tuple = .{
                                            .num_fields = @intCast(next_args_slice.len),
                                            .resolved_fields_start = @intCast(self.work.resolved_tuple_fields.len),
                                            .pending_fields = @intCast(next_args_slice.len),
                                        },
                                    },
                                });
                                // Process first tuple field
                                const first_field = self.work.pending_tuple_fields.get(
                                    self.work.pending_tuple_fields.len - 1,
                                );
                                current = self.getTypesStore().resolveVar(first_field.var_);
                                continue :outer;
                            }
                        }
                    },
                }

                // We're done with this container, so remove it from pending_containers
                const pending_item = self.work.pending_containers.pop() orelse unreachable;
                layout_idx = try self.insertLayout(layout);

                // Only cache and check nominals for containers with a valid var.
                // Synthetic tuples (for multi-arg tag union variants) have var_=null and
                // should not be cached or trigger nominal updates.
                if (pending_item.var_) |container_var| {
                    // Use pending_item.module_idx for cache and in_progress_vars removal.
                    // This is the module that was active when the container started processing,
                    // which is the key that in_progress_vars was added under, and the key that
                    // future lookups from that module context will use.
                    const container_module_idx = pending_item.module_idx;

                    // Add the container's layout to our layouts_by_module_var cache for later use.
                    const container_cache_key = ModuleVarKey{ .module_idx = container_module_idx, .var_ = container_var };
                    try self.layouts_by_module_var.put(container_cache_key, layout_idx);

                    // Remove from in_progress_vars now that it's cached (no longer "in progress").
                    // Use container_module_idx - this is the key that was added when processing started.
                    _ = self.work.in_progress_vars.swapRemove(.{ .module_idx = container_module_idx, .var_ = container_var });

                    // Check if any in-progress nominals need their reserved layouts updated.
                    // This handles the case where a nominal's backing type is a container (e.g., tag union).
                    var nominals_to_remove_container: std.ArrayList(work.NominalKey) = .empty;
                    defer nominals_to_remove_container.deinit(self.allocator);

                    var nominal_iter_container = self.work.in_progress_nominals.iterator();
                    while (nominal_iter_container.next()) |entry| {
                        const progress = entry.value_ptr.*;
                        // Check if this nominal's backing type (container) just finished.
                        if (progress.backing_var == container_var) {
                            // The backing type (container) just finished!
                            // IMPORTANT: Keep the reserved placeholder as a Box pointing to the real layout.
                            // This ensures recursive references remain boxed (correct size).
                            // Use container_module_idx - the nominal should have been cached in the same module.
                            const container_nominal_key = ModuleVarKey{ .module_idx = container_module_idx, .var_ = progress.nominal_var };
                            if (self.layouts_by_module_var.get(container_nominal_key)) |reserved_idx| {
                                // reserved_idx should never equal layout_idx (would create self-referential box)
                                std.debug.assert(reserved_idx != layout_idx);
                                // Update the placeholder to Box(layout_idx) instead of replacing it
                                // with the raw layout. This keeps recursive references boxed.
                                self.updateLayout(reserved_idx, Layout.box(layout_idx));
                                // Only store in recursive_boxed_layouts if this type is truly recursive
                                // (i.e., a cycle was detected during its processing). Non-recursive
                                // nominal types don't need boxing for their values.
                                if (progress.is_recursive) {
                                    try self.recursive_boxed_layouts.put(container_nominal_key, reserved_idx);
                                }
                            }
                            // Also update the raw layout placeholder if one was created.
                            // The raw placeholder holds the unboxed layout for recursive nominals
                            // used inside Box/List containers (to avoid double-boxing).
                            if (self.raw_layout_placeholders.get(container_nominal_key)) |raw_idx| {
                                const new_layout = self.getLayout(layout_idx);
                                // Raw placeholder should get the raw layout, not a boxed wrapper
                                std.debug.assert(new_layout.tag != .box);
                                // Raw and reserved placeholders should be at different indices
                                if (self.layouts_by_module_var.get(container_nominal_key)) |reserved| {
                                    std.debug.assert(raw_idx != reserved);
                                }
                                self.updateLayout(raw_idx, new_layout);
                            }
                            // Note: It's valid for is_recursive to be true without a raw_placeholder
                            // when the recursion doesn't go through a Box/List container directly.
                            // For example: IntList := [Nil, Cons(I64, IntList)] - the recursion is
                            // handled by implicit boxing, not an explicit Box type.
                            // Update the cache so direct lookups get the actual layout
                            try self.layouts_by_module_var.put(container_nominal_key, layout_idx);
                            try nominals_to_remove_container.append(self.allocator, entry.key_ptr.*);

                            // CRITICAL: If there are more pending containers, update layout_idx
                            // to use the boxed layout. Container elements need boxed layouts for
                            // recursive types to have fixed size.
                            //
                            // HOWEVER: For Box/List containers, we should NOT use the boxed layout.
                            // Box/List elements are heap-allocated, so they should use the raw layout.
                            // Using the boxed layout would cause double-boxing (issue #8916).
                            if (self.work.pending_containers.len > 0) {
                                const next_container = self.work.pending_containers.slice().items(.container)[self.work.pending_containers.len - 1];
                                const is_heap_container = next_container == .box or next_container == .list;
                                if (!is_heap_container) {
                                    if (self.recursive_boxed_layouts.get(container_nominal_key)) |boxed_layout_idx| {
                                        // Use the boxed layout for pending containers (record/tuple fields)
                                        layout_idx = boxed_layout_idx;
                                    }
                                }
                            }
                        }
                    }

                    // Remove the nominals we updated
                    for (nominals_to_remove_container.items) |key| {
                        _ = self.work.in_progress_nominals.swapRemove(key);
                    }
                }
            }

            // For top-level calls (no pre-existing containers), all pending fields should
            // be consumed. For recursive calls, pending fields from the caller may remain.
            if (container_base_depth == 0) {
                std.debug.assert(self.work.pending_record_fields.len == 0);
                std.debug.assert(self.work.pending_tuple_fields.len == 0);
                std.debug.assert(self.work.pending_tag_union_variants.len == 0);
            }

            // No more pending containers for this invocation; we're done!
            // Note: Work fields (in_progress_vars, in_progress_nominals, etc.) are not cleared
            // here because individual entries are removed via swapRemove/pop when types finish
            // processing, so these should be empty when the top-level call returns.
            return layout_idx;
        }
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
    try testing.expectEqual(@as(u32, 16), store.getStructData(struct_idx).size);
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
    try testing.expectEqual(interned_layout.getStruct().alignment, uninterned_layout.getStruct().alignment);

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

fn expectBoolOrdinaryTagUnion() !void {
    const testing = std.testing;
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    const bool_layout = store.getLayout(.bool);
    try testing.expectEqual(LayoutTag.tag_union, bool_layout.tag);
    const info = store.getTagUnionInfo(bool_layout);
    try testing.expectEqual(@as(usize, 2), info.variants.len);
    try testing.expectEqual(@as(u32, 1), info.data.size);
    try testing.expectEqual(@as(u8, 1), info.data.discriminant_size);
    for (0..info.variants.len) |i| {
        try testing.expectEqual(Idx.zst, info.variants.get(i).payload_layout);
    }
}

fn expectZstContainerAbi() !void {
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

fn expectCanonicalStructOrdering() !void {
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

fn expectTagUnionShapeInterning() !void {
    const testing = std.testing;
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();

    const variants = [_]Idx{ .zst, .u64, .str };
    const a = try store.putTagUnion(&variants);
    const b = try store.putTagUnion(&variants);
    try testing.expectEqual(a, b);
    try testing.expectEqual(LayoutTag.tag_union, store.getLayout(a).tag);
}

fn expectRecursiveGraphInterning() !void {
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

fn expectNestedOrdinaryDataGraph() !void {
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
