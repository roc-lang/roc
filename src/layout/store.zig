//! Stores Layout values by index.

const std = @import("std");
const builtin = @import("builtin");
const tracy = @import("tracy");
const base = @import("base");
const collections = @import("collections");
const can = @import("can");

const layout_mod = @import("layout.zig");
const graph_mod = @import("./graph.zig");

const ModuleEnv = can.ModuleEnv;
const target = base.target;
const Ident = base.Ident;
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

    /// All module environments for cross-module type resolution
    all_module_envs: []const *const ModuleEnv,

    /// Allocator for all internal allocations
    allocator: std.mem.Allocator,

    /// Optional mutable env reference (used by interpreter for runtime identifier insertion).
    /// When set, getMutableEnv() returns this instead of null.
    mutable_env: ?*ModuleEnv = null,

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

    // Identifier for "Builtin.Str" to recognize the string type without string comparisons
    // (null when compiling Builtin module itself or when Builtin.Str isn't available)
    builtin_str_ident: ?Ident.Idx,

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
            .int => @enumFromInt(2 + @intFromEnum(scalar.data.int)),
            .frac => @enumFromInt(@as(u32, 12) + (@intFromEnum(scalar.data.frac) - @intFromEnum(@TypeOf(scalar.data.frac).f32))),
            .opaque_ptr => .opaque_ptr,
        };
    }

    pub fn init(
        all_module_envs: []const *const ModuleEnv,
        builtin_str_ident: ?Ident.Idx,
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
            .all_module_envs = all_module_envs,
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
            .builtin_str_ident = builtin_str_ident,
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

    /// Get all module environments in the layout store's module-index order.
    pub fn moduleEnvs(self: *const Self) []const *const ModuleEnv {
        return self.all_module_envs;
    }

    pub const GraphCommit = struct {
        root_idx: Idx,
        value_layouts: []Idx,

        pub fn deinit(self_commit: *GraphCommit, allocator: std.mem.Allocator) void {
            allocator.free(self_commit.value_layouts);
        }
    };

    /// Get the mutable module environment (used by interpreter for identifier insertion).
    /// Returns null if no mutable env was set via setMutableEnv.
    pub fn getMutableEnv(self: *Self) ?*ModuleEnv {
        return self.mutable_env;
    }

    /// Set a mutable env reference for runtime identifier insertion (used by interpreter).
    pub fn setMutableEnv(self: *Self, env: *ModuleEnv) void {
        self.mutable_env = env;
    }

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

    /// Update the module env slice used for shared layout queries without
    /// touching source-specific type-resolution caches.
    pub fn setModuleEnvs(self: *Self, new_module_envs: []const *const ModuleEnv) void {
        self.all_module_envs = new_module_envs;
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
                try self.appendInternKeyIdx(layout.data.box);
            },
            .box_of_zst => try self.startInternKey(.box_of_zst),
            .list => {
                try self.startInternKey(.list);
                try self.appendInternKeyIdx(layout.data.list);
            },
            .list_of_zst => try self.startInternKey(.list_of_zst),
            .closure => {
                try self.startInternKey(.closure);
                try self.appendInternKeyIdx(layout.data.closure.captures_layout_idx);
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
                .value_layouts = try self.allocator.alloc(Idx, 0),
            },
            .local => {},
        }

        const raw_layouts = try self.allocator.alloc(Idx, graph.nodes.items.len);
        defer self.allocator.free(raw_layouts);
        const value_layouts = try self.allocator.alloc(Idx, graph.nodes.items.len);
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
                                            .pending, .nominal, .box, .list, .closure, .tag_union => {
                                                has_boxable_slot_edge = true;
                                                break;
                                            },
                                        }
                                    },
                                }
                            }
                        },
                        .pending, .nominal, .box, .list, .closure => {},
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
                    .pending, .box, .list, .closure => false,
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
                    .pending, .box, .list, .closure => {},
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
                .struct_, .tag_union => Layout.zst(),
            });
        }

        for (graph.nodes.items, 0..) |node, i| {
            value_layouts[i] = switch (node) {
                .pending => unreachable,
                .nominal, .box, .list, .closure, .struct_, .tag_union => raw_layouts[i],
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
                            .box, .list, .closure => true,
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
                        .pending, .nominal, .box, .list, .closure, .tag_union => true,
                    },
                    .pending, .nominal, .box, .list, .closure => false,
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
                        .pending, .nominal, .box, .list, .closure, .tag_union => true,
                    },
                    .pending, .nominal, .box, .list, .closure => false,
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
            .list => layout.data.list,
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
            .closure => self.runtimeRepresentationLayoutIdx(layout_val.data.closure.captures_layout_idx),
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
            .box => layout.data.box,
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
        const struct_data = self.getStructData(layout.data.struct_.idx);
        return StructInfo{
            .data = struct_data,
            .alignment = layout.data.struct_.alignment,
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
        const tu_data = self.getTagUnionData(layout.data.tag_union.idx);
        return TagUnionInfo{
            .idx = layout.data.tag_union.idx,
            .data = tu_data,
            .alignment = layout.data.tag_union.alignment,
            .variants = self.tag_union_variants.sliceRange(tu_data.getVariants()),
            .contains_refcounted = self.layoutContainsRefcounted(layout),
        };
    }

    /// Get bundled information about a scalar layout
    pub fn getScalarInfo(self: *const Self, layout: Layout) ScalarInfo {
        std.debug.assert(layout.tag == .scalar);
        const scalar = layout.data.scalar;
        const size_align = self.layoutSizeAlign(layout);
        return ScalarInfo{
            .tag = scalar.tag,
            .size = size_align.size,
            .alignment = @as(u32, 1) << @intFromEnum(size_align.alignment),
            .int_precision = if (scalar.tag == .int) scalar.data.int else null,
            .frac_precision = if (scalar.tag == .frac) scalar.data.frac else null,
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
            .scalar => switch (layout.data.scalar.tag) {
                .int => .{
                    .size = @intCast(layout.data.scalar.data.int.size()),
                    .alignment = layout_mod.RocAlignment.fromByteUnits(@intCast(layout.data.scalar.data.int.alignment().toByteUnits())),
                },
                .frac => .{
                    .size = @intCast(layout.data.scalar.data.frac.size()),
                    .alignment = layout_mod.RocAlignment.fromByteUnits(@intCast(layout.data.scalar.data.frac.alignment().toByteUnits())),
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
            .box, .box_of_zst => .{
                .size = @intCast(target_usize.size()), // a Box is just a pointer to refcounted memory
                .alignment = layout_mod.RocAlignment.fromByteUnits(@intCast(target_usize.size())),
            },
            .list, .list_of_zst => .{
                .size = @intCast(3 * target_usize.size()), // ptr, length, capacity
                .alignment = layout_mod.RocAlignment.fromByteUnits(@intCast(target_usize.size())),
            },
            .struct_ => .{
                .size = @intCast(self.getStructSize(layout.data.struct_.idx, layout.data.struct_.alignment)),
                .alignment = layout_mod.RocAlignment.fromByteUnits(@intCast(layout.data.struct_.alignment.toByteUnits())),
            },
            .closure => blk: {
                // Closure layout: header + aligned capture data
                const header_size = @sizeOf(layout_mod.Closure);
                const captures_layout = self.getLayout(layout.data.closure.captures_layout_idx);
                const captures_size_align = self.layoutSizeAlign(captures_layout);
                const aligned_captures_offset = std.mem.alignForward(u32, header_size, @as(u32, @intCast(captures_size_align.alignment.toByteUnits())));
                break :blk .{
                    .size = @intCast(aligned_captures_offset + captures_size_align.size),
                    .alignment = layout_mod.RocAlignment.fromByteUnits(@intCast(target_usize.size())),
                };
            },
            .tag_union => .{
                .size = @intCast(self.getTagUnionSize(layout.data.tag_union.idx, layout.data.tag_union.alignment)),
                .alignment = layout_mod.RocAlignment.fromByteUnits(@intCast(layout.data.tag_union.alignment.toByteUnits())),
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
        const rc_helper = @import("./rc_helper.zig");
        return rc_helper.Resolver.init(self).plan(helper_key);
    }

    pub fn rcHelperStructFieldCount(self: *const Self, struct_plan: @import("./rc_helper.zig").StructPlan) u32 {
        const rc_helper = @import("./rc_helper.zig");
        return rc_helper.Resolver.init(self).structFieldCount(struct_plan);
    }

    pub fn rcHelperStructFieldPlan(self: *const Self, struct_plan: @import("./rc_helper.zig").StructPlan, field_index: u32) ?@import("./rc_helper.zig").FieldPlan {
        const rc_helper = @import("./rc_helper.zig");
        return rc_helper.Resolver.init(self).structFieldPlan(struct_plan, field_index);
    }

    pub fn rcHelperTagUnionVariantCount(self: *const Self, tag_plan: @import("./rc_helper.zig").TagUnionPlan) u32 {
        const rc_helper = @import("./rc_helper.zig");
        return rc_helper.Resolver.init(self).tagUnionVariantCount(tag_plan);
    }

    pub fn rcHelperTagUnionDiscriminantOffset(self: *const Self, tag_plan: @import("./rc_helper.zig").TagUnionPlan) u16 {
        const rc_helper = @import("./rc_helper.zig");
        return rc_helper.Resolver.init(self).tagUnionDiscriminantOffset(tag_plan);
    }

    pub fn rcHelperTagUnionDiscriminantSize(self: *const Self, tag_plan: @import("./rc_helper.zig").TagUnionPlan) u8 {
        const rc_helper = @import("./rc_helper.zig");
        return rc_helper.Resolver.init(self).tagUnionDiscriminantSize(tag_plan);
    }

    pub fn rcHelperTagUnionTotalSize(self: *const Self, tag_plan: @import("./rc_helper.zig").TagUnionPlan) u32 {
        const rc_helper = @import("./rc_helper.zig");
        return rc_helper.Resolver.init(self).tagUnionTotalSize(tag_plan);
    }

    pub fn rcHelperTagUnionVariantPlan(self: *const Self, tag_plan: @import("./rc_helper.zig").TagUnionPlan, variant_index: u32) ?@import("./rc_helper.zig").HelperKey {
        const rc_helper = @import("./rc_helper.zig");
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
            .scalar => return l.data.scalar.tag == .str,
            .list => return true,
            .list_of_zst => return false,
            .box => return true,
            .box_of_zst => return false,
            .zst => return false,
            .struct_, .tag_union, .closure => {},
        }

        try visit_states.put(key, .active);

        const contains_refcounted = switch (l.tag) {
            .struct_ => blk: {
                const sd = self.getStructData(l.data.struct_.idx);
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
                const tu_data = self.getTagUnionData(l.data.tag_union.idx);
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
                const captures_layout = self.getLayout(l.data.closure.captures_layout_idx);
                break :blk try self.layoutContainsRefcountedInner(captures_layout, visit_states);
            },
            .scalar, .list, .list_of_zst, .box, .box_of_zst, .zst => unreachable,
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

    pub fn insertLayout(self: *Self, layout: Layout) std.mem.Allocator.Error!Idx {
        const trace = tracy.traceNamed(@src(), "layoutStore.insertLayout");
        defer trace.end();

        switch (layout.tag) {
            .scalar => return idxFromScalar(layout.data.scalar),
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
                .box => current = layout.data.box,
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

    var store = try Store.init(&.{}, null, testing.allocator, .u64);
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

    const struct_idx = layout_val.data.struct_.idx;
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

    var store = try Store.init(&.{}, null, testing.allocator, .u64);
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
    try testing.expectEqual(interned_layout.data.struct_.alignment, uninterned_layout.data.struct_.alignment);

    const interned_struct = store.getStructData(interned_layout.data.struct_.idx);
    const uninterned_struct = store.getStructData(uninterned_layout.data.struct_.idx);
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

    var store = try Store.init(&.{}, null, testing.allocator, .u64);
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
