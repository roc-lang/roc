//! Stores Layout values by index.

const std = @import("std");
const tracy = @import("tracy");
const base = @import("base");
const types = @import("types");
const collections = @import("collections");
const can = @import("can");

const layout_mod = @import("layout.zig");
const work = @import("./work.zig");

const ModuleEnv = can.ModuleEnv;
const types_store = types.store;
const target = base.target;

/// Key for cross-module type variable lookup in the global layout cache.
/// Different modules can have type variables with the same numeric value that
/// refer to completely different types, so we key by (module_idx, var).
pub const ModuleVarKey = packed struct {
    module_idx: u32,
    var_: types.Var,
};
const Ident = base.Ident;
const Var = types.Var;
const TypeScope = types.TypeScope;
const StaticDispatchConstraint = types.StaticDispatchConstraint;
const Layout = layout_mod.Layout;
const Idx = layout_mod.Idx;
const RecordField = layout_mod.RecordField;
const Scalar = layout_mod.Scalar;
const RecordData = layout_mod.RecordData;
const RecordIdx = layout_mod.RecordIdx;
const TupleField = layout_mod.TupleField;
const TupleData = layout_mod.TupleData;
const TupleIdx = layout_mod.TupleIdx;
const TagUnionVariant = layout_mod.TagUnionVariant;
const TagUnionData = layout_mod.TagUnionData;
const TagUnionIdx = layout_mod.TagUnionIdx;
const SizeAlign = layout_mod.SizeAlign;
const ListInfo = layout_mod.ListInfo;
const BoxInfo = layout_mod.BoxInfo;
const RecordInfo = layout_mod.RecordInfo;
const TupleInfo = layout_mod.TupleInfo;
const TagUnionInfo = layout_mod.TagUnionInfo;
const ScalarInfo = layout_mod.ScalarInfo;
const Work = work.Work;

/// Errors that can occur during layout computation
/// Stores Layout instances by Idx.
///
/// This is a GLOBAL layout store that serves all modules in the build.
/// Layout indices are only meaningful within their originating store, so using
/// per-module stores causes crashes when layout indices cross module boundaries.
pub const Store = struct {
    const Self = @This();

    /// All module environments for cross-module type resolution
    all_module_envs: []const *const ModuleEnv,

    /// Allocator for all internal allocations
    allocator: std.mem.Allocator,

    /// Current module index during fromTypeVar processing
    current_module_idx: u32 = 0,

    /// Optional override types store (used by interpreter for runtime types).
    /// When set, this is used instead of all_module_envs[module_idx].types.
    override_types_store: ?*const types_store.Store = null,

    /// Optional mutable env reference (used by interpreter for runtime identifier insertion).
    /// When set, getMutableEnv() returns this instead of null.
    mutable_env: ?*ModuleEnv = null,

    layouts: collections.SafeList(Layout),
    tuple_elems: collections.SafeList(Idx),
    record_fields: RecordField.SafeMultiList,
    record_data: collections.SafeList(RecordData),
    tuple_fields: TupleField.SafeMultiList,
    tuple_data: collections.SafeList(TupleData),
    tag_union_variants: TagUnionVariant.SafeMultiList,
    tag_union_data: collections.SafeList(TagUnionData),

    // Cache to avoid duplicate work - keyed by (module_idx, var) for cross-module correctness
    layouts_by_module_var: std.AutoHashMap(ModuleVarKey, Idx),

    // Cache for boxed layouts of recursive nominal types.
    // When a recursive nominal type finishes computing, we store its boxed layout here.
    // This allows List(RecursiveType) to use the boxed element type even after computation.
    // Keyed by (module_idx, var) for cross-module correctness.
    recursive_boxed_layouts: std.AutoHashMap(ModuleVarKey, Idx),

    // Cache for RAW (unboxed) layouts of recursive nominal types.
    // When a recursive nominal is encountered INSIDE a Box/List container during cycle
    // detection, we need a placeholder for the raw layout (not the boxed placeholder).
    // This is because the Box/List container itself provides the boxing.
    // Keyed by (module_idx, var) for cross-module correctness.
    raw_layout_placeholders: std.AutoHashMap(ModuleVarKey, Idx),

    // Reusable work stack for fromTypeVar (so it can be stack-safe instead of recursing)
    work: work.Work,

    // Identifier for "Builtin.Str" to recognize the string type without string comparisons
    // (null when compiling Builtin module itself or when Builtin.Str isn't available)
    builtin_str_ident: ?Ident.Idx,
    // Identifier for unqualified "Str" in the Builtin module (if it exists in this env)
    builtin_str_plain_ident: ?Ident.Idx,

    // Cached List ident to avoid repeated string lookups (null if List doesn't exist in this env)
    list_ident: ?Ident.Idx,

    // Cached Box ident to avoid repeated string lookups (null if Box doesn't exist in this env)
    box_ident: ?Ident.Idx,

    // Cached numeric type idents to avoid repeated string lookups
    u8_ident: ?Ident.Idx,
    i8_ident: ?Ident.Idx,
    u16_ident: ?Ident.Idx,
    i16_ident: ?Ident.Idx,
    u32_ident: ?Ident.Idx,
    i32_ident: ?Ident.Idx,
    u64_ident: ?Ident.Idx,
    i64_ident: ?Ident.Idx,
    u128_ident: ?Ident.Idx,
    i128_ident: ?Ident.Idx,
    f32_ident: ?Ident.Idx,
    f64_ident: ?Ident.Idx,
    dec_ident: ?Ident.Idx,
    bool_ident: ?Ident.Idx,
    // Identifier for unqualified "Bool" in the Builtin module
    bool_plain_ident: ?Ident.Idx,

    // The target's usize type (32-bit or 64-bit) - used for layout calculations
    // This is critical for cross-compilation (e.g., compiling for wasm32 on a 64-bit host)
    target_usize: target.TargetUsize,

    // Number of primitive types that are pre-populated in the layout store
    // Must be kept in sync with the sentinel values in layout.zig Idx enum
    const num_primitives = 17;

    /// Get the sentinel Idx for a given scalar type using pure arithmetic - no branches!
    /// This relies on the careful ordering of ScalarTag and Idx enum values.
    pub fn idxFromScalar(scalar: Scalar) Idx {
        // Map scalar to idx using pure arithmetic:
        // opaque_ptr (tag 0) -> 2
        // str (tag 1) -> 1
        // int (tag 2) with precision p -> 3 + p
        // frac (tag 3) with precision p -> 13 + (p - 2) = 11 + p

        const tag = @intFromEnum(scalar.tag);

        // Get the precision bits directly from the packed representation
        // This works because in a packed union, all fields start at bit 0
        const scalar_bits = @as(u7, @bitCast(scalar));
        const precision = scalar_bits & 0xF; // Lower 4 bits contain precision for numeric types

        // Create masks for different tag ranges
        // is_numeric: 1 when tag >= 2, else 0
        const is_numeric = @as(u7, @intFromBool(tag >= 2));

        // Calculate the base index based on tag mappings
        const base_idx = switch (scalar.tag) {
            .opaque_ptr => @as(u7, 2),
            .str => @as(u7, 1),
            .int => @as(u7, 3),
            .frac => @as(u7, 11), // 13 - 2 = 11, so 11 + p gives correct result
        };

        // Calculate the final index
        // For non-numeric: idx = base_idx (precision is 0)
        // For int: idx = base_idx + precision
        // For frac: idx = base_idx + precision (where base_idx is already adjusted)
        return @enumFromInt(base_idx + (is_numeric * precision));
    }

    pub fn init(
        all_module_envs: []const *const ModuleEnv,
        builtin_str_ident: ?Ident.Idx,
        allocator: std.mem.Allocator,
        target_usize: target.TargetUsize,
    ) std.mem.Allocator.Error!Self {
        // Use module 0's idents for builtin type identification
        const env = all_module_envs[0];

        var layouts = collections.SafeList(Layout){};

        // Pre-populate primitive type layouts in order matching the Idx enum.
        // Changing the order of these can break things!
        _ = try layouts.append(allocator, Layout.boolType());
        _ = try layouts.append(allocator, Layout.str());
        _ = try layouts.append(allocator, Layout.opaquePtr());
        _ = try layouts.append(allocator, Layout.int(.u8));
        _ = try layouts.append(allocator, Layout.int(.i8));
        _ = try layouts.append(allocator, Layout.int(.u16));
        _ = try layouts.append(allocator, Layout.int(.i16));
        _ = try layouts.append(allocator, Layout.int(.u32));
        _ = try layouts.append(allocator, Layout.int(.i32));
        _ = try layouts.append(allocator, Layout.int(.u64));
        _ = try layouts.append(allocator, Layout.int(.i64));
        _ = try layouts.append(allocator, Layout.int(.u128));
        _ = try layouts.append(allocator, Layout.int(.i128));
        _ = try layouts.append(allocator, Layout.frac(.f32));
        _ = try layouts.append(allocator, Layout.frac(.f64));
        _ = try layouts.append(allocator, Layout.frac(.dec));
        _ = try layouts.append(allocator, Layout.zst());

        std.debug.assert(layouts.len() == num_primitives);

        return .{
            .all_module_envs = all_module_envs,
            .allocator = allocator,
            .layouts = layouts,
            .tuple_elems = try collections.SafeList(Idx).initCapacity(allocator, 512),
            .record_fields = try RecordField.SafeMultiList.initCapacity(allocator, 256),
            .record_data = try collections.SafeList(RecordData).initCapacity(allocator, 256),
            .tuple_fields = try TupleField.SafeMultiList.initCapacity(allocator, 256),
            .tuple_data = try collections.SafeList(TupleData).initCapacity(allocator, 256),
            .tag_union_variants = try TagUnionVariant.SafeMultiList.initCapacity(allocator, 64),
            .tag_union_data = try collections.SafeList(TagUnionData).initCapacity(allocator, 64),
            .layouts_by_module_var = std.AutoHashMap(ModuleVarKey, Idx).init(allocator),
            .recursive_boxed_layouts = std.AutoHashMap(ModuleVarKey, Idx).init(allocator),
            .raw_layout_placeholders = std.AutoHashMap(ModuleVarKey, Idx).init(allocator),
            .work = try Work.initCapacity(allocator, 32),
            .builtin_str_ident = builtin_str_ident,
            .builtin_str_plain_ident = env.idents.str,
            .list_ident = env.idents.list,
            .box_ident = env.idents.box,
            .u8_ident = env.idents.u8_type,
            .i8_ident = env.idents.i8_type,
            .u16_ident = env.idents.u16_type,
            .i16_ident = env.idents.i16_type,
            .u32_ident = env.idents.u32_type,
            .i32_ident = env.idents.i32_type,
            .u64_ident = env.idents.u64_type,
            .i64_ident = env.idents.i64_type,
            .u128_ident = env.idents.u128_type,
            .i128_ident = env.idents.i128_type,
            .f32_ident = env.idents.f32_type,
            .f64_ident = env.idents.f64_type,
            .dec_ident = env.idents.dec_type,
            .bool_ident = env.idents.bool_type,
            .bool_plain_ident = env.idents.bool,
            .target_usize = target_usize,
        };
    }

    /// Get the types store for the current module being processed.
    /// If an override types store is set, it takes precedence (used by interpreter).
    fn getTypesStore(self: *const Self) *const types_store.Store {
        if (self.override_types_store) |override| return override;
        return &self.all_module_envs[self.current_module_idx].types;
    }

    /// Get the current module environment
    pub fn currentEnv(self: *const Self) *const ModuleEnv {
        return self.all_module_envs[self.current_module_idx];
    }

    /// Set an override types store for runtime type resolution (used by interpreter).
    /// When set, fromTypeVar will use this store instead of all_module_envs[module_idx].types.
    pub fn setOverrideTypesStore(self: *Self, override: *const types_store.Store) void {
        self.override_types_store = override;
    }

    /// Get the primary module environment (module at index 0) as const.
    /// This is a public accessor for read-only identifier operations.
    pub fn getEnv(self: *const Self) *const ModuleEnv {
        return self.all_module_envs[0];
    }

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
        self.layouts.deinit(self.allocator);
        self.tuple_elems.deinit(self.allocator);
        self.record_fields.deinit(self.allocator);
        self.record_data.deinit(self.allocator);
        self.tuple_fields.deinit(self.allocator);
        self.tuple_data.deinit(self.allocator);
        self.tag_union_variants.deinit(self.allocator);
        self.tag_union_data.deinit(self.allocator);
        self.layouts_by_module_var.deinit();
        self.recursive_boxed_layouts.deinit();
        self.raw_layout_placeholders.deinit();
        self.work.deinit(self.allocator);
    }

    /// Check if a constraint range contains a from_numeral constraint.
    /// This is used to determine if an unbound type variable represents
    /// a numeric type (which should default to Dec) or a phantom type (which is a ZST).
    fn hasFromNumeralConstraint(self: *const Self, constraints: StaticDispatchConstraint.SafeList.Range) bool {
        // Empty constraints can't contain from_numeral
        if (constraints.isEmpty()) {
            return false;
        }
        for (self.getTypesStore().sliceStaticDispatchConstraints(constraints)) |constraint| {
            if (constraint.origin == .from_numeral) {
                return true;
            }
        }
        return false;
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

    /// Insert a record layout with the given alignment and record metadata
    pub fn insertRecord(self: *Self, alignment: std.mem.Alignment, record_idx: RecordIdx) std.mem.Allocator.Error!Idx {
        const layout = Layout.record(alignment, record_idx);
        return try self.insertLayout(layout);
    }

    pub fn putRecord(
        self: *Self,
        env: *const ModuleEnv,
        field_layouts: []const Layout,
        field_names: []const Ident.Idx,
    ) std.mem.Allocator.Error!Idx {
        const trace = tracy.traceNamed(@src(), "layoutStore.putRecord");
        defer trace.end();

        var temp_fields = std.ArrayList(RecordField).empty;
        defer temp_fields.deinit(self.allocator);

        for (field_layouts, field_names) |field_layout, field_name| {
            const field_layout_idx = try self.insertLayout(field_layout);
            try temp_fields.append(self.allocator, .{
                .name = field_name,
                .layout = field_layout_idx,
            });
        }

        // Sort fields
        const AlignmentSortCtx = struct {
            store: *Self,
            env: *const ModuleEnv,
            target_usize: target.TargetUsize,
            pub fn lessThan(ctx: @This(), lhs: RecordField, rhs: RecordField) bool {
                const lhs_layout = ctx.store.getLayout(lhs.layout);
                const rhs_layout = ctx.store.getLayout(rhs.layout);
                const lhs_alignment = lhs_layout.alignment(ctx.target_usize);
                const rhs_alignment = rhs_layout.alignment(ctx.target_usize);
                if (lhs_alignment.toByteUnits() != rhs_alignment.toByteUnits()) {
                    return lhs_alignment.toByteUnits() > rhs_alignment.toByteUnits();
                }
                const lhs_str = ctx.env.getIdent(lhs.name);
                const rhs_str = ctx.env.getIdent(rhs.name);
                return std.mem.order(u8, lhs_str, rhs_str) == .lt;
            }
        };

        // Handle empty records specially to avoid NonEmptyRange with count=0
        if (temp_fields.items.len == 0) {
            return self.getEmptyRecordLayout();
        }

        std.mem.sort(
            RecordField,
            temp_fields.items,
            AlignmentSortCtx{ .store = self, .env = env, .target_usize = self.targetUsize() },
            AlignmentSortCtx.lessThan,
        );

        const fields_start = self.record_fields.items.len;
        for (temp_fields.items) |sorted_field| {
            _ = try self.record_fields.append(self.allocator, sorted_field);
        }

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
        const fields_range = collections.NonEmptyRange{ .start = @intCast(fields_start), .count = @intCast(temp_fields.items.len) };
        const record_idx = RecordIdx{ .int_idx = @intCast(self.record_data.len()) };
        _ = try self.record_data.append(self.allocator, .{
            .size = total_size,
            .fields = fields_range,
        });

        const record_layout = Layout.record(std.mem.Alignment.fromByteUnits(max_alignment), record_idx);
        return try self.insertLayout(record_layout);
    }

    /// Insert a tuple layout with the given alignment and tuple metadata
    pub fn insertTuple(self: *Self, alignment: std.mem.Alignment, tuple_idx: TupleIdx) std.mem.Allocator.Error!Idx {
        const layout = Layout.tuple(alignment, tuple_idx);
        return try self.insertLayout(layout);
    }

    /// Insert a tuple layout from concrete element layouts
    pub fn putTuple(self: *Self, element_layouts: []const Layout) std.mem.Allocator.Error!Idx {
        const trace = tracy.traceNamed(@src(), "layoutStore.putTuple");
        defer trace.end();

        // Collect fields
        var temp_fields = std.ArrayList(TupleField).empty;
        defer temp_fields.deinit(self.allocator);

        for (element_layouts, 0..) |elem_layout, i| {
            const elem_idx = try self.insertLayout(elem_layout);
            try temp_fields.append(self.allocator, .{ .index = @intCast(i), .layout = elem_idx });
        }

        // Sort by alignment desc, then by original index asc
        const AlignmentSortCtx = struct {
            store: *Self,
            target_usize: target.TargetUsize,
            pub fn lessThan(ctx: @This(), lhs: TupleField, rhs: TupleField) bool {
                const lhs_layout = ctx.store.getLayout(lhs.layout);
                const rhs_layout = ctx.store.getLayout(rhs.layout);
                const lhs_alignment = lhs_layout.alignment(ctx.target_usize);
                const rhs_alignment = rhs_layout.alignment(ctx.target_usize);
                if (lhs_alignment.toByteUnits() != rhs_alignment.toByteUnits()) {
                    return lhs_alignment.toByteUnits() > rhs_alignment.toByteUnits();
                }
                return lhs.index < rhs.index;
            }
        };

        std.mem.sort(
            TupleField,
            temp_fields.items,
            AlignmentSortCtx{ .store = self, .target_usize = self.targetUsize() },
            AlignmentSortCtx.lessThan,
        );

        // Append fields
        const fields_start = self.tuple_fields.items.len;
        for (temp_fields.items) |sorted_field| {
            _ = try self.tuple_fields.append(self.allocator, sorted_field);
        }

        // Compute size and alignment
        var max_alignment: usize = 1;
        var current_offset: u32 = 0;
        for (temp_fields.items) |tf| {
            const field_layout = self.getLayout(tf.layout);
            const field_size_align = self.layoutSizeAlign(field_layout);
            const field_alignment = field_size_align.alignment.toByteUnits();
            max_alignment = @max(max_alignment, field_alignment);
            current_offset = @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(field_alignment))));
            current_offset += field_size_align.size;
        }

        const total_size = @as(u32, @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(max_alignment)))));
        const fields_range = collections.NonEmptyRange{ .start = @intCast(fields_start), .count = @intCast(temp_fields.items.len) };
        const tuple_idx = TupleIdx{ .int_idx = @intCast(self.tuple_data.len()) };
        _ = try self.tuple_data.append(self.allocator, TupleData{ .size = total_size, .fields = fields_range });
        const tuple_layout = Layout.tuple(std.mem.Alignment.fromByteUnits(max_alignment), tuple_idx);
        return try self.insertLayout(tuple_layout);
    }

    /// Create a tag union layout from pre-computed variant payload layouts.
    /// `variant_layouts[i]` is the layout Idx for variant i's payload
    /// (use ensureZstLayout() for no-payload variants).
    /// Tags must be sorted alphabetically; variant_layouts[i] corresponds
    /// to the tag at sorted index i.
    pub fn putTagUnion(self: *Self, variant_layouts: []const Idx) std.mem.Allocator.Error!Idx {
        const variants_start: u32 = @intCast(self.tag_union_variants.len());

        var max_payload_size: u32 = 0;
        var max_payload_alignment: std.mem.Alignment = .@"1";

        for (variant_layouts) |variant_layout_idx| {
            const variant_layout = self.getLayout(variant_layout_idx);
            const variant_size = self.layoutSize(variant_layout);
            const variant_alignment = variant_layout.alignment(self.targetUsize());
            if (variant_size > max_payload_size) max_payload_size = variant_size;
            max_payload_alignment = max_payload_alignment.max(variant_alignment);

            _ = try self.tag_union_variants.append(self.allocator, .{
                .payload_layout = variant_layout_idx,
            });
        }

        // Discriminant size from variant count
        const discriminant_size: u8 = if (variant_layouts.len <= 256) 1 else if (variant_layouts.len <= 65536) 2 else if (variant_layouts.len <= (1 << 32)) 4 else 8;
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

        const tag_union_data_idx: u32 = @intCast(self.tag_union_data.len());
        _ = try self.tag_union_data.append(self.allocator, .{
            .size = total_size,
            .discriminant_offset = discriminant_offset,
            .discriminant_size = discriminant_size,
            .variants = .{
                .start = variants_start,
                .count = @intCast(variant_layouts.len),
            },
        });

        const tu_layout = Layout.tagUnion(tag_union_alignment, .{ .int_idx = @intCast(tag_union_data_idx) });
        return try self.insertLayout(tu_layout);
    }

    /// Create a tuple layout representing the sequential layout of closure captures.
    /// Captures are stored with alignment padding between them, like tuple fields.
    pub fn putCaptureStruct(self: *Self, capture_layout_idxs: []const Idx) std.mem.Allocator.Error!Idx {
        var temp_fields = std.ArrayList(TupleField).empty;
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

        const fields_start = self.tuple_fields.items.len;
        for (temp_fields.items) |field| {
            _ = try self.tuple_fields.append(self.allocator, field);
        }

        const fields_range = collections.NonEmptyRange{ .start = @intCast(fields_start), .count = @intCast(temp_fields.items.len) };
        const tuple_idx = TupleIdx{ .int_idx = @intCast(self.tuple_data.len()) };
        _ = try self.tuple_data.append(self.allocator, TupleData{ .size = total_size, .fields = fields_range });
        const capture_layout = Layout.tuple(std.mem.Alignment.fromByteUnits(max_alignment), tuple_idx);
        return try self.insertLayout(capture_layout);
    }

    /// Create a tuple layout representing the sequential layout of a lambda set union.
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

        // Create a tuple layout with a single dummy field (TupleData requires NonEmptyRange)
        const fields_start = self.tuple_fields.items.len;
        _ = try self.tuple_fields.append(self.allocator, .{ .index = 0, .layout = .u64 });
        const fields_range = collections.NonEmptyRange{ .start = @intCast(fields_start), .count = 1 };
        const tuple_idx = TupleIdx{ .int_idx = @intCast(self.tuple_data.len()) };
        _ = try self.tuple_data.append(self.allocator, TupleData{ .size = total_size, .fields = fields_range });
        const union_layout = Layout.tuple(std.mem.Alignment.fromByteUnits(max_alignment), tuple_idx);
        return try self.insertLayout(union_layout);
    }

    pub fn getLayout(self: *const Self, idx: Idx) Layout {
        return self.layouts.get(@enumFromInt(@intFromEnum(idx))).*;
    }

    pub fn getRecordData(self: *const Self, idx: RecordIdx) *const RecordData {
        return self.record_data.get(@enumFromInt(idx.int_idx));
    }

    pub fn getTupleData(self: *const Self, idx: TupleIdx) *const TupleData {
        return self.tuple_data.get(@enumFromInt(idx.int_idx));
    }

    pub fn getTagUnionData(self: *const Self, idx: TagUnionIdx) *const TagUnionData {
        return self.tag_union_data.get(@enumFromInt(idx.int_idx));
    }

    pub fn getTagUnionVariants(self: *const Self, data: *const TagUnionData) TagUnionVariant.SafeMultiList.Slice {
        return self.tag_union_variants.sliceRange(data.getVariants());
    }

    /// Get bundled information about a list layout's element
    pub fn getListInfo(self: *const Self, layout: Layout) ListInfo {
        std.debug.assert(layout.tag == .list or layout.tag == .list_of_zst);
        const elem_layout_idx = layout.data.list;
        const elem_layout = self.getLayout(elem_layout_idx);
        return ListInfo{
            .elem_layout_idx = elem_layout_idx,
            .elem_layout = elem_layout,
            .elem_size = self.layoutSize(elem_layout),
            .elem_alignment = @intCast(elem_layout.alignment(self.targetUsize()).toByteUnits()),
            .contains_refcounted = self.layoutContainsRefcounted(elem_layout),
        };
    }

    /// Get bundled information about a box layout's element
    pub fn getBoxInfo(self: *const Self, layout: Layout) BoxInfo {
        std.debug.assert(layout.tag == .box or layout.tag == .box_of_zst);
        const elem_layout_idx = layout.data.box;
        const elem_layout = self.getLayout(elem_layout_idx);
        return BoxInfo{
            .elem_layout_idx = elem_layout_idx,
            .elem_layout = elem_layout,
            .elem_size = self.layoutSize(elem_layout),
            .elem_alignment = @intCast(elem_layout.alignment(self.targetUsize()).toByteUnits()),
            .contains_refcounted = self.layoutContainsRefcounted(elem_layout),
        };
    }

    /// Get bundled information about a record layout
    pub fn getRecordInfo(self: *const Self, layout: Layout) RecordInfo {
        std.debug.assert(layout.tag == .record);
        const record_data = self.getRecordData(layout.data.record.idx);
        return RecordInfo{
            .data = record_data,
            .alignment = layout.data.record.alignment,
            .fields = self.record_fields.sliceRange(record_data.getFields()),
            .contains_refcounted = self.layoutContainsRefcounted(layout),
        };
    }

    /// Get bundled information about a tuple layout
    pub fn getTupleInfo(self: *const Self, layout: Layout) TupleInfo {
        std.debug.assert(layout.tag == .tuple);
        const tuple_data = self.getTupleData(layout.data.tuple.idx);
        return TupleInfo{
            .data = tuple_data,
            .alignment = layout.data.tuple.alignment,
            .fields = self.tuple_fields.sliceRange(tuple_data.getFields()),
            .contains_refcounted = self.layoutContainsRefcounted(layout),
        };
    }

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

    /// Dynamically compute the discriminant offset for a tag union.
    /// This computes the offset based on current variant payload sizes,
    /// which is necessary for recursive types where placeholder layouts
    /// may have been updated after the tag union was initially created.
    pub fn getTagUnionDiscriminantOffset(self: *const Self, tu_idx: TagUnionIdx) u16 {
        const tu_data = self.getTagUnionData(tu_idx);
        const variants = self.getTagUnionVariants(tu_data);

        // Find the maximum payload size across all variants
        var max_payload_size: u32 = 0;
        for (0..variants.len) |i| {
            const variant = variants.get(i);
            const variant_layout = self.getLayout(variant.payload_layout);
            const variant_size = self.layoutSize(variant_layout);
            if (variant_size > max_payload_size) {
                max_payload_size = variant_size;
            }
        }

        // Align the discriminant offset to the discriminant's alignment
        const disc_align = tu_data.discriminantAlignment();
        return @intCast(std.mem.alignForward(u32, max_payload_size, @intCast(disc_align.toByteUnits())));
    }

    /// Dynamically compute the total size of a tag union.
    /// This computes the size based on current variant payload sizes.
    pub fn getTagUnionSize(self: *const Self, tu_idx: TagUnionIdx, alignment: std.mem.Alignment) u32 {
        const tu_data = self.getTagUnionData(tu_idx);
        const disc_offset = self.getTagUnionDiscriminantOffset(tu_idx);
        const total_unaligned = disc_offset + tu_data.discriminant_size;
        return std.mem.alignForward(u32, total_unaligned, @intCast(alignment.toByteUnits()));
    }

    /// Create a new tag_union layout with a specific variant's payload layout replaced.
    /// This is used when the actual payload layout differs from the type's expected layout,
    /// to ensure correct decref behavior for nested containers (e.g., lists with different
    /// element layouts). Returns a new tag_union layout with correct variant payloads.
    pub fn createTagUnionWithPayload(
        self: *Self,
        original_tu_idx: TagUnionIdx,
        variant_index: u32,
        new_payload_layout_idx: Idx,
    ) std.mem.Allocator.Error!Layout {
        const tu_data = self.getTagUnionData(original_tu_idx);
        const variants = self.getTagUnionVariants(tu_data);

        // Record where new variants will start
        const variants_start: u32 = @intCast(self.tag_union_variants.len());

        // Copy all variants, replacing the specified one's payload layout
        var max_payload_size: u32 = 0;
        var max_payload_alignment: std.mem.Alignment = .@"1";
        for (0..variants.len) |i| {
            const variant = variants.get(i);
            const payload_idx = if (i == variant_index) new_payload_layout_idx else variant.payload_layout;
            _ = try self.tag_union_variants.append(self.allocator, .{
                .payload_layout = payload_idx,
            });

            // Track max size and alignment for the new discriminant offset
            const payload_layout = self.getLayout(payload_idx);
            const payload_size = self.layoutSize(payload_layout);
            const payload_align = payload_layout.alignment(self.targetUsize());
            if (payload_size > max_payload_size) max_payload_size = payload_size;
            max_payload_alignment = max_payload_alignment.max(payload_align);
        }

        // Calculate discriminant offset and total size
        const disc_align = tu_data.discriminantAlignment();
        const discriminant_offset: u16 = @intCast(std.mem.alignForward(u32, max_payload_size, @intCast(disc_align.toByteUnits())));
        const tag_union_alignment = max_payload_alignment.max(disc_align);
        const total_size_unaligned = discriminant_offset + tu_data.discriminant_size;
        const total_size = std.mem.alignForward(u32, total_size_unaligned, @intCast(tag_union_alignment.toByteUnits()));

        // Store new TagUnionData
        const tag_union_data_idx: u32 = @intCast(self.tag_union_data.len());
        _ = try self.tag_union_data.append(self.allocator, .{
            .size = total_size,
            .discriminant_offset = discriminant_offset,
            .discriminant_size = tu_data.discriminant_size,
            .variants = .{
                .start = variants_start,
                .count = @intCast(variants.len),
            },
        });

        return Layout.tagUnion(tag_union_alignment, .{ .int_idx = @intCast(tag_union_data_idx) });
    }

    /// Dynamically compute the total size of a tuple.
    /// This computes the size based on current field layout sizes.
    pub fn getTupleSize(self: *const Self, tuple_idx: TupleIdx, alignment: std.mem.Alignment) u32 {
        const tuple_data = self.getTupleData(tuple_idx);
        const fields = self.tuple_fields.sliceRange(tuple_data.getFields());

        var current_offset: u32 = 0;
        for (0..fields.len) |i| {
            const field = fields.get(i);
            const field_layout = self.getLayout(field.layout);
            const field_size_align = self.layoutSizeAlign(field_layout);

            // Align current offset to field's alignment
            current_offset = @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(field_size_align.alignment.toByteUnits()))));

            // Add field size
            current_offset += field_size_align.size;
        }

        // Final alignment
        return std.mem.alignForward(u32, current_offset, @intCast(alignment.toByteUnits()));
    }

    /// Dynamically compute the total size of a record.
    /// This computes the size based on current field layout sizes.
    pub fn getRecordSize(self: *const Self, record_idx: RecordIdx, alignment: std.mem.Alignment) u32 {
        const record_data = self.getRecordData(record_idx);
        const fields = self.record_fields.sliceRange(record_data.getFields());

        var current_offset: u32 = 0;
        for (0..fields.len) |i| {
            const field = fields.get(i);
            const field_layout = self.getLayout(field.layout);
            const field_size_align = self.layoutSizeAlign(field_layout);

            // Align current offset to field's alignment
            current_offset = @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(field_size_align.alignment.toByteUnits()))));

            // Add field size
            current_offset += field_size_align.size;
        }

        // Final alignment
        return std.mem.alignForward(u32, current_offset, @intCast(alignment.toByteUnits()));
    }

    pub fn getRecordFieldOffset(self: *const Self, record_idx: RecordIdx, field_index_in_sorted_fields: u32) u32 {
        const record_data = self.getRecordData(record_idx);
        const sorted_fields = self.record_fields.sliceRange(record_data.getFields());

        var current_offset: u32 = 0;
        var field_idx: u32 = 0;

        while (field_idx < field_index_in_sorted_fields) : (field_idx += 1) {
            const field = sorted_fields.get(field_idx);
            const field_layout = self.getLayout(field.layout);
            const field_size_align = self.layoutSizeAlign(field_layout);

            // Align current offset to field's alignment
            current_offset = @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(field_size_align.alignment.toByteUnits()))));

            // Add field size
            current_offset += field_size_align.size;
        }

        // Now, align the offset for the requested field
        const requested_field = sorted_fields.get(field_index_in_sorted_fields);
        const requested_field_layout = self.getLayout(requested_field.layout);
        const requested_field_size_align = self.layoutSizeAlign(requested_field_layout);
        return @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(requested_field_size_align.alignment.toByteUnits()))));
    }

    /// Get the size of a record field at the given sorted index.
    pub fn getRecordFieldSize(self: *const Self, record_idx: RecordIdx, field_index_in_sorted_fields: u32) u32 {
        const record_data = self.getRecordData(record_idx);
        const sorted_fields = self.record_fields.sliceRange(record_data.getFields());
        const field = sorted_fields.get(field_index_in_sorted_fields);
        const field_layout = self.getLayout(field.layout);
        return self.layoutSizeAlign(field_layout).size;
    }

    /// Get the layout index of a record field at the given sorted index.
    pub fn getRecordFieldLayout(self: *const Self, record_idx: RecordIdx, field_index_in_sorted_fields: u32) Idx {
        const record_data = self.getRecordData(record_idx);
        const sorted_fields = self.record_fields.sliceRange(record_data.getFields());
        return sorted_fields.get(field_index_in_sorted_fields).layout;
    }

    pub fn getRecordFieldOffsetByName(self: *const Self, record_idx: RecordIdx, field_name_idx: Ident.Idx) ?u32 {
        const record_data = self.getRecordData(record_idx);
        const sorted_fields = self.record_fields.sliceRange(record_data.getFields());

        var current_offset: u32 = 0;
        var i: usize = 0;
        while (i < sorted_fields.len) : (i += 1) {
            const field = sorted_fields.get(i);
            const field_layout = self.getLayout(field.layout);
            const field_size_align = self.layoutSizeAlign(field_layout);

            current_offset = @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(field_size_align.alignment.toByteUnits()))));

            if (field.name == field_name_idx) {
                return current_offset;
            }

            current_offset += field_size_align.size;
        }

        return null;
    }

    pub fn getTupleElementOffset(self: *const Self, tuple_idx: TupleIdx, element_index_in_sorted_elements: u32) u32 {
        const tuple_data = self.getTupleData(tuple_idx);
        const sorted_elements = self.tuple_fields.sliceRange(tuple_data.getFields());

        var current_offset: u32 = 0;
        var element_idx: u32 = 0;

        while (element_idx < element_index_in_sorted_elements) : (element_idx += 1) {
            const element = sorted_elements.get(element_idx);
            const element_layout = self.getLayout(element.layout);
            const element_size_align = self.layoutSizeAlign(element_layout);

            // Align current offset to element's alignment
            current_offset = @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(element_size_align.alignment.toByteUnits()))));

            // Add element size
            current_offset += element_size_align.size;
        }

        // Now, align the offset for the requested element
        const requested_element = sorted_elements.get(element_index_in_sorted_elements);
        const requested_element_layout = self.getLayout(requested_element.layout);
        const requested_element_size_align = self.layoutSizeAlign(requested_element_layout);
        return @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(requested_element_size_align.alignment.toByteUnits()))));
    }

    /// Get the size of a tuple element at the given sorted index.
    pub fn getTupleElementSize(self: *const Self, tuple_idx: TupleIdx, element_index_in_sorted_elements: u32) u32 {
        const tuple_data = self.getTupleData(tuple_idx);
        const sorted_elements = self.tuple_fields.sliceRange(tuple_data.getFields());
        const element = sorted_elements.get(element_index_in_sorted_elements);
        const element_layout = self.getLayout(element.layout);
        return self.layoutSizeAlign(element_layout).size;
    }

    /// Get the layout index of a tuple element at the given sorted index.
    pub fn getTupleElementLayout(self: *const Self, tuple_idx: TupleIdx, element_index_in_sorted_elements: u32) Idx {
        const tuple_data = self.getTupleData(tuple_idx);
        const sorted_elements = self.tuple_fields.sliceRange(tuple_data.getFields());
        return sorted_elements.get(element_index_in_sorted_elements).layout;
    }

    /// Get the offset of a tuple element by its ORIGINAL index (source order).
    /// This searches through the sorted elements to find the one with the matching original index.
    pub fn getTupleElementOffsetByOriginalIndex(self: *const Self, tuple_idx: TupleIdx, original_index: u32) u32 {
        const tuple_data = self.getTupleData(tuple_idx);
        const sorted_elements = self.tuple_fields.sliceRange(tuple_data.getFields());

        // Find the sorted position of the element with the given original index
        var sorted_position: ?u32 = null;
        for (0..sorted_elements.len) |i| {
            const element = sorted_elements.get(@intCast(i));
            if (element.index == original_index) {
                sorted_position = @intCast(i);
                break;
            }
        }

        // Use the sorted position to get the offset
        const pos = sorted_position orelse return 0; // Shouldn't happen if original_index is valid
        return self.getTupleElementOffset(tuple_idx, pos);
    }

    /// Get the layout index of a tuple element by its ORIGINAL index (source order).
    pub fn getTupleElementLayoutByOriginalIndex(self: *const Self, tuple_idx: TupleIdx, original_index: u32) Idx {
        const tuple_data = self.getTupleData(tuple_idx);
        const sorted_elements = self.tuple_fields.sliceRange(tuple_data.getFields());

        for (0..sorted_elements.len) |i| {
            const element = sorted_elements.get(@intCast(i));
            if (element.index == original_index) {
                return element.layout;
            }
        }

        return .none; // Shouldn't happen if original_index is valid
    }

    /// Get the size of a tuple element by its ORIGINAL index (source order).
    pub fn getTupleElementSizeByOriginalIndex(self: *const Self, tuple_idx: TupleIdx, original_index: u32) u32 {
        const tuple_data = self.getTupleData(tuple_idx);
        const sorted_elements = self.tuple_fields.sliceRange(tuple_data.getFields());

        // Find the element with the given original index
        for (0..sorted_elements.len) |i| {
            const element = sorted_elements.get(@intCast(i));
            if (element.index == original_index) {
                const element_layout = self.getLayout(element.layout);
                return self.layoutSizeAlign(element_layout).size;
            }
        }

        return 0; // Shouldn't happen if original_index is valid
    }

    pub fn targetUsize(self: *const Self) target.TargetUsize {
        return self.target_usize;
    }

    /// Get or create an empty record layout (for closures with no captures)
    fn getEmptyRecordLayout(self: *Self) !Idx {
        // Check if we already have an empty record layout
        for (self.record_data.items.items, 0..) |record_data, i| {
            if (record_data.size == 0 and record_data.fields.count == 0) {
                const record_idx = RecordIdx{ .int_idx = @intCast(i) };
                const empty_record_layout = Layout.record(std.mem.Alignment.@"1", record_idx);
                return try self.insertLayout(empty_record_layout);
            }
        }

        // Create new empty record layout
        const record_idx = RecordIdx{ .int_idx = @intCast(self.record_data.len()) };
        _ = try self.record_data.append(self.allocator, .{
            .size = 0,
            .fields = collections.NonEmptyRange{ .start = 0, .count = 0 },
        });
        const empty_record_layout = Layout.record(std.mem.Alignment.@"1", record_idx);
        return try self.insertLayout(empty_record_layout);
    }

    pub fn ensureEmptyRecordLayout(self: *Self) !Idx {
        return self.getEmptyRecordLayout();
    }

    /// Get the boxed layout for a recursive nominal type, if it exists.
    /// This is used for list elements where the element type is a recursive nominal.
    /// Returns null if the type is not a recursive nominal.
    pub fn getRecursiveBoxedLayout(self: *const Self, module_idx: u32, type_var: Var) ?Layout {
        const key = ModuleVarKey{ .module_idx = module_idx, .var_ = type_var };
        if (self.recursive_boxed_layouts.get(key)) |boxed_idx| {
            return self.getLayout(boxed_idx);
        }
        return null;
    }

    /// Check if a nominal type (by identity) is recursive and return its boxed layout.
    /// This is needed because different vars can represent the same nominal type,
    /// and the boxed layout might have been stored under a different var.
    pub fn getRecursiveBoxedLayoutByNominalKey(self: *const Self, nominal_key: work.NominalKey) ?Layout {
        // Iterate through recursive_boxed_layouts to find an entry whose var
        // resolves to this nominal type identity.
        var iter = self.recursive_boxed_layouts.iterator();
        while (iter.next()) |entry| {
            const cache_key = entry.key_ptr.*;
            const boxed_idx = entry.value_ptr.*;
            if (boxed_idx == Idx.none) continue;
            const module_env = self.all_module_envs[cache_key.module_idx];
            const resolved = module_env.types.resolveVar(cache_key.var_);
            if (resolved.desc.content == .structure) {
                const flat_type = resolved.desc.content.structure;
                if (flat_type == .nominal_type) {
                    const nom = flat_type.nominal_type;
                    if (nom.ident.ident_idx == nominal_key.ident_idx and
                        nom.origin_module == nominal_key.origin_module)
                    {
                        return self.getLayout(boxed_idx);
                    }
                }
            }
        }
        return null;
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
                    .size = @intCast(target_usize.size()), // opaque_ptr is pointer-sized
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
            .record => .{
                // Use pre-computed size from RecordData to avoid infinite recursion on recursive types
                .size = @intCast(self.record_data.get(@enumFromInt(layout.data.record.idx.int_idx)).size),
                .alignment = layout_mod.RocAlignment.fromByteUnits(@intCast(layout.data.record.alignment.toByteUnits())),
            },
            .tuple => .{
                // Use pre-computed size from TupleData to avoid infinite recursion on recursive types
                .size = @intCast(self.tuple_data.get(@enumFromInt(layout.data.tuple.idx.int_idx)).size),
                .alignment = layout_mod.RocAlignment.fromByteUnits(@intCast(layout.data.tuple.alignment.toByteUnits())),
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
                // Use pre-computed size from TagUnionData to avoid infinite recursion on recursive types
                .size = @intCast(self.tag_union_data.get(@enumFromInt(layout.data.tag_union.idx.int_idx)).size),
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
        return switch (l.tag) {
            .scalar => switch (l.data.scalar.tag) {
                .str => true,
                else => false,
            },
            .list, .list_of_zst => true,
            .box, .box_of_zst => true,
            .tuple => {
                const tuple_data = self.getTupleData(l.data.tuple.idx);
                const fields = self.tuple_fields.sliceRange(tuple_data.getFields());
                for (0..fields.len) |i| {
                    const field_layout = self.getLayout(fields.get(i).layout);
                    if (self.layoutContainsRefcounted(field_layout)) {
                        return true;
                    }
                }
                return false;
            },
            .record => {
                const record_data = self.getRecordData(l.data.record.idx);
                const fields = self.record_fields.sliceRange(record_data.getFields());
                for (0..fields.len) |i| {
                    const field_layout = self.getLayout(fields.get(i).layout);
                    if (self.layoutContainsRefcounted(field_layout)) {
                        return true;
                    }
                }
                return false;
            },
            .tag_union => {
                const tu_data = self.getTagUnionData(l.data.tag_union.idx);
                const variants = self.getTagUnionVariants(tu_data);
                for (0..variants.len) |i| {
                    const variant_layout = self.getLayout(variants.get(i).payload_layout);
                    if (self.layoutContainsRefcounted(variant_layout)) {
                        return true;
                    }
                }
                return false;
            },
            .closure => {
                // Check if the captured variables contain refcounted data
                const captures_layout = self.getLayout(l.data.closure.captures_layout_idx);
                return self.layoutContainsRefcounted(captures_layout);
            },
            .zst => false,
        };
    }

    /// Add the tag union's tags to self.pending_tags,
    /// then add the tag union's extension fields too (recursively).
    fn gatherTags(
        self: *Self,
        tag_union: types.TagUnion,
    ) std.mem.Allocator.Error!usize {
        var num_tags = tag_union.tags.len();

        const tag_slice = self.getTypesStore().getTagsSlice(tag_union.tags);
        for (tag_slice.items(.name), tag_slice.items(.args)) |name, args| {
            // TODO is it possible that here we're encountering record fields with names
            // already in the list? Would type-checking have already deduped them?
            // We would certainly rather not spend time doing hashmap things if we can avoid it here.
            try self.work.pending_tags.append(self.allocator, .{ .name = name, .args = args });
        }

        var current_ext = tag_union.ext;
        while (true) {
            const resolved_ext = self.getTypesStore().resolveVar(current_ext);
            switch (resolved_ext.desc.content) {
                .structure => |ext_flat_type| switch (ext_flat_type) {
                    .empty_tag_union => {
                        break;
                    },
                    .tag_union => |ext_tag_union| {
                        if (ext_tag_union.tags.len() > 0) {
                            num_tags += ext_tag_union.tags.len();
                            const ext_tag_slice = self.getTypesStore().getTagsSlice(ext_tag_union.tags);
                            for (ext_tag_slice.items(.name), ext_tag_slice.items(.args)) |name, args| {
                                // TODO is it possible that here we're adding fields with names
                                // already in the list? Would type-checking have already collapsed these?
                                // We would certainly rather not spend time doing hashmap things
                                // if we can avoid it here.
                                try self.work.pending_tags.append(self.allocator, .{ .name = name, .args = args });
                            }
                            current_ext = ext_tag_union.ext;
                        } else {
                            break;
                        }
                    },
                    else => unreachable,
                },
                .alias => |alias| {
                    current_ext = self.getTypesStore().getAliasBackingVar(alias);
                },
                // flex and rigid are valid terminal extensions for open unions
                .flex, .rigid => break,
                else => unreachable,
            }
        }

        return num_tags;
    }

    /// Add the record's fields to self.pending_record_fields,
    /// then add the record's extension fields too (recursively).
    fn gatherRecordFields(
        self: *Self,
        record_type: types.Record,
    ) std.mem.Allocator.Error!usize {
        var num_fields = record_type.fields.len();

        const field_slice = self.getTypesStore().getRecordFieldsSlice(record_type.fields);
        for (field_slice.items(.name), field_slice.items(.var_)) |name, var_| {
            // TODO is it possible that here we're encountering record fields with names
            // already in the list? Would type-checking have already deduped them?
            // We would certainly rather not spend time doing hashmap things if we can avoid it here.
            try self.work.pending_record_fields.append(self.allocator, .{ .name = name, .var_ = var_ });
        }

        var current_ext = record_type.ext;
        while (true) {
            const resolved_ext = self.getTypesStore().resolveVar(current_ext);
            switch (resolved_ext.desc.content) {
                .structure => |ext_flat_type| switch (ext_flat_type) {
                    .empty_record => break,
                    .record => |ext_record| {
                        if (ext_record.fields.len() > 0) {
                            num_fields += ext_record.fields.len();
                            const ext_field_slice = self.getTypesStore().getRecordFieldsSlice(ext_record.fields);
                            for (ext_field_slice.items(.name), ext_field_slice.items(.var_)) |name, var_| {
                                // TODO is it possible that here we're adding fields with names
                                // already in the list? Would type-checking have already collapsed these?
                                // We would certainly rather not spend time doing hashmap things
                                // if we can avoid it here.
                                try self.work.pending_record_fields.append(self.allocator, .{ .name = name, .var_ = var_ });
                            }
                            current_ext = ext_record.ext;
                        } else {
                            break;
                        }
                    },
                    .record_unbound => |fields| {
                        if (fields.len() > 0) {
                            num_fields += fields.len();
                            const unbound_field_slice = self.getTypesStore().getRecordFieldsSlice(fields);
                            for (unbound_field_slice.items(.name), unbound_field_slice.items(.var_)) |name, var_| {
                                // TODO is it possible that here we're adding fields with names
                                // already in the list? Would type-checking have already collapsed these?
                                // We would certainly rather not spend time doing hashmap things
                                // if we can avoid it here.
                                try self.work.pending_record_fields.append(self.allocator, .{ .name = name, .var_ = var_ });
                            }
                        }
                        // record_unbound has no extension, so stop here
                        break;
                    },
                    else => unreachable,
                },
                .alias => |alias| {
                    current_ext = self.getTypesStore().getAliasBackingVar(alias);
                },
                .flex => |_| break,
                .rigid => |_| break,
                else => unreachable,
            }
        }

        return num_fields;
    }

    /// Add the tuple's fields to self.pending_tuple_fields
    fn gatherTupleFields(
        self: *Self,
        tuple_type: types.Tuple,
    ) std.mem.Allocator.Error!usize {
        const elem_slice = self.getTypesStore().sliceVars(tuple_type.elems);
        const num_fields = elem_slice.len;

        for (elem_slice, 0..) |var_, index| {
            try self.work.pending_tuple_fields.append(self.allocator, .{ .index = @intCast(index), .var_ = var_ });
        }

        return num_fields;
    }

    fn finishRecord(
        self: *Store,
        updated_record: work.Work.PendingRecord,
    ) std.mem.Allocator.Error!Layout {
        const resolved_fields_end = self.work.resolved_record_fields.len;
        const num_resolved_fields = resolved_fields_end - updated_record.resolved_fields_start;
        const fields_start = self.record_fields.items.len;

        // Copy only this record's resolved fields to the record_fields store
        const field_names = self.work.resolved_record_fields.items(.field_name);
        const field_idxs = self.work.resolved_record_fields.items(.field_idx);

        // First, collect the fields into a temporary array so we can sort them
        var temp_fields = std.ArrayList(RecordField).empty;
        defer temp_fields.deinit(self.allocator);

        for (updated_record.resolved_fields_start..resolved_fields_end) |i| {
            try temp_fields.append(self.allocator, .{
                .name = field_names[i],
                .layout = field_idxs[i],
            });
        }

        // Sort fields by alignment (descending) first, then by name (ascending)
        const AlignmentSortCtx = struct {
            store: *Self,
            env: *const ModuleEnv,
            target_usize: target.TargetUsize,
            pub fn lessThan(ctx: @This(), lhs: RecordField, rhs: RecordField) bool {
                const lhs_layout = ctx.store.getLayout(lhs.layout);
                const rhs_layout = ctx.store.getLayout(rhs.layout);

                const lhs_alignment = lhs_layout.alignment(ctx.target_usize);
                const rhs_alignment = rhs_layout.alignment(ctx.target_usize);

                // First sort by alignment (descending - higher alignment first)
                if (lhs_alignment.toByteUnits() != rhs_alignment.toByteUnits()) {
                    return lhs_alignment.toByteUnits() > rhs_alignment.toByteUnits();
                }

                // Then sort by name (ascending)
                const lhs_str = ctx.env.getIdent(lhs.name);
                const rhs_str = ctx.env.getIdent(rhs.name);
                return std.mem.order(u8, lhs_str, rhs_str) == .lt;
            }
        };

        std.mem.sort(
            RecordField,
            temp_fields.items,
            AlignmentSortCtx{ .store = self, .env = self.currentEnv(), .target_usize = self.targetUsize() },
            AlignmentSortCtx.lessThan,
        );

        // Now add them to the record_fields store in the sorted order
        for (temp_fields.items) |sorted_field| {
            _ = try self.record_fields.append(self.allocator, sorted_field);
        }

        // Calculate max alignment and total size of all fields
        var max_alignment: usize = 1;
        var current_offset: u32 = 0;
        var field_idx: u32 = 0;

        while (field_idx < temp_fields.items.len) : (field_idx += 1) {
            const temp_field = temp_fields.items[field_idx];
            const field_layout = self.getLayout(temp_field.layout);
            const field_size_align = self.layoutSizeAlign(field_layout);

            // Update max alignment
            max_alignment = @max(max_alignment, field_size_align.alignment.toByteUnits());

            // Align current offset to field's alignment
            current_offset = @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(field_size_align.alignment.toByteUnits()))));

            // Add field size
            current_offset = current_offset + field_size_align.size;
        }

        // Final size must be aligned to the record's alignment
        const total_size = @as(u32, @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(max_alignment)))));

        // Create the record layout with the fields range
        const fields_range = collections.NonEmptyRange{ .start = @intCast(fields_start), .count = @intCast(num_resolved_fields) };

        // Store the record data
        const record_idx = RecordIdx{ .int_idx = @intCast(self.record_data.len()) };
        _ = try self.record_data.append(self.allocator, RecordData{
            .size = total_size,
            .fields = fields_range,
        });

        // Remove only this record's resolved fields
        self.work.resolved_record_fields.shrinkRetainingCapacity(updated_record.resolved_fields_start);

        return Layout.record(std.mem.Alignment.fromByteUnits(max_alignment), record_idx);
    }

    fn finishTuple(
        self: *Store,
        updated_tuple: work.Work.PendingTuple,
    ) std.mem.Allocator.Error!Layout {
        const resolved_fields_end = self.work.resolved_tuple_fields.len;
        const num_resolved_fields = resolved_fields_end - updated_tuple.resolved_fields_start;
        const fields_start = self.tuple_fields.items.len;

        // Copy only this tuple's resolved fields to the tuple_fields store
        const field_indices = self.work.resolved_tuple_fields.items(.field_index);
        const field_idxs = self.work.resolved_tuple_fields.items(.field_idx);

        // First, collect the fields into a temporary array so we can sort them
        var temp_fields = std.ArrayList(TupleField).empty;
        defer temp_fields.deinit(self.allocator);

        for (updated_tuple.resolved_fields_start..resolved_fields_end) |i| {
            try temp_fields.append(self.allocator, .{
                .index = field_indices[i],
                .layout = field_idxs[i],
            });
        }

        // Sort fields by alignment (descending) first, then by index (ascending)
        const AlignmentSortCtx = struct {
            store: *Self,
            target_usize: target.TargetUsize,
            pub fn lessThan(ctx: @This(), lhs: TupleField, rhs: TupleField) bool {
                const lhs_layout = ctx.store.getLayout(lhs.layout);
                const rhs_layout = ctx.store.getLayout(rhs.layout);

                const lhs_alignment = lhs_layout.alignment(ctx.target_usize);
                const rhs_alignment = rhs_layout.alignment(ctx.target_usize);

                // First sort by alignment (descending - higher alignment first)
                if (lhs_alignment.toByteUnits() != rhs_alignment.toByteUnits()) {
                    return lhs_alignment.toByteUnits() > rhs_alignment.toByteUnits();
                }

                // Then sort by index (ascending)
                return lhs.index < rhs.index;
            }
        };

        std.mem.sort(
            TupleField,
            temp_fields.items,
            AlignmentSortCtx{ .store = self, .target_usize = self.targetUsize() },
            AlignmentSortCtx.lessThan,
        );

        // Now add them to the tuple_fields store in the sorted order
        for (temp_fields.items) |sorted_field| {
            _ = try self.tuple_fields.append(self.allocator, sorted_field);
        }

        // Calculate max alignment and total size of all fields
        var max_alignment: usize = 1;
        var current_offset: u32 = 0;
        var field_idx: u32 = 0;

        while (field_idx < temp_fields.items.len) : (field_idx += 1) {
            const temp_field = temp_fields.items[field_idx];
            const field_layout = self.getLayout(temp_field.layout);
            const field_size_align = self.layoutSizeAlign(field_layout);

            // Update max alignment
            max_alignment = @max(max_alignment, field_size_align.alignment.toByteUnits());

            // Align current offset to field's alignment
            current_offset = @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(field_size_align.alignment.toByteUnits()))));

            // Add field size
            current_offset = current_offset + field_size_align.size;
        }

        // Final size must be aligned to the tuple's alignment
        const total_size = @as(u32, @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(max_alignment)))));

        // Create the tuple layout with the fields range
        const fields_range = collections.NonEmptyRange{ .start = @intCast(fields_start), .count = @intCast(num_resolved_fields) };

        // Store the tuple data
        const tuple_idx = TupleIdx{ .int_idx = @intCast(self.tuple_data.len()) };
        _ = try self.tuple_data.append(self.allocator, TupleData{
            .size = total_size,
            .fields = fields_range,
        });

        // Remove only this tuple's resolved fields
        self.work.resolved_tuple_fields.shrinkRetainingCapacity(updated_tuple.resolved_fields_start);

        return Layout.tuple(std.mem.Alignment.fromByteUnits(max_alignment), tuple_idx);
    }

    /// Finalizes a tag union layout after all variant payload layouts have been computed.
    ///
    /// This is called once all variants in `pending_tag_union_variants` have been processed
    /// and their layouts stored in `resolved_tag_union_variants`. It:
    /// 1. Collects all resolved variant layouts
    /// 2. Calculates the max payload size and alignment across all variants
    /// 3. Computes the discriminant offset (where the tag ID is stored in memory)
    /// 4. Stores the final TagUnionData with size, discriminant info, and variant layouts
    /// 5. Returns the complete tag union layout
    fn finishTagUnion(
        self: *Self,
        pending: work.Work.PendingTagUnion,
    ) std.mem.Allocator.Error!Layout {
        const resolved_end = self.work.resolved_tag_union_variants.len;

        // Collect resolved variants and sort by index
        var variant_layouts = try self.allocator.alloc(Idx, pending.num_variants);
        defer self.allocator.free(variant_layouts);

        // Initialize all to ZST (for variants that were never processed because they have no payload)
        const zst_idx = try self.ensureZstLayout();
        for (variant_layouts) |*slot| {
            slot.* = zst_idx;
        }

        // Fill in resolved variants
        const indices = self.work.resolved_tag_union_variants.items(.index);
        const layout_idxs = self.work.resolved_tag_union_variants.items(.layout_idx);
        for (pending.resolved_variants_start..resolved_end) |i| {
            variant_layouts[indices[i]] = layout_idxs[i];
        }

        // Calculate max payload size and alignment
        var max_payload_size: u32 = 0;
        var max_payload_alignment: std.mem.Alignment = std.mem.Alignment.@"1";

        // Record variants_start BEFORE appending (this was the issue before - recursive calls would interleave)
        const variants_start: u32 = @intCast(self.tag_union_variants.len());

        for (variant_layouts) |variant_layout_idx| {
            const variant_layout = self.getLayout(variant_layout_idx);
            const variant_size = self.layoutSize(variant_layout);
            const variant_alignment = variant_layout.alignment(self.targetUsize());
            if (variant_size > max_payload_size) {
                max_payload_size = variant_size;
            }
            max_payload_alignment = max_payload_alignment.max(variant_alignment);

            // Store variant layout for runtime refcounting
            _ = try self.tag_union_variants.append(self.allocator, .{
                .payload_layout = variant_layout_idx,
            });
        }

        // Calculate discriminant info from the stored discriminant layout
        const discriminant_layout = self.getLayout(pending.discriminant_layout);
        const discriminant_size: u8 = @intCast(self.layoutSize(discriminant_layout));
        const discriminant_alignment = TagUnionData.alignmentForDiscriminantSize(discriminant_size);

        // Calculate total size: payload at offset 0, discriminant at aligned offset after payload
        const payload_end = max_payload_size;
        const discriminant_offset: u16 = @intCast(std.mem.alignForward(u32, payload_end, @intCast(discriminant_alignment.toByteUnits())));
        const total_size_unaligned = discriminant_offset + discriminant_size;

        // Align total size to the tag union's alignment
        const tag_union_alignment = max_payload_alignment.max(discriminant_alignment);
        const total_size = std.mem.alignForward(u32, total_size_unaligned, @intCast(tag_union_alignment.toByteUnits()));

        // Store TagUnionData
        const tag_union_data_idx: u32 = @intCast(self.tag_union_data.len());
        _ = try self.tag_union_data.append(self.allocator, .{
            .size = total_size,
            .discriminant_offset = discriminant_offset,
            .discriminant_size = discriminant_size,
            .variants = .{
                .start = variants_start,
                .count = @intCast(pending.num_variants),
            },
        });

        // Clear resolved variants for this tag union
        self.work.resolved_tag_union_variants.shrinkRetainingCapacity(pending.resolved_variants_start);

        return Layout.tagUnion(tag_union_alignment, .{ .int_idx = @intCast(tag_union_data_idx) });
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
                                // Create a new placeholder for the raw layout.
                                // Use opaque_ptr as a temporary that can be updated later.
                                const raw_placeholder = try self.insertLayout(Layout.opaquePtr());
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
                        // Valid recursive reference - heap allocation breaks the infinite size
                        layout_idx = try self.insertLayout(Layout.opaquePtr());
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
                                        // Create a new placeholder for the raw layout.
                                        // Use opaque_ptr as a temporary that can be updated later.
                                        const raw_placeholder = try self.insertLayout(Layout.opaquePtr());
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
                    try self.work.in_progress_vars.put(.{ .module_idx = self.current_module_idx, .var_ = current.var_ }, {});
                }

                layout = switch (current.desc.content) {
                    .structure => |flat_type| flat_type: switch (flat_type) {
                        .nominal_type => |nominal_type| {
                            // Special-case Builtin.Str: it has a tag union backing type, but
                            // should have RocStr layout (3 pointers).
                            // Check if this nominal type's identifier matches Builtin.Str
                            const is_builtin_str = blk: {
                                if (self.builtin_str_ident) |builtin_str| {
                                    if (nominal_type.ident.ident_idx == builtin_str) break :blk true;
                                }
                                if (nominal_type.origin_module == self.currentEnv().idents.builtin_module) {
                                    if (self.builtin_str_plain_ident) |plain_str| {
                                        if (nominal_type.ident.ident_idx == plain_str) break :blk true;
                                    }
                                }
                                break :blk false;
                            };
                            if (is_builtin_str) {
                                // This is Builtin.Str - use string layout
                                break :flat_type Layout.str();
                            }

                            // Special-case Builtin.Bool: it has a tag union backing type [False, True],
                            // but should have u8 layout.
                            const is_builtin_bool = blk: {
                                if (self.bool_ident) |bool_id| {
                                    if (nominal_type.ident.ident_idx == bool_id) break :blk true;
                                }
                                if (nominal_type.origin_module == self.currentEnv().idents.builtin_module) {
                                    if (self.bool_plain_ident) |plain_bool| {
                                        if (nominal_type.ident.ident_idx == plain_bool) break :blk true;
                                    }
                                }
                                break :blk false;
                            };
                            if (is_builtin_bool) {
                                // This is Builtin.Bool - use bool layout (u8)
                                break :flat_type Layout.boolType();
                            }

                            // Special handling for Builtin.Box
                            const is_builtin_box = if (self.box_ident) |box_ident|
                                nominal_type.origin_module == self.currentEnv().idents.builtin_module and
                                    nominal_type.ident.ident_idx == box_ident
                            else
                                false;
                            if (is_builtin_box) {
                                // Extract the element type from the type arguments
                                const type_args = self.getTypesStore().sliceNominalArgs(nominal_type);
                                std.debug.assert(type_args.len == 1); // Box must have exactly 1 type parameter
                                const elem_var = type_args[0];

                                // Check if the element type is a known ZST (but NOT flex/rigid - those need opaque_ptr)
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
                                    // Otherwise, add this to the stack of pending work
                                    // (This includes flex/rigid which will resolve to opaque_ptr)
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
                                nominal_type.origin_module == self.currentEnv().idents.builtin_module and
                                    nominal_type.ident.ident_idx == list_ident
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
                                        // No mapping found for this rigid type parameter.
                                        // Try to find ANY rigid mapping as a heuristic - in a monomorphized
                                        // function, all unmapped rigids should map to the same concrete type.
                                        if (caller_module_idx) |caller_mod| {
                                            if (type_scope.scopes.items.len > 0) {
                                                var iter = type_scope.scopes.items[0].iterator();
                                                while (iter.next()) |entry| {
                                                    // Check if this mapping is from a rigid (not a specific structure)
                                                    const ext_env = self.all_module_envs[self.current_module_idx];
                                                    const key_resolved = ext_env.types.resolveVar(entry.key_ptr.*);
                                                    if (key_resolved.desc.content == .rigid) {
                                                        // Found a rigid mapping - use it
                                                        const caller_env = self.all_module_envs[caller_mod];
                                                        const mapped_resolved = caller_env.types.resolveVar(entry.value_ptr.*);
                                                        break :blk switch (mapped_resolved.desc.content) {
                                                            .structure => |ft| switch (ft) {
                                                                .empty_record, .empty_tag_union => true,
                                                                else => false,
                                                            },
                                                            .flex, .rigid => false,
                                                            else => false,
                                                        };
                                                    }
                                                }
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
                            if (nominal_type.origin_module == self.currentEnv().idents.builtin_module) {
                                const ident_idx = nominal_type.ident.ident_idx;
                                const num_layout: ?Layout = blk: {
                                    if (self.u8_ident) |u8_id| if (ident_idx == u8_id) break :blk Layout.int(types.Int.Precision.u8);
                                    if (self.i8_ident) |i8_id| if (ident_idx == i8_id) break :blk Layout.int(types.Int.Precision.i8);
                                    if (self.u16_ident) |u16_id| if (ident_idx == u16_id) break :blk Layout.int(types.Int.Precision.u16);
                                    if (self.i16_ident) |i16_id| if (ident_idx == i16_id) break :blk Layout.int(types.Int.Precision.i16);
                                    if (self.u32_ident) |u32_id| if (ident_idx == u32_id) break :blk Layout.int(types.Int.Precision.u32);
                                    if (self.i32_ident) |i32_id| if (ident_idx == i32_id) break :blk Layout.int(types.Int.Precision.i32);
                                    if (self.u64_ident) |u64_id| if (ident_idx == u64_id) break :blk Layout.int(types.Int.Precision.u64);
                                    if (self.i64_ident) |i64_id| if (ident_idx == i64_id) break :blk Layout.int(types.Int.Precision.i64);
                                    if (self.u128_ident) |u128_id| if (ident_idx == u128_id) break :blk Layout.int(types.Int.Precision.u128);
                                    if (self.i128_ident) |i128_id| if (ident_idx == i128_id) break :blk Layout.int(types.Int.Precision.i128);
                                    if (self.f32_ident) |f32_id| if (ident_idx == f32_id) break :blk Layout.frac(types.Frac.Precision.f32);
                                    if (self.f64_ident) |f64_id| if (ident_idx == f64_id) break :blk Layout.frac(types.Frac.Precision.f64);
                                    if (self.dec_ident) |dec_id| if (ident_idx == dec_id) break :blk Layout.frac(types.Frac.Precision.dec);
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
                            // We use Box(opaque_ptr) as placeholder because:
                            // 1. It's non-scalar, so it gets inserted (not a sentinel)
                            // 2. It's non-ZST, so isZeroSized() returns false
                            // 3. It can be updated with updateLayout() once the real layout is known
                            const reserved_idx = try self.insertLayout(Layout.box(.opaque_ptr));
                            const reserved_cache_key = ModuleVarKey{ .module_idx = self.current_module_idx, .var_ = current.var_ };
                            try self.layouts_by_module_var.put(reserved_cache_key, reserved_idx);

                            // Mark this nominal type as in-progress.
                            // Store the nominal var, backing var, and type args range.
                            // Type args are needed to distinguish different instantiations.
                            // We store the range (indices) rather than a slice to avoid
                            // dangling pointers if the vars storage is reallocated.
                            const type_args_range = types.Store.getNominalArgsRange(nominal_type);
                            try self.work.in_progress_nominals.put(nominal_key, .{
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
                            var num_fields: usize = 0;

                            if (fields.len() > 0) {
                                num_fields = fields.len();
                                const unbound_field_slice = self.getTypesStore().getRecordFieldsSlice(fields);
                                for (unbound_field_slice.items(.name), unbound_field_slice.items(.var_)) |name, var_| {
                                    try self.work.pending_record_fields.append(self.allocator, .{ .name = name, .var_ = var_ });
                                }
                            }

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
                                layout_idx = try self.fromTypeVar(target_module, mapped_var, type_scope, target_module);
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

                        // For flex vars inside containers (list, box), use opaque_ptr
                        // since the container needs a concrete element layout.
                        if (self.work.pending_containers.len > 0) {
                            const pending_item = self.work.pending_containers.get(self.work.pending_containers.len - 1);
                            if (pending_item.container == .box or pending_item.container == .list) {
                                if (!flex.constraints.isEmpty()) {
                                    break :blk Layout.default_num();
                                }
                                break :blk Layout.opaquePtr();
                            }
                        }

                        // Unconstrained flex vars (like the element type of an empty list)
                        // have no concrete type, so they're zero-sized.
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
                                layout_idx = try self.fromTypeVar(target_module, mapped_var, type_scope, target_module);
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

                        // For rigid vars inside containers (list, box), we need to determine
                        // the element layout. If the rigid var has any constraints (like is_eq),
                        // it's likely a numeric type that should default to Dec rather than
                        // opaquePtr. opaquePtr causes crashes when elements are used in
                        // numeric comparisons. (fixes issue #8946)
                        if (self.work.pending_containers.len > 0) {
                            const pending_item = self.work.pending_containers.get(self.work.pending_containers.len - 1);
                            if (pending_item.container == .box or pending_item.container == .list) {
                                // If the rigid var has any constraints, assume it's numeric and default to Dec.
                                if (!rigid.constraints.isEmpty()) {
                                    break :blk Layout.default_num();
                                }
                                break :blk Layout.opaquePtr();
                            }
                        }
                        // Unconstrained rigid vars (like from empty list element types) can be ZST.
                        // This is safe because the code using them either runs with concrete
                        // types or doesn't run at all (like for empty list iterations).
                        if (rigid.constraints.isEmpty()) {
                            break :blk Layout.zst();
                        }

                        // Rigid vars with constraints must be resolvable.
                        unreachable;
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

                // We actually resolved a layout that wasn't zero-sized!
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
                var nominals_to_remove = std.ArrayList(work.NominalKey){};
                defer nominals_to_remove.deinit(self.allocator);

                var nominal_iter = self.work.in_progress_nominals.iterator();
                while (nominal_iter.next()) |entry| {
                    const progress = entry.value_ptr.*;
                    // Check if this nominal's backing type just finished.
                    // The backing_var should match the var we just cached.
                    if (progress.backing_var == current.var_) {
                        // Skip container types - they should be handled in the container finish path.
                        // This prevents incorrect matching when a recursion_var resolves to the same
                        // var as the backing type, but we haven't actually finished processing the container.
                        if (current.desc.content == .structure) {
                            const flat_type = current.desc.content.structure;
                            if (flat_type == .tag_union or flat_type == .record or flat_type == .tuple) {
                                // Container type - will be handled in container path below
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
                            .field_name = pending_field.name,
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
                    var nominals_to_remove_container = std.ArrayList(work.NominalKey){};
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

        // For scalar types, return the appropriate sentinel value instead of inserting
        if (layout.tag == .scalar) {
            const result = idxFromScalar(layout.data.scalar);
            return result;
        }

        // For non-scalar types, insert as normal
        const safe_list_idx = try self.layouts.append(self.allocator, layout);
        const result: Idx = @enumFromInt(@intFromEnum(safe_list_idx));
        return result;
    }

    /// Update an existing layout at the given index.
    /// Used for recursive types where we reserve a slot first and fill it in later.
    pub fn updateLayout(self: *Self, idx: Idx, layout: Layout) void {
        const ptr = self.layouts.get(@enumFromInt(@intFromEnum(idx)));
        ptr.* = layout;
    }

    /// Compute a List layout from a list expression.
    /// This handles cases where the list expression's type var is a flex (due to unresolved
    /// numerics) but we need to compute a proper List layout based on the expression structure.
    pub fn computeListLayout(
        self: *Self,
        module_idx: u32,
        module_env: *ModuleEnv,
        list_elem_span: can.CIR.Expr.Span,
        type_scope: *const TypeScope,
        caller_module_idx: ?u32,
    ) !Idx {
        const elems = module_env.store.exprSlice(list_elem_span);

        if (elems.len == 0) {
            // Empty list - use list of ZST
            return self.insertLayout(Layout.listOfZst());
        }

        // Get the first element's type var and compute its layout
        const first_elem_idx = elems[0];
        const first_elem = module_env.store.getExpr(first_elem_idx);

        // Check if the first element is also a list (for nested list handling)
        if (first_elem == .e_list) {
            // Recursively compute the nested list's layout
            const nested_list_layout_idx = try self.computeListLayout(
                module_idx,
                module_env,
                first_elem.e_list.elems,
                type_scope,
                caller_module_idx,
            );
            // Return List(nested_list_layout)
            const list_layout = Layout.list(nested_list_layout_idx);
            return self.insertLayout(list_layout);
        }

        // For non-list elements, try to compute layout from the type var
        const elem_type_var = ModuleEnv.varFrom(first_elem_idx);
        const elem_layout_idx = try self.fromTypeVar(module_idx, elem_type_var, type_scope, caller_module_idx);

        // Return List(element_layout)
        const list_layout = Layout.list(elem_layout_idx);
        return self.insertLayout(list_layout);
    }
};
