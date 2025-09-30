//! Stores Layout values by index.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const types = @import("types");
const collections = @import("collections");
const can = @import("can");
const builtins = @import("builtins");

const layout_mod = @import("layout.zig");
const work = @import("./work.zig");

const ModuleEnv = can.ModuleEnv;
const types_store = types.store;
const target = base.target;
const Ident = base.Ident;
const Region = base.Region;
const Var = types.Var;
const TypeScope = types.TypeScope;
const Layout = layout_mod.Layout;
const Idx = layout_mod.Idx;
const RecordField = layout_mod.RecordField;
const Scalar = layout_mod.Scalar;
const ScalarTag = layout_mod.ScalarTag;
const RecordData = layout_mod.RecordData;
const RecordIdx = layout_mod.RecordIdx;
const TupleField = layout_mod.TupleField;
const TupleData = layout_mod.TupleData;
const TupleIdx = layout_mod.TupleIdx;
const SizeAlign = layout_mod.SizeAlign;
const Work = work.Work;

/// Errors that can occur during layout computation
pub const LayoutError = error{
    ZeroSizedType,
    TypeContainedMismatch,
    InvalidRecordExtension,
    // Compiler bugs. Hopefully these never come up, but if they do, the caller should gracefully recover.
    BugUnboxedFlexVar,
    BugUnboxedRigidVar,
};

/// Stores Layout instances by Idx.
pub const Store = struct {
    const Self = @This();

    env: *ModuleEnv,
    types_store: *const types_store.Store,
    layouts: collections.SafeList(Layout),
    tuple_elems: collections.SafeList(Idx),
    record_fields: RecordField.SafeMultiList,
    record_data: collections.SafeList(RecordData),
    tuple_fields: TupleField.SafeMultiList,
    tuple_data: collections.SafeList(TupleData),

    // Cache to avoid duplicate work
    layouts_by_var: collections.ArrayListMap(Var, Idx),

    // Reusable work stack for addTypeVar (so it can be stack-safe instead of recursing)
    work: work.Work,

    // Number of primitive types that are pre-populated in the layout store
    // Must be kept in sync with the sentinel values in layout.zig Idx enum
    const num_scalars = 16;

    /// Get the sentinel Idx for a given scalar type using pure arithmetic - no branches!
    /// This relies on the careful ordering of ScalarTag and Idx enum values.
    pub fn idxFromScalar(scalar: Scalar) Idx {
        // Map scalar to idx using pure arithmetic:
        // opaque_ptr (tag 0) -> 2
        // bool (tag 1) -> 0
        // str (tag 2) -> 1
        // int (tag 3) with precision p -> 3 + p
        // frac (tag 4) with precision p -> 13 + (p - 2) = 11 + p

        const tag = @intFromEnum(scalar.tag);

        // Get the precision bits directly from the packed representation
        // This works because in a packed union, all fields start at bit 0
        const scalar_bits = @as(u7, @bitCast(scalar));
        const precision = scalar_bits & 0xF; // Lower 4 bits contain precision for numeric types

        // Create masks for different tag ranges
        // is_numeric: 1 when tag >= 3, else 0
        const is_numeric = @as(u7, @intFromBool(tag >= 3));

        // Calculate the base index based on tag mappings
        const base_idx = switch (scalar.tag) {
            .opaque_ptr => @as(u7, 2),
            .bool => @as(u7, 0),
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
        env: *ModuleEnv,
        type_store: *const types_store.Store,
    ) std.mem.Allocator.Error!Self {
        // Get the number of variables from the type store's slots
        const capacity = type_store.slots.backing.len();
        const layouts_by_var = try collections.ArrayListMap(Var, Idx).init(env.gpa, @intCast(capacity));

        var layouts = collections.SafeList(Layout){};

        // Pre-populate primitive type layouts in order matching the Idx enum.
        // Changing the order of these can break things!
        _ = try layouts.append(env.gpa, Layout.boolType());
        _ = try layouts.append(env.gpa, Layout.str());
        _ = try layouts.append(env.gpa, Layout.opaquePtr());
        _ = try layouts.append(env.gpa, Layout.int(.u8));
        _ = try layouts.append(env.gpa, Layout.int(.i8));
        _ = try layouts.append(env.gpa, Layout.int(.u16));
        _ = try layouts.append(env.gpa, Layout.int(.i16));
        _ = try layouts.append(env.gpa, Layout.int(.u32));
        _ = try layouts.append(env.gpa, Layout.int(.i32));
        _ = try layouts.append(env.gpa, Layout.int(.u64));
        _ = try layouts.append(env.gpa, Layout.int(.i64));
        _ = try layouts.append(env.gpa, Layout.int(.u128));
        _ = try layouts.append(env.gpa, Layout.int(.i128));
        _ = try layouts.append(env.gpa, Layout.frac(.f32));
        _ = try layouts.append(env.gpa, Layout.frac(.f64));
        _ = try layouts.append(env.gpa, Layout.frac(.dec));

        std.debug.assert(layouts.len() == num_scalars);

        return .{
            .env = env,
            .types_store = type_store,
            .layouts = layouts,
            .tuple_elems = try collections.SafeList(Idx).initCapacity(env.gpa, 512),
            .record_fields = try RecordField.SafeMultiList.initCapacity(env.gpa, 256),
            .record_data = try collections.SafeList(RecordData).initCapacity(env.gpa, 256),
            .tuple_fields = try TupleField.SafeMultiList.initCapacity(env.gpa, 256),
            .tuple_data = try collections.SafeList(TupleData).initCapacity(env.gpa, 256),
            .layouts_by_var = layouts_by_var,
            .work = try Work.initCapacity(env.gpa, 32),
        };
    }

    pub fn deinit(self: *Self) void {
        self.layouts.deinit(self.env.gpa);
        self.tuple_elems.deinit(self.env.gpa);
        self.record_fields.deinit(self.env.gpa);
        self.record_data.deinit(self.env.gpa);
        self.tuple_fields.deinit(self.env.gpa);
        self.tuple_data.deinit(self.env.gpa);
        self.layouts_by_var.deinit(self.env.gpa);
        self.work.deinit(self.env.gpa);
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
        field_layouts: []const Layout,
        field_names: []const Ident.Idx,
    ) std.mem.Allocator.Error!Idx {
        var temp_fields = std.array_list.Managed(RecordField).init(self.env.gpa);
        defer temp_fields.deinit();

        for (field_layouts, field_names) |field_layout, field_name| {
            const field_layout_idx = try self.insertLayout(field_layout);
            try temp_fields.append(.{
                .name = field_name,
                .layout = field_layout_idx,
            });
        }

        // Sort fields
        const AlignmentSortCtx = struct {
            store: *Self,
            env: *ModuleEnv,
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

        std.mem.sort(
            RecordField,
            temp_fields.items,
            AlignmentSortCtx{ .store = self, .env = self.env, .target_usize = self.targetUsize() },
            AlignmentSortCtx.lessThan,
        );

        const fields_start = self.record_fields.items.len;
        for (temp_fields.items) |sorted_field| {
            _ = try self.record_fields.append(self.env.gpa, sorted_field);
        }

        var max_alignment = std.mem.Alignment.@"1";
        var current_offset: u32 = 0;
        for (temp_fields.items) |field| {
            const field_layout = self.getLayout(field.layout);
            const field_alignment = field_layout.alignment(self.targetUsize());
            const field_size = self.layoutSize(field_layout);
            max_alignment = max_alignment.max(field_alignment);
            current_offset = @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(field_alignment.toByteUnits()))));
            current_offset += field_size;
        }

        const total_size = @as(u32, @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(max_alignment.toByteUnits())))));
        const fields_range = collections.NonEmptyRange{ .start = @intCast(fields_start), .count = @intCast(temp_fields.items.len) };
        const record_idx = RecordIdx{ .int_idx = @intCast(self.record_data.len()) };
        _ = try self.record_data.append(self.env.gpa, .{
            .size = total_size,
            .fields = fields_range,
        });

        const record_layout = Layout.record(max_alignment, record_idx);
        return try self.insertLayout(record_layout);
    }

    /// Insert a tuple layout with the given alignment and tuple metadata
    pub fn insertTuple(self: *Self, alignment: std.mem.Alignment, tuple_idx: TupleIdx) std.mem.Allocator.Error!Idx {
        const layout = Layout.tuple(alignment, tuple_idx);
        return try self.insertLayout(layout);
    }

    /// Insert a tuple layout from concrete element layouts
    pub fn putTuple(self: *Self, element_layouts: []const Layout) std.mem.Allocator.Error!Idx {
        // Collect fields
        var temp_fields = std.ArrayList(TupleField).init(self.env.gpa);
        defer temp_fields.deinit();

        for (element_layouts, 0..) |elem_layout, i| {
            const elem_idx = try self.insertLayout(elem_layout);
            try temp_fields.append(.{ .index = @intCast(i), .layout = elem_idx });
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
            _ = try self.tuple_fields.append(self.env.gpa, sorted_field);
        }

        // Compute size and alignment
        var max_alignment = std.mem.Alignment.@"1";
        var current_offset: u32 = 0;
        for (temp_fields.items) |tf| {
            const field_layout = self.getLayout(tf.layout);
            const field_alignment = field_layout.alignment(self.targetUsize());
            const field_size = self.layoutSize(field_layout);
            max_alignment = max_alignment.max(field_alignment);
            current_offset = @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(field_alignment.toByteUnits()))));
            current_offset += field_size;
        }

        const total_size = @as(u32, @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(max_alignment.toByteUnits())))));
        const fields_range = collections.NonEmptyRange{ .start = @intCast(fields_start), .count = @intCast(temp_fields.items.len) };
        const tuple_idx = TupleIdx{ .int_idx = @intCast(self.tuple_data.len()) };
        _ = try self.tuple_data.append(self.env.gpa, TupleData{ .size = total_size, .fields = fields_range });
        const tuple_layout = Layout.tuple(max_alignment, tuple_idx);
        return try self.insertLayout(tuple_layout);
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

    pub fn getRecordFieldOffset(self: *const Self, record_idx: RecordIdx, field_index_in_sorted_fields: u32) u32 {
        const target_usize = self.targetUsize();
        const record_data = self.getRecordData(record_idx);
        const sorted_fields = self.record_fields.sliceRange(record_data.getFields());

        var current_offset: u32 = 0;
        var field_idx: u32 = 0;

        while (field_idx < field_index_in_sorted_fields) : (field_idx += 1) {
            const field = sorted_fields.get(field_idx);
            const field_layout = self.getLayout(field.layout);
            const field_alignment = field_layout.alignment(target_usize);
            const field_size = self.layoutSize(field_layout);

            // Align current offset to field's alignment
            current_offset = @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(field_alignment.toByteUnits()))));

            // Add field size
            current_offset += field_size;
        }

        // Now, align the offset for the requested field
        const requested_field = sorted_fields.get(field_index_in_sorted_fields);
        const requested_field_layout = self.getLayout(requested_field.layout);
        const requested_field_alignment = requested_field_layout.alignment(target_usize);
        return @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(requested_field_alignment.toByteUnits()))));
    }

    pub fn getRecordFieldOffsetByName(self: *const Self, record_idx: RecordIdx, field_name: []const u8) ?u32 {
        const target_usize = self.targetUsize();
        const record_data = self.getRecordData(record_idx);
        const sorted_fields = self.record_fields.sliceRange(record_data.getFields());

        var current_offset: u32 = 0;
        var i: usize = 0;
        while (i < sorted_fields.len) : (i += 1) {
            const field = sorted_fields.get(i);
            const field_layout = self.getLayout(field.layout);
            const field_alignment = field_layout.alignment(target_usize);
            const field_size = self.layoutSize(field_layout);

            current_offset = @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(field_alignment.toByteUnits()))));

            const current_field_name = self.env.getIdent(field.name);
            if (std.mem.eql(u8, current_field_name, field_name)) {
                return current_offset;
            }

            current_offset += field_size;
        }

        return null;
    }

    pub fn getTupleElementOffset(self: *const Self, tuple_idx: TupleIdx, element_index_in_sorted_elements: u32) u32 {
        const target_usize = self.targetUsize();
        const tuple_data = self.getTupleData(tuple_idx);
        const sorted_elements = self.tuple_fields.sliceRange(tuple_data.getFields());

        var current_offset: u32 = 0;
        var element_idx: u32 = 0;

        while (element_idx < element_index_in_sorted_elements) : (element_idx += 1) {
            const element = sorted_elements.get(element_idx);
            const element_layout = self.getLayout(element.layout);
            const element_alignment = element_layout.alignment(target_usize);
            const element_size = self.layoutSize(element_layout);

            // Align current offset to element's alignment
            current_offset = @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(element_alignment.toByteUnits()))));

            // Add element size
            current_offset += element_size;
        }

        // Now, align the offset for the requested element
        const requested_element = sorted_elements.get(element_index_in_sorted_elements);
        const requested_element_layout = self.getLayout(requested_element.layout);
        const requested_element_alignment = requested_element_layout.alignment(target_usize);
        return @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(requested_element_alignment.toByteUnits()))));
    }

    pub fn targetUsize(_: *const Self) target.TargetUsize {
        return target.TargetUsize.native;
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
        _ = try self.record_data.append(self.env.gpa, .{
            .size = 0,
            .fields = collections.NonEmptyRange{ .start = 0, .count = 0 },
        });
        const empty_record_layout = Layout.record(std.mem.Alignment.@"1", record_idx);
        return try self.insertLayout(empty_record_layout);
    }

    pub fn ensureEmptyRecordLayout(self: *Self) !Idx {
        return self.getEmptyRecordLayout();
    }

    /// Get the size in bytes of a layout, given the store's target usize.
    pub fn layoutSize(self: *const Self, layout: Layout) u32 {
        // TODO change this to SizeAlign (just return both since they're packed into 4B anyway)
        // and also change it to just return that one field instead of doing any conditionals.
        // also have it take an Idx. if you already have a Layout you can just get that.
        const target_usize = self.targetUsize();
        return switch (layout.tag) {
            .scalar => switch (layout.data.scalar.tag) {
                .int => layout.data.scalar.data.int.size(),
                .frac => layout.data.scalar.data.frac.size(),
                .bool => 1, // bool is 1 byte
                .str => 3 * target_usize.size(), // ptr, byte length, capacity
                .opaque_ptr => target_usize.size(), // opaque_ptr is pointer-sized
            },
            .box, .box_of_zst => target_usize.size(), // a Box is just a pointer to refcounted memory
            .list => 3 * target_usize.size(), // ptr, length, capacity
            .list_of_zst => target_usize.size(), // Zero-sized lists might be different
            .record => self.record_data.get(@enumFromInt(layout.data.record.idx.int_idx)).size,
            .tuple => self.tuple_data.get(@enumFromInt(layout.data.tuple.idx.int_idx)).size,
            .closure => {
                // Closure layout: header + aligned capture data
                const header_size = @sizeOf(layout_mod.Closure);
                const captures_layout = self.getLayout(layout.data.closure.captures_layout_idx);
                const captures_alignment = captures_layout.alignment(self.targetUsize());
                const aligned_captures_offset = std.mem.alignForward(u32, header_size, @intCast(captures_alignment.toByteUnits()));
                const captures_size = self.layoutSize(captures_layout);
                return aligned_captures_offset + captures_size;
            },
        };
    }

    /// Add the record's fields to self.pending_record_fields,
    /// then add the record's extension fields too (recursively).
    fn gatherRecordFields(
        self: *Self,
        record_type: types.Record,
    ) (LayoutError || std.mem.Allocator.Error)!usize {
        var num_fields = record_type.fields.len();

        const field_slice = self.types_store.getRecordFieldsSlice(record_type.fields);
        for (field_slice.items(.name), field_slice.items(.var_)) |name, var_| {
            // TODO is it possible that here we're encountering record fields with names
            // already in the list? Would type-checking have already deduped them?
            // We would certainly rather not spend time doing hashmap things if we can avoid it here.
            try self.work.pending_record_fields.append(self.env.gpa, .{ .name = name, .var_ = var_ });
        }

        var current_ext = record_type.ext;
        while (true) {
            const resolved_ext = self.types_store.resolveVar(current_ext);
            switch (resolved_ext.desc.content) {
                .structure => |ext_flat_type| switch (ext_flat_type) {
                    .empty_record => break,
                    .record => |ext_record| {
                        if (ext_record.fields.len() > 0) {
                            num_fields += ext_record.fields.len();
                            const ext_field_slice = self.types_store.getRecordFieldsSlice(ext_record.fields);
                            for (ext_field_slice.items(.name), ext_field_slice.items(.var_)) |name, var_| {
                                // TODO is it possible that here we're adding fields with names
                                // already in the list? Would type-checking have already collapsed these?
                                // We would certainly rather not spend time doing hashmap things
                                // if we can avoid it here.
                                try self.work.pending_record_fields.append(self.env.gpa, .{ .name = name, .var_ = var_ });
                            }
                            current_ext = ext_record.ext;
                        } else {
                            break;
                        }
                    },
                    .record_unbound => |fields| {
                        if (fields.len() > 0) {
                            num_fields += fields.len();
                            const unbound_field_slice = self.types_store.getRecordFieldsSlice(fields);
                            for (unbound_field_slice.items(.name), unbound_field_slice.items(.var_)) |name, var_| {
                                // TODO is it possible that here we're adding fields with names
                                // already in the list? Would type-checking have already collapsed these?
                                // We would certainly rather not spend time doing hashmap things
                                // if we can avoid it here.
                                try self.work.pending_record_fields.append(self.env.gpa, .{ .name = name, .var_ = var_ });
                            }
                        }
                        // record_unbound has no extension, so stop here
                        break;
                    },
                    .record_poly => |poly| {
                        if (poly.record.fields.len() > 0) {
                            num_fields += poly.record.fields.len();
                            const poly_field_slice = self.types_store.getRecordFieldsSlice(poly.record.fields);
                            for (poly_field_slice.items(.name), poly_field_slice.items(.var_)) |name, var_| {
                                // TODO is it possible that here we're adding fields with names
                                // already in the list? Would type-checking have already collapsed these?
                                // We would certainly rather not spend time doing hashmap things
                                // if we can avoid it here.
                                try self.work.pending_record_fields.append(self.env.gpa, .{ .name = name, .var_ = var_ });
                            }
                        }
                        current_ext = poly.record.ext;
                    },
                    else => return LayoutError.InvalidRecordExtension,
                },
                .alias => |alias| {
                    current_ext = self.types_store.getAliasBackingVar(alias);
                },
                else => return LayoutError.InvalidRecordExtension,
            }
        }

        return num_fields;
    }

    /// Add the tuple's fields to self.pending_tuple_fields
    fn gatherTupleFields(
        self: *Self,
        tuple_type: types.Tuple,
    ) (LayoutError || std.mem.Allocator.Error)!usize {
        const elem_slice = self.types_store.sliceVars(tuple_type.elems);
        const num_fields = elem_slice.len;

        for (elem_slice, 0..) |var_, index| {
            try self.work.pending_tuple_fields.append(self.env.gpa, .{ .index = @intCast(index), .var_ = var_ });
        }

        return num_fields;
    }

    fn finishRecord(
        self: *Store,
        updated_record: work.Work.PendingRecord,
    ) (LayoutError || std.mem.Allocator.Error)!Layout {
        const target_usize = self.targetUsize();
        const resolved_fields_end = self.work.resolved_record_fields.len;
        const num_resolved_fields = resolved_fields_end - updated_record.resolved_fields_start;
        const fields_start = self.record_fields.items.len;

        // Copy only this record's resolved fields to the record_fields store
        const field_names = self.work.resolved_record_fields.items(.field_name);
        const field_idxs = self.work.resolved_record_fields.items(.field_idx);

        // First, collect the fields into a temporary array so we can sort them
        var temp_fields = std.array_list.Managed(RecordField).init(self.env.gpa);
        defer temp_fields.deinit();

        for (updated_record.resolved_fields_start..resolved_fields_end) |i| {
            try temp_fields.append(.{
                .name = field_names[i],
                .layout = field_idxs[i],
            });
        }

        // Sort fields by alignment (descending) first, then by name (ascending)
        const AlignmentSortCtx = struct {
            store: *Self,
            env: *ModuleEnv,
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
            AlignmentSortCtx{ .store = self, .env = self.env, .target_usize = self.targetUsize() },
            AlignmentSortCtx.lessThan,
        );

        // Now add them to the record_fields store in the sorted order
        for (temp_fields.items) |sorted_field| {
            _ = try self.record_fields.append(self.env.gpa, sorted_field);
        }

        // Calculate max alignment and total size of all fields
        var max_alignment = std.mem.Alignment.@"1";
        var current_offset: u32 = 0;
        var field_idx: u32 = 0;

        while (field_idx < temp_fields.items.len) : (field_idx += 1) {
            const temp_field = temp_fields.items[field_idx];
            const field_layout = self.getLayout(temp_field.layout);

            const field_alignment = field_layout.alignment(target_usize);
            const field_size = self.layoutSize(field_layout);

            // Update max alignment
            max_alignment = max_alignment.max(field_alignment);

            // Align current offset to field's alignment
            current_offset = @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(field_alignment.toByteUnits()))));

            // Add field size
            current_offset = current_offset + field_size;
        }

        // Final size must be aligned to the record's alignment
        const total_size = @as(u32, @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(max_alignment.toByteUnits())))));

        // Create the record layout with the fields range
        const fields_range = collections.NonEmptyRange{ .start = @intCast(fields_start), .count = @intCast(num_resolved_fields) };

        // Store the record data
        const record_idx = RecordIdx{ .int_idx = @intCast(self.record_data.len()) };
        _ = try self.record_data.append(self.env.gpa, RecordData{
            .size = total_size,
            .fields = fields_range,
        });

        // Remove only this record's resolved fields
        self.work.resolved_record_fields.shrinkRetainingCapacity(updated_record.resolved_fields_start);

        return Layout.record(max_alignment, record_idx);
    }

    fn finishTuple(
        self: *Store,
        updated_tuple: work.Work.PendingTuple,
    ) (LayoutError || std.mem.Allocator.Error)!Layout {
        const target_usize = self.targetUsize();
        const resolved_fields_end = self.work.resolved_tuple_fields.len;
        const num_resolved_fields = resolved_fields_end - updated_tuple.resolved_fields_start;
        const fields_start = self.tuple_fields.items.len;

        // Copy only this tuple's resolved fields to the tuple_fields store
        const field_indices = self.work.resolved_tuple_fields.items(.field_index);
        const field_idxs = self.work.resolved_tuple_fields.items(.field_idx);

        // First, collect the fields into a temporary array so we can sort them
        var temp_fields = std.array_list.Managed(TupleField).init(self.env.gpa);
        defer temp_fields.deinit();

        for (updated_tuple.resolved_fields_start..resolved_fields_end) |i| {
            try temp_fields.append(.{
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
            _ = try self.tuple_fields.append(self.env.gpa, sorted_field);
        }

        // Calculate max alignment and total size of all fields
        var max_alignment = std.mem.Alignment.@"1";
        var current_offset: u32 = 0;
        var field_idx: u32 = 0;

        while (field_idx < temp_fields.items.len) : (field_idx += 1) {
            const temp_field = temp_fields.items[field_idx];
            const field_layout = self.getLayout(temp_field.layout);

            const field_alignment = field_layout.alignment(target_usize);
            const field_size = self.layoutSize(field_layout);

            // Update max alignment
            max_alignment = max_alignment.max(field_alignment);

            // Align current offset to field's alignment
            current_offset = @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(field_alignment.toByteUnits()))));

            // Add field size
            current_offset = current_offset + field_size;
        }

        // Final size must be aligned to the tuple's alignment
        const total_size = @as(u32, @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(max_alignment.toByteUnits())))));

        // Create the tuple layout with the fields range
        const fields_range = collections.NonEmptyRange{ .start = @intCast(fields_start), .count = @intCast(num_resolved_fields) };

        // Store the tuple data
        const tuple_idx = TupleIdx{ .int_idx = @intCast(self.tuple_data.len()) };
        _ = try self.tuple_data.append(self.env.gpa, TupleData{
            .size = total_size,
            .fields = fields_range,
        });

        // Remove only this tuple's resolved fields
        self.work.resolved_tuple_fields.shrinkRetainingCapacity(updated_tuple.resolved_fields_start);

        return Layout.tuple(max_alignment, tuple_idx);
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
    pub fn addTypeVar(
        self: *Self,
        unresolved_var: Var,
        type_scope: *const TypeScope,
    ) (LayoutError || std.mem.Allocator.Error)!Idx {
        var current = self.types_store.resolveVar(unresolved_var);

        // If we've already seen this var, return the layout we resolved it to.
        if (self.layouts_by_var.get(current.var_)) |cached_idx| {
            return cached_idx;
        }

        // To make this function stack-safe, we use a manual stack instead of recursing.
        // We reuse that stack from call to call to avoid reallocating it.
        self.work.clearRetainingCapacity();

        var iterations: u32 = 0;
        const max_iterations = 10000; // Safety limit to prevent infinite loops
        var layout_idx: Idx = undefined;

        outer: while (true) {
            iterations += 1;
            if (iterations > max_iterations) {
                @panic("Layout computation exceeded iteration limit - possible infinite loop");
            }

            var layout = switch (current.desc.content) {
                .structure => |flat_type| flat_type: switch (flat_type) {
                    .str => Layout.str(),
                    .box => |elem_var| {
                        try self.work.pending_containers.append(self.env.gpa, .{
                            .var_ = current.var_,
                            .container = .box,
                        });
                        // Push a pending Box container and "recurse" on the elem type
                        current = self.types_store.resolveVar(elem_var);
                        continue;
                    },
                    .list => |elem_var| {
                        try self.work.pending_containers.append(self.env.gpa, .{
                            .var_ = current.var_,
                            .container = .list,
                        });
                        // Push a pending List container and "recurse" on the elem type
                        current = self.types_store.resolveVar(elem_var);
                        continue;
                    },
                    .list_unbound => {
                        // For unbound lists (empty lists), use list of zero-sized type
                        const layout = Layout.listOfZst();
                        const idx = try self.insertLayout(layout);
                        try self.layouts_by_var.put(self.env.gpa, current.var_, idx);
                        return idx;
                    },
                    .nominal_type => |nominal_type| {
                        // TODO special-case the builtin Num type here.
                        // If we have one of those, then convert it to a Num layout,
                        // or to a runtime error if it's an invalid elem type.

                        // From a layout perspective, nominal types are identical to type aliases:
                        // all we care about is what's inside, so just unroll it.
                        const backing_var = self.types_store.getNominalBackingVar(nominal_type);
                        const resolved = self.types_store.resolveVar(backing_var);

                        current = resolved;
                        continue;
                    },
                    .num => |num| switch (num) {
                        .num_compact => |compact| switch (compact) {
                            .int => |precision| Layout.int(precision),
                            .frac => |precision| Layout.frac(precision),
                        },
                        .int_precision => |precision| Layout.int(precision),
                        .frac_precision => |precision| Layout.frac(precision),
                        // For polymorphic types, use default precision
                        .num_unbound => Layout.int(types.Num.Int.Precision.default),
                        .int_unbound => Layout.int(types.Num.Int.Precision.default),
                        .frac_unbound => Layout.frac(types.Num.Frac.Precision.default),
                        .num_poly => Layout.int(types.Num.Int.Precision.default),
                        .int_poly => Layout.int(types.Num.Int.Precision.default),
                        .frac_poly => Layout.frac(types.Num.Frac.Precision.default),
                    },
                    .tuple => |tuple_type| {
                        const num_fields = try self.gatherTupleFields(tuple_type);

                        if (num_fields == 0) {
                            continue :flat_type .empty_record; // Empty tuple is like empty record
                        }

                        try self.work.pending_containers.append(self.env.gpa, .{
                            .var_ = current.var_,
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
                        current = self.types_store.resolveVar(last_pending_field.var_);
                        continue :outer;
                    },
                    .fn_pure => |func| {
                        _ = func;
                        // Create empty captures layout for generic function type
                        const empty_captures_idx = try self.getEmptyRecordLayout();
                        break :flat_type Layout.closure(empty_captures_idx);
                    },
                    .fn_effectful => |func| {
                        _ = func;
                        // Create empty captures layout for generic function type
                        const empty_captures_idx = try self.getEmptyRecordLayout();
                        break :flat_type Layout.closure(empty_captures_idx);
                    },
                    .fn_unbound => |func| {
                        _ = func;
                        // Create empty captures layout for generic function type
                        const empty_captures_idx = try self.getEmptyRecordLayout();
                        break :flat_type Layout.closure(empty_captures_idx);
                    },
                    .record => |record_type| {
                        const num_fields = try self.gatherRecordFields(record_type);

                        if (num_fields == 0) {
                            continue :flat_type .empty_record;
                        }

                        try self.work.pending_containers.append(self.env.gpa, .{
                            .var_ = current.var_,
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

                        current = self.types_store.resolveVar(field.var_);
                        continue;
                    },
                    .tag_union => |tag_union| {
                        // Handle tag unions by computing the layout based on:
                        // 1. Discriminant size (based on number of tags)
                        // 2. Maximum payload size and alignment
                        const tags = self.types_store.getTagsSlice(tag_union.tags);

                        // Check if this is a Bool (2 tags with no payload) as a special case
                        // This is a legitimate layout optimization for boolean tag unions
                        if (tags.len == 2) {
                            var is_bool = true;
                            for (tags.items(.args)) |tag_args| {
                                const args_slice = self.types_store.sliceVars(tag_args);
                                if (args_slice.len != 0) {
                                    is_bool = false;
                                    break;
                                }
                            }

                            if (is_bool) {
                                // Bool layout: use predefined bool layout
                                const layout = Layout.boolType();
                                const bool_layout_idx = try self.insertLayout(layout);
                                try self.layouts_by_var.put(self.env.gpa, current.var_, bool_layout_idx);
                                return bool_layout_idx;
                            }
                        }

                        // For general tag unions, we need to compute the layout
                        // First, determine discriminant size based on number of tags
                        const discriminant_layout = if (tags.len == 0)
                            // Empty tag union - should not happen in practice
                            return LayoutError.ZeroSizedType
                        else if (tags.len <= 256)
                            Layout.int(.u8)
                        else if (tags.len <= 65536)
                            Layout.int(.u16)
                        else
                            Layout.int(.u32);

                        // If all tags have no payload, we just need the discriminant
                        var has_payload = false;
                        for (tags.items(.args)) |tag_args| {
                            const args_slice = self.types_store.sliceVars(tag_args);
                            if (args_slice.len > 0) {
                                has_payload = true;
                                break;
                            }
                        }

                        if (!has_payload) {
                            // Simple tag union with no payloads - just use discriminant
                            const tag_layout_idx = try self.insertLayout(discriminant_layout);
                            try self.layouts_by_var.put(self.env.gpa, current.var_, tag_layout_idx);
                            return tag_layout_idx;
                        }

                        // Complex tag union with payloads
                        // Strategy: represent as a record { payload: MaxPayload, tag: Discriminant }
                        // to ensure the payload receives its required alignment and the discriminant
                        // can live in any trailing padding that remains.
                        var max_payload_layout: ?Layout = null;
                        var max_payload_size: u32 = 0;
                        var max_payload_alignment: std.mem.Alignment = std.mem.Alignment.@"1";
                        var max_payload_alignment_any: std.mem.Alignment = std.mem.Alignment.@"1";

                        // Helper to update max with a candidate layout
                        const updateMax = struct {
                            fn go(
                                store: *Self,
                                candidate: Layout,
                                curr_size: *u32,
                                curr_alignment: *std.mem.Alignment,
                                out_layout: *?Layout,
                                max_alignment_any: *std.mem.Alignment,
                            ) void {
                                const size = store.layoutSize(candidate);
                                const alignment = candidate.alignment(store.targetUsize());
                                max_alignment_any.* = max_alignment_any.*.max(alignment);
                                if (size > curr_size.* or (size == curr_size.* and alignment.toByteUnits() > curr_alignment.*.toByteUnits())) {
                                    curr_size.* = size;
                                    curr_alignment.* = alignment;
                                    out_layout.* = candidate;
                                }
                            }
                        }.go;

                        // For each tag, compute its payload layout: () => ZST, 1 arg => layout, >1 => tuple of arg layouts
                        var temp_scope = TypeScope.init(self.env.gpa);
                        defer temp_scope.deinit();
                        for (tags.items(.args)) |tag_args| {
                            const args_slice = self.types_store.sliceVars(tag_args);
                            if (args_slice.len == 0) {
                                // zero-sized payload; nothing to update
                                continue;
                            } else if (args_slice.len == 1) {
                                const arg_var = args_slice[0];
                                const arg_layout_idx = try self.addTypeVar(arg_var, &temp_scope);
                                const layout_val = self.getLayout(arg_layout_idx);
                                updateMax(self, layout_val, &max_payload_size, &max_payload_alignment, &max_payload_layout, &max_payload_alignment_any);
                            } else {
                                // Build tuple layout from argument layouts
                                var elem_layouts = try self.env.gpa.alloc(Layout, args_slice.len);
                                defer self.env.gpa.free(elem_layouts);
                                for (args_slice, 0..) |v, i| {
                                    const elem_idx = try self.addTypeVar(v, &temp_scope);
                                    elem_layouts[i] = self.getLayout(elem_idx);
                                }
                                const tuple_idx = try self.putTuple(elem_layouts);
                                const tuple_layout = self.getLayout(tuple_idx);
                                updateMax(self, tuple_layout, &max_payload_size, &max_payload_alignment, &max_payload_layout, &max_payload_alignment_any);
                            }
                        }

                        const name_payload = try self.env.common.idents.insert(self.env.gpa, Ident.for_text("payload"));
                        const name_tag = try self.env.common.idents.insert(self.env.gpa, Ident.for_text("tag"));

                        const payload_layout = max_payload_layout orelse blk: {
                            const empty_idx = try self.ensureEmptyRecordLayout();
                            break :blk self.getLayout(empty_idx);
                        };

                        var field_layouts = [_]Layout{
                            payload_layout,
                            discriminant_layout,
                        };

                        var field_names = [_]Ident.Idx{ name_payload, name_tag };

                        const rec_idx = try self.putRecord(&field_layouts, &field_names);
                        if (max_payload_alignment_any.toByteUnits() > 1) {
                            const desired_alignment = max_payload_alignment_any;
                            var rec_layout = self.getLayout(rec_idx);
                            const current_alignment = rec_layout.alignment(self.targetUsize());
                            if (desired_alignment.toByteUnits() > current_alignment.toByteUnits()) {
                                std.debug.assert(rec_layout.tag == .record);
                                const record_idx = rec_layout.data.record.idx;
                                const new_layout = Layout.record(desired_alignment, record_idx);
                                self.layouts.set(@enumFromInt(@intFromEnum(rec_idx)), new_layout);
                            }
                        }
                        try self.layouts_by_var.put(self.env.gpa, current.var_, rec_idx);
                        return rec_idx;
                    },
                    .record_unbound => |fields| {
                        // For record_unbound, we need to gather fields directly since it has no Record struct
                        var num_fields: usize = 0;

                        if (fields.len() > 0) {
                            num_fields = fields.len();
                            const unbound_field_slice = self.types_store.getRecordFieldsSlice(fields);
                            for (unbound_field_slice.items(.name), unbound_field_slice.items(.var_)) |name, var_| {
                                try self.work.pending_record_fields.append(self.env.gpa, .{ .name = name, .var_ = var_ });
                            }
                        }

                        if (num_fields == 0) {
                            continue :flat_type .empty_record;
                        }

                        try self.work.pending_containers.append(self.env.gpa, .{
                            .var_ = current.var_,
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

                        current = self.types_store.resolveVar(field.var_);
                        continue;
                    },
                    .record_poly => |poly| {
                        const num_fields = try self.gatherRecordFields(poly.record);

                        if (num_fields == 0) {
                            continue :flat_type .empty_record;
                        }

                        try self.work.pending_containers.append(self.env.gpa, .{
                            .var_ = current.var_,
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

                        current = self.types_store.resolveVar(field.var_);
                        continue;
                    },
                    .empty_record, .empty_tag_union => blk: {
                        // Empty records and tag unions are zero-sized, so we need to do something different
                        // depending on the container we're working on (if any). For example, if we're
                        // working on a record field, then we need to drop that field from the container.
                        if (self.work.pending_containers.len > 0) {
                            const pending_item = self.work.pending_containers.pop() orelse unreachable;
                            switch (pending_item.container) {
                                .list => {
                                    // It turned out we were getting the layout for a List({})
                                    break :blk Layout.listOfZst();
                                },
                                .box => {
                                    // It turned out we were getting the layout for a Box({})
                                    break :blk Layout.boxOfZst();
                                },
                                .record => |pending_record| {
                                    // It turned out we were getting the layout for a record field
                                    std.debug.assert(pending_record.pending_fields > 0);
                                    var updated_record = pending_record;
                                    updated_record.pending_fields -= 1;

                                    // The current field we're working on turned out to be zero-sized, so drop it.
                                    _ = self.work.pending_record_fields.pop() orelse unreachable;

                                    if (updated_record.pending_fields == 0) {
                                        if (self.work.resolved_record_fields.len == updated_record.resolved_fields_start) {
                                            // All fields were zero-sized, so the parent container turned
                                            // out to be an empty record as well.
                                            self.work.resolved_record_fields.shrinkRetainingCapacity(updated_record.resolved_fields_start);

                                            continue :flat_type .empty_record;
                                        } else {
                                            // We finished the record we were working on.
                                            break :blk try self.finishRecord(updated_record);
                                        }
                                    } else {
                                        // Still have pending fields to process
                                        try self.work.pending_containers.append(self.env.gpa, .{
                                            .var_ = pending_item.var_,
                                            .container = .{ .record = updated_record },
                                        });

                                        // Get the next field to process
                                        const next_field = self.work.pending_record_fields.get(self.work.pending_record_fields.len - 1);
                                        current = self.types_store.resolveVar(next_field.var_);
                                        continue;
                                    }
                                },
                                .tuple => |pending_tuple| {
                                    // It turned out we were getting the layout for a tuple field
                                    std.debug.assert(pending_tuple.pending_fields > 0);
                                    var updated_tuple = pending_tuple;
                                    updated_tuple.pending_fields -= 1;

                                    // The current field we're working on turned out to be zero-sized, so drop it.
                                    _ = self.work.pending_tuple_fields.pop() orelse unreachable;

                                    if (updated_tuple.pending_fields == 0) {
                                        if (self.work.resolved_tuple_fields.len == updated_tuple.resolved_fields_start) {
                                            // All fields were zero-sized, so the parent container turned
                                            // out to be an empty tuple as well.
                                            self.work.resolved_tuple_fields.shrinkRetainingCapacity(updated_tuple.resolved_fields_start);

                                            continue :flat_type .empty_record; // Empty tuple is like empty record
                                        } else {
                                            // We finished the tuple we were working on.
                                            break :blk try self.finishTuple(updated_tuple);
                                        }
                                    } else {
                                        // Still have pending fields to process
                                        try self.work.pending_containers.append(self.env.gpa, .{
                                            .var_ = pending_item.var_,
                                            .container = .{ .tuple = updated_tuple },
                                        });

                                        // Get the next field to process
                                        const next_field = self.work.pending_tuple_fields.get(self.work.pending_tuple_fields.len - 1);
                                        current = self.types_store.resolveVar(next_field.var_);
                                        continue;
                                    }
                                },
                            }
                        }

                        // Unboxed zero-sized types cannot have a layout
                        return LayoutError.ZeroSizedType;
                    },
                },
                .flex_var => |_| blk: {
                    // First, check if this flex var is mapped in the TypeScope
                    if (type_scope.lookup(current.var_)) |mapped_var| {
                        // Found a mapping, resolve the mapped variable and continue
                        current = self.types_store.resolveVar(mapped_var);
                        continue :outer;
                    }

                    // Flex vars can only be sent to the host if boxed.
                    if (self.work.pending_containers.len > 0) {
                        const pending_item = self.work.pending_containers.get(self.work.pending_containers.len - 1);
                        if (pending_item.container == .box or pending_item.container == .list) {
                            break :blk Layout.opaquePtr();
                        }
                    }

                    // Flex vars appear in REPL/eval contexts where type constraints haven't been fully solved.
                    // This is a known issue that needs proper constraint solving before layout computation.
                    // For now, default to I64 for numeric flex vars.
                    break :blk Layout.int(.i64);
                },
                .rigid_var => |ident| blk: {
                    _ = ident;
                    // First, check if this rigid var is mapped in the TypeScope
                    if (type_scope.lookup(current.var_)) |mapped_var| {
                        // Found a mapping, resolve the mapped variable and continue
                        current = self.types_store.resolveVar(mapped_var);
                        continue :outer;
                    }

                    // Rigid vars can only be sent to the host if boxed.
                    if (self.work.pending_containers.len > 0) {
                        const pending_item = self.work.pending_containers.get(self.work.pending_containers.len - 1);
                        if (pending_item.container == .box or pending_item.container == .list) {
                            break :blk Layout.opaquePtr();
                        }
                    }

                    // Rigid vars should not appear unboxed in layout computation.
                    // This is likely a bug in the type system.
                    //
                    // Unlike flex vars, rigid vars represent type parameters that should
                    // have been instantiated. This is a bug that should be fixed.
                    return LayoutError.BugUnboxedRigidVar;
                },
                .alias => |alias| {
                    // Follow the alias by updating the work item
                    const backing_var = self.types_store.getAliasBackingVar(alias);
                    current = self.types_store.resolveVar(backing_var);
                    continue;
                },
                .err => return LayoutError.TypeContainedMismatch,
            };

            // We actually resolved a layout that wasn't zero-sized!
            // First things first: add it to the cache.
            layout_idx = try self.insertLayout(layout);
            try self.layouts_by_var.put(self.env.gpa, current.var_, layout_idx);

            // If this was part of a pending container that we're working on, update that container.
            while (self.work.pending_containers.len > 0) {
                // Get a pointer to the last pending container, so we can mutate it in-place.
                switch (self.work.pending_containers.slice().items(.container)[self.work.pending_containers.len - 1]) {
                    .box => {
                        layout = Layout.box(layout_idx);
                    },
                    .list => {
                        layout = Layout.list(layout_idx);
                    },
                    .record => |*pending_record| {
                        std.debug.assert(pending_record.pending_fields > 0);
                        pending_record.pending_fields -= 1;

                        // Pop the field we just processed
                        const pending_field = self.work.pending_record_fields.pop() orelse unreachable;

                        // Add to resolved fields
                        try self.work.resolved_record_fields.append(self.env.gpa, .{
                            .field_name = pending_field.name,
                            .field_idx = layout_idx,
                        });

                        if (pending_record.pending_fields == 0) {
                            layout = try self.finishRecord(pending_record.*);
                        } else {
                            // There are still fields remaining to process, so process the next one in the outer loop.
                            const next_field = self.work.pending_record_fields.get(self.work.pending_record_fields.len - 1);
                            current = self.types_store.resolveVar(next_field.var_);
                            continue :outer;
                        }
                    },
                    .tuple => |*pending_tuple| {
                        std.debug.assert(pending_tuple.pending_fields > 0);
                        pending_tuple.pending_fields -= 1;

                        // Pop the field we just processed
                        const pending_field = self.work.pending_tuple_fields.pop() orelse unreachable;

                        // Add to resolved fields
                        try self.work.resolved_tuple_fields.append(self.env.gpa, .{
                            .field_index = pending_field.index,
                            .field_idx = layout_idx,
                        });

                        if (pending_tuple.pending_fields == 0) {
                            layout = try self.finishTuple(pending_tuple.*);
                        } else {
                            // There are still fields remaining to process, so process the next one in the outer loop.
                            const next_field = self.work.pending_tuple_fields.get(self.work.pending_tuple_fields.len - 1);
                            current = self.types_store.resolveVar(next_field.var_);
                            continue :outer;
                        }
                    },
                }

                // We're done with this container, so remove it from pending_containers
                const pending_item = self.work.pending_containers.pop() orelse unreachable;
                layout_idx = try self.insertLayout(layout);

                // Add the container's layout to our layouts_by_var cache for later use.
                try self.layouts_by_var.put(self.env.gpa, pending_item.var_, layout_idx);
            }

            // Since there are no pending containers remaining, there shouldn't be any pending record or tuple fields either.
            std.debug.assert(self.work.pending_record_fields.len == 0);
            std.debug.assert(self.work.pending_tuple_fields.len == 0);

            // No more pending containers; we're done!
            return layout_idx;
        }
    }

    pub fn insertLayout(self: *Self, layout: Layout) std.mem.Allocator.Error!Idx {
        // For scalar types, return the appropriate sentinel value instead of inserting
        if (layout.tag == .scalar) {
            return idxFromScalar(layout.data.scalar);
        }

        // For non-scalar types, insert as normal
        const safe_list_idx = try self.layouts.append(self.env.gpa, layout);
        return @enumFromInt(@intFromEnum(safe_list_idx));
    }
};
