//! Stores Layout values by index.

const std = @import("std");
const types = @import("../types/types.zig");
const types_store = @import("../types/store.zig");
const layout = @import("./layout.zig");
const base = @import("../base.zig");
const collections = @import("../collections.zig");
const Ident = @import("../base/Ident.zig");
const work = @import("./work.zig");
const Alignment = layout.Alignment;

const Var = types.Var;
const Layout = layout.Layout;
const Idx = layout.Idx;
const Work = work.Work;
const exitOnOom = collections.utils.exitOnOom;

const MkSafeList = collections.SafeList;
const RecordField = layout.RecordField;
const RecordFieldSafeMultiList = RecordField.SafeMultiList;

/// Errors that can occur during layout computation
pub const LayoutError = error{
    ZeroSizedType,
    TypeContainedMismatch,
    RecordFieldTypeMismatch,
    InvalidRecordExtension,
    // Compiler bugs. Hopefully these never come up, but if they do, the caller should gracefully recover.
    BugUnboxedFlexVar,
    BugUnboxedRigidVar,
};

/// Storage for layout information, managing the allocation and retrieval of layouts
pub const Store = struct {
    const Self = @This();

    env: *base.ModuleEnv,

    // Layout storage
    layouts: collections.SafeList(Layout),

    // Lists for parameterized layouts
    tuple_elems: collections.SafeList(Idx),
    record_fields: RecordFieldSafeMultiList,

    // Cache to avoid duplicate work
    var_to_layout: std.AutoHashMapUnmanaged(Var, Idx),

    // Reusable work stack for addTypeVar
    work: work.Work,

    pub fn init(env: *base.ModuleEnv) Self {
        return .{
            .env = env,
            .layouts = collections.SafeList(Layout){},
            .tuple_elems = collections.SafeList(Idx).initCapacity(env.gpa, 512),
            .record_fields = RecordFieldSafeMultiList.initCapacity(env.gpa, 256),
            .var_to_layout = std.AutoHashMapUnmanaged(Var, Idx){},
            .work = Work.initCapacity(env.gpa, 32) catch |err| exitOnOom(err),
        };
    }

    pub fn deinit(self: *Self) void {
        self.layouts.deinit(self.env.gpa);
        self.tuple_elems.deinit(self.env.gpa);
        self.record_fields.deinit(self.env.gpa);
        self.var_to_layout.deinit(self.env.gpa);
        self.work.deinit(self.env.gpa);
    }

    fn insertLayout(self: *Self, layout_: Layout) std.mem.Allocator.Error!Idx {
        const safe_idx = self.layouts.append(self.env.gpa, layout_);
        return Idx{
            .idx = @intCast(@intFromEnum(safe_idx)),
        };
    }

    fn getLayout(self: *Self, idx: Idx) *Layout {
        return self.layouts.get(@enumFromInt(idx.idx));
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
        self: *Store,
        var_store: *const types_store.Store,
        unresolved_var: Var,
    ) (LayoutError || std.mem.Allocator.Error)!Idx {
        var current = var_store.resolveVar(unresolved_var);

        // If we've already seen this var, return the layout we resolved it to.
        if (self.var_to_layout.get(current.var_)) |cached_idx| {
            return cached_idx;
        }

        // To make this function stack-safe, we use a manual stack instead of recursing.
        // We reuse that stack from call to call to avoid reallocating it.
        self.work.clearRetainingCapacity();

        var result: ?Idx = null;
        var iterations: u32 = 0;
        const max_iterations = 10000; // Safety limit to prevent infinite loops

        outer: while (true) {
            iterations += 1;
            if (iterations > max_iterations) {
                std.debug.panic("Layout computation exceeded iteration limit - possible infinite loop\n", .{});
            }

            var layout_idx = switch (current.desc.content) {
                .structure => |flat_type| switch (flat_type) {
                    .str => try self.insertLayout(.str),
                    .box => |elem_var| {
                        try self.work.pending_containers.append(self.env.gpa, .box);
                        // Push a pending Box container and "recurse" on the elem type
                        current = var_store.resolveVar(elem_var);
                        continue;
                    },
                    .list => |elem_var| {
                        try self.work.pending_containers.append(self.env.gpa, .list);
                        // Push a pending Box container and "recurse" on the elem type
                        current = var_store.resolveVar(elem_var);
                        continue;
                    },
                    .custom_type => |custom_type| {
                        // TODO special-case the builtin Num type here.
                        // If we have one of those, then convert it to a num layout,
                        // or to a runtime error if it's an invalid elem type.

                        // From a layout perspective, custom types are identical to type aliases:
                        // all we care about is what's inside, so just unroll it.
                        current = var_store.resolveVar(custom_type.backing_var);
                        continue;
                    },
                    .num => |num| blk: {
                        const layout_to_insert = switch (num) {
                            .int => |int| switch (int) {
                                .exact => |precision| Layout{ .int = precision },
                                // For Int(a), use the default precision for an Int.
                                .flex_var => Layout{ .int = types.Num.Int.Precision.default },
                            },
                            .frac => |frac| switch (frac) {
                                .exact => |precision| Layout{ .frac = precision },
                                // For Frac(a), use the default precision for a Frac.
                                .flex_var => Layout{ .frac = types.Num.Frac.Precision.default },
                            },
                            // Num(a) defaults to Int(a), so use the default precision for an Int.
                            .flex_var => Layout{ .int = types.Num.Int.Precision.default },
                        };
                        break :blk try self.insertLayout(layout_to_insert);
                    },
                    .tuple => |tuple| {
                        // TODO
                        _ = tuple;
                        @panic("TODO: tuple layout");
                    },
                    .func => |func| {
                        // TODO
                        _ = func;
                        @panic("TODO: func layout");
                    },
                    .record => |record_type| {
                        var num_fields = record_type.fields.len();

                        // Collect the record's fields, then add its extension fields.
                        const field_slice = @constCast(var_store).getRecordFieldsSlice(record_type.fields);
                        for (field_slice.items(.name), field_slice.items(.var_)) |name, var_| {
                            // TODO is it possible that here we're encountering record fields with names
                            // already in the list? Would type-checking have already deduped them?
                            // We would certainly rather not spend time doing hashmap things if we can avoid it here.
                            try self.work.pending_record_fields.append(self.env.gpa, .{ .name = name, .var_ = var_ });
                        }

                        var current_ext = record_type.ext;
                        while (true) {
                            const resolved_ext = var_store.resolveVar(current_ext);
                            switch (resolved_ext.desc.content) {
                                .structure => |ext_flat_type| switch (ext_flat_type) {
                                    .empty_record => break,
                                    .record => |ext_record| {
                                        if (ext_record.fields.len() > 0) {
                                            num_fields += ext_record.fields.len();
                                            const ext_field_slice = @constCast(var_store).getRecordFieldsSlice(ext_record.fields);
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
                                    else => return LayoutError.InvalidRecordExtension,
                                },
                                .alias => |alias| {
                                    current_ext = alias.backing_var;
                                },
                                else => return LayoutError.InvalidRecordExtension,
                            }
                        }

                        switch (num_fields) {
                            0 => {
                                // This is an empty record!
                                // Return error for zero-sized type
                                return LayoutError.ZeroSizedType;
                            },
                            else => {
                                // Handle both single-field and multi-field records
                                if (num_fields > 1) {
                                    // Sort these fields *descending* by field name. (Descending because we will pop
                                    // these off one at a time to build up the layout, resulting in the layout being
                                    // sorted *ascending* by field name. Later, it will be further sorted by alignment.)
                                    const SortCtx = struct {
                                        fields: *@TypeOf(self.work.pending_record_fields),
                                        env: *base.ModuleEnv,
                                        pub fn lessThan(ctx: @This(), a_idx: usize, b_idx: usize) bool {
                                            const a = ctx.fields.get(a_idx);
                                            const b = ctx.fields.get(b_idx);
                                            const a_str = ctx.env.idents.getText(a.name);
                                            const b_str = ctx.env.idents.getText(b.name);
                                            // Sort descending (b before a)
                                            return std.mem.order(u8, b_str, a_str) == .lt;
                                        }
                                    };

                                    self.work.pending_record_fields.sortSpan(
                                        self.work.pending_record_fields.len - num_fields,
                                        self.work.pending_record_fields.len,
                                        SortCtx{ .fields = &self.work.pending_record_fields, .env = self.env },
                                    );
                                }

                                try self.work.pending_containers.append(self.env.gpa, .{
                                    .record = .{
                                        .num_fields = @intCast(num_fields),
                                        .pending_fields = @intCast(num_fields),
                                        .resolved_fields_start = @intCast(self.work.resolved_record_fields.len),
                                    },
                                });

                                // Start working on the last pending field (we want to pop them).
                                const field = self.work.pending_record_fields.get(self.work.pending_record_fields.len - 1);

                                current = var_store.resolveVar(field.var_);
                                continue;
                            },
                        }
                        // For the switch expression to type check, we need to continue processing
                        continue;
                    },
                    .tag_union => |tag_union| {
                        // TODO
                        _ = tag_union;
                        @panic("TODO: tag_union layout");
                    },
                    .empty_record => blk: {
                        // Empty records are zero-sized
                        if (self.work.pending_containers.pop()) |pending_container| {
                            switch (pending_container) {
                                .list => {
                                    // It turned out we were getting the layout for a List({})
                                    break :blk try self.insertLayout(.list_zero_sized);
                                },
                                .box => {
                                    // It turned out we were getting the layout for a Box({})
                                    break :blk try self.insertLayout(.box_zero_sized);
                                },
                                .record => |pending_record| {
                                    // It turned out we were getting the layout for a record field
                                    // whose type was {}, which means that field should be dropped.
                                    std.debug.assert(pending_record.pending_fields > 0);
                                    var updated_record = pending_record;
                                    updated_record.pending_fields -= 1;

                                    // The current field we're working on has turned out to be zero-sized, so drop it.
                                    const field = self.work.pending_record_fields.pop() orelse unreachable;

                                    std.debug.assert(current.var_ == field.var_);

                                    if (updated_record.pending_fields == 0) {
                                        // We finished the record we were working on.
                                        // Copy only this record's resolved fields into a contiguous chunk in record_fields
                                        const resolved_fields_end = self.work.resolved_record_fields.len;
                                        const num_resolved_fields = resolved_fields_end - updated_record.resolved_fields_start;

                                        if (num_resolved_fields == 0) {
                                            // All fields were zero-sized, this is an empty record
                                            self.work.resolved_record_fields.shrinkRetainingCapacity(updated_record.resolved_fields_start);

                                            // Check if we're inside another container
                                            if (self.work.pending_containers.items.len > 0) {
                                                // We're inside a container, handle it appropriately
                                                const outer_container = self.work.pending_containers.pop().?;
                                                switch (outer_container) {
                                                    .box => {
                                                        break :blk try self.insertLayout(.box_zero_sized);
                                                    },
                                                    .list => {
                                                        break :blk try self.insertLayout(.list_zero_sized);
                                                    },
                                                    .record => |parent_record| {
                                                        // This record with all zero-sized fields is being processed as a field
                                                        // of another record. Since zero-sized types don't exist at runtime,
                                                        // this field should be dropped from the parent record.

                                                        // We need to continue processing the parent record
                                                        // First, we need to properly handle the parent's state
                                                        var updated_parent = parent_record;
                                                        updated_parent.pending_fields -= 1;

                                                        // Pop the field that turned out to be zero-sized
                                                        _ = self.work.pending_record_fields.pop() orelse unreachable;

                                                        if (updated_parent.pending_fields == 0) {
                                                            // Finished processing parent record
                                                            const parent_resolved_end = self.work.resolved_record_fields.len;
                                                            const parent_num_resolved = parent_resolved_end - updated_parent.resolved_fields_start;

                                                            if (parent_num_resolved == 0) {
                                                                // Parent also has all zero-sized fields
                                                                self.work.resolved_record_fields.shrinkRetainingCapacity(updated_parent.resolved_fields_start);

                                                                // Check if there's another container
                                                                if (self.work.pending_containers.items.len > 0) {
                                                                    const grandparent = self.work.pending_containers.pop().?;
                                                                    switch (grandparent) {
                                                                        .box => break :blk try self.insertLayout(.box_zero_sized),
                                                                        .list => break :blk try self.insertLayout(.list_zero_sized),
                                                                        .record => return LayoutError.ZeroSizedType, // Too deep nesting
                                                                    }
                                                                } else {
                                                                    return LayoutError.ZeroSizedType;
                                                                }
                                                            }

                                                            // Parent has some non-zero fields - complete it
                                                            const parent_fields_start = self.record_fields.items.len;
                                                            const parent_field_names = self.work.resolved_record_fields.items(.field_name);
                                                            const parent_field_idxs = self.work.resolved_record_fields.items(.field_idx);

                                                            // Collect and sort parent's fields
                                                            var parent_temp_fields = std.ArrayList(RecordField).init(self.env.gpa);
                                                            defer parent_temp_fields.deinit();

                                                            for (updated_parent.resolved_fields_start..parent_resolved_end) |i| {
                                                                try parent_temp_fields.append(.{
                                                                    .name = parent_field_names[i],
                                                                    .layout = parent_field_idxs[i],
                                                                });
                                                            }

                                                            // Sort by alignment then name
                                                            const ParentSortCtx = struct {
                                                                store: *Self,
                                                                env: *base.ModuleEnv,
                                                                fn lessThan(ctx: @This(), a: RecordField, b: RecordField) bool {
                                                                    const a_layout = ctx.store.getLayout(a.layout);
                                                                    const b_layout = ctx.store.getLayout(b.layout);
                                                                    const usize_alignment = Alignment.fromBytes(@sizeOf(usize));
                                                                    const a_alignment = a_layout.alignment(usize_alignment);
                                                                    const b_alignment = b_layout.alignment(usize_alignment);

                                                                    if (a_alignment.toBytes() != b_alignment.toBytes()) {
                                                                        return a_alignment.toBytes() > b_alignment.toBytes();
                                                                    }

                                                                    const a_str = ctx.env.idents.getText(a.name);
                                                                    const b_str = ctx.env.idents.getText(b.name);
                                                                    return std.mem.order(u8, a_str, b_str) == .lt;
                                                                }
                                                            };

                                                            std.mem.sort(
                                                                RecordField,
                                                                parent_temp_fields.items,
                                                                ParentSortCtx{ .store = self, .env = self.env },
                                                                ParentSortCtx.lessThan,
                                                            );

                                                            // Add sorted fields to store
                                                            for (parent_temp_fields.items) |sorted_field| {
                                                                _ = self.record_fields.append(self.env.gpa, sorted_field);
                                                            }

                                                            // Calculate alignment and size
                                                            var parent_max_alignment = Alignment.fromBytes(1);
                                                            var parent_current_offset = layout.Size.fromBytes(0);

                                                            for (parent_temp_fields.items) |temp_field| {
                                                                const field_layout = self.getLayout(temp_field.layout);
                                                                const usize_alignment = Alignment.fromBytes(@sizeOf(usize));
                                                                const field_alignment = field_layout.alignment(usize_alignment);
                                                                const field_size = field_layout.size(@sizeOf(usize));

                                                                parent_max_alignment = Alignment.max(parent_max_alignment, field_alignment);
                                                                parent_current_offset = parent_current_offset.alignForward(field_alignment);
                                                                parent_current_offset = layout.Size.add(parent_current_offset, layout.Size.fromBytes(field_size));
                                                            }

                                                            const parent_total_size = parent_current_offset.alignForward(parent_max_alignment);
                                                            const parent_fields_range = collections.NonEmptyRange.init(@intCast(parent_fields_start), @intCast(parent_num_resolved)) catch unreachable;

                                                            self.work.resolved_record_fields.shrinkRetainingCapacity(updated_parent.resolved_fields_start);
                                                            break :blk try self.insertLayout(.{ .record = .{ .fields = parent_fields_range, .alignment = parent_max_alignment, .size = parent_total_size } });
                                                        } else {
                                                            // More fields to process in parent
                                                            try self.work.pending_containers.append(self.env.gpa, .{ .record = updated_parent });
                                                            // Continue with next field
                                                            const next_field = self.work.pending_record_fields.get(self.work.pending_record_fields.len - 1);
                                                            current = var_store.resolveVar(next_field.var_);
                                                            continue :outer;
                                                        }
                                                    },
                                                }
                                            } else {
                                                // This is a top-level zero-sized record
                                                return LayoutError.ZeroSizedType;
                                            }
                                        } else {
                                            const fields_start = self.record_fields.items.len;

                                            // Copy only this record's resolved fields to the record_fields store
                                            const field_names = self.work.resolved_record_fields.items(.field_name);
                                            const field_idxs = self.work.resolved_record_fields.items(.field_idx);

                                            // First, collect the fields into a temporary array so we can sort them
                                            var temp_fields = std.ArrayList(RecordField).init(self.env.gpa);
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
                                                env: *base.ModuleEnv,
                                                fn lessThan(ctx: @This(), a: RecordField, b: RecordField) bool {
                                                    const a_layout = ctx.store.getLayout(a.layout);
                                                    const b_layout = ctx.store.getLayout(b.layout);
                                                    const usize_alignment = Alignment.fromBytes(@sizeOf(usize));
                                                    const a_alignment = a_layout.alignment(usize_alignment);
                                                    const b_alignment = b_layout.alignment(usize_alignment);

                                                    // First sort by alignment (descending - higher alignment first)
                                                    if (a_alignment.toBytes() != b_alignment.toBytes()) {
                                                        return a_alignment.toBytes() > b_alignment.toBytes();
                                                    }

                                                    // Then sort by name (ascending)
                                                    const a_str = ctx.env.idents.getText(a.name);
                                                    const b_str = ctx.env.idents.getText(b.name);
                                                    return std.mem.order(u8, a_str, b_str) == .lt;
                                                }
                                            };

                                            std.mem.sort(
                                                RecordField,
                                                temp_fields.items,
                                                AlignmentSortCtx{ .store = self, .env = self.env },
                                                AlignmentSortCtx.lessThan,
                                            );

                                            // Now add them to the record_fields store in the sorted order
                                            for (temp_fields.items) |sorted_field| {
                                                _ = self.record_fields.append(self.env.gpa, sorted_field);
                                            }

                                            // Calculate max alignment and total size of all fields
                                            var max_alignment = Alignment.fromBytes(1);
                                            var current_offset = layout.Size.fromBytes(0);

                                            var field_idx: u32 = 0;
                                            while (field_idx < temp_fields.items.len) : (field_idx += 1) {
                                                const temp_field = temp_fields.items[field_idx];
                                                const field_layout = self.getLayout(temp_field.layout);
                                                const usize_alignment = Alignment.fromBytes(@sizeOf(usize));
                                                const field_alignment = field_layout.alignment(usize_alignment);
                                                const field_size = field_layout.size(@sizeOf(usize));

                                                // Update max alignment
                                                max_alignment = Alignment.max(max_alignment, field_alignment);

                                                // Align current offset to field's alignment
                                                current_offset = current_offset.alignForward(field_alignment);

                                                // Add field size
                                                current_offset = layout.Size.add(current_offset, layout.Size.fromBytes(field_size));
                                            }

                                            // Final size must be aligned to the record's alignment
                                            const total_size = current_offset.alignForward(max_alignment);

                                            // Create the record layout with the fields range
                                            const fields_range = collections.NonEmptyRange.init(@intCast(fields_start), @intCast(num_resolved_fields)) catch unreachable;
                                            const record_layout = Layout{ .record = .{ .fields = fields_range, .alignment = max_alignment, .size = total_size } };

                                            // Remove only this record's resolved fields
                                            self.work.resolved_record_fields.shrinkRetainingCapacity(updated_record.resolved_fields_start);

                                            break :blk try self.insertLayout(record_layout);
                                        }
                                    } else {
                                        // Still have pending fields to process
                                        try self.work.pending_containers.append(self.env.gpa, .{ .record = updated_record });

                                        // Get the next field to process
                                        const next_field = self.work.pending_record_fields.get(self.work.pending_record_fields.len - 1);
                                        current = var_store.resolveVar(next_field.var_);
                                        continue;
                                    }
                                },
                            }
                        }

                        // Unboxed zero-sized types cannot have a layout
                        return LayoutError.ZeroSizedType;
                    },
                    .empty_tag_union => blk: {
                        // Empty tag unions are zero-sized
                        if (self.work.pending_containers.pop()) |pending_container| {
                            switch (pending_container) {
                                .list => {
                                    break :blk try self.insertLayout(.list_zero_sized);
                                },
                                .box => {
                                    break :blk try self.insertLayout(.box_zero_sized);
                                },
                                .record => |pending_record| {
                                    // There should be at least one remaining pending field!
                                    std.debug.assert(pending_record.pending_fields > 0);
                                    var updated_record = pending_record;
                                    updated_record.pending_fields -= 1;

                                    // The current field we're working on has turned out to be zero-sized, so drop it.
                                    const field = self.work.pending_record_fields.pop() orelse unreachable;

                                    std.debug.assert(current.var_ == field.var_);

                                    if (updated_record.pending_fields == 0) {
                                        // We finished the record we were working on.
                                        const resolved_fields_end = self.work.resolved_record_fields.len;
                                        const num_resolved_fields = resolved_fields_end - updated_record.resolved_fields_start;

                                        if (num_resolved_fields == 0) {
                                            // All fields were zero-sized, this is an empty record
                                            self.work.resolved_record_fields.shrinkRetainingCapacity(updated_record.resolved_fields_start);

                                            // Check if we're inside another container
                                            if (self.work.pending_containers.items.len > 0) {
                                                // We're inside a container, handle it appropriately
                                                const outer_container = self.work.pending_containers.pop().?;
                                                switch (outer_container) {
                                                    .box => {
                                                        break :blk try self.insertLayout(.box_zero_sized);
                                                    },
                                                    .list => {
                                                        break :blk try self.insertLayout(.list_zero_sized);
                                                    },
                                                    .record => |parent_record| {
                                                        // Similar to the empty record case above
                                                        // This empty tag union is a field in a record - drop it
                                                        var updated_parent = parent_record;
                                                        updated_parent.pending_fields -= 1;

                                                        // Pop the field that turned out to be zero-sized
                                                        _ = self.work.pending_record_fields.pop() orelse unreachable;

                                                        if (updated_parent.pending_fields == 0) {
                                                            // Finished processing parent record
                                                            const parent_resolved_end = self.work.resolved_record_fields.len;
                                                            const parent_num_resolved = parent_resolved_end - updated_parent.resolved_fields_start;

                                                            if (parent_num_resolved == 0) {
                                                                // Parent also has all zero-sized fields
                                                                self.work.resolved_record_fields.shrinkRetainingCapacity(updated_parent.resolved_fields_start);

                                                                // Check if there's another container
                                                                if (self.work.pending_containers.items.len > 0) {
                                                                    const grandparent = self.work.pending_containers.pop().?;
                                                                    switch (grandparent) {
                                                                        .box => break :blk try self.insertLayout(.box_zero_sized),
                                                                        .list => break :blk try self.insertLayout(.list_zero_sized),
                                                                        .record => return LayoutError.ZeroSizedType, // Too deep nesting
                                                                    }
                                                                } else {
                                                                    return LayoutError.ZeroSizedType;
                                                                }
                                                            }

                                                            // Complete parent record - reuse same logic as above
                                                            const parent_fields_start = self.record_fields.items.len;
                                                            const parent_field_names = self.work.resolved_record_fields.items(.field_name);
                                                            const parent_field_idxs = self.work.resolved_record_fields.items(.field_idx);

                                                            var parent_temp_fields = std.ArrayList(RecordField).init(self.env.gpa);
                                                            defer parent_temp_fields.deinit();

                                                            for (updated_parent.resolved_fields_start..parent_resolved_end) |i| {
                                                                try parent_temp_fields.append(.{
                                                                    .name = parent_field_names[i],
                                                                    .layout = parent_field_idxs[i],
                                                                });
                                                            }

                                                            const ParentSortCtx2 = struct {
                                                                store: *Self,
                                                                env: *base.ModuleEnv,
                                                                fn lessThan(ctx: @This(), a: RecordField, b: RecordField) bool {
                                                                    const a_layout = ctx.store.getLayout(a.layout);
                                                                    const b_layout = ctx.store.getLayout(b.layout);
                                                                    const usize_alignment = Alignment.fromBytes(@sizeOf(usize));
                                                                    const a_alignment = a_layout.alignment(usize_alignment);
                                                                    const b_alignment = b_layout.alignment(usize_alignment);

                                                                    if (a_alignment.toBytes() != b_alignment.toBytes()) {
                                                                        return a_alignment.toBytes() > b_alignment.toBytes();
                                                                    }

                                                                    const a_str = ctx.env.idents.getText(a.name);
                                                                    const b_str = ctx.env.idents.getText(b.name);
                                                                    return std.mem.order(u8, a_str, b_str) == .lt;
                                                                }
                                                            };

                                                            std.mem.sort(
                                                                RecordField,
                                                                parent_temp_fields.items,
                                                                ParentSortCtx2{ .store = self, .env = self.env },
                                                                ParentSortCtx2.lessThan,
                                                            );

                                                            for (parent_temp_fields.items) |sorted_field| {
                                                                _ = self.record_fields.append(self.env.gpa, sorted_field);
                                                            }

                                                            var parent_max_alignment = Alignment.fromBytes(1);
                                                            var parent_current_offset = layout.Size.fromBytes(0);

                                                            for (parent_temp_fields.items) |temp_field| {
                                                                const field_layout = self.getLayout(temp_field.layout);
                                                                const usize_alignment = Alignment.fromBytes(@sizeOf(usize));
                                                                const field_alignment = field_layout.alignment(usize_alignment);
                                                                const field_size = field_layout.size(@sizeOf(usize));

                                                                parent_max_alignment = Alignment.max(parent_max_alignment, field_alignment);
                                                                parent_current_offset = parent_current_offset.alignForward(field_alignment);
                                                                parent_current_offset = layout.Size.add(parent_current_offset, layout.Size.fromBytes(field_size));
                                                            }

                                                            const parent_total_size = parent_current_offset.alignForward(parent_max_alignment);
                                                            const parent_fields_range = collections.NonEmptyRange.init(@intCast(parent_fields_start), @intCast(parent_num_resolved)) catch unreachable;

                                                            self.work.resolved_record_fields.shrinkRetainingCapacity(updated_parent.resolved_fields_start);
                                                            break :blk try self.insertLayout(.{ .record = .{ .fields = parent_fields_range, .alignment = parent_max_alignment, .size = parent_total_size } });
                                                        } else {
                                                            // More fields to process in parent
                                                            try self.work.pending_containers.append(self.env.gpa, .{ .record = updated_parent });
                                                            // Continue with next field
                                                            const next_field = self.work.pending_record_fields.get(self.work.pending_record_fields.len - 1);
                                                            current = var_store.resolveVar(next_field.var_);
                                                            continue :outer;
                                                        }
                                                    },
                                                }
                                            } else {
                                                // This is a top-level zero-sized record
                                                return LayoutError.ZeroSizedType;
                                            }
                                        }

                                        // We have some resolved fields - complete the record here
                                        const fields_start = self.record_fields.items.len;

                                        // Copy only this record's resolved fields to the record_fields store
                                        const field_names = self.work.resolved_record_fields.items(.field_name);
                                        const field_idxs = self.work.resolved_record_fields.items(.field_idx);

                                        // First, collect the fields into a temporary array so we can sort them
                                        var temp_fields = std.ArrayList(RecordField).init(self.env.gpa);
                                        defer temp_fields.deinit();

                                        var i: u32 = updated_record.resolved_fields_start;
                                        while (i < resolved_fields_end) : (i += 1) {
                                            try temp_fields.append(.{
                                                .name = field_names[i],
                                                .layout = field_idxs[i],
                                            });
                                        }

                                        // Sort fields by alignment (descending) first, then by name (ascending)
                                        const AlignmentSortCtx = struct {
                                            store: *Self,
                                            env: *base.ModuleEnv,
                                            fn lessThan(ctx: @This(), a: RecordField, b: RecordField) bool {
                                                const a_layout = ctx.store.getLayout(a.layout);
                                                const b_layout = ctx.store.getLayout(b.layout);
                                                const usize_alignment = Alignment.fromBytes(@sizeOf(usize));
                                                const a_alignment = a_layout.alignment(usize_alignment);
                                                const b_alignment = b_layout.alignment(usize_alignment);

                                                // First sort by alignment (descending - higher alignment first)
                                                if (a_alignment.toBytes() != b_alignment.toBytes()) {
                                                    return a_alignment.toBytes() > b_alignment.toBytes();
                                                }

                                                // Then sort by name (ascending)
                                                const a_str = ctx.env.idents.getText(a.name);
                                                const b_str = ctx.env.idents.getText(b.name);
                                                return std.mem.order(u8, a_str, b_str) == .lt;
                                            }
                                        };

                                        std.mem.sort(
                                            RecordField,
                                            temp_fields.items,
                                            AlignmentSortCtx{ .store = self, .env = self.env },
                                            AlignmentSortCtx.lessThan,
                                        );

                                        // Now append the sorted fields
                                        for (temp_fields.items) |sorted_field| {
                                            _ = self.record_fields.append(self.env.gpa, sorted_field);
                                        }

                                        // Calculate max alignment and total size
                                        var max_alignment = Alignment.fromBytes(1);
                                        var current_offset = layout.Size.fromBytes(0);

                                        var field_idx: u32 = 0;
                                        while (field_idx < temp_fields.items.len) : (field_idx += 1) {
                                            const temp_field = temp_fields.items[field_idx];
                                            const field_layout = self.getLayout(temp_field.layout);
                                            const usize_alignment = Alignment.fromBytes(@sizeOf(usize));
                                            const field_alignment = field_layout.alignment(usize_alignment);
                                            const field_size = field_layout.size(@sizeOf(usize));

                                            // Update max alignment
                                            max_alignment = Alignment.max(max_alignment, field_alignment);

                                            // Align current offset to field's alignment
                                            current_offset = current_offset.alignForward(field_alignment);

                                            // Add field size
                                            current_offset = layout.Size.add(current_offset, layout.Size.fromBytes(field_size));
                                        }

                                        // Final size must be aligned to the record's alignment
                                        const total_size = current_offset.alignForward(max_alignment);

                                        // Create the record layout with the fields range
                                        const fields_range = collections.NonEmptyRange.init(@intCast(fields_start), @intCast(num_resolved_fields)) catch unreachable;
                                        const record_layout = Layout{ .record = .{ .fields = fields_range, .alignment = max_alignment, .size = total_size } };

                                        // Remove only this record's resolved fields
                                        self.work.resolved_record_fields.shrinkRetainingCapacity(updated_record.resolved_fields_start);

                                        break :blk try self.insertLayout(record_layout);
                                    } else {
                                        // Still have fields to process, push the container back
                                        try self.work.pending_containers.append(self.env.gpa, .{ .record = updated_record });

                                        // Get the next field to process
                                        const next_field = self.work.pending_record_fields.get(self.work.pending_record_fields.len - 1);
                                        current = var_store.resolveVar(next_field.var_);
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
                    // Flex vars can only be sent to the host if boxed.
                    if (self.work.pending_containers.getLastOrNull()) |pending_container| {
                        if (pending_container == .box or pending_container == .list) {
                            break :blk try self.insertLayout(.host_opaque);
                        }
                    }

                    std.debug.assert(false);
                    return LayoutError.BugUnboxedFlexVar;
                },
                .rigid_var => |_| blk: {
                    // Rigid vars can only be sent to the host if boxed.
                    if (self.work.pending_containers.getLastOrNull()) |pending_container| {
                        if (pending_container == .box or pending_container == .list) {
                            break :blk try self.insertLayout(.host_opaque);
                        }
                    }

                    std.debug.assert(false);
                    return LayoutError.BugUnboxedRigidVar;
                },
                .alias => |alias| {
                    // Follow the alias by updating the work item
                    current = var_store.resolveVar(alias.backing_var);
                    continue;
                },
                .effectful => @panic("TODO: effectful doesn't make sense as a layout; should be moved out of Content"),
                .pure => @panic("pure doesn't make sense as a layout; should be moved out of Content"),
                .err => return LayoutError.TypeContainedMismatch,
            };

            // We actually resolved a layout and didn't `continue`!
            // First things first: add it to the cache.
            try self.var_to_layout.put(self.env.gpa, current.var_, layout_idx);

            // Next, see if this was part of some pending work.
            while (self.work.pending_containers.items.len > 0) {
                const pending_container = self.work.pending_containers.pop().?;
                switch (pending_container) {
                    .box => {
                        // We got the layout for the box's content
                        // Check if the content is zero-sized
                        const content_layout = self.getLayout(layout_idx);
                        const box_layout = switch (content_layout.*) {
                            .box_zero_sized, .list_zero_sized => Layout.box_zero_sized,
                            else => Layout{ .box = layout_idx },
                        };
                        const box_layout_idx = try self.insertLayout(box_layout);
                        layout_idx = box_layout_idx;
                    },
                    .list => {
                        // We got the layout for the list's element
                        // Check if the element is zero-sized
                        const element_layout = self.getLayout(layout_idx);
                        const list_layout = switch (element_layout.*) {
                            .box_zero_sized, .list_zero_sized => Layout.list_zero_sized,
                            else => Layout{ .list = layout_idx },
                        };
                        const list_layout_idx = try self.insertLayout(list_layout);
                        layout_idx = list_layout_idx;
                    },
                    .record => |pending_record| {
                        std.debug.assert(pending_record.pending_fields > 0);
                        var updated_record = pending_record;
                        updated_record.pending_fields -= 1;

                        // Pop the field we just processed
                        const pending_field = self.work.pending_record_fields.pop() orelse unreachable;

                        // Add to resolved fields
                        try self.work.resolved_record_fields.append(self.env.gpa, .{
                            .field_name = pending_field.name,
                            .field_idx = layout_idx,
                        });

                        if (updated_record.pending_fields == 0) {
                            // We finished the record we were working on.
                            // Copy only this record's resolved fields into a contiguous chunk in record_fields
                            const resolved_fields_end = self.work.resolved_record_fields.len;
                            const num_resolved_fields = resolved_fields_end - updated_record.resolved_fields_start;

                            // Check if all fields were zero-sized
                            if (num_resolved_fields == 0) {
                                // All fields were zero-sized, this is an empty record
                                // Clean up
                                self.work.resolved_record_fields.shrinkRetainingCapacity(updated_record.resolved_fields_start);

                                // Check if we're inside another container
                                if (self.work.pending_containers.items.len > 0) {
                                    // We're inside a container (box/list)
                                    const inner_outer_container = self.work.pending_containers.pop().?;
                                    switch (inner_outer_container) {
                                        .box => {
                                            layout_idx = try self.insertLayout(.box_zero_sized);
                                        },
                                        .list => {
                                            layout_idx = try self.insertLayout(.list_zero_sized);
                                        },
                                        .record => |inner_pending_record| {
                                            // It turned out we were getting the layout for a record field
                                            // whose type is a zero-sized type, which means that field should be dropped.
                                            std.debug.assert(inner_pending_record.pending_fields > 0);
                                            var inner_updated_record = inner_pending_record;
                                            inner_updated_record.pending_fields -= 1;

                                            // The current field we're working on has turned out to be zero-sized, so drop it.
                                            const field = self.work.pending_record_fields.pop() orelse unreachable;

                                            std.debug.assert(current.var_ == field.var_);

                                            if (inner_updated_record.pending_fields == 0) {
                                                // We finished the record we were working on.
                                                const inner_resolved_fields_end = self.work.resolved_record_fields.len;
                                                const inner_num_resolved_fields = inner_resolved_fields_end - inner_updated_record.resolved_fields_start;

                                                if (inner_num_resolved_fields == 0) {
                                                    // All fields were zero-sized, this is an empty record
                                                    self.work.resolved_record_fields.shrinkRetainingCapacity(inner_updated_record.resolved_fields_start);

                                                    // Check if we're inside another container
                                                    if (self.work.pending_containers.items.len > 0) {
                                                        // We're inside a container, handle it appropriately
                                                        const outer_container = self.work.pending_containers.pop().?;
                                                        switch (outer_container) {
                                                            .box => {
                                                                layout_idx = try self.insertLayout(.box_zero_sized);
                                                            },
                                                            .list => {
                                                                layout_idx = try self.insertLayout(.list_zero_sized);
                                                            },
                                                            .record => unreachable, // Records shouldn't be directly nested in this way
                                                        }
                                                    } else {
                                                        // This is a top-level zero-sized record
                                                        return LayoutError.ZeroSizedType;
                                                    }
                                                } else {
                                                    const inner_fields_start = self.record_fields.items.len;

                                                    // Copy only this record's resolved fields to the record_fields store
                                                    const inner_field_names = self.work.resolved_record_fields.items(.field_name);
                                                    const inner_field_idxs = self.work.resolved_record_fields.items(.field_idx);

                                                    // First, collect the fields into a temporary array so we can sort them
                                                    var inner_temp_fields = std.ArrayList(RecordField).init(self.env.gpa);
                                                    defer inner_temp_fields.deinit();

                                                    for (inner_updated_record.resolved_fields_start..inner_resolved_fields_end) |i| {
                                                        try inner_temp_fields.append(.{
                                                            .name = inner_field_names[i],
                                                            .layout = inner_field_idxs[i],
                                                        });
                                                    }

                                                    // Sort the fields by name
                                                    const InnerFieldSorter = struct {
                                                        store: *Self,
                                                        env: *base.ModuleEnv,
                                                        pub fn lessThan(ctx: @This(), lhs: RecordField, rhs: RecordField) bool {
                                                            const lhs_layout = ctx.store.getLayout(lhs.layout);
                                                            const rhs_layout = ctx.store.getLayout(rhs.layout);
                                                            const usize_alignment = Alignment.fromBytes(@sizeOf(usize));
                                                            const lhs_alignment = lhs_layout.alignment(usize_alignment);
                                                            const rhs_alignment = rhs_layout.alignment(usize_alignment);

                                                            // First sort by alignment (descending - higher alignment first)
                                                            if (lhs_alignment.toBytes() != rhs_alignment.toBytes()) {
                                                                return lhs_alignment.toBytes() > rhs_alignment.toBytes();
                                                            }

                                                            // Then sort by name (ascending)
                                                            const lhs_str = ctx.env.idents.getText(lhs.name);
                                                            const rhs_str = ctx.env.idents.getText(rhs.name);
                                                            return std.mem.order(u8, lhs_str, rhs_str) == .lt;
                                                        }
                                                    };
                                                    std.mem.sort(RecordField, inner_temp_fields.items, InnerFieldSorter{ .store = self, .env = self.env }, InnerFieldSorter.lessThan);

                                                    // Now add them to the record_fields store
                                                    for (inner_temp_fields.items) |inner_field| {
                                                        _ = self.record_fields.append(self.env.gpa, inner_field);
                                                    }

                                                    // Calculate max alignment and total size of all fields
                                                    var max_alignment = Alignment.fromBytes(1);
                                                    var current_offset = layout.Size.fromBytes(0);

                                                    var field_idx: u32 = 0;
                                                    while (field_idx < inner_temp_fields.items.len) : (field_idx += 1) {
                                                        const temp_field = inner_temp_fields.items[field_idx];
                                                        const field_layout = self.getLayout(temp_field.layout);
                                                        const usize_alignment = Alignment.fromBytes(@sizeOf(usize));
                                                        const field_alignment = field_layout.alignment(usize_alignment);
                                                        const field_size = field_layout.size(@sizeOf(usize));

                                                        // Update max alignment
                                                        max_alignment = Alignment.max(max_alignment, field_alignment);

                                                        // Align current offset to field's alignment
                                                        current_offset = current_offset.alignForward(field_alignment);

                                                        // Add field size
                                                        current_offset = layout.Size.add(current_offset, layout.Size.fromBytes(field_size));
                                                    }

                                                    // Final size must be aligned to the record's alignment
                                                    const total_size = current_offset.alignForward(max_alignment);

                                                    // Clean up resolved fields for this record
                                                    self.work.resolved_record_fields.shrinkRetainingCapacity(inner_updated_record.resolved_fields_start);

                                                    // Create the record layout with the fields range
                                                    const fields_range = collections.NonEmptyRange.init(@intCast(inner_fields_start), @intCast(inner_temp_fields.items.len)) catch unreachable;
                                                    layout_idx = try self.insertLayout(.{ .record = .{ .fields = fields_range, .alignment = max_alignment, .size = total_size } });
                                                }
                                            } else {
                                                // More fields to process, push the updated record back
                                                try self.work.pending_containers.append(self.env.gpa, .{ .record = inner_updated_record });
                                                continue :outer;
                                            }

                                            // Continue with the layout we just created
                                            // The layout_idx is ready to be processed by the outer loop
                                        },
                                    }
                                } else {
                                    // This is a top-level zero-sized record
                                    return LayoutError.ZeroSizedType;
                                }
                            } else {
                                const fields_start = self.record_fields.items.len;

                                // Copy only this record's resolved fields to the record_fields store
                                const field_names = self.work.resolved_record_fields.items(.field_name);
                                const field_idxs = self.work.resolved_record_fields.items(.field_idx);

                                // First, collect the fields into a temporary array so we can sort them
                                var temp_fields = std.ArrayList(RecordField).init(self.env.gpa);
                                defer temp_fields.deinit();

                                var i: u32 = updated_record.resolved_fields_start;
                                while (i < resolved_fields_end) : (i += 1) {
                                    try temp_fields.append(.{
                                        .name = field_names[i],
                                        .layout = field_idxs[i],
                                    });
                                }

                                // Sort fields by alignment (descending) first, then by name (ascending)
                                const AlignmentSortCtx = struct {
                                    store: *Self,
                                    env: *base.ModuleEnv,
                                    fn lessThan(ctx: @This(), a: RecordField, b: RecordField) bool {
                                        const a_layout = ctx.store.getLayout(a.layout);
                                        const b_layout = ctx.store.getLayout(b.layout);
                                        const usize_alignment = Alignment.fromBytes(@sizeOf(usize));
                                        const a_alignment = a_layout.alignment(usize_alignment);
                                        const b_alignment = b_layout.alignment(usize_alignment);

                                        // First sort by alignment (descending - higher alignment first)
                                        if (a_alignment.toBytes() != b_alignment.toBytes()) {
                                            return a_alignment.toBytes() > b_alignment.toBytes();
                                        }

                                        // Then sort by name (ascending)
                                        const a_str = ctx.env.idents.getText(a.name);
                                        const b_str = ctx.env.idents.getText(b.name);
                                        return std.mem.order(u8, a_str, b_str) == .lt;
                                    }
                                };

                                std.mem.sort(
                                    RecordField,
                                    temp_fields.items,
                                    AlignmentSortCtx{ .store = self, .env = self.env },
                                    AlignmentSortCtx.lessThan,
                                );

                                // Now append the sorted fields
                                for (temp_fields.items) |sorted_field| {
                                    _ = self.record_fields.append(self.env.gpa, sorted_field);
                                }

                                // Calculate max alignment and total size
                                var max_alignment = Alignment.fromBytes(1);
                                var current_offset = layout.Size.fromBytes(0);

                                var field_idx: u32 = 0;
                                while (field_idx < temp_fields.items.len) : (field_idx += 1) {
                                    const temp_field = temp_fields.items[field_idx];
                                    const field_layout = self.getLayout(temp_field.layout);
                                    const usize_alignment = Alignment.fromBytes(@sizeOf(usize));
                                    const field_alignment = field_layout.alignment(usize_alignment);
                                    const field_size = field_layout.size(@sizeOf(usize));

                                    // Update max alignment
                                    max_alignment = Alignment.max(max_alignment, field_alignment);

                                    // Align current offset to field's alignment
                                    current_offset = current_offset.alignForward(field_alignment);

                                    // Add field size
                                    current_offset = layout.Size.add(current_offset, layout.Size.fromBytes(field_size));
                                }

                                // Final size must be aligned to the record's alignment
                                const total_size = current_offset.alignForward(max_alignment);

                                // Create the record layout with the fields range
                                const fields_range = collections.NonEmptyRange.init(@intCast(fields_start), @intCast(num_resolved_fields)) catch unreachable;
                                const record_layout = Layout{ .record = .{ .fields = fields_range, .alignment = max_alignment, .size = total_size } };
                                layout_idx = try self.insertLayout(record_layout);

                                // Remove only this record's resolved fields
                                self.work.resolved_record_fields.shrinkRetainingCapacity(updated_record.resolved_fields_start);
                            }
                        } else {
                            // Still have fields to process, push the container back
                            try self.work.pending_containers.append(self.env.gpa, .{ .record = updated_record });

                            // Get the next field to process
                            const next_field = self.work.pending_record_fields.get(self.work.pending_record_fields.len - 1);
                            current = var_store.resolveVar(next_field.var_);
                            break;
                        }
                    },
                }
            }

            // Save the layout result
            result = layout_idx;

            // Check if we have more work to do
            if (self.work.pending_record_fields.len > 0) {
                // Get the next field to process
                const next_field = self.work.pending_record_fields.get(self.work.pending_record_fields.len - 1);
                current = var_store.resolveVar(next_field.var_);
                continue;
            }

            // No more work, we're done
            break;
        }

        if (result) |r| {
            return r;
        } else {
            // This should never happen - we should always have a result or have returned an error
            std.debug.panic("Internal error: addTypeVar completed without setting a result\n", .{});
        }
    }
};

test "addTypeVar - str" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create a str type variable
    const str_var = type_store.freshFromContent(.{ .structure = .str });

    // Convert to layout
    const str_layout_idx = try layout_store.addTypeVar(&type_store, str_var);

    // Verify the layout
    const str_layout = layout_store.getLayout(str_layout_idx);
    try testing.expect(str_layout.* == .str);
}

test "addTypeVar - list of strings" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create a str type variable
    const str_var = type_store.freshFromContent(.{ .structure = .str });

    // Create a list of str type variable
    const list_str_var = type_store.freshFromContent(.{ .structure = .{ .list = str_var } });

    // Convert to layout
    const list_layout_idx = try layout_store.addTypeVar(&type_store, list_str_var);

    // Verify the layout
    const list_layout = layout_store.getLayout(list_layout_idx);
    try testing.expect(list_layout.* == .list);

    // Verify the element layout
    const elem_layout = layout_store.getLayout(list_layout.list);
    try testing.expect(elem_layout.* == .str);
}

test "addTypeVar - list of box of strings" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create a str type variable
    const str_var = type_store.freshFromContent(.{ .structure = .str });

    // Create a box of str type variable
    const box_str_var = type_store.freshFromContent(.{ .structure = .{ .box = str_var } });

    // Create a list of box of str type variable
    const list_box_str_var = type_store.freshFromContent(.{ .structure = .{ .list = box_str_var } });

    // Convert to layout
    const list_layout_idx = try layout_store.addTypeVar(&type_store, list_box_str_var);

    // Verify the list layout
    const list_layout = layout_store.getLayout(list_layout_idx);
    try testing.expect(list_layout.* == .list);

    // Verify the box layout
    const box_layout = layout_store.getLayout(list_layout.list);
    try testing.expect(box_layout.* == .box);

    // Verify the str layout
    const str_layout = layout_store.getLayout(box_layout.box);
    try testing.expect(str_layout.* == .str);
}

test "addTypeVar - box of flex_var compiles to box of host_opaque" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create a flex_var type variable
    const flex_var = type_store.freshFromContent(.{ .flex_var = null });

    // Create a box of flex_var type variable
    const box_flex_var = type_store.freshFromContent(.{ .structure = .{ .box = flex_var } });

    // Convert to layout
    const box_layout_idx = try layout_store.addTypeVar(&type_store, box_flex_var);

    // Verify the box layout
    const box_layout = layout_store.getLayout(box_layout_idx);
    try testing.expect(box_layout.* == .box);

    // Verify the element is host_opaque
    const elem_layout = layout_store.getLayout(box_layout.box);
    try testing.expect(elem_layout.* == .host_opaque);
}

test "addTypeVar - num u32" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create a u32 type variable
    const u32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u32 } } } });

    // Convert to layout
    const u32_layout_idx = try layout_store.addTypeVar(&type_store, u32_var);

    // Verify the layout
    const u32_layout = layout_store.getLayout(u32_layout_idx);
    try testing.expect(u32_layout.* == .int);
    try testing.expect(u32_layout.int == .u32);
}

test "addTypeVar - num f64" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create a f64 type variable
    const f64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .frac = .{ .exact = .f64 } } } });

    // Convert to layout
    const f64_layout_idx = try layout_store.addTypeVar(&type_store, f64_var);

    // Verify the layout
    const f64_layout = layout_store.getLayout(f64_layout_idx);
    try testing.expect(f64_layout.* == .frac);
    try testing.expect(f64_layout.frac == .f64);
}

test "addTypeVar - list of num i128" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create an i128 type variable
    const i128_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i128 } } } });

    // Create a list of i128 type variable
    const list_i128_var = type_store.freshFromContent(.{ .structure = .{ .list = i128_var } });

    // Convert to layout
    const list_layout_idx = try layout_store.addTypeVar(&type_store, list_i128_var);

    // Verify the list layout
    const list_layout = layout_store.getLayout(list_layout_idx);
    try testing.expect(list_layout.* == .list);

    // Verify the element layout
    const elem_layout = layout_store.getLayout(list_layout.list);
    try testing.expect(elem_layout.* == .int);
    try testing.expect(elem_layout.int == .i128);
}

test "addTypeVar - num dec" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create a dec type variable
    const dec_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .frac = .{ .exact = .dec } } } });

    // Convert to layout
    const dec_layout_idx = try layout_store.addTypeVar(&type_store, dec_var);

    // Verify the layout
    const dec_layout = layout_store.getLayout(dec_layout_idx);
    try testing.expect(dec_layout.* == .frac);
    try testing.expect(dec_layout.frac == .dec);
}

test "addTypeVar - flex num var defaults to i128" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create a flex number type variable (Num(a))
    const flex_num_var = type_store.freshFromContent(.{ .structure = .{ .num = .flex_var } });

    // Convert to layout - should default to i128
    const layout_idx = try layout_store.addTypeVar(&type_store, flex_num_var);

    // Verify the layout
    const num_layout = layout_store.getLayout(layout_idx);
    try testing.expect(num_layout.* == .int);
    try testing.expect(num_layout.int == .i128);
}

test "addTypeVar - flex int var defaults to i128" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create a flex int type variable (Int(a))
    const flex_int_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .flex_var } } });

    // Convert to layout - should default to i128
    const layout_idx = try layout_store.addTypeVar(&type_store, flex_int_var);

    // Verify the layout
    const int_layout = layout_store.getLayout(layout_idx);
    try testing.expect(int_layout.* == .int);
    try testing.expect(int_layout.int == .i128);
}

test "addTypeVar - flex frac var defaults to dec" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create a flex frac type variable (Frac(a))
    const flex_frac_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .frac = .flex_var } } });

    // Convert to layout - should default to dec
    const layout_idx = try layout_store.addTypeVar(&type_store, flex_frac_var);

    // Verify the layout
    const frac_layout = layout_store.getLayout(layout_idx);
    try testing.expect(frac_layout.* == .frac);
    try testing.expect(frac_layout.frac == .dec);
}

test "addTypeVar - list of flex num var defaults to list of i128" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create a flex num type variable (Num(a))
    const flex_num_var = type_store.freshFromContent(.{ .structure = .{ .num = .flex_var } });

    // Create a list of flex num type variable
    const list_flex_num_var = type_store.freshFromContent(.{ .structure = .{ .list = flex_num_var } });

    // Convert to layout - should default to list of i128
    const list_layout_idx = try layout_store.addTypeVar(&type_store, list_flex_num_var);

    // Verify the list layout
    const list_layout = layout_store.getLayout(list_layout_idx);
    try testing.expect(list_layout.* == .list);

    // Verify the element layout defaults to i128
    const elem_layout = layout_store.getLayout(list_layout.list);
    try testing.expect(elem_layout.* == .int);
    try testing.expect(elem_layout.int == .i128);
}

test "addTypeVar - box of flex frac var defaults to box of dec" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create a flex frac type variable (Frac(a))
    const flex_frac_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .frac = .flex_var } } });

    // Create a box of flex frac type variable
    const box_flex_frac_var = type_store.freshFromContent(.{ .structure = .{ .box = flex_frac_var } });

    // Convert to layout - should default to box of dec
    const box_layout_idx = try layout_store.addTypeVar(&type_store, box_flex_frac_var);

    // Verify the box layout
    const box_layout = layout_store.getLayout(box_layout_idx);
    try testing.expect(box_layout.* == .box);

    // Verify the element defaults to dec
    const elem_layout = layout_store.getLayout(box_layout.box);
    try testing.expect(elem_layout.* == .frac);
    try testing.expect(elem_layout.frac == .dec);
}

test "addTypeVar - box of rigid_var compiles to box of host_opaque" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create an ident for the rigid var
    const ident_idx = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("a"), base.Region.zero());

    // Create a rigid_var type variable
    const rigid_var = type_store.freshFromContent(.{ .rigid_var = ident_idx });

    // Create a box of rigid_var type variable
    const box_rigid_var = type_store.freshFromContent(.{ .structure = .{ .box = rigid_var } });

    // Convert to layout
    const box_layout_idx = try layout_store.addTypeVar(&type_store, box_rigid_var);

    // Verify the box layout
    const box_layout = layout_store.getLayout(box_layout_idx);
    try testing.expect(box_layout.* == .box);

    // Verify the element is host_opaque
    const elem_layout = layout_store.getLayout(box_layout.box);
    try testing.expect(elem_layout.* == .host_opaque);
}

test "addTypeVar - box of empty record compiles to box_zero_sized" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create an empty record type variable
    const empty_record_var = type_store.freshFromContent(.{ .structure = .empty_record });

    // Create a box of empty record type variable
    const box_empty_record_var = type_store.freshFromContent(.{ .structure = .{ .box = empty_record_var } });

    // Convert to layout
    const box_layout_idx = try layout_store.addTypeVar(&type_store, box_empty_record_var);

    // Verify the layout is box_zero_sized
    const box_layout = layout_store.getLayout(box_layout_idx);
    try testing.expect(box_layout.* == .box_zero_sized);
}

test "addTypeVar - list of empty tag union compiles to list_zero_sized" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create an empty tag union type variable
    const empty_tag_union_var = type_store.freshFromContent(.{ .structure = .empty_tag_union });

    // Create a list of empty tag union type variable
    const list_empty_tag_union_var = type_store.freshFromContent(.{ .structure = .{ .list = empty_tag_union_var } });

    // Convert to layout
    const list_layout_idx = try layout_store.addTypeVar(&type_store, list_empty_tag_union_var);

    // Verify the layout is list_zero_sized
    const list_layout = layout_store.getLayout(list_layout_idx);
    try testing.expect(list_layout.* == .list_zero_sized);
}

test "alignment - record alignment is max of field alignments" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create field identifiers
    const field1_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("field1"), base.Region.zero());
    const field2_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("field2"), base.Region.zero());
    const field3_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("field3"), base.Region.zero());

    // Create type variables for fields
    const u8_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u8 } } } });
    const u32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u32 } } } });
    const u64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u64 } } } });

    // Create record type { field1: U8, field2: U32, field3: U64 }
    const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = field1_ident, .var_ = u8_var },
        .{ .name = field2_ident, .var_ = u32_var },
        .{ .name = field3_ident, .var_ = u64_var },
    });

    const ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(&type_store, record_var);
    const record_layout = layout_store.getLayout(record_layout_idx);

    // Test alignment calculation
    // Record alignment should be the max of its field alignments
    // U8 has alignment 1, U32 has alignment 4, U64 has alignment 8
    // So the record should have alignment 8
    const usize_alignment = Alignment.fromBytes(@sizeOf(usize));
    const alignment = record_layout.alignment(usize_alignment);
    try testing.expectEqual(@as(u32, 8), alignment.toBytes());

    // Test with different field order - alignment should still be the same
    const fields2 = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = field3_ident, .var_ = u64_var },
        .{ .name = field1_ident, .var_ = u8_var },
        .{ .name = field2_ident, .var_ = u32_var },
    });

    const ext2 = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var2 = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields2, .ext = ext2 } } });

    const record_layout_idx2 = try layout_store.addTypeVar(&type_store, record_var2);
    const record_layout2 = layout_store.getLayout(record_layout_idx2);

    const alignment2 = record_layout2.alignment(usize_alignment);
    try testing.expectEqual(@as(u32, 8), alignment2.toBytes());
}

test "alignment - deeply nested record alignment (non-recursive)" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create field identifiers
    const inner_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("inner"), base.Region.zero());
    const middle_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("middle"), base.Region.zero());
    const outer_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("outer"), base.Region.zero());
    const data_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("data"), base.Region.zero());

    // Create a U64 field (alignment 8)
    const u64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u64 } } } });

    // Create innermost record: { data: U64 }
    const inner_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = data_ident, .var_ = u64_var },
    });
    const inner_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const inner_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = inner_fields, .ext = inner_ext } } });

    // Create middle record: { inner: { data: U64 } }
    const middle_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = inner_ident, .var_ = inner_record_var },
    });
    const middle_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const middle_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = middle_fields, .ext = middle_ext } } });

    // Create outer record: { middle: { inner: { data: U64 } } }
    const outer_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = middle_ident, .var_ = middle_record_var },
    });
    const outer_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const outer_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = outer_fields, .ext = outer_ext } } });

    // Create outermost record: { outer: { middle: { inner: { data: U64 } } } }
    const outermost_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = outer_ident, .var_ = outer_record_var },
    });
    const outermost_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const outermost_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = outermost_fields, .ext = outermost_ext } } });

    // Convert to layout
    const outermost_layout_idx = try layout_store.addTypeVar(&type_store, outermost_record_var);
    const outermost_layout = layout_store.getLayout(outermost_layout_idx);

    // Test alignment calculation
    // The deeply nested record should still have alignment 8 (from the U64 field)
    const usize_alignment = Alignment.fromBytes(@sizeOf(usize));
    const alignment = outermost_layout.alignment(usize_alignment);
    try testing.expectEqual(@as(u32, 8), alignment.toBytes());
}

test "addTypeVar - bare empty record returns error" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create an empty record type variable
    const empty_record_var = type_store.freshFromContent(.{ .structure = .empty_record });

    // Try to convert to layout - should fail
    const result = layout_store.addTypeVar(&type_store, empty_record_var);
    try testing.expectError(LayoutError.ZeroSizedType, result);
}

test "addTypeVar - bare empty tag union returns error" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create an empty tag union type variable
    const empty_tag_union_var = type_store.freshFromContent(.{ .structure = .empty_tag_union });

    // Try to convert to layout - should fail
    const result = layout_store.addTypeVar(&type_store, empty_tag_union_var);
    try testing.expectError(LayoutError.ZeroSizedType, result);
}

test "addTypeVar - list of box of empty record compiles to list_zero_sized" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create an empty record type variable
    const empty_record_var = type_store.freshFromContent(.{ .structure = .empty_record });

    // Create a box of empty record type variable
    const box_empty_record_var = type_store.freshFromContent(.{ .structure = .{ .box = empty_record_var } });

    // Create a list of box of empty record type variable
    const list_box_empty_record_var = type_store.freshFromContent(.{ .structure = .{ .list = box_empty_record_var } });

    // Convert to layout
    const list_layout_idx = try layout_store.addTypeVar(&type_store, list_box_empty_record_var);

    // Verify the layout is list_zero_sized
    const list_layout = layout_store.getLayout(list_layout_idx);
    try testing.expect(list_layout.* == .list_zero_sized);
}

test "addTypeVar - simple record" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create field types
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const u32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u32 } } } });

    // Create field identifiers
    const name_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("name"), base.Region.zero());
    const age_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("age"), base.Region.zero());

    // Create record type { name: str, age: u32 }
    const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = name_ident, .var_ = str_var },
        .{ .name = age_ident, .var_ = u32_var },
    });

    // Create record type
    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = empty_ext } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(&type_store, record_var);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.* == .record);

    // Verify the fields are sorted by alignment then name
    // Both str and u32 have same alignment on 64-bit systems (8 bytes for str pointer, 4 bytes for u32 but u32 comes first due to smaller alignment)
    // Actually str has alignment of usize (8 on 64-bit), u32 has alignment 4
    // So str should come first (higher alignment), then u32
    const field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(record_layout.record.fields.start), .end = @enumFromInt(record_layout.record.fields.start + record_layout.record.fields.count) });

    // First field: name (str) - higher alignment (8 bytes on 64-bit)
    const name_field = field_slice.get(0);
    try testing.expect(name_field.name == name_ident);
    const name_layout = layout_store.getLayout(name_field.layout);
    try testing.expect(name_layout.* == .str);

    // Second field: age (u32) - lower alignment (4 bytes)
    const age_field = field_slice.get(1);
    try testing.expect(age_field.name == age_ident);
    const age_layout = layout_store.getLayout(age_field.layout);
    try testing.expect(age_layout.* == .int);
    try testing.expect(age_layout.int == .u32);

    // Only 2 fields
    try testing.expectEqual(@as(usize, 2), field_slice.len);
}

test "record size calculation" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Test record with multiple fields requiring padding
    // { a: u8, b: u32, c: u8, d: u64 }
    const u8_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u8 } } } });
    const u32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u32 } } } });
    const u64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u64 } } } });

    const a_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("a"), base.Region.zero());
    const b_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("b"), base.Region.zero());
    const c_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("c"), base.Region.zero());
    const d_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("d"), base.Region.zero());

    const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = a_ident, .var_ = u8_var },
        .{ .name = b_ident, .var_ = u32_var },
        .{ .name = c_ident, .var_ = u8_var },
        .{ .name = d_ident, .var_ = u64_var },
    });

    const ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(&type_store, record_var);
    const record_layout = layout_store.getLayout(record_layout_idx);

    try testing.expect(record_layout.* == .record);

    // After sorting by alignment then name:
    // d: u64 (8 bytes) at offset 0
    // b: u32 (4 bytes) at offset 8
    // a: u8 (1 byte) at offset 12
    // c: u8 (1 byte) at offset 13
    // Total: 14 bytes, but aligned to 8 bytes = 16 bytes
    try testing.expectEqual(@as(u32, 16), record_layout.size(@sizeOf(usize)));
    const usize_alignment = Alignment.fromBytes(@sizeOf(usize));
    try testing.expectEqual(@as(u32, 8), record_layout.alignment(usize_alignment).toBytes());
}

test "addTypeVar - nested record" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create inner record type { x: i32, y: i32 }
    const i32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i32 } } } });
    const x_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("x"), base.Region.zero());
    const y_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("y"), base.Region.zero());

    const point_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = i32_var },
        .{ .name = y_ident, .var_ = i32_var },
    });

    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const point_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = point_fields, .ext = empty_ext } } });

    // Create outer record type { name: Str, position: { x: i32, y: i32 } }
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const name_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("name"), base.Region.zero());
    const position_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("position"), base.Region.zero());

    const player_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = name_ident, .var_ = str_var },
        .{ .name = position_ident, .var_ = point_var },
    });

    const player_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = player_fields, .ext = empty_ext } } });

    // Convert to layout
    const player_layout_idx = try layout_store.addTypeVar(&type_store, player_var);

    // Verify the outer layout
    const player_layout = layout_store.getLayout(player_layout_idx);
    try testing.expect(player_layout.* == .record);

    // Verify the outer fields
    const outer_field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(player_layout.record.fields.start), .end = @enumFromInt(player_layout.record.fields.start + player_layout.record.fields.count) });

    // First field: name (str)
    const name_field = outer_field_slice.get(0);
    try testing.expect(name_field.name == name_ident);
    const name_layout = layout_store.getLayout(name_field.layout);
    try testing.expect(name_layout.* == .str);

    // Second field: position (record)
    const position_field = outer_field_slice.get(1);
    try testing.expect(position_field.name == position_ident);
    const position_layout = layout_store.getLayout(position_field.layout);
    try testing.expect(position_layout.* == .record);

    // Exactly 2 outer fields
    try testing.expectEqual(@as(usize, 2), outer_field_slice.len);

    // Verify the inner record fields
    const inner_field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(position_layout.record.fields.start), .end = @enumFromInt(position_layout.record.fields.start + position_layout.record.fields.count) });

    // Inner field x (i32)
    const x_field = inner_field_slice.get(0);
    try testing.expect(x_field.name == x_ident);
    const x_layout = layout_store.getLayout(x_field.layout);
    try testing.expect(x_layout.* == .int);
    try testing.expect(x_layout.int == .i32);

    // Inner field y (i32)
    const y_field = inner_field_slice.get(1);
    try testing.expect(y_field.name == y_ident);
    const y_layout = layout_store.getLayout(y_field.layout);
    try testing.expect(y_layout.* == .int);
    try testing.expect(y_layout.int == .i32);

    // Exactly 2 inner fields
    try testing.expectEqual(@as(usize, 2), inner_field_slice.len);
}

test "addTypeVar - list of records" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create record type { id: u64, active: bool }
    const u64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u64 } } } });
    // For bool, we'll use u8 as a placeholder
    const bool_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u8 } } } });
    const id_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("id"), base.Region.zero());
    const active_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("active"), base.Region.zero());

    const record_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = id_ident, .var_ = u64_var },
        .{ .name = active_ident, .var_ = bool_var },
    });

    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = record_fields, .ext = empty_ext } } });

    // Create list of records
    const list_record_var = type_store.freshFromContent(.{ .structure = .{ .list = record_var } });

    // Convert to layout
    const list_layout_idx = try layout_store.addTypeVar(&type_store, list_record_var);

    // Verify the list layout
    const list_layout = layout_store.getLayout(list_layout_idx);
    try testing.expect(list_layout.* == .list);

    // Verify the record element
    const record_layout = layout_store.getLayout(list_layout.list);
    try testing.expect(record_layout.* == .record);

    // Verify the record fields
    const field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(record_layout.record.fields.start), .end = @enumFromInt(record_layout.record.fields.start + record_layout.record.fields.count) });

    // First field: id (u64)
    const id_field = field_slice.get(0);
    try testing.expect(id_field.name == id_ident);
    const id_layout = layout_store.getLayout(id_field.layout);
    try testing.expect(id_layout.* == .int);
    try testing.expect(id_layout.int == .u64);

    // The bool field is actually a u8
    const active_field = field_slice.get(1);
    try testing.expect(active_field.name == active_ident);
    const active_layout = layout_store.getLayout(active_field.layout);
    try testing.expect(active_layout.* == .int);
    try testing.expect(active_layout.int == .u8);

    // Exactly 2 fields
    try testing.expectEqual(@as(usize, 2), field_slice.len);
}

test "addTypeVar - record with extension" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create extension record { y: i32, z: f64 }
    const i32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i32 } } } });
    const f64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .frac = .{ .exact = .f64 } } } });
    const y_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("y"), base.Region.zero());
    const z_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("z"), base.Region.zero());

    const ext_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = y_ident, .var_ = i32_var },
        .{ .name = z_ident, .var_ = f64_var },
    });

    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const ext_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = ext_fields, .ext = empty_ext } } });

    // Create main record { x: str } extending the above
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const x_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("x"), base.Region.zero());

    const main_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = str_var },
    });

    const main_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = main_fields, .ext = ext_record_var } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(&type_store, main_record_var);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.* == .record);

    // Verify we have all 3 fields (x from main, y and z from extension)
    const field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(record_layout.record.fields.start), .end = @enumFromInt(record_layout.record.fields.start + record_layout.record.fields.count) });
    try testing.expectEqual(@as(usize, 3), field_slice.len);

    // Fields are sorted by alignment (descending) then by name (ascending)
    // str and f64 are 8-byte aligned, i32 is 4-byte aligned
    // So order should be: x (str, 8-byte), z (f64, 8-byte), y (i32, 4-byte)

    // Field x (str)
    const x_field = field_slice.get(0);
    try testing.expect(x_field.name == x_ident);
    const x_layout = layout_store.getLayout(x_field.layout);
    try testing.expect(x_layout.* == .str);

    // Field z (f64) - comes before y due to alignment
    const z_field = field_slice.get(1);
    try testing.expect(z_field.name == z_ident);
    const z_layout = layout_store.getLayout(z_field.layout);
    try testing.expect(z_layout.* == .frac);
    try testing.expect(z_layout.frac == .f64);

    // Field y (i32) - comes last due to smaller alignment
    const y_field = field_slice.get(2);
    try testing.expect(y_field.name == y_ident);
    const y_layout = layout_store.getLayout(y_field.layout);
    try testing.expect(y_layout.* == .int);
    try testing.expect(y_layout.int == .i32);

    // Exactly 3 fields
    try testing.expectEqual(@as(usize, 3), field_slice.len);
}

test "addTypeVar - record with duplicate field in extension (matching types)" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create types
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const i32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i32 } } } });
    const x_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("x"), base.Region.zero());
    const y_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("y"), base.Region.zero());

    // Create extension record { x: str, y: i32 }
    const ext_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = str_var },
        .{ .name = y_ident, .var_ = i32_var },
    });

    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const ext_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = ext_fields, .ext = empty_ext } } });

    // Create main record { x: str } extending the above (x appears in both with same type)
    const main_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = str_var },
    });

    const main_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = main_fields, .ext = ext_record_var } } });

    // Convert to layout - should succeed since types match
    const record_layout_idx = try layout_store.addTypeVar(&type_store, main_record_var);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.* == .record);

    // Verify we have 3 fields (x appears twice - from main and extension, plus y from extension)
    // TODO: Field deduplication should happen at the type-checking level, not in layout generation
    const field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(record_layout.record.fields.start), .end = @enumFromInt(record_layout.record.fields.start + record_layout.record.fields.count) });

    // Fields are sorted by alignment (descending) then by name (ascending)
    // All fields here have same type alignment, so sorted by name: x, x, y

    // Field x (str) - first occurrence
    const x_field1 = field_slice.get(0);
    try testing.expect(x_field1.name == x_ident);
    const x_layout1 = layout_store.getLayout(x_field1.layout);
    try testing.expect(x_layout1.* == .str);

    // Field x (str) - second occurrence
    const x_field2 = field_slice.get(1);
    try testing.expect(x_field2.name == x_ident);
    const x_layout2 = layout_store.getLayout(x_field2.layout);
    try testing.expect(x_layout2.* == .str);

    // Field y (i32)
    const y_field = field_slice.get(2);
    try testing.expect(y_field.name == y_ident);
    const y_layout = layout_store.getLayout(y_field.layout);
    try testing.expect(y_layout.* == .int);
    try testing.expect(y_layout.int == .i32);

    // Exactly 3 fields
    try testing.expectEqual(@as(usize, 3), field_slice.len);
}

test "addTypeVar - record with duplicate field in extension (mismatched types)" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create types
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const i32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i32 } } } });
    const x_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("x"), base.Region.zero());

    // Create extension record { x: i32 }
    const ext_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = i32_var },
    });

    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const ext_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = ext_fields, .ext = empty_ext } } });

    // Create main record { x: str } extending the above (x appears in both with different types)
    const main_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = str_var },
    });

    const main_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = main_fields, .ext = ext_record_var } } });

    // Convert to layout - currently succeeds with both fields present
    // TODO: Type checking should catch duplicate fields with mismatched types before layout generation
    const record_layout_idx = try layout_store.addTypeVar(&type_store, main_record_var);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.* == .record);

    // We get both fields even though they have the same name but different types
    const field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(record_layout.record.fields.start), .end = @enumFromInt(record_layout.record.fields.start + record_layout.record.fields.count) });

    // Fields are sorted by alignment (descending) then by name (ascending)
    // str is 8-byte aligned, i32 is 4-byte aligned
    // So order should be: x (str, 8-byte), x (i32, 4-byte)

    // Field x (str) - from main record
    const x_field1 = field_slice.get(0);
    try testing.expect(x_field1.name == x_ident);
    const x_layout1 = layout_store.getLayout(x_field1.layout);
    try testing.expect(x_layout1.* == .str);

    // Field x (i32) - from extension
    const x_field2 = field_slice.get(1);
    try testing.expect(x_field2.name == x_ident);
    const x_layout2 = layout_store.getLayout(x_field2.layout);
    try testing.expect(x_layout2.* == .int);
    try testing.expect(x_layout2.int == .i32);

    // Exactly 2 fields
    try testing.expectEqual(@as(usize, 2), field_slice.len);
}

test "addTypeVar - record with invalid extension type" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create a str type to use as invalid extension
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const x_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("x"), base.Region.zero());

    // Create main record { x: str } with str as extension (invalid)
    const main_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = str_var },
    });

    const main_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = main_fields, .ext = str_var } } });

    // Convert to layout - should fail due to invalid extension
    const result = layout_store.addTypeVar(&type_store, main_record_var);
    try testing.expectError(LayoutError.InvalidRecordExtension, result);
}

test "addTypeVar - record with chained extensions" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create types
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const i32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i32 } } } });
    const f64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .frac = .{ .exact = .f64 } } } });
    const u8_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u8 } } } });

    const w_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("w"), base.Region.zero());
    const x_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("x"), base.Region.zero());
    const y_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("y"), base.Region.zero());
    const z_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("z"), base.Region.zero());

    // Create innermost extension record { z: u8 }
    const inner_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = z_ident, .var_ = u8_var },
    });

    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const inner_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = inner_fields, .ext = empty_ext } } });

    // Create middle extension record { y: f64 } extending inner
    const middle_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = y_ident, .var_ = f64_var },
    });

    const middle_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = middle_fields, .ext = inner_record_var } } });

    // Create outermost record { w: str, x: i32 } extending middle
    const outer_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = w_ident, .var_ = str_var },
        .{ .name = x_ident, .var_ = i32_var },
    });

    const outer_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = outer_fields, .ext = middle_record_var } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(&type_store, outer_record_var);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.* == .record);

    // Verify we have all 4 fields from all levels
    const field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(record_layout.record.fields.start), .end = @enumFromInt(record_layout.record.fields.start + record_layout.record.fields.count) });

    // Fields are sorted by alignment (descending) then by name (ascending)
    // str and f64 are 8-byte aligned, i32 is 4-byte aligned, u8 is 1-byte aligned
    // So order should be: w (str, 8-byte), y (f64, 8-byte), x (i32, 4-byte), z (u8, 1-byte)

    // Field w (str)
    const w_field = field_slice.get(0);
    try testing.expect(w_field.name == w_ident);
    const w_layout = layout_store.getLayout(w_field.layout);
    try testing.expect(w_layout.* == .str);

    // Field y (f64) - comes before x due to alignment
    const y_field = field_slice.get(1);
    try testing.expect(y_field.name == y_ident);
    const y_layout = layout_store.getLayout(y_field.layout);
    try testing.expect(y_layout.* == .frac);
    try testing.expect(y_layout.frac == .f64);

    // Field x (i32)
    const x_field = field_slice.get(2);
    try testing.expect(x_field.name == x_ident);
    const x_layout = layout_store.getLayout(x_field.layout);
    try testing.expect(x_layout.* == .int);
    try testing.expect(x_layout.int == .i32);

    // Field z (u8)
    const z_field = field_slice.get(3);
    try testing.expect(z_field.name == z_ident);
    const z_layout = layout_store.getLayout(z_field.layout);
    try testing.expect(z_layout.* == .int);
    try testing.expect(z_layout.int == .u8);

    // Exactly 4 fields
    try testing.expectEqual(@as(usize, 4), field_slice.len);
}

test "addTypeVar - record with zero-sized fields dropped" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create types
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const empty_record_var = type_store.freshFromContent(.{ .structure = .empty_record });
    const i32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i32 } } } });

    const name_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("name"), base.Region.zero());
    const empty_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("empty"), base.Region.zero());
    const age_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("age"), base.Region.zero());

    // Create record { name: str, empty: {}, age: i32 }
    const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = name_ident, .var_ = str_var },
        .{ .name = empty_ident, .var_ = empty_record_var },
        .{ .name = age_ident, .var_ = i32_var },
    });

    const ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(&type_store, record_var);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.* == .record);

    // Verify we only have 2 fields (empty field should be dropped)
    const field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(record_layout.record.fields.start), .end = @enumFromInt(record_layout.record.fields.start + record_layout.record.fields.count) });

    // Debug: Check the actual field count
    const field_count = field_slice.len;
    try testing.expect(field_count == 2);

    // Field name (str)
    const name_field = field_slice.get(0);
    try testing.expect(name_field.name == name_ident);
    const name_layout = layout_store.getLayout(name_field.layout);
    try testing.expect(name_layout.* == .str);

    // Second field: age (i32)
    const age_field = field_slice.get(1);
    try testing.expect(age_field.name == age_ident);
    const age_layout = layout_store.getLayout(age_field.layout);
    try testing.expect(age_layout.* == .int);
    try testing.expect(age_layout.int == .i32);

    // Exactly 2 fields (empty field was dropped)
    try testing.expectEqual(@as(usize, 2), field_slice.len);
}

test "addTypeVar - record with all zero-sized fields becomes empty" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create types
    const empty_record_var = type_store.freshFromContent(.{ .structure = .empty_record });
    const empty_tag_union_var = type_store.freshFromContent(.{ .structure = .empty_tag_union });

    const field1_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("field1"), base.Region.zero());
    const field2_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("field2"), base.Region.zero());

    // Create record { field1: {}, field2: [] }
    const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = field1_ident, .var_ = empty_record_var },
        .{ .name = field2_ident, .var_ = empty_tag_union_var },
    });

    const ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Convert to layout - should fail because all fields are zero-sized
    const result = layout_store.addTypeVar(&type_store, record_var);
    try testing.expectError(LayoutError.ZeroSizedType, result);
}

test "addTypeVar - box of record with all zero-sized fields" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create types
    const empty_record_var = type_store.freshFromContent(.{ .structure = .empty_record });

    const field1_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("field1"), base.Region.zero());
    const field2_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("field2"), base.Region.zero());

    // Create record { field1: {}, field2: {} }
    const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = field1_ident, .var_ = empty_record_var },
        .{ .name = field2_ident, .var_ = empty_record_var },
    });

    const ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Create box of this record
    const box_record_var = type_store.freshFromContent(.{ .structure = .{ .box = record_var } });

    // Convert to layout - should become box_zero_sized
    const box_layout_idx = try layout_store.addTypeVar(&type_store, box_record_var);

    // Verify the layout is box_zero_sized
    const box_layout = layout_store.getLayout(box_layout_idx);
    try testing.expect(box_layout.* == .box_zero_sized);
}

test "addTypeVar - comprehensive nested record combinations" {
    // This tests:
    // 1. 1344 different combinations (64 + 256 + 1024) of nested record structures
    // 2. Proper dropping of of zero-sized fields at all nesting levels
    // 3. Proper layout generation for fields that aren't zero-sized

    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create field names we'll reuse
    const field_names = [_]Ident.Idx{
        type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("a"), base.Region.zero()),
        type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("b"), base.Region.zero()),
        type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("c"), base.Region.zero()),
        type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("d"), base.Region.zero()),
        type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("e"), base.Region.zero()),
        type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("f"), base.Region.zero()),
    };

    // Test all combinations
    var outer_field_count: usize = 1;
    while (outer_field_count <= 3) : (outer_field_count += 1) {
        // Generate all possible field type combinations for outer record
        var field_type_combo: usize = 0;
        const max_combo = std.math.pow(usize, 4, outer_field_count); // 4 possibilities per field

        while (field_type_combo < max_combo) : (field_type_combo += 1) {
            // Create a new type store and layout store for each test
            var test_type_store = types_store.Store.init(&module_env);
            defer test_type_store.deinit();

            var test_layout_store = Store.init(&module_env);
            defer test_layout_store.deinit();

            // Build outer record fields
            var outer_fields = std.ArrayList(types.RecordField).init(gpa);
            defer outer_fields.deinit();

            var expected_non_zero_fields: usize = 0;
            var expected_total_fields: usize = 0;

            var field_idx: usize = 0;
            while (field_idx < outer_field_count) : (field_idx += 1) {
                // Determine field type based on combination
                const field_type_idx = (field_type_combo / std.math.pow(usize, 4, field_idx)) % 4;

                const field_var = switch (field_type_idx) {
                    0 => blk: {
                        // Non-record: str
                        expected_non_zero_fields += 1;
                        expected_total_fields += 1;
                        break :blk test_type_store.freshFromContent(.{ .structure = .str });
                    },
                    1 => blk: {
                        // Empty record (0 fields)
                        expected_total_fields += 1;
                        // This field will be dropped as zero-sized
                        break :blk test_type_store.freshFromContent(.{ .structure = .empty_record });
                    },
                    2 => blk: {
                        // Record with 1-2 non-zero fields
                        expected_total_fields += 1;
                        const inner_field_count = 1 + (field_idx % 2); // 1 or 2 fields
                        var inner_fields = std.ArrayList(types.RecordField).init(gpa);
                        defer inner_fields.deinit();

                        var inner_idx: usize = 0;
                        while (inner_idx < inner_field_count) : (inner_idx += 1) {
                            const inner_var = test_type_store.freshFromContent(.{ .structure = .str });
                            try inner_fields.append(.{
                                .name = field_names[3 + inner_idx],
                                .var_ = inner_var,
                            });
                        }

                        expected_non_zero_fields += 1; // The nested record itself counts as 1
                        expected_total_fields += inner_field_count;

                        const inner_fields_slice = test_type_store.record_fields.appendSlice(test_type_store.env.gpa, inner_fields.items);
                        const empty_ext = test_type_store.freshFromContent(.{ .structure = .empty_record });
                        break :blk test_type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = inner_fields_slice, .ext = empty_ext } } });
                    },
                    3 => blk: {
                        // Record with mix of zero and non-zero fields
                        expected_total_fields += 1;
                        var inner_fields = std.ArrayList(types.RecordField).init(gpa);
                        defer inner_fields.deinit();

                        // Add one empty record field (will be dropped)
                        const empty_record_var = test_type_store.freshFromContent(.{ .structure = .empty_record });
                        try inner_fields.append(.{
                            .name = field_names[3],
                            .var_ = empty_record_var,
                        });
                        expected_total_fields += 1;

                        // Add one str field (will be kept)
                        const str_var = test_type_store.freshFromContent(.{ .structure = .str });
                        try inner_fields.append(.{
                            .name = field_names[4],
                            .var_ = str_var,
                        });
                        expected_total_fields += 1;

                        // This nested record will have 1 non-zero field (the str field) after dropping zero-sized ones
                        expected_non_zero_fields += 1;

                        const inner_fields_slice = test_type_store.record_fields.appendSlice(test_type_store.env.gpa, inner_fields.items);
                        const empty_ext = test_type_store.freshFromContent(.{ .structure = .empty_record });
                        break :blk test_type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = inner_fields_slice, .ext = empty_ext } } });
                    },
                    else => unreachable,
                };

                try outer_fields.append(.{
                    .name = field_names[field_idx],
                    .var_ = field_var,
                });
            }

            // Create outer record
            const outer_fields_slice = test_type_store.record_fields.appendSlice(test_type_store.env.gpa, outer_fields.items);
            const empty_ext = test_type_store.freshFromContent(.{ .structure = .empty_record });
            const outer_record_var = test_type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = outer_fields_slice, .ext = empty_ext } } });

            // Convert to layout
            const result = test_layout_store.addTypeVar(&test_type_store, outer_record_var) catch |err| {
                if (err == LayoutError.ZeroSizedType) {
                    // This is expected if all fields were zero-sized
                    try testing.expect(expected_non_zero_fields == 0);
                    continue;
                }
                return err;
            };

            // Verify the result
            const result_layout = test_layout_store.getLayout(result);

            if (expected_non_zero_fields == 0) {
                // Should have returned an error, not reached here
                try testing.expect(false);
            } else {
                try testing.expect(result_layout.* == .record);

                // Count actual non-zero fields in the result
                const field_slice = test_layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(result_layout.record.fields.start), .end = @enumFromInt(result_layout.record.fields.start + result_layout.record.fields.count) });
                const actual_field_count = field_slice.len;

                // Verify each field has a valid layout
                for (0..field_slice.len) |i| {
                    const field = field_slice.get(i);
                    const field_layout = test_layout_store.getLayout(field.layout);
                    switch (field_layout.*) {
                        .str => {}, // Valid non-zero field
                        .record => |rec| {
                            // Verify nested record has fields
                            const nested_slice = test_layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(rec.fields.start), .end = @enumFromInt(rec.fields.start + rec.fields.count) });
                            try testing.expect(nested_slice.len > 0);
                        },
                        else => {
                            // Unexpected layout type
                            try testing.expect(false);
                        },
                    }
                }

                try testing.expect(actual_field_count == expected_non_zero_fields);
            }
        }
    }
}

test "addTypeVar - nested record with inner record having all zero-sized fields" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create inner record with only zero-sized fields
    const empty_record_var = type_store.freshFromContent(.{ .structure = .empty_record });
    const a_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("a"), base.Region.zero());
    const b_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("b"), base.Region.zero());

    // Create inner record
    const inner_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = a_ident, .var_ = empty_record_var },
        .{ .name = b_ident, .var_ = empty_record_var },
    });

    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const inner_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = inner_fields, .ext = empty_ext } } });

    // Create outer record { name: str, data: inner_record }
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const name_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("name"), base.Region.zero());
    const data_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("data"), base.Region.zero());

    const outer_fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = name_ident, .var_ = str_var },
        .{ .name = data_ident, .var_ = inner_record_var },
    });

    const outer_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = outer_fields, .ext = empty_ext } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(&type_store, outer_record_var);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.* == .record);

    // Verify we only have 1 field (data field should be dropped because inner record is empty)
    const field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(record_layout.record.fields.start), .end = @enumFromInt(record_layout.record.fields.start + record_layout.record.fields.count) });

    // Field name (str)
    const name_field = field_slice.get(0);
    try testing.expect(name_field.name == name_ident);
    const name_layout = layout_store.getLayout(name_field.layout);
    try testing.expect(name_layout.* == .str);

    // Only 1 field (data field was dropped because the inner record was empty)
    try testing.expectEqual(@as(usize, 1), field_slice.len);
}

test "addTypeVar - list of record with all zero-sized fields" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create empty record type
    const empty_record_var = type_store.freshFromContent(.{ .structure = .empty_record });
    const field_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("field"), base.Region.zero());

    // Create record { field: {} }
    const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = field_ident, .var_ = empty_record_var },
    });

    const ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Create list of that record
    const list_var = type_store.freshFromContent(.{ .structure = .{ .list = record_var } });

    // Convert to layout - should be list_zero_sized
    const list_layout_idx = try layout_store.addTypeVar(&type_store, list_var);
    const list_layout = layout_store.getLayout(list_layout_idx);

    try testing.expect(list_layout.* == .list_zero_sized);
}

test "alignment - record with log2 alignment representation" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Test 1: Record with U8 field (alignment 1, log2 = 0)
    {
        const u8_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u8 } } } });
        const field_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("field"), base.Region.zero());
        const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
            .{ .name = field_ident, .var_ = u8_var },
        });
        const ext = type_store.freshFromContent(.{ .structure = .empty_record });
        const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

        const record_layout_idx = try layout_store.addTypeVar(&type_store, record_var);
        const record_layout = layout_store.getLayout(record_layout_idx);

        try testing.expect(record_layout.* == .record);
        try testing.expectEqual(@as(u4, 0), record_layout.record.alignment.log2_value); // log2(1) = 0
        const usize_alignment = Alignment.fromBytes(@sizeOf(usize));
        try testing.expectEqual(@as(u32, 1), record_layout.alignment(usize_alignment).toBytes());
        try testing.expectEqual(@as(u32, 1), record_layout.size(@sizeOf(usize))); // size = 1 byte
    }

    // Test 2: Record with U32 field (alignment 4, log2 = 2)
    {
        const u32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u32 } } } });
        const field_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("field"), base.Region.zero());
        const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
            .{ .name = field_ident, .var_ = u32_var },
        });
        const ext = type_store.freshFromContent(.{ .structure = .empty_record });
        const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

        const record_layout_idx = try layout_store.addTypeVar(&type_store, record_var);
        const record_layout = layout_store.getLayout(record_layout_idx);

        try testing.expect(record_layout.* == .record);
        try testing.expectEqual(@as(u4, 2), record_layout.record.alignment.log2_value); // log2(4) = 2
        const usize_alignment2 = Alignment.fromBytes(@sizeOf(usize));
        try testing.expectEqual(@as(u32, 4), record_layout.alignment(usize_alignment2).toBytes());
        try testing.expectEqual(@as(u32, 4), record_layout.size(@sizeOf(usize))); // size = 4 bytes
    }

    // Test 3: Record with U64 field (alignment 8, log2 = 3)
    {
        const u64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u64 } } } });
        const field_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("field"), base.Region.zero());
        const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
            .{ .name = field_ident, .var_ = u64_var },
        });
        const ext = type_store.freshFromContent(.{ .structure = .empty_record });
        const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

        const record_layout_idx = try layout_store.addTypeVar(&type_store, record_var);
        const record_layout = layout_store.getLayout(record_layout_idx);

        try testing.expect(record_layout.* == .record);
        try testing.expectEqual(@as(u4, 3), record_layout.record.alignment.log2_value); // log2(8) = 3
        const usize_alignment3 = Alignment.fromBytes(@sizeOf(usize));
        try testing.expectEqual(@as(u32, 8), record_layout.alignment(usize_alignment3).toBytes());
        try testing.expectEqual(@as(u32, 8), record_layout.size(@sizeOf(usize))); // size = 8 bytes
    }

    // Test 4: Record with mixed fields - should use max alignment
    {
        const u8_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u8 } } } });
        const u64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u64 } } } });
        const field1_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("small"), base.Region.zero());
        const field2_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("large"), base.Region.zero());
        const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
            .{ .name = field1_ident, .var_ = u8_var },
            .{ .name = field2_ident, .var_ = u64_var },
        });
        const ext = type_store.freshFromContent(.{ .structure = .empty_record });
        const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

        const record_layout_idx = try layout_store.addTypeVar(&type_store, record_var);
        const record_layout = layout_store.getLayout(record_layout_idx);

        try testing.expect(record_layout.* == .record);
        try testing.expectEqual(@as(u4, 3), record_layout.record.alignment.log2_value); // max(log2(1), log2(8)) = 3
        const usize_alignment4 = Alignment.fromBytes(@sizeOf(usize));
        try testing.expectEqual(@as(u32, 8), record_layout.alignment(usize_alignment4).toBytes());
        // After sorting: u64 (8 bytes) at offset 0, u8 (1 byte) at offset 8, total size 16 (aligned to 8)
        try testing.expectEqual(@as(u32, 16), record_layout.size(@sizeOf(usize)));
    }
}

test "record fields sorted by alignment then name" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create types with different alignments
    const u8_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u8 } } } });
    const u32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u32 } } } });
    const u64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u64 } } } });

    // Create field names that would sort differently alphabetically
    const a_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("a"), base.Region.zero());
    const b_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("b"), base.Region.zero());
    const c_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("c"), base.Region.zero());
    const d_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("d"), base.Region.zero());

    // Create record with fields in a specific order to test sorting
    // { a: u32, b: u64, c: u8, d: u64 }
    const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = a_ident, .var_ = u32_var }, // alignment 4
        .{ .name = b_ident, .var_ = u64_var }, // alignment 8
        .{ .name = c_ident, .var_ = u8_var }, // alignment 1
        .{ .name = d_ident, .var_ = u64_var }, // alignment 8
    });

    const ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(&type_store, record_var);
    const record_layout = layout_store.getLayout(record_layout_idx);

    try testing.expect(record_layout.* == .record);

    // Verify fields are sorted by alignment (descending) then by name (ascending)
    // Expected order: b (u64, align 8), d (u64, align 8), a (u32, align 4), c (u8, align 1)
    const field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(record_layout.record.fields.start), .end = @enumFromInt(record_layout.record.fields.start + record_layout.record.fields.count) });

    // First field: b (u64, alignment 8)
    const field1 = field_slice.get(0);
    try testing.expect(field1.name == b_ident);
    const layout1 = layout_store.getLayout(field1.layout);
    try testing.expect(layout1.* == .int);
    try testing.expect(layout1.int == .u64);

    // Second field: d (u64, alignment 8)
    const field2 = field_slice.get(1);
    try testing.expect(field2.name == d_ident);
    const layout2 = layout_store.getLayout(field2.layout);
    try testing.expect(layout2.* == .int);
    try testing.expect(layout2.int == .u64);

    // Third field: a (u32, alignment 4)
    const field3 = field_slice.get(2);
    try testing.expect(field3.name == a_ident);
    const layout3 = layout_store.getLayout(field3.layout);
    try testing.expect(layout3.* == .int);
    try testing.expect(layout3.int == .u32);

    // Fourth field: c (u8, alignment 1)
    const field4 = field_slice.get(3);
    try testing.expect(field4.name == c_ident);
    const layout4 = layout_store.getLayout(field4.layout);
    try testing.expect(layout4.* == .int);
    try testing.expect(layout4.int == .u8);

    // Exactly 4 fields
    try testing.expectEqual(@as(usize, 4), field_slice.len);
}

test "record fields with same alignment sorted by name" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create types with same alignment
    const i32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i32 } } } });
    const u32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u32 } } } });
    const f32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .frac = .{ .exact = .f32 } } } });

    // Create field names that are not in alphabetical order
    const zebra_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("zebra"), base.Region.zero());
    const apple_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("apple"), base.Region.zero());
    const banana_ident = type_store.env.idents.insert(type_store.env.gpa, base.Ident.for_text("banana"), base.Region.zero());

    // Create record with fields that all have alignment 4
    // { zebra: i32, apple: u32, banana: f32 }
    const fields = type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = zebra_ident, .var_ = i32_var },
        .{ .name = apple_ident, .var_ = u32_var },
        .{ .name = banana_ident, .var_ = f32_var },
    });

    const ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(&type_store, record_var);
    const record_layout = layout_store.getLayout(record_layout_idx);

    try testing.expect(record_layout.* == .record);

    // Verify fields are sorted alphabetically since they all have the same alignment
    // Expected order: apple, banana, zebra
    const field_slice = layout_store.record_fields.rangeToSlice(.{ .start = @enumFromInt(record_layout.record.fields.start), .end = @enumFromInt(record_layout.record.fields.start + record_layout.record.fields.count) });

    // First field: apple
    const field1 = field_slice.get(0);
    try testing.expect(field1.name == apple_ident);
    const layout1 = layout_store.getLayout(field1.layout);
    try testing.expect(layout1.* == .int);
    try testing.expect(layout1.int == .u32);

    // Second field: banana
    const field2 = field_slice.get(1);
    try testing.expect(field2.name == banana_ident);
    const layout2 = layout_store.getLayout(field2.layout);
    try testing.expect(layout2.* == .frac);
    try testing.expect(layout2.frac == .f32);

    // Third field: zebra
    const field3 = field_slice.get(2);
    try testing.expect(field3.name == zebra_ident);
    const layout3 = layout_store.getLayout(field3.layout);
    try testing.expect(layout3.* == .int);
    try testing.expect(layout3.int == .i32);

    // Exactly 3 fields
    try testing.expectEqual(@as(usize, 3), field_slice.len);
}

test "Layout type size in memory" {
    const testing = std.testing;

    // Print the size of the Layout tagged union
    const layout_size = @sizeOf(Layout);
    std.debug.print("\n=== Layout Memory Size ===\n", .{});
    std.debug.print("Layout tagged union size: {} bytes\n", .{layout_size});

    // Print sizes of key components
    std.debug.print("Individual component sizes:\n", .{});
    std.debug.print("  - Idx: {} bytes\n", .{@sizeOf(Idx)});
    std.debug.print("  - Record: {} bytes\n", .{@sizeOf(layout.Record)});
    std.debug.print("  - NonEmptyRange: {} bytes\n", .{@sizeOf(collections.NonEmptyRange)});
    std.debug.print("  - Alignment: {} bytes\n", .{@sizeOf(layout.Alignment)});
    std.debug.print("  - Size: {} bytes\n", .{@sizeOf(layout.Size)});
    std.debug.print("========================\n\n", .{});

    // The Layout should be reasonably small since it's used frequently
    try testing.expect(layout_size <= 24); // Reasonable upper bound for a tagged union
}
