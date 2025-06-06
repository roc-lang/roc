//! Stores Layout values by index.

const std = @import("std");
const types = @import("../types/types.zig");
const types_store = @import("../types/store.zig");
const layout_ = @import("./layout.zig");
const base = @import("../base.zig");
const target = @import("../base/target.zig");
const collections = @import("../collections.zig");
const Ident = @import("../base/Ident.zig");
const work = @import("./work.zig");

const Var = types.Var;
const Layout = layout_.Layout;
const Idx = layout_.Idx;
const RecordField = layout_.RecordField;
const RecordData = layout_.RecordData;
const RecordIdx = layout_.RecordIdx;
const Work = work.Work;
const exitOnOom = collections.utils.exitOnOom;

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

    env: *base.ModuleEnv,
    layouts: collections.SafeList(Layout),
    tuple_elems: collections.SafeList(Idx),
    record_fields: RecordField.SafeMultiList,
    record_data: collections.SafeList(RecordData),

    // Cache to avoid duplicate work
    // TODO make this be a flat array with `!0` values indicating emptiness,
    // initialized to the same length as the Subs equivalent.
    layouts_by_var: std.AutoHashMapUnmanaged(Var, Idx),

    // Reusable work stack for addTypeVar (so it can be stack-safe instead of recursing)
    work: work.Work,

    pub fn init(env: *base.ModuleEnv) Self {
        return .{
            .env = env,
            .layouts = collections.SafeList(Layout){},
            .tuple_elems = collections.SafeList(Idx).initCapacity(env.gpa, 512),
            .record_fields = RecordField.SafeMultiList.initCapacity(env.gpa, 256),
            .record_data = collections.SafeList(RecordData).initCapacity(env.gpa, 256),
            .layouts_by_var = std.AutoHashMapUnmanaged(Var, Idx){},
            .work = Work.initCapacity(env.gpa, 32) catch |err| exitOnOom(err),
        };
    }

    pub fn deinit(self: *Self) void {
        self.layouts.deinit(self.env.gpa);
        self.tuple_elems.deinit(self.env.gpa);
        self.record_fields.deinit(self.env.gpa);
        self.record_data.deinit(self.env.gpa);
        self.layouts_by_var.deinit(self.env.gpa);
        self.work.deinit(self.env.gpa);
    }

    fn insertLayout(self: *Self, layout: Layout) std.mem.Allocator.Error!Idx {
        const safe_idx = self.layouts.append(self.env.gpa, layout);
        return Idx{
            .int_idx = @intCast(@intFromEnum(safe_idx)),
        };
    }

    pub fn getLayout(self: *Self, idx: Idx) *const Layout {
        return self.layouts.get(@enumFromInt(idx.int_idx));
    }

    pub fn getRecordData(self: *const Self, idx: RecordIdx) *const RecordData {
        return self.record_data.get(@enumFromInt(idx.int_idx));
    }

    fn targetUsize(self: *const Self) target.TargetUsize {
        return self.env.target.target_usize;
    }

    /// Get the size in bytes of a layout, given the store's target usize.
    pub fn layoutSize(self: *const Self, layout: Layout) u32 {
        const target_usize = self.targetUsize();
        return switch (layout.tag) {
            .int => layout.data.int.size(),
            .frac => layout.data.frac.size(),
            .host_opaque => target_usize.size(), // a void* pointer
            .box, .box_of_zst => target_usize.size(), // a Box is just a pointer to refcounted memory
            .str, .list, .list_of_zst => target_usize.size(), // TODO: get this from RocStr.zig and RocList.zig
            .record => self.record_data.get(@enumFromInt(layout.data.record.idx.int_idx)).size,
        };
    }

    /// Add the record's fields to self.pending_record_fields,
    /// then add the record's extension fields too (recursively).
    fn gatherRecordFields(
        self: *Self,
        var_store: *const types_store.Store,
        record_type: types.Record,
    ) (LayoutError || std.mem.Allocator.Error)!usize {
        var num_fields = record_type.fields.len();

        const field_slice = var_store.getRecordFieldsSlice(record_type.fields);
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
                            const ext_field_slice = var_store.getRecordFieldsSlice(ext_record.fields);
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
                const lhs_str = ctx.env.idents.getText(lhs.name);
                const rhs_str = ctx.env.idents.getText(rhs.name);
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
            _ = self.record_fields.append(self.env.gpa, sorted_field);
        }

        // Calculate max alignment and total size of all fields
        var max_alignment = std.mem.Alignment.@"1";
        var current_offset: u32 = 0;
        var field_idx: u32 = 0;

        while (field_idx < temp_fields.items.len) : (field_idx += 1) {
            const temp_field = temp_fields.items[field_idx];
            const field_layout = self.getLayout(temp_field.layout);

            const field_alignment = field_layout.alignment(target_usize);
            const field_size = self.layoutSize(field_layout.*);

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
        const fields_range = collections.NonEmptyRange.init(@intCast(fields_start), @intCast(num_resolved_fields)) catch unreachable;

        // Store the record data
        const record_idx = RecordIdx{ .int_idx = @intCast(self.record_data.len()) };
        _ = self.record_data.append(self.env.gpa, RecordData{
            .size = total_size,
            .fields = fields_range,
        });

        // Remove only this record's resolved fields
        self.work.resolved_record_fields.shrinkRetainingCapacity(updated_record.resolved_fields_start);

        return Layout.record(max_alignment, record_idx);
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
        var_store: *const types_store.Store,
        unresolved_var: Var,
    ) (LayoutError || std.mem.Allocator.Error)!Idx {
        var current = var_store.resolveVar(unresolved_var);

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
                std.debug.panic("Layout computation exceeded iteration limit - possible infinite loop\n", .{});
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
                        current = var_store.resolveVar(elem_var);
                        continue;
                    },
                    .list => |elem_var| {
                        try self.work.pending_containers.append(self.env.gpa, .{
                            .var_ = current.var_,
                            .container = .list,
                        });
                        // Push a pending List container and "recurse" on the elem type
                        current = var_store.resolveVar(elem_var);
                        continue;
                    },
                    .custom_type => |custom_type| {
                        // TODO special-case the builtin Num type here.
                        // If we have one of those, then convert it to a Num layout,
                        // or to a runtime error if it's an invalid elem type.

                        // From a layout perspective, custom types are identical to type aliases:
                        // all we care about is what's inside, so just unroll it.
                        current = var_store.resolveVar(custom_type.backing_var);
                        continue;
                    },
                    .num => |num| switch (num) {
                        .int => |int| switch (int) {
                            .exact => |precision| Layout.int(precision),
                            // For Int(a), use the default precision for an Int.
                            .flex_var => Layout.int(types.Num.Int.Precision.default),
                        },
                        .frac => |frac| switch (frac) {
                            .exact => |precision| Layout.frac(precision),
                            // For Frac(a), use the default precision for a Frac.
                            .flex_var => Layout.frac(types.Num.Frac.Precision.default),
                        },
                        // Num(a) defaults to Int(a), so use the default precision for an Int.
                        .flex_var => Layout.int(types.Num.Int.Precision.default),
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
                        const num_fields = try self.gatherRecordFields(var_store, record_type);

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

                        current = var_store.resolveVar(field.var_);
                        continue;
                    },
                    .tag_union => |tag_union| {
                        // TODO
                        _ = tag_union;
                        @panic("TODO: tag_union layout");
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
                    if (self.work.pending_containers.len > 0) {
                        const pending_item = self.work.pending_containers.get(self.work.pending_containers.len - 1);
                        if (pending_item.container == .box or pending_item.container == .list) {
                            break :blk Layout.hostOpaque();
                        }
                    }

                    std.debug.assert(false);
                    return LayoutError.BugUnboxedFlexVar;
                },
                .rigid_var => |_| blk: {
                    // Rigid vars can only be sent to the host if boxed.
                    if (self.work.pending_containers.len > 0) {
                        const pending_item = self.work.pending_containers.get(self.work.pending_containers.len - 1);
                        if (pending_item.container == .box or pending_item.container == .list) {
                            break :blk Layout.hostOpaque();
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
                            current = var_store.resolveVar(next_field.var_);
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

            // Since there are no pending containers remaining, there shouldn't be any pending record fields either.
            std.debug.assert(self.work.pending_record_fields.len == 0);

            // No more pending containers; we're done!
            return layout_idx;
        }
    }
};
