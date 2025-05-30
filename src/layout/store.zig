//! The store of solved types
//! Contains both Slot & Descriptor stores

const std = @import("std");
const types = @import("../types/types.zig");
const types_store = @import("../types/store.zig");
const layout = @import("./layout.zig");
const base = @import("../base.zig");
const collections = @import("../collections.zig");
const Ident = @import("../base/Ident.zig");
const record = @import("./record.zig");
const work = @import("./work.zig");

const Var = types.Var;
const Layout = layout.Layout;
const Idx = layout.Idx;
const Work = work.Work;

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
    ident_store: *const Ident.Store,

    // Layout storage
    layouts: std.ArrayListUnmanaged(Layout),

    // Lists for parameterized layouts
    tuple_elems: collections.SafeList(Idx),
    record_fields: RecordFieldSafeMultiList,

    // Cache to avoid duplicate work
    var_to_layout: std.AutoHashMapUnmanaged(Var, Idx),

    // Reusable work stack for addTypeVar
    work: work.Work,

    pub fn init(env: *base.ModuleEnv, ident_store: *const Ident.Store) std.mem.Allocator.Error!Self {
        return .{
            .env = env,
            .ident_store = ident_store,
            .layouts = std.ArrayListUnmanaged(Layout){},
            .tuple_elems = try collections.SafeList(Idx).initCapacity(env.gpa, 512),
            .record_fields = try RecordFieldSafeMultiList.initCapacity(env.gpa, 256),

            .var_to_layout = std.AutoHashMapUnmanaged(Var, Idx),
            .work = Work.initCapacity(env.gpa, 32),
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
        const idx = self.layouts.items.len;
        try self.layouts.append(self.env.gpa, layout_);
        return Idx{
            .attributes = .{ ._padding = 0 },
            .idx = @intCast(idx),
        };
    }

    fn getLayout(self: *Self, idx: Idx) *Layout {
        return &self.layouts.items[idx.idx];
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

        while (true) {
            const layout_idx = switch (current.desc.content) {
                .structure => |flat_type| switch (flat_type) {
                    .str => try self.insertLayout(.str),
                    .box => |elem_var| {
                        try self.work.pending_containers.append(self.env.gpa, .box);
                        // Push a pending Box container and "recurse" on the elem type
                        current = var_store.resolveVar(elem_var).var_;
                        continue;
                    },
                    .list => |elem_var| {
                        try self.work.pending_containers.append(self.env.gpa, .list);
                        // Push a pending Box container and "recurse" on the elem type
                        current = var_store.resolveVar(elem_var).var_;
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
                        const field_iter = var_store.iterMulti(record_type.fields);
                        while (field_iter.next()) |field| {
                            // TODO is it possible that here we're encountering record fields with names
                            // already in the list? Would type-checking have already deduped them?
                            // We would certainly rather not spend time doing hashmap things if we can avoid it here.
                            try self.work.pending_record_fields.append(field);
                        }

                        var current_ext = record_type.ext;
                        while (true) {
                            switch (var_store.resolveVar(current_ext).desc.content) {
                                .structure => |ext_flat_type| switch (ext_flat_type) {
                                    .empty_record => break,
                                    .record => |ext_record| {
                                        if (ext_record.fields.len() > 0) {
                                            num_fields += ext_record.fields.len();
                                            const ext_field_iter = var_store.iterMulti(ext_record.fields);
                                            while (ext_field_iter.next()) |field| {
                                                // TODO is it possible that here we're adding fields with names
                                                // already in the list? Would type-checking have already collapsed these?
                                                // We would certainly rather not spend time doing hashmap things
                                                // if we can avoid it here.
                                                try self.work.pending_record_fields.append(field);
                                            }
                                            current_ext = ext_record.ext;
                                        } else {
                                            break;
                                        }
                                    },
                                    else => return LayoutError.InvalidRecordExtension,
                                },
                                .alias => |alias| {
                                    current_ext = var_store.resolveVar(alias.backing_var).var_;
                                },
                                else => return LayoutError.InvalidRecordExtension,
                            }
                        }

                        switch (num_fields) {
                            0 => {
                                // This is an empty record!
                            },
                            1 => {
                                // This is a single-field record; unwrap it and move on.
                                const field = self.work.pending_record_fields.pop() orelse unreachable;
                                current = var_store.resolveVar(field.var_);
                                continue;
                            },
                            _ => {
                                // Sort these fields *descending* by field name. (Descending because we will pop
                                // these off one at a time to build up the layout, resulting in the layout being
                                // sorted *ascending* by field name. Later, it will be further sorted by alignment.)
                                const SortCtx = struct {
                                    ident_store: *Ident.Store,
                                    fn lessThan(ctx: @This(), a: types.RecordField, b: types.RecordField) bool {
                                        const a_str = ctx.ident_store.getText(a.name);
                                        const b_str = ctx.ident_store.getText(b.name);
                                        // Sort descending (b before a)
                                        return std.mem.order(u8, b_str, a_str) == .lt;
                                    }
                                };

                                self.work.pending_record_fields.sortSpan(
                                    self.work.pending_record_fields.len - num_fields,
                                    self.work.pending_record_fields.len,
                                    SortCtx{ .ident_store = self.ident_store },
                                );

                                try self.work.pending_containers.append(self.env.gpa, .{
                                    .record = .{
                                        .num_fields = num_fields,
                                        .pending_fields = num_fields,
                                        .resolved_fields_start = @intCast(self.work.resolved_record_fields.len),
                                    },
                                });

                                // Start working on the last pending field (we want to pop them).
                                const field = self.work.pending_record_fields.get(num_fields - 1);
                                current = var_store.resolveVar(field.var_);
                                continue;
                            },
                        }
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
                                            break :blk try self.insertLayout(.empty_record);
                                        }

                                        const fields_start = self.record_fields.items.len;

                                        // Copy only this record's resolved fields to the record_fields store
                                        const field_names = self.work.resolved_record_fields.items(.field_name);
                                        const field_idxs = self.work.resolved_record_fields.items(.field_idx);

                                        var i: u32 = updated_record.resolved_fields_start;
                                        while (i < resolved_fields_end) : (i += 1) {
                                            try self.record_fields.append(self.env.gpa, .{
                                                .name = field_names[i],
                                                .layout = field_idxs[i],
                                            });
                                        }

                                        // Create the record layout with the fields range
                                        const fields_range = try collections.NonEmptyRange.init(@intCast(fields_start), @intCast(num_resolved_fields));
                                        const record_layout = Layout{ .record = .{ .fields = fields_range } };

                                        // Remove only this record's resolved fields
                                        self.work.resolved_record_fields.shrinkRetainingCapacity(updated_record.resolved_fields_start);

                                        break :blk try self.insertLayout(record_layout);
                                    }

                                    // Still have fields to process, push the container back
                                    try self.work.pending_containers.append(self.env.gpa, .{ .record = updated_record });

                                    // Continue working on the other fields in the record.
                                    continue;
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

                                    // The current field we're working on has turned out to be zero-sized, so drop it.
                                    const field = self.work.pending_record_fields.pop() orelse unreachable;

                                    std.debug.assert(current.var_ == field.var_);

                                    // We dropped a zero-sized field, check if there are more fields to process
                                    if (self.work.pending_record_fields.items.len > 0) {
                                        // Get the next field to process
                                        const next_field = self.work.pending_record_fields.items[self.work.pending_record_fields.items.len - 1];
                                        current = var_store.resolveVar(next_field.var_);
                                        continue;
                                    }
                                    // All fields processed but all were zero-sized
                                    break;
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
            if (self.work.pending_containers.pop()) |pending_container| {
                switch (pending_container) {
                    .box => {
                        // TODO
                    },
                    .list => {
                        // TODO
                    },
                    .record => |pending_record| {
                        std.debug.assert(pending_record.pending_fields > 0);
                        var updated_record = pending_record;
                        updated_record.pending_fields -= 1;

                        // Pop the field we just processed
                        const pending_field = self.work.pending_record_fields.pop() orelse unreachable;
                        std.debug.assert(pending_field.var_ == current.var_);

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
                            const fields_start = self.record_fields.items.len;

                            // Copy only this record's resolved fields to the record_fields store
                            const field_names = self.work.resolved_record_fields.items(.field_name);
                            const field_idxs = self.work.resolved_record_fields.items(.field_idx);

                            var i: u32 = updated_record.resolved_fields_start;
                            while (i < resolved_fields_end) : (i += 1) {
                                try self.record_fields.append(self.env.gpa, .{
                                    .name = field_names[i],
                                    .layout = field_idxs[i],
                                });
                            }

                            // Create the record layout with the fields range
                            const fields_range = try collections.NonEmptyRange.init(@intCast(fields_start), @intCast(num_resolved_fields));
                            layout_idx = try self.insertLayout(Layout{ .record = .{ .fields = fields_range } });

                            // Remove only this record's resolved fields
                            self.work.resolved_record_fields.shrinkRetainingCapacity(updated_record.resolved_fields_start);
                        } else {
                            // Still have fields to process, push the container back
                            try self.work.pending_containers.append(self.env.gpa, .{ .record = updated_record });
                        }
                    },
                }
            }

            // Check if we have more record fields to process
            if (self.work.pending_record_fields.items.len > 0) {
                // Get the next field to process
                const next_field = self.work.pending_record_fields.items[self.work.pending_record_fields.items.len - 1];
                current = var_store.resolveVar(next_field.var_);
                continue;
            }

            // For now, just return the layout we created
            if (result == null) {
                result = layout_idx;
            }
            break;
        }

        return result.?;
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
    var layout_store = try Store.init(&module_env);
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
    var layout_store = try Store.init(&module_env);
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
    var layout_store = try Store.init(&module_env);
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
    var layout_store = try Store.init(&module_env);
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
    var layout_store = try Store.init(&module_env);
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
    var layout_store = try Store.init(&module_env);
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
    var layout_store = try Store.init(&module_env);
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
    var layout_store = try Store.init(&module_env);
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
    var layout_store = try Store.init(&module_env);
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
    var layout_store = try Store.init(&module_env);
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
    var layout_store = try Store.init(&module_env);
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
    var layout_store = try Store.init(&module_env);
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
    var layout_store = try Store.init(&module_env);
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
    var layout_store = try Store.init(&module_env);
    defer layout_store.deinit();

    // Create an ident for the rigid var
    const ident_idx = type_store.env.ident_store.insert("a");

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
    var layout_store = try Store.init(&module_env);
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
    var layout_store = try Store.init(&module_env);
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

test "addTypeVar - bare empty record returns error" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Create type store
    var type_store = types_store.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = try Store.init(&module_env);
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
    var layout_store = try Store.init(&module_env);
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
    var layout_store = try Store.init(&module_env);
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
    var layout_store = try Store.init(&module_env);
    defer layout_store.deinit();

    // Create field types
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const u32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u32 } } } });

    // Create field names
    const name_ident = type_store.env.ident_store.insert("name");
    const age_ident = type_store.env.ident_store.insert("age");

    // Create record fields
    const fields = try type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
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

    // Verify the fields
    const field_iter = layout_store.record_fields.iter(record_layout.record.fields);

    // First field: name (str)
    const name_field = field_iter.next().?;
    try testing.expect(name_field.name == name_ident);
    const name_layout = layout_store.getLayout(name_field.layout);
    try testing.expect(name_layout.* == .str);

    // Second field: age (u32)
    const age_field = field_iter.next().?;
    try testing.expect(age_field.name == age_ident);
    const age_layout = layout_store.getLayout(age_field.layout);
    try testing.expect(age_layout.* == .int);
    try testing.expect(age_layout.int == .u32);

    // No more fields
    try testing.expect(field_iter.next() == null);
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
    var layout_store = try Store.init(&module_env);
    defer layout_store.deinit();

    // Create inner record type { x: i32, y: i32 }
    const i32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i32 } } } });
    const x_ident = type_store.env.ident_store.insert("x");
    const y_ident = type_store.env.ident_store.insert("y");

    const point_fields = try type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = i32_var },
        .{ .name = y_ident, .var_ = i32_var },
    });

    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const point_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = point_fields, .ext = empty_ext } } });

    // Create outer record type { name: Str, position: { x: i32, y: i32 } }
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const name_ident = type_store.env.ident_store.insert("name");
    const position_ident = type_store.env.ident_store.insert("position");

    const player_fields = try type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
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
    const outer_field_iter = layout_store.record_fields.iter(player_layout.record.fields);

    // First field: name (str)
    const name_field = outer_field_iter.next().?;
    try testing.expect(name_field.name == name_ident);
    const name_layout = layout_store.getLayout(name_field.layout);
    try testing.expect(name_layout.* == .str);

    // Second field: position (record)
    const position_field = outer_field_iter.next().?;
    try testing.expect(position_field.name == position_ident);
    const position_layout = layout_store.getLayout(position_field.layout);
    try testing.expect(position_layout.* == .record);

    // No more outer fields
    try testing.expect(outer_field_iter.next() == null);

    // Verify the inner record fields
    const inner_field_iter = layout_store.record_fields.iter(position_layout.record.fields);

    // Inner field: x (i32)
    const x_field = inner_field_iter.next().?;
    try testing.expect(x_field.name == x_ident);
    const x_layout = layout_store.getLayout(x_field.layout);
    try testing.expect(x_layout.* == .int);
    try testing.expect(x_layout.int == .i32);

    // Inner field: y (i32)
    const y_field = inner_field_iter.next().?;
    try testing.expect(y_field.name == y_ident);
    const y_layout = layout_store.getLayout(y_field.layout);
    try testing.expect(y_layout.* == .int);
    try testing.expect(y_layout.int == .i32);

    // No more inner fields
    try testing.expect(inner_field_iter.next() == null);
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
    var layout_store = try Store.init(&module_env);
    defer layout_store.deinit();

    // Create record type { id: u64, active: bool }
    const u64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u64 } } } });
    // For bool, we'll use u8 as a placeholder
    const bool_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u8 } } } });
    const id_ident = type_store.env.ident_store.insert("id");
    const active_ident = type_store.env.ident_store.insert("active");

    const record_fields = try type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
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
    const field_iter = layout_store.record_fields.iter(record_layout.record.fields);

    // First field: id (u64)
    const id_field = field_iter.next().?;
    try testing.expect(id_field.name == id_ident);
    const id_layout = layout_store.getLayout(id_field.layout);
    try testing.expect(id_layout.* == .int);
    try testing.expect(id_layout.int == .u64);

    // Second field: active (u8)
    const active_field = field_iter.next().?;
    try testing.expect(active_field.name == active_ident);
    const active_layout = layout_store.getLayout(active_field.layout);
    try testing.expect(active_layout.* == .int);
    try testing.expect(active_layout.int == .u8);

    // No more fields
    try testing.expect(field_iter.next() == null);
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
    var layout_store = try Store.init(&module_env);
    defer layout_store.deinit();

    // Create extension record { y: i32, z: f64 }
    const i32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i32 } } } });
    const f64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .frac = .{ .exact = .f64 } } } });
    const y_ident = type_store.env.ident_store.insert("y");
    const z_ident = type_store.env.ident_store.insert("z");

    const ext_fields = try type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = y_ident, .var_ = i32_var },
        .{ .name = z_ident, .var_ = f64_var },
    });

    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const ext_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = ext_fields, .ext = empty_ext } } });

    // Create main record { x: str } extending the above
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const x_ident = type_store.env.ident_store.insert("x");

    const main_fields = try type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = str_var },
    });

    const main_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = main_fields, .ext = ext_record_var } } });

    // Convert to layout
    const record_layout_idx = try layout_store.addTypeVar(&type_store, main_record_var);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.* == .record);

    // Verify we have all 3 fields (x from main, y and z from extension)
    const field_iter = layout_store.record_fields.iter(record_layout.record.fields);

    // Field x (str)
    const x_field = field_iter.next().?;
    try testing.expect(x_field.name == x_ident);
    const x_layout = layout_store.getLayout(x_field.layout);
    try testing.expect(x_layout.* == .str);

    // Field y (i32)
    const y_field = field_iter.next().?;
    try testing.expect(y_field.name == y_ident);
    const y_layout = layout_store.getLayout(y_field.layout);
    try testing.expect(y_layout.* == .int);
    try testing.expect(y_layout.int == .i32);

    // Field z (f64)
    const z_field = field_iter.next().?;
    try testing.expect(z_field.name == z_ident);
    const z_layout = layout_store.getLayout(z_field.layout);
    try testing.expect(z_layout.* == .frac);
    try testing.expect(z_layout.frac == .f64);

    // No more fields
    try testing.expect(field_iter.next() == null);
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
    var layout_store = try Store.init(&module_env);
    defer layout_store.deinit();

    // Create types
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const i32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i32 } } } });
    const x_ident = type_store.env.ident_store.insert("x");
    const y_ident = type_store.env.ident_store.insert("y");

    // Create extension record { x: str, y: i32 }
    const ext_fields = try type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = str_var },
        .{ .name = y_ident, .var_ = i32_var },
    });

    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const ext_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = ext_fields, .ext = empty_ext } } });

    // Create main record { x: str } extending the above (x appears in both with same type)
    const main_fields = try type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = str_var },
    });

    const main_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = main_fields, .ext = ext_record_var } } });

    // Convert to layout - should succeed since types match
    const record_layout_idx = try layout_store.addTypeVar(&type_store, main_record_var);

    // Verify the layout
    const record_layout = layout_store.getLayout(record_layout_idx);
    try testing.expect(record_layout.* == .record);

    // Verify we have 2 fields (x appears only once, y from extension)
    const field_iter = layout_store.record_fields.iter(record_layout.record.fields);

    // Field x (str)
    const x_field = field_iter.next().?;
    try testing.expect(x_field.name == x_ident);
    const x_layout = layout_store.getLayout(x_field.layout);
    try testing.expect(x_layout.* == .str);

    // Field y (i32)
    const y_field = field_iter.next().?;
    try testing.expect(y_field.name == y_ident);
    const y_layout = layout_store.getLayout(y_field.layout);
    try testing.expect(y_layout.* == .int);
    try testing.expect(y_layout.int == .i32);

    // No more fields
    try testing.expect(field_iter.next() == null);
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
    var layout_store = try Store.init(&module_env);
    defer layout_store.deinit();

    // Create types
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const i32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i32 } } } });
    const x_ident = type_store.env.ident_store.insert("x");

    // Create extension record { x: i32 }
    const ext_fields = try type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = i32_var },
    });

    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const ext_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = ext_fields, .ext = empty_ext } } });

    // Create main record { x: str } extending the above (x appears in both with different types)
    const main_fields = try type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = x_ident, .var_ = str_var },
    });

    const main_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = main_fields, .ext = ext_record_var } } });

    // Convert to layout - should fail due to type mismatch
    const result = layout_store.addTypeVar(&type_store, main_record_var);
    try testing.expectError(LayoutError.RecordFieldTypeMismatch, result);
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
    var layout_store = try Store.init(&module_env);
    defer layout_store.deinit();

    // Create a str type to use as invalid extension
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const x_ident = type_store.env.ident_store.insert("x");

    // Create main record { x: str } with str as extension (invalid)
    const main_fields = try type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
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
    var layout_store = try Store.init(&module_env);
    defer layout_store.deinit();

    // Create types
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const i32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i32 } } } });
    const f64_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .frac = .{ .exact = .f64 } } } });
    const u8_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .u8 } } } });

    const w_ident = type_store.env.ident_store.insert("w");
    const x_ident = type_store.env.ident_store.insert("x");
    const y_ident = type_store.env.ident_store.insert("y");
    const z_ident = type_store.env.ident_store.insert("z");

    // Create innermost extension record { z: u8 }
    const inner_fields = try type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = z_ident, .var_ = u8_var },
    });

    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const inner_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = inner_fields, .ext = empty_ext } } });

    // Create middle extension record { y: f64 } extending inner
    const middle_fields = try type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = y_ident, .var_ = f64_var },
    });

    const middle_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = middle_fields, .ext = inner_record_var } } });

    // Create outermost record { w: str, x: i32 } extending middle
    const outer_fields = try type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
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
    const field_iter = layout_store.record_fields.iter(record_layout.record.fields);

    // Field w (str)
    const w_field = field_iter.next().?;
    try testing.expect(w_field.name == w_ident);
    const w_layout = layout_store.getLayout(w_field.layout);
    try testing.expect(w_layout.* == .str);

    // Field x (i32)
    const x_field = field_iter.next().?;
    try testing.expect(x_field.name == x_ident);
    const x_layout = layout_store.getLayout(x_field.layout);
    try testing.expect(x_layout.* == .int);
    try testing.expect(x_layout.int == .i32);

    // Field y (f64)
    const y_field = field_iter.next().?;
    try testing.expect(y_field.name == y_ident);
    const y_layout = layout_store.getLayout(y_field.layout);
    try testing.expect(y_layout.* == .frac);
    try testing.expect(y_layout.frac == .f64);

    // Field z (u8)
    const z_field = field_iter.next().?;
    try testing.expect(z_field.name == z_ident);
    const z_layout = layout_store.getLayout(z_field.layout);
    try testing.expect(z_layout.* == .int);
    try testing.expect(z_layout.int == .u8);

    // No more fields
    try testing.expect(field_iter.next() == null);
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
    var layout_store = try Store.init(&module_env);
    defer layout_store.deinit();

    // Create types
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const empty_record_var = type_store.freshFromContent(.{ .structure = .empty_record });
    const i32_var = type_store.freshFromContent(.{ .structure = .{ .num = .{ .int = .{ .exact = .i32 } } } });

    const name_ident = type_store.env.ident_store.insert("name");
    const empty_ident = type_store.env.ident_store.insert("empty");
    const age_ident = type_store.env.ident_store.insert("age");

    // Create record { name: str, empty: {}, age: i32 }
    const fields = try type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
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
    const field_iter = layout_store.record_fields.iter(record_layout.record.fields);

    // Debug: Check the actual field count
    var field_count: u32 = 0;
    var temp_iter = layout_store.record_fields.iter(record_layout.record.fields);
    while (temp_iter.next()) |_| {
        field_count += 1;
    }
    try testing.expect(field_count == 2);

    // Field name (str)
    const name_field = field_iter.next().?;
    try testing.expect(name_field.name == name_ident);
    const name_layout = layout_store.getLayout(name_field.layout);
    try testing.expect(name_layout.* == .str);

    // Field age (i32) - empty field should be skipped
    const age_field = field_iter.next().?;
    try testing.expect(age_field.name == age_ident);
    const age_layout = layout_store.getLayout(age_field.layout);
    try testing.expect(age_layout.* == .int);
    try testing.expect(age_layout.int == .i32);

    // No more fields
    try testing.expect(field_iter.next() == null);
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
    var layout_store = try Store.init(&module_env);
    defer layout_store.deinit();

    // Create types
    const empty_record_var = type_store.freshFromContent(.{ .structure = .empty_record });
    const empty_tag_union_var = type_store.freshFromContent(.{ .structure = .empty_tag_union });

    const field1_ident = type_store.env.ident_store.insert("field1");
    const field2_ident = type_store.env.ident_store.insert("field2");

    // Create record { field1: {}, field2: [] }
    const fields = try type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
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
    var layout_store = try Store.init(&module_env);
    defer layout_store.deinit();

    // Create types
    const empty_record_var = type_store.freshFromContent(.{ .structure = .empty_record });

    const field1_ident = type_store.env.ident_store.insert("field1");
    const field2_ident = type_store.env.ident_store.insert("field2");

    // Create record { field1: {}, field2: {} }
    const fields = try type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
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
    var layout_store = try Store.init(&module_env);
    defer layout_store.deinit();

    // Create field names we'll reuse
    const field_names = [_]Ident.Idx{
        type_store.env.ident_store.insert("a"),
        type_store.env.ident_store.insert("b"),
        type_store.env.ident_store.insert("c"),
        type_store.env.ident_store.insert("d"),
        type_store.env.ident_store.insert("e"),
        type_store.env.ident_store.insert("f"),
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

            var test_layout_store = try Store.init(&module_env);
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
                        const empty_ext = test_type_store.freshFromContent(.{ .structure = .empty_record });
                        break :blk test_type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = try test_type_store.record_fields.appendSlice(test_type_store.env.gpa, &[_]types.RecordField{}), .ext = empty_ext } } });
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

                        const inner_fields_slice = try test_type_store.record_fields.appendSlice(test_type_store.env.gpa, inner_fields.items);
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

                        // Check if this nested record will have any fields after dropping zero-sized ones
                        if (inner_fields.items.len > 1) { // We know we have 1 non-zero field
                            expected_non_zero_fields += 1;
                        }

                        const inner_fields_slice = try test_type_store.record_fields.appendSlice(test_type_store.env.gpa, inner_fields.items);
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
            const outer_fields_slice = try test_type_store.record_fields.appendSlice(test_type_store.env.gpa, outer_fields.items);
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
                var actual_field_count: usize = 0;
                const field_iter = test_layout_store.record_fields.iter(result_layout.record.fields);
                while (field_iter.next()) |field| {
                    actual_field_count += 1;

                    // Verify each field has a valid layout
                    const field_layout = test_layout_store.getLayout(field.layout);
                    switch (field_layout.*) {
                        .str => {}, // Valid non-zero field
                        .record => |rec| {
                            // Verify nested record has fields
                            const nested_iter = test_layout_store.record_fields.iter(rec.fields);
                            var has_fields = false;
                            while (nested_iter.next()) |_| {
                                has_fields = true;
                            }
                            try testing.expect(has_fields);
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
    var layout_store = try Store.init(&module_env);
    defer layout_store.deinit();

    // Create inner record with only zero-sized fields
    const empty_record_var = type_store.freshFromContent(.{ .structure = .empty_record });
    const a_ident = type_store.env.ident_store.insert("a");
    const b_ident = type_store.env.ident_store.insert("b");

    const inner_fields = try type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = a_ident, .var_ = empty_record_var },
        .{ .name = b_ident, .var_ = empty_record_var },
    });

    const empty_ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const inner_record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = inner_fields, .ext = empty_ext } } });

    // Create outer record { name: str, data: inner_record }
    const str_var = type_store.freshFromContent(.{ .structure = .str });
    const name_ident = type_store.env.ident_store.insert("name");
    const data_ident = type_store.env.ident_store.insert("data");

    const outer_fields = try type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
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
    const field_iter = layout_store.record_fields.iter(record_layout.record.fields);

    // Field name (str)
    const name_field = field_iter.next().?;
    try testing.expect(name_field.name == name_ident);
    const name_layout = layout_store.getLayout(name_field.layout);
    try testing.expect(name_layout.* == .str);

    // No more fields (data field should be dropped)
    try testing.expect(field_iter.next() == null);
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
    var layout_store = try Store.init(&module_env);
    defer layout_store.deinit();

    // Create types
    const empty_record_var = type_store.freshFromContent(.{ .structure = .empty_record });

    const field_ident = type_store.env.ident_store.insert("field");

    // Create record { field: {} }
    const fields = try type_store.record_fields.appendSlice(type_store.env.gpa, &[_]types.RecordField{
        .{ .name = field_ident, .var_ = empty_record_var },
    });

    const ext = type_store.freshFromContent(.{ .structure = .empty_record });
    const record_var = type_store.freshFromContent(.{ .structure = .{ .record = .{ .fields = fields, .ext = ext } } });

    // Create list of this record
    const list_record_var = type_store.freshFromContent(.{ .structure = .{ .list = record_var } });

    // Convert to layout - should become list_zero_sized
    const list_layout_idx = try layout_store.addTypeVar(&type_store, list_record_var);

    // Verify the layout is list_zero_sized
    const list_layout = layout_store.getLayout(list_layout_idx);
    try testing.expect(list_layout.* == .list_zero_sized);
}
