//! The store of solved types
//! Contains both Slot & Descriptor stores

const std = @import("std");
const types = @import("../types/types.zig");
const types_store = @import("../types/store.zig");
const layout = @import("./layout.zig");
const base = @import("../base.zig");
const collections = @import("../collections.zig");
const Ident = @import("../base/Ident.zig");

const Var = types.Var;
const Layout = layout.Layout;
const Idx = layout.Idx;

const MkSafeList = collections.SafeList;
const RecordField = layout.RecordField;
const RecordFieldSafeMultiList = RecordField.SafeMultiList;

pub const LayoutError = error{
    ZeroSizedType,
    TypeContainedMismatch,
    RecordFieldTypeMismatch,
    InvalidRecordExtension,
    // Compiler bugs. Hopefully these never come up, but if they do, the caller should gracefully recover.
    BugUnboxedFlexVar,
    BugUnboxedRigidVar,
};

pub const Store = struct {
    const Self = @This();

    env: *base.ModuleEnv,

    // Layout storage
    layouts: std.ArrayListUnmanaged(Layout),

    // Lists for parameterized layouts
    tuple_elems: collections.SafeList(Idx),
    record_fields: RecordFieldSafeMultiList,

    // Cache to avoid duplicate work
    var_to_layout: std.AutoHashMapUnmanaged(Var, Idx),

    pub fn init(env: *base.ModuleEnv) std.mem.Allocator.Error!Self {
        return .{
            .env = env,
            .layouts = std.ArrayListUnmanaged(Layout){},
            .tuple_elems = try collections.SafeList(Idx).initCapacity(env.gpa, 512),
            .record_fields = try RecordFieldSafeMultiList.initCapacity(env.gpa, 256),
            .var_to_layout = std.AutoHashMapUnmanaged(Var, Idx){},
        };
    }

    pub fn deinit(self: *Self) void {
        self.layouts.deinit(self.env.gpa);
        self.tuple_elems.deinit(self.env.gpa);
        self.record_fields.deinit(self.env.gpa);
        self.var_to_layout.deinit(self.env.gpa);
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

    fn isZeroSized(self: *const Self, idx: Idx) bool {
        const layout_ptr = &self.layouts.items[idx.idx];
        return switch (layout_ptr.*) {
            .record => |record| record.fields.count == 0,
            else => false,
        };
    }

    /// Note: the caller MUST verify ahead of time that the given variable does not
    /// resolve to a flex var or rigid var, unless that flex var or rigid var is
    /// wrapped in a Box.
    pub fn addTypeVar(
        self: *Store,
        var_store: *const types_store.Store,
        var_: Var,
    ) (LayoutError || std.mem.Allocator.Error)!Idx {
        const initial_resolved = var_store.resolveVar(var_);

        // If we've already done
        if (self.var_to_layout.get(initial_resolved.var_)) |cached_idx| {
            return cached_idx;
        }

        // To make this function stack-safe, we use a manual stack instead of recursing.
        const WorkItem = struct {
            var_to_process: Var,
            parent: Parent,

            const Parent = union(enum) {
                none,
                box_elem: struct { idx: Idx },
                list_elem: struct { idx: Idx },
                record_field: struct { idx: Idx, field_name: Ident.Idx },
            };
        };

        var work_stack = std.ArrayList(WorkItem).init(self.env.gpa);
        defer work_stack.deinit();

        var result: ?Idx = null;

        // Initialize with the initial variable
        var work_item = WorkItem{
            .var_to_process = initial_resolved.var_,
            .parent = .none,
        };

        while (true) {
            // The var we're aabout to work on should already have been resolved.
            if (std.debug.runtime_safety) {
                const resolved_check = var_store.resolveVar(work_item.var_to_process);
                std.debug.assert(work_item.var_to_process == resolved_check.var_);
            }

            const resolved = var_store.resolveVar(work_item.var_to_process);
            const layout_idx = switch (resolved.desc.content) {
                .structure => |flat_type| switch (flat_type) {
                    .str => try self.insertLayout(.str),
                    .box => |elem_var| blk: {
                        // Create placeholder box layout
                        const placeholder_idx = Idx{
                            .attributes = .{ ._padding = 0 },
                            .idx = 0,
                        };
                        const box_idx = try self.insertLayout(Layout{ .box = placeholder_idx });

                        // Process element
                        try work_stack.append(.{
                            .var_to_process = var_store.resolveVar(elem_var).var_,
                            .parent = .{ .box_elem = .{ .idx = box_idx } },
                        });

                        break :blk box_idx;
                    },
                    .list => |elem_var| blk: {
                        // Create placeholder list layout
                        const placeholder_idx = Idx{
                            .attributes = .{ ._padding = 0 },
                            .idx = 0,
                        };
                        const list_idx = try self.insertLayout(Layout{ .list = placeholder_idx });

                        // Process element
                        try work_stack.append(.{
                            .var_to_process = var_store.resolveVar(elem_var).var_,
                            .parent = .{ .list_elem = .{ .idx = list_idx } },
                        });

                        break :blk list_idx;
                    },
                    .custom_type => |custom_type| {
                        // TODO special-case the builtin Num type here.
                        // If we have one of those, then convert it to a num layout,
                        // or to a runtime error if it's an invalid elem type.

                        // From a layout perspective, custom types are identical to type aliases:
                        // all we care about is what's inside.
                        work_item.var_to_process = var_store.resolveVar(custom_type.backing_var).var_;
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
                    .record => |record| blk: {
                        // Process record fields
                        const fields_start = self.record_fields.len();

                        // First, we need to get all the fields from the type store
                        const field_iter = var_store.iterMulti(record.fields);

                        // Create a placeholder record layout
                        const placeholder_range = RecordFieldSafeMultiList.Range{
                            .start = 0,
                            .count = 0,
                        };
                        const record_idx = try self.insertLayout(Layout{ .record = .{ .fields = placeholder_range } });

                        // Process each field
                        var field_count: u32 = 0;
                        while (field_iter.next()) |field| {
                            // Push work item for this field
                            try work_stack.append(.{
                                .var_to_process = var_store.resolveVar(field.var_).var_,
                                .parent = .{ .record_field = .{ .idx = record_idx, .field_name = field.name } },
                            });
                            field_count += 1;
                        }

                        // Process the extension variable
                        const ext_resolved = var_store.resolveVar(record.ext);
                        const ext_content = ext_resolved.desc.content;

                        switch (ext_content) {
                            .structure => |ext_flat_type| switch (ext_flat_type) {
                                .empty_record => {
                                    // Empty extension, nothing to do
                                },
                                .record => |ext_record| {
                                    // Extension is another record, we need to merge its fields
                                    const ext_field_iter = var_store.iterMulti(ext_record.fields);
                                    while (ext_field_iter.next()) |field| {
                                        // Push work item for this extension field
                                        try work_stack.append(.{
                                            .var_to_process = var_store.resolveVar(field.var_).var_,
                                            .parent = .{ .record_field = .{ .idx = record_idx, .field_name = field.name } },
                                        });
                                        field_count += 1;
                                    }

                                    // Recursively process the extension's extension
                                    work_item.var_to_process = var_store.resolveVar(ext_record.ext).var_;
                                    continue;
                                },
                                else => {
                                    // Invalid extension type
                                    return LayoutError.InvalidRecordExtension;
                                },
                            },
                            .alias => |alias| {
                                // Follow the alias
                                work_item.var_to_process = var_store.resolveVar(alias.backing_var).var_;
                                continue;
                            },
                            else => {
                                // Invalid extension type
                                return LayoutError.InvalidRecordExtension;
                            },
                        }

                        // Store the fields range in the placeholder
                        // The count will be updated as fields are actually added (non-zero-sized only)
                        const record_layout = self.getLayout(record_idx);
                        record_layout.record.fields.start = @intCast(fields_start);
                        record_layout.record.fields.count = 0; // Will be incremented as fields are added

                        break :blk record_idx;
                    },
                    .tag_union => |tag_union| {
                        // TODO
                        _ = tag_union;
                        @panic("TODO: tag_union layout");
                    },
                    .empty_record => blk: {
                        // If this zero-sized type is inside a box or list, we can handle it
                        switch (work_item.parent) {
                            .box_elem => {
                                // Signal to parent to use box_zero_sized
                                break :blk try self.insertLayout(.box_zero_sized);
                            },
                            .list_elem => {
                                // Signal to parent to use list_zero_sized
                                break :blk try self.insertLayout(.list_zero_sized);
                            },
                            else => return LayoutError.ZeroSizedType,
                        }
                    },
                    .empty_tag_union => blk: {
                        // If this zero-sized type is inside a box or list, we can handle it
                        switch (work_item.parent) {
                            .box_elem => {
                                // Signal to parent to use box_zero_sized
                                break :blk try self.insertLayout(.box_zero_sized);
                            },
                            .list_elem => {
                                // Signal to parent to use list_zero_sized
                                break :blk try self.insertLayout(.list_zero_sized);
                            },
                            else => return LayoutError.ZeroSizedType,
                        }
                    },
                },
                .flex_var => |_| blk: {
                    // Flex vars can only be sent to the host if boxed.
                    // The caller should have verified this invariant already.
                    switch (work_item.parent) {
                        .box_elem => {},
                        else => {
                            std.debug.assert(false);
                            return LayoutError.BugUnboxedFlexVar;
                        },
                    }

                    break :blk try self.insertLayout(.host_opaque);
                },
                .rigid_var => |_| blk: {
                    // Rigid vars can only be sent to the host if boxed.
                    // The caller should have verified this invariant already.
                    switch (work_item.parent) {
                        .box_elem => {},
                        else => {
                            std.debug.assert(false);
                            return LayoutError.BugUnboxedRigidVar;
                        },
                    }

                    break :blk try self.insertLayout(.host_opaque);
                },
                .alias => |alias| {
                    // Follow the alias by updating the work item
                    work_item.var_to_process = var_store.resolveVar(alias.backing_var).var_;
                    continue;
                },
                .effectful => @panic("TODO: effectful doesn't make sense as a layout; should be moved out of Content"),
                .pure => @panic("pure doesn't make sense as a layout; should be moved out of Content"),
                .err => return LayoutError.TypeContainedMismatch,
            };

            // Cache the layout
            try self.var_to_layout.put(self.env.gpa, work_item.var_to_process, layout_idx);

            // Update parent if needed
            switch (work_item.parent) {
                .none => {},
                .box_elem => |box_parent| {
                    const parent_layout = self.getLayout(box_parent.idx);
                    const child_layout = self.getLayout(layout_idx);

                    // Check if child is a zero-sized placeholder
                    if (child_layout.* == .box_zero_sized) {
                        parent_layout.* = .box_zero_sized;
                    } else {
                        parent_layout.* = Layout{ .box = layout_idx };
                    }
                },
                .list_elem => |list_parent| {
                    const parent_layout = self.getLayout(list_parent.idx);
                    const child_layout = self.getLayout(layout_idx);

                    // Check if child is a zero-sized placeholder
                    if (child_layout.* == .list_zero_sized) {
                        parent_layout.* = .list_zero_sized;
                    } else {
                        parent_layout.* = Layout{ .list = layout_idx };
                    }
                },
                .record_field => |record_parent| {
                    // Only add non-zero-sized fields to the record
                    if (!self.isZeroSized(layout_idx)) {
                        // Add this field to the record_fields list, checking for duplicates
                        const parent_record = self.getLayout(record_parent.idx);
                        const field_name = record_parent.field_name;

                        // Check if this field already exists
                        const existing_fields = self.record_fields.slice(parent_record.record.fields);
                        for (existing_fields) |existing_field| {
                            if (existing_field.name == field_name) {
                                // Field already exists, check if types match
                                if (existing_field.layout != layout_idx) {
                                    return LayoutError.RecordFieldTypeMismatch;
                                }
                                // Types match, skip adding duplicate
                                break;
                            }
                        } else {
                            // Field doesn't exist yet, add it
                            const field = RecordField{
                                .name = field_name,
                                .layout = layout_idx,
                            };
                            try self.record_fields.append(self.env.gpa, field);

                            // Update the parent's field count
                            parent_record.record.fields.count += 1;
                        }
                    }
                },
            }

            // If this was the root item, save the result
            if (work_item.parent == .none) {
                result = layout_idx;
            }

            // Check if we just finished processing a record that ended up empty
            const finished_layout = self.getLayout(layout_idx);
            if (finished_layout.* == .record and finished_layout.record.fields.count == 0) {
                // This record has no non-zero-sized fields, handle based on parent
                switch (work_item.parent) {
                    .none => {
                        // Root level empty record is an error
                        return LayoutError.ZeroSizedType;
                    },
                    .box_elem => |box_parent| {
                        // Replace parent with box_zero_sized
                        const parent_layout = self.getLayout(box_parent.idx);
                        parent_layout.* = .box_zero_sized;
                    },
                    .list_elem => |list_parent| {
                        // Replace parent with list_zero_sized
                        const parent_layout = self.getLayout(list_parent.idx);
                        parent_layout.* = .list_zero_sized;
                    },
                    .record_field => {
                        // The parent record will drop this field when checking isZeroSized
                    },
                }
            }

            // Check if there's more work to do
            if (work_stack.popOrNull()) |next_item| {
                work_item = next_item;

                // Check cache for the next item
                if (self.var_to_layout.get(work_item.var_to_process)) |cached_idx| {
                    // Update parent if needed
                    switch (work_item.parent) {
                        .none => {
                            // This was the root item, save the result
                            result = cached_idx;
                        },
                        .box_elem => |box_parent| {
                            const parent_layout = self.getLayout(box_parent.idx);
                            const child_layout = self.getLayout(cached_idx);

                            // Check if child is a zero-sized placeholder
                            if (child_layout.* == .box_zero_sized) {
                                parent_layout.* = .box_zero_sized;
                            } else {
                                parent_layout.* = Layout{ .box = cached_idx };
                            }
                        },
                        .list_elem => |list_parent| {
                            const parent_layout = self.getLayout(list_parent.idx);
                            const child_layout = self.getLayout(cached_idx);

                            // Check if child is a zero-sized placeholder
                            if (child_layout.* == .list_zero_sized) {
                                parent_layout.* = .list_zero_sized;
                            } else {
                                parent_layout.* = Layout{ .list = cached_idx };
                            }
                        },
                        .record_field => |record_parent| {
                            // Only add non-zero-sized fields to the record
                            if (!self.isZeroSized(cached_idx)) {
                                // Add this field to the record_fields list, checking for duplicates
                                const parent_record = self.getLayout(record_parent.idx);
                                const field_name = record_parent.field_name;

                                // Check if this field already exists
                                const existing_fields = self.record_fields.slice(parent_record.record.fields);
                                for (existing_fields) |existing_field| {
                                    if (existing_field.name == field_name) {
                                        // Field already exists, check if types match
                                        if (existing_field.layout != cached_idx) {
                                            return LayoutError.RecordFieldTypeMismatch;
                                        }
                                        // Types match, skip adding duplicate
                                        break;
                                    }
                                } else {
                                    // Field doesn't exist yet, add it
                                    const field = RecordField{
                                        .name = field_name,
                                        .layout = cached_idx,
                                    };
                                    try self.record_fields.append(self.env.gpa, field);

                                    // Update the parent's field count
                                    parent_record.record.fields.count += 1;
                                }
                            }
                        },
                    }
                    continue;
                }
            } else {
                break;
            }
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
