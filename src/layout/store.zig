//! The store of solved types
//! Contains both Slot & Descriptor stores

const std = @import("std");
const types = @import("../types/types.zig");
const layout = @import("./layout.zig");
const base = @import("../base.zig");
const collections = @import("../collections.zig");

const Var = types.Var;
const Layout = layout.Layout;
const Idx = layout.Idx;

const MkSafeList = collections.SafeList;
const exitOnOutOfMemory = collections.utils.exitOnOom;

pub const Store = struct {
    const Self = @This();

    env: *base.ModuleEnv,

    // Layout storage
    layouts: std.ArrayListUnmanaged(Layout),

    // Lists for parameterized layouts
    tuple_elems: collections.SafeList(Idx),

    // Cache to avoid duplicate work
    var_to_layout: std.AutoHashMapUnmanaged(Var, Idx),

    pub fn init(env: *base.ModuleEnv) Self {
        return .{
            .env = env,
            .layouts = std.ArrayListUnmanaged(Layout){},
            .tuple_elems = collections.SafeList(Idx).initCapacity(env.gpa, 512) catch |err| exitOnOutOfMemory(err),
            .var_to_layout = std.AutoHashMapUnmanaged(Var, Idx){},
        };
    }

    pub fn deinit(self: *Self) void {
        self.layouts.deinit(self.env.gpa);
        self.tuple_elems.deinit(self.env.gpa);
        self.var_to_layout.deinit(self.env.gpa);
    }

    fn insertLayout(self: *Self, layout_: Layout) Idx {
        const idx = self.layouts.items.len;
        self.layouts.append(self.env.gpa, layout_) catch |err| exitOnOutOfMemory(err);
        return Idx{
            .attributes = .{ ._padding = 0 },
            .idx = @intCast(idx),
        };
    }

    fn getLayout(self: *Self, idx: Idx) *Layout {
        return &self.layouts.items[idx.idx];
    }

    pub fn addTypeVar(
        self: *Store,
        var_store: *const types.Store,
        var_: Var,
    ) Idx {
        // Check cache first
        if (self.var_to_layout.get(var_)) |cached_idx| {
            return cached_idx;
        }

        // Stack to avoid recursion
        const WorkItem = struct {
            var_to_process: Var,
            parent_idx: ?Idx,
            parent_needs_box_elem: bool,
            parent_needs_list_elem: bool,
        };

        var work_stack = std.ArrayList(WorkItem).init(self.env.gpa);
        defer work_stack.deinit();

        var result: ?Idx = null;

        // Initialize with the initial variable
        var work_item = WorkItem{
            .var_to_process = var_,
            .parent_idx = null,
            .parent_needs_box_elem = false,
            .parent_needs_list_elem = false,
        };

        while (true) {
            switch (var_store.resolveVar(work_item.var_to_process).desc.content) {
                .structure => |flat_type| {
                    switch (flat_type) {
                        .str => {
                            // String layout - no parameters
                            const layout_idx = self.insertLayout(.str);

                            // Cache it
                            self.var_to_layout.put(self.env.gpa, work_item.var_to_process, layout_idx) catch |err| exitOnOutOfMemory(err);

                            // Update parent if needed
                            if (work_item.parent_idx) |parent_idx| {
                                const parent_layout = self.getLayout(parent_idx);
                                if (work_item.parent_needs_box_elem) {
                                    parent_layout.* = Layout{ .box = layout_idx };
                                } else if (work_item.parent_needs_list_elem) {
                                    parent_layout.* = Layout{ .list = layout_idx };
                                }
                            }

                            // If this was the root item, save the result
                            if (work_item.parent_idx == null) {
                                result = layout_idx;
                            }
                        },
                        .box => |elem_var| {
                            // Create placeholder box layout
                            const placeholder_idx = Idx{
                                .attributes = .{ ._padding = 0 },
                                .idx = 0,
                            };
                            const box_idx = self.insertLayout(Layout{ .box = placeholder_idx });

                            // Cache it immediately to handle recursive types
                            self.var_to_layout.put(self.env.gpa, work_item.var_to_process, box_idx) catch |err| exitOnOutOfMemory(err);

                            // Update parent if needed
                            if (work_item.parent_idx) |parent_idx| {
                                const parent_layout = self.getLayout(parent_idx);
                                if (work_item.parent_needs_box_elem) {
                                    parent_layout.* = Layout{ .box = box_idx };
                                } else if (work_item.parent_needs_list_elem) {
                                    parent_layout.* = Layout{ .list = box_idx };
                                }
                            }

                            // If this was the root item, save the result
                            if (work_item.parent_idx == null) {
                                result = box_idx;
                            }

                            // Process element
                            work_stack.append(.{
                                .var_to_process = elem_var,
                                .parent_idx = box_idx,
                                .parent_needs_box_elem = true,
                                .parent_needs_list_elem = false,
                            }) catch |err| exitOnOutOfMemory(err);
                        },
                        .list => |elem_var| {
                            // Create placeholder list layout
                            const placeholder_idx = Idx{
                                .attributes = .{ ._padding = 0 },
                                .idx = 0,
                            };
                            const list_idx = self.insertLayout(Layout{ .list = placeholder_idx });

                            // Cache it immediately to handle recursive types
                            self.var_to_layout.put(self.env.gpa, work_item.var_to_process, list_idx) catch |err| exitOnOutOfMemory(err);

                            // Update parent if needed
                            if (work_item.parent_idx) |parent_idx| {
                                const parent_layout = self.getLayout(parent_idx);
                                if (work_item.parent_needs_box_elem) {
                                    parent_layout.* = Layout{ .box = list_idx };
                                } else if (work_item.parent_needs_list_elem) {
                                    parent_layout.* = Layout{ .list = list_idx };
                                }
                            }

                            // If this was the root item, save the result
                            if (work_item.parent_idx == null) {
                                result = list_idx;
                            }

                            // Process element
                            work_stack.append(.{
                                .var_to_process = elem_var,
                                .parent_idx = list_idx,
                                .parent_needs_box_elem = false,
                                .parent_needs_list_elem = true,
                            }) catch |err| exitOnOutOfMemory(err);
                        },
                        .tuple => |tuple| {
                            // TODO
                            _ = tuple;
                        },
                        .num => |num| {
                            // TODO
                            _ = num;
                        },
                        .custom_type => |custom_type| {
                            // TODO
                            _ = custom_type;
                        },
                        .func => |func| {
                            // TODO
                            _ = func;
                        },
                        .record => |record| {
                            // TODO
                            _ = record;
                        },
                        .empty_record => {
                            // TODO
                        },
                        .tag_union => |tag_union| {
                            // TODO
                            _ = tag_union;
                        },
                        .empty_tag_union => {
                            // TODO
                        },
                    }
                },
                .flex_var => |_| {
                    // TODO: isn't there some scenario where we can send a flex var to the host as void*?
                },
                .rigid_var => |_| {
                    // TODO: isn't there some scenario where we can send a rigid var to the host as void*?
                },
                .alias => |alias| {
                    // Follow the alias by updating the work item
                    work_item.var_to_process = alias.backing_var;
                    continue;
                },
                .effectful => {
                    // doesn't make sense; ideally we'd move this out of Content to make impossible states impossible
                },
                .pure => {
                    // doesn't make sense; ideally we'd move this out of Content to make impossible states impossible
                },
                .err => {
                    // generate a crash prob
                },
            }

            // Check if there's more work to do
            if (work_stack.popOrNull()) |next_item| {
                work_item = next_item;

                // Check cache for the next item
                if (self.var_to_layout.get(work_item.var_to_process)) |cached_idx| {
                    // Update parent if needed
                    if (work_item.parent_idx) |parent_idx| {
                        const parent_layout = self.getLayout(parent_idx);
                        if (work_item.parent_needs_box_elem) {
                            parent_layout.* = Layout{ .box = cached_idx };
                        } else if (work_item.parent_needs_list_elem) {
                            parent_layout.* = Layout{ .list = cached_idx };
                        }
                    }

                    // If this was the root item, save the result
                    if (work_item.parent_idx == null) {
                        result = cached_idx;
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
    var type_store = types.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create a str type variable
    const str_var = type_store.freshFromContent(.{ .structure = .str });

    // Convert to layout
    const str_layout_idx = layout_store.addTypeVar(&type_store, str_var);

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
    var type_store = types.Store.init(&module_env);
    defer type_store.deinit();

    // Create layout store
    var layout_store = Store.init(&module_env);
    defer layout_store.deinit();

    // Create a str type variable
    const str_var = type_store.freshFromContent(.{ .structure = .str });

    // Create a list of str type variable
    const list_str_var = type_store.freshFromContent(.{ .structure = .{ .list = str_var } });

    // Convert to layout
    const list_layout_idx = layout_store.addTypeVar(&type_store, list_str_var);

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
    var type_store = types.Store.init(&module_env);
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
    const list_layout_idx = layout_store.addTypeVar(&type_store, list_box_str_var);

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
