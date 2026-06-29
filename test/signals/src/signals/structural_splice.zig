const std = @import("std");
const descriptor_stream = @import("descriptor_stream.zig");
const scope_runtime = @import("scope_runtime.zig");

pub const EachSite = scope_runtime.EachSite;

pub const ReplacementTarget = union(enum) {
    scope: u64,
    each_site: EachSite,
};

pub const PatchTargets = struct {
    removed: ReplacementTarget,
    replacement: ReplacementTarget,
};

pub const Splice = struct {
    removed_elem_ids: []u64,
    touched_parent_ids: []u64,
    replacement_elem_ids: []u64,
    replacement_on_change_indices: []usize,
    replacement_mount_indices: []usize,

    pub fn deinit(self: Splice, allocator: std.mem.Allocator) void {
        allocator.free(self.removed_elem_ids);
        allocator.free(self.touched_parent_ids);
        allocator.free(self.replacement_elem_ids);
        allocator.free(self.replacement_on_change_indices);
        allocator.free(self.replacement_mount_indices);
    }
};

pub const SpliceAndTargets = struct {
    splice: Splice,
    targets: PatchTargets,
};

pub const RenderRemovalScan = struct {
    removed_elem_ids: []u64,
    touched_parent_ids: []u64,
    removed_render_count: usize,
    target_scan_count: usize,

    pub fn deinit(self: RenderRemovalScan, allocator: std.mem.Allocator) void {
        allocator.free(self.removed_elem_ids);
        allocator.free(self.touched_parent_ids);
    }
};

pub const ElemOwnedRemovalScratch = struct {
    element_indexes: std.ArrayListUnmanaged(usize) = .empty,
    text_node_indexes: std.ArrayListUnmanaged(usize) = .empty,
    signal_text_node_indexes: std.ArrayListUnmanaged(usize) = .empty,
    static_text_attr_indexes: std.ArrayListUnmanaged(usize) = .empty,
    signal_text_attr_indexes: std.ArrayListUnmanaged(usize) = .empty,
    static_bool_attr_indexes: std.ArrayListUnmanaged(usize) = .empty,
    signal_bool_attr_indexes: std.ArrayListUnmanaged(usize) = .empty,
    event_indexes: std.ArrayListUnmanaged(usize) = .empty,
    named_event_indexes: std.ArrayListUnmanaged(usize) = .empty,

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        self.element_indexes.deinit(allocator);
        self.text_node_indexes.deinit(allocator);
        self.signal_text_node_indexes.deinit(allocator);
        self.static_text_attr_indexes.deinit(allocator);
        self.signal_text_attr_indexes.deinit(allocator);
        self.static_bool_attr_indexes.deinit(allocator);
        self.signal_bool_attr_indexes.deinit(allocator);
        self.event_indexes.deinit(allocator);
        self.named_event_indexes.deinit(allocator);
        self.* = .{};
    }

    pub fn assertEmpty(self: *const @This()) void {
        if (self.element_indexes.items.len != 0 or
            self.text_node_indexes.items.len != 0 or
            self.signal_text_node_indexes.items.len != 0 or
            self.static_text_attr_indexes.items.len != 0 or
            self.signal_text_attr_indexes.items.len != 0 or
            self.static_bool_attr_indexes.items.len != 0 or
            self.signal_bool_attr_indexes.items.len != 0 or
            self.event_indexes.items.len != 0 or
            self.named_event_indexes.items.len != 0)
        {
            @panic("elem-owned removal scratch was already active");
        }
    }

    pub fn clearRetainingCapacity(self: *@This()) void {
        self.element_indexes.clearRetainingCapacity();
        self.text_node_indexes.clearRetainingCapacity();
        self.signal_text_node_indexes.clearRetainingCapacity();
        self.static_text_attr_indexes.clearRetainingCapacity();
        self.signal_text_attr_indexes.clearRetainingCapacity();
        self.static_bool_attr_indexes.clearRetainingCapacity();
        self.signal_bool_attr_indexes.clearRetainingCapacity();
        self.event_indexes.clearRetainingCapacity();
        self.named_event_indexes.clearRetainingCapacity();
    }

    pub fn appendDescriptorIndexes(self: *@This(), allocator: std.mem.Allocator, descriptor_index: anytype) void {
        appendRemovalIndex(allocator, &self.element_indexes, descriptor_index.element);
        appendRemovalIndex(allocator, &self.text_node_indexes, descriptor_index.text_node);
        appendRemovalIndex(allocator, &self.signal_text_node_indexes, descriptor_index.signal_text_node);
        appendTextFieldRemovalIndexes(allocator, &self.static_text_attr_indexes, descriptor_index.static_text_attrs);
        appendTextFieldRemovalIndexes(allocator, &self.signal_text_attr_indexes, descriptor_index.signal_text_attrs);
        appendBoolFieldRemovalIndexes(allocator, &self.static_bool_attr_indexes, descriptor_index.static_bool_attrs);
        appendBoolFieldRemovalIndexes(allocator, &self.signal_bool_attr_indexes, descriptor_index.signal_bool_attrs);
        appendEventRemovalIndexes(allocator, &self.event_indexes, descriptor_index.events);
    }

    pub fn sortDescending(self: *@This()) void {
        sortRemovalIndexesDescending(self.element_indexes.items);
        sortRemovalIndexesDescending(self.text_node_indexes.items);
        sortRemovalIndexesDescending(self.signal_text_node_indexes.items);
        sortRemovalIndexesDescending(self.static_text_attr_indexes.items);
        sortRemovalIndexesDescending(self.signal_text_attr_indexes.items);
        sortRemovalIndexesDescending(self.static_bool_attr_indexes.items);
        sortRemovalIndexesDescending(self.signal_bool_attr_indexes.items);
        sortRemovalIndexesDescending(self.event_indexes.items);
        sortRemovalIndexesDescending(self.named_event_indexes.items);
    }
};

fn u64SliceContains(items: []const u64, target: u64) bool {
    for (items) |item| {
        if (item == target) return true;
    }
    return false;
}

fn appendUniqueU64(allocator: std.mem.Allocator, values: *std.ArrayListUnmanaged(u64), value: u64) void {
    if (u64SliceContains(values.items, value)) return;
    values.append(allocator, value) catch @panic("out of memory");
}

pub fn scopeIsInTargetSet(target_scopes: []const bool, scope_id: u64) bool {
    if (scope_id >= target_scopes.len) @panic("descriptor referenced scope outside replacement target set");
    return target_scopes[@intCast(scope_id)];
}

fn removalIndexDesc(_: void, lhs: usize, rhs: usize) bool {
    return lhs > rhs;
}

pub fn sortRemovalIndexesDescending(indexes: []usize) void {
    std.mem.sort(usize, indexes, {}, removalIndexDesc);
}

pub fn appendRemovalIndex(allocator: std.mem.Allocator, indexes: *std.ArrayListUnmanaged(usize), index: ?usize) void {
    indexes.append(allocator, index orelse return) catch @panic("out of memory");
}

pub fn appendTextFieldRemovalIndexes(allocator: std.mem.Allocator, indexes: *std.ArrayListUnmanaged(usize), fields: anytype) void {
    appendRemovalIndex(allocator, indexes, fields.text);
    appendRemovalIndex(allocator, indexes, fields.role);
    appendRemovalIndex(allocator, indexes, fields.label);
    appendRemovalIndex(allocator, indexes, fields.test_id);
    appendRemovalIndex(allocator, indexes, fields.value);
    appendRemovalIndex(allocator, indexes, fields.class);
}

pub fn appendBoolFieldRemovalIndexes(allocator: std.mem.Allocator, indexes: *std.ArrayListUnmanaged(usize), fields: anytype) void {
    appendRemovalIndex(allocator, indexes, fields.checked);
    appendRemovalIndex(allocator, indexes, fields.disabled);
}

pub fn appendEventRemovalIndexes(allocator: std.mem.Allocator, indexes: *std.ArrayListUnmanaged(usize), events: anytype) void {
    appendRemovalIndex(allocator, indexes, events.click);
    appendRemovalIndex(allocator, indexes, events.input);
    appendRemovalIndex(allocator, indexes, events.check);
    appendRemovalIndex(allocator, indexes, events.pointer_down);
    appendRemovalIndex(allocator, indexes, events.pointer_up);
    appendRemovalIndex(allocator, indexes, events.pointer_enter);
    appendRemovalIndex(allocator, indexes, events.pointer_leave);
}

pub fn buildTargetScopeSet(comptime Scope: type, allocator: std.mem.Allocator, scratch: *std.ArrayListUnmanaged(bool), scopes: []const Scope, target: ReplacementTarget, lookup: anytype) []const bool {
    if (scratch.items.len != 0) @panic("replacement target scope scratch was already active");
    scratch.resize(allocator, scopes.len) catch @panic("out of memory");
    const target_scopes = scratch.items;
    for (scopes) |scope| {
        if (scope.scope_id >= target_scopes.len) @panic("scope descriptor id exceeded replacement target set");
        target_scopes[@intCast(scope.scope_id)] = lookup.scopeIsInTarget(scope.scope_id, target);
    }
    return target_scopes;
}

pub fn collectRenderRemovalScan(comptime Stream: type, allocator: std.mem.Allocator, stream: *const Stream, render_insert_index: usize, target_scopes: []const bool) RenderRemovalScan {
    if (render_insert_index > stream.render_nodes.items.len) @panic("structural replacement render insertion point is outside the active stream");

    var removed_elem_ids: std.ArrayListUnmanaged(u64) = .empty;
    errdefer removed_elem_ids.deinit(allocator);
    var touched_parent_ids: std.ArrayListUnmanaged(u64) = .empty;
    errdefer touched_parent_ids.deinit(allocator);

    var removed_render_count: usize = 0;
    var target_scan_count: usize = 0;
    var render_index = render_insert_index;
    while (render_index < stream.render_nodes.items.len) : (render_index += 1) {
        const node = stream.render_nodes.items[render_index];
        target_scan_count += 1;
        if (!scopeIsInTargetSet(target_scopes, descriptor_stream.renderNodeScopeId(Stream, stream, node))) break;
        removed_render_count += 1;
        removed_elem_ids.append(allocator, node.elem_id) catch @panic("out of memory");
        appendUniqueU64(allocator, &touched_parent_ids, descriptor_stream.renderNodeParentElemId(Stream, stream, node));
    }

    var touched_parent_write_index: usize = 0;
    for (touched_parent_ids.items) |parent_elem_id| {
        if (u64SliceContains(removed_elem_ids.items, parent_elem_id)) continue;
        touched_parent_ids.items[touched_parent_write_index] = parent_elem_id;
        touched_parent_write_index += 1;
    }
    touched_parent_ids.items.len = touched_parent_write_index;

    return .{
        .removed_elem_ids = removed_elem_ids.toOwnedSlice(allocator) catch @panic("out of memory"),
        .touched_parent_ids = touched_parent_ids.toOwnedSlice(allocator) catch @panic("out of memory"),
        .removed_render_count = removed_render_count,
        .target_scan_count = target_scan_count,
    };
}

pub fn renderElemIds(allocator: std.mem.Allocator, render_nodes: anytype) []u64 {
    const elem_ids = allocator.alloc(u64, render_nodes.len) catch @panic("out of memory");
    for (render_nodes, 0..) |node, index| {
        elem_ids[index] = node.elem_id;
    }
    return elem_ids;
}

pub fn indexRange(allocator: std.mem.Allocator, start: usize, count: usize) []usize {
    const indexes = allocator.alloc(usize, count) catch @panic("out of memory");
    for (indexes, 0..) |*index, offset| {
        index.* = start + offset;
    }
    return indexes;
}

pub fn adjustScopeSiteRenderInsertIndices(scope_sites: anytype, replace_index: usize, removed_render_count: usize, replacement_render_count: usize) void {
    for (scope_sites) |*desc| {
        desc.render_insert_index = descriptor_stream.adjustedRenderInsertIndex(desc.render_insert_index, replace_index, removed_render_count, replacement_render_count);
    }
}

test "structural splice owns replacement slices" {
    const allocator = std.testing.allocator;
    const splice = Splice{
        .removed_elem_ids = try allocator.dupe(u64, &.{ 1, 2 }),
        .touched_parent_ids = try allocator.dupe(u64, &.{3}),
        .replacement_elem_ids = try allocator.dupe(u64, &.{ 4, 5 }),
        .replacement_on_change_indices = try allocator.dupe(usize, &.{6}),
        .replacement_mount_indices = try allocator.dupe(usize, &.{7}),
    };
    splice.deinit(allocator);
}

test "structural splice allocates replacement metadata snapshots" {
    const RenderNode = struct {
        elem_id: u64,
    };
    const ScopeSite = struct {
        render_insert_index: usize,
    };
    const allocator = std.testing.allocator;

    const render_nodes = [_]RenderNode{ .{ .elem_id = 8 }, .{ .elem_id = 13 } };
    const elem_ids = renderElemIds(allocator, render_nodes[0..]);
    defer allocator.free(elem_ids);
    try std.testing.expectEqualSlices(u64, &.{ 8, 13 }, elem_ids);

    const indexes = indexRange(allocator, 4, 3);
    defer allocator.free(indexes);
    try std.testing.expectEqualSlices(usize, &.{ 4, 5, 6 }, indexes);

    var scope_sites = [_]ScopeSite{
        .{ .render_insert_index = 2 },
        .{ .render_insert_index = 9 },
    };
    adjustScopeSiteRenderInsertIndices(scope_sites[0..], 4, 2, 5);
    try std.testing.expectEqual(@as(usize, 2), scope_sites[0].render_insert_index);
    try std.testing.expectEqual(@as(usize, 12), scope_sites[1].render_insert_index);
}

test "structural splice collects removal indexes" {
    const TextFields = struct {
        text: ?usize = 1,
        role: ?usize = null,
        label: ?usize = 7,
        test_id: ?usize = null,
        value: ?usize = 3,
        class: ?usize = null,
    };

    var indexes: std.ArrayListUnmanaged(usize) = .empty;
    defer indexes.deinit(std.testing.allocator);

    appendTextFieldRemovalIndexes(std.testing.allocator, &indexes, TextFields{});
    sortRemovalIndexesDescending(indexes.items);

    try std.testing.expectEqualSlices(usize, &.{ 7, 3, 1 }, indexes.items);
    try std.testing.expect(scopeIsInTargetSet(&.{ false, true, false }, 1));
}

test "structural splice scratch collects descriptor indexes" {
    const DescriptorIndex = struct {
        element: ?usize = 3,
        text_node: ?usize = null,
        signal_text_node: ?usize = 9,
        static_text_attrs: struct {
            text: ?usize = 4,
            role: ?usize = null,
            label: ?usize = 1,
            test_id: ?usize = null,
            value: ?usize = null,
            class: ?usize = null,
        } = .{},
        signal_text_attrs: struct {
            text: ?usize = null,
            role: ?usize = null,
            label: ?usize = null,
            test_id: ?usize = null,
            value: ?usize = null,
            class: ?usize = null,
        } = .{},
        static_bool_attrs: struct {
            checked: ?usize = 2,
            disabled: ?usize = null,
        } = .{},
        signal_bool_attrs: struct {
            checked: ?usize = null,
            disabled: ?usize = null,
        } = .{},
        events: struct {
            click: ?usize = 8,
            input: ?usize = null,
            check: ?usize = null,
            pointer_down: ?usize = null,
            pointer_up: ?usize = null,
            pointer_enter: ?usize = null,
            pointer_leave: ?usize = null,
        } = .{},
    };

    var scratch: ElemOwnedRemovalScratch = .{};
    defer scratch.deinit(std.testing.allocator);

    scratch.assertEmpty();
    scratch.appendDescriptorIndexes(std.testing.allocator, DescriptorIndex{});
    scratch.sortDescending();

    try std.testing.expectEqualSlices(usize, &.{3}, scratch.element_indexes.items);
    try std.testing.expectEqualSlices(usize, &.{9}, scratch.signal_text_node_indexes.items);
    try std.testing.expectEqualSlices(usize, &.{ 4, 1 }, scratch.static_text_attr_indexes.items);
    try std.testing.expectEqualSlices(usize, &.{2}, scratch.static_bool_attr_indexes.items);
    try std.testing.expectEqualSlices(usize, &.{8}, scratch.event_indexes.items);

    scratch.clearRetainingCapacity();
    scratch.assertEmpty();
}

test "structural splice builds target scope set through explicit lookup" {
    const TestScope = struct {
        scope_id: u64,
    };
    const Lookup = struct {
        pub fn scopeIsInTarget(_: @This(), scope_id: u64, target: ReplacementTarget) bool {
            return switch (target) {
                .scope => |root_scope_id| scope_id >= root_scope_id,
                .each_site => false,
            };
        }
    };

    var scratch: std.ArrayListUnmanaged(bool) = .empty;
    defer scratch.deinit(std.testing.allocator);
    const scopes = [_]TestScope{ .{ .scope_id = 0 }, .{ .scope_id = 1 }, .{ .scope_id = 2 } };

    const target_scopes = buildTargetScopeSet(TestScope, std.testing.allocator, &scratch, scopes[0..], .{ .scope = 1 }, Lookup{});
    try std.testing.expectEqualSlices(bool, &.{ false, true, true }, target_scopes);
}

const TestStream = struct {
    pub const RenderNode = descriptor_stream.RenderNode;
    pub const ElementDesc = descriptor_stream.ElementDesc;
    pub const TextNodeDesc = descriptor_stream.TextNodeDesc;
    pub const SignalTextNodeDesc = descriptor_stream.TextNodeDesc;

    render_nodes: std.ArrayListUnmanaged(RenderNode) = .empty,
    elements: std.ArrayListUnmanaged(ElementDesc) = .empty,
    text_nodes: std.ArrayListUnmanaged(TextNodeDesc) = .empty,
    signal_text_nodes: std.ArrayListUnmanaged(SignalTextNodeDesc) = .empty,

    fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        self.render_nodes.deinit(allocator);
        self.elements.deinit(allocator);
        self.text_nodes.deinit(allocator);
        self.signal_text_nodes.deinit(allocator);
    }

    pub fn elemDescriptorIndex(self: *const @This(), elem_id: u64) ?descriptor_stream.ElemDescriptorIndex {
        for (self.elements.items, 0..) |desc, index| {
            if (desc.elem_id == elem_id) return .{ .element = index };
        }
        for (self.text_nodes.items, 0..) |desc, index| {
            if (desc.elem_id == elem_id) return .{ .text_node = index };
        }
        for (self.signal_text_nodes.items, 0..) |desc, index| {
            if (desc.elem_id == elem_id) return .{ .signal_text_node = index };
        }
        return null;
    }
};

test "structural splice scans removed render range" {
    const allocator = std.testing.allocator;
    var stream = TestStream{};
    defer stream.deinit(allocator);

    stream.render_nodes.appendSlice(allocator, &.{
        .{ .elem_id = 1, .kind = .element },
        .{ .elem_id = 2, .kind = .text },
        .{ .elem_id = 3, .kind = .text },
        .{ .elem_id = 4, .kind = .element },
    }) catch @panic("out of memory");
    stream.elements.appendSlice(allocator, &.{
        .{ .elem_id = 1, .parent_elem_id = 0, .scope_id = 10, .tag = "div" },
        .{ .elem_id = 4, .parent_elem_id = 0, .scope_id = 20, .tag = "aside" },
    }) catch @panic("out of memory");
    stream.text_nodes.appendSlice(allocator, &.{
        .{ .elem_id = 2, .parent_elem_id = 1, .scope_id = 10, .value = "a" },
        .{ .elem_id = 3, .parent_elem_id = 1, .scope_id = 10, .value = "b" },
    }) catch @panic("out of memory");

    var target_scopes = [_]bool{false} ** 21;
    target_scopes[10] = true;
    const scan = collectRenderRemovalScan(TestStream, allocator, &stream, 0, target_scopes[0..]);
    defer scan.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 3), scan.removed_render_count);
    try std.testing.expectEqual(@as(usize, 4), scan.target_scan_count);
    try std.testing.expectEqualSlices(u64, &.{ 1, 2, 3 }, scan.removed_elem_ids);
    try std.testing.expectEqualSlices(u64, &.{0}, scan.touched_parent_ids);
}
