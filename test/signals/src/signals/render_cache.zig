const std = @import("std");
const builtin = @import("builtin");
const render = @import("render_commands.zig");

pub const TextField = render.TextField;
pub const BoolField = render.BoolField;
pub const EventKind = render.EventKind;
pub const EventPayloadKind = render.EventPayloadKind;
pub const EventPayloadAccessor = render.EventPayloadAccessor;

pub const EventBinding = struct {
    event_id: u64,
    payload_accessor: EventPayloadAccessor,
};

pub const NamedEventBinding = struct {
    event_id: u64,
    options: u32,
    payload_kind: EventPayloadKind,
    payload_accessor: EventPayloadAccessor,
};

pub const EventBindings = struct {
    click: ?EventBinding = null,
    input: ?EventBinding = null,
    check: ?EventBinding = null,
    pointer_down: ?EventBinding = null,
    pointer_up: ?EventBinding = null,
    pointer_enter: ?EventBinding = null,
    pointer_leave: ?EventBinding = null,
};

pub fn eventBindingSlot(bindings: *EventBindings, kind: EventKind) *?EventBinding {
    return switch (kind) {
        .click => &bindings.click,
        .input => &bindings.input,
        .check => &bindings.check,
        .pointer_down => &bindings.pointer_down,
        .pointer_up => &bindings.pointer_up,
        .pointer_enter => &bindings.pointer_enter,
        .pointer_leave => &bindings.pointer_leave,
    };
}

pub const CustomTextAttr = struct {
    name: []const u8,
    value: []const u8,

    fn deinit(self: CustomTextAttr, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        allocator.free(self.value);
    }
};

pub const NamedEvent = struct {
    name: []const u8,
    event_id: u64,
    options: u32,
    payload_kind: EventPayloadKind,
    payload_accessor: EventPayloadAccessor,

    fn deinit(self: NamedEvent, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
    }
};

pub const ScalarNode = struct {
    active: bool = false,
    tag: ?[]const u8 = null,
    parent_id: ?u64 = null,
    children: std.ArrayListUnmanaged(u64) = .empty,
    bound_click_event: ?u64 = null,
    bound_click_accessor: ?EventPayloadAccessor = null,
    bound_input_event: ?u64 = null,
    bound_input_accessor: ?EventPayloadAccessor = null,
    bound_check_event: ?u64 = null,
    bound_check_accessor: ?EventPayloadAccessor = null,
    bound_pointer_down_event: ?u64 = null,
    bound_pointer_down_accessor: ?EventPayloadAccessor = null,
    bound_pointer_up_event: ?u64 = null,
    bound_pointer_up_accessor: ?EventPayloadAccessor = null,
    bound_pointer_enter_event: ?u64 = null,
    bound_pointer_enter_accessor: ?EventPayloadAccessor = null,
    bound_pointer_leave_event: ?u64 = null,
    bound_pointer_leave_accessor: ?EventPayloadAccessor = null,
    text: ?[]const u8 = null,
    role: ?[]const u8 = null,
    label: ?[]const u8 = null,
    test_id: ?[]const u8 = null,
    value: ?[]const u8 = null,
    class: ?[]const u8 = null,
    custom_text_attrs: std.ArrayListUnmanaged(CustomTextAttr) = .empty,
    named_events: std.ArrayListUnmanaged(NamedEvent) = .empty,
    checked: ?bool = null,
    disabled: ?bool = null,

    fn deinit(self: *ScalarNode, allocator: std.mem.Allocator) void {
        if (self.tag) |tag| allocator.free(tag);
        if (self.text) |text| allocator.free(text);
        if (self.role) |role| allocator.free(role);
        if (self.label) |label| allocator.free(label);
        if (self.test_id) |test_id| allocator.free(test_id);
        if (self.value) |value| allocator.free(value);
        if (self.class) |class| allocator.free(class);
        for (self.custom_text_attrs.items) |attr| {
            attr.deinit(allocator);
        }
        self.custom_text_attrs.deinit(allocator);
        for (self.named_events.items) |event| {
            event.deinit(allocator);
        }
        self.named_events.deinit(allocator);
        self.children.deinit(allocator);
        self.* = .{};
    }

    fn initActive(allocator: std.mem.Allocator, tag: []const u8) ScalarNode {
        return .{
            .active = true,
            .tag = allocator.dupe(u8, tag) catch @panic("out of memory"),
        };
    }

    fn textSlot(self: *ScalarNode, field: TextField) *?[]const u8 {
        return switch (field) {
            .text => &self.text,
            .role => &self.role,
            .label => &self.label,
            .test_id => &self.test_id,
            .value => &self.value,
            .class => &self.class,
        };
    }

    fn boolSlot(self: *ScalarNode, field: BoolField) *?bool {
        return switch (field) {
            .checked => &self.checked,
            .disabled => &self.disabled,
        };
    }

    pub fn customTextAttrIndex(self: *const ScalarNode, name: []const u8) ?usize {
        for (self.custom_text_attrs.items, 0..) |attr, index| {
            if (std.mem.eql(u8, attr.name, name)) return index;
        }
        return null;
    }

    pub fn namedEventIndex(self: *const ScalarNode, name: []const u8) ?usize {
        for (self.named_events.items, 0..) |event, index| {
            if (std.mem.eql(u8, event.name, name)) return index;
        }
        return null;
    }

    fn eventSlot(self: *ScalarNode, kind: EventKind) *?u64 {
        return switch (kind) {
            .click => &self.bound_click_event,
            .input => &self.bound_input_event,
            .check => &self.bound_check_event,
            .pointer_down => &self.bound_pointer_down_event,
            .pointer_up => &self.bound_pointer_up_event,
            .pointer_enter => &self.bound_pointer_enter_event,
            .pointer_leave => &self.bound_pointer_leave_event,
        };
    }

    fn eventAccessorSlot(self: *ScalarNode, kind: EventKind) *?EventPayloadAccessor {
        return switch (kind) {
            .click => &self.bound_click_accessor,
            .input => &self.bound_input_accessor,
            .check => &self.bound_check_accessor,
            .pointer_down => &self.bound_pointer_down_accessor,
            .pointer_up => &self.bound_pointer_up_accessor,
            .pointer_enter => &self.bound_pointer_enter_accessor,
            .pointer_leave => &self.bound_pointer_leave_accessor,
        };
    }
};

fn u64SliceIndex(items: []const u64, target: u64) ?usize {
    for (items, 0..) |item, index| {
        if (item == target) return index;
    }
    return null;
}

fn stableSubsequenceLength(indexes: []const usize, scratch: []usize) usize {
    var len: usize = 0;
    for (indexes) |index| {
        var low: usize = 0;
        var high = len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            if (scratch[mid] < index) {
                low = mid + 1;
            } else {
                high = mid;
            }
        }
        scratch[low] = index;
        if (low == len) len += 1;
    }
    return len;
}

pub fn Cache(comptime Ctx: type) type {
    return struct {
        const Self = @This();

        nodes: std.ArrayListUnmanaged(ScalarNode) = .empty,
        move_child_indexes: std.AutoHashMapUnmanaged(u64, usize) = .empty,
        move_old_indexes: std.ArrayListUnmanaged(usize) = .empty,
        move_stable_subsequence: std.ArrayListUnmanaged(usize) = .empty,

        pub fn deinit(self: *Self, ctx: Ctx.Handle) void {
            const allocator = Ctx.allocator(ctx);
            for (self.nodes.items) |*node| {
                node.deinit(allocator);
            }
            self.nodes.deinit(allocator);
            self.move_child_indexes.deinit(allocator);
            self.move_old_indexes.deinit(allocator);
            self.move_stable_subsequence.deinit(allocator);
            self.* = .{};
        }

        pub fn hasRoot(self: *const Self) bool {
            return self.nodes.items.len != 0 and self.nodes.items[0].active;
        }

        pub fn reset(self: *Self, ctx: Ctx.Handle) void {
            const allocator = Ctx.allocator(ctx);
            for (self.nodes.items) |*node| {
                node.deinit(allocator);
            }
            self.nodes.items.len = 0;
            self.nodes.append(allocator, ScalarNode.initActive(allocator, "root")) catch @panic("out of memory");
            Ctx.sink(ctx).reset();
        }

        fn ensureCacheNode(self: *Self, ctx: Ctx.Handle, elem_id: u64, tag: []const u8) bool {
            const allocator = Ctx.allocator(ctx);
            const index: usize = @intCast(elem_id);
            if (index > self.nodes.items.len) {
                @panic("render cache node ids must be dense and ordered by elem id");
            }
            if (index == self.nodes.items.len) {
                self.nodes.append(allocator, ScalarNode.initActive(allocator, tag)) catch @panic("out of memory");
                return true;
            }
            const node = &self.nodes.items[index];
            if (!node.active) {
                @panic("render descriptor referenced an inactive render cache identity");
            }
            if (node.tag == null or !std.mem.eql(u8, node.tag.?, tag)) {
                @panic("render descriptor changed the tag for an existing render cache identity");
            }
            return false;
        }

        pub fn appendNode(self: *Self, ctx: Ctx.Handle, elem_id: u64, parent_elem_id: u64, tag: []const u8) void {
            const created = self.ensureCacheNode(ctx, elem_id, tag);
            if (!created) @panic("initial render append reused an existing render cache identity");
            const parent = self.activeNode(parent_elem_id);
            const child = self.activeNode(elem_id);
            child.parent_id = parent_elem_id;
            parent.children.append(Ctx.allocator(ctx), elem_id) catch @panic("out of memory");
            Ctx.sink(ctx).appendNode(elem_id, parent_elem_id, tag);
        }

        pub fn ensureNode(self: *Self, ctx: Ctx.Handle, elem_id: u64, tag: []const u8, counts: *render.Counts) void {
            if (!self.ensureCacheNode(ctx, elem_id, tag)) return;
            Ctx.sink(ctx).ensureNode(elem_id, tag);
            counts.addCreateElement();
        }

        pub fn removeNode(self: *Self, ctx: Ctx.Handle, elem_id: u64, counts: *render.Counts) void {
            const allocator = Ctx.allocator(ctx);
            const index: usize = @intCast(elem_id);
            if (index >= self.nodes.items.len or !self.nodes.items[index].active) {
                @panic("render cache removed a missing element");
            }
            if (elem_id == 0) @panic("render cache attempted to remove the host DOM root");

            if (self.nodes.items[index].parent_id) |parent_id| {
                const parent_index: usize = @intCast(parent_id);
                if (parent_index < self.nodes.items.len and self.nodes.items[parent_index].active) {
                    const parent = &self.nodes.items[parent_index];
                    if (u64SliceIndex(parent.children.items, elem_id)) |child_index| {
                        _ = parent.children.orderedRemove(child_index);
                    }
                }
            }
            self.nodes.items[index].deinit(allocator);
            Ctx.sink(ctx).removeNode(elem_id);
            counts.addRemoveNode();
        }

        pub fn activeNode(self: *Self, elem_id: u64) *ScalarNode {
            const index: usize = @intCast(elem_id);
            if (index >= self.nodes.items.len or !self.nodes.items[index].active) {
                @panic("render command referenced missing element cache");
            }
            return &self.nodes.items[index];
        }

        pub fn namedEventNameAt(self: *Self, elem_id: u64, index: usize) ?[]const u8 {
            const events = self.activeNode(elem_id).named_events.items;
            if (index >= events.len) return null;
            return events[index].name;
        }

        pub fn customTextAttrNameAt(self: *Self, elem_id: u64, index: usize) ?[]const u8 {
            const attrs = self.activeNode(elem_id).custom_text_attrs.items;
            if (index >= attrs.len) return null;
            return attrs[index].name;
        }

        pub fn replaceChildren(self: *Self, ctx: Ctx.Handle, parent_elem_id: u64, next_child_ids: []const u64, counts: *render.Counts) void {
            const allocator = Ctx.allocator(ctx);
            const parent = self.activeNode(parent_elem_id);

            for (next_child_ids, 0..) |child_id, new_index| {
                const child = self.activeNode(child_id);
                const old_parent_id = child.parent_id;
                const old_child_index = if (old_parent_id) |id| u64SliceIndex(self.activeNode(id).children.items, child_id) else null;

                if (old_parent_id == null or old_parent_id.? != parent_elem_id or old_child_index == null) {
                    counts.addAppendChild();
                } else if (old_child_index.? != new_index) {
                    counts.addMoveBefore();
                }
                child.parent_id = parent_elem_id;
            }

            parent.children.deinit(allocator);
            parent.children = .empty;
            parent.children.appendSlice(allocator, next_child_ids) catch @panic("out of memory");
            Ctx.sink(ctx).replaceChildren(parent_elem_id, next_child_ids);
        }

        pub fn replaceChildrenForMoves(self: *Self, ctx: Ctx.Handle, parent_elem_id: u64, next_child_ids: []const u64, counts: *render.Counts) void {
            const allocator = Ctx.allocator(ctx);
            const parent = self.activeNode(parent_elem_id);
            if (parent.children.items.len != next_child_ids.len) @panic("pure structural move changed child count");

            const old_child_indexes = &self.move_child_indexes;
            old_child_indexes.clearRetainingCapacity();
            defer old_child_indexes.clearRetainingCapacity();
            for (parent.children.items, 0..) |child_id, index| {
                const entry = old_child_indexes.getOrPut(allocator, child_id) catch @panic("out of memory");
                if (entry.found_existing) @panic("parent child list contained duplicate element ids");
                entry.value_ptr.* = index;
            }

            self.move_old_indexes.resize(allocator, next_child_ids.len) catch @panic("out of memory");
            defer self.move_old_indexes.clearRetainingCapacity();
            const old_indexes_in_next_order = self.move_old_indexes.items;
            for (next_child_ids, 0..) |child_id, index| {
                const child = self.activeNode(child_id);
                if (child.parent_id == null or child.parent_id.? != parent_elem_id) @panic("pure structural move crossed parent boundary");
                old_indexes_in_next_order[index] = old_child_indexes.get(child_id) orelse @panic("pure structural move inserted a child");
            }

            self.move_stable_subsequence.resize(allocator, next_child_ids.len) catch @panic("out of memory");
            defer self.move_stable_subsequence.clearRetainingCapacity();
            const stable_scratch = self.move_stable_subsequence.items;
            const stable_len = stableSubsequenceLength(old_indexes_in_next_order, stable_scratch);
            const displaced_count = next_child_ids.len - stable_len;
            var displaced_index: usize = 0;
            while (displaced_index < displaced_count) : (displaced_index += 1) {
                counts.addMoveBefore();
            }

            for (next_child_ids) |child_id| {
                self.activeNode(child_id).parent_id = parent_elem_id;
            }
            parent.children.deinit(allocator);
            parent.children = .empty;
            parent.children.appendSlice(allocator, next_child_ids) catch @panic("out of memory");
            Ctx.sink(ctx).replaceChildrenForMoves(parent_elem_id, next_child_ids);
        }

        pub fn applyEventBinding(self: *Self, ctx: Ctx.Handle, elem_id: u64, kind: EventKind, binding: ?EventBinding, counts: *render.Counts) void {
            const node = self.activeNode(elem_id);
            const event_slot = node.eventSlot(kind);
            const accessor_slot = node.eventAccessorSlot(kind);
            const event_id = if (binding) |payload| payload.event_id else null;
            const payload_accessor = if (binding) |payload| payload.payload_accessor else null;
            if (event_slot.* == event_id and accessor_slot.* == payload_accessor) return;

            event_slot.* = event_id;
            accessor_slot.* = payload_accessor;
            if (event_id) |id| {
                Ctx.sink(ctx).bindEventKind(elem_id, kind, id, payload_accessor orelse @panic("event binding was missing payload accessor"));
            } else {
                Ctx.sink(ctx).clearEvent(elem_id, kind);
            }
            counts.addEventBinding();
        }

        pub fn applyNamedEventBinding(self: *Self, ctx: Ctx.Handle, elem_id: u64, name: []const u8, binding: ?NamedEventBinding, counts: *render.Counts) void {
            const allocator = Ctx.allocator(ctx);
            const node = self.activeNode(elem_id);
            const existing_index = node.namedEventIndex(name);

            if (binding) |next| {
                if (existing_index) |index| {
                    const existing = &node.named_events.items[index];
                    if (existing.event_id == next.event_id and
                        existing.options == next.options and
                        existing.payload_kind == next.payload_kind and
                        existing.payload_accessor == next.payload_accessor)
                    {
                        return;
                    }

                    existing.event_id = next.event_id;
                    existing.options = next.options;
                    existing.payload_kind = next.payload_kind;
                    existing.payload_accessor = next.payload_accessor;
                } else {
                    const name_copy = allocator.dupe(u8, name) catch @panic("out of memory");
                    node.named_events.append(allocator, .{
                        .name = name_copy,
                        .event_id = next.event_id,
                        .options = next.options,
                        .payload_kind = next.payload_kind,
                        .payload_accessor = next.payload_accessor,
                    }) catch {
                        allocator.free(name_copy);
                        @panic("out of memory");
                    };
                }

                Ctx.sink(ctx).bindEventName(elem_id, name, next.event_id, next.options, next.payload_kind, next.payload_accessor);
                counts.addEventBinding();
                return;
            }

            const index = existing_index orelse return;
            const removed = node.named_events.orderedRemove(index);
            Ctx.sink(ctx).clearEventName(elem_id, removed.name);
            removed.deinit(allocator);
            counts.addEventBinding();
        }

        pub fn debugAssertMatchesSink(self: *Self, ctx: Ctx.Handle) void {
            if (comptime builtin.mode != .Debug) return;

            for (self.nodes.items, 0..) |cached, index| {
                Ctx.sink(ctx).debugAssertNode(
                    @intCast(index),
                    cached.active,
                    cached.tag,
                    cached.parent_id,
                    cached.children.items,
                    cached.bound_click_event,
                    cached.bound_input_event,
                    cached.bound_check_event,
                    cached.bound_pointer_down_event,
                    cached.bound_pointer_up_event,
                    cached.bound_pointer_enter_event,
                    cached.bound_pointer_leave_event,
                );
            }
        }

        pub fn applyTextField(self: *Self, ctx: Ctx.Handle, elem_id: u64, field: TextField, value: []const u8) bool {
            const allocator = Ctx.allocator(ctx);
            const slot = self.activeNode(elem_id).textSlot(field);
            if (slot.*) |existing| {
                if (std.mem.eql(u8, existing, value)) return false;
            }

            const value_copy = allocator.dupe(u8, value) catch @panic("out of memory");
            if (slot.*) |existing| allocator.free(existing);
            slot.* = value_copy;
            Ctx.sink(ctx).applyTextField(elem_id, field, value);
            return true;
        }

        pub fn applyTextAttr(self: *Self, ctx: Ctx.Handle, elem_id: u64, name: []const u8, value: []const u8) bool {
            const allocator = Ctx.allocator(ctx);
            const node = self.activeNode(elem_id);
            if (node.customTextAttrIndex(name)) |index| {
                const attr = &node.custom_text_attrs.items[index];
                if (std.mem.eql(u8, attr.value, value)) return false;

                const value_copy = allocator.dupe(u8, value) catch @panic("out of memory");
                allocator.free(attr.value);
                attr.value = value_copy;
                Ctx.sink(ctx).applyTextAttr(elem_id, name, value);
                return true;
            }

            const name_copy = allocator.dupe(u8, name) catch @panic("out of memory");
            const value_copy = allocator.dupe(u8, value) catch {
                allocator.free(name_copy);
                @panic("out of memory");
            };
            node.custom_text_attrs.append(allocator, .{
                .name = name_copy,
                .value = value_copy,
            }) catch {
                allocator.free(name_copy);
                allocator.free(value_copy);
                @panic("out of memory");
            };
            Ctx.sink(ctx).applyTextAttr(elem_id, name, value);
            return true;
        }

        pub fn applyBoolField(self: *Self, ctx: Ctx.Handle, elem_id: u64, field: BoolField, value: bool) bool {
            const slot = self.activeNode(elem_id).boolSlot(field);
            if (slot.*) |existing| {
                if (existing == value) return false;
            }

            slot.* = value;
            Ctx.sink(ctx).applyBoolField(elem_id, field, value);
            return true;
        }

        pub fn clearTextField(self: *Self, ctx: Ctx.Handle, elem_id: u64, field: TextField) bool {
            const allocator = Ctx.allocator(ctx);
            const slot = self.activeNode(elem_id).textSlot(field);
            const existing = slot.* orelse return false;
            allocator.free(existing);
            slot.* = null;
            Ctx.sink(ctx).clearTextField(elem_id, field);
            return true;
        }

        pub fn clearTextAttr(self: *Self, ctx: Ctx.Handle, elem_id: u64, name: []const u8) bool {
            const allocator = Ctx.allocator(ctx);
            const node = self.activeNode(elem_id);
            const index = node.customTextAttrIndex(name) orelse return false;
            const removed = node.custom_text_attrs.orderedRemove(index);
            Ctx.sink(ctx).clearTextAttr(elem_id, removed.name);
            removed.deinit(allocator);
            return true;
        }

        pub fn clearBoolField(self: *Self, ctx: Ctx.Handle, elem_id: u64, field: BoolField) bool {
            const slot = self.activeNode(elem_id).boolSlot(field);
            const existing = slot.* orelse return false;
            slot.* = null;
            if (!existing) return false;
            Ctx.sink(ctx).clearBoolField(elem_id, field);
            return true;
        }
    };
}

const TestHost = struct {
    apply_text_field_count: u64 = 0,
    apply_text_attr_count: u64 = 0,
    clear_text_attr_count: u64 = 0,
    bind_event_count: u64 = 0,
    clear_event_count: u64 = 0,
    bind_named_event_count: u64 = 0,
    clear_named_event_count: u64 = 0,
};

const TestCtx = struct {
    pub const Handle = *TestHost;
    pub const Sink = TestSink;

    pub fn allocator(_: Handle) std.mem.Allocator {
        return std.testing.allocator;
    }

    pub fn sink(host: Handle) Sink {
        return .{ .host = host };
    }
};

const TestSink = struct {
    host: *TestHost,

    pub fn reset(_: TestSink) void {}
    pub fn appendNode(_: TestSink, _: u64, _: u64, _: []const u8) void {}
    pub fn ensureNode(_: TestSink, _: u64, _: []const u8) void {}
    pub fn removeNode(_: TestSink, _: u64) void {}
    pub fn replaceChildren(_: TestSink, _: u64, _: []const u64) void {}
    pub fn replaceChildrenForMoves(_: TestSink, _: u64, _: []const u64) void {}
    pub fn applyTextField(self: TestSink, _: u64, _: TextField, _: []const u8) void {
        self.host.apply_text_field_count += 1;
    }
    pub fn applyTextAttr(self: TestSink, _: u64, _: []const u8, _: []const u8) void {
        self.host.apply_text_attr_count += 1;
    }
    pub fn applyBoolField(_: TestSink, _: u64, _: BoolField, _: bool) void {}
    pub fn clearTextField(_: TestSink, _: u64, _: TextField) void {}
    pub fn clearTextAttr(self: TestSink, _: u64, _: []const u8) void {
        self.host.clear_text_attr_count += 1;
    }
    pub fn clearBoolField(_: TestSink, _: u64, _: BoolField) void {}
    pub fn bindEventKind(self: TestSink, _: u64, _: EventKind, _: u64, _: EventPayloadAccessor) void {
        self.host.bind_event_count += 1;
    }
    pub fn clearEvent(self: TestSink, _: u64, _: EventKind) void {
        self.host.clear_event_count += 1;
    }
    pub fn bindEventName(self: TestSink, _: u64, _: []const u8, _: u64, _: u32, _: EventPayloadKind, _: EventPayloadAccessor) void {
        self.host.bind_named_event_count += 1;
    }
    pub fn clearEventName(self: TestSink, _: u64, _: []const u8) void {
        self.host.clear_named_event_count += 1;
    }
    pub fn debugAssertNode(_: TestSink, _: u64, _: bool, _: ?[]const u8, _: ?u64, _: []const u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64) void {}
};

test "applying unchanged text field emits no duplicate command" {
    var host = TestHost{};
    var cache: Cache(TestCtx) = .{};
    defer cache.deinit(&host);

    cache.reset(&host);
    var counts: render.Counts = .{};
    cache.ensureNode(&host, 1, "div", &counts);

    try std.testing.expect(cache.applyTextField(&host, 1, .text, "hello"));
    try std.testing.expect(!cache.applyTextField(&host, 1, .text, "hello"));
    try std.testing.expectEqual(@as(u64, 1), host.apply_text_field_count);
}

test "reordering children counts only displaced moves" {
    var host = TestHost{};
    var cache: Cache(TestCtx) = .{};
    defer cache.deinit(&host);

    cache.reset(&host);
    var counts: render.Counts = .{};
    cache.ensureNode(&host, 1, "div", &counts);
    cache.ensureNode(&host, 2, "div", &counts);
    cache.ensureNode(&host, 3, "div", &counts);
    cache.replaceChildren(&host, 0, &.{ 1, 2, 3 }, &counts);

    counts = .{};
    cache.replaceChildrenForMoves(&host, 0, &.{ 2, 1, 3 }, &counts);

    try std.testing.expectEqual(@as(u64, 1), counts.move_before);
    try std.testing.expectEqual(@as(u64, 1), counts.total);
}

test "unchanged event binding emits no duplicate command" {
    var host = TestHost{};
    var cache: Cache(TestCtx) = .{};
    defer cache.deinit(&host);

    cache.reset(&host);
    var counts: render.Counts = .{};
    cache.ensureNode(&host, 1, "button", &counts);

    const binding = EventBinding{ .event_id = 1, .payload_accessor = .none };
    cache.applyEventBinding(&host, 1, .click, binding, &counts);
    cache.applyEventBinding(&host, 1, .click, binding, &counts);
    try std.testing.expectEqual(@as(u64, 1), counts.bind_event);
    try std.testing.expectEqual(@as(u64, 1), host.bind_event_count);

    cache.applyEventBinding(&host, 1, .click, null, &counts);
    cache.applyEventBinding(&host, 1, .click, null, &counts);
    try std.testing.expectEqual(@as(u64, 2), counts.bind_event);
    try std.testing.expectEqual(@as(u64, 1), host.clear_event_count);
}

test "event binding slots are keyed by event kind" {
    var bindings = EventBindings{};
    const click = EventBinding{ .event_id = 1, .payload_accessor = .none };
    const input = EventBinding{ .event_id = 2, .payload_accessor = .target_value };
    const pointer_down = EventBinding{ .event_id = 3, .payload_accessor = .target_checked };

    eventBindingSlot(&bindings, .click).* = click;
    eventBindingSlot(&bindings, .input).* = input;
    eventBindingSlot(&bindings, .pointer_down).* = pointer_down;

    try std.testing.expectEqual(click, bindings.click.?);
    try std.testing.expectEqual(input, bindings.input.?);
    try std.testing.expectEqual(pointer_down, bindings.pointer_down.?);
    try std.testing.expectEqual(@as(?EventBinding, null), bindings.check);
    try std.testing.expectEqual(@as(?EventBinding, null), bindings.pointer_up);
    try std.testing.expectEqual(@as(?EventBinding, null), bindings.pointer_enter);
    try std.testing.expectEqual(@as(?EventBinding, null), bindings.pointer_leave);
}

test "custom text attr application and clear are idempotent" {
    var host = TestHost{};
    var cache: Cache(TestCtx) = .{};
    defer cache.deinit(&host);

    cache.reset(&host);
    var counts: render.Counts = .{};
    cache.ensureNode(&host, 1, "div", &counts);

    try std.testing.expect(cache.applyTextAttr(&host, 1, "data-x", "a"));
    try std.testing.expect(!cache.applyTextAttr(&host, 1, "data-x", "a"));
    try std.testing.expect(cache.applyTextAttr(&host, 1, "data-x", "b"));
    try std.testing.expectEqual(@as(u64, 2), host.apply_text_attr_count);

    try std.testing.expect(!cache.clearTextAttr(&host, 1, "data-missing"));
    try std.testing.expect(cache.clearTextAttr(&host, 1, "data-x"));
    try std.testing.expect(!cache.clearTextAttr(&host, 1, "data-x"));
    try std.testing.expectEqual(@as(u64, 1), host.clear_text_attr_count);
}

test "named event replacement and clear are idempotent" {
    var host = TestHost{};
    var cache: Cache(TestCtx) = .{};
    defer cache.deinit(&host);

    cache.reset(&host);
    var counts: render.Counts = .{};
    cache.ensureNode(&host, 1, "form", &counts);

    const first = NamedEventBinding{
        .event_id = 1,
        .options = 0,
        .payload_kind = .unit,
        .payload_accessor = .none,
    };
    const second = NamedEventBinding{
        .event_id = 2,
        .options = render.listener_option_prevent_default,
        .payload_kind = .str,
        .payload_accessor = .target_value,
    };

    cache.applyNamedEventBinding(&host, 1, "submit", first, &counts);
    cache.applyNamedEventBinding(&host, 1, "submit", first, &counts);
    try std.testing.expectEqualStrings("submit", cache.namedEventNameAt(1, 0).?);

    cache.applyNamedEventBinding(&host, 1, "submit", second, &counts);
    try std.testing.expectEqual(@as(u64, 2), host.bind_named_event_count);
    try std.testing.expectEqual(@as(u64, 2), counts.bind_event);

    cache.applyNamedEventBinding(&host, 1, "submit", null, &counts);
    cache.applyNamedEventBinding(&host, 1, "submit", null, &counts);
    try std.testing.expectEqual(@as(u64, 1), host.clear_named_event_count);
    try std.testing.expectEqual(@as(u64, 3), counts.bind_event);
}
