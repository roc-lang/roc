const std = @import("std");
const render = @import("render_commands.zig");

pub const TextField = render.TextField;
pub const BoolField = render.BoolField;
pub const EventKind = render.EventKind;

pub const ScopeSiteKind = enum {
    component,
    state,
    when,
    each,
};

pub const TextFieldDescriptorIndexes = struct {
    text: ?usize = null,
    role: ?usize = null,
    label: ?usize = null,
    test_id: ?usize = null,
    value: ?usize = null,
    class: ?usize = null,

    pub fn get(self: TextFieldDescriptorIndexes, field: TextField) ?usize {
        return switch (field) {
            .text => self.text,
            .role => self.role,
            .label => self.label,
            .test_id => self.test_id,
            .value => self.value,
            .class => self.class,
        };
    }

    pub fn slot(self: *TextFieldDescriptorIndexes, field: TextField) *?usize {
        return switch (field) {
            .text => &self.text,
            .role => &self.role,
            .label => &self.label,
            .test_id => &self.test_id,
            .value => &self.value,
            .class => &self.class,
        };
    }
};

pub const BoolFieldDescriptorIndexes = struct {
    checked: ?usize = null,
    disabled: ?usize = null,

    pub fn get(self: BoolFieldDescriptorIndexes, field: BoolField) ?usize {
        return switch (field) {
            .checked => self.checked,
            .disabled => self.disabled,
        };
    }

    pub fn slot(self: *BoolFieldDescriptorIndexes, field: BoolField) *?usize {
        return switch (field) {
            .checked => &self.checked,
            .disabled => &self.disabled,
        };
    }
};

pub const EventDescriptorIndexes = struct {
    click: ?usize = null,
    input: ?usize = null,
    check: ?usize = null,
    pointer_down: ?usize = null,
    pointer_up: ?usize = null,
    pointer_enter: ?usize = null,
    pointer_leave: ?usize = null,

    pub fn get(self: EventDescriptorIndexes, kind: EventKind) ?usize {
        return switch (kind) {
            .click => self.click,
            .input => self.input,
            .check => self.check,
            .pointer_down => self.pointer_down,
            .pointer_up => self.pointer_up,
            .pointer_enter => self.pointer_enter,
            .pointer_leave => self.pointer_leave,
        };
    }

    pub fn slot(self: *EventDescriptorIndexes, kind: EventKind) *?usize {
        return switch (kind) {
            .click => &self.click,
            .input => &self.input,
            .check => &self.check,
            .pointer_down => &self.pointer_down,
            .pointer_up => &self.pointer_up,
            .pointer_enter => &self.pointer_enter,
            .pointer_leave => &self.pointer_leave,
        };
    }
};

pub const RenderElemIndex = struct {
    render_node: ?usize = null,
    first_child: ?u64 = null,
    last_child: ?u64 = null,
    next_sibling: ?u64 = null,

    pub fn empty(self: RenderElemIndex) bool {
        return self.render_node == null and self.first_child == null and self.last_child == null and self.next_sibling == null;
    }
};

pub const ElemDescriptorIndex = struct {
    element: ?usize = null,
    text_node: ?usize = null,
    signal_text_node: ?usize = null,
    static_text_attrs: TextFieldDescriptorIndexes = .{},
    signal_text_attrs: TextFieldDescriptorIndexes = .{},
    static_bool_attrs: BoolFieldDescriptorIndexes = .{},
    signal_bool_attrs: BoolFieldDescriptorIndexes = .{},
    events: EventDescriptorIndexes = .{},
};

pub const ScopeSiteDescriptorIndexes = struct {
    component: ?usize = null,
    state: ?usize = null,
    when: ?usize = null,
    each: ?usize = null,

    pub fn get(self: ScopeSiteDescriptorIndexes, kind: ScopeSiteKind) ?usize {
        return switch (kind) {
            .component => self.component,
            .state => self.state,
            .when => self.when,
            .each => self.each,
        };
    }

    pub fn slot(self: *ScopeSiteDescriptorIndexes, kind: ScopeSiteKind) *?usize {
        return switch (kind) {
            .component => &self.component,
            .state => &self.state,
            .when => &self.when,
            .each => &self.each,
        };
    }
};

pub const NodeDescriptorIndex = struct {
    scope_sites: ScopeSiteDescriptorIndexes = .{},
    state: ?usize = null,
    when: ?usize = null,
    each: ?usize = null,
};

pub fn setFreshIndex(slot: *?usize, value: usize) void {
    if (slot.* != null) @panic("descriptor stream recorded duplicate descriptor index");
    slot.* = value;
}

pub fn updateIndex(slot: *?usize, value: usize) void {
    if (slot.* == null) @panic("descriptor stream updated a missing descriptor index");
    slot.* = value;
}

pub fn clearIndex(slot: *?usize, expected: usize) void {
    const existing = slot.* orelse @panic("descriptor stream cleared a missing descriptor index");
    if (existing != expected) @panic("descriptor stream cleared the wrong descriptor index");
    slot.* = null;
}

test "field descriptor indexes round-trip by enum field" {
    var text: TextFieldDescriptorIndexes = .{};
    text.slot(.label).* = 12;
    text.slot(.class).* = 18;
    try std.testing.expectEqual(@as(?usize, 12), text.get(.label));
    try std.testing.expectEqual(@as(?usize, 18), text.get(.class));
    try std.testing.expectEqual(@as(?usize, null), text.get(.role));

    var bools: BoolFieldDescriptorIndexes = .{};
    bools.slot(.checked).* = 3;
    try std.testing.expectEqual(@as(?usize, 3), bools.get(.checked));
    try std.testing.expectEqual(@as(?usize, null), bools.get(.disabled));

    var events: EventDescriptorIndexes = .{};
    events.slot(.pointer_enter).* = 7;
    try std.testing.expectEqual(@as(?usize, 7), events.get(.pointer_enter));
    try std.testing.expectEqual(@as(?usize, null), events.get(.click));
}

test "scope site descriptor indexes round-trip by kind" {
    var indexes: ScopeSiteDescriptorIndexes = .{};
    indexes.slot(.component).* = 1;
    indexes.slot(.when).* = 5;

    try std.testing.expectEqual(@as(?usize, 1), indexes.get(.component));
    try std.testing.expectEqual(@as(?usize, 5), indexes.get(.when));
    try std.testing.expectEqual(@as(?usize, null), indexes.get(.each));
}

test "descriptor index mutation helpers preserve explicit slots" {
    var slot: ?usize = null;

    setFreshIndex(&slot, 4);
    try std.testing.expectEqual(@as(?usize, 4), slot);

    updateIndex(&slot, 9);
    try std.testing.expectEqual(@as(?usize, 9), slot);

    clearIndex(&slot, 9);
    try std.testing.expectEqual(@as(?usize, null), slot);
}

test "render elem index reports empty only when no render metadata remains" {
    var index: RenderElemIndex = .{};
    try std.testing.expect(index.empty());

    index.render_node = 1;
    try std.testing.expect(!index.empty());

    index.render_node = null;
    index.first_child = 3;
    try std.testing.expect(!index.empty());

    index.first_child = null;
    try std.testing.expect(index.empty());
}
