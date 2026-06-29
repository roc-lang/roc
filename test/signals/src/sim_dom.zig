const std = @import("std");

const signals = @import("signals");
const render = signals.render;
const spec_parser = @import("spec/spec_parser.zig");

pub const Element = struct {
    id: u64,
    tag: []const u8,
    role: ?[]const u8,
    label: ?[]const u8,
    test_id: ?[]const u8,
    class: ?[]const u8,
    text: ?[]const u8,
    value: ?[]const u8,
    checked: bool,
    disabled: bool,
    parent_id: ?u64,
    children: std.ArrayListUnmanaged(u64),
    bound_click_event: ?u64,
    bound_input_event: ?u64,
    bound_check_event: ?u64,
    bound_pointer_down_event: ?u64,
    bound_pointer_up_event: ?u64,
    bound_pointer_enter_event: ?u64,
    bound_pointer_leave_event: ?u64,
    active: bool,
    text_update_count: u64,
    value_update_count: u64,
    checked_update_count: u64,
    disabled_update_count: u64,
    attrs: std.ArrayListUnmanaged(TextAttr),
    named_events: std.ArrayListUnmanaged(NamedEvent),

    pub fn init(id: u64, tag: []const u8) Element {
        return .{
            .id = id,
            .tag = tag,
            .role = null,
            .label = null,
            .test_id = null,
            .class = null,
            .text = null,
            .value = null,
            .checked = false,
            .disabled = false,
            .parent_id = null,
            .children = .empty,
            .bound_click_event = null,
            .bound_input_event = null,
            .bound_check_event = null,
            .bound_pointer_down_event = null,
            .bound_pointer_up_event = null,
            .bound_pointer_enter_event = null,
            .bound_pointer_leave_event = null,
            .active = true,
            .text_update_count = 0,
            .value_update_count = 0,
            .checked_update_count = 0,
            .disabled_update_count = 0,
            .attrs = .empty,
            .named_events = .empty,
        };
    }

    pub fn deinit(self: *Element, allocator: std.mem.Allocator) void {
        allocator.free(self.tag);
        if (self.role) |role| allocator.free(role);
        if (self.label) |label| allocator.free(label);
        if (self.test_id) |test_id| allocator.free(test_id);
        if (self.class) |class| allocator.free(class);
        if (self.text) |text| allocator.free(text);
        if (self.value) |value| allocator.free(value);
        for (self.attrs.items) |attr| {
            attr.deinit(allocator);
        }
        self.attrs.deinit(allocator);
        for (self.named_events.items) |event| {
            event.deinit(allocator);
        }
        self.named_events.deinit(allocator);
        self.children.deinit(allocator);
    }

    pub fn textAttrIndex(self: *const Element, name: []const u8) ?usize {
        for (self.attrs.items, 0..) |attr, index| {
            if (std.mem.eql(u8, attr.name, name)) return index;
        }
        return null;
    }

    pub fn namedEventIndex(self: *const Element, name: []const u8) ?usize {
        for (self.named_events.items, 0..) |event, index| {
            if (std.mem.eql(u8, event.name, name)) return index;
        }
        return null;
    }
};

pub const TextAttr = struct {
    name: []const u8,
    value: []const u8,

    pub fn deinit(self: TextAttr, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        allocator.free(self.value);
    }
};

pub const NamedEvent = struct {
    name: []const u8,
    event_id: u64,
    options: u32,
    payload_kind: render.EventPayloadKind,
    payload_accessor: render.EventPayloadAccessor,

    pub fn deinit(self: NamedEvent, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
    }
};

pub fn implicitRole(elem: *const Element) ?[]const u8 {
    if (elem.role) |role| return role;
    if (std.mem.eql(u8, elem.tag, "button")) return "button";
    if (std.mem.eql(u8, elem.tag, "h1") or
        std.mem.eql(u8, elem.tag, "h2") or
        std.mem.eql(u8, elem.tag, "h3") or
        std.mem.eql(u8, elem.tag, "h4") or
        std.mem.eql(u8, elem.tag, "h5") or
        std.mem.eql(u8, elem.tag, "h6")) return "heading";
    if (std.mem.eql(u8, elem.tag, "section")) return "region";
    return null;
}

pub fn accessibleName(elem: *const Element) []const u8 {
    if (elem.label) |label| return label;
    if (elem.text) |text| return text;
    if (elem.value) |value| return value;
    return "";
}

pub fn matchesLocator(elem: *const Element, locator: spec_parser.Locator) bool {
    return switch (locator.kind) {
        .none => false,
        .role_name => blk: {
            const role = implicitRole(elem) orelse break :blk false;
            const expected_role = locator.role orelse break :blk false;
            const expected_name = locator.name orelse break :blk false;
            break :blk std.mem.eql(u8, role, expected_role) and std.mem.eql(u8, accessibleName(elem), expected_name);
        },
        .label => blk: {
            const expected = locator.label orelse break :blk false;
            const label = elem.label orelse break :blk false;
            break :blk std.mem.eql(u8, label, expected);
        },
        .text => blk: {
            const expected = locator.text orelse break :blk false;
            const text = elem.text orelse break :blk false;
            break :blk std.mem.eql(u8, text, expected);
        },
        .test_id => blk: {
            const expected = locator.test_id orelse break :blk false;
            const test_id = elem.test_id orelse break :blk false;
            break :blk std.mem.eql(u8, test_id, expected);
        },
    };
}

fn replaceOwnedString(allocator: std.mem.Allocator, field: *?[]const u8, value: []const u8) bool {
    if (field.*) |existing| {
        if (std.mem.eql(u8, existing, value)) return false;
        allocator.free(existing);
    }
    field.* = allocator.dupe(u8, value) catch std.process.exit(1);
    return true;
}

pub fn setOwnedString(allocator: std.mem.Allocator, field: *?[]const u8, value: []const u8) void {
    if (field.*) |existing| {
        allocator.free(existing);
    }
    field.* = allocator.dupe(u8, value) catch std.process.exit(1);
}

pub fn clearOwnedString(allocator: std.mem.Allocator, field: *?[]const u8) void {
    if (field.*) |existing| {
        allocator.free(existing);
    }
    field.* = null;
}

pub fn setText(allocator: std.mem.Allocator, elem: *Element, text: []const u8) void {
    setOwnedString(allocator, &elem.text, text);
    elem.text_update_count += 1;
}

pub fn setValueIfChanged(allocator: std.mem.Allocator, elem: *Element, value: []const u8) bool {
    if (replaceOwnedString(allocator, &elem.value, value)) {
        elem.value_update_count += 1;
        return true;
    }
    return false;
}

pub fn setValue(allocator: std.mem.Allocator, elem: *Element, value: []const u8) void {
    setOwnedString(allocator, &elem.value, value);
    elem.value_update_count += 1;
}

pub fn clearText(allocator: std.mem.Allocator, elem: *Element) void {
    clearOwnedString(allocator, &elem.text);
    elem.text_update_count += 1;
}

pub fn clearValue(allocator: std.mem.Allocator, elem: *Element) void {
    clearOwnedString(allocator, &elem.value);
    elem.value_update_count += 1;
}

pub fn setTextAttr(allocator: std.mem.Allocator, elem: *Element, name: []const u8, value: []const u8) void {
    if (elem.textAttrIndex(name)) |index| {
        const attr = &elem.attrs.items[index];
        allocator.free(attr.value);
        attr.value = allocator.dupe(u8, value) catch std.process.exit(1);
        return;
    }

    const name_copy = allocator.dupe(u8, name) catch std.process.exit(1);
    const value_copy = allocator.dupe(u8, value) catch {
        allocator.free(name_copy);
        std.process.exit(1);
    };
    elem.attrs.append(allocator, .{
        .name = name_copy,
        .value = value_copy,
    }) catch {
        allocator.free(name_copy);
        allocator.free(value_copy);
        std.process.exit(1);
    };
}

pub fn clearTextAttr(allocator: std.mem.Allocator, elem: *Element, name: []const u8) void {
    const index = elem.textAttrIndex(name) orelse return;
    const removed = elem.attrs.orderedRemove(index);
    removed.deinit(allocator);
}

pub fn textAttr(elem: *const Element, name: []const u8) ?[]const u8 {
    const index = elem.textAttrIndex(name) orelse return null;
    return elem.attrs.items[index].value;
}

pub fn setCheckedIfChanged(elem: *Element, checked: bool) bool {
    if (elem.checked != checked) {
        elem.checked = checked;
        elem.checked_update_count += 1;
        return true;
    }
    return false;
}

pub fn setChecked(elem: *Element, checked: bool) void {
    elem.checked = checked;
    elem.checked_update_count += 1;
}

pub fn setDisabled(elem: *Element, disabled: bool) void {
    elem.disabled = disabled;
    elem.disabled_update_count += 1;
}

pub fn childIndex(elem: *const Element, child_id: u64) ?usize {
    for (elem.children.items, 0..) |id, index| {
        if (id == child_id) return index;
    }
    return null;
}

pub fn namedEvent(elem: *const Element, name: []const u8) ?NamedEvent {
    const index = elem.namedEventIndex(name) orelse return null;
    return elem.named_events.items[index];
}

pub fn bindEventKind(elem: *Element, kind: render.EventKind, event_id: u64) void {
    switch (kind) {
        .click => elem.bound_click_event = event_id,
        .input => elem.bound_input_event = event_id,
        .check => elem.bound_check_event = event_id,
        .pointer_down => elem.bound_pointer_down_event = event_id,
        .pointer_up => elem.bound_pointer_up_event = event_id,
        .pointer_enter => elem.bound_pointer_enter_event = event_id,
        .pointer_leave => elem.bound_pointer_leave_event = event_id,
    }
}

pub fn clearEventKind(elem: *Element, kind: render.EventKind) void {
    switch (kind) {
        .click => elem.bound_click_event = null,
        .input => elem.bound_input_event = null,
        .check => elem.bound_check_event = null,
        .pointer_down => elem.bound_pointer_down_event = null,
        .pointer_up => elem.bound_pointer_up_event = null,
        .pointer_enter => elem.bound_pointer_enter_event = null,
        .pointer_leave => elem.bound_pointer_leave_event = null,
    }
}

pub fn bindEventName(allocator: std.mem.Allocator, elem: *Element, name: []const u8, event_id: u64, options: u32, payload_kind: render.EventPayloadKind, payload_accessor: render.EventPayloadAccessor) void {
    if (elem.namedEventIndex(name)) |index| {
        const event = &elem.named_events.items[index];
        event.event_id = event_id;
        event.options = options;
        event.payload_kind = payload_kind;
        event.payload_accessor = payload_accessor;
        return;
    }

    const name_copy = allocator.dupe(u8, name) catch std.process.exit(1);
    elem.named_events.append(allocator, .{
        .name = name_copy,
        .event_id = event_id,
        .options = options,
        .payload_kind = payload_kind,
        .payload_accessor = payload_accessor,
    }) catch {
        allocator.free(name_copy);
        std.process.exit(1);
    };
}

pub fn clearEventName(allocator: std.mem.Allocator, elem: *Element, name: []const u8) void {
    const index = elem.namedEventIndex(name) orelse return;
    const removed = elem.named_events.orderedRemove(index);
    removed.deinit(allocator);
}

pub fn reset(allocator: std.mem.Allocator, elements: *std.ArrayListUnmanaged(Element)) void {
    for (elements.items) |*elem| {
        elem.deinit(allocator);
    }
    elements.items.len = 0;

    const root_tag = allocator.dupe(u8, "root") catch std.process.exit(1);
    elements.append(allocator, Element.init(0, root_tag)) catch {
        allocator.free(root_tag);
        std.process.exit(1);
    };
}

pub fn appendDetached(allocator: std.mem.Allocator, elements: *std.ArrayListUnmanaged(Element), elem_id: u64, tag: []const u8) void {
    const tag_copy = allocator.dupe(u8, tag) catch std.process.exit(1);
    elements.append(allocator, Element.init(elem_id, tag_copy)) catch {
        allocator.free(tag_copy);
        std.process.exit(1);
    };
}

pub fn appendChild(allocator: std.mem.Allocator, parent: *Element, child: *Element) void {
    child.parent_id = parent.id;
    parent.children.append(allocator, child.id) catch std.process.exit(1);
}

pub fn removeChildAt(parent: *Element, child_index: usize) void {
    _ = parent.children.orderedRemove(child_index);
}

pub fn deactivateRemovedNode(allocator: std.mem.Allocator, elem: *Element) void {
    elem.active = false;
    elem.parent_id = null;
    elem.bound_click_event = null;
    elem.bound_input_event = null;
    elem.bound_check_event = null;
    elem.bound_pointer_down_event = null;
    elem.bound_pointer_up_event = null;
    elem.bound_pointer_enter_event = null;
    elem.bound_pointer_leave_event = null;
    for (elem.named_events.items) |event| {
        event.deinit(allocator);
    }
    elem.named_events.clearRetainingCapacity();
    elem.children.deinit(allocator);
    elem.children = .empty;
}

pub fn replaceChildren(allocator: std.mem.Allocator, elements: []Element, parent: *Element, next_child_ids: []const u64) void {
    for (next_child_ids) |child_id| {
        elements[@intCast(child_id)].parent_id = parent.id;
    }
    parent.children.deinit(allocator);
    parent.children = .empty;
    parent.children.appendSlice(allocator, next_child_ids) catch std.process.exit(1);
}

test "simulated DOM element indexes attrs and named events" {
    const allocator = std.testing.allocator;
    const tag = try allocator.dupe(u8, "button");
    var elem = Element.init(7, tag);
    defer elem.deinit(allocator);

    try elem.attrs.append(allocator, .{
        .name = try allocator.dupe(u8, "data-state"),
        .value = try allocator.dupe(u8, "ready"),
    });
    try elem.named_events.append(allocator, .{
        .name = try allocator.dupe(u8, "submit"),
        .event_id = 42,
        .options = 0,
        .payload_kind = .unit,
        .payload_accessor = .none,
    });

    try std.testing.expectEqual(@as(?usize, 0), elem.textAttrIndex("data-state"));
    try std.testing.expectEqual(@as(?usize, null), elem.textAttrIndex("missing"));
    try std.testing.expectEqual(@as(?usize, 0), elem.namedEventIndex("submit"));
    try std.testing.expectEqual(@as(?usize, null), elem.namedEventIndex("click"));
}

test "simulated DOM matches spec locators" {
    const allocator = std.testing.allocator;
    const tag = try allocator.dupe(u8, "button");
    var elem = Element.init(1, tag);
    defer elem.deinit(allocator);

    elem.text = try allocator.dupe(u8, "Save");
    elem.test_id = try allocator.dupe(u8, "save-button");

    try std.testing.expect(matchesLocator(&elem, .{
        .kind = .role_name,
        .role = "button",
        .name = "Save",
    }));
    try std.testing.expect(matchesLocator(&elem, .{
        .kind = .test_id,
        .test_id = "save-button",
    }));
    try std.testing.expect(!matchesLocator(&elem, .{
        .kind = .text,
        .text = "Cancel",
    }));
}

test "simulated DOM mutation helpers update owned fields and counters" {
    const allocator = std.testing.allocator;
    const tag = try allocator.dupe(u8, "input");
    var elem = Element.init(2, tag);
    defer elem.deinit(allocator);

    setText(allocator, &elem, "Name");
    setValue(allocator, &elem, "Ada");
    try std.testing.expectEqualStrings("Name", elem.text.?);
    try std.testing.expectEqualStrings("Ada", elem.value.?);
    try std.testing.expectEqual(@as(u64, 1), elem.text_update_count);
    try std.testing.expectEqual(@as(u64, 1), elem.value_update_count);

    try std.testing.expect(!setValueIfChanged(allocator, &elem, "Ada"));
    try std.testing.expect(setValueIfChanged(allocator, &elem, "Grace"));
    try std.testing.expectEqual(@as(u64, 2), elem.value_update_count);

    setTextAttr(allocator, &elem, "data-state", "ready");
    try std.testing.expectEqualStrings("ready", textAttr(&elem, "data-state").?);
    setTextAttr(allocator, &elem, "data-state", "done");
    try std.testing.expectEqualStrings("done", textAttr(&elem, "data-state").?);
    clearTextAttr(allocator, &elem, "data-state");
    try std.testing.expect(textAttr(&elem, "data-state") == null);

    try std.testing.expect(setCheckedIfChanged(&elem, true));
    setDisabled(&elem, true);
    try std.testing.expect(elem.checked);
    try std.testing.expect(elem.disabled);
}

test "simulated DOM binds and clears events" {
    const allocator = std.testing.allocator;
    const tag = try allocator.dupe(u8, "form");
    var elem = Element.init(3, tag);
    defer elem.deinit(allocator);

    bindEventKind(&elem, .click, 11);
    try std.testing.expectEqual(@as(?u64, 11), elem.bound_click_event);
    clearEventKind(&elem, .click);
    try std.testing.expectEqual(@as(?u64, null), elem.bound_click_event);

    bindEventName(allocator, &elem, "submit", 21, 7, .unit, .none);
    try std.testing.expectEqual(@as(u64, 21), namedEvent(&elem, "submit").?.event_id);
    bindEventName(allocator, &elem, "submit", 22, 9, .bytes, .record_key_shift);
    const updated = namedEvent(&elem, "submit").?;
    try std.testing.expectEqual(@as(u64, 22), updated.event_id);
    try std.testing.expectEqual(@as(u32, 9), updated.options);
    try std.testing.expectEqual(render.EventPayloadKind.bytes, updated.payload_kind);
    try std.testing.expectEqual(render.EventPayloadAccessor.record_key_shift, updated.payload_accessor);
    clearEventName(allocator, &elem, "submit");
    try std.testing.expect(namedEvent(&elem, "submit") == null);
}

test "simulated DOM resets and appends children" {
    const allocator = std.testing.allocator;
    var elements: std.ArrayListUnmanaged(Element) = .empty;
    defer {
        for (elements.items) |*elem| {
            elem.deinit(allocator);
        }
        elements.deinit(allocator);
    }

    reset(allocator, &elements);
    try std.testing.expectEqual(@as(usize, 1), elements.items.len);
    try std.testing.expectEqualStrings("root", elements.items[0].tag);

    appendDetached(allocator, &elements, 1, "section");
    appendChild(allocator, &elements.items[0], &elements.items[1]);
    try std.testing.expectEqual(@as(?u64, 0), elements.items[1].parent_id);
    try std.testing.expectEqualSlices(u64, &.{1}, elements.items[0].children.items);
}

test "simulated DOM replaces children and deactivates removed nodes" {
    const allocator = std.testing.allocator;
    var elements: std.ArrayListUnmanaged(Element) = .empty;
    defer {
        for (elements.items) |*elem| {
            elem.deinit(allocator);
        }
        elements.deinit(allocator);
    }

    reset(allocator, &elements);
    appendDetached(allocator, &elements, 1, "section");
    appendDetached(allocator, &elements, 2, "p");
    appendDetached(allocator, &elements, 3, "button");
    appendChild(allocator, &elements.items[0], &elements.items[1]);
    appendChild(allocator, &elements.items[1], &elements.items[2]);

    replaceChildren(allocator, elements.items, &elements.items[0], &.{3});
    try std.testing.expectEqual(@as(?u64, 0), elements.items[3].parent_id);
    try std.testing.expectEqualSlices(u64, &.{3}, elements.items[0].children.items);

    removeChildAt(&elements.items[0], 0);
    try std.testing.expectEqual(@as(usize, 0), elements.items[0].children.items.len);
    bindEventName(allocator, &elements.items[3], "click", 9, 0, .unit, .none);
    deactivateRemovedNode(allocator, &elements.items[3]);
    try std.testing.expect(!elements.items[3].active);
    try std.testing.expectEqual(@as(?u64, null), elements.items[3].parent_id);
    try std.testing.expectEqual(@as(usize, 0), elements.items[3].named_events.items.len);
}
