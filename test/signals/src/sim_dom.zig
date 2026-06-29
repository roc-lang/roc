const std = @import("std");

const render = @import("render_commands.zig");

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
