const std = @import("std");
const abi = @import("roc_platform_abi.zig");
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

pub const RenderNodeKind = enum {
    element,
    text,
    signal_text,
};

pub const RenderNode = struct {
    elem_id: u64,
    kind: RenderNodeKind,
};

pub const ElementDesc = struct {
    elem_id: u64,
    parent_elem_id: u64,
    scope_id: u64,
    tag: []const u8,
};

pub const TextNodeDesc = struct {
    elem_id: u64,
    parent_elem_id: u64,
    scope_id: u64,
    value: []const u8,
};

pub const StaticTextAttrDesc = struct {
    elem_id: u64,
    field: TextField,
    value: []const u8,
};

pub const StaticCustomTextAttrDesc = struct {
    elem_id: u64,
    name: []const u8,
    value: []const u8,
};

pub const StaticBoolAttrDesc = struct {
    elem_id: u64,
    field: BoolField,
    value: bool,
};

pub const MountDesc = struct {
    scope_id: u64,
    to_cmd: abi.RocErasedCallable,
    run_on_mount: bool,
};

pub const CleanupDesc = struct {
    scope_id: u64,
    name: []const u8,
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

pub fn findElementDesc(comptime Stream: type, stream: *const Stream, elem_id: u64) ?Stream.ElementDesc {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return null;
    const index = descriptor_index.element orelse return null;
    if (index >= stream.elements.items.len) @panic("element descriptor index exceeded descriptor table");
    const desc = stream.elements.items[index];
    if (desc.elem_id != elem_id) @panic("element descriptor index pointed at the wrong elem id");
    return desc;
}

pub fn findTextNodeDesc(comptime Stream: type, stream: *const Stream, elem_id: u64) ?Stream.TextNodeDesc {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return null;
    const index = descriptor_index.text_node orelse return null;
    if (index >= stream.text_nodes.items.len) @panic("text node descriptor index exceeded descriptor table");
    const desc = stream.text_nodes.items[index];
    if (desc.elem_id != elem_id) @panic("text node descriptor index pointed at the wrong elem id");
    return desc;
}

pub fn findSignalTextNodeDesc(comptime Stream: type, stream: *const Stream, elem_id: u64) ?Stream.SignalTextNodeDesc {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return null;
    const index = descriptor_index.signal_text_node orelse return null;
    if (index >= stream.signal_text_nodes.items.len) @panic("signal text node descriptor index exceeded descriptor table");
    const desc = stream.signal_text_nodes.items[index];
    if (desc.elem_id != elem_id) @panic("signal text node descriptor index pointed at the wrong elem id");
    return desc;
}

pub fn findSignalTextNodeDescMutable(comptime Stream: type, stream: *Stream, elem_id: u64) ?*Stream.SignalTextNodeDesc {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return null;
    const index = descriptor_index.signal_text_node orelse return null;
    if (index >= stream.signal_text_nodes.items.len) @panic("signal text node descriptor index exceeded descriptor table");
    const desc = &stream.signal_text_nodes.items[index];
    if (desc.elem_id != elem_id) @panic("signal text node descriptor index pointed at the wrong elem id");
    return desc;
}

pub fn streamHasTextField(comptime Stream: type, stream: *const Stream, elem_id: u64, field: TextField) bool {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return false;
    if (field == .text and descriptor_index.text_node != null) return true;
    if (field == .text and descriptor_index.signal_text_node != null) return true;

    if (descriptor_index.static_text_attrs.get(field)) |attr_index| {
        if (attr_index >= stream.static_text_attrs.items.len) @panic("static text attr descriptor index exceeded descriptor table");
        const desc = stream.static_text_attrs.items[attr_index];
        if (desc.elem_id != elem_id or desc.field != field) @panic("static text attr descriptor index pointed at the wrong field");
        return true;
    }
    if (descriptor_index.signal_text_attrs.get(field)) |attr_index| {
        if (attr_index >= stream.signal_text_attrs.items.len) @panic("signal text attr descriptor index exceeded descriptor table");
        const desc = stream.signal_text_attrs.items[attr_index];
        if (desc.elem_id != elem_id or desc.field != field) @panic("signal text attr descriptor index pointed at the wrong field");
        return true;
    }
    return false;
}

pub fn streamHasCustomTextAttr(comptime Stream: type, stream: *const Stream, elem_id: u64, name: []const u8) bool {
    for (stream.static_custom_text_attrs.items) |desc| {
        if (desc.elem_id == elem_id and std.mem.eql(u8, desc.name, name)) return true;
    }
    for (stream.signal_custom_text_attrs.items) |desc| {
        if (desc.elem_id == elem_id and std.mem.eql(u8, desc.name, name)) return true;
    }
    return false;
}

pub fn streamHasBoolField(comptime Stream: type, stream: *const Stream, elem_id: u64, field: BoolField) bool {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return false;
    if (descriptor_index.static_bool_attrs.get(field)) |attr_index| {
        if (attr_index >= stream.static_bool_attrs.items.len) @panic("static bool attr descriptor index exceeded descriptor table");
        const desc = stream.static_bool_attrs.items[attr_index];
        if (desc.elem_id != elem_id or desc.field != field) @panic("static bool attr descriptor index pointed at the wrong field");
        return true;
    }
    if (descriptor_index.signal_bool_attrs.get(field)) |attr_index| {
        if (attr_index >= stream.signal_bool_attrs.items.len) @panic("signal bool attr descriptor index exceeded descriptor table");
        const desc = stream.signal_bool_attrs.items[attr_index];
        if (desc.elem_id != elem_id or desc.field != field) @panic("signal bool attr descriptor index pointed at the wrong field");
        return true;
    }
    return false;
}

pub fn maxRenderElemId(comptime Stream: type, stream: *const Stream) u64 {
    var max_elem_id: u64 = 0;
    for (stream.render_nodes.items) |node| {
        max_elem_id = @max(max_elem_id, node.elem_id);
    }
    return max_elem_id;
}

pub fn renderNodeTag(comptime Stream: type, stream: *const Stream, node: Stream.RenderNode) []const u8 {
    return switch (node.kind) {
        .element => (findElementDesc(Stream, stream, node.elem_id) orelse @panic("renderNodeTag: render node has no matching descriptor")).tag,
        .text, .signal_text => "text",
    };
}

pub fn streamElemTag(comptime Stream: type, stream: *const Stream, elem_id: u64) []const u8 {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse @panic("elem id had no descriptor index");
    if (descriptor_index.element) |index| {
        if (index >= stream.elements.items.len) @panic("element descriptor index exceeded descriptor table");
        const desc = stream.elements.items[index];
        if (desc.elem_id != elem_id) @panic("element descriptor index pointed at the wrong elem id");
        return desc.tag;
    }
    if (descriptor_index.text_node != null or descriptor_index.signal_text_node != null) return "text";
    @panic("elem id had no render descriptor");
}

pub fn renderNodeParentElemId(comptime Stream: type, stream: *const Stream, node: Stream.RenderNode) u64 {
    return switch (node.kind) {
        .element => (findElementDesc(Stream, stream, node.elem_id) orelse @panic("renderNodeParentElemId: render node has no matching descriptor")).parent_elem_id,
        .text => (findTextNodeDesc(Stream, stream, node.elem_id) orelse @panic("renderNodeParentElemId: render node has no matching descriptor")).parent_elem_id,
        .signal_text => (findSignalTextNodeDesc(Stream, stream, node.elem_id) orelse @panic("renderNodeParentElemId: render node has no matching descriptor")).parent_elem_id,
    };
}

pub fn streamElemParentElemId(comptime Stream: type, stream: *const Stream, elem_id: u64) u64 {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse @panic("elem id had no descriptor index");
    if (descriptor_index.element) |index| {
        if (index >= stream.elements.items.len) @panic("element descriptor index exceeded descriptor table");
        const desc = stream.elements.items[index];
        if (desc.elem_id != elem_id) @panic("element descriptor index pointed at the wrong elem id");
        return desc.parent_elem_id;
    }
    if (descriptor_index.text_node) |index| {
        if (index >= stream.text_nodes.items.len) @panic("text node descriptor index exceeded descriptor table");
        const desc = stream.text_nodes.items[index];
        if (desc.elem_id != elem_id) @panic("text node descriptor index pointed at the wrong elem id");
        return desc.parent_elem_id;
    }
    if (descriptor_index.signal_text_node) |index| {
        if (index >= stream.signal_text_nodes.items.len) @panic("signal text node descriptor index exceeded descriptor table");
        const desc = stream.signal_text_nodes.items[index];
        if (desc.elem_id != elem_id) @panic("signal text node descriptor index pointed at the wrong elem id");
        return desc.parent_elem_id;
    }
    @panic("elem id had no render descriptor");
}

pub fn streamDirectChildren(comptime Stream: type, allocator: std.mem.Allocator, stream: *const Stream, parent_elem_id: u64) []u64 {
    var children: std.ArrayListUnmanaged(u64) = .empty;
    errdefer children.deinit(allocator);

    var child = stream.firstRenderChild(parent_elem_id);
    while (child) |child_id| {
        children.append(allocator, child_id) catch @panic("out of memory");
        child = stream.nextRenderSibling(child_id);
    }

    return children.toOwnedSlice(allocator) catch @panic("out of memory");
}

pub fn renderNodeScopeId(comptime Stream: type, stream: *const Stream, node: Stream.RenderNode) u64 {
    return switch (node.kind) {
        .element => (findElementDesc(Stream, stream, node.elem_id) orelse @panic("renderNodeScopeId: render node has no matching descriptor")).scope_id,
        .text => (findTextNodeDesc(Stream, stream, node.elem_id) orelse @panic("renderNodeScopeId: render node has no matching descriptor")).scope_id,
        .signal_text => (findSignalTextNodeDesc(Stream, stream, node.elem_id) orelse @panic("renderNodeScopeId: render node has no matching descriptor")).scope_id,
    };
}

pub fn elemScopeId(comptime Stream: type, stream: *const Stream, elem_id: u64) ?u64 {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return null;
    if (descriptor_index.element) |index| {
        if (index >= stream.elements.items.len) @panic("element descriptor index exceeded descriptor table");
        const desc = stream.elements.items[index];
        if (desc.elem_id != elem_id) @panic("element descriptor index pointed at the wrong elem id");
        return desc.scope_id;
    }
    if (descriptor_index.text_node) |index| {
        if (index >= stream.text_nodes.items.len) @panic("text node descriptor index exceeded descriptor table");
        const desc = stream.text_nodes.items[index];
        if (desc.elem_id != elem_id) @panic("text node descriptor index pointed at the wrong elem id");
        return desc.scope_id;
    }
    if (descriptor_index.signal_text_node) |index| {
        if (index >= stream.signal_text_nodes.items.len) @panic("signal text node descriptor index exceeded descriptor table");
        const desc = stream.signal_text_nodes.items[index];
        if (desc.elem_id != elem_id) @panic("signal text node descriptor index pointed at the wrong elem id");
        return desc.scope_id;
    }
    return null;
}

pub fn adjustedRenderInsertIndex(old_index: usize, replace_index: usize, removed_count: usize, replacement_count: usize) usize {
    if (removed_count == 0) {
        if (old_index < replace_index) return old_index;
        return old_index + replacement_count;
    }
    if (old_index <= replace_index) return old_index;
    if (old_index < replace_index + removed_count) @panic("scope site inside replaced scope was not removed");
    return old_index - removed_count + replacement_count;
}

const TestElementDesc = struct {
    elem_id: u64,
    parent_elem_id: u64,
    scope_id: u64,
    tag: []const u8,
};

const TestTextNodeDesc = struct {
    elem_id: u64,
    parent_elem_id: u64,
    scope_id: u64,
};

const TestStaticTextAttrDesc = struct {
    elem_id: u64,
    field: TextField,
};

const TestCustomTextAttrDesc = struct {
    elem_id: u64,
    name: []const u8,
};

const TestStaticBoolAttrDesc = struct {
    elem_id: u64,
    field: BoolField,
};

const TestRenderNode = RenderNode;

const TestStream = struct {
    pub const RenderNode = TestRenderNode;
    pub const ElementDesc = TestElementDesc;
    pub const TextNodeDesc = TestTextNodeDesc;
    pub const SignalTextNodeDesc = TestTextNodeDesc;

    render_nodes: std.ArrayListUnmanaged(TestRenderNode) = .empty,
    elements: std.ArrayListUnmanaged(TestElementDesc) = .empty,
    text_nodes: std.ArrayListUnmanaged(TestTextNodeDesc) = .empty,
    signal_text_nodes: std.ArrayListUnmanaged(TestTextNodeDesc) = .empty,
    static_text_attrs: std.ArrayListUnmanaged(TestStaticTextAttrDesc) = .empty,
    signal_text_attrs: std.ArrayListUnmanaged(TestStaticTextAttrDesc) = .empty,
    static_custom_text_attrs: std.ArrayListUnmanaged(TestCustomTextAttrDesc) = .empty,
    signal_custom_text_attrs: std.ArrayListUnmanaged(TestCustomTextAttrDesc) = .empty,
    static_bool_attrs: std.ArrayListUnmanaged(TestStaticBoolAttrDesc) = .empty,
    signal_bool_attrs: std.ArrayListUnmanaged(TestStaticBoolAttrDesc) = .empty,
    descriptor_indexes_by_elem_id: std.ArrayListUnmanaged(ElemDescriptorIndex) = .empty,
    first_child_by_parent: std.AutoHashMapUnmanaged(u64, u64) = .empty,
    next_sibling_by_elem_id: std.AutoHashMapUnmanaged(u64, u64) = .empty,

    fn deinit(self: *TestStream, allocator: std.mem.Allocator) void {
        self.render_nodes.deinit(allocator);
        self.elements.deinit(allocator);
        self.text_nodes.deinit(allocator);
        self.signal_text_nodes.deinit(allocator);
        self.static_text_attrs.deinit(allocator);
        self.signal_text_attrs.deinit(allocator);
        self.static_custom_text_attrs.deinit(allocator);
        self.signal_custom_text_attrs.deinit(allocator);
        self.static_bool_attrs.deinit(allocator);
        self.signal_bool_attrs.deinit(allocator);
        self.descriptor_indexes_by_elem_id.deinit(allocator);
        self.first_child_by_parent.deinit(allocator);
        self.next_sibling_by_elem_id.deinit(allocator);
        self.* = .{};
    }

    fn elemDescriptorIndex(self: *const TestStream, elem_id: u64) ?ElemDescriptorIndex {
        if (elem_id >= self.descriptor_indexes_by_elem_id.items.len) return null;
        return self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)];
    }

    fn firstRenderChild(self: *const TestStream, parent_elem_id: u64) ?u64 {
        return self.first_child_by_parent.get(parent_elem_id);
    }

    fn nextRenderSibling(self: *const TestStream, elem_id: u64) ?u64 {
        return self.next_sibling_by_elem_id.get(elem_id);
    }
};

fn ensureTestElemDescriptorIndex(stream: *TestStream, allocator: std.mem.Allocator, elem_id: u64) *ElemDescriptorIndex {
    const index: usize = @intCast(elem_id);
    while (stream.descriptor_indexes_by_elem_id.items.len <= index) {
        stream.descriptor_indexes_by_elem_id.append(allocator, .{}) catch @panic("out of memory");
    }
    return &stream.descriptor_indexes_by_elem_id.items[index];
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

test "stream reader helpers validate descriptor indexes" {
    const allocator = std.testing.allocator;
    var stream = TestStream{};
    defer stream.deinit(allocator);

    stream.elements.append(allocator, .{
        .elem_id = 1,
        .parent_elem_id = 0,
        .scope_id = 10,
        .tag = "div",
    }) catch @panic("out of memory");
    ensureTestElemDescriptorIndex(&stream, allocator, 1).element = 0;

    stream.text_nodes.append(allocator, .{
        .elem_id = 2,
        .parent_elem_id = 1,
        .scope_id = 10,
    }) catch @panic("out of memory");
    ensureTestElemDescriptorIndex(&stream, allocator, 2).text_node = 0;

    stream.signal_text_nodes.append(allocator, .{
        .elem_id = 3,
        .parent_elem_id = 1,
        .scope_id = 11,
    }) catch @panic("out of memory");
    ensureTestElemDescriptorIndex(&stream, allocator, 3).signal_text_node = 0;

    stream.static_text_attrs.append(allocator, .{
        .elem_id = 1,
        .field = .label,
    }) catch @panic("out of memory");
    ensureTestElemDescriptorIndex(&stream, allocator, 1).static_text_attrs.slot(.label).* = 0;

    stream.static_custom_text_attrs.append(allocator, .{
        .elem_id = 1,
        .name = "data-id",
    }) catch @panic("out of memory");

    stream.static_bool_attrs.append(allocator, .{
        .elem_id = 1,
        .field = .checked,
    }) catch @panic("out of memory");
    ensureTestElemDescriptorIndex(&stream, allocator, 1).static_bool_attrs.slot(.checked).* = 0;

    stream.render_nodes.appendSlice(allocator, &.{
        .{ .elem_id = 1, .kind = .element },
        .{ .elem_id = 2, .kind = .text },
        .{ .elem_id = 3, .kind = .signal_text },
    }) catch @panic("out of memory");
    stream.first_child_by_parent.put(allocator, 1, 2) catch @panic("out of memory");
    stream.next_sibling_by_elem_id.put(allocator, 2, 3) catch @panic("out of memory");

    const element = findElementDesc(TestStream, &stream, 1) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqualStrings("div", element.tag);
    try std.testing.expectEqual(@as(u64, 1), renderNodeParentElemId(TestStream, &stream, .{ .elem_id = 2, .kind = .text }));
    try std.testing.expectEqual(@as(u64, 11), renderNodeScopeId(TestStream, &stream, .{ .elem_id = 3, .kind = .signal_text }));
    try std.testing.expectEqualStrings("text", streamElemTag(TestStream, &stream, 2));
    try std.testing.expect(streamHasTextField(TestStream, &stream, 1, .label));
    try std.testing.expect(streamHasCustomTextAttr(TestStream, &stream, 1, "data-id"));
    try std.testing.expect(streamHasBoolField(TestStream, &stream, 1, .checked));
    try std.testing.expectEqual(@as(u64, 3), maxRenderElemId(TestStream, &stream));
    try std.testing.expectEqual(@as(?u64, 10), elemScopeId(TestStream, &stream, 1));

    const children = streamDirectChildren(TestStream, allocator, &stream, 1);
    defer allocator.free(children);
    try std.testing.expectEqualSlices(u64, &.{ 2, 3 }, children);
}
