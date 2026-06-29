const std = @import("std");
const abi = @import("roc_platform_abi.zig");
const render = @import("render_commands.zig");
const retained = @import("retained_values.zig");
const signal_records = @import("signal_records.zig");

pub const TextField = render.TextField;
pub const BoolField = render.BoolField;
pub const EventKind = render.EventKind;
pub const EventPayloadKind = render.EventPayloadKind;
pub const EventPayloadAccessor = render.EventPayloadAccessor;
pub const HostSignalBinding = signal_records.Binding;
pub const HostSignalCacheSlot = signal_records.CacheSlot;
pub const HostValueCapability = retained.HostValueCapability;
pub const HostTextRead = retained.HostTextRead;
pub const HostBoolRead = retained.HostBoolRead;
pub const HostEventReducer = retained.HostEventReducer;
pub const HostEachOps = retained.HostEachOps;

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

/// Identity token interned per `Ui.state` binder.
pub const BinderToken = *u64;

/// Binds a state binder token to the node id it resolves to within a scope.
pub const BinderBinding = struct {
    token: BinderToken,
    node_id: u64,
};

pub const ScopeSiteDesc = struct {
    node_id: u64,
    scope_id: u64,
    ordinal: u64,
    parent_elem_id: u64,
    render_insert_index: usize,
    kind: ScopeSiteKind,
    binder_bindings: []BinderBinding,
};

pub const SignalTextNodeDesc = struct {
    elem_id: u64,
    parent_elem_id: u64,
    scope_id: u64,
    signal: HostSignalBinding,
    read: HostTextRead,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const SignalTextAttrDesc = struct {
    elem_id: u64,
    field: TextField,
    signal: HostSignalBinding,
    read: HostTextRead,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const SignalCustomTextAttrDesc = struct {
    elem_id: u64,
    name: []const u8,
    signal: HostSignalBinding,
    read: HostTextRead,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const SignalBoolAttrDesc = struct {
    elem_id: u64,
    field: BoolField,
    signal: HostSignalBinding,
    read: HostBoolRead,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const OnChangeDesc = struct {
    scope_id: u64,
    signal: HostSignalBinding,
    to_cmd: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const EventDesc = struct {
    elem_id: u64,
    kind: EventKind,
    binder_token: BinderToken,
    target_node_id: u64,
    payload_kind: EventPayloadKind,
    payload_accessor: EventPayloadAccessor,
    payload_reducer: HostEventReducer,
    owns_payload_reducer: bool = true,
};

pub const NamedEventDesc = struct {
    elem_id: u64,
    name: []const u8,
    options: u32,
    binder_token: BinderToken,
    target_node_id: u64,
    payload_kind: EventPayloadKind,
    payload_accessor: EventPayloadAccessor,
    payload_reducer: HostEventReducer,
    owns_payload_reducer: bool = true,
};

pub const StateDesc = struct {
    node_id: u64,
    initial: abi.RocErasedCallable,
    cap: HostValueCapability,
};

pub const WhenDesc = struct {
    node_id: u64,
    condition: HostSignalBinding,
    read: HostBoolRead,
    when_false: abi.Elem,
    when_true: abi.Elem,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const EachDesc = struct {
    node_id: u64,
    items: HostSignalBinding,
    ops: HostEachOps,
    cached_value: HostSignalCacheSlot = .absent,
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

pub fn ensureRenderMetadata(comptime Stream: type, stream: *Stream, allocator: std.mem.Allocator, elem_id: u64) *RenderElemIndex {
    const entry = stream.render_metadata_by_elem_id.getOrPut(allocator, elem_id) catch @panic("out of memory");
    if (!entry.found_existing) entry.value_ptr.* = .{};
    return entry.value_ptr;
}

pub fn removeRenderMetadataIfEmpty(comptime Stream: type, stream: *Stream, elem_id: u64) void {
    const metadata = stream.render_metadata_by_elem_id.get(elem_id) orelse return;
    if (metadata.empty()) {
        _ = stream.render_metadata_by_elem_id.fetchRemove(elem_id) orelse @panic("render metadata disappeared during removal");
    }
}

pub fn renderNodeIndex(comptime Stream: type, stream: *const Stream, elem_id: u64) ?usize {
    const metadata = stream.render_metadata_by_elem_id.get(elem_id) orelse return null;
    return metadata.render_node;
}

pub fn recordRenderNodeIndex(comptime Stream: type, stream: *Stream, allocator: std.mem.Allocator, elem_id: u64, index: usize) void {
    const metadata = ensureRenderMetadata(Stream, stream, allocator, elem_id);
    if (metadata.render_node != null) @panic("descriptor stream recorded duplicate render index");
    metadata.render_node = index;
}

pub fn updateRenderNodeIndex(comptime Stream: type, stream: *Stream, elem_id: u64, index: usize) void {
    const metadata = stream.render_metadata_by_elem_id.getPtr(elem_id) orelse @panic("descriptor stream updated a missing render index");
    if (metadata.render_node == null) @panic("descriptor stream updated a missing render index");
    metadata.render_node = index;
}

pub fn clearRenderNodeIndex(comptime Stream: type, stream: *Stream, elem_id: u64, expected: usize) void {
    const metadata = stream.render_metadata_by_elem_id.getPtr(elem_id) orelse @panic("descriptor stream cleared a missing render index");
    const existing = metadata.render_node orelse @panic("descriptor stream cleared a missing render index");
    if (existing != expected) @panic("descriptor stream cleared the wrong render index");
    metadata.render_node = null;
    removeRenderMetadataIfEmpty(Stream, stream, elem_id);
}

pub fn ensureFirstRenderChildSlot(comptime Stream: type, stream: *Stream, allocator: std.mem.Allocator, parent_elem_id: u64) *?u64 {
    return &ensureRenderMetadata(Stream, stream, allocator, parent_elem_id).first_child;
}

pub fn ensureLastRenderChildSlot(comptime Stream: type, stream: *Stream, allocator: std.mem.Allocator, parent_elem_id: u64) *?u64 {
    return &ensureRenderMetadata(Stream, stream, allocator, parent_elem_id).last_child;
}

pub fn ensureNextRenderSiblingSlot(comptime Stream: type, stream: *Stream, allocator: std.mem.Allocator, elem_id: u64) *?u64 {
    return &ensureRenderMetadata(Stream, stream, allocator, elem_id).next_sibling;
}

pub fn firstRenderChild(comptime Stream: type, stream: *const Stream, parent_elem_id: u64) ?u64 {
    const metadata = stream.render_metadata_by_elem_id.get(parent_elem_id) orelse return null;
    return metadata.first_child;
}

pub fn lastRenderChild(comptime Stream: type, stream: *const Stream, parent_elem_id: u64) ?u64 {
    const metadata = stream.render_metadata_by_elem_id.get(parent_elem_id) orelse return null;
    return metadata.last_child;
}

pub fn nextRenderSibling(comptime Stream: type, stream: *const Stream, elem_id: u64) ?u64 {
    const metadata = stream.render_metadata_by_elem_id.get(elem_id) orelse return null;
    return metadata.next_sibling;
}

pub fn appendRenderChild(comptime Stream: type, stream: *Stream, allocator: std.mem.Allocator, parent_elem_id: u64, elem_id: u64) void {
    _ = ensureRenderMetadata(Stream, stream, allocator, parent_elem_id);
    _ = ensureRenderMetadata(Stream, stream, allocator, elem_id);

    const parent_metadata = stream.render_metadata_by_elem_id.getPtr(parent_elem_id) orelse @panic("render child index was missing its parent links");
    const elem_metadata = stream.render_metadata_by_elem_id.getPtr(elem_id) orelse @panic("render child index was missing its child links");
    const last = parent_metadata.last_child;
    elem_metadata.next_sibling = null;
    if (last) |last_child| {
        const last_metadata = stream.render_metadata_by_elem_id.getPtr(last_child) orelse @panic("render child index was missing its last child links");
        last_metadata.next_sibling = elem_id;
    } else {
        parent_metadata.first_child = elem_id;
    }
    parent_metadata.last_child = elem_id;
}

pub fn clearRenderChildren(comptime Stream: type, stream: *Stream, parent_elem_id: u64) void {
    var child = firstRenderChild(Stream, stream, parent_elem_id);
    while (child) |child_id| {
        const next = nextRenderSibling(Stream, stream, child_id);
        const child_metadata = stream.render_metadata_by_elem_id.getPtr(child_id) orelse @panic("render child index referenced a child without links");
        child_metadata.next_sibling = null;
        removeRenderMetadataIfEmpty(Stream, stream, child_id);
        child = next;
    }
    if (stream.render_metadata_by_elem_id.getPtr(parent_elem_id)) |parent_metadata| {
        parent_metadata.first_child = null;
        parent_metadata.last_child = null;
    }
    removeRenderMetadataIfEmpty(Stream, stream, parent_elem_id);
}

pub fn removeRenderChild(comptime Stream: type, stream: *Stream, parent_elem_id: u64, elem_id: u64) void {
    const parent_metadata = stream.render_metadata_by_elem_id.getPtr(parent_elem_id) orelse @panic("render child index was missing its parent list");

    var previous: ?u64 = null;
    var current = parent_metadata.first_child;
    while (current) |child_id| {
        const next = nextRenderSibling(Stream, stream, child_id);
        if (child_id == elem_id) {
            if (previous) |previous_id| {
                const previous_metadata = stream.render_metadata_by_elem_id.getPtr(previous_id) orelse @panic("render child index referenced a previous child without links");
                previous_metadata.next_sibling = next;
            } else {
                parent_metadata.first_child = next;
            }
            if (lastRenderChild(Stream, stream, parent_elem_id) == elem_id) {
                parent_metadata.last_child = previous;
            }
            const elem_metadata = stream.render_metadata_by_elem_id.getPtr(elem_id) orelse @panic("render child index removed a child without links");
            elem_metadata.next_sibling = null;
            removeRenderMetadataIfEmpty(Stream, stream, elem_id);
            removeRenderMetadataIfEmpty(Stream, stream, parent_elem_id);
            return;
        }
        previous = child_id;
        current = next;
    }
    @panic("render child index was missing a child");
}

pub fn insertRenderChildren(comptime Stream: type, stream: *Stream, allocator: std.mem.Allocator, parent_elem_id: u64, index: usize, elem_ids: []const u64) void {
    if (elem_ids.len == 0) return;

    _ = ensureRenderMetadata(Stream, stream, allocator, parent_elem_id);
    for (elem_ids) |elem_id| {
        _ = ensureRenderMetadata(Stream, stream, allocator, elem_id);
    }

    const parent_metadata = stream.render_metadata_by_elem_id.getPtr(parent_elem_id) orelse @panic("render child insertion was missing parent links");

    var previous: ?u64 = null;
    var next = parent_metadata.first_child;
    var cursor: usize = 0;
    while (cursor < index) : (cursor += 1) {
        const child_id = next orelse @panic("render child insertion index exceeded parent child list");
        previous = child_id;
        next = nextRenderSibling(Stream, stream, child_id);
    }

    for (elem_ids, 0..) |elem_id, elem_index| {
        const next_insert = if (elem_index + 1 < elem_ids.len) elem_ids[elem_index + 1] else next;
        ensureNextRenderSiblingSlot(Stream, stream, allocator, elem_id).* = next_insert;
    }

    if (previous) |previous_id| {
        const previous_metadata = stream.render_metadata_by_elem_id.getPtr(previous_id) orelse @panic("render child insertion referenced a previous child without links");
        previous_metadata.next_sibling = elem_ids[0];
    } else {
        parent_metadata.first_child = elem_ids[0];
    }
    if (next == null) {
        parent_metadata.last_child = elem_ids[elem_ids.len - 1];
    }
}

pub fn replaceRenderChildrenIndex(comptime Stream: type, stream: *Stream, allocator: std.mem.Allocator, parent_elem_id: u64, elem_ids: []const u64) void {
    clearRenderChildren(Stream, stream, parent_elem_id);
    insertRenderChildren(Stream, stream, allocator, parent_elem_id, 0, elem_ids);
}

pub fn childInsertionIndexForRenderIndex(comptime Stream: type, stream: *const Stream, parent_elem_id: u64, render_insert_index: usize) usize {
    var index: usize = 0;
    var child = firstRenderChild(Stream, stream, parent_elem_id);
    while (child) |child_id| : (index += 1) {
        const child_render_index = renderNodeIndex(Stream, stream, child_id) orelse @panic("render child index referenced a child without a render index");
        if (child_render_index >= render_insert_index) return index;
        child = nextRenderSibling(Stream, stream, child_id);
    }
    return index;
}

pub fn refreshRenderIndexesFrom(comptime Stream: type, stream: *Stream, allocator: std.mem.Allocator, start_index: usize, metrics: anytype) void {
    if (start_index > stream.render_nodes.items.len) @panic("render index refresh started past render node table");
    metrics.bump(.render_indexes_refreshed, @intCast(stream.render_nodes.items.len - start_index));
    for (stream.render_nodes.items[start_index..], start_index..) |node, index| {
        ensureRenderMetadata(Stream, stream, allocator, node.elem_id).render_node = index;
    }
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
    render_metadata_by_elem_id: std.AutoHashMapUnmanaged(u64, RenderElemIndex) = .empty,

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
        self.render_metadata_by_elem_id.deinit(allocator);
        self.* = .{};
    }

    fn elemDescriptorIndex(self: *const TestStream, elem_id: u64) ?ElemDescriptorIndex {
        if (elem_id >= self.descriptor_indexes_by_elem_id.items.len) return null;
        return self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)];
    }

    fn firstRenderChild(self: *const TestStream, parent_elem_id: u64) ?u64 {
        const metadata = self.render_metadata_by_elem_id.get(parent_elem_id) orelse return null;
        return metadata.first_child;
    }

    fn nextRenderSibling(self: *const TestStream, elem_id: u64) ?u64 {
        const metadata = self.render_metadata_by_elem_id.get(elem_id) orelse return null;
        return metadata.next_sibling;
    }
};

fn ensureTestElemDescriptorIndex(stream: *TestStream, allocator: std.mem.Allocator, elem_id: u64) *ElemDescriptorIndex {
    const index: usize = @intCast(elem_id);
    while (stream.descriptor_indexes_by_elem_id.items.len <= index) {
        stream.descriptor_indexes_by_elem_id.append(allocator, .{}) catch @panic("out of memory");
    }
    return &stream.descriptor_indexes_by_elem_id.items[index];
}

const TestMetrics = struct {
    render_indexes_refreshed: u64 = 0,

    pub fn bump(self: *TestMetrics, comptime field: anytype, count: u64) void {
        switch (field) {
            .render_indexes_refreshed => self.render_indexes_refreshed += count,
            else => {},
        }
    }
};

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
    appendRenderChild(TestStream, &stream, allocator, 1, 2);
    appendRenderChild(TestStream, &stream, allocator, 1, 3);

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

test "render metadata helpers maintain child order and indexes" {
    const allocator = std.testing.allocator;
    var stream = TestStream{};
    defer stream.deinit(allocator);

    stream.render_nodes.appendSlice(allocator, &.{
        .{ .elem_id = 1, .kind = .element },
        .{ .elem_id = 2, .kind = .element },
        .{ .elem_id = 3, .kind = .element },
    }) catch @panic("out of memory");

    recordRenderNodeIndex(TestStream, &stream, allocator, 1, 0);
    recordRenderNodeIndex(TestStream, &stream, allocator, 2, 1);
    recordRenderNodeIndex(TestStream, &stream, allocator, 3, 2);
    appendRenderChild(TestStream, &stream, allocator, 0, 1);
    appendRenderChild(TestStream, &stream, allocator, 0, 3);
    insertRenderChildren(TestStream, &stream, allocator, 0, 1, &.{2});

    var children = streamDirectChildren(TestStream, allocator, &stream, 0);
    defer allocator.free(children);
    try std.testing.expectEqualSlices(u64, &.{ 1, 2, 3 }, children);
    try std.testing.expectEqual(@as(usize, 1), childInsertionIndexForRenderIndex(TestStream, &stream, 0, 1));

    removeRenderChild(TestStream, &stream, 0, 2);
    allocator.free(children);
    children = streamDirectChildren(TestStream, allocator, &stream, 0);
    try std.testing.expectEqualSlices(u64, &.{ 1, 3 }, children);

    clearRenderNodeIndex(TestStream, &stream, 2, 1);
    try std.testing.expectEqual(@as(?usize, null), renderNodeIndex(TestStream, &stream, 2));

    stream.render_nodes.items[1] = .{ .elem_id = 3, .kind = .element };
    stream.render_nodes.items[2] = .{ .elem_id = 2, .kind = .element };
    var metrics = TestMetrics{};
    refreshRenderIndexesFrom(TestStream, &stream, allocator, 1, &metrics);

    try std.testing.expectEqual(@as(?usize, 1), renderNodeIndex(TestStream, &stream, 3));
    try std.testing.expectEqual(@as(?usize, 2), renderNodeIndex(TestStream, &stream, 2));
    try std.testing.expectEqual(@as(u64, 2), metrics.render_indexes_refreshed);
}
