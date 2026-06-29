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

pub const HostSignalToken = signal_records.HostSignalToken;
const SignalRecord = signal_records.Record;

const retainHostTextRead = retained.retainHostTextRead;
const releaseHostTextRead = retained.releaseHostTextRead;
const retainHostBoolRead = retained.retainHostBoolRead;
const releaseHostBoolRead = retained.releaseHostBoolRead;
const retainHostEventReducer = retained.retainHostEventReducer;
const releaseHostEventReducer = retained.releaseHostEventReducer;
const retainHostValueCapability = retained.retainHostValueCapability;
const releaseHostValueCapability = retained.releaseHostValueCapability;
const retainHostEachOps = retained.retainHostEachOps;
const releaseHostEachOps = retained.releaseHostEachOps;

const StreamRenderNode = RenderNode;
const StreamElementDesc = ElementDesc;
const StreamTextNodeDesc = TextNodeDesc;
const StreamSignalTextNodeDesc = SignalTextNodeDesc;

fn renderNodeSliceContainsElem(items: []const RenderNode, elem_id: u64) bool {
    for (items) |item| {
        if (item.elem_id == elem_id) return true;
    }
    return false;
}

// Stream methods keep the public method names while delegating to the generic
// helpers below; these aliases avoid method/helper name collisions.
const appendCleanupImpl = appendCleanup;
const appendElementImpl = appendElement;
const appendRenderChildImpl = appendRenderChild;
const appendScopeSiteImpl = appendScopeSite;
const appendScopeSiteAtImpl = appendScopeSiteAt;
const appendStaticBoolAttrImpl = appendStaticBoolAttr;
const appendStaticCustomTextAttrImpl = appendStaticCustomTextAttr;
const appendStaticTextAttrImpl = appendStaticTextAttr;
const appendTextNodeImpl = appendTextNode;
const childInsertionIndexForRenderIndexImpl = childInsertionIndexForRenderIndex;
const clearEachIndexImpl = clearEachIndex;
const clearElementIndexImpl = clearElementIndex;
const clearEventIndexImpl = clearEventIndex;
const clearRenderChildrenImpl = clearRenderChildren;
const clearRenderNodeIndexImpl = clearRenderNodeIndex;
const clearScopeSiteIndexImpl = clearScopeSiteIndex;
const clearSignalBoolAttrIndexImpl = clearSignalBoolAttrIndex;
const clearSignalTextAttrIndexImpl = clearSignalTextAttrIndex;
const clearSignalTextNodeIndexImpl = clearSignalTextNodeIndex;
const clearStateIndexImpl = clearStateIndex;
const clearStaticBoolAttrIndexImpl = clearStaticBoolAttrIndex;
const clearStaticTextAttrIndexImpl = clearStaticTextAttrIndex;
const clearTextNodeIndexImpl = clearTextNodeIndex;
const clearWhenIndexImpl = clearWhenIndex;
const customTextAttrDescriptorExistsImpl = customTextAttrDescriptorExists;
const elemDescriptorIndexImpl = elemDescriptorIndex;
const ensureElemDescriptorIndexImpl = ensureElemDescriptorIndex;
const ensureFirstRenderChildSlotImpl = ensureFirstRenderChildSlot;
const ensureLastRenderChildSlotImpl = ensureLastRenderChildSlot;
const ensureNextRenderSiblingSlotImpl = ensureNextRenderSiblingSlot;
const ensureNodeDescriptorIndexImpl = ensureNodeDescriptorIndex;
const ensureRenderMetadataImpl = ensureRenderMetadata;
const firstRenderChildImpl = firstRenderChild;
const insertRenderChildrenImpl = insertRenderChildren;
const lastRenderChildImpl = lastRenderChild;
const nextRenderSiblingImpl = nextRenderSibling;
const nodeDescriptorIndexImpl = nodeDescriptorIndex;
const recordEachIndexImpl = recordEachIndex;
const recordElementIndexImpl = recordElementIndex;
const recordEventIndexImpl = recordEventIndex;
const recordRenderNodeIndexImpl = recordRenderNodeIndex;
const recordScopeSiteIndexImpl = recordScopeSiteIndex;
const recordSignalBoolAttrIndexImpl = recordSignalBoolAttrIndex;
const recordSignalTextAttrIndexImpl = recordSignalTextAttrIndex;
const recordSignalTextNodeIndexImpl = recordSignalTextNodeIndex;
const recordStateIndexImpl = recordStateIndex;
const recordStaticBoolAttrIndexImpl = recordStaticBoolAttrIndex;
const recordStaticTextAttrIndexImpl = recordStaticTextAttrIndex;
const recordTextNodeIndexImpl = recordTextNodeIndex;
const recordWhenIndexImpl = recordWhenIndex;
const refreshRenderIndexesFromImpl = refreshRenderIndexesFrom;
const removeRenderChildImpl = removeRenderChild;
const removeRenderMetadataIfEmptyImpl = removeRenderMetadataIfEmpty;
const renderNodeIndexImpl = renderNodeIndex;
const replaceRenderChildrenIndexImpl = replaceRenderChildrenIndex;
const updateEachIndexImpl = updateEachIndex;
const updateElementIndexImpl = updateElementIndex;
const updateEventIndexImpl = updateEventIndex;
const updateRenderNodeIndexImpl = updateRenderNodeIndex;
const updateScopeSiteIndexImpl = updateScopeSiteIndex;
const updateSignalBoolAttrIndexImpl = updateSignalBoolAttrIndex;
const updateSignalTextAttrIndexImpl = updateSignalTextAttrIndex;
const updateSignalTextNodeIndexImpl = updateSignalTextNodeIndex;
const updateStateIndexImpl = updateStateIndex;
const updateStaticBoolAttrIndexImpl = updateStaticBoolAttrIndex;
const updateStaticTextAttrIndexImpl = updateStaticTextAttrIndex;
const updateTextNodeIndexImpl = updateTextNodeIndex;
const updateWhenIndexImpl = updateWhenIndex;

pub const Stream = struct {
    pub const RenderNode = StreamRenderNode;
    pub const ElementDesc = StreamElementDesc;
    pub const TextNodeDesc = StreamTextNodeDesc;
    pub const SignalTextNodeDesc = StreamSignalTextNodeDesc;

    render_nodes: std.ArrayListUnmanaged(StreamRenderNode) = .empty,
    elements: std.ArrayListUnmanaged(StreamElementDesc) = .empty,
    text_nodes: std.ArrayListUnmanaged(StreamTextNodeDesc) = .empty,
    signal_text_nodes: std.ArrayListUnmanaged(StreamSignalTextNodeDesc) = .empty,
    static_text_attrs: std.ArrayListUnmanaged(StaticTextAttrDesc) = .empty,
    signal_text_attrs: std.ArrayListUnmanaged(SignalTextAttrDesc) = .empty,
    static_custom_text_attrs: std.ArrayListUnmanaged(StaticCustomTextAttrDesc) = .empty,
    signal_custom_text_attrs: std.ArrayListUnmanaged(SignalCustomTextAttrDesc) = .empty,
    static_bool_attrs: std.ArrayListUnmanaged(StaticBoolAttrDesc) = .empty,
    signal_bool_attrs: std.ArrayListUnmanaged(SignalBoolAttrDesc) = .empty,
    on_changes: std.ArrayListUnmanaged(OnChangeDesc) = .empty,
    mounts: std.ArrayListUnmanaged(MountDesc) = .empty,
    cleanups: std.ArrayListUnmanaged(CleanupDesc) = .empty,
    events: std.ArrayListUnmanaged(EventDesc) = .empty,
    named_events: std.ArrayListUnmanaged(NamedEventDesc) = .empty,
    scope_sites: std.ArrayListUnmanaged(ScopeSiteDesc) = .empty,
    states: std.ArrayListUnmanaged(StateDesc) = .empty,
    whens: std.ArrayListUnmanaged(WhenDesc) = .empty,
    eaches: std.ArrayListUnmanaged(EachDesc) = .empty,
    signal_records_by_token: std.AutoHashMapUnmanaged(usize, *SignalRecord) = .{},
    signal_record_descriptor_uses_by_token: std.AutoHashMapUnmanaged(usize, usize) = .{},
    render_metadata_by_elem_id: std.AutoHashMapUnmanaged(u64, RenderElemIndex) = .{},
    descriptor_indexes_by_elem_id: std.ArrayListUnmanaged(ElemDescriptorIndex) = .empty,
    descriptor_indexes_by_node_id: std.ArrayListUnmanaged(NodeDescriptorIndex) = .empty,
    next_elem_id: u64 = 1,

    pub fn ensureElemDescriptorIndex(self: *Stream, allocator: std.mem.Allocator, elem_id: u64) *ElemDescriptorIndex {
        return ensureElemDescriptorIndexImpl(Stream, self, allocator, elem_id);
    }

    pub fn elemDescriptorIndex(self: *const Stream, elem_id: u64) ?ElemDescriptorIndex {
        return elemDescriptorIndexImpl(Stream, self, elem_id);
    }

    pub fn ensureNodeDescriptorIndex(self: *Stream, allocator: std.mem.Allocator, node_id: u64) *NodeDescriptorIndex {
        return ensureNodeDescriptorIndexImpl(Stream, self, allocator, node_id);
    }

    pub fn nodeDescriptorIndex(self: *const Stream, node_id: u64) ?NodeDescriptorIndex {
        return nodeDescriptorIndexImpl(Stream, self, node_id);
    }

    pub fn ensureRenderMetadata(self: *Stream, allocator: std.mem.Allocator, elem_id: u64) *RenderElemIndex {
        return ensureRenderMetadataImpl(Stream, self, allocator, elem_id);
    }

    pub fn removeRenderMetadataIfEmpty(self: *Stream, elem_id: u64) void {
        removeRenderMetadataIfEmptyImpl(Stream, self, elem_id);
    }

    pub fn renderNodeIndex(self: *const Stream, elem_id: u64) ?usize {
        return renderNodeIndexImpl(Stream, self, elem_id);
    }

    pub fn recordRenderNodeIndex(self: *Stream, allocator: std.mem.Allocator, elem_id: u64, index: usize) void {
        recordRenderNodeIndexImpl(Stream, self, allocator, elem_id, index);
    }

    pub fn updateRenderNodeIndex(self: *Stream, elem_id: u64, index: usize) void {
        updateRenderNodeIndexImpl(Stream, self, elem_id, index);
    }

    pub fn clearRenderNodeIndex(self: *Stream, elem_id: u64, expected: usize) void {
        clearRenderNodeIndexImpl(Stream, self, elem_id, expected);
    }

    pub fn ensureFirstRenderChildSlot(self: *Stream, allocator: std.mem.Allocator, parent_elem_id: u64) *?u64 {
        return ensureFirstRenderChildSlotImpl(Stream, self, allocator, parent_elem_id);
    }

    pub fn ensureLastRenderChildSlot(self: *Stream, allocator: std.mem.Allocator, parent_elem_id: u64) *?u64 {
        return ensureLastRenderChildSlotImpl(Stream, self, allocator, parent_elem_id);
    }

    pub fn ensureNextRenderSiblingSlot(self: *Stream, allocator: std.mem.Allocator, elem_id: u64) *?u64 {
        return ensureNextRenderSiblingSlotImpl(Stream, self, allocator, elem_id);
    }

    pub fn firstRenderChild(self: *const Stream, parent_elem_id: u64) ?u64 {
        return firstRenderChildImpl(Stream, self, parent_elem_id);
    }

    pub fn lastRenderChild(self: *const Stream, parent_elem_id: u64) ?u64 {
        return lastRenderChildImpl(Stream, self, parent_elem_id);
    }

    pub fn nextRenderSibling(self: *const Stream, elem_id: u64) ?u64 {
        return nextRenderSiblingImpl(Stream, self, elem_id);
    }

    pub fn appendRenderChild(self: *Stream, allocator: std.mem.Allocator, parent_elem_id: u64, elem_id: u64) void {
        appendRenderChildImpl(Stream, self, allocator, parent_elem_id, elem_id);
    }

    pub fn clearRenderChildren(self: *Stream, parent_elem_id: u64) void {
        clearRenderChildrenImpl(Stream, self, parent_elem_id);
    }

    pub fn removeRenderChild(self: *Stream, parent_elem_id: u64, elem_id: u64) void {
        removeRenderChildImpl(Stream, self, parent_elem_id, elem_id);
    }

    pub fn insertRenderChildren(self: *Stream, allocator: std.mem.Allocator, parent_elem_id: u64, index: usize, elem_ids: []const u64) void {
        insertRenderChildrenImpl(Stream, self, allocator, parent_elem_id, index, elem_ids);
    }

    pub fn replaceRenderChildrenIndex(self: *Stream, allocator: std.mem.Allocator, parent_elem_id: u64, elem_ids: []const u64) void {
        replaceRenderChildrenIndexImpl(Stream, self, allocator, parent_elem_id, elem_ids);
    }

    pub fn childInsertionIndexForRenderIndex(self: *const Stream, parent_elem_id: u64, render_insert_index: usize) usize {
        return childInsertionIndexForRenderIndexImpl(Stream, self, parent_elem_id, render_insert_index);
    }

    pub fn refreshRenderIndexesFrom(self: *Stream, allocator: std.mem.Allocator, start_index: usize, metrics: anytype) void {
        refreshRenderIndexesFromImpl(Stream, self, allocator, start_index, metrics);
    }

    pub fn moveReplacementRenderChildren(self: *Stream, allocator: std.mem.Allocator, replacement: *Stream, elem_id: u64) void {
        self.clearRenderChildren(elem_id);
        const first_child = replacement.firstRenderChild(elem_id) orelse return;
        self.ensureFirstRenderChildSlot(allocator, elem_id).* = first_child;
        self.ensureLastRenderChildSlot(allocator, elem_id).* = replacement.lastRenderChild(elem_id) orelse @panic("replacement child index was missing its last child");

        var child: ?u64 = first_child;
        while (child) |child_id| {
            const next = replacement.nextRenderSibling(child_id);
            self.ensureNextRenderSiblingSlot(allocator, child_id).* = next;
            child = next;
        }

        if (replacement.render_metadata_by_elem_id.getPtr(elem_id)) |replacement_metadata| {
            replacement_metadata.first_child = null;
            replacement_metadata.last_child = null;
        }
        replacement.removeRenderMetadataIfEmpty(elem_id);
    }

    pub fn replaceRenderRangeWithStream(self: *Stream, allocator: std.mem.Allocator, render_start: usize, removed_nodes: []const StreamRenderNode, replacement: *Stream, metrics: anytype) void {
        const ChildInsert = struct {
            parent_elem_id: u64,
            insertion_index: usize,
            elem_ids: std.ArrayListUnmanaged(u64) = .empty,

            fn deinit(insert: *@This(), alloc: std.mem.Allocator) void {
                insert.elem_ids.deinit(alloc);
                insert.* = undefined;
            }
        };

        var child_inserts: std.ArrayListUnmanaged(ChildInsert) = .empty;
        defer {
            for (child_inserts.items) |*insert| {
                insert.deinit(allocator);
            }
            child_inserts.deinit(allocator);
        }

        for (removed_nodes, render_start..) |node, index| {
            const parent_elem_id = renderNodeParentElemId(Stream, self, node);
            if (!renderNodeSliceContainsElem(removed_nodes, parent_elem_id)) {
                self.removeRenderChild(parent_elem_id, node.elem_id);
            }
            self.clearRenderChildren(node.elem_id);
            self.clearRenderNodeIndex(node.elem_id, index);
        }

        for (replacement.render_nodes.items) |node| {
            const parent_elem_id = renderNodeParentElemId(Stream, replacement, node);
            const parent_in_replacement = parent_elem_id != 0 and replacement.renderNodeIndex(parent_elem_id) != null;
            if (!parent_in_replacement) {
                var group_index: ?usize = null;
                for (child_inserts.items, 0..) |insert, index| {
                    if (insert.parent_elem_id == parent_elem_id) {
                        group_index = index;
                        break;
                    }
                }

                const index = group_index orelse index: {
                    const insertion_index = self.childInsertionIndexForRenderIndex(parent_elem_id, render_start);
                    child_inserts.append(allocator, .{
                        .parent_elem_id = parent_elem_id,
                        .insertion_index = insertion_index,
                    }) catch @panic("out of memory");
                    break :index child_inserts.items.len - 1;
                };
                child_inserts.items[index].elem_ids.append(allocator, node.elem_id) catch @panic("out of memory");
            }
            self.moveReplacementRenderChildren(allocator, replacement, node.elem_id);
        }

        for (child_inserts.items) |insert| {
            self.insertRenderChildren(allocator, insert.parent_elem_id, insert.insertion_index, insert.elem_ids.items);
            if (insert.elem_ids.items.len != 0 and self.firstRenderChild(insert.parent_elem_id) == null) {
                @panic("render child insertion did not update parent child index");
            }
        }

        self.render_nodes.replaceRange(allocator, render_start, removed_nodes.len, replacement.render_nodes.items) catch @panic("out of memory");
        replacement.render_nodes.items.len = 0;
        self.refreshRenderIndexesFrom(allocator, render_start, metrics);
    }

    pub fn recordElementIndex(self: *Stream, allocator: std.mem.Allocator, elem_id: u64, index: usize) void {
        recordElementIndexImpl(Stream, self, allocator, elem_id, index);
    }

    pub fn updateElementIndex(self: *Stream, elem_id: u64, index: usize) void {
        updateElementIndexImpl(Stream, self, elem_id, index);
    }

    pub fn clearElementIndex(self: *Stream, elem_id: u64, expected: usize) void {
        clearElementIndexImpl(Stream, self, elem_id, expected);
    }

    pub fn recordTextNodeIndex(self: *Stream, allocator: std.mem.Allocator, elem_id: u64, index: usize) void {
        recordTextNodeIndexImpl(Stream, self, allocator, elem_id, index);
    }

    pub fn updateTextNodeIndex(self: *Stream, elem_id: u64, index: usize) void {
        updateTextNodeIndexImpl(Stream, self, elem_id, index);
    }

    pub fn clearTextNodeIndex(self: *Stream, elem_id: u64, expected: usize) void {
        clearTextNodeIndexImpl(Stream, self, elem_id, expected);
    }

    pub fn recordSignalTextNodeIndex(self: *Stream, allocator: std.mem.Allocator, elem_id: u64, index: usize) void {
        recordSignalTextNodeIndexImpl(Stream, self, allocator, elem_id, index);
    }

    pub fn updateSignalTextNodeIndex(self: *Stream, elem_id: u64, index: usize) void {
        updateSignalTextNodeIndexImpl(Stream, self, elem_id, index);
    }

    pub fn clearSignalTextNodeIndex(self: *Stream, elem_id: u64, expected: usize) void {
        clearSignalTextNodeIndexImpl(Stream, self, elem_id, expected);
    }

    pub fn recordStaticTextAttrIndex(self: *Stream, allocator: std.mem.Allocator, elem_id: u64, field: TextField, index: usize) void {
        recordStaticTextAttrIndexImpl(Stream, self, allocator, elem_id, field, index);
    }

    pub fn updateStaticTextAttrIndex(self: *Stream, elem_id: u64, field: TextField, index: usize) void {
        updateStaticTextAttrIndexImpl(Stream, self, elem_id, field, index);
    }

    pub fn clearStaticTextAttrIndex(self: *Stream, elem_id: u64, field: TextField, expected: usize) void {
        clearStaticTextAttrIndexImpl(Stream, self, elem_id, field, expected);
    }

    pub fn recordSignalTextAttrIndex(self: *Stream, allocator: std.mem.Allocator, elem_id: u64, field: TextField, index: usize) void {
        recordSignalTextAttrIndexImpl(Stream, self, allocator, elem_id, field, index);
    }

    pub fn updateSignalTextAttrIndex(self: *Stream, elem_id: u64, field: TextField, index: usize) void {
        updateSignalTextAttrIndexImpl(Stream, self, elem_id, field, index);
    }

    pub fn clearSignalTextAttrIndex(self: *Stream, elem_id: u64, field: TextField, expected: usize) void {
        clearSignalTextAttrIndexImpl(Stream, self, elem_id, field, expected);
    }

    pub fn recordStaticBoolAttrIndex(self: *Stream, allocator: std.mem.Allocator, elem_id: u64, field: BoolField, index: usize) void {
        recordStaticBoolAttrIndexImpl(Stream, self, allocator, elem_id, field, index);
    }

    pub fn updateStaticBoolAttrIndex(self: *Stream, elem_id: u64, field: BoolField, index: usize) void {
        updateStaticBoolAttrIndexImpl(Stream, self, elem_id, field, index);
    }

    pub fn clearStaticBoolAttrIndex(self: *Stream, elem_id: u64, field: BoolField, expected: usize) void {
        clearStaticBoolAttrIndexImpl(Stream, self, elem_id, field, expected);
    }

    pub fn recordSignalBoolAttrIndex(self: *Stream, allocator: std.mem.Allocator, elem_id: u64, field: BoolField, index: usize) void {
        recordSignalBoolAttrIndexImpl(Stream, self, allocator, elem_id, field, index);
    }

    pub fn updateSignalBoolAttrIndex(self: *Stream, elem_id: u64, field: BoolField, index: usize) void {
        updateSignalBoolAttrIndexImpl(Stream, self, elem_id, field, index);
    }

    pub fn clearSignalBoolAttrIndex(self: *Stream, elem_id: u64, field: BoolField, expected: usize) void {
        clearSignalBoolAttrIndexImpl(Stream, self, elem_id, field, expected);
    }

    pub fn recordEventIndex(self: *Stream, allocator: std.mem.Allocator, elem_id: u64, kind: EventKind, index: usize) void {
        recordEventIndexImpl(Stream, self, allocator, elem_id, kind, index);
    }

    pub fn updateEventIndex(self: *Stream, elem_id: u64, kind: EventKind, index: usize) void {
        updateEventIndexImpl(Stream, self, elem_id, kind, index);
    }

    pub fn clearEventIndex(self: *Stream, elem_id: u64, kind: EventKind, expected: usize) void {
        clearEventIndexImpl(Stream, self, elem_id, kind, expected);
    }

    pub fn recordScopeSiteIndex(self: *Stream, allocator: std.mem.Allocator, node_id: u64, kind: ScopeSiteKind, index: usize) void {
        recordScopeSiteIndexImpl(Stream, self, allocator, node_id, kind, index);
    }

    pub fn updateScopeSiteIndex(self: *Stream, node_id: u64, kind: ScopeSiteKind, index: usize) void {
        updateScopeSiteIndexImpl(Stream, self, node_id, kind, index);
    }

    pub fn clearScopeSiteIndex(self: *Stream, node_id: u64, kind: ScopeSiteKind, expected: usize) void {
        clearScopeSiteIndexImpl(Stream, self, node_id, kind, expected);
    }

    pub fn recordStateIndex(self: *Stream, allocator: std.mem.Allocator, node_id: u64, index: usize) void {
        recordStateIndexImpl(Stream, self, allocator, node_id, index);
    }

    pub fn updateStateIndex(self: *Stream, node_id: u64, index: usize) void {
        updateStateIndexImpl(Stream, self, node_id, index);
    }

    pub fn clearStateIndex(self: *Stream, node_id: u64, expected: usize) void {
        clearStateIndexImpl(Stream, self, node_id, expected);
    }

    pub fn recordWhenIndex(self: *Stream, allocator: std.mem.Allocator, node_id: u64, index: usize) void {
        recordWhenIndexImpl(Stream, self, allocator, node_id, index);
    }

    pub fn updateWhenIndex(self: *Stream, node_id: u64, index: usize) void {
        updateWhenIndexImpl(Stream, self, node_id, index);
    }

    pub fn clearWhenIndex(self: *Stream, node_id: u64, expected: usize) void {
        clearWhenIndexImpl(Stream, self, node_id, expected);
    }

    pub fn recordEachIndex(self: *Stream, allocator: std.mem.Allocator, node_id: u64, index: usize) void {
        recordEachIndexImpl(Stream, self, allocator, node_id, index);
    }

    pub fn updateEachIndex(self: *Stream, node_id: u64, index: usize) void {
        updateEachIndexImpl(Stream, self, node_id, index);
    }

    pub fn clearEachIndex(self: *Stream, node_id: u64, expected: usize) void {
        clearEachIndexImpl(Stream, self, node_id, expected);
    }

    pub fn deinit(self: *Stream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype) void {
        self.render_nodes.deinit(allocator);

        for (self.elements.items) |desc| {
            allocator.free(desc.tag);
        }
        self.elements.deinit(allocator);

        for (self.text_nodes.items) |desc| {
            allocator.free(desc.value);
        }
        self.text_nodes.deinit(allocator);

        for (self.signal_text_nodes.items) |*desc| {
            desc.cached_value.deinit(ctx, roc_host, metrics);
            desc.signal.deinit(allocator, ctx, roc_host, metrics);
            releaseHostTextRead(desc.read, roc_host, metrics);
        }
        self.signal_text_nodes.deinit(allocator);

        for (self.static_text_attrs.items) |desc| {
            allocator.free(desc.value);
        }
        self.static_text_attrs.deinit(allocator);

        for (self.signal_text_attrs.items) |*desc| {
            desc.cached_value.deinit(ctx, roc_host, metrics);
            desc.signal.deinit(allocator, ctx, roc_host, metrics);
            releaseHostTextRead(desc.read, roc_host, metrics);
        }
        self.signal_text_attrs.deinit(allocator);

        for (self.static_custom_text_attrs.items) |desc| {
            allocator.free(desc.name);
            allocator.free(desc.value);
        }
        self.static_custom_text_attrs.deinit(allocator);

        for (self.signal_custom_text_attrs.items) |*desc| {
            allocator.free(desc.name);
            desc.cached_value.deinit(ctx, roc_host, metrics);
            desc.signal.deinit(allocator, ctx, roc_host, metrics);
            releaseHostTextRead(desc.read, roc_host, metrics);
        }
        self.signal_custom_text_attrs.deinit(allocator);

        self.static_bool_attrs.deinit(allocator);

        for (self.signal_bool_attrs.items) |*desc| {
            desc.cached_value.deinit(ctx, roc_host, metrics);
            desc.signal.deinit(allocator, ctx, roc_host, metrics);
            releaseHostBoolRead(desc.read, roc_host, metrics);
        }
        self.signal_bool_attrs.deinit(allocator);

        for (self.on_changes.items) |*desc| {
            desc.cached_value.deinit(ctx, roc_host, metrics);
            desc.signal.deinit(allocator, ctx, roc_host, metrics);
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(desc.to_cmd, roc_host);
        }
        self.on_changes.deinit(allocator);

        for (self.mounts.items) |desc| {
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(desc.to_cmd, roc_host);
        }
        self.mounts.deinit(allocator);

        for (self.cleanups.items) |desc| {
            allocator.free(desc.name);
        }
        self.cleanups.deinit(allocator);

        for (self.events.items) |desc| {
            if (desc.owns_payload_reducer) releaseHostEventReducer(desc.payload_reducer, roc_host, metrics);
        }
        self.events.deinit(allocator);

        for (self.named_events.items) |desc| {
            allocator.free(desc.name);
            if (desc.owns_payload_reducer) releaseHostEventReducer(desc.payload_reducer, roc_host, metrics);
        }
        self.named_events.deinit(allocator);

        for (self.scope_sites.items) |desc| {
            allocator.free(desc.binder_bindings);
        }
        self.scope_sites.deinit(allocator);

        for (self.states.items) |desc| {
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(desc.initial, roc_host);
            releaseHostValueCapability(desc.cap, roc_host, metrics);
        }
        self.states.deinit(allocator);

        for (self.whens.items) |*desc| {
            desc.cached_value.deinit(ctx, roc_host, metrics);
            desc.condition.deinit(allocator, ctx, roc_host, metrics);
            releaseHostBoolRead(desc.read, roc_host, metrics);
            abi.decrefElem(desc.when_false, roc_host);
            abi.decrefElem(desc.when_true, roc_host);
        }
        self.whens.deinit(allocator);

        for (self.eaches.items) |*desc| {
            desc.cached_value.deinit(ctx, roc_host, metrics);
            desc.items.deinit(allocator, ctx, roc_host, metrics);
            releaseHostEachOps(desc.ops, roc_host, metrics);
        }
        self.eaches.deinit(allocator);

        self.signal_records_by_token.deinit(allocator);
        self.signal_record_descriptor_uses_by_token.deinit(allocator);
        self.render_metadata_by_elem_id.deinit(allocator);
        self.descriptor_indexes_by_elem_id.deinit(allocator);
        self.descriptor_indexes_by_node_id.deinit(allocator);

        self.* = .{};
    }

    pub fn signalRecordByToken(self: *Stream, token: HostSignalToken) ?*SignalRecord {
        return self.signal_records_by_token.get(@intFromPtr(token));
    }

    pub fn rememberSignalRecord(self: *Stream, allocator: std.mem.Allocator, record: *SignalRecord) void {
        const token = record.token() orelse return;
        const entry = self.signal_records_by_token.getOrPut(allocator, @intFromPtr(token)) catch @panic("out of memory");
        if (entry.found_existing) {
            if (entry.value_ptr.* != record) @panic("signal token was bound to multiple host records");
            return;
        }
        entry.value_ptr.* = record;
    }

    fn incrementSignalRecordDescriptorUse(self: *Stream, allocator: std.mem.Allocator, record: *SignalRecord) void {
        const token = record.token() orelse return;
        const key = @intFromPtr(token);
        const entry = self.signal_record_descriptor_uses_by_token.getOrPut(allocator, key) catch @panic("out of memory");
        if (entry.found_existing) {
            entry.value_ptr.* += 1;
        } else {
            entry.value_ptr.* = 1;
        }
    }

    fn decrementSignalRecordDescriptorUse(self: *Stream, record: *SignalRecord) void {
        const token = record.token() orelse return;
        const key = @intFromPtr(token);
        const count = self.signal_record_descriptor_uses_by_token.getPtr(key) orelse @panic("signal token descriptor use underflow");
        if (count.* == 0) @panic("signal token descriptor use underflow");
        count.* -= 1;
        if (count.* != 0) return;

        _ = self.signal_record_descriptor_uses_by_token.fetchRemove(key) orelse @panic("signal token descriptor use disappeared during removal");
        const existing = self.signal_records_by_token.get(key) orelse @panic("signal token descriptor use had no record");
        if (existing != record) @panic("signal token descriptor use pointed at the wrong record");
        _ = self.signal_records_by_token.fetchRemove(key) orelse @panic("signal token record disappeared during removal");
    }

    pub fn rememberSignalRecordTree(self: *Stream, allocator: std.mem.Allocator, record: *SignalRecord) void {
        self.rememberSignalRecord(allocator, record);
        self.incrementSignalRecordDescriptorUse(allocator, record);
        switch (record.payload) {
            .ref, .const_value => {},
            .map => |payload| self.rememberSignalRecordTree(allocator, payload.input),
            .map2 => |payload| {
                self.rememberSignalRecordTree(allocator, payload.left);
                self.rememberSignalRecordTree(allocator, payload.right);
            },
            .combine => |payload| {
                for (payload.children) |child| {
                    self.rememberSignalRecordTree(allocator, child);
                }
            },
            .task_source, .interval_source => {},
        }
    }

    pub fn forgetSignalRecordTree(self: *Stream, record: *SignalRecord) void {
        self.decrementSignalRecordDescriptorUse(record);
        switch (record.payload) {
            .ref, .const_value => {},
            .map => |payload| self.forgetSignalRecordTree(payload.input),
            .map2 => |payload| {
                self.forgetSignalRecordTree(payload.left);
                self.forgetSignalRecordTree(payload.right);
            },
            .combine => |payload| {
                for (payload.children) |child| {
                    self.forgetSignalRecordTree(child);
                }
            },
            .task_source, .interval_source => {},
        }
    }

    pub fn appendElement(self: *Stream, allocator: std.mem.Allocator, elem_id: u64, parent_elem_id: u64, scope_id: u64, tag: []const u8) u64 {
        return appendElementImpl(Stream, self, allocator, elem_id, parent_elem_id, scope_id, tag);
    }

    pub fn appendTextNode(self: *Stream, allocator: std.mem.Allocator, elem_id: u64, parent_elem_id: u64, scope_id: u64, value: []const u8) void {
        appendTextNodeImpl(Stream, self, allocator, elem_id, parent_elem_id, scope_id, value);
    }

    pub fn appendSignalTextNode(self: *Stream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, elem_id: u64, parent_elem_id: u64, scope_id: u64, signal: HostSignalBinding, read: HostTextRead) void {
        self.next_elem_id += 1;
        self.rememberSignalRecordTree(allocator, signal.record);
        const retained_read = retainHostTextRead(read, metrics);
        const signal_text_node_index = self.signal_text_nodes.items.len;
        const render_index = self.render_nodes.items.len;

        self.render_nodes.append(allocator, .{ .elem_id = elem_id, .kind = .signal_text }) catch {
            var owned_signal = signal;
            owned_signal.deinit(allocator, ctx, roc_host, metrics);
            releaseHostTextRead(retained_read, roc_host, metrics);
            @panic("out of memory");
        };
        self.signal_text_nodes.append(allocator, .{
            .elem_id = elem_id,
            .parent_elem_id = parent_elem_id,
            .scope_id = scope_id,
            .signal = signal,
            .read = retained_read,
        }) catch {
            var owned_signal = signal;
            owned_signal.deinit(allocator, ctx, roc_host, metrics);
            releaseHostTextRead(retained_read, roc_host, metrics);
            @panic("out of memory");
        };
        self.recordSignalTextNodeIndex(allocator, elem_id, signal_text_node_index);
        self.recordRenderNodeIndex(allocator, elem_id, render_index);
        self.appendRenderChild(allocator, parent_elem_id, elem_id);
    }

    pub fn appendStaticTextAttr(self: *Stream, allocator: std.mem.Allocator, elem_id: u64, field: TextField, value: []const u8) void {
        appendStaticTextAttrImpl(Stream, self, allocator, elem_id, field, value);
    }

    pub fn appendSignalTextAttr(self: *Stream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, elem_id: u64, field: TextField, signal: HostSignalBinding, read: HostTextRead) void {
        self.rememberSignalRecordTree(allocator, signal.record);
        const retained_read = retainHostTextRead(read, metrics);
        const attr_index = self.signal_text_attrs.items.len;
        self.signal_text_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .signal = signal,
            .read = retained_read,
        }) catch {
            var owned_signal = signal;
            owned_signal.deinit(allocator, ctx, roc_host, metrics);
            releaseHostTextRead(retained_read, roc_host, metrics);
            @panic("out of memory");
        };
        self.recordSignalTextAttrIndex(allocator, elem_id, field, attr_index);
    }

    pub fn customTextAttrDescriptorExists(self: *const Stream, elem_id: u64, name: []const u8) bool {
        return customTextAttrDescriptorExistsImpl(Stream, self, elem_id, name);
    }

    pub fn appendStaticCustomTextAttr(self: *Stream, allocator: std.mem.Allocator, elem_id: u64, name: []const u8, value: []const u8) void {
        appendStaticCustomTextAttrImpl(Stream, self, allocator, elem_id, name, value);
    }

    pub fn appendSignalCustomTextAttr(self: *Stream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, elem_id: u64, name: []const u8, signal: HostSignalBinding, read: HostTextRead) void {
        if (name.len == 0) @panic("custom text attr descriptor used an empty name");
        if (self.customTextAttrDescriptorExists(elem_id, name)) @panic("element has duplicate custom text attr descriptors");

        self.rememberSignalRecordTree(allocator, signal.record);
        const name_copy = allocator.dupe(u8, name) catch @panic("out of memory");
        const retained_read = retainHostTextRead(read, metrics);
        self.signal_custom_text_attrs.append(allocator, .{
            .elem_id = elem_id,
            .name = name_copy,
            .signal = signal,
            .read = retained_read,
        }) catch {
            allocator.free(name_copy);
            var owned_signal = signal;
            owned_signal.deinit(allocator, ctx, roc_host, metrics);
            releaseHostTextRead(retained_read, roc_host, metrics);
            @panic("out of memory");
        };
    }

    pub fn appendStaticBoolAttr(self: *Stream, allocator: std.mem.Allocator, elem_id: u64, field: BoolField, value: bool) void {
        appendStaticBoolAttrImpl(Stream, self, allocator, elem_id, field, value);
    }

    pub fn appendSignalBoolAttr(self: *Stream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, elem_id: u64, field: BoolField, signal: HostSignalBinding, read: HostBoolRead) void {
        self.rememberSignalRecordTree(allocator, signal.record);
        const retained_read = retainHostBoolRead(read, metrics);
        const attr_index = self.signal_bool_attrs.items.len;
        self.signal_bool_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .signal = signal,
            .read = retained_read,
        }) catch {
            var owned_signal = signal;
            owned_signal.deinit(allocator, ctx, roc_host, metrics);
            releaseHostBoolRead(retained_read, roc_host, metrics);
            @panic("out of memory");
        };
        self.recordSignalBoolAttrIndex(allocator, elem_id, field, attr_index);
    }

    pub fn appendOnChange(self: *Stream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, scope_id: u64, signal: HostSignalBinding, to_cmd: abi.RocErasedCallable) void {
        self.rememberSignalRecordTree(allocator, signal.record);
        abi.increfErasedCallable(to_cmd, 1);
        metrics.bump(.closure_retains, 1);
        self.on_changes.append(allocator, .{
            .scope_id = scope_id,
            .signal = signal,
            .to_cmd = to_cmd,
        }) catch {
            var owned_signal = signal;
            owned_signal.deinit(allocator, ctx, roc_host, metrics);
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(to_cmd, roc_host);
            @panic("out of memory");
        };
    }

    pub fn appendMount(self: *Stream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype, scope_id: u64, to_cmd: abi.RocErasedCallable, run_on_mount: bool) void {
        abi.increfErasedCallable(to_cmd, 1);
        metrics.bump(.closure_retains, 1);
        self.mounts.append(allocator, .{
            .scope_id = scope_id,
            .to_cmd = to_cmd,
            .run_on_mount = run_on_mount,
        }) catch {
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(to_cmd, roc_host);
            @panic("out of memory");
        };
    }

    pub fn appendCleanup(self: *Stream, allocator: std.mem.Allocator, scope_id: u64, name: []const u8) void {
        appendCleanupImpl(Stream, self, allocator, scope_id, name);
    }

    pub fn appendEvent(self: *Stream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype, elem_id: u64, kind: EventKind, binder_token: BinderToken, target_node_id: u64, payload_kind: EventPayloadKind, payload_accessor: EventPayloadAccessor, payload_reducer: HostEventReducer) void {
        const retained_reducer = retainHostEventReducer(payload_reducer, metrics);
        const event_index = self.events.items.len;
        self.events.append(allocator, .{
            .elem_id = elem_id,
            .kind = kind,
            .binder_token = binder_token,
            .target_node_id = target_node_id,
            .payload_kind = payload_kind,
            .payload_accessor = payload_accessor,
            .payload_reducer = retained_reducer,
        }) catch {
            releaseHostEventReducer(retained_reducer, roc_host, metrics);
            @panic("out of memory");
        };
        self.recordEventIndex(allocator, elem_id, kind, event_index);
    }

    pub fn namedEventDescriptorExists(self: *const Stream, elem_id: u64, name: []const u8) bool {
        for (self.named_events.items) |desc| {
            if (desc.elem_id == elem_id and std.mem.eql(u8, desc.name, name)) return true;
        }
        return false;
    }

    pub fn appendNamedEvent(self: *Stream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype, elem_id: u64, name: []const u8, options: u64, binder_token: BinderToken, target_node_id: u64, payload_kind: EventPayloadKind, payload_accessor: EventPayloadAccessor, payload_reducer: HostEventReducer) void {
        if (name.len == 0) @panic("named event descriptor used an empty event name");
        if (self.namedEventDescriptorExists(elem_id, name)) @panic("element has duplicate named event descriptors");

        const retained_reducer = retainHostEventReducer(payload_reducer, metrics);
        const name_copy = allocator.dupe(u8, name) catch {
            releaseHostEventReducer(retained_reducer, roc_host, metrics);
            @panic("out of memory");
        };
        self.named_events.append(allocator, .{
            .elem_id = elem_id,
            .name = name_copy,
            .options = std.math.cast(u32, options) orelse @panic("named event listener options exceeded u32 range"),
            .binder_token = binder_token,
            .target_node_id = target_node_id,
            .payload_kind = payload_kind,
            .payload_accessor = payload_accessor,
            .payload_reducer = retained_reducer,
        }) catch {
            allocator.free(name_copy);
            releaseHostEventReducer(retained_reducer, roc_host, metrics);
            @panic("out of memory");
        };
    }

    pub fn appendScopeSite(self: *Stream, allocator: std.mem.Allocator, node_id: u64, scope_id: u64, ordinal: u64, parent_elem_id: u64, kind: ScopeSiteKind, binder_bindings: []const BinderBinding) void {
        appendScopeSiteImpl(Stream, self, allocator, node_id, scope_id, ordinal, parent_elem_id, kind, binder_bindings);
    }

    pub fn appendScopeSiteAt(self: *Stream, allocator: std.mem.Allocator, node_id: u64, scope_id: u64, ordinal: u64, parent_elem_id: u64, render_insert_index: usize, kind: ScopeSiteKind, binder_bindings: []const BinderBinding) void {
        appendScopeSiteAtImpl(Stream, self, allocator, node_id, scope_id, ordinal, parent_elem_id, render_insert_index, kind, binder_bindings);
    }

    pub fn appendState(self: *Stream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype, node_id: u64, initial: abi.RocErasedCallable, cap: HostValueCapability) void {
        _ = retainHostValueCapability(cap, metrics);
        abi.increfErasedCallable(initial, 1);
        metrics.bump(.closure_retains, 1);
        const state_index = self.states.items.len;
        self.states.append(allocator, .{
            .node_id = node_id,
            .initial = initial,
            .cap = cap,
        }) catch {
            releaseHostValueCapability(cap, roc_host, metrics);
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(initial, roc_host);
            @panic("out of memory");
        };
        self.recordStateIndex(allocator, node_id, state_index);
    }

    pub fn appendWhen(self: *Stream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, node_id: u64, condition: HostSignalBinding, read: HostBoolRead, when_false: abi.Elem, when_true: abi.Elem) void {
        self.rememberSignalRecordTree(allocator, condition.record);
        const retained_read = retainHostBoolRead(read, metrics);
        abi.increfElem(when_false, 1);
        abi.increfElem(when_true, 1);
        const when_index = self.whens.items.len;
        self.whens.append(allocator, .{
            .node_id = node_id,
            .condition = condition,
            .read = retained_read,
            .when_false = when_false,
            .when_true = when_true,
        }) catch {
            var owned_condition = condition;
            owned_condition.deinit(allocator, ctx, roc_host, metrics);
            releaseHostBoolRead(retained_read, roc_host, metrics);
            abi.decrefElem(when_false, roc_host);
            abi.decrefElem(when_true, roc_host);
            @panic("out of memory");
        };
        self.recordWhenIndex(allocator, node_id, when_index);
    }

    pub fn appendEach(self: *Stream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, node_id: u64, items: HostSignalBinding, ops: HostEachOps) void {
        self.rememberSignalRecordTree(allocator, items.record);
        const retained_ops = retainHostEachOps(ops, metrics);
        const each_index = self.eaches.items.len;
        self.eaches.append(allocator, .{
            .node_id = node_id,
            .items = items,
            .ops = retained_ops,
        }) catch {
            var owned_items = items;
            owned_items.deinit(allocator, ctx, roc_host, metrics);
            releaseHostEachOps(retained_ops, roc_host, metrics);
            @panic("out of memory");
        };
        self.recordEachIndex(allocator, node_id, each_index);
    }
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

pub fn ensureElemDescriptorIndex(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, elem_id: u64) *ElemDescriptorIndex {
    const index: usize = @intCast(elem_id);
    while (stream.descriptor_indexes_by_elem_id.items.len <= index) {
        stream.descriptor_indexes_by_elem_id.append(allocator, .{}) catch @panic("out of memory");
    }
    return &stream.descriptor_indexes_by_elem_id.items[index];
}

pub fn elemDescriptorIndex(comptime StreamType: type, stream: *const StreamType, elem_id: u64) ?ElemDescriptorIndex {
    if (elem_id >= stream.descriptor_indexes_by_elem_id.items.len) return null;
    return stream.descriptor_indexes_by_elem_id.items[@intCast(elem_id)];
}

pub fn ensureNodeDescriptorIndex(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, node_id: u64) *NodeDescriptorIndex {
    const index: usize = @intCast(node_id);
    while (stream.descriptor_indexes_by_node_id.items.len <= index) {
        stream.descriptor_indexes_by_node_id.append(allocator, .{}) catch @panic("out of memory");
    }
    return &stream.descriptor_indexes_by_node_id.items[index];
}

pub fn nodeDescriptorIndex(comptime StreamType: type, stream: *const StreamType, node_id: u64) ?NodeDescriptorIndex {
    if (node_id >= stream.descriptor_indexes_by_node_id.items.len) return null;
    return stream.descriptor_indexes_by_node_id.items[@intCast(node_id)];
}

pub fn recordElementIndex(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, elem_id: u64, index: usize) void {
    setFreshIndex(&ensureElemDescriptorIndex(StreamType, stream, allocator, elem_id).element, index);
}

pub fn updateElementIndex(comptime StreamType: type, stream: *StreamType, elem_id: u64, index: usize) void {
    updateIndex(&stream.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].element, index);
}

pub fn clearElementIndex(comptime StreamType: type, stream: *StreamType, elem_id: u64, expected: usize) void {
    clearIndex(&stream.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].element, expected);
}

pub fn recordTextNodeIndex(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, elem_id: u64, index: usize) void {
    setFreshIndex(&ensureElemDescriptorIndex(StreamType, stream, allocator, elem_id).text_node, index);
}

pub fn updateTextNodeIndex(comptime StreamType: type, stream: *StreamType, elem_id: u64, index: usize) void {
    updateIndex(&stream.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].text_node, index);
}

pub fn clearTextNodeIndex(comptime StreamType: type, stream: *StreamType, elem_id: u64, expected: usize) void {
    clearIndex(&stream.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].text_node, expected);
}

pub fn recordSignalTextNodeIndex(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, elem_id: u64, index: usize) void {
    setFreshIndex(&ensureElemDescriptorIndex(StreamType, stream, allocator, elem_id).signal_text_node, index);
}

pub fn updateSignalTextNodeIndex(comptime StreamType: type, stream: *StreamType, elem_id: u64, index: usize) void {
    updateIndex(&stream.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_text_node, index);
}

pub fn clearSignalTextNodeIndex(comptime StreamType: type, stream: *StreamType, elem_id: u64, expected: usize) void {
    clearIndex(&stream.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_text_node, expected);
}

pub fn recordStaticTextAttrIndex(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, elem_id: u64, field: TextField, index: usize) void {
    setFreshIndex(ensureElemDescriptorIndex(StreamType, stream, allocator, elem_id).static_text_attrs.slot(field), index);
}

pub fn updateStaticTextAttrIndex(comptime StreamType: type, stream: *StreamType, elem_id: u64, field: TextField, index: usize) void {
    updateIndex(stream.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].static_text_attrs.slot(field), index);
}

pub fn clearStaticTextAttrIndex(comptime StreamType: type, stream: *StreamType, elem_id: u64, field: TextField, expected: usize) void {
    clearIndex(stream.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].static_text_attrs.slot(field), expected);
}

pub fn recordSignalTextAttrIndex(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, elem_id: u64, field: TextField, index: usize) void {
    setFreshIndex(ensureElemDescriptorIndex(StreamType, stream, allocator, elem_id).signal_text_attrs.slot(field), index);
}

pub fn updateSignalTextAttrIndex(comptime StreamType: type, stream: *StreamType, elem_id: u64, field: TextField, index: usize) void {
    updateIndex(stream.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_text_attrs.slot(field), index);
}

pub fn clearSignalTextAttrIndex(comptime StreamType: type, stream: *StreamType, elem_id: u64, field: TextField, expected: usize) void {
    clearIndex(stream.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_text_attrs.slot(field), expected);
}

pub fn recordStaticBoolAttrIndex(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, elem_id: u64, field: BoolField, index: usize) void {
    setFreshIndex(ensureElemDescriptorIndex(StreamType, stream, allocator, elem_id).static_bool_attrs.slot(field), index);
}

pub fn updateStaticBoolAttrIndex(comptime StreamType: type, stream: *StreamType, elem_id: u64, field: BoolField, index: usize) void {
    updateIndex(stream.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].static_bool_attrs.slot(field), index);
}

pub fn clearStaticBoolAttrIndex(comptime StreamType: type, stream: *StreamType, elem_id: u64, field: BoolField, expected: usize) void {
    clearIndex(stream.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].static_bool_attrs.slot(field), expected);
}

pub fn recordSignalBoolAttrIndex(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, elem_id: u64, field: BoolField, index: usize) void {
    setFreshIndex(ensureElemDescriptorIndex(StreamType, stream, allocator, elem_id).signal_bool_attrs.slot(field), index);
}

pub fn updateSignalBoolAttrIndex(comptime StreamType: type, stream: *StreamType, elem_id: u64, field: BoolField, index: usize) void {
    updateIndex(stream.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_bool_attrs.slot(field), index);
}

pub fn clearSignalBoolAttrIndex(comptime StreamType: type, stream: *StreamType, elem_id: u64, field: BoolField, expected: usize) void {
    clearIndex(stream.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_bool_attrs.slot(field), expected);
}

pub fn recordEventIndex(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, elem_id: u64, kind: EventKind, index: usize) void {
    setFreshIndex(ensureElemDescriptorIndex(StreamType, stream, allocator, elem_id).events.slot(kind), index);
}

pub fn updateEventIndex(comptime StreamType: type, stream: *StreamType, elem_id: u64, kind: EventKind, index: usize) void {
    updateIndex(stream.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].events.slot(kind), index);
}

pub fn clearEventIndex(comptime StreamType: type, stream: *StreamType, elem_id: u64, kind: EventKind, expected: usize) void {
    clearIndex(stream.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].events.slot(kind), expected);
}

pub fn recordScopeSiteIndex(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, node_id: u64, kind: ScopeSiteKind, index: usize) void {
    setFreshIndex(ensureNodeDescriptorIndex(StreamType, stream, allocator, node_id).scope_sites.slot(kind), index);
}

pub fn updateScopeSiteIndex(comptime StreamType: type, stream: *StreamType, node_id: u64, kind: ScopeSiteKind, index: usize) void {
    updateIndex(stream.descriptor_indexes_by_node_id.items[@intCast(node_id)].scope_sites.slot(kind), index);
}

pub fn clearScopeSiteIndex(comptime StreamType: type, stream: *StreamType, node_id: u64, kind: ScopeSiteKind, expected: usize) void {
    clearIndex(stream.descriptor_indexes_by_node_id.items[@intCast(node_id)].scope_sites.slot(kind), expected);
}

pub fn recordStateIndex(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, node_id: u64, index: usize) void {
    setFreshIndex(&ensureNodeDescriptorIndex(StreamType, stream, allocator, node_id).state, index);
}

pub fn updateStateIndex(comptime StreamType: type, stream: *StreamType, node_id: u64, index: usize) void {
    updateIndex(&stream.descriptor_indexes_by_node_id.items[@intCast(node_id)].state, index);
}

pub fn clearStateIndex(comptime StreamType: type, stream: *StreamType, node_id: u64, expected: usize) void {
    clearIndex(&stream.descriptor_indexes_by_node_id.items[@intCast(node_id)].state, expected);
}

pub fn recordWhenIndex(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, node_id: u64, index: usize) void {
    setFreshIndex(&ensureNodeDescriptorIndex(StreamType, stream, allocator, node_id).when, index);
}

pub fn updateWhenIndex(comptime StreamType: type, stream: *StreamType, node_id: u64, index: usize) void {
    updateIndex(&stream.descriptor_indexes_by_node_id.items[@intCast(node_id)].when, index);
}

pub fn clearWhenIndex(comptime StreamType: type, stream: *StreamType, node_id: u64, expected: usize) void {
    clearIndex(&stream.descriptor_indexes_by_node_id.items[@intCast(node_id)].when, expected);
}

pub fn recordEachIndex(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, node_id: u64, index: usize) void {
    setFreshIndex(&ensureNodeDescriptorIndex(StreamType, stream, allocator, node_id).each, index);
}

pub fn updateEachIndex(comptime StreamType: type, stream: *StreamType, node_id: u64, index: usize) void {
    updateIndex(&stream.descriptor_indexes_by_node_id.items[@intCast(node_id)].each, index);
}

pub fn clearEachIndex(comptime StreamType: type, stream: *StreamType, node_id: u64, expected: usize) void {
    clearIndex(&stream.descriptor_indexes_by_node_id.items[@intCast(node_id)].each, expected);
}

pub fn ensureRenderMetadata(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, elem_id: u64) *RenderElemIndex {
    const entry = stream.render_metadata_by_elem_id.getOrPut(allocator, elem_id) catch @panic("out of memory");
    if (!entry.found_existing) entry.value_ptr.* = .{};
    return entry.value_ptr;
}

pub fn removeRenderMetadataIfEmpty(comptime StreamType: type, stream: *StreamType, elem_id: u64) void {
    const metadata = stream.render_metadata_by_elem_id.get(elem_id) orelse return;
    if (metadata.empty()) {
        _ = stream.render_metadata_by_elem_id.fetchRemove(elem_id) orelse @panic("render metadata disappeared during removal");
    }
}

pub fn renderNodeIndex(comptime StreamType: type, stream: *const StreamType, elem_id: u64) ?usize {
    const metadata = stream.render_metadata_by_elem_id.get(elem_id) orelse return null;
    return metadata.render_node;
}

pub fn recordRenderNodeIndex(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, elem_id: u64, index: usize) void {
    const metadata = ensureRenderMetadata(StreamType, stream, allocator, elem_id);
    if (metadata.render_node != null) @panic("descriptor stream recorded duplicate render index");
    metadata.render_node = index;
}

pub fn updateRenderNodeIndex(comptime StreamType: type, stream: *StreamType, elem_id: u64, index: usize) void {
    const metadata = stream.render_metadata_by_elem_id.getPtr(elem_id) orelse @panic("descriptor stream updated a missing render index");
    if (metadata.render_node == null) @panic("descriptor stream updated a missing render index");
    metadata.render_node = index;
}

pub fn clearRenderNodeIndex(comptime StreamType: type, stream: *StreamType, elem_id: u64, expected: usize) void {
    const metadata = stream.render_metadata_by_elem_id.getPtr(elem_id) orelse @panic("descriptor stream cleared a missing render index");
    const existing = metadata.render_node orelse @panic("descriptor stream cleared a missing render index");
    if (existing != expected) @panic("descriptor stream cleared the wrong render index");
    metadata.render_node = null;
    removeRenderMetadataIfEmpty(StreamType, stream, elem_id);
}

pub fn ensureFirstRenderChildSlot(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, parent_elem_id: u64) *?u64 {
    return &ensureRenderMetadata(StreamType, stream, allocator, parent_elem_id).first_child;
}

pub fn ensureLastRenderChildSlot(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, parent_elem_id: u64) *?u64 {
    return &ensureRenderMetadata(StreamType, stream, allocator, parent_elem_id).last_child;
}

pub fn ensureNextRenderSiblingSlot(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, elem_id: u64) *?u64 {
    return &ensureRenderMetadata(StreamType, stream, allocator, elem_id).next_sibling;
}

pub fn firstRenderChild(comptime StreamType: type, stream: *const StreamType, parent_elem_id: u64) ?u64 {
    const metadata = stream.render_metadata_by_elem_id.get(parent_elem_id) orelse return null;
    return metadata.first_child;
}

pub fn lastRenderChild(comptime StreamType: type, stream: *const StreamType, parent_elem_id: u64) ?u64 {
    const metadata = stream.render_metadata_by_elem_id.get(parent_elem_id) orelse return null;
    return metadata.last_child;
}

pub fn nextRenderSibling(comptime StreamType: type, stream: *const StreamType, elem_id: u64) ?u64 {
    const metadata = stream.render_metadata_by_elem_id.get(elem_id) orelse return null;
    return metadata.next_sibling;
}

pub fn appendRenderChild(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, parent_elem_id: u64, elem_id: u64) void {
    _ = ensureRenderMetadata(StreamType, stream, allocator, parent_elem_id);
    _ = ensureRenderMetadata(StreamType, stream, allocator, elem_id);

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

pub fn clearRenderChildren(comptime StreamType: type, stream: *StreamType, parent_elem_id: u64) void {
    var child = firstRenderChild(StreamType, stream, parent_elem_id);
    while (child) |child_id| {
        const next = nextRenderSibling(StreamType, stream, child_id);
        const child_metadata = stream.render_metadata_by_elem_id.getPtr(child_id) orelse @panic("render child index referenced a child without links");
        child_metadata.next_sibling = null;
        removeRenderMetadataIfEmpty(StreamType, stream, child_id);
        child = next;
    }
    if (stream.render_metadata_by_elem_id.getPtr(parent_elem_id)) |parent_metadata| {
        parent_metadata.first_child = null;
        parent_metadata.last_child = null;
    }
    removeRenderMetadataIfEmpty(StreamType, stream, parent_elem_id);
}

pub fn removeRenderChild(comptime StreamType: type, stream: *StreamType, parent_elem_id: u64, elem_id: u64) void {
    const parent_metadata = stream.render_metadata_by_elem_id.getPtr(parent_elem_id) orelse @panic("render child index was missing its parent list");

    var previous: ?u64 = null;
    var current = parent_metadata.first_child;
    while (current) |child_id| {
        const next = nextRenderSibling(StreamType, stream, child_id);
        if (child_id == elem_id) {
            if (previous) |previous_id| {
                const previous_metadata = stream.render_metadata_by_elem_id.getPtr(previous_id) orelse @panic("render child index referenced a previous child without links");
                previous_metadata.next_sibling = next;
            } else {
                parent_metadata.first_child = next;
            }
            if (lastRenderChild(StreamType, stream, parent_elem_id) == elem_id) {
                parent_metadata.last_child = previous;
            }
            const elem_metadata = stream.render_metadata_by_elem_id.getPtr(elem_id) orelse @panic("render child index removed a child without links");
            elem_metadata.next_sibling = null;
            removeRenderMetadataIfEmpty(StreamType, stream, elem_id);
            removeRenderMetadataIfEmpty(StreamType, stream, parent_elem_id);
            return;
        }
        previous = child_id;
        current = next;
    }
    @panic("render child index was missing a child");
}

pub fn insertRenderChildren(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, parent_elem_id: u64, index: usize, elem_ids: []const u64) void {
    if (elem_ids.len == 0) return;

    _ = ensureRenderMetadata(StreamType, stream, allocator, parent_elem_id);
    for (elem_ids) |elem_id| {
        _ = ensureRenderMetadata(StreamType, stream, allocator, elem_id);
    }

    const parent_metadata = stream.render_metadata_by_elem_id.getPtr(parent_elem_id) orelse @panic("render child insertion was missing parent links");

    var previous: ?u64 = null;
    var next = parent_metadata.first_child;
    var cursor: usize = 0;
    while (cursor < index) : (cursor += 1) {
        const child_id = next orelse @panic("render child insertion index exceeded parent child list");
        previous = child_id;
        next = nextRenderSibling(StreamType, stream, child_id);
    }

    for (elem_ids, 0..) |elem_id, elem_index| {
        const next_insert = if (elem_index + 1 < elem_ids.len) elem_ids[elem_index + 1] else next;
        ensureNextRenderSiblingSlot(StreamType, stream, allocator, elem_id).* = next_insert;
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

pub fn replaceRenderChildrenIndex(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, parent_elem_id: u64, elem_ids: []const u64) void {
    clearRenderChildren(StreamType, stream, parent_elem_id);
    insertRenderChildren(StreamType, stream, allocator, parent_elem_id, 0, elem_ids);
}

pub fn childInsertionIndexForRenderIndex(comptime StreamType: type, stream: *const StreamType, parent_elem_id: u64, render_insert_index: usize) usize {
    var index: usize = 0;
    var child = firstRenderChild(StreamType, stream, parent_elem_id);
    while (child) |child_id| : (index += 1) {
        const child_render_index = renderNodeIndex(StreamType, stream, child_id) orelse @panic("render child index referenced a child without a render index");
        if (child_render_index >= render_insert_index) return index;
        child = nextRenderSibling(StreamType, stream, child_id);
    }
    return index;
}

pub fn refreshRenderIndexesFrom(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, start_index: usize, metrics: anytype) void {
    if (start_index > stream.render_nodes.items.len) @panic("render index refresh started past render node table");
    metrics.bump(.render_indexes_refreshed, @intCast(stream.render_nodes.items.len - start_index));
    for (stream.render_nodes.items[start_index..], start_index..) |node, index| {
        ensureRenderMetadata(StreamType, stream, allocator, node.elem_id).render_node = index;
    }
}

pub fn appendElement(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, elem_id: u64, parent_elem_id: u64, scope_id: u64, tag: []const u8) u64 {
    stream.next_elem_id += 1;

    const tag_copy = allocator.dupe(u8, tag) catch @panic("out of memory");
    const element_index = stream.elements.items.len;
    const render_index = stream.render_nodes.items.len;
    stream.render_nodes.append(allocator, .{ .elem_id = elem_id, .kind = .element }) catch {
        allocator.free(tag_copy);
        @panic("out of memory");
    };
    stream.elements.append(allocator, .{
        .elem_id = elem_id,
        .parent_elem_id = parent_elem_id,
        .scope_id = scope_id,
        .tag = tag_copy,
    }) catch {
        allocator.free(tag_copy);
        @panic("out of memory");
    };
    recordElementIndex(StreamType, stream, allocator, elem_id, element_index);
    recordRenderNodeIndex(StreamType, stream, allocator, elem_id, render_index);
    appendRenderChild(StreamType, stream, allocator, parent_elem_id, elem_id);
    return elem_id;
}

pub fn appendTextNode(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, elem_id: u64, parent_elem_id: u64, scope_id: u64, value: []const u8) void {
    stream.next_elem_id += 1;

    const value_copy = allocator.dupe(u8, value) catch @panic("out of memory");
    const text_node_index = stream.text_nodes.items.len;
    const render_index = stream.render_nodes.items.len;
    stream.render_nodes.append(allocator, .{ .elem_id = elem_id, .kind = .text }) catch {
        allocator.free(value_copy);
        @panic("out of memory");
    };
    stream.text_nodes.append(allocator, .{
        .elem_id = elem_id,
        .parent_elem_id = parent_elem_id,
        .scope_id = scope_id,
        .value = value_copy,
    }) catch {
        allocator.free(value_copy);
        @panic("out of memory");
    };
    recordTextNodeIndex(StreamType, stream, allocator, elem_id, text_node_index);
    recordRenderNodeIndex(StreamType, stream, allocator, elem_id, render_index);
    appendRenderChild(StreamType, stream, allocator, parent_elem_id, elem_id);
}

pub fn appendStaticTextAttr(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, elem_id: u64, field: TextField, value: []const u8) void {
    const value_copy = allocator.dupe(u8, value) catch @panic("out of memory");
    const attr_index = stream.static_text_attrs.items.len;
    stream.static_text_attrs.append(allocator, .{
        .elem_id = elem_id,
        .field = field,
        .value = value_copy,
    }) catch {
        allocator.free(value_copy);
        @panic("out of memory");
    };
    recordStaticTextAttrIndex(StreamType, stream, allocator, elem_id, field, attr_index);
}

pub fn customTextAttrDescriptorExists(comptime StreamType: type, stream: *const StreamType, elem_id: u64, name: []const u8) bool {
    for (stream.static_custom_text_attrs.items) |desc| {
        if (desc.elem_id == elem_id and std.mem.eql(u8, desc.name, name)) return true;
    }
    for (stream.signal_custom_text_attrs.items) |desc| {
        if (desc.elem_id == elem_id and std.mem.eql(u8, desc.name, name)) return true;
    }
    return false;
}

pub fn appendStaticCustomTextAttr(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, elem_id: u64, name: []const u8, value: []const u8) void {
    if (name.len == 0) @panic("custom text attr descriptor used an empty name");
    if (customTextAttrDescriptorExists(StreamType, stream, elem_id, name)) @panic("element has duplicate custom text attr descriptors");

    const name_copy = allocator.dupe(u8, name) catch @panic("out of memory");
    const value_copy = allocator.dupe(u8, value) catch {
        allocator.free(name_copy);
        @panic("out of memory");
    };
    stream.static_custom_text_attrs.append(allocator, .{
        .elem_id = elem_id,
        .name = name_copy,
        .value = value_copy,
    }) catch {
        allocator.free(name_copy);
        allocator.free(value_copy);
        @panic("out of memory");
    };
}

pub fn appendStaticBoolAttr(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, elem_id: u64, field: BoolField, value: bool) void {
    const attr_index = stream.static_bool_attrs.items.len;
    stream.static_bool_attrs.append(allocator, .{
        .elem_id = elem_id,
        .field = field,
        .value = value,
    }) catch @panic("out of memory");
    recordStaticBoolAttrIndex(StreamType, stream, allocator, elem_id, field, attr_index);
}

pub fn appendCleanup(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, scope_id: u64, name: []const u8) void {
    const name_copy = allocator.dupe(u8, name) catch @panic("out of memory");
    stream.cleanups.append(allocator, .{
        .scope_id = scope_id,
        .name = name_copy,
    }) catch {
        allocator.free(name_copy);
        @panic("out of memory");
    };
}

pub fn appendScopeSite(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, node_id: u64, scope_id: u64, ordinal: u64, parent_elem_id: u64, kind: ScopeSiteKind, binder_bindings: []const BinderBinding) void {
    appendScopeSiteAt(StreamType, stream, allocator, node_id, scope_id, ordinal, parent_elem_id, stream.render_nodes.items.len, kind, binder_bindings);
}

pub fn appendScopeSiteAt(comptime StreamType: type, stream: *StreamType, allocator: std.mem.Allocator, node_id: u64, scope_id: u64, ordinal: u64, parent_elem_id: u64, render_insert_index: usize, kind: ScopeSiteKind, binder_bindings: []const BinderBinding) void {
    const binder_copy = allocator.dupe(BinderBinding, binder_bindings) catch @panic("out of memory");
    const scope_site_index = stream.scope_sites.items.len;
    stream.scope_sites.append(allocator, .{
        .node_id = node_id,
        .scope_id = scope_id,
        .ordinal = ordinal,
        .parent_elem_id = parent_elem_id,
        .render_insert_index = render_insert_index,
        .kind = kind,
        .binder_bindings = binder_copy,
    }) catch {
        allocator.free(binder_copy);
        @panic("out of memory");
    };
    recordScopeSiteIndex(StreamType, stream, allocator, node_id, kind, scope_site_index);
}

pub fn findElementDesc(comptime StreamType: type, stream: *const StreamType, elem_id: u64) ?StreamType.ElementDesc {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return null;
    const index = descriptor_index.element orelse return null;
    if (index >= stream.elements.items.len) @panic("element descriptor index exceeded descriptor table");
    const desc = stream.elements.items[index];
    if (desc.elem_id != elem_id) @panic("element descriptor index pointed at the wrong elem id");
    return desc;
}

pub fn findTextNodeDesc(comptime StreamType: type, stream: *const StreamType, elem_id: u64) ?StreamType.TextNodeDesc {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return null;
    const index = descriptor_index.text_node orelse return null;
    if (index >= stream.text_nodes.items.len) @panic("text node descriptor index exceeded descriptor table");
    const desc = stream.text_nodes.items[index];
    if (desc.elem_id != elem_id) @panic("text node descriptor index pointed at the wrong elem id");
    return desc;
}

pub fn findSignalTextNodeDesc(comptime StreamType: type, stream: *const StreamType, elem_id: u64) ?StreamType.SignalTextNodeDesc {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return null;
    const index = descriptor_index.signal_text_node orelse return null;
    if (index >= stream.signal_text_nodes.items.len) @panic("signal text node descriptor index exceeded descriptor table");
    const desc = stream.signal_text_nodes.items[index];
    if (desc.elem_id != elem_id) @panic("signal text node descriptor index pointed at the wrong elem id");
    return desc;
}

pub fn findSignalTextNodeDescMutable(comptime StreamType: type, stream: *StreamType, elem_id: u64) ?*StreamType.SignalTextNodeDesc {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return null;
    const index = descriptor_index.signal_text_node orelse return null;
    if (index >= stream.signal_text_nodes.items.len) @panic("signal text node descriptor index exceeded descriptor table");
    const desc = &stream.signal_text_nodes.items[index];
    if (desc.elem_id != elem_id) @panic("signal text node descriptor index pointed at the wrong elem id");
    return desc;
}

pub fn streamHasTextField(comptime StreamType: type, stream: *const StreamType, elem_id: u64, field: TextField) bool {
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

pub fn streamHasCustomTextAttr(comptime StreamType: type, stream: *const StreamType, elem_id: u64, name: []const u8) bool {
    for (stream.static_custom_text_attrs.items) |desc| {
        if (desc.elem_id == elem_id and std.mem.eql(u8, desc.name, name)) return true;
    }
    for (stream.signal_custom_text_attrs.items) |desc| {
        if (desc.elem_id == elem_id and std.mem.eql(u8, desc.name, name)) return true;
    }
    return false;
}

pub fn streamHasBoolField(comptime StreamType: type, stream: *const StreamType, elem_id: u64, field: BoolField) bool {
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

pub fn maxRenderElemId(comptime StreamType: type, stream: *const StreamType) u64 {
    var max_elem_id: u64 = 0;
    for (stream.render_nodes.items) |node| {
        max_elem_id = @max(max_elem_id, node.elem_id);
    }
    return max_elem_id;
}

pub fn renderNodeTag(comptime StreamType: type, stream: *const StreamType, node: StreamType.RenderNode) []const u8 {
    return switch (node.kind) {
        .element => (findElementDesc(StreamType, stream, node.elem_id) orelse @panic("renderNodeTag: render node has no matching descriptor")).tag,
        .text, .signal_text => "text",
    };
}

pub fn streamElemTag(comptime StreamType: type, stream: *const StreamType, elem_id: u64) []const u8 {
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

pub fn renderNodeParentElemId(comptime StreamType: type, stream: *const StreamType, node: StreamType.RenderNode) u64 {
    return switch (node.kind) {
        .element => (findElementDesc(StreamType, stream, node.elem_id) orelse @panic("renderNodeParentElemId: render node has no matching descriptor")).parent_elem_id,
        .text => (findTextNodeDesc(StreamType, stream, node.elem_id) orelse @panic("renderNodeParentElemId: render node has no matching descriptor")).parent_elem_id,
        .signal_text => (findSignalTextNodeDesc(StreamType, stream, node.elem_id) orelse @panic("renderNodeParentElemId: render node has no matching descriptor")).parent_elem_id,
    };
}

pub fn streamElemParentElemId(comptime StreamType: type, stream: *const StreamType, elem_id: u64) u64 {
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

pub fn streamDirectChildren(comptime StreamType: type, allocator: std.mem.Allocator, stream: *const StreamType, parent_elem_id: u64) []u64 {
    var children: std.ArrayListUnmanaged(u64) = .empty;
    errdefer children.deinit(allocator);

    var child = stream.firstRenderChild(parent_elem_id);
    while (child) |child_id| {
        children.append(allocator, child_id) catch @panic("out of memory");
        child = stream.nextRenderSibling(child_id);
    }

    return children.toOwnedSlice(allocator) catch @panic("out of memory");
}

pub fn renderNodeScopeId(comptime StreamType: type, stream: *const StreamType, node: StreamType.RenderNode) u64 {
    return switch (node.kind) {
        .element => (findElementDesc(StreamType, stream, node.elem_id) orelse @panic("renderNodeScopeId: render node has no matching descriptor")).scope_id,
        .text => (findTextNodeDesc(StreamType, stream, node.elem_id) orelse @panic("renderNodeScopeId: render node has no matching descriptor")).scope_id,
        .signal_text => (findSignalTextNodeDesc(StreamType, stream, node.elem_id) orelse @panic("renderNodeScopeId: render node has no matching descriptor")).scope_id,
    };
}

pub fn elemScopeId(comptime StreamType: type, stream: *const StreamType, elem_id: u64) ?u64 {
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
