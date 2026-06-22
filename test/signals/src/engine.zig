//! Shared reactive-engine internals for the Signals hosts.
//!
//! This module is the home for the host-agnostic reactive engine that
//! `native_host.zig` and `wasm_host.zig` will both drive (see the G-B0 plan).
//! It starts with the retained "HostValue / thunk / scope" adapter: the value
//! cell that owns a boxed Roc value plus its equality/drop thunks, and the
//! each-row scope step that carries those cells through the scope forest.
//!
//! Host-specific concerns stay out: methods take the active `*abi.RocHost`
//! explicitly, the metrics sink is a duck-typed `anytype` (so a host can pass a
//! real `RuntimeMetrics` or a zero-size `NoMetrics`), and cloning a HostValue is
//! delegated to a `ctx` the host supplies (`ctx.cloneHostValue`).

const std = @import("std");
const abi = @import("roc_platform_abi.zig");
const scope_tree = @import("scope_tree.zig");
const erased_calls = @import("erased_calls.zig");
const render = @import("render_commands.zig");

pub const RenderTextField = render.TextField;
pub const RenderBoolField = render.BoolField;
pub const RenderEventKind = render.EventKind;

pub const HostValue = u64;

/// A retained Roc value plus the equality and drop thunks that own it. Holds
/// exactly one refcount on each thunk while live.
pub const HostValueCell = struct {
    value: HostValue,
    eq: abi.RocErasedCallable,
    drop: abi.RocErasedCallable,

    pub fn initRetained(value: HostValue, eq: abi.RocErasedCallable, drop: abi.RocErasedCallable, metrics: anytype) HostValueCell {
        abi.increfErasedCallable(eq, 1);
        abi.increfErasedCallable(drop, 1);
        metrics.bump(.closure_retains, 2);
        return .{ .value = value, .eq = eq, .drop = drop };
    }

    /// Clone the cell, retaining the thunks and cloning the boxed value through
    /// `ctx.cloneHostValue` (which the host implements with its registry).
    pub fn cloneRetained(self: HostValueCell, ctx: anytype, metrics: anytype) HostValueCell {
        const value = ctx.cloneHostValue(self.value);
        abi.increfErasedCallable(self.eq, 1);
        abi.increfErasedCallable(self.drop, 1);
        metrics.bump(.closure_retains, 2);
        return .{ .value = value, .eq = self.eq, .drop = self.drop };
    }

    pub fn deinit(self: *HostValueCell, roc_host: *abi.RocHost, metrics: anytype) void {
        erased_calls.callErasedHostValueToUnit(roc_host, self.drop, self.value);
        abi.decrefErasedCallable(self.eq, roc_host);
        abi.decrefErasedCallable(self.drop, roc_host);
        metrics.bump(.closure_releases, 2);
        self.* = undefined;
    }

    pub fn valueEquals(self: *const HostValueCell, roc_host: *abi.RocHost, value: HostValue) bool {
        return erased_calls.callErasedHostValueHostValueToBool(roc_host, self.eq, self.value, value);
    }

    pub fn dropIncoming(self: *const HostValueCell, roc_host: *abi.RocHost, value: HostValue) void {
        erased_calls.callErasedHostValueToUnit(roc_host, self.drop, value);
    }

    pub fn replaceValue(self: *HostValueCell, roc_host: *abi.RocHost, value: HostValue) void {
        erased_calls.callErasedHostValueToUnit(roc_host, self.drop, self.value);
        self.value = value;
    }
};

/// Per-row payload carried in an `Ui.each` scope: the row's key and item cells,
/// keyed by the construction-site ordinal.
pub const HostEachRowScopeStep = struct {
    site_ordinal: u64,
    key: HostValueCell,
    item: HostValueCell,
};

pub const HostScopeStep = scope_tree.Step(HostEachRowScopeStep);
pub const HostScope = scope_tree.Scope(HostEachRowScopeStep);

/// Drop the retained cells owned by an each-row scope step (no-op for the
/// structural scope kinds, which carry no Roc values).
pub fn deinitHostScopeStep(step: *HostScopeStep, roc_host: *abi.RocHost, metrics: anytype) void {
    switch (step.*) {
        .each_row => |*row| {
            row.key.deinit(roc_host, metrics);
            row.item.deinit(roc_host, metrics);
        },
        .root, .component, .when_branch => {},
    }
}

/// Retain one refcount on a Roc thunk the host is about to store.
pub fn retainHostCallable(callable: abi.RocErasedCallable, metrics: anytype) abi.RocErasedCallable {
    abi.increfErasedCallable(callable, 1);
    metrics.bump(.closure_retains, 1);
    return callable;
}

/// Opaque carrier type tag attached to a HostValue. The native host enables
/// debug type-tag assertions; the browser host disables them.
pub const HostValueTypeTag = *anyopaque;

/// Identity token interned per signal expression node.
pub const HostSignalToken = *u64;

pub fn retainHostSignalToken(token: HostSignalToken) HostSignalToken {
    abi.increfBox(@ptrCast(token), 1);
    return token;
}

/// Memoized output of a signal record: absent until first evaluated, then a
/// retained cell holding the last computed value plus its eq/drop thunks.
pub const HostSignalCacheSlot = union(enum) {
    absent,
    present: HostValueCell,

    pub fn deinit(self: *HostSignalCacheSlot, roc_host: *abi.RocHost, metrics: anytype) void {
        switch (self.*) {
            .absent => {},
            .present => |*cached| cached.deinit(roc_host, metrics),
        }
        self.* = .absent;
    }

    pub fn replace(self: *HostSignalCacheSlot, roc_host: *abi.RocHost, metrics: anytype, value: HostValue, eq: abi.RocErasedCallable, drop: abi.RocErasedCallable) void {
        self.deinit(roc_host, metrics);
        self.* = .{ .present = HostValueCell.initRetained(value, eq, drop, metrics) };
    }

    pub fn replaceValue(self: *HostSignalCacheSlot, roc_host: *abi.RocHost, value: HostValue) void {
        switch (self.*) {
            .absent => @panic("dirty signal expression was evaluated before its initial value was cached"),
            .present => |*cached| cached.replaceValue(roc_host, value),
        }
    }

    pub fn cloneRetained(self: HostSignalCacheSlot, ctx: anytype, metrics: anytype) HostSignalCacheSlot {
        return switch (self) {
            .absent => .absent,
            .present => |cached| .{ .present = cached.cloneRetained(ctx, metrics) },
        };
    }
};

pub const HostSignalConstRecord = struct {
    token: HostSignalToken,
    init: abi.RocErasedCallable,
    eq: abi.RocErasedCallable,
    drop: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostSignalMapRecord = struct {
    token: HostSignalToken,
    input: *HostSignalRecord,
    transform: abi.RocErasedCallable,
    eq: abi.RocErasedCallable,
    drop: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostSignalMap2Record = struct {
    token: HostSignalToken,
    left: *HostSignalRecord,
    right: *HostSignalRecord,
    transform: abi.RocErasedCallable,
    eq: abi.RocErasedCallable,
    drop: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostSignalCombineRecord = struct {
    token: HostSignalToken,
    children: []*HostSignalRecord,
    transform: abi.RocErasedCallable,
    eq: abi.RocErasedCallable,
    drop: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostSignalTaskSourceRecord = struct {
    token: HostSignalToken,
    name: []const u8,
    payload_tag: HostValueTypeTag,
    payload_drop: abi.RocErasedCallable,
    initial: abi.RocErasedCallable,
    done: abi.RocErasedCallable,
    failed: abi.RocErasedCallable,
    eq: abi.RocErasedCallable,
    drop: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostSignalIntervalSourceRecord = struct {
    token: HostSignalToken,
    period_ms: u64,
    initial: abi.RocErasedCallable,
    tick: abi.RocErasedCallable,
    eq: abi.RocErasedCallable,
    drop: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostSignalRecordPayload = union(enum) {
    ref: u64,
    const_value: HostSignalConstRecord,
    map: HostSignalMapRecord,
    map2: HostSignalMap2Record,
    combine: HostSignalCombineRecord,
    task_source: HostSignalTaskSourceRecord,
    interval_source: HostSignalIntervalSourceRecord,
};

/// A refcounted, shareable node in the signal graph. Owns its transform/eq/drop
/// thunks plus a memoized cached value; the active graph holds one reference
/// while a record is mounted.
pub const HostSignalRecord = struct {
    ref_count: usize,
    payload: HostSignalRecordPayload,
    active_graph_id: ?u64 = null,
    active_use_count: usize = 0,
    last_dirty_generation: u64 = 0,
    last_dirty_changed: bool = false,

    pub fn init(allocator: std.mem.Allocator, payload: HostSignalRecordPayload) *HostSignalRecord {
        const record = allocator.create(HostSignalRecord) catch @panic("out of memory");
        record.* = .{
            .ref_count = 1,
            .payload = payload,
        };
        return record;
    }

    pub fn token(self: *const HostSignalRecord) ?HostSignalToken {
        return switch (self.payload) {
            .ref => null,
            .const_value => |payload| payload.token,
            .map => |payload| payload.token,
            .map2 => |payload| payload.token,
            .combine => |payload| payload.token,
            .task_source => |payload| payload.token,
            .interval_source => |payload| payload.token,
        };
    }

    pub fn retain(self: *HostSignalRecord) *HostSignalRecord {
        self.ref_count += 1;
        return self;
    }

    pub fn release(self: *HostSignalRecord, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype) void {
        if (self.ref_count == 0) @panic("host signal record release underflow");
        if (self.ref_count == 1 and self.active_graph_id != null) @panic("active signal graph held the last signal record reference");
        self.ref_count -= 1;
        if (self.ref_count != 0) return;

        switch (self.payload) {
            .ref => {},
            .const_value => |payload| {
                var cached_value = payload.cached_value;
                cached_value.deinit(roc_host, metrics);
                abi.decrefBox(@ptrCast(payload.token), roc_host);
                abi.decrefErasedCallable(payload.init, roc_host);
                abi.decrefErasedCallable(payload.eq, roc_host);
                abi.decrefErasedCallable(payload.drop, roc_host);
                metrics.bump(.closure_releases, 3);
            },
            .map => |payload| {
                payload.input.release(allocator, roc_host, metrics);
                var cached_value = payload.cached_value;
                cached_value.deinit(roc_host, metrics);
                abi.decrefBox(@ptrCast(payload.token), roc_host);
                abi.decrefErasedCallable(payload.transform, roc_host);
                abi.decrefErasedCallable(payload.eq, roc_host);
                abi.decrefErasedCallable(payload.drop, roc_host);
                metrics.bump(.closure_releases, 3);
            },
            .map2 => |payload| {
                payload.left.release(allocator, roc_host, metrics);
                payload.right.release(allocator, roc_host, metrics);
                var cached_value = payload.cached_value;
                cached_value.deinit(roc_host, metrics);
                abi.decrefBox(@ptrCast(payload.token), roc_host);
                abi.decrefErasedCallable(payload.transform, roc_host);
                abi.decrefErasedCallable(payload.eq, roc_host);
                abi.decrefErasedCallable(payload.drop, roc_host);
                metrics.bump(.closure_releases, 3);
            },
            .combine => |payload| {
                for (payload.children) |child| {
                    child.release(allocator, roc_host, metrics);
                }
                allocator.free(payload.children);
                var cached_value = payload.cached_value;
                cached_value.deinit(roc_host, metrics);
                abi.decrefBox(@ptrCast(payload.token), roc_host);
                abi.decrefErasedCallable(payload.transform, roc_host);
                abi.decrefErasedCallable(payload.eq, roc_host);
                abi.decrefErasedCallable(payload.drop, roc_host);
                metrics.bump(.closure_releases, 3);
            },
            .task_source => |payload| {
                var cached_value = payload.cached_value;
                cached_value.deinit(roc_host, metrics);
                abi.decrefBox(@ptrCast(payload.token), roc_host);
                allocator.free(payload.name);
                abi.decrefBox(@ptrCast(payload.payload_tag), roc_host);
                abi.decrefErasedCallable(payload.payload_drop, roc_host);
                abi.decrefErasedCallable(payload.initial, roc_host);
                abi.decrefErasedCallable(payload.done, roc_host);
                abi.decrefErasedCallable(payload.failed, roc_host);
                abi.decrefErasedCallable(payload.eq, roc_host);
                abi.decrefErasedCallable(payload.drop, roc_host);
                metrics.bump(.closure_releases, 6);
            },
            .interval_source => |payload| {
                var cached_value = payload.cached_value;
                cached_value.deinit(roc_host, metrics);
                abi.decrefBox(@ptrCast(payload.token), roc_host);
                abi.decrefErasedCallable(payload.initial, roc_host);
                abi.decrefErasedCallable(payload.tick, roc_host);
                abi.decrefErasedCallable(payload.eq, roc_host);
                abi.decrefErasedCallable(payload.drop, roc_host);
                metrics.bump(.closure_releases, 4);
            },
        }

        allocator.destroy(self);
    }
};

/// A reference to a shared signal record plus the source state-node ids that
/// feed it; the unit a descriptor edge owns.
pub const HostSignalBinding = struct {
    record: *HostSignalRecord,
    source_node_ids: []u64,

    pub fn cloneRetained(self: HostSignalBinding, allocator: std.mem.Allocator, metrics: anytype) HostSignalBinding {
        _ = metrics;
        return .{
            .record = self.record.retain(),
            .source_node_ids = allocator.dupe(u64, self.source_node_ids) catch @panic("out of memory"),
        };
    }

    pub fn deinit(self: *HostSignalBinding, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype) void {
        self.record.release(allocator, roc_host, metrics);
        allocator.free(self.source_node_ids);
    }
};

// --- Descriptor layer ---------------------------------------------------------
//
// The host's ingested view of the Roc `Elem` tree: one descriptor per element,
// text node, attribute, event binding, scope site, state, when, and each.
// Markup carries no identity of its own; dynamic descriptors reference shared
// signal records and event reducers by retained thunk.

/// Typed payload a reducer reads off a real DOM event.
pub const EventPayloadKind = enum(u64) {
    unit = 1,
    str = 2,
    bool = 3,
};

/// Identity token interned per `Ui.state` binder.
pub const HostBinderToken = *u64;

/// Binds a state binder token to the node id it resolves to within a scope.
pub const HostBinderBinding = struct {
    token: HostBinderToken,
    node_id: u64,
};

pub const HostNodeScopeSiteKind = enum {
    component,
    state,
    when,
    each,
};

pub const HostRenderNodeKind = enum {
    element,
    text,
    signal_text,
};

/// A node that occupies a slot in the rendered child order.
pub const HostRenderNode = struct {
    elem_id: u64,
    kind: HostRenderNodeKind,
};

pub const HostNodeScopeSiteDesc = struct {
    node_id: u64,
    scope_id: u64,
    ordinal: u64,
    parent_elem_id: u64,
    render_insert_index: usize,
    kind: HostNodeScopeSiteKind,
    binder_bindings: []HostBinderBinding,
};

pub const HostElementDesc = struct {
    elem_id: u64,
    parent_elem_id: u64,
    scope_id: u64,
    tag: []const u8,
};

pub const HostNodeTextNodeDesc = struct {
    elem_id: u64,
    parent_elem_id: u64,
    scope_id: u64,
    value: []const u8,
};

pub const HostNodeSignalTextNodeDesc = struct {
    elem_id: u64,
    parent_elem_id: u64,
    scope_id: u64,
    signal: HostSignalBinding,
    read: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostNodeStaticTextAttrDesc = struct {
    elem_id: u64,
    field: RenderTextField,
    value: []const u8,
};

pub const HostNodeSignalTextAttrDesc = struct {
    elem_id: u64,
    field: RenderTextField,
    signal: HostSignalBinding,
    read: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostNodeStaticBoolAttrDesc = struct {
    elem_id: u64,
    field: RenderBoolField,
    value: bool,
};

pub const HostNodeSignalBoolAttrDesc = struct {
    elem_id: u64,
    field: RenderBoolField,
    signal: HostSignalBinding,
    read: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostNodeOnChangeDesc = struct {
    scope_id: u64,
    signal: HostSignalBinding,
    to_cmd: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostNodeCleanupDesc = struct {
    scope_id: u64,
    name: []const u8,
};

pub const HostNodeEventDesc = struct {
    elem_id: u64,
    kind: RenderEventKind,
    binder_token: HostBinderToken,
    target_node_id: u64,
    payload_kind: EventPayloadKind,
    payload_tag: HostValueTypeTag,
    payload_drop: abi.RocErasedCallable,
    transform: abi.RocErasedCallable,
    owns_payload_tag: bool = true,
    owns_payload_drop: bool = true,
    owns_transform: bool = true,
};

pub const HostNodeStateDesc = struct {
    node_id: u64,
    initial: abi.RocErasedCallable,
    eq: abi.RocErasedCallable,
    drop: abi.RocErasedCallable,
};

pub const HostNodeWhenDesc = struct {
    node_id: u64,
    condition: HostSignalBinding,
    read: abi.RocErasedCallable,
    when_false: abi.Elem,
    when_true: abi.Elem,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostNodeEachDesc = struct {
    node_id: u64,
    items: HostSignalBinding,
    items_to_values: abi.RocErasedCallable,
    key_hash: abi.RocErasedCallable,
    key_of: abi.RocErasedCallable,
    key_eq: abi.RocErasedCallable,
    key_drop: abi.RocErasedCallable,
    item_eq: abi.RocErasedCallable,
    item_drop: abi.RocErasedCallable,
    row: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostSignalRecordTokenEntry = struct {
    token: HostSignalToken,
    record: *HostSignalRecord,
};

// --- Descriptor stream -------------------------------------------------------
//
// The retained stream of node descriptors plus the per-elem index that makes
// descriptor lookup O(1). Its append* methods ingest the Roc Elem tree; the
// host drives ingestion and consumes the stream to render.

pub const HostTextFieldDescriptorIndexes = struct {
    text: ?usize = null,
    role: ?usize = null,
    label: ?usize = null,
    test_id: ?usize = null,
    value: ?usize = null,

    pub fn get(self: HostTextFieldDescriptorIndexes, field: RenderTextField) ?usize {
        return switch (field) {
            .text => self.text,
            .role => self.role,
            .label => self.label,
            .test_id => self.test_id,
            .value => self.value,
        };
    }

    pub fn slot(self: *HostTextFieldDescriptorIndexes, field: RenderTextField) *?usize {
        return switch (field) {
            .text => &self.text,
            .role => &self.role,
            .label => &self.label,
            .test_id => &self.test_id,
            .value => &self.value,
        };
    }
};

pub const HostBoolFieldDescriptorIndexes = struct {
    checked: ?usize = null,
    disabled: ?usize = null,

    pub fn get(self: HostBoolFieldDescriptorIndexes, field: RenderBoolField) ?usize {
        return switch (field) {
            .checked => self.checked,
            .disabled => self.disabled,
        };
    }

    pub fn slot(self: *HostBoolFieldDescriptorIndexes, field: RenderBoolField) *?usize {
        return switch (field) {
            .checked => &self.checked,
            .disabled => &self.disabled,
        };
    }
};

pub const HostEventDescriptorIndexes = struct {
    click: ?usize = null,
    input: ?usize = null,
    check: ?usize = null,

    pub fn get(self: HostEventDescriptorIndexes, kind: RenderEventKind) ?usize {
        return switch (kind) {
            .click => self.click,
            .input => self.input,
            .check => self.check,
        };
    }

    pub fn slot(self: *HostEventDescriptorIndexes, kind: RenderEventKind) *?usize {
        return switch (kind) {
            .click => &self.click,
            .input => &self.input,
            .check => &self.check,
        };
    }
};

pub const HostElemDescriptorIndex = struct {
    element: ?usize = null,
    text_node: ?usize = null,
    signal_text_node: ?usize = null,
    static_text_attrs: HostTextFieldDescriptorIndexes = .{},
    signal_text_attrs: HostTextFieldDescriptorIndexes = .{},
    static_bool_attrs: HostBoolFieldDescriptorIndexes = .{},
    signal_bool_attrs: HostBoolFieldDescriptorIndexes = .{},
    events: HostEventDescriptorIndexes = .{},
};

pub const HostNodeDescriptorStream = struct {
    render_nodes: std.ArrayListUnmanaged(HostRenderNode) = .empty,
    elements: std.ArrayListUnmanaged(HostElementDesc) = .empty,
    text_nodes: std.ArrayListUnmanaged(HostNodeTextNodeDesc) = .empty,
    signal_text_nodes: std.ArrayListUnmanaged(HostNodeSignalTextNodeDesc) = .empty,
    static_text_attrs: std.ArrayListUnmanaged(HostNodeStaticTextAttrDesc) = .empty,
    signal_text_attrs: std.ArrayListUnmanaged(HostNodeSignalTextAttrDesc) = .empty,
    static_bool_attrs: std.ArrayListUnmanaged(HostNodeStaticBoolAttrDesc) = .empty,
    signal_bool_attrs: std.ArrayListUnmanaged(HostNodeSignalBoolAttrDesc) = .empty,
    on_changes: std.ArrayListUnmanaged(HostNodeOnChangeDesc) = .empty,
    cleanups: std.ArrayListUnmanaged(HostNodeCleanupDesc) = .empty,
    events: std.ArrayListUnmanaged(HostNodeEventDesc) = .empty,
    scope_sites: std.ArrayListUnmanaged(HostNodeScopeSiteDesc) = .empty,
    states: std.ArrayListUnmanaged(HostNodeStateDesc) = .empty,
    whens: std.ArrayListUnmanaged(HostNodeWhenDesc) = .empty,
    eaches: std.ArrayListUnmanaged(HostNodeEachDesc) = .empty,
    signal_records_by_token: std.ArrayListUnmanaged(HostSignalRecordTokenEntry) = .empty,
    descriptor_indexes_by_elem_id: std.ArrayListUnmanaged(HostElemDescriptorIndex) = .empty,
    next_elem_id: u64 = 1,

    pub fn ensureElemDescriptorIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64) *HostElemDescriptorIndex {
        const index: usize = @intCast(elem_id);
        while (self.descriptor_indexes_by_elem_id.items.len <= index) {
            self.descriptor_indexes_by_elem_id.append(allocator, .{}) catch @panic("out of memory");
        }
        return &self.descriptor_indexes_by_elem_id.items[index];
    }

    pub fn elemDescriptorIndex(self: *const HostNodeDescriptorStream, elem_id: u64) ?HostElemDescriptorIndex {
        if (elem_id >= self.descriptor_indexes_by_elem_id.items.len) return null;
        return self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)];
    }

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

    pub fn recordElementIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(&self.ensureElemDescriptorIndex(allocator, elem_id).element, index);
    }

    pub fn updateElementIndex(self: *HostNodeDescriptorStream, elem_id: u64, index: usize) void {
        HostNodeDescriptorStream.updateIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].element, index);
    }

    pub fn clearElementIndex(self: *HostNodeDescriptorStream, elem_id: u64, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].element, expected);
    }

    pub fn recordTextNodeIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(&self.ensureElemDescriptorIndex(allocator, elem_id).text_node, index);
    }

    pub fn updateTextNodeIndex(self: *HostNodeDescriptorStream, elem_id: u64, index: usize) void {
        HostNodeDescriptorStream.updateIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].text_node, index);
    }

    pub fn clearTextNodeIndex(self: *HostNodeDescriptorStream, elem_id: u64, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].text_node, expected);
    }

    pub fn recordSignalTextNodeIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(&self.ensureElemDescriptorIndex(allocator, elem_id).signal_text_node, index);
    }

    pub fn updateSignalTextNodeIndex(self: *HostNodeDescriptorStream, elem_id: u64, index: usize) void {
        HostNodeDescriptorStream.updateIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_text_node, index);
    }

    pub fn clearSignalTextNodeIndex(self: *HostNodeDescriptorStream, elem_id: u64, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_text_node, expected);
    }

    pub fn recordStaticTextAttrIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderTextField, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(self.ensureElemDescriptorIndex(allocator, elem_id).static_text_attrs.slot(field), index);
    }

    pub fn updateStaticTextAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderTextField, index: usize) void {
        HostNodeDescriptorStream.updateIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].static_text_attrs.slot(field), index);
    }

    pub fn clearStaticTextAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderTextField, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].static_text_attrs.slot(field), expected);
    }

    pub fn recordSignalTextAttrIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderTextField, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(self.ensureElemDescriptorIndex(allocator, elem_id).signal_text_attrs.slot(field), index);
    }

    pub fn updateSignalTextAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderTextField, index: usize) void {
        HostNodeDescriptorStream.updateIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_text_attrs.slot(field), index);
    }

    pub fn clearSignalTextAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderTextField, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_text_attrs.slot(field), expected);
    }

    pub fn recordStaticBoolAttrIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderBoolField, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(self.ensureElemDescriptorIndex(allocator, elem_id).static_bool_attrs.slot(field), index);
    }

    pub fn updateStaticBoolAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderBoolField, index: usize) void {
        HostNodeDescriptorStream.updateIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].static_bool_attrs.slot(field), index);
    }

    pub fn clearStaticBoolAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderBoolField, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].static_bool_attrs.slot(field), expected);
    }

    pub fn recordSignalBoolAttrIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderBoolField, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(self.ensureElemDescriptorIndex(allocator, elem_id).signal_bool_attrs.slot(field), index);
    }

    pub fn updateSignalBoolAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderBoolField, index: usize) void {
        HostNodeDescriptorStream.updateIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_bool_attrs.slot(field), index);
    }

    pub fn clearSignalBoolAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderBoolField, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_bool_attrs.slot(field), expected);
    }

    pub fn recordEventIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, kind: RenderEventKind, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(self.ensureElemDescriptorIndex(allocator, elem_id).events.slot(kind), index);
    }

    pub fn updateEventIndex(self: *HostNodeDescriptorStream, elem_id: u64, kind: RenderEventKind, index: usize) void {
        HostNodeDescriptorStream.updateIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].events.slot(kind), index);
    }

    pub fn clearEventIndex(self: *HostNodeDescriptorStream, elem_id: u64, kind: RenderEventKind, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].events.slot(kind), expected);
    }

    pub fn deinit(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype) void {
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
            desc.cached_value.deinit(roc_host, metrics);
            desc.signal.deinit(allocator, roc_host, metrics);
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(desc.read, roc_host);
        }
        self.signal_text_nodes.deinit(allocator);

        for (self.static_text_attrs.items) |desc| {
            allocator.free(desc.value);
        }
        self.static_text_attrs.deinit(allocator);

        for (self.signal_text_attrs.items) |*desc| {
            desc.cached_value.deinit(roc_host, metrics);
            desc.signal.deinit(allocator, roc_host, metrics);
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(desc.read, roc_host);
        }
        self.signal_text_attrs.deinit(allocator);

        self.static_bool_attrs.deinit(allocator);

        for (self.signal_bool_attrs.items) |*desc| {
            desc.cached_value.deinit(roc_host, metrics);
            desc.signal.deinit(allocator, roc_host, metrics);
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(desc.read, roc_host);
        }
        self.signal_bool_attrs.deinit(allocator);

        for (self.on_changes.items) |*desc| {
            desc.cached_value.deinit(roc_host, metrics);
            desc.signal.deinit(allocator, roc_host, metrics);
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(desc.to_cmd, roc_host);
        }
        self.on_changes.deinit(allocator);

        for (self.cleanups.items) |desc| {
            allocator.free(desc.name);
        }
        self.cleanups.deinit(allocator);

        for (self.events.items) |desc| {
            if (desc.owns_payload_tag) {
                abi.decrefBox(@ptrCast(desc.payload_tag), roc_host);
            }
            if (desc.owns_payload_drop) {
                metrics.bump(.closure_releases, 1);
                abi.decrefErasedCallable(desc.payload_drop, roc_host);
            }
            if (desc.owns_transform) {
                metrics.bump(.closure_releases, 1);
                abi.decrefErasedCallable(desc.transform, roc_host);
            }
        }
        self.events.deinit(allocator);

        for (self.scope_sites.items) |desc| {
            allocator.free(desc.binder_bindings);
        }
        self.scope_sites.deinit(allocator);

        for (self.states.items) |desc| {
            metrics.bump(.closure_releases, 3);
            abi.decrefErasedCallable(desc.initial, roc_host);
            abi.decrefErasedCallable(desc.eq, roc_host);
            abi.decrefErasedCallable(desc.drop, roc_host);
        }
        self.states.deinit(allocator);

        for (self.whens.items) |*desc| {
            desc.cached_value.deinit(roc_host, metrics);
            desc.condition.deinit(allocator, roc_host, metrics);
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(desc.read, roc_host);
            abi.decrefElem(desc.when_false, roc_host);
            abi.decrefElem(desc.when_true, roc_host);
        }
        self.whens.deinit(allocator);

        for (self.eaches.items) |*desc| {
            desc.cached_value.deinit(roc_host, metrics);
            desc.items.deinit(allocator, roc_host, metrics);
            metrics.bump(.closure_releases, 8);
            abi.decrefErasedCallable(desc.items_to_values, roc_host);
            abi.decrefErasedCallable(desc.key_hash, roc_host);
            abi.decrefErasedCallable(desc.key_of, roc_host);
            abi.decrefErasedCallable(desc.key_eq, roc_host);
            abi.decrefErasedCallable(desc.key_drop, roc_host);
            abi.decrefErasedCallable(desc.item_eq, roc_host);
            abi.decrefErasedCallable(desc.item_drop, roc_host);
            abi.decrefErasedCallable(desc.row, roc_host);
        }
        self.eaches.deinit(allocator);

        self.signal_records_by_token.deinit(allocator);
        self.descriptor_indexes_by_elem_id.deinit(allocator);

        self.* = .{};
    }

    pub fn signalRecordByToken(self: *HostNodeDescriptorStream, token: HostSignalToken) ?*HostSignalRecord {
        for (self.signal_records_by_token.items) |entry| {
            if (entry.token == token) return entry.record;
        }
        return null;
    }

    pub fn rememberSignalRecord(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, record: *HostSignalRecord) void {
        const token = record.token() orelse return;
        if (self.signalRecordByToken(token)) |existing| {
            if (existing != record) @panic("signal token was bound to multiple host records");
            return;
        }
        self.signal_records_by_token.append(allocator, .{
            .token = token,
            .record = record,
        }) catch @panic("out of memory");
    }

    pub fn rememberSignalRecordTree(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, record: *HostSignalRecord) void {
        self.rememberSignalRecord(allocator, record);
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

    pub fn appendElement(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, parent_elem_id: u64, scope_id: u64, tag: []const u8) u64 {
        self.next_elem_id += 1;

        const tag_copy = allocator.dupe(u8, tag) catch @panic("out of memory");
        const element_index = self.elements.items.len;
        self.render_nodes.append(allocator, .{ .elem_id = elem_id, .kind = .element }) catch {
            allocator.free(tag_copy);
            @panic("out of memory");
        };
        self.elements.append(allocator, .{
            .elem_id = elem_id,
            .parent_elem_id = parent_elem_id,
            .scope_id = scope_id,
            .tag = tag_copy,
        }) catch {
            allocator.free(tag_copy);
            @panic("out of memory");
        };
        self.recordElementIndex(allocator, elem_id, element_index);
        return elem_id;
    }

    pub fn appendTextNode(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, parent_elem_id: u64, scope_id: u64, value: []const u8) void {
        self.next_elem_id += 1;

        const value_copy = allocator.dupe(u8, value) catch @panic("out of memory");
        const text_node_index = self.text_nodes.items.len;
        self.render_nodes.append(allocator, .{ .elem_id = elem_id, .kind = .text }) catch {
            allocator.free(value_copy);
            @panic("out of memory");
        };
        self.text_nodes.append(allocator, .{
            .elem_id = elem_id,
            .parent_elem_id = parent_elem_id,
            .scope_id = scope_id,
            .value = value_copy,
        }) catch {
            allocator.free(value_copy);
            @panic("out of memory");
        };
        self.recordTextNodeIndex(allocator, elem_id, text_node_index);
    }

    pub fn appendSignalTextNode(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype, elem_id: u64, parent_elem_id: u64, scope_id: u64, signal: HostSignalBinding, read: abi.RocErasedCallable) void {
        self.next_elem_id += 1;
        self.rememberSignalRecordTree(allocator, signal.record);
        abi.increfErasedCallable(read, 1);
        metrics.bump(.closure_retains, 1);
        const signal_text_node_index = self.signal_text_nodes.items.len;

        self.render_nodes.append(allocator, .{ .elem_id = elem_id, .kind = .signal_text }) catch {
            var owned_signal = signal;
            owned_signal.deinit(allocator, roc_host, metrics);
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(read, roc_host);
            @panic("out of memory");
        };
        self.signal_text_nodes.append(allocator, .{
            .elem_id = elem_id,
            .parent_elem_id = parent_elem_id,
            .scope_id = scope_id,
            .signal = signal,
            .read = read,
        }) catch {
            var owned_signal = signal;
            owned_signal.deinit(allocator, roc_host, metrics);
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(read, roc_host);
            @panic("out of memory");
        };
        self.recordSignalTextNodeIndex(allocator, elem_id, signal_text_node_index);
    }

    pub fn appendStaticTextAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderTextField, value: []const u8) void {
        const value_copy = allocator.dupe(u8, value) catch @panic("out of memory");
        const attr_index = self.static_text_attrs.items.len;
        self.static_text_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .value = value_copy,
        }) catch {
            allocator.free(value_copy);
            @panic("out of memory");
        };
        self.recordStaticTextAttrIndex(allocator, elem_id, field, attr_index);
    }

    pub fn appendSignalTextAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype, elem_id: u64, field: RenderTextField, signal: HostSignalBinding, read: abi.RocErasedCallable) void {
        self.rememberSignalRecordTree(allocator, signal.record);
        abi.increfErasedCallable(read, 1);
        metrics.bump(.closure_retains, 1);
        const attr_index = self.signal_text_attrs.items.len;
        self.signal_text_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .signal = signal,
            .read = read,
        }) catch {
            var owned_signal = signal;
            owned_signal.deinit(allocator, roc_host, metrics);
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(read, roc_host);
            @panic("out of memory");
        };
        self.recordSignalTextAttrIndex(allocator, elem_id, field, attr_index);
    }

    pub fn appendStaticBoolAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderBoolField, value: bool) void {
        const attr_index = self.static_bool_attrs.items.len;
        self.static_bool_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .value = value,
        }) catch @panic("out of memory");
        self.recordStaticBoolAttrIndex(allocator, elem_id, field, attr_index);
    }

    pub fn appendSignalBoolAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype, elem_id: u64, field: RenderBoolField, signal: HostSignalBinding, read: abi.RocErasedCallable) void {
        self.rememberSignalRecordTree(allocator, signal.record);
        abi.increfErasedCallable(read, 1);
        metrics.bump(.closure_retains, 1);
        const attr_index = self.signal_bool_attrs.items.len;
        self.signal_bool_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .signal = signal,
            .read = read,
        }) catch {
            var owned_signal = signal;
            owned_signal.deinit(allocator, roc_host, metrics);
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(read, roc_host);
            @panic("out of memory");
        };
        self.recordSignalBoolAttrIndex(allocator, elem_id, field, attr_index);
    }

    pub fn appendOnChange(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype, scope_id: u64, signal: HostSignalBinding, to_cmd: abi.RocErasedCallable) void {
        self.rememberSignalRecordTree(allocator, signal.record);
        abi.increfErasedCallable(to_cmd, 1);
        metrics.bump(.closure_retains, 1);
        self.on_changes.append(allocator, .{
            .scope_id = scope_id,
            .signal = signal,
            .to_cmd = to_cmd,
        }) catch {
            var owned_signal = signal;
            owned_signal.deinit(allocator, roc_host, metrics);
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(to_cmd, roc_host);
            @panic("out of memory");
        };
    }

    pub fn appendCleanup(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, scope_id: u64, name: []const u8) void {
        const name_copy = allocator.dupe(u8, name) catch @panic("out of memory");
        self.cleanups.append(allocator, .{
            .scope_id = scope_id,
            .name = name_copy,
        }) catch {
            allocator.free(name_copy);
            @panic("out of memory");
        };
    }

    pub fn appendEventWithOwnedPayloadTag(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype, elem_id: u64, kind: RenderEventKind, binder_token: HostBinderToken, target_node_id: u64, payload_kind: EventPayloadKind, payload_tag: HostValueTypeTag, payload_drop: abi.RocErasedCallable, transform: abi.RocErasedCallable) void {
        abi.increfErasedCallable(payload_drop, 1);
        abi.increfErasedCallable(transform, 1);
        metrics.bump(.closure_retains, 2);
        const event_index = self.events.items.len;
        self.events.append(allocator, .{
            .elem_id = elem_id,
            .kind = kind,
            .binder_token = binder_token,
            .target_node_id = target_node_id,
            .payload_kind = payload_kind,
            .payload_tag = payload_tag,
            .payload_drop = payload_drop,
            .transform = transform,
        }) catch {
            abi.decrefBox(@ptrCast(payload_tag), roc_host);
            metrics.bump(.closure_releases, 2);
            abi.decrefErasedCallable(payload_drop, roc_host);
            abi.decrefErasedCallable(transform, roc_host);
            @panic("out of memory");
        };
        self.recordEventIndex(allocator, elem_id, kind, event_index);
    }

    pub fn appendScopeSite(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, node_id: u64, scope_id: u64, ordinal: u64, parent_elem_id: u64, kind: HostNodeScopeSiteKind, binder_bindings: []const HostBinderBinding) void {
        self.appendScopeSiteAt(allocator, node_id, scope_id, ordinal, parent_elem_id, self.render_nodes.items.len, kind, binder_bindings);
    }

    pub fn appendScopeSiteAt(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, node_id: u64, scope_id: u64, ordinal: u64, parent_elem_id: u64, render_insert_index: usize, kind: HostNodeScopeSiteKind, binder_bindings: []const HostBinderBinding) void {
        const binder_copy = allocator.dupe(HostBinderBinding, binder_bindings) catch @panic("out of memory");
        self.scope_sites.append(allocator, .{
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
    }

    pub fn appendState(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype, node_id: u64, initial: abi.RocErasedCallable, eq: abi.RocErasedCallable, drop: abi.RocErasedCallable) void {
        abi.increfErasedCallable(initial, 1);
        abi.increfErasedCallable(eq, 1);
        abi.increfErasedCallable(drop, 1);
        metrics.bump(.closure_retains, 3);
        self.states.append(allocator, .{
            .node_id = node_id,
            .initial = initial,
            .eq = eq,
            .drop = drop,
        }) catch {
            metrics.bump(.closure_releases, 3);
            abi.decrefErasedCallable(initial, roc_host);
            abi.decrefErasedCallable(eq, roc_host);
            abi.decrefErasedCallable(drop, roc_host);
            @panic("out of memory");
        };
    }

    pub fn appendEventWithBorrowedPayloadTag(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype, elem_id: u64, kind: RenderEventKind, binder_token: HostBinderToken, target_node_id: u64, payload_kind: EventPayloadKind, payload_tag: HostValueTypeTag, payload_drop: abi.RocErasedCallable, transform: abi.RocErasedCallable) void {
        abi.increfBox(@ptrCast(payload_tag), 1);
        self.appendEventWithOwnedPayloadTag(allocator, roc_host, metrics, elem_id, kind, binder_token, target_node_id, payload_kind, payload_tag, payload_drop, transform);
    }

    pub fn appendEvent(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype, elem_id: u64, kind: RenderEventKind, binder_token: HostBinderToken, target_node_id: u64, payload_kind: EventPayloadKind, payload_tag: HostValueTypeTag, payload_drop: abi.RocErasedCallable, transform: abi.RocErasedCallable) void {
        self.appendEventWithBorrowedPayloadTag(allocator, roc_host, metrics, elem_id, kind, binder_token, target_node_id, payload_kind, payload_tag, payload_drop, transform);
    }

    pub fn appendWhen(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype, node_id: u64, condition: HostSignalBinding, read: abi.RocErasedCallable, when_false: abi.Elem, when_true: abi.Elem) void {
        self.rememberSignalRecordTree(allocator, condition.record);
        abi.increfErasedCallable(read, 1);
        abi.increfElem(when_false, 1);
        abi.increfElem(when_true, 1);
        metrics.bump(.closure_retains, 1);
        self.whens.append(allocator, .{
            .node_id = node_id,
            .condition = condition,
            .read = read,
            .when_false = when_false,
            .when_true = when_true,
        }) catch {
            var owned_condition = condition;
            owned_condition.deinit(allocator, roc_host, metrics);
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(read, roc_host);
            abi.decrefElem(when_false, roc_host);
            abi.decrefElem(when_true, roc_host);
            @panic("out of memory");
        };
    }

    pub fn appendEach(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype, node_id: u64, items: HostSignalBinding, items_to_values: abi.RocErasedCallable, key_hash: abi.RocErasedCallable, key_of: abi.RocErasedCallable, key_eq: abi.RocErasedCallable, key_drop: abi.RocErasedCallable, item_eq: abi.RocErasedCallable, item_drop: abi.RocErasedCallable, row: abi.RocErasedCallable) void {
        self.rememberSignalRecordTree(allocator, items.record);
        abi.increfErasedCallable(items_to_values, 1);
        abi.increfErasedCallable(key_hash, 1);
        abi.increfErasedCallable(key_of, 1);
        abi.increfErasedCallable(key_eq, 1);
        abi.increfErasedCallable(key_drop, 1);
        abi.increfErasedCallable(item_eq, 1);
        abi.increfErasedCallable(item_drop, 1);
        abi.increfErasedCallable(row, 1);
        metrics.bump(.closure_retains, 8);
        self.eaches.append(allocator, .{
            .node_id = node_id,
            .items = items,
            .items_to_values = items_to_values,
            .key_hash = key_hash,
            .key_of = key_of,
            .key_eq = key_eq,
            .key_drop = key_drop,
            .item_eq = item_eq,
            .item_drop = item_drop,
            .row = row,
        }) catch {
            var owned_items = items;
            owned_items.deinit(allocator, roc_host, metrics);
            metrics.bump(.closure_releases, 8);
            abi.decrefErasedCallable(items_to_values, roc_host);
            abi.decrefErasedCallable(key_hash, roc_host);
            abi.decrefErasedCallable(key_of, roc_host);
            abi.decrefErasedCallable(key_eq, roc_host);
            abi.decrefErasedCallable(key_drop, roc_host);
            abi.decrefErasedCallable(item_eq, roc_host);
            abi.decrefErasedCallable(item_drop, roc_host);
            abi.decrefErasedCallable(row, roc_host);
            @panic("out of memory");
        };
    }
};
