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
