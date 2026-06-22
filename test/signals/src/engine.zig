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
const builtin = @import("builtin");
const abi = @import("roc_platform_abi.zig");
const scope_tree = @import("scope_tree.zig");
const erased_calls = @import("erased_calls.zig");
const render = @import("render_commands.zig");
const signal_graph = @import("signal_graph.zig");
const identity_table = @import("identity_table.zig");
const host_value_registry = @import("host_value_registry.zig");
const hv = @import("host_values.zig");

pub const RenderTextField = render.TextField;
pub const RenderBoolField = render.BoolField;
pub const RenderEventKind = render.EventKind;

pub const HostValue = u64;

pub const RuntimeMetrics = struct {
    active_graph_records_rebuilt: u64,
    append_child: u64,
    allocs_this_event: u64,
    bind_event: u64,
    closure_releases: u64,
    closure_retains: u64,
    create_element: u64,
    deallocs_this_event: u64,
    derived_calls_into_roc: u64,
    each_key_compares: u64,
    events_processed: u64,
    move_before: u64,
    nodes_recomputed: u64,
    patches_emitted: u64,
    propagation_prunes: u64,
    recompute_batches: u64,
    remove_node: u64,
    retained_alloc_delta: i64,
    reset_dom: u64,
    rows_created: u64,
    rows_removed: u64,
    rows_reused: u64,
    scopes_created: u64,
    scopes_disposed: u64,
    set_checked: u64,
    set_disabled: u64,
    set_metadata: u64,
    set_text: u64,
    set_value: u64,
    stream_nodes_scanned: u64,

    pub const Field = std.meta.FieldEnum(@This());

    pub inline fn bump(self: *RuntimeMetrics, comptime field: Field, n: u64) void {
        @field(self, @tagName(field)) += n;
    }
};

pub fn zeroRuntimeMetrics() RuntimeMetrics {
    return .{
        .active_graph_records_rebuilt = 0,
        .append_child = 0,
        .allocs_this_event = 0,
        .bind_event = 0,
        .closure_releases = 0,
        .closure_retains = 0,
        .create_element = 0,
        .deallocs_this_event = 0,
        .derived_calls_into_roc = 0,
        .each_key_compares = 0,
        .events_processed = 0,
        .move_before = 0,
        .nodes_recomputed = 0,
        .patches_emitted = 0,
        .propagation_prunes = 0,
        .recompute_batches = 0,
        .remove_node = 0,
        .retained_alloc_delta = 0,
        .reset_dom = 0,
        .rows_created = 0,
        .rows_removed = 0,
        .rows_reused = 0,
        .scopes_created = 0,
        .scopes_disposed = 0,
        .set_checked = 0,
        .set_disabled = 0,
        .set_metadata = 0,
        .set_text = 0,
        .set_value = 0,
        .stream_nodes_scanned = 0,
    };
}

/// Zero-size stand-in for `RuntimeMetrics`: every `bump` is a comptime no-op.
pub const NoMetrics = struct {
    pub const Field = RuntimeMetrics.Field;

    pub inline fn bump(_: *NoMetrics, comptime _: Field, _: u64) void {}
};

pub const native_host_value_type_tags_enabled = switch (builtin.mode) {
    .Debug, .ReleaseSafe => true,
    .ReleaseFast, .ReleaseSmall => false,
};

pub fn verifyRegistryOps(comptime Ops: type) void {
    inline for (.{
        "retainBox",
        "releaseBox",
        "retainTag",
        "releaseTag",
        "tagId",
    }) |decl_name| {
        if (!@hasDecl(Ops, decl_name)) {
            @compileError("engine RegistryOps is missing " ++ decl_name);
        }
    }
}

pub fn verifyCtx(comptime Ctx: type) void {
    inline for (.{
        "HostValueTypeTag",
        "host_value_type_tags_enabled",
        "RegistryOps",
        "Metrics",
        "zeroMetrics",
    }) |decl_name| {
        if (!@hasDecl(Ctx, decl_name)) {
            @compileError("engine Ctx is missing " ++ decl_name);
        }
    }
    verifyRegistryOps(Ctx.RegistryOps);
}

pub const NativeCtx = struct {
    pub const HostValueTypeTag = *anyopaque;
    pub const host_value_type_tags_enabled = native_host_value_type_tags_enabled;
    pub const RegistryOps = hv.RegistryOps;
    pub const Metrics = RuntimeMetrics;

    pub fn zeroMetrics() Metrics {
        return zeroRuntimeMetrics();
    }
};

fn u64SliceContains(items: []const u64, target: u64) bool {
    for (items) |item| {
        if (item == target) return true;
    }
    return false;
}

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

pub const SignalKind = enum(u64) {
    source = 1,
    map = 2,
    map2 = 3,
};

pub const HostEventDescriptor = struct {
    event_id: u64,
    payload_kind: EventPayloadKind,
};

pub fn HostActiveEventDesc(comptime TypeTag: type) type {
    return struct {
        target_node_id: u64,
        payload_kind: EventPayloadKind,
        payload_tag: TypeTag,
        payload_drop: abi.RocErasedCallable,
        transform: abi.RocErasedCallable,
    };
}

pub const HostPendingTask = struct {
    request_id: u64,
    owner_scope_id: u64,
    task_token: HostSignalToken,
    task_name: []const u8,
    request: []const u8,
    active: bool,
};

pub const HostSignalEventRoute = struct {
    event_id: u64,
    signal_ids: []u64,
};

pub const HostState = struct {
    state_id: u64,
    cell: HostValueCell,
    version: u64,
    active: bool,
};

pub const HostSignalDescriptor = struct {
    signal_id: u64,
    kind: SignalKind,
    source_state_ids: []u64,
    source_event_ids: []u64,
    input_signal_ids: []u64,
    rank: u64,
};

pub const HostSignalRoute = struct {
    state_id: u64,
    signal_ids: []u64,
};

pub const HostSignalDependentsRoute = struct {
    signal_id: u64,
    signal_ids: []u64,
};

pub const HostActiveSignalGraphNode = signal_graph.Node(HostSignalRecord);
pub const HostNodeIdentity = identity_table.NodeIdentity;
pub const HostDomIdentity = identity_table.DomIdentity;
pub const HostScopeBranch = scope_tree.Branch;

pub const HostActiveTextSignalSinkKind = enum {
    text_node,
    text_attr,
};

pub const HostActiveTextSignalSink = struct {
    kind: HostActiveTextSignalSinkKind,
    index: usize,
};

pub const HostActiveBoolSignalSink = struct {
    index: usize,
};

pub const HostActiveChangeSignalSink = struct {
    index: usize,
};

pub const HostActiveStructuralSignalKind = enum {
    when,
    each,
};

pub const HostActiveStructuralSignal = struct {
    kind: HostActiveStructuralSignalKind,
    index: usize,
};

pub const HostDirtyStructuralSignal = struct {
    kind: HostActiveStructuralSignalKind,
    node_id: u64,
    branch: ?HostScopeBranch = null,
};

pub const HostEachSite = struct {
    parent_scope_id: u64,
    site_ordinal: u64,
};

pub const HostStructuralReplacementTarget = union(enum) {
    scope: u64,
    each_site: HostEachSite,
};

pub const HostStructuralPatchTargets = struct {
    removed: HostStructuralReplacementTarget,
    replacement: HostStructuralReplacementTarget,
};

pub const HostStructuralSplice = struct {
    removed_elem_ids: []u64,
    touched_parent_ids: []u64,
    replacement_elem_ids: []u64,
    replacement_on_change_indices: []usize,

    pub fn deinit(self: HostStructuralSplice, allocator: std.mem.Allocator) void {
        allocator.free(self.removed_elem_ids);
        allocator.free(self.touched_parent_ids);
        allocator.free(self.replacement_elem_ids);
        allocator.free(self.replacement_on_change_indices);
    }
};

pub const HostStructuralSpliceAndTargets = struct {
    splice: HostStructuralSplice,
    targets: HostStructuralPatchTargets,
};

pub const RecomputeApplyOutcome = struct {
    structural_render_required: bool,
};

pub const HostKeyedRowDiffResult = struct {
    scope_ids: []u64,
    row_items_changed: []bool,
    removed_scope_ids: []u64,
    rows_reused: u64,
    rows_created: u64,
    rows_removed: u64,
    row_items_unchanged: u64,
    row_items_updated: u64,

    pub fn deinit(self: HostKeyedRowDiffResult, allocator: std.mem.Allocator) void {
        allocator.free(self.scope_ids);
        allocator.free(self.row_items_changed);
        allocator.free(self.removed_scope_ids);
    }
};

pub const HostEachRowRenderSegment = struct {
    scope_id: u64,
    start: usize,
    len: usize,
};

pub const HostEachRowRenderMove = struct {
    old_start: usize,
    new_start: usize,
    len: usize,
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

pub fn Engine(comptime Ctx: type) type {
    verifyCtx(Ctx);

    return struct {
        const Self = @This();

        pub const Context = Ctx;
        pub const HostValueTypeTagForCtx = Ctx.HostValueTypeTag;
        pub const Metrics = Ctx.Metrics;
        pub const RegistryOps = Ctx.RegistryOps;
        pub const HostValueRegistry = host_value_registry.Registry(Ctx.HostValueTypeTag, Ctx.host_value_type_tags_enabled);
        pub const ActiveEventDesc = HostActiveEventDesc(Ctx.HostValueTypeTag);
        pub const IdentityInternError = scope_tree.Error || identity_table.Error;
        pub const EventLookupError = error{
            EventIdZero,
            MissingSignalEventRoute,
            SignalEventRouteIndexMismatch,
            MissingEventDescriptor,
            EventDescriptorIndexMismatch,
        };
        pub const SignalLookupError = error{
            MissingSignalRoute,
            SignalRouteIndexMismatch,
            MissingSignalDependentRoute,
            SignalDependentRouteIndexMismatch,
            MissingSignalDescriptor,
            SignalDescriptorIndexMismatch,
        };
        pub const ActiveEventLookupError = error{
            MissingActiveEvent,
        };
        pub const StateLookupError = error{
            MissingActiveState,
        };
        pub const RocHostRequiredError = error{
            MissingRocHost,
        };

        host_values: HostValueRegistry = .{},
        active_events: std.ArrayListUnmanaged(ActiveEventDesc) = .empty,
        event_descriptors: std.ArrayListUnmanaged(HostEventDescriptor) = .empty,
        signal_event_routes: std.ArrayListUnmanaged(HostSignalEventRoute) = .empty,
        signal_descriptors: std.ArrayListUnmanaged(HostSignalDescriptor) = .empty,
        signal_routes: std.ArrayListUnmanaged(HostSignalRoute) = .empty,
        signal_dependents: std.ArrayListUnmanaged(HostSignalDependentsRoute) = .empty,
        signal_cache: std.ArrayListUnmanaged(HostSignalCacheSlot) = .empty,
        states: std.ArrayListUnmanaged(HostState) = .empty,
        scopes: std.ArrayListUnmanaged(HostScope) = .empty,
        node_identities: std.ArrayListUnmanaged(HostNodeIdentity) = .empty,
        dom_identities: std.ArrayListUnmanaged(HostDomIdentity) = .empty,
        active_stream: HostNodeDescriptorStream = .{},
        active_signal_graph: std.ArrayListUnmanaged(HostActiveSignalGraphNode) = .empty,
        active_source_signal_routes: std.ArrayListUnmanaged(std.ArrayListUnmanaged(u64)) = .empty,
        active_text_signal_routes: std.ArrayListUnmanaged(std.ArrayListUnmanaged(HostActiveTextSignalSink)) = .empty,
        active_bool_signal_routes: std.ArrayListUnmanaged(std.ArrayListUnmanaged(HostActiveBoolSignalSink)) = .empty,
        active_change_signal_routes: std.ArrayListUnmanaged(std.ArrayListUnmanaged(HostActiveChangeSignalSink)) = .empty,
        active_structural_signal_routes: std.ArrayListUnmanaged(std.ArrayListUnmanaged(HostActiveStructuralSignal)) = .empty,
        pending_tasks: std.ArrayListUnmanaged(HostPendingTask) = .empty,
        cleanup_events: std.ArrayListUnmanaged([]const u8) = .empty,
        next_task_request_id: u64 = 1,
        next_elem_id: u64 = 0,
        roc_host: ?*abi.RocHost = null,
        root_elem: ?abi.Elem = null,
        last_runtime_metrics: Metrics = Ctx.zeroMetrics(),
        pending_roc_metrics: Metrics = Ctx.zeroMetrics(),
        dirty_signal_generation: u64 = 0,

        pub fn init() Self {
            return .{};
        }

        pub fn recordStreamNodesScanned(self: *Self, count: usize) void {
            self.pending_roc_metrics.bump(.stream_nodes_scanned, @intCast(count));
        }

        pub fn recordScopeCreated(self: *Self) void {
            var metrics = self.pending_roc_metrics;
            metrics.bump(.scopes_created, 1);
            self.pending_roc_metrics = metrics;
        }

        pub fn recordEachKeyCompare(self: *Self) void {
            var metrics = self.pending_roc_metrics;
            metrics.bump(.each_key_compares, 1);
            self.pending_roc_metrics = metrics;
        }

        pub fn clearEventDescriptors(self: *Self) void {
            self.event_descriptors.items.len = 0;
        }

        pub fn deinitActiveEventDesc(self: *Self, roc_host: *abi.RocHost, desc: ActiveEventDesc) void {
            abi.decrefBox(@ptrCast(desc.payload_tag), roc_host);
            abi.decrefErasedCallable(desc.payload_drop, roc_host);
            abi.decrefErasedCallable(desc.transform, roc_host);
            self.pending_roc_metrics.bump(.closure_releases, 2);
        }

        pub fn clearActiveEvents(self: *Self) RocHostRequiredError!void {
            const roc_host = self.roc_host orelse {
                if (self.active_events.items.len != 0) return RocHostRequiredError.MissingRocHost;
                return;
            };
            for (self.active_events.items) |desc| {
                self.deinitActiveEventDesc(roc_host, desc);
            }
            self.active_events.items.len = 0;
        }

        pub fn clearSignalCache(self: *Self) RocHostRequiredError!void {
            const roc_host = self.roc_host orelse {
                if (self.signal_cache.items.len != 0) return RocHostRequiredError.MissingRocHost;
                return;
            };
            for (self.signal_cache.items) |*slot| {
                slot.deinit(roc_host, &self.pending_roc_metrics);
            }
            self.signal_cache.items.len = 0;
        }

        pub fn clearStates(self: *Self) RocHostRequiredError!void {
            const roc_host = self.roc_host orelse {
                for (self.states.items) |state| {
                    if (state.active) return RocHostRequiredError.MissingRocHost;
                }
                self.states.items.len = 0;
                return;
            };
            for (self.states.items) |*state| {
                if (!state.active) continue;
                state.cell.deinit(roc_host, &self.pending_roc_metrics);
                state.active = false;
            }
            self.states.items.len = 0;
        }

        pub fn deactivateState(self: *Self, roc_host: *abi.RocHost, node_id: u64) void {
            const state_index = self.stateIndexByNodeId(node_id) orelse return;
            const state = &self.states.items[state_index];
            state.cell.deinit(roc_host, &self.pending_roc_metrics);
            state.active = false;
        }

        pub fn clearScopes(self: *Self) RocHostRequiredError!void {
            if (self.roc_host) |roc_host| {
                for (self.scopes.items) |*scope| {
                    if (!scope.active) continue;
                    deinitHostScopeStep(&scope.step, roc_host, &self.pending_roc_metrics);
                }
            } else if (self.scopes.items.len != 0) {
                return RocHostRequiredError.MissingRocHost;
            }
            self.scopes.items.len = 0;
        }

        pub fn cleanupEventCount(self: *const Self, name: []const u8) u64 {
            var count: u64 = 0;
            for (self.cleanup_events.items) |event_name| {
                if (std.mem.eql(u8, event_name, name)) count += 1;
            }
            return count;
        }

        pub fn activeTaskRecordByToken(self: *Self, token: HostSignalToken) ?*HostSignalRecord {
            for (self.active_signal_graph.items) |node| {
                switch (node.record.payload) {
                    .task_source => |payload| {
                        if (payload.token == token) return node.record;
                    },
                    .ref, .const_value, .map, .map2, .combine, .interval_source => {},
                }
            }
            return null;
        }

        pub fn activeIntervalRecordCountByPeriod(self: *const Self, period_ms: u64) u64 {
            var count: u64 = 0;
            for (self.active_signal_graph.items) |node| {
                switch (node.record.payload) {
                    .interval_source => |payload| {
                        if (payload.period_ms == period_ms) count += 1;
                    },
                    .ref, .const_value, .map, .map2, .combine, .task_source => {},
                }
            }
            return count;
        }

        pub fn pendingTaskCountByName(self: *const Self, name: []const u8) u64 {
            var count: u64 = 0;
            for (self.pending_tasks.items) |task| {
                if (task.active and std.mem.eql(u8, task.task_name, name)) count += 1;
            }
            return count;
        }

        pub fn sourceSignalIdsForEvent(self: *Self, event_id: u64) EventLookupError![]const u64 {
            if (event_id == 0) return EventLookupError.EventIdZero;

            const route_index = event_id - 1;
            if (route_index >= self.signal_event_routes.items.len) return EventLookupError.MissingSignalEventRoute;

            const route = self.signal_event_routes.items[@intCast(route_index)];
            if (route.event_id != event_id) return EventLookupError.SignalEventRouteIndexMismatch;
            return route.signal_ids;
        }

        pub fn eventPayloadKind(self: *Self, event_id: u64) EventLookupError!EventPayloadKind {
            if (event_id == 0) return EventLookupError.EventIdZero;

            const event_index = event_id - 1;
            if (event_index >= self.event_descriptors.items.len) return EventLookupError.MissingEventDescriptor;

            const descriptor = self.event_descriptors.items[@intCast(event_index)];
            if (descriptor.event_id != event_id) return EventLookupError.EventDescriptorIndexMismatch;
            return descriptor.payload_kind;
        }

        pub fn signalIdsForState(self: *Self, state_id: u64) SignalLookupError![]const u64 {
            if (state_id >= self.signal_routes.items.len) return SignalLookupError.MissingSignalRoute;

            const route = self.signal_routes.items[@intCast(state_id)];
            if (route.state_id != state_id) return SignalLookupError.SignalRouteIndexMismatch;
            return route.signal_ids;
        }

        pub fn dependentSignalIdsForSignal(self: *Self, signal_id: u64) SignalLookupError![]const u64 {
            if (signal_id >= self.signal_dependents.items.len) return SignalLookupError.MissingSignalDependentRoute;

            const route = self.signal_dependents.items[@intCast(signal_id)];
            if (route.signal_id != signal_id) return SignalLookupError.SignalDependentRouteIndexMismatch;
            return route.signal_ids;
        }

        pub fn signalRank(self: *Self, signal_id: u64) SignalLookupError!u64 {
            if (signal_id >= self.signal_descriptors.items.len) return SignalLookupError.MissingSignalDescriptor;

            const descriptor = self.signal_descriptors.items[@intCast(signal_id)];
            if (descriptor.signal_id != signal_id) return SignalLookupError.SignalDescriptorIndexMismatch;
            return descriptor.rank;
        }

        pub fn nextDirtySignalGeneration(self: *Self) u64 {
            if (self.dirty_signal_generation == std.math.maxInt(u64)) {
                @panic("host dirty signal generation overflowed");
            }
            self.dirty_signal_generation += 1;
            return self.dirty_signal_generation;
        }

        pub fn appendSignalAndDependents(self: *Self, allocator: std.mem.Allocator, signal_ids: *std.ArrayListUnmanaged(u64), signal_id: u64) void {
            if (!u64SliceContains(signal_ids.items, signal_id)) {
                signal_ids.append(allocator, signal_id) catch @panic("out of memory");
            }

            var index: usize = 0;
            while (index < signal_ids.items.len) : (index += 1) {
                const current_signal_id = signal_ids.items[index];
                const dependents = self.dependentSignalIdsForSignal(current_signal_id) catch @panic("host signal dependent route table is invalid");
                for (dependents) |dependent_signal_id| {
                    if (!u64SliceContains(signal_ids.items, dependent_signal_id)) {
                        signal_ids.append(allocator, dependent_signal_id) catch @panic("out of memory");
                    }
                }
            }
        }

        pub fn sortSignalIdsByRank(self: *Self, signal_ids: []u64) void {
            var index: usize = 1;
            while (index < signal_ids.len) : (index += 1) {
                const value = signal_ids[index];
                const value_rank = self.signalRank(value) catch @panic("host signal rank table is invalid");
                var insert_index = index;
                while (insert_index > 0) {
                    const previous = signal_ids[insert_index - 1];
                    const previous_rank = self.signalRank(previous) catch @panic("host signal rank table is invalid");
                    if (previous_rank < value_rank or (previous_rank == value_rank and previous < value)) break;
                    signal_ids[insert_index] = previous;
                    insert_index -= 1;
                }
                signal_ids[insert_index] = value;
            }
        }

        pub fn dirtySignalIdsForEvent(self: *Self, allocator: std.mem.Allocator, event_id: u64) []u64 {
            var dirty_signal_ids: std.ArrayListUnmanaged(u64) = .empty;
            errdefer dirty_signal_ids.deinit(allocator);

            const source_signal_ids = self.sourceSignalIdsForEvent(event_id) catch @panic("event id has no host source signal route descriptor");
            for (source_signal_ids) |signal_id| {
                self.appendSignalAndDependents(allocator, &dirty_signal_ids, signal_id);
            }

            const signal_ids = dirty_signal_ids.toOwnedSlice(allocator) catch @panic("out of memory");
            self.sortSignalIdsByRank(signal_ids);
            return signal_ids;
        }

        pub fn activeSignalRank(self: *Self, record_id: u64) u64 {
            return signal_graph.rank(HostSignalRecord, self.active_signal_graph.items, record_id) catch @panic("active signal record id has no graph node");
        }

        pub fn dependentActiveSignalRecordIds(self: *Self, record_id: u64) []const u64 {
            return signal_graph.dependentIds(HostSignalRecord, self.active_signal_graph.items, record_id) catch @panic("active signal record id has no dependent table");
        }

        pub fn appendActiveSignalAndDependents(self: *Self, allocator: std.mem.Allocator, record_ids: *std.ArrayListUnmanaged(u64), record_id: u64) void {
            signal_graph.appendReachableDependents(HostSignalRecord, allocator, self.active_signal_graph.items, record_ids, record_id) catch |err| switch (err) {
                error.OutOfMemory => @panic("out of memory"),
                else => @panic("active signal dependent traversal referenced an unknown record"),
            };
        }

        pub fn sortActiveSignalRecordIdsByRank(self: *Self, record_ids: []u64) void {
            signal_graph.sortIdsByRank(HostSignalRecord, self.active_signal_graph.items, record_ids) catch {
                @panic("active signal rank sort referenced an unknown record");
            };
        }

        pub fn dirtyActiveSignalRecordIdsForSources(self: *Self, allocator: std.mem.Allocator, dirty_source_node_ids: []const u64) []u64 {
            var dirty_record_ids: std.ArrayListUnmanaged(u64) = .empty;
            errdefer dirty_record_ids.deinit(allocator);

            for (dirty_source_node_ids) |source_node_id| {
                const route_index: usize = @intCast(source_node_id);
                if (route_index >= self.active_source_signal_routes.items.len) continue;

                for (self.active_source_signal_routes.items[route_index].items) |record_id| {
                    self.appendActiveSignalAndDependents(allocator, &dirty_record_ids, record_id);
                }
            }

            const record_ids = dirty_record_ids.toOwnedSlice(allocator) catch @panic("out of memory");
            self.sortActiveSignalRecordIdsByRank(record_ids);
            return record_ids;
        }

        pub fn dirtyActiveSignalRecordIdsForRoots(self: *Self, allocator: std.mem.Allocator, root_record_ids: []const u64) []u64 {
            var dirty_record_ids: std.ArrayListUnmanaged(u64) = .empty;
            errdefer dirty_record_ids.deinit(allocator);

            for (root_record_ids) |record_id| {
                if (record_id >= self.active_signal_graph.items.len) @panic("dirty active signal root referenced an unknown record");
                self.appendActiveSignalAndDependents(allocator, &dirty_record_ids, record_id);
            }

            const record_ids = dirty_record_ids.toOwnedSlice(allocator) catch @panic("out of memory");
            self.sortActiveSignalRecordIdsByRank(record_ids);
            return record_ids;
        }

        pub fn stateIndexByNodeId(self: *Self, node_id: u64) ?usize {
            for (self.states.items, 0..) |state, index| {
                if (state.active and state.state_id == node_id) return index;
            }
            return null;
        }

        pub fn stateEqCallable(self: *Self, node_id: u64) StateLookupError!abi.RocErasedCallable {
            const state_index = self.stateIndexByNodeId(node_id) orelse return StateLookupError.MissingActiveState;
            return self.states.items[state_index].cell.eq;
        }

        pub fn stateDropCallable(self: *Self, node_id: u64) StateLookupError!abi.RocErasedCallable {
            const state_index = self.stateIndexByNodeId(node_id) orelse return StateLookupError.MissingActiveState;
            return self.states.items[state_index].cell.drop;
        }

        pub fn activeEventTransformByIndex(self: *Self, event_index: usize) ActiveEventLookupError!abi.RocErasedCallable {
            if (event_index >= self.active_events.items.len) return ActiveEventLookupError.MissingActiveEvent;
            return self.active_events.items[event_index].transform;
        }

        pub fn activeEventPayloadDropByIndex(self: *Self, event_index: usize) ActiveEventLookupError!abi.RocErasedCallable {
            if (event_index >= self.active_events.items.len) return ActiveEventLookupError.MissingActiveEvent;
            return self.active_events.items[event_index].payload_drop;
        }

        pub fn activeEventPayloadTagByIndex(self: *Self, event_index: usize) ActiveEventLookupError!Ctx.HostValueTypeTag {
            if (event_index >= self.active_events.items.len) return ActiveEventLookupError.MissingActiveEvent;
            return self.active_events.items[event_index].payload_tag;
        }

        pub fn activeScopeSiteByNodeId(self: *Self, node_id: u64, kind: HostNodeScopeSiteKind) ?HostNodeScopeSiteDesc {
            for (self.active_stream.scope_sites.items) |site| {
                if (site.node_id == node_id and site.kind == kind) return site;
            }
            return null;
        }

        pub fn activeWhenIndexByNodeId(self: *Self, node_id: u64) ?usize {
            for (self.active_stream.whens.items, 0..) |when, index| {
                if (when.node_id == node_id) return index;
            }
            return null;
        }

        pub fn activeEachIndexByNodeId(self: *Self, node_id: u64) ?usize {
            for (self.active_stream.eaches.items, 0..) |each, index| {
                if (each.node_id == node_id) return index;
            }
            return null;
        }

        pub fn recordSliceContains(records: []const *HostSignalRecord, record: *HostSignalRecord) bool {
            for (records) |existing| {
                if (existing == record) return true;
            }
            return false;
        }

        pub fn activeWhenBranchScopeId(self: *Self, parent_scope_id: u64, site_ordinal: u64, branch: HostScopeBranch) scope_tree.Error!?u64 {
            return scope_tree.activeWhenBranch(HostEachRowScopeStep, self.scopes.items, parent_scope_id, site_ordinal, branch);
        }

        pub fn validateScopeId(self: *Self, scope_id: u64) scope_tree.Error!void {
            return scope_tree.validate(HostEachRowScopeStep, self.scopes.items, scope_id);
        }

        pub fn internRootScope(self: *Self, allocator: std.mem.Allocator) scope_tree.Error!u64 {
            const result = try scope_tree.internRoot(HostEachRowScopeStep, allocator, &self.scopes);
            if (result.created) self.recordScopeCreated();
            return result.scope_id;
        }

        pub fn internComponentScope(self: *Self, allocator: std.mem.Allocator, parent_scope_id: u64, site_ordinal: u64) scope_tree.Error!u64 {
            const result = try scope_tree.internComponent(HostEachRowScopeStep, allocator, &self.scopes, parent_scope_id, site_ordinal);
            if (result.created) self.recordScopeCreated();
            return result.scope_id;
        }

        pub fn internWhenBranchScope(self: *Self, allocator: std.mem.Allocator, parent_scope_id: u64, site_ordinal: u64, branch: HostScopeBranch) scope_tree.Error!u64 {
            const result = try scope_tree.internWhenBranch(HostEachRowScopeStep, allocator, &self.scopes, parent_scope_id, site_ordinal, branch);
            if (result.created) self.recordScopeCreated();
            return result.scope_id;
        }

        pub fn internNodeIdentity(self: *Self, allocator: std.mem.Allocator, scope_id: u64, ordinal: u64) IdentityInternError!u64 {
            try self.validateScopeId(scope_id);
            return identity_table.internNode(allocator, &self.node_identities, scope_id, ordinal);
        }

        pub fn internDomIdentity(self: *Self, allocator: std.mem.Allocator, scope_id: u64, ordinal: u64) IdentityInternError!u64 {
            try self.validateScopeId(scope_id);
            return identity_table.internDom(allocator, &self.dom_identities, scope_id, ordinal);
        }

        pub fn activeEachRowScopes(self: *Self, allocator: std.mem.Allocator, parent_scope_id: u64, site_ordinal: u64) scope_tree.Error![]u64 {
            return scope_tree.activeEachRows(HostEachRowScopeStep, allocator, self.scopes.items, parent_scope_id, site_ordinal);
        }

        pub fn eachSiteRowAncestorScopeId(self: *Self, scope_id: u64, site: HostEachSite) scope_tree.Error!?u64 {
            return scope_tree.eachSiteRowAncestor(HostEachRowScopeStep, self.scopes.items, scope_id, site.parent_scope_id, site.site_ordinal);
        }

        pub fn scopeIsDescendantOrSelf(self: *Self, scope_id: u64, root_scope_id: u64) scope_tree.Error!bool {
            return scope_tree.descendantOrSelf(HostEachRowScopeStep, self.scopes.items, scope_id, root_scope_id);
        }

        pub fn scopeIsEachSiteRowDescendantOrSelf(self: *Self, scope_id: u64, site: HostEachSite) scope_tree.Error!bool {
            return scope_tree.eachSiteRowDescendantOrSelf(HostEachRowScopeStep, self.scopes.items, scope_id, site.parent_scope_id, site.site_ordinal);
        }

        pub fn eachDiffPreservesSurvivorRenderOrder(old_render_rows: []const u64, next_scope_ids: []const u64) bool {
            var old_index: usize = 0;
            for (next_scope_ids) |next_scope_id| {
                if (!u64SliceContains(old_render_rows, next_scope_id)) continue;
                while (old_index < old_render_rows.len and !u64SliceContains(next_scope_ids, old_render_rows[old_index])) {
                    old_index += 1;
                }
                if (old_index >= old_render_rows.len) return false;
                if (old_render_rows[old_index] != next_scope_id) return false;
                old_index += 1;
            }
            return true;
        }
    };
}

comptime {
    verifyCtx(NativeCtx);
    std.debug.assert(@sizeOf(NoMetrics) == 0);
    _ = Engine(NativeCtx);
}
