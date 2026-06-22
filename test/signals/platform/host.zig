//! Platform host for testing signal-based reactive UI applications.
//!
//! The host owns simulated DOM state, event routing, signal evaluation,
//! scope/key lifecycle, and render batching. Roc returns a retained immutable
//! descriptor tree from `roc_ui_init`; the host drives all later events.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const abi = @import("roc_platform_abi.zig");

const ElemBox = @typeInfo(@TypeOf(abi.roc_ui_init)).@"fn".return_type.?;
const RocStr = abi.RocStr;
const HostValue = u64;
const HostValueTypeTag = *anyopaque;
const HostValueList = abi.RocListWith(HostValue, false);
const I64List = abi.RocListWith(i64, false);

const host_value_type_tags_enabled = switch (builtin.mode) {
    .Debug, .ReleaseSafe => true,
    .ReleaseFast, .ReleaseSmall => false,
};

const RuntimeMetrics = struct {
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
    nodes_recomputed: u64,
    patches_emitted: u64,
    propagation_prunes: u64,
    recompute_batches: u64,
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
};

const EventPayloadKind = enum(u64) {
    unit = 1,
    str = 2,
    bool = 3,
};

const SignalKind = enum(u64) {
    source = 1,
    map = 2,
    map2 = 3,
};

const RenderTextField = enum(u64) {
    text = 1,
    role = 2,
    label = 3,
    test_id = 4,
    value = 5,
};

const RenderBoolField = enum(u64) {
    checked = 1,
    disabled = 2,
};

const RenderEventKind = enum(u64) {
    click = 1,
    input = 2,
    check = 3,
};

const HostEventDescriptor = struct {
    event_id: u64,
    payload_kind: EventPayloadKind,
};

const HostActiveEventDesc = struct {
    target_node_id: u64,
    payload_kind: EventPayloadKind,
    payload_tag: HostValueTypeTag,
    payload_drop: abi.RocErasedCallable,
    transform: abi.RocErasedCallable,
};

const HostPendingTask = struct {
    request_id: u64,
    owner_scope_id: u64,
    task_token: HostSignalToken,
    task_name: []const u8,
    request: []const u8,
    active: bool,
};

const HostSignalEventRoute = struct {
    event_id: u64,
    signal_ids: []u64,
};

const HostValueCell = struct {
    value: HostValue,
    eq: abi.RocErasedCallable,
    drop: abi.RocErasedCallable,

    fn initRetained(value: HostValue, eq: abi.RocErasedCallable, drop: abi.RocErasedCallable, metrics: *RuntimeMetrics) HostValueCell {
        abi.increfErasedCallable(eq, 1);
        abi.increfErasedCallable(drop, 1);
        metrics.closure_retains += 2;
        return .{ .value = value, .eq = eq, .drop = drop };
    }

    fn cloneRetained(self: HostValueCell, host: *HostEnv, metrics: *RuntimeMetrics) HostValueCell {
        const value = host.cloneHostValue(self.value);
        abi.increfErasedCallable(self.eq, 1);
        abi.increfErasedCallable(self.drop, 1);
        metrics.closure_retains += 2;
        return .{ .value = value, .eq = self.eq, .drop = self.drop };
    }

    fn deinit(self: *HostValueCell, roc_host: *abi.RocHost, metrics: *RuntimeMetrics) void {
        callErasedHostValueToUnit(roc_host, self.drop, self.value);
        abi.decrefErasedCallable(self.eq, roc_host);
        abi.decrefErasedCallable(self.drop, roc_host);
        metrics.closure_releases += 2;
        self.* = undefined;
    }

    fn valueEquals(self: *const HostValueCell, roc_host: *abi.RocHost, value: HostValue) bool {
        return callErasedHostValueHostValueToBool(roc_host, self.eq, self.value, value);
    }

    fn dropIncoming(self: *const HostValueCell, roc_host: *abi.RocHost, value: HostValue) void {
        callErasedHostValueToUnit(roc_host, self.drop, value);
    }

    fn replaceValue(self: *HostValueCell, roc_host: *abi.RocHost, value: HostValue) void {
        callErasedHostValueToUnit(roc_host, self.drop, self.value);
        self.value = value;
    }
};

const HostState = struct {
    state_id: u64,
    cell: HostValueCell,
    version: u64,
    active: bool,
};

const HostSignalDescriptor = struct {
    signal_id: u64,
    kind: SignalKind,
    source_state_ids: []u64,
    source_event_ids: []u64,
    input_signal_ids: []u64,
    rank: u64,
};

const HostSignalRoute = struct {
    state_id: u64,
    signal_ids: []u64,
};

const HostSignalDependentsRoute = struct {
    signal_id: u64,
    signal_ids: []u64,
};

const HostSignalCacheSlot = union(enum) {
    absent,
    present: HostValueCell,

    fn deinit(self: *HostSignalCacheSlot, roc_host: *abi.RocHost, metrics: *RuntimeMetrics) void {
        switch (self.*) {
            .absent => {},
            .present => |*cached| cached.deinit(roc_host, metrics),
        }
        self.* = .absent;
    }

    fn replace(self: *HostSignalCacheSlot, roc_host: *abi.RocHost, metrics: *RuntimeMetrics, value: HostValue, eq: abi.RocErasedCallable, drop: abi.RocErasedCallable) void {
        self.deinit(roc_host, metrics);
        self.* = .{ .present = HostValueCell.initRetained(value, eq, drop, metrics) };
    }

    fn replaceValue(self: *HostSignalCacheSlot, roc_host: *abi.RocHost, value: HostValue) void {
        switch (self.*) {
            .absent => failHost("dirty signal expression was evaluated before its initial value was cached"),
            .present => |*cached| cached.replaceValue(roc_host, value),
        }
    }

    fn cloneRetained(self: HostSignalCacheSlot, host: *HostEnv, metrics: *RuntimeMetrics) HostSignalCacheSlot {
        return switch (self) {
            .absent => .absent,
            .present => |cached| .{ .present = cached.cloneRetained(host, metrics) },
        };
    }
};

const HostScopeBranch = enum(u8) {
    false_branch,
    true_branch,

    fn opposite(self: HostScopeBranch) HostScopeBranch {
        return switch (self) {
            .false_branch => .true_branch,
            .true_branch => .false_branch,
        };
    }
};

const HostWhenBranchScopeStep = struct {
    site_ordinal: u64,
    branch: HostScopeBranch,
};

const HostComponentScopeStep = struct {
    site_ordinal: u64,
};

const HostEachRowScopeStep = struct {
    site_ordinal: u64,
    key: HostValueCell,
    item: HostValueCell,
};

const HostScopeStep = union(enum) {
    root,
    component: HostComponentScopeStep,
    when_branch: HostWhenBranchScopeStep,
    each_row: HostEachRowScopeStep,

    fn deinit(self: *HostScopeStep, roc_host: *abi.RocHost, metrics: *RuntimeMetrics) void {
        switch (self.*) {
            .each_row => |*row| {
                row.key.deinit(roc_host, metrics);
                row.item.deinit(roc_host, metrics);
            },
            .root, .component, .when_branch => {},
        }
    }
};

const HostScope = struct {
    scope_id: u64,
    parent_scope_id: ?u64,
    step: HostScopeStep,
    active: bool,
};

const HostNodeIdentity = struct {
    node_id: u64,
    scope_id: u64,
    ordinal: u64,
    active: bool,
};

const HostDomIdentity = struct {
    elem_id: u64,
    scope_id: u64,
    ordinal: u64,
    active: bool,
};

const HostNodeScopeSiteKind = enum {
    component,
    state,
    when,
    each,
};

const HostRenderNodeKind = enum {
    element,
    text,
    signal_text,
};

const HostRenderNode = struct {
    elem_id: u64,
    kind: HostRenderNodeKind,
};

const HostBinderToken = *u64;
const HostSignalToken = *u64;

const HostBinderBinding = struct {
    token: HostBinderToken,
    node_id: u64,
};

fn retainHostCallable(callable: abi.RocErasedCallable, metrics: *RuntimeMetrics) abi.RocErasedCallable {
    abi.increfErasedCallable(callable, 1);
    metrics.closure_retains += 1;
    return callable;
}

fn retainHostSignalToken(token: HostSignalToken) HostSignalToken {
    abi.increfBox(@ptrCast(token), 1);
    return token;
}

const HostSignalConstRecord = struct {
    token: HostSignalToken,
    init: abi.RocErasedCallable,
    eq: abi.RocErasedCallable,
    drop: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

const HostSignalMapRecord = struct {
    token: HostSignalToken,
    input: *HostSignalRecord,
    transform: abi.RocErasedCallable,
    eq: abi.RocErasedCallable,
    drop: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

const HostSignalMap2Record = struct {
    token: HostSignalToken,
    left: *HostSignalRecord,
    right: *HostSignalRecord,
    transform: abi.RocErasedCallable,
    eq: abi.RocErasedCallable,
    drop: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

const HostSignalCombineRecord = struct {
    token: HostSignalToken,
    children: []*HostSignalRecord,
    transform: abi.RocErasedCallable,
    eq: abi.RocErasedCallable,
    drop: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

const HostSignalTaskSourceRecord = struct {
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

const HostSignalIntervalSourceRecord = struct {
    token: HostSignalToken,
    period_ms: u64,
    initial: abi.RocErasedCallable,
    tick: abi.RocErasedCallable,
    eq: abi.RocErasedCallable,
    drop: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

const HostSignalRecordPayload = union(enum) {
    ref: u64,
    const_value: HostSignalConstRecord,
    map: HostSignalMapRecord,
    map2: HostSignalMap2Record,
    combine: HostSignalCombineRecord,
    task_source: HostSignalTaskSourceRecord,
    interval_source: HostSignalIntervalSourceRecord,
};

const HostSignalRecord = struct {
    ref_count: usize,
    payload: HostSignalRecordPayload,
    active_graph_id: ?u64 = null,
    active_use_count: usize = 0,
    last_dirty_generation: u64 = 0,
    last_dirty_changed: bool = false,

    fn init(allocator: std.mem.Allocator, payload: HostSignalRecordPayload) *HostSignalRecord {
        const record = allocator.create(HostSignalRecord) catch std.process.exit(1);
        record.* = .{
            .ref_count = 1,
            .payload = payload,
        };
        return record;
    }

    fn token(self: *const HostSignalRecord) ?HostSignalToken {
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

    fn retain(self: *HostSignalRecord) *HostSignalRecord {
        self.ref_count += 1;
        return self;
    }

    fn release(self: *HostSignalRecord, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: *RuntimeMetrics) void {
        if (self.ref_count == 0) failHost("host signal record release underflow");
        if (self.ref_count == 1 and self.active_graph_id != null) failHost("active signal graph held the last signal record reference");
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
                metrics.closure_releases += 3;
            },
            .map => |payload| {
                payload.input.release(allocator, roc_host, metrics);
                var cached_value = payload.cached_value;
                cached_value.deinit(roc_host, metrics);
                abi.decrefBox(@ptrCast(payload.token), roc_host);
                abi.decrefErasedCallable(payload.transform, roc_host);
                abi.decrefErasedCallable(payload.eq, roc_host);
                abi.decrefErasedCallable(payload.drop, roc_host);
                metrics.closure_releases += 3;
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
                metrics.closure_releases += 3;
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
                metrics.closure_releases += 3;
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
                metrics.closure_releases += 6;
            },
            .interval_source => |payload| {
                var cached_value = payload.cached_value;
                cached_value.deinit(roc_host, metrics);
                abi.decrefBox(@ptrCast(payload.token), roc_host);
                abi.decrefErasedCallable(payload.initial, roc_host);
                abi.decrefErasedCallable(payload.tick, roc_host);
                abi.decrefErasedCallable(payload.eq, roc_host);
                abi.decrefErasedCallable(payload.drop, roc_host);
                metrics.closure_releases += 4;
            },
        }

        allocator.destroy(self);
    }
};

const HostSignalBinding = struct {
    record: *HostSignalRecord,
    source_node_ids: []u64,

    fn cloneRetained(self: HostSignalBinding, allocator: std.mem.Allocator, metrics: *RuntimeMetrics) HostSignalBinding {
        _ = metrics;
        return .{
            .record = self.record.retain(),
            .source_node_ids = allocator.dupe(u64, self.source_node_ids) catch std.process.exit(1),
        };
    }

    fn deinit(self: *HostSignalBinding, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: *RuntimeMetrics) void {
        self.record.release(allocator, roc_host, metrics);
        allocator.free(self.source_node_ids);
    }
};

const HostNodeScopeSiteDesc = struct {
    node_id: u64,
    scope_id: u64,
    ordinal: u64,
    parent_elem_id: u64,
    render_insert_index: usize,
    kind: HostNodeScopeSiteKind,
    binder_bindings: []HostBinderBinding,
};

const HostElementDesc = struct {
    elem_id: u64,
    parent_elem_id: u64,
    scope_id: u64,
    tag: []const u8,
};

const HostNodeTextNodeDesc = struct {
    elem_id: u64,
    parent_elem_id: u64,
    scope_id: u64,
    value: []const u8,
};

const HostNodeSignalTextNodeDesc = struct {
    elem_id: u64,
    parent_elem_id: u64,
    scope_id: u64,
    signal: HostSignalBinding,
    read: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

const HostNodeStaticTextAttrDesc = struct {
    elem_id: u64,
    field: RenderTextField,
    value: []const u8,
};

const HostNodeSignalTextAttrDesc = struct {
    elem_id: u64,
    field: RenderTextField,
    signal: HostSignalBinding,
    read: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

const HostNodeStaticBoolAttrDesc = struct {
    elem_id: u64,
    field: RenderBoolField,
    value: bool,
};

const HostNodeSignalBoolAttrDesc = struct {
    elem_id: u64,
    field: RenderBoolField,
    signal: HostSignalBinding,
    read: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

const HostNodeOnChangeDesc = struct {
    scope_id: u64,
    signal: HostSignalBinding,
    to_cmd: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

const HostNodeCleanupDesc = struct {
    scope_id: u64,
    name: []const u8,
};

const HostNodeEventDesc = struct {
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

const HostNodeStateDesc = struct {
    node_id: u64,
    initial: abi.RocErasedCallable,
    eq: abi.RocErasedCallable,
    drop: abi.RocErasedCallable,
};

const HostNodeWhenDesc = struct {
    node_id: u64,
    condition: HostSignalBinding,
    read: abi.RocErasedCallable,
    when_false: abi.Elem,
    when_true: abi.Elem,
    cached_value: HostSignalCacheSlot = .absent,
};

const HostNodeEachDesc = struct {
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

const HostSignalRecordTokenEntry = struct {
    token: HostSignalToken,
    record: *HostSignalRecord,
};

const HostActiveSignalGraphNode = struct {
    record: *HostSignalRecord,
    rank: u64 = 0,
    dependents: []u64 = &.{},
};

const HostTextFieldDescriptorIndexes = struct {
    text: ?usize = null,
    role: ?usize = null,
    label: ?usize = null,
    test_id: ?usize = null,
    value: ?usize = null,

    fn get(self: HostTextFieldDescriptorIndexes, field: RenderTextField) ?usize {
        return switch (field) {
            .text => self.text,
            .role => self.role,
            .label => self.label,
            .test_id => self.test_id,
            .value => self.value,
        };
    }

    fn slot(self: *HostTextFieldDescriptorIndexes, field: RenderTextField) *?usize {
        return switch (field) {
            .text => &self.text,
            .role => &self.role,
            .label => &self.label,
            .test_id => &self.test_id,
            .value => &self.value,
        };
    }
};

const HostBoolFieldDescriptorIndexes = struct {
    checked: ?usize = null,
    disabled: ?usize = null,

    fn get(self: HostBoolFieldDescriptorIndexes, field: RenderBoolField) ?usize {
        return switch (field) {
            .checked => self.checked,
            .disabled => self.disabled,
        };
    }

    fn slot(self: *HostBoolFieldDescriptorIndexes, field: RenderBoolField) *?usize {
        return switch (field) {
            .checked => &self.checked,
            .disabled => &self.disabled,
        };
    }
};

const HostEventDescriptorIndexes = struct {
    click: ?usize = null,
    input: ?usize = null,
    check: ?usize = null,

    fn get(self: HostEventDescriptorIndexes, kind: RenderEventKind) ?usize {
        return switch (kind) {
            .click => self.click,
            .input => self.input,
            .check => self.check,
        };
    }

    fn slot(self: *HostEventDescriptorIndexes, kind: RenderEventKind) *?usize {
        return switch (kind) {
            .click => &self.click,
            .input => &self.input,
            .check => &self.check,
        };
    }
};

const HostElemDescriptorIndex = struct {
    element: ?usize = null,
    text_node: ?usize = null,
    signal_text_node: ?usize = null,
    static_text_attrs: HostTextFieldDescriptorIndexes = .{},
    signal_text_attrs: HostTextFieldDescriptorIndexes = .{},
    static_bool_attrs: HostBoolFieldDescriptorIndexes = .{},
    signal_bool_attrs: HostBoolFieldDescriptorIndexes = .{},
    events: HostEventDescriptorIndexes = .{},
};

const HostNodeDescriptorStream = struct {
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

    fn ensureElemDescriptorIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64) *HostElemDescriptorIndex {
        const index: usize = @intCast(elem_id);
        while (self.descriptor_indexes_by_elem_id.items.len <= index) {
            self.descriptor_indexes_by_elem_id.append(allocator, .{}) catch std.process.exit(1);
        }
        return &self.descriptor_indexes_by_elem_id.items[index];
    }

    fn elemDescriptorIndex(self: *const HostNodeDescriptorStream, elem_id: u64) ?HostElemDescriptorIndex {
        if (elem_id >= self.descriptor_indexes_by_elem_id.items.len) return null;
        return self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)];
    }

    fn setFreshIndex(slot: *?usize, value: usize) void {
        if (slot.* != null) failHost("descriptor stream recorded duplicate descriptor index");
        slot.* = value;
    }

    fn updateIndex(slot: *?usize, value: usize) void {
        if (slot.* == null) failHost("descriptor stream updated a missing descriptor index");
        slot.* = value;
    }

    fn clearIndex(slot: *?usize, expected: usize) void {
        const existing = slot.* orelse failHost("descriptor stream cleared a missing descriptor index");
        if (existing != expected) failHost("descriptor stream cleared the wrong descriptor index");
        slot.* = null;
    }

    fn recordElementIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(&self.ensureElemDescriptorIndex(allocator, elem_id).element, index);
    }

    fn updateElementIndex(self: *HostNodeDescriptorStream, elem_id: u64, index: usize) void {
        HostNodeDescriptorStream.updateIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].element, index);
    }

    fn clearElementIndex(self: *HostNodeDescriptorStream, elem_id: u64, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].element, expected);
    }

    fn recordTextNodeIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(&self.ensureElemDescriptorIndex(allocator, elem_id).text_node, index);
    }

    fn updateTextNodeIndex(self: *HostNodeDescriptorStream, elem_id: u64, index: usize) void {
        HostNodeDescriptorStream.updateIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].text_node, index);
    }

    fn clearTextNodeIndex(self: *HostNodeDescriptorStream, elem_id: u64, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].text_node, expected);
    }

    fn recordSignalTextNodeIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(&self.ensureElemDescriptorIndex(allocator, elem_id).signal_text_node, index);
    }

    fn updateSignalTextNodeIndex(self: *HostNodeDescriptorStream, elem_id: u64, index: usize) void {
        HostNodeDescriptorStream.updateIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_text_node, index);
    }

    fn clearSignalTextNodeIndex(self: *HostNodeDescriptorStream, elem_id: u64, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_text_node, expected);
    }

    fn recordStaticTextAttrIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderTextField, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(self.ensureElemDescriptorIndex(allocator, elem_id).static_text_attrs.slot(field), index);
    }

    fn updateStaticTextAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderTextField, index: usize) void {
        HostNodeDescriptorStream.updateIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].static_text_attrs.slot(field), index);
    }

    fn clearStaticTextAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderTextField, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].static_text_attrs.slot(field), expected);
    }

    fn recordSignalTextAttrIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderTextField, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(self.ensureElemDescriptorIndex(allocator, elem_id).signal_text_attrs.slot(field), index);
    }

    fn updateSignalTextAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderTextField, index: usize) void {
        HostNodeDescriptorStream.updateIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_text_attrs.slot(field), index);
    }

    fn clearSignalTextAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderTextField, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_text_attrs.slot(field), expected);
    }

    fn recordStaticBoolAttrIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderBoolField, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(self.ensureElemDescriptorIndex(allocator, elem_id).static_bool_attrs.slot(field), index);
    }

    fn updateStaticBoolAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderBoolField, index: usize) void {
        HostNodeDescriptorStream.updateIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].static_bool_attrs.slot(field), index);
    }

    fn clearStaticBoolAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderBoolField, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].static_bool_attrs.slot(field), expected);
    }

    fn recordSignalBoolAttrIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderBoolField, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(self.ensureElemDescriptorIndex(allocator, elem_id).signal_bool_attrs.slot(field), index);
    }

    fn updateSignalBoolAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderBoolField, index: usize) void {
        HostNodeDescriptorStream.updateIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_bool_attrs.slot(field), index);
    }

    fn clearSignalBoolAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderBoolField, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_bool_attrs.slot(field), expected);
    }

    fn recordEventIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, kind: RenderEventKind, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(self.ensureElemDescriptorIndex(allocator, elem_id).events.slot(kind), index);
    }

    fn updateEventIndex(self: *HostNodeDescriptorStream, elem_id: u64, kind: RenderEventKind, index: usize) void {
        HostNodeDescriptorStream.updateIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].events.slot(kind), index);
    }

    fn clearEventIndex(self: *HostNodeDescriptorStream, elem_id: u64, kind: RenderEventKind, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].events.slot(kind), expected);
    }

    fn deinit(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: *RuntimeMetrics) void {
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
            metrics.closure_releases += 1;
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
            metrics.closure_releases += 1;
            abi.decrefErasedCallable(desc.read, roc_host);
        }
        self.signal_text_attrs.deinit(allocator);

        self.static_bool_attrs.deinit(allocator);

        for (self.signal_bool_attrs.items) |*desc| {
            desc.cached_value.deinit(roc_host, metrics);
            desc.signal.deinit(allocator, roc_host, metrics);
            metrics.closure_releases += 1;
            abi.decrefErasedCallable(desc.read, roc_host);
        }
        self.signal_bool_attrs.deinit(allocator);

        for (self.on_changes.items) |*desc| {
            desc.cached_value.deinit(roc_host, metrics);
            desc.signal.deinit(allocator, roc_host, metrics);
            metrics.closure_releases += 1;
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
                metrics.closure_releases += 1;
                abi.decrefErasedCallable(desc.payload_drop, roc_host);
            }
            if (desc.owns_transform) {
                metrics.closure_releases += 1;
                abi.decrefErasedCallable(desc.transform, roc_host);
            }
        }
        self.events.deinit(allocator);

        for (self.scope_sites.items) |desc| {
            allocator.free(desc.binder_bindings);
        }
        self.scope_sites.deinit(allocator);

        for (self.states.items) |desc| {
            metrics.closure_releases += 3;
            abi.decrefErasedCallable(desc.initial, roc_host);
            abi.decrefErasedCallable(desc.eq, roc_host);
            abi.decrefErasedCallable(desc.drop, roc_host);
        }
        self.states.deinit(allocator);

        for (self.whens.items) |*desc| {
            desc.cached_value.deinit(roc_host, metrics);
            desc.condition.deinit(allocator, roc_host, metrics);
            metrics.closure_releases += 1;
            abi.decrefErasedCallable(desc.read, roc_host);
            abi.decrefElem(desc.when_false, roc_host);
            abi.decrefElem(desc.when_true, roc_host);
        }
        self.whens.deinit(allocator);

        for (self.eaches.items) |*desc| {
            desc.cached_value.deinit(roc_host, metrics);
            desc.items.deinit(allocator, roc_host, metrics);
            metrics.closure_releases += 8;
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

    fn signalRecordByToken(self: *HostNodeDescriptorStream, token: HostSignalToken) ?*HostSignalRecord {
        for (self.signal_records_by_token.items) |entry| {
            if (entry.token == token) return entry.record;
        }
        return null;
    }

    fn rememberSignalRecord(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, record: *HostSignalRecord) void {
        const token = record.token() orelse return;
        if (self.signalRecordByToken(token)) |existing| {
            if (existing != record) failHost("signal token was bound to multiple host records");
            return;
        }
        self.signal_records_by_token.append(allocator, .{
            .token = token,
            .record = record,
        }) catch std.process.exit(1);
    }

    fn rememberSignalRecordTree(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, record: *HostSignalRecord) void {
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

    fn appendElement(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, parent_elem_id: u64, scope_id: u64, tag: []const u8) u64 {
        self.next_elem_id += 1;

        const tag_copy = allocator.dupe(u8, tag) catch std.process.exit(1);
        const element_index = self.elements.items.len;
        self.render_nodes.append(allocator, .{ .elem_id = elem_id, .kind = .element }) catch {
            allocator.free(tag_copy);
            std.process.exit(1);
        };
        self.elements.append(allocator, .{
            .elem_id = elem_id,
            .parent_elem_id = parent_elem_id,
            .scope_id = scope_id,
            .tag = tag_copy,
        }) catch {
            allocator.free(tag_copy);
            std.process.exit(1);
        };
        self.recordElementIndex(allocator, elem_id, element_index);
        return elem_id;
    }

    fn appendTextNode(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, parent_elem_id: u64, scope_id: u64, value: []const u8) void {
        self.next_elem_id += 1;

        const value_copy = allocator.dupe(u8, value) catch std.process.exit(1);
        const text_node_index = self.text_nodes.items.len;
        self.render_nodes.append(allocator, .{ .elem_id = elem_id, .kind = .text }) catch {
            allocator.free(value_copy);
            std.process.exit(1);
        };
        self.text_nodes.append(allocator, .{
            .elem_id = elem_id,
            .parent_elem_id = parent_elem_id,
            .scope_id = scope_id,
            .value = value_copy,
        }) catch {
            allocator.free(value_copy);
            std.process.exit(1);
        };
        self.recordTextNodeIndex(allocator, elem_id, text_node_index);
    }

    fn appendSignalTextNode(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: *RuntimeMetrics, elem_id: u64, parent_elem_id: u64, scope_id: u64, signal: HostSignalBinding, read: abi.RocErasedCallable) void {
        self.next_elem_id += 1;
        self.rememberSignalRecordTree(allocator, signal.record);
        abi.increfErasedCallable(read, 1);
        metrics.closure_retains += 1;
        const signal_text_node_index = self.signal_text_nodes.items.len;

        self.render_nodes.append(allocator, .{ .elem_id = elem_id, .kind = .signal_text }) catch {
            var owned_signal = signal;
            owned_signal.deinit(allocator, roc_host, metrics);
            metrics.closure_releases += 1;
            abi.decrefErasedCallable(read, roc_host);
            std.process.exit(1);
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
            metrics.closure_releases += 1;
            abi.decrefErasedCallable(read, roc_host);
            std.process.exit(1);
        };
        self.recordSignalTextNodeIndex(allocator, elem_id, signal_text_node_index);
    }

    fn appendStaticTextAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderTextField, value: []const u8) void {
        const value_copy = allocator.dupe(u8, value) catch std.process.exit(1);
        const attr_index = self.static_text_attrs.items.len;
        self.static_text_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .value = value_copy,
        }) catch {
            allocator.free(value_copy);
            std.process.exit(1);
        };
        self.recordStaticTextAttrIndex(allocator, elem_id, field, attr_index);
    }

    fn appendSignalTextAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: *RuntimeMetrics, elem_id: u64, field: RenderTextField, signal: HostSignalBinding, read: abi.RocErasedCallable) void {
        self.rememberSignalRecordTree(allocator, signal.record);
        abi.increfErasedCallable(read, 1);
        metrics.closure_retains += 1;
        const attr_index = self.signal_text_attrs.items.len;
        self.signal_text_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .signal = signal,
            .read = read,
        }) catch {
            var owned_signal = signal;
            owned_signal.deinit(allocator, roc_host, metrics);
            metrics.closure_releases += 1;
            abi.decrefErasedCallable(read, roc_host);
            std.process.exit(1);
        };
        self.recordSignalTextAttrIndex(allocator, elem_id, field, attr_index);
    }

    fn appendStaticBoolAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderBoolField, value: bool) void {
        const attr_index = self.static_bool_attrs.items.len;
        self.static_bool_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .value = value,
        }) catch std.process.exit(1);
        self.recordStaticBoolAttrIndex(allocator, elem_id, field, attr_index);
    }

    fn appendSignalBoolAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: *RuntimeMetrics, elem_id: u64, field: RenderBoolField, signal: HostSignalBinding, read: abi.RocErasedCallable) void {
        self.rememberSignalRecordTree(allocator, signal.record);
        abi.increfErasedCallable(read, 1);
        metrics.closure_retains += 1;
        const attr_index = self.signal_bool_attrs.items.len;
        self.signal_bool_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .signal = signal,
            .read = read,
        }) catch {
            var owned_signal = signal;
            owned_signal.deinit(allocator, roc_host, metrics);
            metrics.closure_releases += 1;
            abi.decrefErasedCallable(read, roc_host);
            std.process.exit(1);
        };
        self.recordSignalBoolAttrIndex(allocator, elem_id, field, attr_index);
    }

    fn appendOnChange(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: *RuntimeMetrics, scope_id: u64, signal: HostSignalBinding, to_cmd: abi.RocErasedCallable) void {
        self.rememberSignalRecordTree(allocator, signal.record);
        abi.increfErasedCallable(to_cmd, 1);
        metrics.closure_retains += 1;
        self.on_changes.append(allocator, .{
            .scope_id = scope_id,
            .signal = signal,
            .to_cmd = to_cmd,
        }) catch {
            var owned_signal = signal;
            owned_signal.deinit(allocator, roc_host, metrics);
            metrics.closure_releases += 1;
            abi.decrefErasedCallable(to_cmd, roc_host);
            std.process.exit(1);
        };
    }

    fn appendCleanup(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, scope_id: u64, name: []const u8) void {
        const name_copy = allocator.dupe(u8, name) catch std.process.exit(1);
        self.cleanups.append(allocator, .{
            .scope_id = scope_id,
            .name = name_copy,
        }) catch {
            allocator.free(name_copy);
            std.process.exit(1);
        };
    }

    fn appendEventWithOwnedPayloadTag(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: *RuntimeMetrics, elem_id: u64, kind: RenderEventKind, binder_token: HostBinderToken, target_node_id: u64, payload_kind: EventPayloadKind, payload_tag: HostValueTypeTag, payload_drop: abi.RocErasedCallable, transform: abi.RocErasedCallable) void {
        abi.increfErasedCallable(payload_drop, 1);
        abi.increfErasedCallable(transform, 1);
        metrics.closure_retains += 2;
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
            metrics.closure_releases += 2;
            abi.decrefErasedCallable(payload_drop, roc_host);
            abi.decrefErasedCallable(transform, roc_host);
            std.process.exit(1);
        };
        self.recordEventIndex(allocator, elem_id, kind, event_index);
    }

    fn appendScopeSite(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, node_id: u64, scope_id: u64, ordinal: u64, parent_elem_id: u64, kind: HostNodeScopeSiteKind, binder_bindings: []const HostBinderBinding) void {
        self.appendScopeSiteAt(allocator, node_id, scope_id, ordinal, parent_elem_id, self.render_nodes.items.len, kind, binder_bindings);
    }

    fn appendScopeSiteAt(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, node_id: u64, scope_id: u64, ordinal: u64, parent_elem_id: u64, render_insert_index: usize, kind: HostNodeScopeSiteKind, binder_bindings: []const HostBinderBinding) void {
        const binder_copy = allocator.dupe(HostBinderBinding, binder_bindings) catch std.process.exit(1);
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
            std.process.exit(1);
        };
    }

    fn appendState(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: *RuntimeMetrics, node_id: u64, initial: abi.RocErasedCallable, eq: abi.RocErasedCallable, drop: abi.RocErasedCallable) void {
        abi.increfErasedCallable(initial, 1);
        abi.increfErasedCallable(eq, 1);
        abi.increfErasedCallable(drop, 1);
        metrics.closure_retains += 3;
        self.states.append(allocator, .{
            .node_id = node_id,
            .initial = initial,
            .eq = eq,
            .drop = drop,
        }) catch {
            metrics.closure_releases += 3;
            abi.decrefErasedCallable(initial, roc_host);
            abi.decrefErasedCallable(eq, roc_host);
            abi.decrefErasedCallable(drop, roc_host);
            std.process.exit(1);
        };
    }

    fn appendEventWithBorrowedPayloadTag(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: *RuntimeMetrics, elem_id: u64, kind: RenderEventKind, binder_token: HostBinderToken, target_node_id: u64, payload_kind: EventPayloadKind, payload_tag: HostValueTypeTag, payload_drop: abi.RocErasedCallable, transform: abi.RocErasedCallable) void {
        abi.increfBox(@ptrCast(payload_tag), 1);
        self.appendEventWithOwnedPayloadTag(allocator, roc_host, metrics, elem_id, kind, binder_token, target_node_id, payload_kind, payload_tag, payload_drop, transform);
    }

    fn appendEvent(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: *RuntimeMetrics, elem_id: u64, kind: RenderEventKind, binder_token: HostBinderToken, target_node_id: u64, payload_kind: EventPayloadKind, payload_tag: HostValueTypeTag, payload_drop: abi.RocErasedCallable, transform: abi.RocErasedCallable) void {
        self.appendEventWithBorrowedPayloadTag(allocator, roc_host, metrics, elem_id, kind, binder_token, target_node_id, payload_kind, payload_tag, payload_drop, transform);
    }

    fn appendWhen(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: *RuntimeMetrics, node_id: u64, condition: HostSignalBinding, read: abi.RocErasedCallable, when_false: abi.Elem, when_true: abi.Elem) void {
        self.rememberSignalRecordTree(allocator, condition.record);
        abi.increfErasedCallable(read, 1);
        abi.increfElem(when_false, 1);
        abi.increfElem(when_true, 1);
        metrics.closure_retains += 1;
        self.whens.append(allocator, .{
            .node_id = node_id,
            .condition = condition,
            .read = read,
            .when_false = when_false,
            .when_true = when_true,
        }) catch {
            var owned_condition = condition;
            owned_condition.deinit(allocator, roc_host, metrics);
            metrics.closure_releases += 1;
            abi.decrefErasedCallable(read, roc_host);
            abi.decrefElem(when_false, roc_host);
            abi.decrefElem(when_true, roc_host);
            std.process.exit(1);
        };
    }

    fn appendEach(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: *RuntimeMetrics, node_id: u64, items: HostSignalBinding, items_to_values: abi.RocErasedCallable, key_hash: abi.RocErasedCallable, key_of: abi.RocErasedCallable, key_eq: abi.RocErasedCallable, key_drop: abi.RocErasedCallable, item_eq: abi.RocErasedCallable, item_drop: abi.RocErasedCallable, row: abi.RocErasedCallable) void {
        self.rememberSignalRecordTree(allocator, items.record);
        abi.increfErasedCallable(items_to_values, 1);
        abi.increfErasedCallable(key_hash, 1);
        abi.increfErasedCallable(key_of, 1);
        abi.increfErasedCallable(key_eq, 1);
        abi.increfErasedCallable(key_drop, 1);
        abi.increfErasedCallable(item_eq, 1);
        abi.increfErasedCallable(item_drop, 1);
        abi.increfErasedCallable(row, 1);
        metrics.closure_retains += 8;
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
            metrics.closure_releases += 8;
            abi.decrefErasedCallable(items_to_values, roc_host);
            abi.decrefErasedCallable(key_hash, roc_host);
            abi.decrefErasedCallable(key_of, roc_host);
            abi.decrefErasedCallable(key_eq, roc_host);
            abi.decrefErasedCallable(key_drop, roc_host);
            abi.decrefErasedCallable(item_eq, roc_host);
            abi.decrefErasedCallable(item_drop, roc_host);
            abi.decrefErasedCallable(row, roc_host);
            std.process.exit(1);
        };
    }
};

const HostActiveTextSignalSinkKind = enum {
    text_node,
    text_attr,
};

const HostActiveTextSignalSink = struct {
    kind: HostActiveTextSignalSinkKind,
    index: usize,
};

const HostActiveBoolSignalSink = struct {
    index: usize,
};

const HostActiveChangeSignalSink = struct {
    index: usize,
};

const HostActiveStructuralSignalKind = enum {
    when,
    each,
};

const HostActiveStructuralSignal = struct {
    kind: HostActiveStructuralSignalKind,
    index: usize,
};

const HostDirtyStructuralSignal = struct {
    kind: HostActiveStructuralSignalKind,
    node_id: u64,
    branch: ?HostScopeBranch = null,
};

const HostEachSite = struct {
    parent_scope_id: u64,
    site_ordinal: u64,
};

const HostStructuralReplacementTarget = union(enum) {
    scope: u64,
    each_site: HostEachSite,
};

const HostStructuralPatchTargets = struct {
    removed: HostStructuralReplacementTarget,
    replacement: HostStructuralReplacementTarget,
};

const HostStructuralSplice = struct {
    removed_elem_ids: []u64,
    touched_parent_ids: []u64,
    replacement_elem_ids: []u64,
    replacement_on_change_indices: []usize,

    fn deinit(self: HostStructuralSplice, allocator: std.mem.Allocator) void {
        allocator.free(self.removed_elem_ids);
        allocator.free(self.touched_parent_ids);
        allocator.free(self.replacement_elem_ids);
        allocator.free(self.replacement_on_change_indices);
    }
};

const HostStructuralSpliceAndTargets = struct {
    splice: HostStructuralSplice,
    targets: HostStructuralPatchTargets,
};

const RecomputeApplyOutcome = struct {
    structural_render_required: bool,
};

const HostKeyedRowDiffResult = struct {
    scope_ids: []u64,
    row_items_changed: []bool,
    removed_scope_ids: []u64,
    rows_reused: u64,
    rows_created: u64,
    rows_removed: u64,
    row_items_unchanged: u64,
    row_items_updated: u64,

    fn deinit(self: HostKeyedRowDiffResult, allocator: std.mem.Allocator) void {
        allocator.free(self.scope_ids);
        allocator.free(self.row_items_changed);
        allocator.free(self.removed_scope_ids);
    }
};

const HostEachRowRenderSegment = struct {
    scope_id: u64,
    start: usize,
    len: usize,
};

const HostEachRowRenderMove = struct {
    old_start: usize,
    new_start: usize,
    len: usize,
};

const HostRenderMetrics = struct {
    patches_emitted: u64 = 0,
    reset_dom: u64 = 0,
    create_element: u64 = 0,
    append_child: u64 = 0,
    set_text: u64 = 0,
    set_value: u64 = 0,
    set_checked: u64 = 0,
    set_disabled: u64 = 0,
    set_metadata: u64 = 0,
    bind_event: u64 = 0,

    fn addCommandCounts(self: *HostRenderMetrics, counts: CommandCounts) void {
        self.patches_emitted += counts.total;
        self.reset_dom += counts.reset_dom;
        self.create_element += counts.create_element;
        self.append_child += counts.append_child;
        self.set_text += counts.set_text;
        self.set_value += counts.set_value;
        self.set_checked += counts.set_checked;
        self.set_disabled += counts.set_disabled;
        self.set_metadata += counts.set_metadata;
        self.bind_event += counts.bind_event;
    }
};

const HostDispatchMetrics = struct {
    events_processed: u64 = 0,
    recompute_batches: u64 = 0,
};

pub const std_options: std.Options = .{
    .logFn = std.log.defaultLog,
    .log_level = .warn,
    .allow_stack_tracing = false,
};

pub const panic = std.debug.FullPanic(panicImpl);

fn writeStderr(bytes: []const u8) void {
    std.Io.File.stderr().writeStreamingAll(std.Io.Threaded.global_single_threaded.io(), bytes) catch {};
}

fn writeStdout(bytes: []const u8) void {
    std.Io.File.stdout().writeStreamingAll(std.Io.Threaded.global_single_threaded.io(), bytes) catch {};
}

fn printStdout(comptime fmt: []const u8, args: anytype) void {
    var buf: [2048]u8 = undefined;
    const out = std.fmt.bufPrint(&buf, fmt, args) catch return;
    writeStdout(out);
}

fn benchmarkNowNs() u64 {
    const ns = std.Io.Clock.awake.now(std.Io.Threaded.global_single_threaded.io()).nanoseconds;
    return @intCast(@max(ns, 0));
}

fn panicImpl(msg: []const u8, addr: ?usize) noreturn {
    writeStderr("\n=== PANIC (no stack trace) ===\n");
    writeStderr(msg);
    if (addr) |a| {
        var buf: [32]u8 = undefined;
        const hex = std.fmt.bufPrint(&buf, " at address 0x{x}\n", .{a}) catch "";
        writeStderr(hex);
    } else {
        writeStderr("\n");
    }
    std.process.abort();
}

const STACK_OVERFLOW_MESSAGE = "\nThis Roc application overflowed its stack memory and crashed.\n\n";

fn handleRocStackOverflow() noreturn {
    if (comptime builtin.os.tag == .windows) {
        const DWORD = u32;
        const HANDLE = ?*anyopaque;
        const STD_ERROR_HANDLE: DWORD = @bitCast(@as(i32, -12));

        const kernel32 = struct {
            extern "kernel32" fn GetStdHandle(nStdHandle: DWORD) callconv(.winapi) HANDLE;
            extern "kernel32" fn WriteFile(hFile: HANDLE, lpBuffer: [*]const u8, nNumberOfBytesToWrite: DWORD, lpNumberOfBytesWritten: ?*DWORD, lpOverlapped: ?*anyopaque) callconv(.winapi) i32;
            extern "kernel32" fn ExitProcess(uExitCode: c_uint) callconv(.winapi) noreturn;
        };

        const stderr_handle = kernel32.GetStdHandle(STD_ERROR_HANDLE);
        var bytes_written: DWORD = 0;
        _ = kernel32.WriteFile(stderr_handle, STACK_OVERFLOW_MESSAGE.ptr, STACK_OVERFLOW_MESSAGE.len, &bytes_written, null);
        kernel32.ExitProcess(134);
    } else if (comptime builtin.os.tag != .wasi) {
        writeStderr(STACK_OVERFLOW_MESSAGE);
        std.process.exit(134);
    } else {
        std.process.exit(134);
    }
}

fn handleRocAccessViolation(fault_addr: usize, _: base.signal_handler.AccessViolationContext) noreturn {
    if (comptime builtin.os.tag == .windows) {
        const DWORD = u32;
        const HANDLE = ?*anyopaque;
        const STD_ERROR_HANDLE: DWORD = @bitCast(@as(i32, -12));

        const kernel32 = struct {
            extern "kernel32" fn GetStdHandle(nStdHandle: DWORD) callconv(.winapi) HANDLE;
            extern "kernel32" fn WriteFile(hFile: HANDLE, lpBuffer: [*]const u8, nNumberOfBytesToWrite: DWORD, lpNumberOfBytesWritten: ?*DWORD, lpOverlapped: ?*anyopaque) callconv(.winapi) i32;
            extern "kernel32" fn ExitProcess(uExitCode: c_uint) callconv(.winapi) noreturn;
        };

        var addr_buf: [18]u8 = undefined;
        const addr_str = base.signal_handler.formatHex(fault_addr, &addr_buf);
        const msg1 = "\nSegmentation fault (SIGSEGV) in this Roc program.\nFault address: ";
        const msg2 = "\n\n";
        const stderr_handle = kernel32.GetStdHandle(STD_ERROR_HANDLE);
        var bytes_written: DWORD = 0;
        _ = kernel32.WriteFile(stderr_handle, msg1.ptr, msg1.len, &bytes_written, null);
        _ = kernel32.WriteFile(stderr_handle, addr_str.ptr, @intCast(addr_str.len), &bytes_written, null);
        _ = kernel32.WriteFile(stderr_handle, msg2.ptr, msg2.len, &bytes_written, null);
        kernel32.ExitProcess(139);
    } else {
        writeStderr("\nSegmentation fault (SIGSEGV) in this Roc program.\nFault address: ");
        var addr_buf: [18]u8 = undefined;
        writeStderr(base.signal_handler.formatHex(fault_addr, &addr_buf));
        writeStderr("\n\n");
        std.process.exit(139);
    }
}

const DIVISION_BY_ZERO_MESSAGE = "\nThis Roc application divided by zero and crashed.\n\n";

fn handleRocArithmeticError() noreturn {
    if (comptime builtin.os.tag == .windows) {
        const DWORD = u32;
        const HANDLE = ?*anyopaque;
        const STD_ERROR_HANDLE: DWORD = @bitCast(@as(i32, -12));

        const kernel32 = struct {
            extern "kernel32" fn GetStdHandle(nStdHandle: DWORD) callconv(.winapi) HANDLE;
            extern "kernel32" fn WriteFile(hFile: HANDLE, lpBuffer: [*]const u8, nNumberOfBytesToWrite: DWORD, lpNumberOfBytesWritten: ?*DWORD, lpOverlapped: ?*anyopaque) callconv(.winapi) i32;
            extern "kernel32" fn ExitProcess(uExitCode: c_uint) callconv(.winapi) noreturn;
        };

        const stderr_handle = kernel32.GetStdHandle(STD_ERROR_HANDLE);
        var bytes_written: DWORD = 0;
        _ = kernel32.WriteFile(stderr_handle, DIVISION_BY_ZERO_MESSAGE.ptr, DIVISION_BY_ZERO_MESSAGE.len, &bytes_written, null);
        kernel32.ExitProcess(136);
    } else if (comptime builtin.os.tag != .wasi) {
        writeStderr(DIVISION_BY_ZERO_MESSAGE);
        std.process.exit(136);
    } else {
        std.process.exit(136);
    }
}

const DomElement = struct {
    id: u64,
    tag: []const u8,
    role: ?[]const u8,
    label: ?[]const u8,
    test_id: ?[]const u8,
    text: ?[]const u8,
    value: ?[]const u8,
    checked: bool,
    disabled: bool,
    parent_id: ?u64,
    children: std.ArrayListUnmanaged(u64),
    bound_click_event: ?u64,
    bound_input_event: ?u64,
    bound_check_event: ?u64,
    active: bool,
    text_update_count: u64,
    value_update_count: u64,
    checked_update_count: u64,
    disabled_update_count: u64,

    fn init(id: u64, tag: []const u8) DomElement {
        return .{
            .id = id,
            .tag = tag,
            .role = null,
            .label = null,
            .test_id = null,
            .text = null,
            .value = null,
            .checked = false,
            .disabled = false,
            .parent_id = null,
            .children = .empty,
            .bound_click_event = null,
            .bound_input_event = null,
            .bound_check_event = null,
            .active = true,
            .text_update_count = 0,
            .value_update_count = 0,
            .checked_update_count = 0,
            .disabled_update_count = 0,
        };
    }

    fn deinit(self: *DomElement, allocator: std.mem.Allocator) void {
        allocator.free(self.tag);
        if (self.role) |role| allocator.free(role);
        if (self.label) |label| allocator.free(label);
        if (self.test_id) |test_id| allocator.free(test_id);
        if (self.text) |text| allocator.free(text);
        if (self.value) |value| allocator.free(value);
        self.children.deinit(allocator);
    }
};

const SpecCommandType = enum {
    click,
    fill,
    check,
    uncheck,
    expect_text,
    expect_visible,
    expect_absent,
    expect_value,
    expect_checked,
    expect_disabled,
    expect_updates,
    resolve_task,
    reject_task,
    tick_interval,
    expect_cleanup,
    expect_pending_task,
    expect_interval,
    mark_metrics,
    expect_metric_delta,
};

const LocatorKind = enum {
    none,
    role_name,
    label,
    text,
    test_id,
};

const Locator = struct {
    kind: LocatorKind,
    role: ?[]const u8 = null,
    name: ?[]const u8 = null,
    label: ?[]const u8 = null,
    text: ?[]const u8 = null,
    test_id: ?[]const u8 = null,

    fn deinit(self: Locator, allocator: std.mem.Allocator) void {
        if (self.role) |value| allocator.free(value);
        if (self.name) |value| allocator.free(value);
        if (self.label) |value| allocator.free(value);
        if (self.text) |value| allocator.free(value);
        if (self.test_id) |value| allocator.free(value);
    }
};

fn emptyLocator() Locator {
    return .{ .kind = .none };
}

const SpecCommand = struct {
    cmd_type: SpecCommandType,
    locator: Locator,
    task_name: ?[]const u8 = null,
    interval_ms: ?u64 = null,
    expected_text: ?[]const u8,
    expected_count: ?u64,
    expected_metric_delta: ?i64 = null,
    expected_bool: ?bool,
    line_num: usize,
};

fn freeSpecCommands(allocator: std.mem.Allocator, commands: []SpecCommand) void {
    for (commands) |cmd| {
        cmd.locator.deinit(allocator);
        if (cmd.task_name) |name| allocator.free(name);
        if (cmd.expected_text) |text| allocator.free(text);
    }
    if (commands.len > 0) {
        allocator.free(commands);
    }
}

const TestState = struct {
    verbose: bool,
    commands: []SpecCommand,
    metrics_mark: ?RuntimeMetrics,

    fn init() TestState {
        return .{
            .verbose = false,
            .commands = &.{},
            .metrics_mark = null,
        };
    }
};

const ParseError = error{
    InvalidFormat,
    OutOfMemory,
    FileNotFound,
    IoError,
};

fn parseTestSpecFile(allocator: std.mem.Allocator, file_path: []const u8) ParseError![]SpecCommand {
    const io = std.Io.Threaded.global_single_threaded.io();
    const content = std.Io.Dir.cwd().readFileAlloc(io, file_path, allocator, .limited(1024 * 1024)) catch |err| switch (err) {
        error.FileNotFound => return ParseError.FileNotFound,
        else => return ParseError.IoError,
    };
    defer allocator.free(content);

    return parseTestSpec(allocator, content);
}

const SplitTrailingQuoted = struct {
    head: []const u8,
    quoted: []const u8,
};

fn splitTrailingQuoted(input: []const u8) ParseError!SplitTrailingQuoted {
    const end_quote = std.mem.lastIndexOfScalar(u8, input, '"') orelse return ParseError.InvalidFormat;
    if (end_quote == 0) return ParseError.InvalidFormat;
    const before_end = input[0..end_quote];
    const start_quote = std.mem.lastIndexOfScalar(u8, before_end, '"') orelse return ParseError.InvalidFormat;
    const tail = std.mem.trim(u8, input[end_quote + 1 ..], " \t");
    if (tail.len != 0) return ParseError.InvalidFormat;
    return .{
        .head = std.mem.trim(u8, input[0..start_quote], " \t"),
        .quoted = input[start_quote + 1 .. end_quote],
    };
}

fn splitTrailingToken(input: []const u8) ParseError!struct { head: []const u8, token: []const u8 } {
    const trimmed = std.mem.trim(u8, input, " \t");
    const space_idx = std.mem.lastIndexOfAny(u8, trimmed, " \t") orelse return ParseError.InvalidFormat;
    return .{
        .head = std.mem.trim(u8, trimmed[0..space_idx], " \t"),
        .token = std.mem.trim(u8, trimmed[space_idx + 1 ..], " \t"),
    };
}

fn parseSingleQuoted(input: []const u8) ParseError![]const u8 {
    const trimmed = std.mem.trim(u8, input, " \t");
    if (trimmed.len < 2 or trimmed[0] != '"' or trimmed[trimmed.len - 1] != '"') return ParseError.InvalidFormat;
    return trimmed[1 .. trimmed.len - 1];
}

fn splitTwoQuoted(input: []const u8) ParseError!struct { first: []const u8, second: []const u8 } {
    const trimmed = std.mem.trim(u8, input, " \t");
    if (trimmed.len < 5 or trimmed[0] != '"') return ParseError.InvalidFormat;
    const first_end = std.mem.indexOfScalarPos(u8, trimmed, 1, '"') orelse return ParseError.InvalidFormat;
    const rest = std.mem.trim(u8, trimmed[first_end + 1 ..], " \t");
    if (rest.len < 2 or rest[0] != '"' or rest[rest.len - 1] != '"') return ParseError.InvalidFormat;
    return .{
        .first = trimmed[1..first_end],
        .second = rest[1 .. rest.len - 1],
    };
}

fn parseQuotedValue(allocator: std.mem.Allocator, prefix: []const u8, input: []const u8) ParseError!?[]const u8 {
    if (!std.mem.startsWith(u8, input, prefix)) return null;
    const rest = std.mem.trim(u8, input[prefix.len..], " \t");
    if (rest.len < 2 or rest[0] != '"' or rest[rest.len - 1] != '"') return ParseError.InvalidFormat;
    return allocator.dupe(u8, rest[1 .. rest.len - 1]) catch ParseError.OutOfMemory;
}

fn parseLocator(allocator: std.mem.Allocator, input: []const u8) ParseError!Locator {
    const trimmed = std.mem.trim(u8, input, " \t");
    if (trimmed.len == 0) return ParseError.InvalidFormat;

    if (std.mem.startsWith(u8, trimmed, "role:")) {
        const rest = trimmed["role:".len..];
        const space_idx = std.mem.indexOfAny(u8, rest, " \t") orelse return ParseError.InvalidFormat;
        const role = rest[0..space_idx];
        const name_part = std.mem.trim(u8, rest[space_idx + 1 ..], " \t");
        const name = (try parseQuotedValue(allocator, "name:", name_part)) orelse return ParseError.InvalidFormat;
        const role_copy = allocator.dupe(u8, role) catch return ParseError.OutOfMemory;
        return .{
            .kind = .role_name,
            .role = role_copy,
            .name = name,
        };
    }

    if ((try parseQuotedValue(allocator, "label:", trimmed))) |value| return .{ .kind = .label, .label = value };
    if ((try parseQuotedValue(allocator, "text:", trimmed))) |value| return .{ .kind = .text, .text = value };
    if ((try parseQuotedValue(allocator, "test_id:", trimmed))) |value| return .{ .kind = .test_id, .test_id = value };

    return ParseError.InvalidFormat;
}

fn appendSpecCommand(
    commands: *std.ArrayListUnmanaged(SpecCommand),
    allocator: std.mem.Allocator,
    cmd_type: SpecCommandType,
    locator: Locator,
    expected_text: ?[]const u8,
    expected_count: ?u64,
    expected_bool: ?bool,
    line_num: usize,
) ParseError!void {
    commands.append(allocator, .{
        .cmd_type = cmd_type,
        .locator = locator,
        .expected_text = expected_text,
        .expected_count = expected_count,
        .expected_metric_delta = null,
        .expected_bool = expected_bool,
        .line_num = line_num,
    }) catch return ParseError.OutOfMemory;
}

fn parseBoolToken(token: []const u8) ParseError!bool {
    if (std.mem.eql(u8, token, "true")) return true;
    if (std.mem.eql(u8, token, "false")) return false;
    return ParseError.InvalidFormat;
}

fn parseTestSpec(allocator: std.mem.Allocator, content: []const u8) ParseError![]SpecCommand {
    var commands: std.ArrayListUnmanaged(SpecCommand) = .empty;
    errdefer commands.deinit(allocator);

    var line_num: usize = 0;
    var lines = std.mem.splitScalar(u8, content, '\n');

    while (lines.next()) |line| {
        line_num += 1;
        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (trimmed.len == 0 or trimmed[0] == '#') continue;

        if (std.mem.startsWith(u8, trimmed, "click ")) {
            try appendSpecCommand(&commands, allocator, .click, try parseLocator(allocator, trimmed[6..]), null, null, null, line_num);
        } else if (std.mem.eql(u8, trimmed, "mark_metrics")) {
            try appendSpecCommand(&commands, allocator, .mark_metrics, emptyLocator(), null, null, null, line_num);
        } else if (std.mem.startsWith(u8, trimmed, "fill ")) {
            const split = try splitTrailingQuoted(trimmed[5..]);
            const value_copy = allocator.dupe(u8, split.quoted) catch return ParseError.OutOfMemory;
            try appendSpecCommand(&commands, allocator, .fill, try parseLocator(allocator, split.head), value_copy, null, null, line_num);
        } else if (std.mem.startsWith(u8, trimmed, "check ")) {
            try appendSpecCommand(&commands, allocator, .check, try parseLocator(allocator, trimmed[6..]), null, null, null, line_num);
        } else if (std.mem.startsWith(u8, trimmed, "uncheck ")) {
            try appendSpecCommand(&commands, allocator, .uncheck, try parseLocator(allocator, trimmed[8..]), null, null, null, line_num);
        } else if (std.mem.startsWith(u8, trimmed, "expect_text ")) {
            const split = try splitTrailingQuoted(trimmed[12..]);
            const text_copy = allocator.dupe(u8, split.quoted) catch return ParseError.OutOfMemory;
            try appendSpecCommand(&commands, allocator, .expect_text, try parseLocator(allocator, split.head), text_copy, null, null, line_num);
        } else if (std.mem.startsWith(u8, trimmed, "expect_visible ")) {
            try appendSpecCommand(&commands, allocator, .expect_visible, try parseLocator(allocator, trimmed[15..]), null, null, null, line_num);
        } else if (std.mem.startsWith(u8, trimmed, "expect_absent ")) {
            try appendSpecCommand(&commands, allocator, .expect_absent, try parseLocator(allocator, trimmed[14..]), null, null, null, line_num);
        } else if (std.mem.startsWith(u8, trimmed, "expect_value ")) {
            const split = try splitTrailingQuoted(trimmed[13..]);
            const value_copy = allocator.dupe(u8, split.quoted) catch return ParseError.OutOfMemory;
            try appendSpecCommand(&commands, allocator, .expect_value, try parseLocator(allocator, split.head), value_copy, null, null, line_num);
        } else if (std.mem.startsWith(u8, trimmed, "expect_checked ")) {
            const split = try splitTrailingToken(trimmed[15..]);
            try appendSpecCommand(&commands, allocator, .expect_checked, try parseLocator(allocator, split.head), null, null, try parseBoolToken(split.token), line_num);
        } else if (std.mem.startsWith(u8, trimmed, "expect_disabled ")) {
            const split = try splitTrailingToken(trimmed[16..]);
            try appendSpecCommand(&commands, allocator, .expect_disabled, try parseLocator(allocator, split.head), null, null, try parseBoolToken(split.token), line_num);
        } else if (std.mem.startsWith(u8, trimmed, "expect_updates ")) {
            const split = try splitTrailingToken(trimmed[15..]);
            const expected_count = std.fmt.parseInt(u64, split.token, 10) catch return ParseError.InvalidFormat;
            try appendSpecCommand(&commands, allocator, .expect_updates, try parseLocator(allocator, split.head), null, expected_count, null, line_num);
        } else if (std.mem.startsWith(u8, trimmed, "resolve_task ")) {
            const split = try splitTwoQuoted(trimmed["resolve_task ".len..]);
            const task_name = allocator.dupe(u8, split.first) catch return ParseError.OutOfMemory;
            errdefer allocator.free(task_name);
            const payload = allocator.dupe(u8, split.second) catch return ParseError.OutOfMemory;
            errdefer allocator.free(payload);
            try appendSpecCommand(&commands, allocator, .resolve_task, emptyLocator(), payload, null, null, line_num);
            commands.items[commands.items.len - 1].task_name = task_name;
        } else if (std.mem.startsWith(u8, trimmed, "reject_task ")) {
            const split = try splitTwoQuoted(trimmed["reject_task ".len..]);
            const task_name = allocator.dupe(u8, split.first) catch return ParseError.OutOfMemory;
            errdefer allocator.free(task_name);
            const payload = allocator.dupe(u8, split.second) catch return ParseError.OutOfMemory;
            errdefer allocator.free(payload);
            try appendSpecCommand(&commands, allocator, .reject_task, emptyLocator(), payload, null, null, line_num);
            commands.items[commands.items.len - 1].task_name = task_name;
        } else if (std.mem.startsWith(u8, trimmed, "tick_interval ")) {
            const period_text = std.mem.trim(u8, trimmed["tick_interval ".len..], " \t");
            const period_ms = std.fmt.parseInt(u64, period_text, 10) catch return ParseError.InvalidFormat;
            try appendSpecCommand(&commands, allocator, .tick_interval, emptyLocator(), null, null, null, line_num);
            commands.items[commands.items.len - 1].interval_ms = period_ms;
        } else if (std.mem.startsWith(u8, trimmed, "expect_cleanup ")) {
            const split = try splitTrailingToken(trimmed["expect_cleanup ".len..]);
            const name_value = try parseSingleQuoted(split.head);
            const task_name = allocator.dupe(u8, name_value) catch return ParseError.OutOfMemory;
            errdefer allocator.free(task_name);
            const expected_count = std.fmt.parseInt(u64, split.token, 10) catch return ParseError.InvalidFormat;
            try appendSpecCommand(&commands, allocator, .expect_cleanup, emptyLocator(), null, expected_count, null, line_num);
            commands.items[commands.items.len - 1].task_name = task_name;
        } else if (std.mem.startsWith(u8, trimmed, "expect_pending_task ")) {
            const split = try splitTrailingToken(trimmed["expect_pending_task ".len..]);
            const name_value = try parseSingleQuoted(split.head);
            const task_name = allocator.dupe(u8, name_value) catch return ParseError.OutOfMemory;
            errdefer allocator.free(task_name);
            const expected_count = std.fmt.parseInt(u64, split.token, 10) catch return ParseError.InvalidFormat;
            try appendSpecCommand(&commands, allocator, .expect_pending_task, emptyLocator(), null, expected_count, null, line_num);
            commands.items[commands.items.len - 1].task_name = task_name;
        } else if (std.mem.startsWith(u8, trimmed, "expect_interval ")) {
            const split = try splitTrailingToken(trimmed["expect_interval ".len..]);
            const period_ms = std.fmt.parseInt(u64, split.head, 10) catch return ParseError.InvalidFormat;
            const expected_count = std.fmt.parseInt(u64, split.token, 10) catch return ParseError.InvalidFormat;
            try appendSpecCommand(&commands, allocator, .expect_interval, emptyLocator(), null, expected_count, null, line_num);
            commands.items[commands.items.len - 1].interval_ms = period_ms;
        } else if (std.mem.startsWith(u8, trimmed, "expect_metric_delta ")) {
            const split = try splitTrailingToken(trimmed[20..]);
            const metric_name = allocator.dupe(u8, split.head) catch return ParseError.OutOfMemory;
            const expected_delta = std.fmt.parseInt(i64, split.token, 10) catch return ParseError.InvalidFormat;
            try appendSpecCommand(&commands, allocator, .expect_metric_delta, emptyLocator(), metric_name, null, null, line_num);
            commands.items[commands.items.len - 1].expected_metric_delta = expected_delta;
        } else {
            return ParseError.InvalidFormat;
        }
    }

    return commands.toOwnedSlice(allocator) catch ParseError.OutOfMemory;
}

fn u64SliceContains(items: []const u64, target: u64) bool {
    for (items) |item| {
        if (item == target) return true;
    }
    return false;
}

const RocAllocation = struct {
    ptr: [*]u8,
    total_size: usize,
    alignment: std.mem.Alignment,
};

const RocAllocationHeader = extern struct {
    total_size: usize,
    ledger_index: usize,
};

const HostValueRegistryCell = if (host_value_type_tags_enabled) struct {
    box: abi.RocBox,
    tag: ?HostValueTypeTag,
} else struct {
    box: abi.RocBox,
};

const HostValueRegistrySlot = union(enum) {
    vacant,
    occupied: HostValueRegistryCell,
};

const TestHostValueKind = enum {
    unit,
    i64,
    bool,
    str,
    i64_list,
};

const HostEnv = struct {
    gpa: std.heap.DebugAllocator(.{ .safety = true }),
    test_state: TestState,
    roc_allocations: std.ArrayListUnmanaged(RocAllocation) = .empty,
    host_values: std.ArrayListUnmanaged(HostValueRegistrySlot) = .empty,
    test_host_value_kinds: std.ArrayListUnmanaged(?TestHostValueKind) = .empty,
    alloc_count: usize = 0,
    dealloc_count: usize = 0,
    dom_elements: std.ArrayListUnmanaged(DomElement) = .empty,
    active_events: std.ArrayListUnmanaged(HostActiveEventDesc) = .empty,
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
    render_metrics: HostRenderMetrics = .{},
    dispatch_metrics: HostDispatchMetrics = .{},
    next_elem_id: u64 = 0,
    roc_host: ?*abi.RocHost = null,
    root_elem: ?abi.Elem = null,
    last_runtime_metrics: RuntimeMetrics = zeroRuntimeMetrics(),
    pending_roc_metrics: RuntimeMetrics = zeroRuntimeMetrics(),
    dirty_signal_generation: u64 = 0,

    fn init() HostEnv {
        return .{
            .gpa = std.heap.DebugAllocator(.{ .safety = true }){},
            .test_state = TestState.init(),
        };
    }

    fn activeRocHost(self: *HostEnv) *abi.RocHost {
        return self.roc_host orelse failHost("HostValue type tag release requires an active Roc host");
    }

    fn hostValueRegistryCell(box: abi.RocBox, owned_tag: ?HostValueTypeTag) HostValueRegistryCell {
        if (comptime host_value_type_tags_enabled) {
            return .{ .box = box, .tag = owned_tag };
        } else {
            return .{ .box = box };
        }
    }

    fn releaseOwnedHostValueTypeTag(self: *HostEnv, tag: HostValueTypeTag) void {
        abi.decrefBox(@ptrCast(tag), self.activeRocHost());
    }

    fn releaseRegistryHostValueTag(self: *HostEnv, cell: HostValueRegistryCell) void {
        if (comptime host_value_type_tags_enabled) {
            if (cell.tag) |tag| self.releaseOwnedHostValueTypeTag(tag);
        }
    }

    fn retainHostValueTypeTag(tag: HostValueTypeTag) void {
        abi.increfBox(@ptrCast(tag), 1);
    }

    fn hostValueTypeTagId(tag: HostValueTypeTag) u64 {
        const payload: *const u64 = @ptrCast(@alignCast(tag));
        return payload.*;
    }

    fn hostValueTypeTagsMatch(actual_tag: HostValueTypeTag, expected_tag: HostValueTypeTag) bool {
        if (actual_tag == expected_tag) return true;
        const actual_id = HostEnv.hostValueTypeTagId(actual_tag);
        return actual_id != 0 and actual_id == HostEnv.hostValueTypeTagId(expected_tag);
    }

    fn storeHostValueWithOwnedTag(self: *HostEnv, box: abi.RocBox, owned_tag: ?HostValueTypeTag) HostValue {
        const allocator = self.gpa.allocator();
        const registry_tag: ?HostValueTypeTag = if (comptime host_value_type_tags_enabled) owned_tag else blk: {
            if (owned_tag) |tag| self.releaseOwnedHostValueTypeTag(tag);
            break :blk null;
        };
        for (self.host_values.items, 0..) |*slot, index| {
            switch (slot.*) {
                .vacant => {
                    slot.* = .{ .occupied = HostEnv.hostValueRegistryCell(box, registry_tag) };
                    if (builtin.is_test) self.test_host_value_kinds.items[index] = null;
                    return @intCast(index + 1);
                },
                .occupied => {},
            }
        }
        self.host_values.append(allocator, .{ .occupied = HostEnv.hostValueRegistryCell(box, registry_tag) }) catch std.process.exit(1);
        if (builtin.is_test) {
            self.test_host_value_kinds.append(allocator, null) catch std.process.exit(1);
        }
        return @intCast(self.host_values.items.len);
    }

    fn storeHostValueWithRetainedTag(self: *HostEnv, box: abi.RocBox, borrowed_tag: ?HostValueTypeTag) HostValue {
        if (comptime host_value_type_tags_enabled) {
            if (borrowed_tag) |tag| HostEnv.retainHostValueTypeTag(tag);
            return self.storeHostValueWithOwnedTag(box, borrowed_tag);
        } else {
            return self.storeHostValueWithOwnedTag(box, null);
        }
    }

    fn storeHostValue(self: *HostEnv, box: abi.RocBox) HostValue {
        return self.storeHostValueWithOwnedTag(box, null);
    }

    fn storeTaggedHostValue(self: *HostEnv, box: abi.RocBox, owned_tag: HostValueTypeTag) HostValue {
        return self.storeHostValueWithOwnedTag(box, owned_tag);
    }

    fn setTestHostValueKind(self: *HostEnv, value: HostValue, kind: TestHostValueKind) void {
        if (!builtin.is_test) @compileError("setTestHostValueKind is test-only");
        const index = value - 1;
        if (index >= self.test_host_value_kinds.items.len) failHost("test HostValue kind referenced an unknown value");
        self.test_host_value_kinds.items[@intCast(index)] = kind;
    }

    fn testHostValueKind(self: *HostEnv, value: HostValue) TestHostValueKind {
        if (!builtin.is_test) @compileError("testHostValueKind is test-only");
        const index = value - 1;
        if (index >= self.test_host_value_kinds.items.len) failHost("test HostValue kind referenced an unknown value");
        return self.test_host_value_kinds.items[@intCast(index)] orelse @panic("test HostValue kind was not recorded");
    }

    fn hostValueSlot(self: *HostEnv, value: HostValue) *HostValueRegistrySlot {
        if (value == 0) failHost("HostValue handle 0 is invalid");
        const index = value - 1;
        if (index >= self.host_values.items.len) failHost("HostValue handle referenced an unknown value");
        return &self.host_values.items[@intCast(index)];
    }

    fn getHostValue(self: *HostEnv, value: HostValue) abi.RocBox {
        const slot = self.hostValueSlot(value);
        return switch (slot.*) {
            .vacant => failHost("HostValue handle referenced a released value"),
            .occupied => |cell| blk: {
                abi.increfBox(cell.box, 1);
                break :blk cell.box;
            },
        };
    }

    fn hostValueTypeTag(self: *HostEnv, value: HostValue) ?HostValueTypeTag {
        if (comptime host_value_type_tags_enabled) {
            const slot = self.hostValueSlot(value);
            return switch (slot.*) {
                .vacant => failHost("HostValue handle referenced a released value"),
                .occupied => |cell| cell.tag,
            };
        } else {
            return null;
        }
    }

    fn assertHostValueTypeTag(self: *HostEnv, value: HostValue, expected_tag: HostValueTypeTag) void {
        if (comptime host_value_type_tags_enabled) {
            const actual_tag = self.hostValueTypeTag(value) orelse failHost("HostValue read crossed erasure boundary without a type tag");
            if (!HostEnv.hostValueTypeTagsMatch(actual_tag, expected_tag)) {
                var buf: [192]u8 = undefined;
                const message = std.fmt.bufPrint(
                    &buf,
                    "HostValue read crossed erasure boundary with the wrong type tag: value={} actual=0x{x} expected=0x{x}",
                    .{ value, @intFromPtr(actual_tag), @intFromPtr(expected_tag) },
                ) catch "HostValue read crossed erasure boundary with the wrong type tag";
                failHost(message);
            }
        }
    }

    fn setHostValueTypeTag(self: *HostEnv, value: HostValue, borrowed_tag: HostValueTypeTag) void {
        if (comptime host_value_type_tags_enabled) {
            const slot = self.hostValueSlot(value);
            switch (slot.*) {
                .vacant => failHost("HostValue handle referenced a released value"),
                .occupied => |*cell| {
                    if (cell.tag) |actual_tag| {
                        if (!HostEnv.hostValueTypeTagsMatch(actual_tag, borrowed_tag)) failHost("HostValue was tagged with a conflicting type tag");
                        return;
                    }
                    HostEnv.retainHostValueTypeTag(borrowed_tag);
                    cell.tag = borrowed_tag;
                },
            }
        }
    }

    fn getTaggedHostValue(self: *HostEnv, value: HostValue, tag: HostValueTypeTag) abi.RocBox {
        self.assertHostValueTypeTag(value, tag);
        return self.getHostValue(value);
    }

    fn takeHostValue(self: *HostEnv, value: HostValue) abi.RocBox {
        const slot = self.hostValueSlot(value);
        return switch (slot.*) {
            .vacant => failHost("HostValue handle referenced a released value"),
            .occupied => |cell| blk: {
                slot.* = .vacant;
                self.releaseRegistryHostValueTag(cell);
                if (builtin.is_test) self.test_host_value_kinds.items[@intCast(value - 1)] = null;
                break :blk cell.box;
            },
        };
    }

    fn takeTaggedHostValue(self: *HostEnv, value: HostValue, tag: HostValueTypeTag) abi.RocBox {
        self.assertHostValueTypeTag(value, tag);
        return self.takeHostValue(value);
    }

    fn cloneHostValue(self: *HostEnv, value: HostValue) HostValue {
        const tag = self.hostValueTypeTag(value);
        const box = self.getHostValue(value);
        const cloned = self.storeHostValueWithRetainedTag(box, tag);
        if (builtin.is_test) {
            self.setTestHostValueKind(cloned, self.testHostValueKind(value));
        }
        return cloned;
    }

    fn nextDirtySignalGeneration(self: *HostEnv) u64 {
        if (self.dirty_signal_generation == std.math.maxInt(u64)) {
            failHost("host dirty signal generation overflowed");
        }
        self.dirty_signal_generation += 1;
        return self.dirty_signal_generation;
    }

    fn eventPayloadKindFromAbi(payload_kind: u64) EventPayloadKind {
        return switch (payload_kind) {
            @intFromEnum(EventPayloadKind.unit) => .unit,
            @intFromEnum(EventPayloadKind.str) => .str,
            @intFromEnum(EventPayloadKind.bool) => .bool,
            else => failHost("Roc event descriptor used an unknown payload kind"),
        };
    }

    fn clearEventDescriptors(self: *HostEnv) void {
        self.event_descriptors.items.len = 0;
    }

    fn clearActiveEvents(self: *HostEnv) void {
        for (self.active_events.items) |desc| {
            abi.decrefBox(@ptrCast(desc.payload_tag), self.roc_host.?);
            abi.decrefErasedCallable(desc.payload_drop, self.roc_host.?);
            abi.decrefErasedCallable(desc.transform, self.roc_host.?);
            self.pending_roc_metrics.closure_releases += 2;
        }
        self.active_events.items.len = 0;
    }

    fn rebuildActiveEventsFromStream(self: *HostEnv, stream: *HostNodeDescriptorStream) void {
        const allocator = self.gpa.allocator();
        self.clearActiveEvents();

        for (stream.events.items) |*desc| {
            if (!desc.owns_payload_tag) failHost("event descriptor payload tag ownership was already transferred");
            if (!desc.owns_payload_drop) failHost("event descriptor payload drop ownership was already transferred");
            if (!desc.owns_transform) failHost("event descriptor transform ownership was already transferred");
            self.active_events.append(allocator, .{
                .target_node_id = desc.target_node_id,
                .payload_kind = desc.payload_kind,
                .payload_tag = desc.payload_tag,
                .payload_drop = desc.payload_drop,
                .transform = desc.transform,
            }) catch std.process.exit(1);
            desc.owns_payload_tag = false;
            desc.owns_payload_drop = false;
            desc.owns_transform = false;
        }
    }

    fn clearSignalEventRoutes(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
        for (self.signal_event_routes.items) |route| {
            allocator.free(route.signal_ids);
        }
        self.signal_event_routes.items.len = 0;
    }

    fn rebuildSignalEventRoutesFromSignals(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
        var route_lists = allocator.alloc(std.ArrayListUnmanaged(u64), self.event_descriptors.items.len) catch std.process.exit(1);
        defer allocator.free(route_lists);

        for (route_lists) |*list| {
            list.* = .empty;
        }
        errdefer {
            for (route_lists) |*list| {
                list.deinit(allocator);
            }
        }

        for (self.signal_descriptors.items) |signal| {
            if (signal.kind != .source) continue;
            for (signal.source_event_ids) |event_id| {
                if (event_id == 0 or event_id > self.event_descriptors.items.len) {
                    failHost("host source signal registry contains an unknown event id");
                }
                route_lists[@intCast(event_id - 1)].append(allocator, signal.signal_id) catch std.process.exit(1);
            }
        }

        self.clearSignalEventRoutes();

        for (route_lists, 0..) |*route_list, index| {
            const signal_ids = route_list.toOwnedSlice(allocator) catch std.process.exit(1);
            self.signal_event_routes.append(allocator, .{
                .event_id = @intCast(index + 1),
                .signal_ids = signal_ids,
            }) catch {
                allocator.free(signal_ids);
                std.process.exit(1);
            };
        }
    }

    fn sourceSignalIdsForEvent(self: *HostEnv, event_id: u64) []const u64 {
        if (event_id == 0) failHost("event id 0 is not registered");

        const route_index = event_id - 1;
        if (route_index >= self.signal_event_routes.items.len) failHost("event id has no host source signal route descriptor");

        const route = self.signal_event_routes.items[@intCast(route_index)];
        if (route.event_id != event_id) failHost("host signal event route table is not indexed by event id");
        return route.signal_ids;
    }

    fn eventPayloadKind(self: *HostEnv, event_id: u64) EventPayloadKind {
        if (event_id == 0) failHost("event id 0 is not registered");

        const event_index = event_id - 1;
        if (event_index >= self.event_descriptors.items.len) failHost("event id has no host event descriptor");

        const descriptor = self.event_descriptors.items[@intCast(event_index)];
        if (descriptor.event_id != event_id) failHost("host event descriptor table is not indexed by event id");
        return descriptor.payload_kind;
    }

    fn validateEventPayload(self: *HostEnv, event_id: u64, expected_payload_kind: EventPayloadKind) void {
        _ = self.sourceSignalIdsForEvent(event_id);
        if (self.eventPayloadKind(event_id) != expected_payload_kind) {
            failHost("DOM binding payload kind does not match Roc event descriptor");
        }
    }

    fn signalKindFromAbi(kind: u64) SignalKind {
        return switch (kind) {
            @intFromEnum(SignalKind.source) => .source,
            @intFromEnum(SignalKind.map) => .map,
            @intFromEnum(SignalKind.map2) => .map2,
            else => failHost("Roc signal descriptor used an unknown signal kind"),
        };
    }

    fn renderTextFieldFromAbi(field: u64) RenderTextField {
        return switch (field) {
            @intFromEnum(RenderTextField.text) => .text,
            @intFromEnum(RenderTextField.role) => .role,
            @intFromEnum(RenderTextField.label) => .label,
            @intFromEnum(RenderTextField.test_id) => .test_id,
            @intFromEnum(RenderTextField.value) => .value,
            else => failHost("Roc render text descriptor used an unknown field"),
        };
    }

    fn renderBoolFieldFromAbi(field: u64) RenderBoolField {
        return switch (field) {
            @intFromEnum(RenderBoolField.checked) => .checked,
            @intFromEnum(RenderBoolField.disabled) => .disabled,
            else => failHost("Roc render bool descriptor used an unknown field"),
        };
    }

    fn renderEventKindFromAbi(kind: u64) RenderEventKind {
        return switch (kind) {
            @intFromEnum(RenderEventKind.click) => .click,
            @intFromEnum(RenderEventKind.input) => .input,
            @intFromEnum(RenderEventKind.check) => .check,
            else => failHost("Roc render event descriptor used an unknown event kind"),
        };
    }

    fn validateSignalSourceStateIds(self: *HostEnv, kind: SignalKind, source_state_ids: []const u64) void {
        switch (kind) {
            .source => {
                if (source_state_ids.len != 1) {
                    failHost("Roc source signal descriptor must have exactly one source state id");
                }
            },
            .map, .map2 => {
                if (source_state_ids.len != 0) {
                    failHost("Roc derived signal descriptor must not have source state ids");
                }
            },
        }

        for (source_state_ids, 0..) |state_id, index| {
            if (state_id >= self.states.items.len) {
                failHost("Roc signal descriptor referenced an unknown source state id");
            }

            for (source_state_ids[0..index]) |previous| {
                if (previous == state_id) {
                    failHost("Roc signal descriptor must not contain duplicate source state ids");
                }
            }
        }
    }

    fn validateSignalInputIds(self: *HostEnv, signal_id: u64, kind: SignalKind, input_signal_ids: []const u64) void {
        _ = self;
        switch (kind) {
            .source => {
                if (input_signal_ids.len != 0) {
                    failHost("Roc source signal descriptor must not have input signal ids");
                }
            },
            .map => {
                if (input_signal_ids.len != 1) {
                    failHost("Roc map signal descriptor must have exactly one input signal id");
                }
            },
            .map2 => {
                if (input_signal_ids.len != 2) {
                    failHost("Roc map2 signal descriptor must have exactly two input signal ids");
                }
            },
        }

        for (input_signal_ids) |input_signal_id| {
            if (input_signal_id >= signal_id) {
                failHost("Roc signal descriptor input ids must reference prior signal ids");
            }
        }
    }

    fn clearSignalDescriptors(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
        for (self.signal_descriptors.items) |descriptor| {
            allocator.free(descriptor.source_state_ids);
            allocator.free(descriptor.source_event_ids);
            allocator.free(descriptor.input_signal_ids);
        }
        self.signal_descriptors.items.len = 0;
    }

    fn clearSignalRoutes(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
        for (self.signal_routes.items) |route| {
            allocator.free(route.signal_ids);
        }
        self.signal_routes.items.len = 0;
    }

    fn clearSignalDependents(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
        for (self.signal_dependents.items) |route| {
            allocator.free(route.signal_ids);
        }
        self.signal_dependents.items.len = 0;
    }

    fn clearSignalCache(self: *HostEnv) void {
        for (self.signal_cache.items) |*slot| {
            slot.deinit(self.roc_host.?, &self.pending_roc_metrics);
        }
        self.signal_cache.items.len = 0;
    }

    fn clearActiveSignalGraph(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
        const roc_host = self.roc_host orelse {
            if (self.active_signal_graph.items.len != 0) failHost("active signal graph cannot release records without a Roc host");
            self.active_signal_graph.items.len = 0;
            return;
        };
        for (self.active_signal_graph.items, 0..) |node, index| {
            allocator.free(node.dependents);
            const active_graph_id = node.record.active_graph_id orelse failHost("active signal graph record was missing its dense id");
            if (active_graph_id != index) failHost("active signal graph record dense id did not match its slot");
            node.record.active_graph_id = null;
            node.record.active_use_count = 0;
            node.record.release(allocator, roc_host, &self.pending_roc_metrics);
        }
        self.active_signal_graph.items.len = 0;
    }

    fn clearActiveSignalRoutes(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
        for (self.active_source_signal_routes.items) |*route| {
            route.deinit(allocator);
        }
        self.active_source_signal_routes.items.len = 0;

        for (self.active_text_signal_routes.items) |*route| {
            route.deinit(allocator);
        }
        self.active_text_signal_routes.items.len = 0;

        for (self.active_bool_signal_routes.items) |*route| {
            route.deinit(allocator);
        }
        self.active_bool_signal_routes.items.len = 0;

        for (self.active_change_signal_routes.items) |*route| {
            route.deinit(allocator);
        }
        self.active_change_signal_routes.items.len = 0;

        for (self.active_structural_signal_routes.items) |*route| {
            route.deinit(allocator);
        }
        self.active_structural_signal_routes.items.len = 0;
    }

    fn clearActiveSinkSignalRoutes(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
        for (self.active_text_signal_routes.items) |*route| {
            route.deinit(allocator);
        }
        self.active_text_signal_routes.items.len = 0;

        for (self.active_bool_signal_routes.items) |*route| {
            route.deinit(allocator);
        }
        self.active_bool_signal_routes.items.len = 0;

        for (self.active_change_signal_routes.items) |*route| {
            route.deinit(allocator);
        }
        self.active_change_signal_routes.items.len = 0;

        for (self.active_structural_signal_routes.items) |*route| {
            route.deinit(allocator);
        }
        self.active_structural_signal_routes.items.len = 0;
    }

    fn activeSignalRecordId(self: *HostEnv, record: *const HostSignalRecord) ?u64 {
        const record_id = record.active_graph_id orelse return null;
        if (record_id >= self.active_signal_graph.items.len) failHost("active signal record dense id exceeded the graph table");
        if (self.active_signal_graph.items[@intCast(record_id)].record != record) {
            failHost("active signal record dense id pointed at a different record");
        }
        return record_id;
    }

    fn requireActiveSignalRecordId(self: *HostEnv, record: *const HostSignalRecord) u64 {
        return self.activeSignalRecordId(record) orelse failHost("active signal graph referenced a record that was not registered");
    }

    fn appendActiveSignalGraphNode(self: *HostEnv, record: *HostSignalRecord, rank: u64) u64 {
        const allocator = self.gpa.allocator();
        const record_id: u64 = @intCast(self.active_signal_graph.items.len);
        self.active_signal_graph.append(allocator, .{
            .record = record.retain(),
            .rank = rank,
        }) catch std.process.exit(1);
        record.active_graph_id = record_id;
        self.pending_roc_metrics.active_graph_records_rebuilt += 1;
        return record_id;
    }

    fn appendActiveSignalDependentId(self: *HostEnv, input_record_id: u64, dependent_record_id: u64) void {
        const allocator = self.gpa.allocator();
        const input_index: usize = @intCast(input_record_id);
        if (input_index >= self.active_signal_graph.items.len) failHost("active signal dependent referenced an unknown input record");
        const dependents = &self.active_signal_graph.items[input_index].dependents;
        if (!u64SliceContains(dependents.*, dependent_record_id)) {
            const previous_len = dependents.*.len;
            dependents.* = allocator.realloc(dependents.*, previous_len + 1) catch std.process.exit(1);
            dependents.*[previous_len] = dependent_record_id;
        }
    }

    fn removeActiveSignalDependentId(self: *HostEnv, input_record_id: u64, dependent_record_id: u64) void {
        const input_index: usize = @intCast(input_record_id);
        if (input_index >= self.active_signal_graph.items.len) failHost("active signal dependent removal referenced an unknown input record");
        const dependents = &self.active_signal_graph.items[input_index].dependents;
        for (dependents.*, 0..) |existing_id, index| {
            if (existing_id != dependent_record_id) continue;
            std.mem.copyForwards(u64, dependents.*[index..], dependents.*[index + 1 ..]);
            dependents.* = self.gpa.allocator().realloc(dependents.*, dependents.*.len - 1) catch std.process.exit(1);
            return;
        }
        failHost("active signal dependent removal missed its edge");
    }

    fn replaceActiveSignalDependentId(self: *HostEnv, input_record_id: u64, old_dependent_id: u64, new_dependent_id: u64) void {
        const input_index: usize = @intCast(input_record_id);
        if (input_index >= self.active_signal_graph.items.len) failHost("active signal dependent rewrite referenced an unknown input record");
        const dependents = self.active_signal_graph.items[input_index].dependents;
        for (dependents) |*existing_id| {
            if (existing_id.* != old_dependent_id) continue;
            existing_id.* = new_dependent_id;
            return;
        }
        failHost("active signal dependent rewrite missed its edge");
    }

    fn appendActiveSourceSignalRoute(self: *HostEnv, source_node_id: u64, record_id: u64) void {
        const route = self.ensureActiveSourceSignalRoute(source_node_id);
        if (!u64SliceContains(route.items, record_id)) {
            route.append(self.gpa.allocator(), record_id) catch std.process.exit(1);
        }
    }

    fn removeActiveSourceSignalRoute(self: *HostEnv, source_node_id: u64, record_id: u64) void {
        if (source_node_id >= self.active_source_signal_routes.items.len) failHost("active source signal route removal referenced an unknown source node");
        var route = &self.active_source_signal_routes.items[@intCast(source_node_id)];
        for (route.items, 0..) |existing_id, index| {
            if (existing_id != record_id) continue;
            _ = route.swapRemove(index);
            return;
        }
        failHost("active source signal route removal missed its record");
    }

    fn replaceActiveSourceSignalRouteId(self: *HostEnv, source_node_id: u64, old_record_id: u64, new_record_id: u64) void {
        if (source_node_id >= self.active_source_signal_routes.items.len) failHost("active source signal route rewrite referenced an unknown source node");
        const route = self.active_source_signal_routes.items[@intCast(source_node_id)].items;
        for (route) |*existing_id| {
            if (existing_id.* != old_record_id) continue;
            existing_id.* = new_record_id;
            return;
        }
        failHost("active source signal route rewrite missed its record");
    }

    fn recordSliceContains(records: []const *HostSignalRecord, record: *HostSignalRecord) bool {
        for (records) |existing| {
            if (existing == record) return true;
        }
        return false;
    }

    fn appendUniqueActiveInputRecord(self: *HostEnv, records: *std.ArrayListUnmanaged(*HostSignalRecord), record: *HostSignalRecord) void {
        if (!HostEnv.recordSliceContains(records.items, record)) {
            records.append(self.gpa.allocator(), record) catch std.process.exit(1);
        }
    }

    fn appendActiveSignalInputRecords(self: *HostEnv, records: *std.ArrayListUnmanaged(*HostSignalRecord), record: *HostSignalRecord) void {
        switch (record.payload) {
            .ref, .const_value, .task_source, .interval_source => {},
            .map => |payload| self.appendUniqueActiveInputRecord(records, payload.input),
            .map2 => |payload| {
                self.appendUniqueActiveInputRecord(records, payload.left);
                self.appendUniqueActiveInputRecord(records, payload.right);
            },
            .combine => |payload| {
                for (payload.children) |child| {
                    self.appendUniqueActiveInputRecord(records, child);
                }
            },
        }
    }

    fn retainActiveSignalRecord(self: *HostEnv, record: *HostSignalRecord) void {
        if (record.active_use_count != 0) {
            record.active_use_count += 1;
            return;
        }

        record.active_use_count = 1;
        var rank: u64 = 0;

        switch (record.payload) {
            .ref, .const_value, .task_source, .interval_source => {},
            .map => |payload| {
                self.retainActiveSignalRecord(payload.input);
                const input_id = self.requireActiveSignalRecordId(payload.input);
                rank = self.active_signal_graph.items[@intCast(input_id)].rank + 1;
            },
            .map2 => |payload| {
                self.retainActiveSignalRecord(payload.left);
                if (payload.right != payload.left) {
                    self.retainActiveSignalRecord(payload.right);
                }
                const left_id = self.requireActiveSignalRecordId(payload.left);
                const right_id = self.requireActiveSignalRecordId(payload.right);
                rank = @max(
                    self.active_signal_graph.items[@intCast(left_id)].rank,
                    self.active_signal_graph.items[@intCast(right_id)].rank,
                ) + 1;
            },
            .combine => |payload| {
                for (payload.children, 0..) |child, index| {
                    if (HostEnv.recordSliceContains(payload.children[0..index], child)) continue;
                    self.retainActiveSignalRecord(child);
                    const child_id = self.requireActiveSignalRecordId(child);
                    rank = @max(rank, self.active_signal_graph.items[@intCast(child_id)].rank + 1);
                }
            },
        }

        const record_id = self.appendActiveSignalGraphNode(record, rank);

        switch (record.payload) {
            .ref => |source_node_id| self.appendActiveSourceSignalRoute(source_node_id, record_id),
            .const_value, .task_source, .interval_source => {},
            .map => |payload| self.appendActiveSignalDependentId(self.requireActiveSignalRecordId(payload.input), record_id),
            .map2 => |payload| {
                self.appendActiveSignalDependentId(self.requireActiveSignalRecordId(payload.left), record_id);
                if (payload.right != payload.left) {
                    self.appendActiveSignalDependentId(self.requireActiveSignalRecordId(payload.right), record_id);
                }
            },
            .combine => |payload| {
                for (payload.children, 0..) |child, index| {
                    if (HostEnv.recordSliceContains(payload.children[0..index], child)) continue;
                    self.appendActiveSignalDependentId(self.requireActiveSignalRecordId(child), record_id);
                }
            },
        }
    }

    fn updateMovedActiveSignalRecordEdges(self: *HostEnv, moved_record: *HostSignalRecord, old_record_id: u64, new_record_id: u64) void {
        switch (moved_record.payload) {
            .ref => |source_node_id| self.replaceActiveSourceSignalRouteId(source_node_id, old_record_id, new_record_id),
            .const_value, .task_source, .interval_source => {},
            .map => |payload| self.replaceActiveSignalDependentId(self.requireActiveSignalRecordId(payload.input), old_record_id, new_record_id),
            .map2 => |payload| {
                self.replaceActiveSignalDependentId(self.requireActiveSignalRecordId(payload.left), old_record_id, new_record_id);
                if (payload.right != payload.left) {
                    self.replaceActiveSignalDependentId(self.requireActiveSignalRecordId(payload.right), old_record_id, new_record_id);
                }
            },
            .combine => |payload| {
                for (payload.children, 0..) |child, index| {
                    if (HostEnv.recordSliceContains(payload.children[0..index], child)) continue;
                    self.replaceActiveSignalDependentId(self.requireActiveSignalRecordId(child), old_record_id, new_record_id);
                }
            },
        }
    }

    fn removeActiveSignalGraphNode(self: *HostEnv, record_id: u64, record: *HostSignalRecord) void {
        const allocator = self.gpa.allocator();
        const record_index: usize = @intCast(record_id);
        if (record_index >= self.active_signal_graph.items.len) failHost("active signal graph removal referenced an unknown record");
        if (self.active_signal_graph.items[record_index].record != record) failHost("active signal graph removal referenced the wrong record");
        if (self.active_signal_graph.items[record_index].dependents.len != 0) failHost("active signal graph removed a record with live dependents");

        allocator.free(self.active_signal_graph.items[record_index].dependents);
        const last_index = self.active_signal_graph.items.len - 1;
        _ = self.active_signal_graph.swapRemove(record_index);
        record.active_graph_id = null;
        record.release(allocator, self.roc_host.?, &self.pending_roc_metrics);

        if (record_index != last_index) {
            const moved_id: u64 = @intCast(record_index);
            const old_moved_id: u64 = @intCast(last_index);
            const moved_record = self.active_signal_graph.items[record_index].record;
            moved_record.active_graph_id = moved_id;
            self.updateMovedActiveSignalRecordEdges(moved_record, old_moved_id, moved_id);
        }
    }

    fn releaseActiveSignalRecord(self: *HostEnv, record: *HostSignalRecord) void {
        if (record.active_use_count == 0) failHost("active signal graph record use count underflow");
        record.active_use_count -= 1;
        if (record.active_use_count != 0) return;

        const allocator = self.gpa.allocator();
        const record_id = self.requireActiveSignalRecordId(record);
        var input_records: std.ArrayListUnmanaged(*HostSignalRecord) = .empty;
        defer input_records.deinit(allocator);
        self.appendActiveSignalInputRecords(&input_records, record);

        switch (record.payload) {
            .ref => |source_node_id| self.removeActiveSourceSignalRoute(source_node_id, record_id),
            .const_value, .task_source, .interval_source => {},
            .map, .map2, .combine => {},
        }

        for (input_records.items) |input_record| {
            self.removeActiveSignalDependentId(self.requireActiveSignalRecordId(input_record), record_id);
        }

        self.removeActiveSignalGraphNode(record_id, record);

        for (input_records.items) |input_record| {
            self.releaseActiveSignalRecord(input_record);
        }
    }

    fn ensureActiveSourceSignalRoute(self: *HostEnv, source_node_id: u64) *std.ArrayListUnmanaged(u64) {
        if (source_node_id >= self.node_identities.items.len) failHost("active source signal route referenced an unknown source node");
        const allocator = self.gpa.allocator();
        const route_index: usize = @intCast(source_node_id);
        while (self.active_source_signal_routes.items.len <= route_index) {
            self.active_source_signal_routes.append(allocator, .empty) catch std.process.exit(1);
        }
        return &self.active_source_signal_routes.items[route_index];
    }

    fn ensureActiveTextSignalRoute(self: *HostEnv, record_id: u64) *std.ArrayListUnmanaged(HostActiveTextSignalSink) {
        if (record_id >= self.active_signal_graph.items.len) failHost("active text signal route referenced an unknown signal record");
        const allocator = self.gpa.allocator();
        const route_index: usize = @intCast(record_id);
        while (self.active_text_signal_routes.items.len <= route_index) {
            self.active_text_signal_routes.append(allocator, .empty) catch std.process.exit(1);
        }
        return &self.active_text_signal_routes.items[route_index];
    }

    fn ensureActiveBoolSignalRoute(self: *HostEnv, record_id: u64) *std.ArrayListUnmanaged(HostActiveBoolSignalSink) {
        if (record_id >= self.active_signal_graph.items.len) failHost("active bool signal route referenced an unknown signal record");
        const allocator = self.gpa.allocator();
        const route_index: usize = @intCast(record_id);
        while (self.active_bool_signal_routes.items.len <= route_index) {
            self.active_bool_signal_routes.append(allocator, .empty) catch std.process.exit(1);
        }
        return &self.active_bool_signal_routes.items[route_index];
    }

    fn ensureActiveChangeSignalRoute(self: *HostEnv, record_id: u64) *std.ArrayListUnmanaged(HostActiveChangeSignalSink) {
        if (record_id >= self.active_signal_graph.items.len) failHost("active change signal route referenced an unknown signal record");
        const allocator = self.gpa.allocator();
        const route_index: usize = @intCast(record_id);
        while (self.active_change_signal_routes.items.len <= route_index) {
            self.active_change_signal_routes.append(allocator, .empty) catch std.process.exit(1);
        }
        return &self.active_change_signal_routes.items[route_index];
    }

    fn ensureActiveStructuralSignalRoute(self: *HostEnv, record_id: u64) *std.ArrayListUnmanaged(HostActiveStructuralSignal) {
        if (record_id >= self.active_signal_graph.items.len) failHost("active structural signal route referenced an unknown signal record");
        const allocator = self.gpa.allocator();
        const route_index: usize = @intCast(record_id);
        while (self.active_structural_signal_routes.items.len <= route_index) {
            self.active_structural_signal_routes.append(allocator, .empty) catch std.process.exit(1);
        }
        return &self.active_structural_signal_routes.items[route_index];
    }

    fn rebuildActiveSinkSignalRoutesFromStream(self: *HostEnv, stream: *const HostNodeDescriptorStream) void {
        const allocator = self.gpa.allocator();
        self.clearActiveSinkSignalRoutes();

        for (stream.signal_text_nodes.items, 0..) |desc, index| {
            const record_id = self.requireActiveSignalRecordId(desc.signal.record);
            self.ensureActiveTextSignalRoute(record_id).append(allocator, .{
                .kind = .text_node,
                .index = index,
            }) catch std.process.exit(1);
        }

        for (stream.signal_text_attrs.items, 0..) |desc, index| {
            const record_id = self.requireActiveSignalRecordId(desc.signal.record);
            self.ensureActiveTextSignalRoute(record_id).append(allocator, .{
                .kind = .text_attr,
                .index = index,
            }) catch std.process.exit(1);
        }

        for (stream.signal_bool_attrs.items, 0..) |desc, index| {
            const record_id = self.requireActiveSignalRecordId(desc.signal.record);
            self.ensureActiveBoolSignalRoute(record_id).append(allocator, .{
                .index = index,
            }) catch std.process.exit(1);
        }

        for (stream.on_changes.items, 0..) |desc, index| {
            const record_id = self.requireActiveSignalRecordId(desc.signal.record);
            self.ensureActiveChangeSignalRoute(record_id).append(allocator, .{
                .index = index,
            }) catch std.process.exit(1);
        }

        for (stream.whens.items, 0..) |desc, index| {
            const record_id = self.requireActiveSignalRecordId(desc.condition.record);
            self.ensureActiveStructuralSignalRoute(record_id).append(allocator, .{
                .kind = .when,
                .index = index,
            }) catch std.process.exit(1);
        }

        for (stream.eaches.items, 0..) |desc, index| {
            const record_id = self.requireActiveSignalRecordId(desc.items.record);
            self.ensureActiveStructuralSignalRoute(record_id).append(allocator, .{
                .kind = .each,
                .index = index,
            }) catch std.process.exit(1);
        }
    }

    fn rebuildActiveSignalGraphFromStream(self: *HostEnv, stream: *const HostNodeDescriptorStream) void {
        self.clearActiveSignalRoutes();
        self.clearActiveSignalGraph();

        for (stream.signal_text_nodes.items) |*desc| {
            self.retainActiveSignalRecord(desc.signal.record);
        }
        for (stream.signal_text_attrs.items) |*desc| {
            self.retainActiveSignalRecord(desc.signal.record);
        }
        for (stream.signal_bool_attrs.items) |*desc| {
            self.retainActiveSignalRecord(desc.signal.record);
        }
        for (stream.on_changes.items) |*desc| {
            self.retainActiveSignalRecord(desc.signal.record);
        }
        for (stream.whens.items) |*desc| {
            self.retainActiveSignalRecord(desc.condition.record);
        }
        for (stream.eaches.items) |*desc| {
            self.retainActiveSignalRecord(desc.items.record);
        }

        self.rebuildActiveSinkSignalRoutesFromStream(stream);
    }

    fn rebuildSignalRoutesFromSignals(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
        var route_lists = allocator.alloc(std.ArrayListUnmanaged(u64), self.states.items.len) catch std.process.exit(1);
        defer allocator.free(route_lists);

        for (route_lists) |*list| {
            list.* = .empty;
        }
        errdefer {
            for (route_lists) |*list| {
                list.deinit(allocator);
            }
        }

        for (self.signal_descriptors.items) |signal| {
            if (signal.kind != .source) continue;
            for (signal.source_state_ids) |state_id| {
                if (state_id >= self.states.items.len) {
                    failHost("host signal registry contains an unknown state id");
                }
                route_lists[@intCast(state_id)].append(allocator, signal.signal_id) catch std.process.exit(1);
            }
        }

        self.clearSignalRoutes();

        for (route_lists, 0..) |*route_list, index| {
            const signal_ids = route_list.toOwnedSlice(allocator) catch std.process.exit(1);
            self.signal_routes.append(allocator, .{
                .state_id = @intCast(index),
                .signal_ids = signal_ids,
            }) catch {
                allocator.free(signal_ids);
                std.process.exit(1);
            };
        }
    }

    fn validateSignalSourceEventIds(self: *HostEnv, kind: SignalKind, source_event_ids: []const u64) void {
        switch (kind) {
            .source => {
                if (source_event_ids.len == 0) {
                    failHost("Roc source signal descriptor must have at least one source event id");
                }
            },
            .map, .map2 => {
                if (source_event_ids.len != 0) {
                    failHost("Roc derived signal descriptor must not have source event ids");
                }
            },
        }

        for (source_event_ids, 0..) |event_id, index| {
            if (event_id == 0 or event_id > self.event_descriptors.items.len) {
                failHost("Roc signal descriptor referenced an unknown source event id");
            }

            for (source_event_ids[0..index]) |previous| {
                if (previous == event_id) {
                    failHost("Roc signal descriptor must not contain duplicate source event ids");
                }
            }
        }
    }

    fn rebuildSignalTopologyFromSignals(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
        const signal_count = self.signal_descriptors.items.len;
        var route_lists = allocator.alloc(std.ArrayListUnmanaged(u64), signal_count) catch std.process.exit(1);
        defer allocator.free(route_lists);
        const ranks = allocator.alloc(u64, signal_count) catch std.process.exit(1);
        defer allocator.free(ranks);

        for (route_lists) |*list| {
            list.* = .empty;
        }
        @memset(ranks, 0);

        errdefer {
            for (route_lists) |*list| {
                list.deinit(allocator);
            }
        }

        for (self.signal_descriptors.items, 0..) |*signal, index| {
            if (signal.signal_id != index) {
                failHost("host signal registry is not indexed by signal id");
            }

            for (signal.input_signal_ids) |input_signal_id| {
                if (input_signal_id >= index) {
                    failHost("host signal topology must reference prior signal ids");
                }

                const input_index: usize = @intCast(input_signal_id);
                const next_rank = ranks[input_index] + 1;
                if (next_rank > ranks[index]) {
                    ranks[index] = next_rank;
                }
                if (!u64SliceContains(route_lists[input_index].items, signal.signal_id)) {
                    route_lists[input_index].append(allocator, signal.signal_id) catch std.process.exit(1);
                }
            }

            signal.rank = ranks[index];
        }

        self.clearSignalDependents();

        for (route_lists, 0..) |*route_list, index| {
            const signal_ids = route_list.toOwnedSlice(allocator) catch std.process.exit(1);
            self.signal_dependents.append(allocator, .{
                .signal_id = @intCast(index),
                .signal_ids = signal_ids,
            }) catch {
                allocator.free(signal_ids);
                std.process.exit(1);
            };
        }
    }

    fn signalIdsForState(self: *HostEnv, state_id: u64) []const u64 {
        if (state_id >= self.signal_routes.items.len) failHost("state id has no host signal route descriptor");

        const route = self.signal_routes.items[@intCast(state_id)];
        if (route.state_id != state_id) failHost("host signal route table is not indexed by state id");
        return route.signal_ids;
    }

    fn dependentSignalIdsForSignal(self: *HostEnv, signal_id: u64) []const u64 {
        if (signal_id >= self.signal_dependents.items.len) failHost("signal id has no host dependent route descriptor");

        const route = self.signal_dependents.items[@intCast(signal_id)];
        if (route.signal_id != signal_id) failHost("host signal dependent route table is not indexed by signal id");
        return route.signal_ids;
    }

    fn appendSignalAndDependents(self: *HostEnv, allocator: std.mem.Allocator, signal_ids: *std.ArrayListUnmanaged(u64), signal_id: u64) void {
        if (!u64SliceContains(signal_ids.items, signal_id)) {
            signal_ids.append(allocator, signal_id) catch std.process.exit(1);
        }

        var index: usize = 0;
        while (index < signal_ids.items.len) : (index += 1) {
            const current_signal_id = signal_ids.items[index];
            for (self.dependentSignalIdsForSignal(current_signal_id)) |dependent_signal_id| {
                if (!u64SliceContains(signal_ids.items, dependent_signal_id)) {
                    signal_ids.append(allocator, dependent_signal_id) catch std.process.exit(1);
                }
            }
        }
    }

    fn signalRank(self: *HostEnv, signal_id: u64) u64 {
        if (signal_id >= self.signal_descriptors.items.len) failHost("signal id has no host signal descriptor");

        const descriptor = self.signal_descriptors.items[@intCast(signal_id)];
        if (descriptor.signal_id != signal_id) failHost("host signal descriptor table is not indexed by signal id");
        return descriptor.rank;
    }

    fn sortSignalIdsByRank(self: *HostEnv, signal_ids: []u64) void {
        var index: usize = 1;
        while (index < signal_ids.len) : (index += 1) {
            const value = signal_ids[index];
            const value_rank = self.signalRank(value);
            var insert_index = index;
            while (insert_index > 0) {
                const previous = signal_ids[insert_index - 1];
                const previous_rank = self.signalRank(previous);
                if (previous_rank < value_rank or (previous_rank == value_rank and previous < value)) break;
                signal_ids[insert_index] = previous;
                insert_index -= 1;
            }
            signal_ids[insert_index] = value;
        }
    }

    fn dirtySignalIdsForEvent(self: *HostEnv, allocator: std.mem.Allocator, event_id: u64) []u64 {
        var dirty_signal_ids: std.ArrayListUnmanaged(u64) = .empty;
        errdefer dirty_signal_ids.deinit(allocator);

        for (self.sourceSignalIdsForEvent(event_id)) |signal_id| {
            self.appendSignalAndDependents(allocator, &dirty_signal_ids, signal_id);
        }

        const signal_ids = dirty_signal_ids.toOwnedSlice(allocator) catch std.process.exit(1);
        self.sortSignalIdsByRank(signal_ids);
        return signal_ids;
    }

    fn activeSignalRank(self: *HostEnv, record_id: u64) u64 {
        if (record_id >= self.active_signal_graph.items.len) failHost("active signal record id has no graph node");
        return self.active_signal_graph.items[@intCast(record_id)].rank;
    }

    fn dependentActiveSignalRecordIds(self: *HostEnv, record_id: u64) []const u64 {
        if (record_id >= self.active_signal_graph.items.len) failHost("active signal record id has no dependent table");
        return self.active_signal_graph.items[@intCast(record_id)].dependents;
    }

    fn appendActiveSignalAndDependents(self: *HostEnv, allocator: std.mem.Allocator, record_ids: *std.ArrayListUnmanaged(u64), record_id: u64) void {
        if (!u64SliceContains(record_ids.items, record_id)) {
            record_ids.append(allocator, record_id) catch std.process.exit(1);
        }

        var index: usize = 0;
        while (index < record_ids.items.len) : (index += 1) {
            const current_record_id = record_ids.items[index];
            for (self.dependentActiveSignalRecordIds(current_record_id)) |dependent_record_id| {
                if (!u64SliceContains(record_ids.items, dependent_record_id)) {
                    record_ids.append(allocator, dependent_record_id) catch std.process.exit(1);
                }
            }
        }
    }

    fn sortActiveSignalRecordIdsByRank(self: *HostEnv, record_ids: []u64) void {
        var index: usize = 1;
        while (index < record_ids.len) : (index += 1) {
            const value = record_ids[index];
            const value_rank = self.activeSignalRank(value);
            var insert_index = index;
            while (insert_index > 0) {
                const previous = record_ids[insert_index - 1];
                const previous_rank = self.activeSignalRank(previous);
                if (previous_rank < value_rank or (previous_rank == value_rank and previous < value)) break;
                record_ids[insert_index] = previous;
                insert_index -= 1;
            }
            record_ids[insert_index] = value;
        }
    }

    fn dirtyActiveSignalRecordIdsForSources(self: *HostEnv, allocator: std.mem.Allocator, dirty_source_node_ids: []const u64) []u64 {
        var dirty_record_ids: std.ArrayListUnmanaged(u64) = .empty;
        errdefer dirty_record_ids.deinit(allocator);

        for (dirty_source_node_ids) |source_node_id| {
            const route_index: usize = @intCast(source_node_id);
            if (route_index >= self.active_source_signal_routes.items.len) continue;

            for (self.active_source_signal_routes.items[route_index].items) |record_id| {
                self.appendActiveSignalAndDependents(allocator, &dirty_record_ids, record_id);
            }
        }

        const record_ids = dirty_record_ids.toOwnedSlice(allocator) catch std.process.exit(1);
        self.sortActiveSignalRecordIdsByRank(record_ids);
        return record_ids;
    }

    fn dirtyActiveSignalRecordIdsForRoots(self: *HostEnv, allocator: std.mem.Allocator, root_record_ids: []const u64) []u64 {
        var dirty_record_ids: std.ArrayListUnmanaged(u64) = .empty;
        errdefer dirty_record_ids.deinit(allocator);

        for (root_record_ids) |record_id| {
            if (record_id >= self.active_signal_graph.items.len) failHost("dirty active signal root referenced an unknown record");
            self.appendActiveSignalAndDependents(allocator, &dirty_record_ids, record_id);
        }

        const record_ids = dirty_record_ids.toOwnedSlice(allocator) catch std.process.exit(1);
        self.sortActiveSignalRecordIdsByRank(record_ids);
        return record_ids;
    }

    fn recordDispatch(self: *HostEnv) void {
        self.dispatch_metrics.events_processed += 1;
        self.dispatch_metrics.recompute_batches += 1;
    }

    fn recordStreamNodesScanned(self: *HostEnv, count: usize) void {
        self.pending_roc_metrics.stream_nodes_scanned += @intCast(count);
    }

    fn deinitPendingTask(self: *HostEnv, task: *HostPendingTask) void {
        const allocator = self.gpa.allocator();
        abi.decrefBox(@ptrCast(task.task_token), self.roc_host.?);
        allocator.free(task.task_name);
        allocator.free(task.request);
        task.* = undefined;
    }

    fn clearPendingTasks(self: *HostEnv) void {
        for (self.pending_tasks.items) |*task| {
            self.deinitPendingTask(task);
        }
        self.pending_tasks.items.len = 0;
    }

    fn cancelPendingTasksInScopeSubtree(self: *HostEnv, scope_id: u64) void {
        var write_index: usize = 0;
        for (self.pending_tasks.items) |*task| {
            if (self.scopeIsDescendantOrSelf(task.owner_scope_id, scope_id)) {
                self.deinitPendingTask(task);
                continue;
            }
            self.pending_tasks.items[write_index] = task.*;
            write_index += 1;
        }
        self.pending_tasks.items.len = write_index;
    }

    fn appendCleanupEvent(self: *HostEnv, name: []const u8) void {
        const allocator = self.gpa.allocator();
        const copy = allocator.dupe(u8, name) catch std.process.exit(1);
        self.cleanup_events.append(allocator, copy) catch {
            allocator.free(copy);
            std.process.exit(1);
        };
    }

    fn cleanupEventCount(self: *const HostEnv, name: []const u8) u64 {
        var count: u64 = 0;
        for (self.cleanup_events.items) |event_name| {
            if (std.mem.eql(u8, event_name, name)) count += 1;
        }
        return count;
    }

    fn activeTaskRecordByToken(self: *HostEnv, token: HostSignalToken) ?*HostSignalRecord {
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

    fn activeTaskRecordByName(self: *HostEnv, name: []const u8) ?*HostSignalRecord {
        var found: ?*HostSignalRecord = null;
        for (self.active_signal_graph.items) |node| {
            switch (node.record.payload) {
                .task_source => |payload| {
                    if (!std.mem.eql(u8, payload.name, name)) continue;
                    if (found != null) failHost("fake task result matched more than one active task source");
                    found = node.record;
                },
                .ref, .const_value, .map, .map2, .combine, .interval_source => {},
            }
        }
        return found;
    }

    fn activeIntervalRecordCountByPeriod(self: *const HostEnv, period_ms: u64) u64 {
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

    fn activeIntervalRecordByPeriod(self: *HostEnv, period_ms: u64) ?*HostSignalRecord {
        var found: ?*HostSignalRecord = null;
        for (self.active_signal_graph.items) |node| {
            switch (node.record.payload) {
                .interval_source => |payload| {
                    if (payload.period_ms != period_ms) continue;
                    if (found != null) failHost("tick_interval matched more than one active interval source");
                    found = node.record;
                },
                .ref, .const_value, .map, .map2, .combine, .task_source => {},
            }
        }
        return found;
    }

    fn pendingTaskIndexByName(self: *HostEnv, name: []const u8) ?usize {
        var found: ?usize = null;
        for (self.pending_tasks.items, 0..) |task, index| {
            if (!task.active) continue;
            if (!std.mem.eql(u8, task.task_name, name)) continue;
            if (found != null) failHost("fake task result matched more than one pending request");
            found = index;
        }
        return found;
    }

    fn pendingTaskCountByName(self: *const HostEnv, name: []const u8) u64 {
        var count: u64 = 0;
        for (self.pending_tasks.items) |task| {
            if (task.active and std.mem.eql(u8, task.task_name, name)) count += 1;
        }
        return count;
    }

    fn removePendingTaskAt(self: *HostEnv, index: usize) HostPendingTask {
        if (index >= self.pending_tasks.items.len) failHost("pending task index is out of bounds");
        const task = self.pending_tasks.items[index];
        const last_index = self.pending_tasks.items.len - 1;
        if (index != last_index) {
            self.pending_tasks.items[index] = self.pending_tasks.items[last_index];
        }
        self.pending_tasks.items.len = last_index;
        return task;
    }

    fn appendPendingTask(self: *HostEnv, owner_scope_id: u64, task_token: HostSignalToken, task_name: []const u8, request: []const u8) void {
        const allocator = self.gpa.allocator();
        if (self.next_task_request_id == std.math.maxInt(u64)) failHost("host task request id overflowed");
        const request_id = self.next_task_request_id;
        self.next_task_request_id += 1;

        abi.increfBox(@ptrCast(task_token), 1);
        const task_name_copy = allocator.dupe(u8, task_name) catch std.process.exit(1);
        const request_copy = allocator.dupe(u8, request) catch {
            allocator.free(task_name_copy);
            std.process.exit(1);
        };
        self.pending_tasks.append(allocator, .{
            .request_id = request_id,
            .owner_scope_id = owner_scope_id,
            .task_token = task_token,
            .task_name = task_name_copy,
            .request = request_copy,
            .active = true,
        }) catch {
            abi.decrefBox(@ptrCast(task_token), self.roc_host.?);
            allocator.free(task_name_copy);
            allocator.free(request_copy);
            std.process.exit(1);
        };
    }

    fn clearStates(self: *HostEnv) void {
        for (self.states.items) |*state| {
            if (!state.active) continue;
            state.cell.deinit(self.roc_host.?, &self.pending_roc_metrics);
            state.active = false;
        }
        self.states.items.len = 0;
    }

    fn stateIndexByNodeId(self: *HostEnv, node_id: u64) ?usize {
        for (self.states.items, 0..) |state, index| {
            if (state.active and state.state_id == node_id) return index;
        }
        return null;
    }

    fn stateValueByNodeId(self: *HostEnv, node_id: u64) HostValue {
        const state_index = self.stateIndexByNodeId(node_id) orelse failHost("signal referenced an unknown active state node");
        return self.cloneHostValue(self.states.items[state_index].cell.value);
    }

    fn ensureStateFromDesc(self: *HostEnv, roc_host: *abi.RocHost, desc: HostNodeStateDesc) void {
        if (self.stateIndexByNodeId(desc.node_id) != null) return;

        const initial = callValueInitThunk(roc_host, desc.initial);
        var cell = HostValueCell.initRetained(initial, desc.eq, desc.drop, &self.pending_roc_metrics);
        self.states.append(self.gpa.allocator(), .{
            .state_id = desc.node_id,
            .cell = cell,
            .version = 0,
            .active = true,
        }) catch {
            cell.deinit(roc_host, &self.pending_roc_metrics);
            std.process.exit(1);
        };
    }

    fn syncStatesFromNodeStream(self: *HostEnv, roc_host: *abi.RocHost, stream: *const HostNodeDescriptorStream) void {
        for (stream.states.items) |desc| {
            self.ensureStateFromDesc(roc_host, desc);
        }
    }

    fn updateStateValue(self: *HostEnv, roc_host: *abi.RocHost, node_id: u64, value: HostValue) bool {
        const state_index = self.stateIndexByNodeId(node_id) orelse failHost("event referenced an unknown active state node");
        const state = &self.states.items[state_index];
        if (state.cell.valueEquals(roc_host, value)) {
            state.cell.dropIncoming(roc_host, value);
            return false;
        }

        state.cell.replaceValue(roc_host, value);
        state.version += 1;
        return true;
    }

    fn deactivateState(self: *HostEnv, roc_host: *abi.RocHost, node_id: u64) void {
        const state_index = self.stateIndexByNodeId(node_id) orelse return;
        const state = &self.states.items[state_index];
        state.cell.deinit(roc_host, &self.pending_roc_metrics);
        state.active = false;
    }

    fn stateEqCallable(self: *HostEnv, node_id: u64) abi.RocErasedCallable {
        const state_index = self.stateIndexByNodeId(node_id) orelse failHost("active state has no equality callable");
        return self.states.items[state_index].cell.eq;
    }

    fn stateDropCallable(self: *HostEnv, node_id: u64) abi.RocErasedCallable {
        const state_index = self.stateIndexByNodeId(node_id) orelse failHost("active state has no drop callable");
        return self.states.items[state_index].cell.drop;
    }

    fn validateScopeId(self: *HostEnv, scope_id: u64) void {
        if (scope_id >= self.scopes.items.len) {
            failHost("scope id has no host scope descriptor");
        }
        const scope = self.scopes.items[@intCast(scope_id)];
        if (scope.scope_id != scope_id) {
            failHost("host scope table is not indexed by scope id");
        }
        if (!scope.active) {
            failHost("scope id references a disposed host scope");
        }
    }

    fn internRootScope(self: *HostEnv) u64 {
        const allocator = self.gpa.allocator();
        if (self.scopes.items.len == 0) {
            self.scopes.append(allocator, .{
                .scope_id = 0,
                .parent_scope_id = null,
                .step = .root,
                .active = true,
            }) catch std.process.exit(1);
            var metrics = self.pending_roc_metrics;
            metrics.scopes_created += 1;
            self.pending_roc_metrics = metrics;
            return 0;
        }

        const root = self.scopes.items[0];
        if (root.scope_id != 0 or root.parent_scope_id != null or root.step != .root or !root.active) {
            failHost("host root scope descriptor is not indexed by scope id");
        }
        return 0;
    }

    fn internComponentScope(self: *HostEnv, parent_scope_id: u64, site_ordinal: u64) u64 {
        self.validateScopeId(parent_scope_id);

        for (self.scopes.items) |scope| {
            if (!scope.active) continue;
            if (scope.parent_scope_id != parent_scope_id) continue;
            switch (scope.step) {
                .component => |step| {
                    if (step.site_ordinal == site_ordinal) {
                        return scope.scope_id;
                    }
                },
                .root, .when_branch, .each_row => {},
            }
        }

        const scope_id: u64 = @intCast(self.scopes.items.len);
        self.scopes.append(self.gpa.allocator(), .{
            .scope_id = scope_id,
            .parent_scope_id = parent_scope_id,
            .step = .{ .component = .{ .site_ordinal = site_ordinal } },
            .active = true,
        }) catch std.process.exit(1);
        var metrics = self.pending_roc_metrics;
        metrics.scopes_created += 1;
        self.pending_roc_metrics = metrics;
        return scope_id;
    }

    fn internWhenBranchScope(self: *HostEnv, parent_scope_id: u64, site_ordinal: u64, branch: HostScopeBranch) u64 {
        self.validateScopeId(parent_scope_id);

        for (self.scopes.items) |scope| {
            if (!scope.active) continue;
            if (scope.parent_scope_id != parent_scope_id) continue;
            switch (scope.step) {
                .when_branch => |step| {
                    if (step.site_ordinal == site_ordinal and step.branch == branch) {
                        return scope.scope_id;
                    }
                },
                .root, .component, .each_row => {},
            }
        }

        const scope_id: u64 = @intCast(self.scopes.items.len);
        self.scopes.append(self.gpa.allocator(), .{
            .scope_id = scope_id,
            .parent_scope_id = parent_scope_id,
            .step = .{ .when_branch = .{ .site_ordinal = site_ordinal, .branch = branch } },
            .active = true,
        }) catch std.process.exit(1);
        var metrics = self.pending_roc_metrics;
        metrics.scopes_created += 1;
        self.pending_roc_metrics = metrics;
        return scope_id;
    }

    fn createEachRowScope(self: *HostEnv, parent_scope_id: u64, site_ordinal: u64, key: HostValue, item: HostValue, key_eq: abi.RocErasedCallable, key_drop: abi.RocErasedCallable, item_eq: abi.RocErasedCallable, item_drop: abi.RocErasedCallable) u64 {
        self.validateScopeId(parent_scope_id);

        const key_cell = HostValueCell.initRetained(key, key_eq, key_drop, &self.pending_roc_metrics);
        const item_cell = HostValueCell.initRetained(item, item_eq, item_drop, &self.pending_roc_metrics);

        const scope_id: u64 = @intCast(self.scopes.items.len);
        self.scopes.append(self.gpa.allocator(), .{
            .scope_id = scope_id,
            .parent_scope_id = parent_scope_id,
            .step = .{ .each_row = .{ .site_ordinal = site_ordinal, .key = key_cell, .item = item_cell } },
            .active = true,
        }) catch std.process.exit(1);
        var metrics = self.pending_roc_metrics;
        metrics.scopes_created += 1;
        self.pending_roc_metrics = metrics;
        return scope_id;
    }

    fn internNodeIdentity(self: *HostEnv, scope_id: u64, ordinal: u64) u64 {
        self.validateScopeId(scope_id);

        for (self.node_identities.items) |identity| {
            if (!identity.active) continue;
            if (identity.scope_id == scope_id and identity.ordinal == ordinal) {
                return identity.node_id;
            }
        }

        const node_id: u64 = @intCast(self.node_identities.items.len);
        self.node_identities.append(self.gpa.allocator(), .{
            .node_id = node_id,
            .scope_id = scope_id,
            .ordinal = ordinal,
            .active = true,
        }) catch std.process.exit(1);
        return node_id;
    }

    fn internDomIdentity(self: *HostEnv, scope_id: u64, ordinal: u64) u64 {
        self.validateScopeId(scope_id);

        for (self.dom_identities.items) |identity| {
            if (!identity.active) continue;
            if (identity.scope_id == scope_id and identity.ordinal == ordinal) {
                return identity.elem_id;
            }
        }

        const elem_id: u64 = @intCast(self.dom_identities.items.len + 1);
        self.dom_identities.append(self.gpa.allocator(), .{
            .elem_id = elem_id,
            .scope_id = scope_id,
            .ordinal = ordinal,
            .active = true,
        }) catch std.process.exit(1);
        return elem_id;
    }

    fn deactivateDomElement(self: *HostEnv, elem_id: u64) void {
        if (elem_id >= self.dom_elements.items.len) return;
        const elem = &self.dom_elements.items[@intCast(elem_id)];
        elem.active = false;
        elem.bound_click_event = null;
        elem.bound_input_event = null;
        elem.bound_check_event = null;
    }

    fn disposeScopeSubtree(self: *HostEnv, roc_host: *abi.RocHost, scope_id: u64) void {
        self.validateScopeId(scope_id);

        var child_index: usize = 0;
        while (child_index < self.scopes.items.len) : (child_index += 1) {
            const child = self.scopes.items[child_index];
            if (!child.active) continue;
            if (child.parent_scope_id == scope_id) {
                self.disposeScopeSubtree(roc_host, child.scope_id);
            }
        }

        for (self.node_identities.items) |*identity| {
            if (identity.active and identity.scope_id == scope_id) {
                self.deactivateState(roc_host, identity.node_id);
                identity.active = false;
            }
        }

        for (self.active_stream.cleanups.items) |cleanup| {
            if (cleanup.scope_id == scope_id) {
                self.appendCleanupEvent(cleanup.name);
            }
        }

        self.cancelPendingTasksInScopeSubtree(scope_id);

        for (self.dom_identities.items) |*identity| {
            if (identity.active and identity.scope_id == scope_id) {
                self.deactivateDomElement(identity.elem_id);
                identity.active = false;
            }
        }

        const scope = &self.scopes.items[@intCast(scope_id)];
        scope.step.deinit(roc_host, &self.pending_roc_metrics);
        scope.active = false;
        var metrics = self.pending_roc_metrics;
        metrics.scopes_disposed += 1;
        self.pending_roc_metrics = metrics;
    }

    fn activeEachRowScopes(self: *HostEnv, allocator: std.mem.Allocator, parent_scope_id: u64, site_ordinal: u64) []u64 {
        var ids: std.ArrayListUnmanaged(u64) = .empty;
        errdefer ids.deinit(allocator);

        for (self.scopes.items) |scope| {
            if (!scope.active) continue;
            if (scope.parent_scope_id != parent_scope_id) continue;
            switch (scope.step) {
                .each_row => |row| {
                    if (row.site_ordinal == site_ordinal) {
                        ids.append(allocator, scope.scope_id) catch std.process.exit(1);
                    }
                },
                .root, .component, .when_branch => {},
            }
        }

        return ids.toOwnedSlice(allocator) catch std.process.exit(1);
    }

    fn eachRowScopeKeyEquals(self: *HostEnv, roc_host: *abi.RocHost, scope_id: u64, key: HostValue) bool {
        self.validateScopeId(scope_id);
        const scope = &self.scopes.items[@intCast(scope_id)];
        self.recordEachKeyCompare();
        return switch (scope.step) {
            .each_row => |*row| row.key.valueEquals(roc_host, key),
            .root, .component, .when_branch => failHost("scope id does not reference an each-row scope"),
        };
    }

    fn eachRowScopeKeyValue(self: *HostEnv, scope_id: u64) HostValue {
        self.validateScopeId(scope_id);
        const scope = &self.scopes.items[@intCast(scope_id)];
        return switch (scope.step) {
            .each_row => |row| row.key.value,
            .root, .component, .when_branch => failHost("scope id does not reference an each-row scope"),
        };
    }

    fn recordEachKeyCompare(self: *HostEnv) void {
        var metrics = self.pending_roc_metrics;
        metrics.each_key_compares += 1;
        self.pending_roc_metrics = metrics;
    }

    fn hashEachKeyValue(self: *HostEnv, roc_host: *abi.RocHost, key_hash: abi.RocErasedCallable, key: HostValue) u64 {
        self.recordEachKeyCompare();
        return callErasedHostValueToU64(roc_host, key_hash, key);
    }

    fn eachKeysEqual(self: *HostEnv, roc_host: *abi.RocHost, key_eq: abi.RocErasedCallable, left: HostValue, right: HostValue) bool {
        self.recordEachKeyCompare();
        return callErasedHostValueHostValueToBool(roc_host, key_eq, left, right);
    }

    fn eachRowScopeItemEquals(self: *HostEnv, roc_host: *abi.RocHost, scope_id: u64, item: HostValue) bool {
        self.validateScopeId(scope_id);
        const scope = &self.scopes.items[@intCast(scope_id)];
        return switch (scope.step) {
            .each_row => |*row| row.item.valueEquals(roc_host, item),
            .root, .component, .when_branch => failHost("scope id does not reference an each-row scope"),
        };
    }

    fn replaceEachRowScopeItem(self: *HostEnv, roc_host: *abi.RocHost, scope_id: u64, item: HostValue) void {
        self.validateScopeId(scope_id);
        const scope = &self.scopes.items[@intCast(scope_id)];
        switch (scope.step) {
            .each_row => {},
            .root, .component, .when_branch => failHost("scope id does not reference an each-row scope"),
        }

        scope.step.each_row.item.replaceValue(roc_host, item);
    }

    fn eachRowScopeValues(self: *HostEnv, scope_id: u64) struct { key: HostValue, item: HostValue } {
        self.validateScopeId(scope_id);
        const scope = &self.scopes.items[@intCast(scope_id)];
        return switch (scope.step) {
            .each_row => |row| .{ .key = row.key.value, .item = row.item.value },
            .root, .component, .when_branch => failHost("scope id does not reference an each-row scope"),
        };
    }

    fn appendIndexToHashBucket(allocator: std.mem.Allocator, buckets: *std.AutoHashMapUnmanaged(u64, std.ArrayListUnmanaged(usize)), hash: u64, index: usize) void {
        const entry = buckets.getOrPut(allocator, hash) catch std.process.exit(1);
        if (!entry.found_existing) {
            entry.value_ptr.* = .empty;
        }
        entry.value_ptr.append(allocator, index) catch std.process.exit(1);
    }

    fn deinitHashBuckets(allocator: std.mem.Allocator, buckets: *std.AutoHashMapUnmanaged(u64, std.ArrayListUnmanaged(usize))) void {
        var values = buckets.valueIterator();
        while (values.next()) |bucket| {
            bucket.deinit(allocator);
        }
        buckets.deinit(allocator);
    }

    fn syncEachRowScopes(self: *HostEnv, roc_host: *abi.RocHost, parent_scope_id: u64, site_ordinal: u64, keys: []const HostValue, items: []const HostValue, key_hash: abi.RocErasedCallable, key_eq: abi.RocErasedCallable, key_drop: abi.RocErasedCallable, item_eq: abi.RocErasedCallable, item_drop: abi.RocErasedCallable) HostKeyedRowDiffResult {
        self.validateScopeId(parent_scope_id);
        if (keys.len != items.len) failHost("Ui.each keyed scope received mismatched key and item lists");

        const allocator = self.gpa.allocator();
        const existing_scope_ids = self.activeEachRowScopes(allocator, parent_scope_id, site_ordinal);
        defer allocator.free(existing_scope_ids);

        const matched_existing = allocator.alloc(bool, existing_scope_ids.len) catch std.process.exit(1);
        defer allocator.free(matched_existing);
        @memset(matched_existing, false);

        var next_scope_ids = allocator.alloc(u64, keys.len) catch std.process.exit(1);
        errdefer allocator.free(next_scope_ids);
        var row_items_changed = allocator.alloc(bool, keys.len) catch std.process.exit(1);
        errdefer allocator.free(row_items_changed);
        var removed_scope_ids: std.ArrayListUnmanaged(u64) = .empty;
        errdefer removed_scope_ids.deinit(allocator);

        var rows_reused: u64 = 0;
        var rows_created: u64 = 0;
        var row_items_unchanged: u64 = 0;
        var row_items_updated: u64 = 0;

        const key_hashes = allocator.alloc(u64, keys.len) catch std.process.exit(1);
        defer allocator.free(key_hashes);

        var next_key_indexes_by_hash: std.AutoHashMapUnmanaged(u64, std.ArrayListUnmanaged(usize)) = .{};
        defer HostEnv.deinitHashBuckets(allocator, &next_key_indexes_by_hash);
        for (keys, 0..) |key, key_index| {
            const hash = self.hashEachKeyValue(roc_host, key_hash, key);
            key_hashes[key_index] = hash;
            if (next_key_indexes_by_hash.getPtr(hash)) |bucket| {
                for (bucket.items) |previous_index| {
                    if (self.eachKeysEqual(roc_host, key_eq, keys[previous_index], key)) {
                        failHost("Ui.each keyed scope received duplicate keys");
                    }
                }
            }
            HostEnv.appendIndexToHashBucket(allocator, &next_key_indexes_by_hash, hash, key_index);
        }

        var existing_scope_indexes_by_hash: std.AutoHashMapUnmanaged(u64, std.ArrayListUnmanaged(usize)) = .{};
        defer HostEnv.deinitHashBuckets(allocator, &existing_scope_indexes_by_hash);
        for (existing_scope_ids, 0..) |scope_id, existing_index| {
            const existing_key = self.eachRowScopeKeyValue(scope_id);
            const hash = self.hashEachKeyValue(roc_host, key_hash, existing_key);
            HostEnv.appendIndexToHashBucket(allocator, &existing_scope_indexes_by_hash, hash, existing_index);
        }

        for (keys, items, 0..) |key, item, key_index| {
            var matched_scope_id: ?u64 = null;
            if (existing_scope_indexes_by_hash.getPtr(key_hashes[key_index])) |bucket| {
                for (bucket.items) |existing_index| {
                    if (matched_existing[existing_index]) continue;
                    const scope_id = existing_scope_ids[existing_index];
                    if (self.eachRowScopeKeyEquals(roc_host, scope_id, key)) {
                        matched_existing[existing_index] = true;
                        matched_scope_id = scope_id;
                        break;
                    }
                }
            }

            if (matched_scope_id) |scope_id| {
                next_scope_ids[key_index] = scope_id;
                rows_reused += 1;
                callErasedHostValueToUnit(roc_host, key_drop, key);
                if (self.eachRowScopeItemEquals(roc_host, scope_id, item)) {
                    callErasedHostValueToUnit(roc_host, item_drop, item);
                    row_items_changed[key_index] = false;
                    row_items_unchanged += 1;
                } else {
                    self.replaceEachRowScopeItem(roc_host, scope_id, item);
                    row_items_changed[key_index] = true;
                    row_items_updated += 1;
                }
            } else {
                next_scope_ids[key_index] = self.createEachRowScope(parent_scope_id, site_ordinal, key, item, key_eq, key_drop, item_eq, item_drop);
                row_items_changed[key_index] = true;
                rows_created += 1;
            }
        }

        var rows_removed: u64 = 0;
        for (existing_scope_ids, 0..) |scope_id, existing_index| {
            if (matched_existing[existing_index]) continue;
            removed_scope_ids.append(allocator, scope_id) catch std.process.exit(1);
            self.disposeScopeSubtree(roc_host, scope_id);
            rows_removed += 1;
        }

        var metrics = self.pending_roc_metrics;
        metrics.rows_reused += rows_reused;
        metrics.rows_created += rows_created;
        metrics.rows_removed += rows_removed;
        self.pending_roc_metrics = metrics;

        return .{
            .scope_ids = next_scope_ids,
            .row_items_changed = row_items_changed,
            .removed_scope_ids = removed_scope_ids.toOwnedSlice(allocator) catch std.process.exit(1),
            .rows_reused = rows_reused,
            .rows_created = rows_created,
            .rows_removed = rows_removed,
            .row_items_unchanged = row_items_unchanged,
            .row_items_updated = row_items_updated,
        };
    }

    fn walkElemIdentitySites(self: *HostEnv, elem: abi.Elem, scope_id: u64, ordinal: *u64) void {
        self.validateScopeId(scope_id);

        switch (elem.tag) {
            .Element => {
                for (elem.payload.element.children.items()) |child| {
                    self.walkElemIdentitySites(child, scope_id, ordinal);
                }
            },
            .State => {
                _ = self.internNodeIdentity(scope_id, ordinal.*);
                ordinal.* += 1;
                self.walkElemIdentitySites(elem.payload.state.child.*, scope_id, ordinal);
            },
            .Component => {
                const site_ordinal = ordinal.*;
                _ = self.internNodeIdentity(scope_id, site_ordinal);
                ordinal.* += 1;
                const component_scope_id = self.internComponentScope(scope_id, site_ordinal);
                var component_ordinal: u64 = 0;
                self.walkElemIdentitySites(elem.payload.component.child.*, component_scope_id, &component_ordinal);
            },
            .When => {
                _ = self.internNodeIdentity(scope_id, ordinal.*);
                ordinal.* += 1;
            },
            .Each => {
                _ = self.internNodeIdentity(scope_id, ordinal.*);
                ordinal.* += 1;
            },
            .Cleanup, .OnChange, .Text, .TextSignal => {},
        }
    }

    fn walkElemRootIdentitySites(self: *HostEnv, root: abi.Elem) void {
        const root_scope_id = self.internRootScope();
        var ordinal: u64 = 0;
        self.walkElemIdentitySites(root, root_scope_id, &ordinal);
    }

    fn walkElemWhenBranchIdentitySites(self: *HostEnv, parent_scope_id: u64, site_ordinal: u64, branch: HostScopeBranch, elem: abi.Elem) u64 {
        const branch_scope_id = self.internWhenBranchScope(parent_scope_id, site_ordinal, branch);
        var ordinal: u64 = 0;
        self.walkElemIdentitySites(elem, branch_scope_id, &ordinal);
        return branch_scope_id;
    }

    fn resolveNodeBinderRef(binder_stack: []const HostBinderBinding, token: HostBinderToken) u64 {
        var index = binder_stack.len;
        while (index > 0) {
            index -= 1;
            const binding = binder_stack[index];
            if (binding.token == token) return binding.node_id;
        }
        failHost("Node.BinderRef referenced a state binder outside the active scope");
    }

    fn validateExistingSignalRecord(record: *HostSignalRecord, expected_tag: std.meta.Tag(HostSignalRecordPayload)) void {
        if (std.meta.activeTag(record.payload) != expected_tag) {
            failHost("signal token was reused for a different signal expression kind");
        }
    }

    fn bindNodeSignalExpr(self: *HostEnv, allocator: std.mem.Allocator, stream: *HostNodeDescriptorStream, expr: abi.NodeSignalExpr, binder_stack: []const HostBinderBinding) *HostSignalRecord {
        return switch (expr.tag) {
            .Ref => blk: {
                const node_id = HostEnv.resolveNodeBinderRef(binder_stack, expr.payload.ref);
                break :blk HostSignalRecord.init(allocator, .{ .ref = node_id });
            },
            .ConstValue => blk: {
                const token = expr.payload.const_value._0;
                if (stream.signalRecordByToken(token)) |record| {
                    HostEnv.validateExistingSignalRecord(record, .const_value);
                    break :blk record.retain();
                }

                const record = HostSignalRecord.init(allocator, .{ .const_value = .{
                    .token = retainHostSignalToken(token),
                    .init = retainHostCallable(expr.payload.const_value._1, &self.pending_roc_metrics),
                    .eq = retainHostCallable(expr.payload.const_value._2, &self.pending_roc_metrics),
                    .drop = retainHostCallable(expr.payload.const_value._3, &self.pending_roc_metrics),
                } });
                stream.rememberSignalRecord(allocator, record);
                break :blk record;
            },
            .Map => blk: {
                const token = expr.payload.map._0;
                if (stream.signalRecordByToken(token)) |record| {
                    HostEnv.validateExistingSignalRecord(record, .map);
                    break :blk record.retain();
                }

                const input = self.bindNodeSignalExpr(allocator, stream, expr.payload.map._1.*, binder_stack);
                const record = HostSignalRecord.init(allocator, .{ .map = .{
                    .token = retainHostSignalToken(token),
                    .input = input,
                    .transform = retainHostCallable(expr.payload.map._2, &self.pending_roc_metrics),
                    .eq = retainHostCallable(expr.payload.map._3, &self.pending_roc_metrics),
                    .drop = retainHostCallable(expr.payload.map._4, &self.pending_roc_metrics),
                } });
                stream.rememberSignalRecord(allocator, record);
                break :blk record;
            },
            .Map2 => blk: {
                const token = expr.payload.map2._0;
                if (stream.signalRecordByToken(token)) |record| {
                    HostEnv.validateExistingSignalRecord(record, .map2);
                    break :blk record.retain();
                }

                const left = self.bindNodeSignalExpr(allocator, stream, expr.payload.map2._1.*, binder_stack);
                const right = self.bindNodeSignalExpr(allocator, stream, expr.payload.map2._2.*, binder_stack);
                const record = HostSignalRecord.init(allocator, .{ .map2 = .{
                    .token = retainHostSignalToken(token),
                    .left = left,
                    .right = right,
                    .transform = retainHostCallable(expr.payload.map2._3, &self.pending_roc_metrics),
                    .eq = retainHostCallable(expr.payload.map2._4, &self.pending_roc_metrics),
                    .drop = retainHostCallable(expr.payload.map2._5, &self.pending_roc_metrics),
                } });
                stream.rememberSignalRecord(allocator, record);
                break :blk record;
            },
            .Combine => blk: {
                const token = expr.payload.combine._0;
                if (stream.signalRecordByToken(token)) |record| {
                    HostEnv.validateExistingSignalRecord(record, .combine);
                    break :blk record.retain();
                }

                const source_children = expr.payload.combine._1.items();
                const children = allocator.alloc(*HostSignalRecord, source_children.len) catch std.process.exit(1);
                for (source_children, children) |child, *dest| {
                    dest.* = self.bindNodeSignalExpr(allocator, stream, child, binder_stack);
                }
                const record = HostSignalRecord.init(allocator, .{ .combine = .{
                    .token = retainHostSignalToken(token),
                    .children = children,
                    .transform = retainHostCallable(expr.payload.combine._2, &self.pending_roc_metrics),
                    .eq = retainHostCallable(expr.payload.combine._3, &self.pending_roc_metrics),
                    .drop = retainHostCallable(expr.payload.combine._4, &self.pending_roc_metrics),
                } });
                stream.rememberSignalRecord(allocator, record);
                break :blk record;
            },
            .TaskSource => blk: {
                const payload = expr.payload.task_source;
                const token = payload.token;
                if (stream.signalRecordByToken(token)) |record| {
                    HostEnv.validateExistingSignalRecord(record, .task_source);
                    break :blk record.retain();
                }

                abi.increfBox(@ptrCast(payload.payload_tag), 1);
                const record = HostSignalRecord.init(allocator, .{ .task_source = .{
                    .token = retainHostSignalToken(token),
                    .name = allocator.dupe(u8, payload.name.asSlice()) catch std.process.exit(1),
                    .payload_tag = @ptrCast(payload.payload_tag),
                    .payload_drop = retainHostCallable(payload.payload_drop, &self.pending_roc_metrics),
                    .initial = retainHostCallable(payload.initial, &self.pending_roc_metrics),
                    .done = retainHostCallable(payload.done, &self.pending_roc_metrics),
                    .failed = retainHostCallable(payload.failed, &self.pending_roc_metrics),
                    .eq = retainHostCallable(payload.eq, &self.pending_roc_metrics),
                    .drop = retainHostCallable(payload.drop, &self.pending_roc_metrics),
                } });
                stream.rememberSignalRecord(allocator, record);
                break :blk record;
            },
            .IntervalSource => blk: {
                const payload = expr.payload.interval_source;
                const token = payload.token;
                if (stream.signalRecordByToken(token)) |record| {
                    HostEnv.validateExistingSignalRecord(record, .interval_source);
                    break :blk record.retain();
                }

                const record = HostSignalRecord.init(allocator, .{ .interval_source = .{
                    .token = retainHostSignalToken(token),
                    .period_ms = payload.period_ms,
                    .initial = retainHostCallable(payload.initial, &self.pending_roc_metrics),
                    .tick = retainHostCallable(payload.tick, &self.pending_roc_metrics),
                    .eq = retainHostCallable(payload.eq, &self.pending_roc_metrics),
                    .drop = retainHostCallable(payload.drop, &self.pending_roc_metrics),
                } });
                stream.rememberSignalRecord(allocator, record);
                break :blk record;
            },
        };
    }

    fn appendSignalRecordSourceNodeIds(allocator: std.mem.Allocator, source_node_ids: *std.ArrayListUnmanaged(u64), record: *HostSignalRecord) void {
        switch (record.payload) {
            .ref => |node_id| {
                if (!u64SliceContains(source_node_ids.items, node_id)) {
                    source_node_ids.append(allocator, node_id) catch std.process.exit(1);
                }
            },
            .const_value => {},
            .map => |payload| HostEnv.appendSignalRecordSourceNodeIds(allocator, source_node_ids, payload.input),
            .map2 => |payload| {
                HostEnv.appendSignalRecordSourceNodeIds(allocator, source_node_ids, payload.left);
                HostEnv.appendSignalRecordSourceNodeIds(allocator, source_node_ids, payload.right);
            },
            .combine => |payload| {
                for (payload.children) |child| {
                    HostEnv.appendSignalRecordSourceNodeIds(allocator, source_node_ids, child);
                }
            },
            .task_source, .interval_source => {},
        }
    }

    fn bindNodeSignal(self: *HostEnv, allocator: std.mem.Allocator, stream: *HostNodeDescriptorStream, expr: abi.NodeSignalExpr, binder_stack: []const HostBinderBinding) HostSignalBinding {
        const record = self.bindNodeSignalExpr(allocator, stream, expr, binder_stack);
        var source_node_ids: std.ArrayListUnmanaged(u64) = .empty;
        HostEnv.appendSignalRecordSourceNodeIds(allocator, &source_node_ids, record);
        return .{
            .record = record,
            .source_node_ids = source_node_ids.toOwnedSlice(allocator) catch std.process.exit(1),
        };
    }

    fn collectNodeAttrDescriptor(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, elem_id: u64, attr: abi.NodeAttr, binder_stack: []const HostBinderBinding) void {
        const allocator = self.gpa.allocator();
        switch (attr.tag) {
            .StaticText => {
                const payload = attr.payload.static_text;
                const field = HostEnv.renderTextFieldFromAbi(payload.field);
                stream.appendStaticTextAttr(allocator, elem_id, field, payload.value.asSlice());
            },
            .SignalText => {
                const payload = attr.payload.signal_text;
                const field = HostEnv.renderTextFieldFromAbi(payload.field);
                const signal = self.bindNodeSignal(allocator, stream, payload.signal.*, binder_stack);
                stream.appendSignalTextAttr(allocator, roc_host, &self.pending_roc_metrics, elem_id, field, signal, payload.read);
            },
            .StaticBool => {
                const payload = attr.payload.static_bool;
                const field = HostEnv.renderBoolFieldFromAbi(payload.field);
                stream.appendStaticBoolAttr(allocator, elem_id, field, payload.value);
            },
            .SignalBool => {
                const payload = attr.payload.signal_bool;
                const field = HostEnv.renderBoolFieldFromAbi(payload.field);
                const signal = self.bindNodeSignal(allocator, stream, payload.signal.*, binder_stack);
                stream.appendSignalBoolAttr(allocator, roc_host, &self.pending_roc_metrics, elem_id, field, signal, payload.read);
            },
            .OnEvent => {
                const payload = attr.payload.on_event;
                const msg = payload.msg;
                const kind = HostEnv.renderEventKindFromAbi(payload.kind);
                const payload_kind = HostEnv.eventPayloadKindFromAbi(msg.payload_kind);
                const target_node_id = HostEnv.resolveNodeBinderRef(binder_stack, msg.binder);
                const payload_tag: HostValueTypeTag = switch (payload_kind) {
                    .unit => @ptrCast(msg.payload_unit_tag),
                    .str => @ptrCast(msg.payload_str_tag),
                    .bool => @ptrCast(msg.payload_bool_tag),
                };
                stream.appendEvent(allocator, roc_host, &self.pending_roc_metrics, elem_id, kind, msg.binder, target_node_id, payload_kind, payload_tag, msg.payload_drop, msg.transform);
            },
        }
    }

    fn collectElemDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, elem: abi.Elem, scope_id: u64, parent_elem_id: u64, ordinal: *u64, dom_ordinal: *u64, binder_stack: *std.ArrayListUnmanaged(HostBinderBinding)) void {
        self.validateScopeId(scope_id);

        const allocator = self.gpa.allocator();
        switch (elem.tag) {
            .Element => {
                const payload = elem.payload.element;
                const elem_id = self.internDomIdentity(scope_id, dom_ordinal.*);
                dom_ordinal.* += 1;
                _ = stream.appendElement(allocator, elem_id, parent_elem_id, scope_id, payload.tag.asSlice());
                for (payload.attrs.items()) |attr| {
                    self.collectNodeAttrDescriptor(roc_host, stream, elem_id, attr, binder_stack.items);
                }
                for (payload.children.items()) |child| {
                    self.collectElemDescriptors(roc_host, stream, child, scope_id, elem_id, ordinal, dom_ordinal, binder_stack);
                }
            },
            .Text => {
                const elem_id = self.internDomIdentity(scope_id, dom_ordinal.*);
                dom_ordinal.* += 1;
                stream.appendTextNode(allocator, elem_id, parent_elem_id, scope_id, elem.payload.text.asSlice());
            },
            .TextSignal => {
                const elem_id = self.internDomIdentity(scope_id, dom_ordinal.*);
                dom_ordinal.* += 1;
                const text_signal = elem.payload.text_signal;
                const signal = self.bindNodeSignal(allocator, stream, text_signal.signal.*, binder_stack.items);
                stream.appendSignalTextNode(allocator, roc_host, &self.pending_roc_metrics, elem_id, parent_elem_id, scope_id, signal, text_signal.read);
            },
            .Cleanup => {
                stream.appendCleanup(allocator, scope_id, elem.payload.cleanup.cleanup.asSlice());
            },
            .OnChange => {
                const payload = elem.payload.on_change;
                const signal = self.bindNodeSignal(allocator, stream, payload.signal.*, binder_stack.items);
                stream.appendOnChange(allocator, roc_host, &self.pending_roc_metrics, scope_id, signal, payload.to_cmd);
            },
            .State => {
                const site_ordinal = ordinal.*;
                const node_id = self.internNodeIdentity(scope_id, site_ordinal);
                ordinal.* += 1;
                stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .state, binder_stack.items);
                stream.appendState(allocator, roc_host, &self.pending_roc_metrics, node_id, elem.payload.state.initial, elem.payload.state.eq, elem.payload.state.drop);
                binder_stack.append(allocator, .{ .token = elem.payload.state.binder, .node_id = node_id }) catch std.process.exit(1);
                self.collectElemDescriptors(roc_host, stream, elem.payload.state.child.*, scope_id, parent_elem_id, ordinal, dom_ordinal, binder_stack);
                _ = binder_stack.pop() orelse unreachable;
            },
            .Component => {
                const site_ordinal = ordinal.*;
                const node_id = self.internNodeIdentity(scope_id, site_ordinal);
                ordinal.* += 1;
                stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .component, binder_stack.items);
                const component_scope_id = self.internComponentScope(scope_id, site_ordinal);
                var component_ordinal: u64 = 0;
                var component_dom_ordinal: u64 = 0;
                self.collectElemDescriptors(roc_host, stream, elem.payload.component.child.*, component_scope_id, parent_elem_id, &component_ordinal, &component_dom_ordinal, binder_stack);
            },
            .When => {
                const site_ordinal = ordinal.*;
                const node_id = self.internNodeIdentity(scope_id, site_ordinal);
                ordinal.* += 1;
                stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .when, binder_stack.items);
                const condition = self.bindNodeSignal(allocator, stream, elem.payload.when.condition.*, binder_stack.items);
                stream.appendWhen(allocator, roc_host, &self.pending_roc_metrics, node_id, condition, elem.payload.when.read, elem.payload.when.when_false.*, elem.payload.when.when_true.*);
            },
            .Each => {
                const site_ordinal = ordinal.*;
                const node_id = self.internNodeIdentity(scope_id, site_ordinal);
                ordinal.* += 1;
                stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .each, binder_stack.items);
                const items = self.bindNodeSignal(allocator, stream, elem.payload.each.items.*, binder_stack.items);
                stream.appendEach(
                    allocator,
                    roc_host,
                    &self.pending_roc_metrics,
                    node_id,
                    items,
                    elem.payload.each.items_to_values,
                    elem.payload.each.key_hash,
                    elem.payload.each.key_of,
                    elem.payload.each.key_eq,
                    elem.payload.each.key_drop,
                    elem.payload.each.item_eq,
                    elem.payload.each.item_drop,
                    elem.payload.each.row,
                );
            },
        }
    }

    fn collectElemRootDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, root: abi.Elem) void {
        const root_scope_id = self.internRootScope();
        const allocator = self.gpa.allocator();
        var binder_stack: std.ArrayListUnmanaged(HostBinderBinding) = .empty;
        defer binder_stack.deinit(allocator);
        var ordinal: u64 = 0;
        var dom_ordinal: u64 = 0;
        self.collectElemDescriptors(roc_host, stream, root, root_scope_id, 0, &ordinal, &dom_ordinal, &binder_stack);
    }

    fn collectElemWhenBranchDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, site: HostNodeScopeSiteDesc, branch: HostScopeBranch, elem: abi.Elem) u64 {
        if (site.kind != .when) {
            failHost("Elem branch collection requires a when scope site");
        }
        const branch_scope_id = self.internWhenBranchScope(site.scope_id, site.ordinal, branch);
        const allocator = self.gpa.allocator();
        var binder_stack: std.ArrayListUnmanaged(HostBinderBinding) = .empty;
        defer binder_stack.deinit(allocator);
        binder_stack.appendSlice(allocator, site.binder_bindings) catch std.process.exit(1);
        var ordinal: u64 = 0;
        var dom_ordinal: u64 = 0;
        self.collectElemDescriptors(roc_host, stream, elem, branch_scope_id, site.parent_elem_id, &ordinal, &dom_ordinal, &binder_stack);
        return branch_scope_id;
    }

    fn activeWhenBranchScopeId(self: *HostEnv, parent_scope_id: u64, site_ordinal: u64, branch: HostScopeBranch) ?u64 {
        self.validateScopeId(parent_scope_id);

        for (self.scopes.items) |scope| {
            if (!scope.active) continue;
            if (scope.parent_scope_id != parent_scope_id) continue;
            switch (scope.step) {
                .when_branch => |step| {
                    if (step.site_ordinal == site_ordinal and step.branch == branch) {
                        return scope.scope_id;
                    }
                },
                .root, .component, .each_row => {},
            }
        }

        return null;
    }

    fn collectElemActiveWhenBranchDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, site: HostNodeScopeSiteDesc, when: HostNodeWhenDesc, active_branch: HostScopeBranch, elem: abi.Elem) u64 {
        if (site.kind != .when) {
            failHost("Elem active branch collection requires a when scope site");
        }
        if (site.node_id != when.node_id) {
            failHost("Elem active branch collection received mismatched when descriptors");
        }

        if (self.activeWhenBranchScopeId(site.scope_id, site.ordinal, active_branch.opposite())) |inactive_scope_id| {
            self.disposeScopeSubtree(roc_host, inactive_scope_id);
        }

        return self.collectElemWhenBranchDescriptors(roc_host, stream, site, active_branch, elem);
    }

    fn collectActiveWhenBranchDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, site: HostNodeScopeSiteDesc, when: HostNodeWhenDesc, active_branch: HostScopeBranch, dirty_source_node_ids: []const u64) u64 {
        if (site.kind != .when) {
            failHost("active branch collection requires a when scope site");
        }
        if (site.node_id != when.node_id) {
            failHost("active branch collection received mismatched when descriptors");
        }

        if (self.activeWhenBranchScopeId(site.scope_id, site.ordinal, active_branch.opposite())) |inactive_scope_id| {
            self.disposeScopeSubtree(roc_host, inactive_scope_id);
        }

        const branch_scope_id = self.internWhenBranchScope(site.scope_id, site.ordinal, active_branch);
        const allocator = self.gpa.allocator();
        var binder_stack: std.ArrayListUnmanaged(HostBinderBinding) = .empty;
        defer binder_stack.deinit(allocator);
        binder_stack.appendSlice(allocator, site.binder_bindings) catch std.process.exit(1);

        const branch_elem = switch (active_branch) {
            .true_branch => when.when_true,
            .false_branch => when.when_false,
        };
        var ordinal: u64 = 0;
        var dom_ordinal: u64 = 0;
        self.collectActiveElemDescriptors(roc_host, stream, branch_elem, branch_scope_id, site.parent_elem_id, &ordinal, &dom_ordinal, &binder_stack, dirty_source_node_ids);
        return branch_scope_id;
    }

    fn collectElemEachRowDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, site: HostNodeScopeSiteDesc, each: HostNodeEachDesc, items: []const HostValue) HostKeyedRowDiffResult {
        if (site.kind != .each) {
            failHost("Elem row collection requires an each scope site");
        }
        if (site.node_id != each.node_id) {
            failHost("Elem row collection received mismatched each descriptors");
        }

        const allocator = self.gpa.allocator();
        const keys = allocator.alloc(HostValue, items.len) catch std.process.exit(1);
        defer allocator.free(keys);

        for (items, 0..) |item, index| {
            keys[index] = callErasedHostValueToHostValue(roc_host, each.key_of, item);
        }

        const diff = self.syncEachRowScopes(roc_host, site.scope_id, site.ordinal, keys, items, each.key_hash, each.key_eq, each.key_drop, each.item_eq, each.item_drop);

        var binder_stack: std.ArrayListUnmanaged(HostBinderBinding) = .empty;
        defer binder_stack.deinit(allocator);
        binder_stack.appendSlice(allocator, site.binder_bindings) catch std.process.exit(1);

        for (diff.scope_ids) |row_scope_id| {
            const row_values = self.eachRowScopeValues(row_scope_id);
            const row_elem = callErasedHostValueHostValueToElem(roc_host, each.row, row_values.key, row_values.item);
            defer abi.decrefElem(row_elem, roc_host);

            var ordinal: u64 = 0;
            var dom_ordinal: u64 = 0;
            self.collectElemDescriptors(roc_host, stream, row_elem, row_scope_id, site.parent_elem_id, &ordinal, &dom_ordinal, &binder_stack);
        }

        return diff;
    }

    fn syncActiveEachRowScopes(self: *HostEnv, roc_host: *abi.RocHost, site: HostNodeScopeSiteDesc, each: HostNodeEachDesc) HostKeyedRowDiffResult {
        if (site.kind != .each) {
            failHost("active row sync requires an each scope site");
        }
        if (site.node_id != each.node_id) {
            failHost("active row sync received mismatched each descriptors");
        }

        const allocator = self.gpa.allocator();
        const items_value = cloneCachedSignalValue(self, &each.cached_value);
        defer callErasedHostValueToUnit(roc_host, hostSignalBindingDropCallable(self, &each.items), items_value);

        const items = callErasedHostValueToHostValueList(roc_host, each.items_to_values, items_value);
        defer items.decref(roc_host);
        const item_values = items.items();

        const keys = allocator.alloc(HostValue, item_values.len) catch std.process.exit(1);
        defer allocator.free(keys);

        for (item_values, 0..) |item, index| {
            keys[index] = callErasedHostValueToHostValue(roc_host, each.key_of, item);
        }

        return self.syncEachRowScopes(roc_host, site.scope_id, site.ordinal, keys, item_values, each.key_hash, each.key_eq, each.key_drop, each.item_eq, each.item_drop);
    }

    fn collectActiveEachRowDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, site: HostNodeScopeSiteDesc, each: HostNodeEachDesc, dirty_source_node_ids: []const u64) void {
        const allocator = self.gpa.allocator();
        const diff = self.syncActiveEachRowScopes(roc_host, site, each);
        defer diff.deinit(allocator);
        self.collectActiveEachRowDescriptorsFromDiff(roc_host, stream, site, each, diff, dirty_source_node_ids);
    }

    fn collectActiveEachRowDescriptorsFromDiff(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, site: HostNodeScopeSiteDesc, each: HostNodeEachDesc, diff: HostKeyedRowDiffResult, dirty_source_node_ids: []const u64) void {
        const allocator = self.gpa.allocator();
        var binder_stack: std.ArrayListUnmanaged(HostBinderBinding) = .empty;
        defer binder_stack.deinit(allocator);
        binder_stack.appendSlice(allocator, site.binder_bindings) catch std.process.exit(1);

        for (diff.scope_ids, diff.row_items_changed) |row_scope_id, row_item_changed| {
            if (!row_item_changed and !self.scopeSubtreeHasDirtyStructuralSource(&self.active_stream, row_scope_id, dirty_source_node_ids)) {
                self.copyActiveScopeSubtreeDescriptors(roc_host, stream, row_scope_id);
                continue;
            }

            const row_values = self.eachRowScopeValues(row_scope_id);
            const row_elem = callErasedHostValueHostValueToElem(roc_host, each.row, row_values.key, row_values.item);
            defer abi.decrefElem(row_elem, roc_host);

            var ordinal: u64 = 0;
            var dom_ordinal: u64 = 0;
            self.collectActiveElemDescriptors(roc_host, stream, row_elem, row_scope_id, site.parent_elem_id, &ordinal, &dom_ordinal, &binder_stack, dirty_source_node_ids);
        }
    }

    fn collectActiveEachSingleRowDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, site: HostNodeScopeSiteDesc, each: HostNodeEachDesc, row_scope_id: u64, dirty_source_node_ids: []const u64) void {
        const allocator = self.gpa.allocator();
        var binder_stack: std.ArrayListUnmanaged(HostBinderBinding) = .empty;
        defer binder_stack.deinit(allocator);
        binder_stack.appendSlice(allocator, site.binder_bindings) catch std.process.exit(1);

        const row_values = self.eachRowScopeValues(row_scope_id);
        const row_elem = callErasedHostValueHostValueToElem(roc_host, each.row, row_values.key, row_values.item);
        defer abi.decrefElem(row_elem, roc_host);

        var ordinal: u64 = 0;
        var dom_ordinal: u64 = 0;
        self.collectActiveElemDescriptors(roc_host, stream, row_elem, row_scope_id, site.parent_elem_id, &ordinal, &dom_ordinal, &binder_stack, dirty_source_node_ids);
    }

    fn eachSiteRowAncestorScopeId(self: *HostEnv, scope_id: u64, site: HostEachSite) ?u64 {
        var current: ?u64 = scope_id;
        while (current) |id| {
            if (id >= self.scopes.items.len) failHost("scope descriptor referenced an unknown parent scope");
            const scope = self.scopes.items[@intCast(id)];
            switch (scope.step) {
                .each_row => |row| {
                    if (scope.parent_scope_id == site.parent_scope_id and row.site_ordinal == site.site_ordinal) return id;
                },
                .root, .component, .when_branch => {},
            }
            current = scope.parent_scope_id;
        }
        return null;
    }

    fn activeEachRowScopesInRenderOrder(self: *HostEnv, allocator: std.mem.Allocator, site: HostEachSite) []u64 {
        var ids: std.ArrayListUnmanaged(u64) = .empty;
        errdefer ids.deinit(allocator);

        self.recordStreamNodesScanned(self.active_stream.render_nodes.items.len);
        for (self.active_stream.render_nodes.items) |node| {
            const scope_id = HostEnv.renderNodeScopeId(&self.active_stream, node);
            const row_scope_id = self.eachSiteRowAncestorScopeId(scope_id, site) orelse continue;
            appendUniqueU64(allocator, &ids, row_scope_id);
        }

        return ids.toOwnedSlice(allocator) catch std.process.exit(1);
    }

    fn eachDiffPreservesSurvivorRenderOrder(old_render_rows: []const u64, next_scope_ids: []const u64) bool {
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

    fn eachDiffIsPurePermutation(self: *HostEnv, old_render_rows: []const u64, diff: HostKeyedRowDiffResult, dirty_source_node_ids: []const u64) bool {
        if (diff.rows_created != 0 or diff.rows_removed != 0) return false;
        if (diff.scope_ids.len != old_render_rows.len) return false;
        for (diff.scope_ids, diff.row_items_changed) |scope_id, row_item_changed| {
            if (row_item_changed) return false;
            if (!u64SliceContains(old_render_rows, scope_id)) return false;
            if (self.scopeSubtreeHasDirtyStructuralSource(&self.active_stream, scope_id, dirty_source_node_ids)) return false;
        }
        return true;
    }

    fn applyDirtyEachPermutationMoves(self: *HostEnv, site: HostNodeScopeSiteDesc, next_scope_ids: []const u64) CommandCounts {
        if (site.kind != .each) failHost("dirty each permutation move received a non-each site");

        const allocator = self.gpa.allocator();
        const each_site = HostEachSite{ .parent_scope_id = site.scope_id, .site_ordinal = site.ordinal };
        var segments: std.ArrayListUnmanaged(HostEachRowRenderSegment) = .empty;
        defer segments.deinit(allocator);
        var segment_indexes_by_scope: std.AutoHashMapUnmanaged(u64, usize) = .{};
        defer segment_indexes_by_scope.deinit(allocator);

        var render_index: usize = 0;
        while (render_index < self.active_stream.render_nodes.items.len) {
            const node = self.active_stream.render_nodes.items[render_index];
            const scope_id = HostEnv.renderNodeScopeId(&self.active_stream, node);
            const row_scope_id = self.eachSiteRowAncestorScopeId(scope_id, each_site) orelse {
                render_index += 1;
                continue;
            };
            const start = render_index;
            render_index += 1;
            while (render_index < self.active_stream.render_nodes.items.len) : (render_index += 1) {
                const next_node = self.active_stream.render_nodes.items[render_index];
                const next_scope_id = HostEnv.renderNodeScopeId(&self.active_stream, next_node);
                const next_row_scope_id = self.eachSiteRowAncestorScopeId(next_scope_id, each_site);
                if (next_row_scope_id == null or next_row_scope_id.? != row_scope_id) break;
            }

            const segment_index = segments.items.len;
            segments.append(allocator, .{
                .scope_id = row_scope_id,
                .start = start,
                .len = render_index - start,
            }) catch std.process.exit(1);
            const entry = segment_indexes_by_scope.getOrPut(allocator, row_scope_id) catch std.process.exit(1);
            if (entry.found_existing) failHost("each row render nodes were split across multiple segments");
            entry.value_ptr.* = segment_index;
        }

        if (segments.items.len != next_scope_ids.len) failHost("pure each permutation did not cover every rendered row");
        if (segments.items.len == 0) return .{};

        const region_start = segments.items[0].start;
        var expected_start = region_start;
        var total_len: usize = 0;
        for (segments.items) |segment| {
            if (segment.start != expected_start) failHost("each row render segments were not contiguous");
            expected_start += segment.len;
            total_len += segment.len;
        }

        var moves_by_scope: std.AutoHashMapUnmanaged(u64, HostEachRowRenderMove) = .{};
        defer moves_by_scope.deinit(allocator);
        const reordered_nodes = allocator.alloc(HostRenderNode, total_len) catch std.process.exit(1);
        defer allocator.free(reordered_nodes);

        var write_index: usize = 0;
        for (next_scope_ids) |scope_id| {
            const segment_index = segment_indexes_by_scope.get(scope_id) orelse failHost("pure each permutation referenced a row without render nodes");
            const segment = segments.items[segment_index];
            const next_start = region_start + write_index;
            @memcpy(reordered_nodes[write_index..][0..segment.len], self.active_stream.render_nodes.items[segment.start..][0..segment.len]);
            moves_by_scope.put(allocator, scope_id, .{
                .old_start = segment.start,
                .new_start = next_start,
                .len = segment.len,
            }) catch std.process.exit(1);
            write_index += segment.len;
        }

        if (write_index != total_len) failHost("pure each permutation wrote the wrong render-node count");
        @memcpy(self.active_stream.render_nodes.items[region_start..][0..total_len], reordered_nodes);

        for (self.active_stream.scope_sites.items) |*scope_site| {
            const row_scope_id = self.eachSiteRowAncestorScopeId(scope_site.scope_id, each_site) orelse continue;
            const move = moves_by_scope.get(row_scope_id) orelse failHost("scope site referenced a row missing from pure each permutation");
            if (scope_site.render_insert_index < move.old_start) failHost("scope site insertion point preceded its row render segment");
            const offset = scope_site.render_insert_index - move.old_start;
            if (offset > move.len) failHost("scope site insertion point exceeded its row render segment");
            scope_site.render_insert_index = move.new_start + offset;
        }

        var counts: CommandCounts = .{};
        const children = streamDirectChildren(allocator, &self.active_stream, site.parent_elem_id);
        defer allocator.free(children);
        replaceDomChildrenForStructuralParentMoves(self, site.parent_elem_id, children, &counts);
        self.render_metrics.addCommandCounts(counts);
        return counts;
    }

    fn lastRenderEndIndexInScopeSubtree(self: *HostEnv, stream: *const HostNodeDescriptorStream, root_scope_id: u64) ?usize {
        var end_index: ?usize = null;
        self.recordStreamNodesScanned(stream.render_nodes.items.len);
        for (stream.render_nodes.items, 0..) |node, index| {
            if (self.renderNodeInScopeSubtree(stream, node, root_scope_id)) {
                end_index = index + 1;
            }
        }
        return end_index;
    }

    fn renderInsertIndexForEachRow(self: *HostEnv, site: HostNodeScopeSiteDesc, next_scope_ids: []const u64, row_index: usize) usize {
        if (row_index >= next_scope_ids.len) failHost("each row insertion index was requested outside the next row order");

        if (self.firstRenderIndexInScopeSubtree(&self.active_stream, next_scope_ids[row_index])) |existing_index| {
            return existing_index;
        }

        var next_index = row_index + 1;
        while (next_index < next_scope_ids.len) : (next_index += 1) {
            if (self.firstRenderIndexInScopeSubtree(&self.active_stream, next_scope_ids[next_index])) |insert_before| {
                return insert_before;
            }
        }

        var previous_index = row_index;
        while (previous_index > 0) {
            previous_index -= 1;
            if (self.lastRenderEndIndexInScopeSubtree(&self.active_stream, next_scope_ids[previous_index])) |insert_after| {
                return insert_after;
            }
        }

        return site.render_insert_index;
    }

    fn scopeIsDescendantOrSelf(self: *HostEnv, scope_id: u64, root_scope_id: u64) bool {
        var current: ?u64 = scope_id;
        while (current) |id| {
            if (id == root_scope_id) return true;
            if (id >= self.scopes.items.len) failHost("scope descriptor referenced an unknown parent scope");
            current = self.scopes.items[@intCast(id)].parent_scope_id;
        }
        return false;
    }

    fn renderNodeScopeId(stream: *const HostNodeDescriptorStream, node: HostRenderNode) u64 {
        return switch (node.kind) {
            .element => (findElementDesc(stream, node.elem_id) orelse failMissingRenderDescriptor("renderNodeScopeId", node)).scope_id,
            .text => (findTextNodeDesc(stream, node.elem_id) orelse failMissingRenderDescriptor("renderNodeScopeId", node)).scope_id,
            .signal_text => (findSignalTextNodeDesc(stream, node.elem_id) orelse failMissingRenderDescriptor("renderNodeScopeId", node)).scope_id,
        };
    }

    fn elemScopeId(stream: *const HostNodeDescriptorStream, elem_id: u64) ?u64 {
        const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return null;
        if (descriptor_index.element) |index| {
            if (index >= stream.elements.items.len) failHost("element descriptor index exceeded descriptor table");
            const desc = stream.elements.items[index];
            if (desc.elem_id != elem_id) failHost("element descriptor index pointed at the wrong elem id");
            return desc.scope_id;
        }
        if (descriptor_index.text_node) |index| {
            if (index >= stream.text_nodes.items.len) failHost("text node descriptor index exceeded descriptor table");
            const desc = stream.text_nodes.items[index];
            if (desc.elem_id != elem_id) failHost("text node descriptor index pointed at the wrong elem id");
            return desc.scope_id;
        }
        if (descriptor_index.signal_text_node) |index| {
            if (index >= stream.signal_text_nodes.items.len) failHost("signal text node descriptor index exceeded descriptor table");
            const desc = stream.signal_text_nodes.items[index];
            if (desc.elem_id != elem_id) failHost("signal text node descriptor index pointed at the wrong elem id");
            return desc.scope_id;
        }
        return null;
    }

    fn streamNodeIdInScopeSubtree(self: *HostEnv, previous: *const HostNodeDescriptorStream, node_id: u64, root_scope_id: u64) bool {
        self.recordStreamNodesScanned(previous.scope_sites.items.len);
        for (previous.scope_sites.items) |site| {
            if (site.node_id == node_id and self.scopeIsDescendantOrSelf(site.scope_id, root_scope_id)) return true;
        }
        return false;
    }

    fn scopeSubtreeHasDirtyStructuralSource(self: *HostEnv, previous: *const HostNodeDescriptorStream, root_scope_id: u64, dirty_source_node_ids: []const u64) bool {
        if (dirty_source_node_ids.len == 0) return false;

        self.recordStreamNodesScanned(previous.whens.items.len);
        for (previous.whens.items) |desc| {
            if (!self.streamNodeIdInScopeSubtree(previous, desc.node_id, root_scope_id)) continue;
            if (sourceNodeIdsIntersect(desc.condition.source_node_ids, dirty_source_node_ids)) return true;
        }
        self.recordStreamNodesScanned(previous.eaches.items.len);
        for (previous.eaches.items) |desc| {
            if (!self.streamNodeIdInScopeSubtree(previous, desc.node_id, root_scope_id)) continue;
            if (sourceNodeIdsIntersect(desc.items.source_node_ids, dirty_source_node_ids)) return true;
        }
        return false;
    }

    fn cloneHostSignalCacheSlot(self: *HostEnv, slot: HostSignalCacheSlot, metrics: *RuntimeMetrics) HostSignalCacheSlot {
        return slot.cloneRetained(self, metrics);
    }

    fn activeEventTransformByIndex(self: *HostEnv, event_index: usize) abi.RocErasedCallable {
        if (event_index >= self.active_events.items.len) failHost("active event table is missing a retained transform");
        return self.active_events.items[event_index].transform;
    }

    fn activeEventPayloadDropByIndex(self: *HostEnv, event_index: usize) abi.RocErasedCallable {
        if (event_index >= self.active_events.items.len) failHost("active event table is missing a retained payload drop");
        return self.active_events.items[event_index].payload_drop;
    }

    fn activeEventPayloadTagByIndex(self: *HostEnv, event_index: usize) HostValueTypeTag {
        if (event_index >= self.active_events.items.len) failHost("active event table is missing a retained payload tag");
        return self.active_events.items[event_index].payload_tag;
    }

    fn activeScopeSiteByNodeId(self: *HostEnv, node_id: u64, kind: HostNodeScopeSiteKind) ?HostNodeScopeSiteDesc {
        for (self.active_stream.scope_sites.items) |site| {
            if (site.node_id == node_id and site.kind == kind) return site;
        }
        return null;
    }

    fn activeWhenIndexByNodeId(self: *HostEnv, node_id: u64) ?usize {
        for (self.active_stream.whens.items, 0..) |when, index| {
            if (when.node_id == node_id) return index;
        }
        return null;
    }

    fn activeEachIndexByNodeId(self: *HostEnv, node_id: u64) ?usize {
        for (self.active_stream.eaches.items, 0..) |each, index| {
            if (each.node_id == node_id) return index;
        }
        return null;
    }

    fn scopeIsEachSiteRowDescendantOrSelf(self: *HostEnv, scope_id: u64, site: HostEachSite) bool {
        var current: ?u64 = scope_id;
        while (current) |id| {
            if (id >= self.scopes.items.len) failHost("scope descriptor referenced an unknown parent scope");
            const scope = self.scopes.items[@intCast(id)];
            switch (scope.step) {
                .each_row => |row| {
                    if (scope.parent_scope_id == site.parent_scope_id and row.site_ordinal == site.site_ordinal) return true;
                },
                .root, .component, .when_branch => {},
            }
            current = scope.parent_scope_id;
        }
        return false;
    }

    fn scopeIsInReplacementTarget(self: *HostEnv, scope_id: u64, target: HostStructuralReplacementTarget) bool {
        return switch (target) {
            .scope => |root_scope_id| self.scopeIsDescendantOrSelf(scope_id, root_scope_id),
            .each_site => |site| self.scopeIsEachSiteRowDescendantOrSelf(scope_id, site),
        };
    }

    fn renderNodeInScopeSubtree(self: *HostEnv, stream: *const HostNodeDescriptorStream, node: HostRenderNode, root_scope_id: u64) bool {
        return self.scopeIsDescendantOrSelf(HostEnv.renderNodeScopeId(stream, node), root_scope_id);
    }

    fn renderNodeInReplacementTarget(self: *HostEnv, stream: *const HostNodeDescriptorStream, node: HostRenderNode, target: HostStructuralReplacementTarget) bool {
        return self.scopeIsInReplacementTarget(HostEnv.renderNodeScopeId(stream, node), target);
    }

    fn elemIdInScopeSubtree(self: *HostEnv, stream: *const HostNodeDescriptorStream, elem_id: u64, root_scope_id: u64) bool {
        const scope_id = HostEnv.elemScopeId(stream, elem_id) orelse failHost("descriptor referenced an element outside the render stream");
        return self.scopeIsDescendantOrSelf(scope_id, root_scope_id);
    }

    fn elemIdInReplacementTarget(self: *HostEnv, stream: *const HostNodeDescriptorStream, elem_id: u64, target: HostStructuralReplacementTarget) bool {
        const scope_id = HostEnv.elemScopeId(stream, elem_id) orelse failHost("descriptor referenced an element outside the render stream");
        return self.scopeIsInReplacementTarget(scope_id, target);
    }

    fn countRenderNodesInScopeSubtree(self: *HostEnv, stream: *const HostNodeDescriptorStream, root_scope_id: u64) usize {
        var count: usize = 0;
        self.recordStreamNodesScanned(stream.render_nodes.items.len);
        for (stream.render_nodes.items) |node| {
            if (self.renderNodeInScopeSubtree(stream, node, root_scope_id)) count += 1;
        }
        return count;
    }

    fn countRenderNodesInReplacementTarget(self: *HostEnv, stream: *const HostNodeDescriptorStream, target: HostStructuralReplacementTarget) usize {
        var count: usize = 0;
        self.recordStreamNodesScanned(stream.render_nodes.items.len);
        for (stream.render_nodes.items) |node| {
            if (self.renderNodeInReplacementTarget(stream, node, target)) count += 1;
        }
        return count;
    }

    fn firstRenderIndexInScopeSubtree(self: *HostEnv, stream: *const HostNodeDescriptorStream, root_scope_id: u64) ?usize {
        self.recordStreamNodesScanned(stream.render_nodes.items.len);
        for (stream.render_nodes.items, 0..) |node, index| {
            if (self.renderNodeInScopeSubtree(stream, node, root_scope_id)) return index;
        }
        return null;
    }

    fn streamNodeIdInReplacementTarget(self: *HostEnv, previous: *const HostNodeDescriptorStream, node_id: u64, target: HostStructuralReplacementTarget) bool {
        self.recordStreamNodesScanned(previous.scope_sites.items.len);
        for (previous.scope_sites.items) |site| {
            if (site.node_id == node_id and self.scopeIsInReplacementTarget(site.scope_id, target)) return true;
        }
        return false;
    }

    fn adjustedRenderInsertIndex(old_index: usize, replace_index: usize, removed_count: usize, replacement_count: usize) usize {
        if (old_index <= replace_index) return old_index;
        if (old_index < replace_index + removed_count) failHost("scope site inside replaced scope was not removed");
        return old_index - removed_count + replacement_count;
    }

    fn appendRenderNodeDescriptorFromStream(self: *HostEnv, roc_host: *abi.RocHost, dest: *HostNodeDescriptorStream, source: *const HostNodeDescriptorStream, node: HostRenderNode) void {
        const allocator = self.gpa.allocator();
        switch (node.kind) {
            .element => {
                const desc = findElementDesc(source, node.elem_id) orelse failMissingRenderDescriptor("appendRenderNodeDescriptorFromStream", node);
                _ = dest.appendElement(allocator, desc.elem_id, desc.parent_elem_id, desc.scope_id, desc.tag);
            },
            .text => {
                const desc = findTextNodeDesc(source, node.elem_id) orelse failMissingRenderDescriptor("appendRenderNodeDescriptorFromStream", node);
                dest.appendTextNode(allocator, desc.elem_id, desc.parent_elem_id, desc.scope_id, desc.value);
            },
            .signal_text => {
                const desc = findSignalTextNodeDesc(source, node.elem_id) orelse failMissingRenderDescriptor("appendRenderNodeDescriptorFromStream", node);
                const signal = desc.signal.cloneRetained(allocator, &self.pending_roc_metrics);
                dest.appendSignalTextNode(allocator, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.parent_elem_id, desc.scope_id, signal, desc.read);
                dest.signal_text_nodes.items[dest.signal_text_nodes.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(desc.cached_value, &self.pending_roc_metrics);
            },
        }
    }

    fn appendEventDescriptorFromStream(self: *HostEnv, roc_host: *abi.RocHost, dest: *HostNodeDescriptorStream, desc: HostNodeEventDesc, event_index: usize) void {
        const allocator = self.gpa.allocator();
        const payload_tag = if (desc.owns_payload_tag) desc.payload_tag else self.activeEventPayloadTagByIndex(event_index);
        const payload_drop = if (desc.owns_payload_drop) desc.payload_drop else self.activeEventPayloadDropByIndex(event_index);
        const transform = if (desc.owns_transform) desc.transform else self.activeEventTransformByIndex(event_index);
        dest.appendEventWithBorrowedPayloadTag(allocator, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.kind, desc.binder_token, desc.target_node_id, desc.payload_kind, payload_tag, payload_drop, transform);
    }

    fn appendPreviousNonRenderDescriptorsExcludingTarget(self: *HostEnv, roc_host: *abi.RocHost, dest: *HostNodeDescriptorStream, target: HostStructuralReplacementTarget, replace_index: usize, removed_render_count: usize, replacement_render_count: usize) void {
        const allocator = self.gpa.allocator();
        const previous = &self.active_stream;

        for (previous.static_text_attrs.items) |desc| {
            if (self.elemIdInReplacementTarget(previous, desc.elem_id, target)) continue;
            dest.appendStaticTextAttr(allocator, desc.elem_id, desc.field, desc.value);
        }
        for (previous.signal_text_attrs.items) |desc| {
            if (self.elemIdInReplacementTarget(previous, desc.elem_id, target)) continue;
            const signal = desc.signal.cloneRetained(allocator, &self.pending_roc_metrics);
            dest.appendSignalTextAttr(allocator, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.field, signal, desc.read);
            dest.signal_text_attrs.items[dest.signal_text_attrs.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(desc.cached_value, &self.pending_roc_metrics);
        }
        for (previous.static_bool_attrs.items) |desc| {
            if (self.elemIdInReplacementTarget(previous, desc.elem_id, target)) continue;
            dest.appendStaticBoolAttr(allocator, desc.elem_id, desc.field, desc.value);
        }
        for (previous.signal_bool_attrs.items) |desc| {
            if (self.elemIdInReplacementTarget(previous, desc.elem_id, target)) continue;
            const signal = desc.signal.cloneRetained(allocator, &self.pending_roc_metrics);
            dest.appendSignalBoolAttr(allocator, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.field, signal, desc.read);
            dest.signal_bool_attrs.items[dest.signal_bool_attrs.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(desc.cached_value, &self.pending_roc_metrics);
        }
        for (previous.on_changes.items) |desc| {
            if (self.scopeIsInReplacementTarget(desc.scope_id, target)) continue;
            const signal = desc.signal.cloneRetained(allocator, &self.pending_roc_metrics);
            dest.appendOnChange(allocator, roc_host, &self.pending_roc_metrics, desc.scope_id, signal, desc.to_cmd);
            dest.on_changes.items[dest.on_changes.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(desc.cached_value, &self.pending_roc_metrics);
        }
        for (previous.cleanups.items) |desc| {
            if (self.scopeIsInReplacementTarget(desc.scope_id, target)) continue;
            dest.appendCleanup(allocator, desc.scope_id, desc.name);
        }
        for (previous.events.items, 0..) |desc, event_index| {
            if (self.elemIdInReplacementTarget(previous, desc.elem_id, target)) continue;
            self.appendEventDescriptorFromStream(roc_host, dest, desc, event_index);
        }

        for (previous.scope_sites.items) |desc| {
            if (self.scopeIsInReplacementTarget(desc.scope_id, target)) continue;
            const render_insert_index = HostEnv.adjustedRenderInsertIndex(desc.render_insert_index, replace_index, removed_render_count, replacement_render_count);
            dest.appendScopeSiteAt(allocator, desc.node_id, desc.scope_id, desc.ordinal, desc.parent_elem_id, render_insert_index, desc.kind, desc.binder_bindings);
        }
        for (previous.states.items) |desc| {
            if (self.streamNodeIdInReplacementTarget(previous, desc.node_id, target)) continue;
            dest.appendState(allocator, roc_host, &self.pending_roc_metrics, desc.node_id, desc.initial, desc.eq, desc.drop);
        }
        for (previous.whens.items) |desc| {
            if (self.streamNodeIdInReplacementTarget(previous, desc.node_id, target)) continue;
            const condition = desc.condition.cloneRetained(allocator, &self.pending_roc_metrics);
            dest.appendWhen(allocator, roc_host, &self.pending_roc_metrics, desc.node_id, condition, desc.read, desc.when_false, desc.when_true);
            dest.whens.items[dest.whens.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(desc.cached_value, &self.pending_roc_metrics);
        }
        for (previous.eaches.items) |desc| {
            if (self.streamNodeIdInReplacementTarget(previous, desc.node_id, target)) continue;
            const items = desc.items.cloneRetained(allocator, &self.pending_roc_metrics);
            dest.appendEach(allocator, roc_host, &self.pending_roc_metrics, desc.node_id, items, desc.items_to_values, desc.key_hash, desc.key_of, desc.key_eq, desc.key_drop, desc.item_eq, desc.item_drop, desc.row);
            dest.eaches.items[dest.eaches.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(desc.cached_value, &self.pending_roc_metrics);
        }
    }

    fn appendReplacementNonRenderDescriptors(self: *HostEnv, roc_host: *abi.RocHost, dest: *HostNodeDescriptorStream, replacement: *const HostNodeDescriptorStream, render_insert_offset: usize) void {
        const allocator = self.gpa.allocator();

        for (replacement.static_text_attrs.items) |desc| {
            dest.appendStaticTextAttr(allocator, desc.elem_id, desc.field, desc.value);
        }
        for (replacement.signal_text_attrs.items) |desc| {
            const signal = desc.signal.cloneRetained(allocator, &self.pending_roc_metrics);
            dest.appendSignalTextAttr(allocator, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.field, signal, desc.read);
            dest.signal_text_attrs.items[dest.signal_text_attrs.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(desc.cached_value, &self.pending_roc_metrics);
        }
        for (replacement.static_bool_attrs.items) |desc| {
            dest.appendStaticBoolAttr(allocator, desc.elem_id, desc.field, desc.value);
        }
        for (replacement.signal_bool_attrs.items) |desc| {
            const signal = desc.signal.cloneRetained(allocator, &self.pending_roc_metrics);
            dest.appendSignalBoolAttr(allocator, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.field, signal, desc.read);
            dest.signal_bool_attrs.items[dest.signal_bool_attrs.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(desc.cached_value, &self.pending_roc_metrics);
        }
        for (replacement.on_changes.items) |desc| {
            const signal = desc.signal.cloneRetained(allocator, &self.pending_roc_metrics);
            dest.appendOnChange(allocator, roc_host, &self.pending_roc_metrics, desc.scope_id, signal, desc.to_cmd);
            dest.on_changes.items[dest.on_changes.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(desc.cached_value, &self.pending_roc_metrics);
        }
        for (replacement.cleanups.items) |desc| {
            dest.appendCleanup(allocator, desc.scope_id, desc.name);
        }
        for (replacement.events.items, 0..) |desc, event_index| {
            self.appendEventDescriptorFromStream(roc_host, dest, desc, event_index);
        }

        for (replacement.scope_sites.items) |desc| {
            dest.appendScopeSiteAt(allocator, desc.node_id, desc.scope_id, desc.ordinal, desc.parent_elem_id, render_insert_offset + desc.render_insert_index, desc.kind, desc.binder_bindings);
        }
        for (replacement.states.items) |desc| {
            dest.appendState(allocator, roc_host, &self.pending_roc_metrics, desc.node_id, desc.initial, desc.eq, desc.drop);
        }
        for (replacement.whens.items) |desc| {
            const condition = desc.condition.cloneRetained(allocator, &self.pending_roc_metrics);
            dest.appendWhen(allocator, roc_host, &self.pending_roc_metrics, desc.node_id, condition, desc.read, desc.when_false, desc.when_true);
            dest.whens.items[dest.whens.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(desc.cached_value, &self.pending_roc_metrics);
        }
        for (replacement.eaches.items) |desc| {
            const items = desc.items.cloneRetained(allocator, &self.pending_roc_metrics);
            dest.appendEach(allocator, roc_host, &self.pending_roc_metrics, desc.node_id, items, desc.items_to_values, desc.key_hash, desc.key_of, desc.key_eq, desc.key_drop, desc.item_eq, desc.item_drop, desc.row);
            dest.eaches.items[dest.eaches.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(desc.cached_value, &self.pending_roc_metrics);
        }
    }

    fn copyActiveStreamReplacingTarget(self: *HostEnv, roc_host: *abi.RocHost, dest: *HostNodeDescriptorStream, target: HostStructuralReplacementTarget, render_insert_index: usize, replacement: *const HostNodeDescriptorStream) void {
        const previous = &self.active_stream;
        if (render_insert_index > previous.render_nodes.items.len) failHost("structural replacement render insertion point is outside the active stream");

        const removed_render_count = self.countRenderNodesInReplacementTarget(previous, target);
        const replacement_render_count = replacement.render_nodes.items.len;
        var inserted_replacement = false;

        for (previous.render_nodes.items, 0..) |node, index| {
            if (!inserted_replacement and index == render_insert_index) {
                for (replacement.render_nodes.items) |replacement_node| {
                    self.appendRenderNodeDescriptorFromStream(roc_host, dest, replacement, replacement_node);
                }
                inserted_replacement = true;
            }

            if (self.renderNodeInReplacementTarget(previous, node, target)) continue;
            self.appendRenderNodeDescriptorFromStream(roc_host, dest, previous, node);
        }

        if (!inserted_replacement) {
            if (render_insert_index != previous.render_nodes.items.len) failHost("structural replacement render insertion point was not reached");
            for (replacement.render_nodes.items) |replacement_node| {
                self.appendRenderNodeDescriptorFromStream(roc_host, dest, replacement, replacement_node);
            }
        }

        self.appendPreviousNonRenderDescriptorsExcludingTarget(roc_host, dest, target, render_insert_index, removed_render_count, replacement_render_count);
        self.appendReplacementNonRenderDescriptors(roc_host, dest, replacement, render_insert_index);
    }

    fn copyActiveStreamReplacingScope(self: *HostEnv, roc_host: *abi.RocHost, dest: *HostNodeDescriptorStream, replaced_scope_id: u64, render_insert_index: usize, replacement: *const HostNodeDescriptorStream) void {
        self.copyActiveStreamReplacingTarget(roc_host, dest, .{ .scope = replaced_scope_id }, render_insert_index, replacement);
    }

    fn deinitActiveSignalTextNodeDesc(self: *HostEnv, roc_host: *abi.RocHost, desc: *HostNodeSignalTextNodeDesc) void {
        desc.cached_value.deinit(roc_host, &self.pending_roc_metrics);
        desc.signal.deinit(self.gpa.allocator(), roc_host, &self.pending_roc_metrics);
        self.pending_roc_metrics.closure_releases += 1;
        abi.decrefErasedCallable(desc.read, roc_host);
    }

    fn deinitActiveSignalTextAttrDesc(self: *HostEnv, roc_host: *abi.RocHost, desc: *HostNodeSignalTextAttrDesc) void {
        desc.cached_value.deinit(roc_host, &self.pending_roc_metrics);
        desc.signal.deinit(self.gpa.allocator(), roc_host, &self.pending_roc_metrics);
        self.pending_roc_metrics.closure_releases += 1;
        abi.decrefErasedCallable(desc.read, roc_host);
    }

    fn deinitActiveSignalBoolAttrDesc(self: *HostEnv, roc_host: *abi.RocHost, desc: *HostNodeSignalBoolAttrDesc) void {
        desc.cached_value.deinit(roc_host, &self.pending_roc_metrics);
        desc.signal.deinit(self.gpa.allocator(), roc_host, &self.pending_roc_metrics);
        self.pending_roc_metrics.closure_releases += 1;
        abi.decrefErasedCallable(desc.read, roc_host);
    }

    fn deinitActiveOnChangeDesc(self: *HostEnv, roc_host: *abi.RocHost, desc: *HostNodeOnChangeDesc) void {
        desc.cached_value.deinit(roc_host, &self.pending_roc_metrics);
        desc.signal.deinit(self.gpa.allocator(), roc_host, &self.pending_roc_metrics);
        self.pending_roc_metrics.closure_releases += 1;
        abi.decrefErasedCallable(desc.to_cmd, roc_host);
    }

    fn deinitActiveEventDesc(self: *HostEnv, roc_host: *abi.RocHost, desc: HostActiveEventDesc) void {
        abi.decrefBox(@ptrCast(desc.payload_tag), roc_host);
        abi.decrefErasedCallable(desc.payload_drop, roc_host);
        abi.decrefErasedCallable(desc.transform, roc_host);
        self.pending_roc_metrics.closure_releases += 2;
    }

    fn removeActiveElementDescriptorsInTarget(self: *HostEnv, target: HostStructuralReplacementTarget) void {
        const allocator = self.gpa.allocator();
        var write_index: usize = 0;
        self.recordStreamNodesScanned(self.active_stream.elements.items.len);
        for (self.active_stream.elements.items, 0..) |desc, read_index| {
            if (self.scopeIsInReplacementTarget(desc.scope_id, target)) {
                self.active_stream.clearElementIndex(desc.elem_id, read_index);
                allocator.free(desc.tag);
                continue;
            }
            self.active_stream.elements.items[write_index] = desc;
            self.active_stream.updateElementIndex(desc.elem_id, write_index);
            write_index += 1;
        }
        self.active_stream.elements.items.len = write_index;
    }

    fn removeActiveTextNodeDescriptorsInTarget(self: *HostEnv, target: HostStructuralReplacementTarget) void {
        const allocator = self.gpa.allocator();
        var write_index: usize = 0;
        self.recordStreamNodesScanned(self.active_stream.text_nodes.items.len);
        for (self.active_stream.text_nodes.items, 0..) |desc, read_index| {
            if (self.scopeIsInReplacementTarget(desc.scope_id, target)) {
                self.active_stream.clearTextNodeIndex(desc.elem_id, read_index);
                allocator.free(desc.value);
                continue;
            }
            self.active_stream.text_nodes.items[write_index] = desc;
            self.active_stream.updateTextNodeIndex(desc.elem_id, write_index);
            write_index += 1;
        }
        self.active_stream.text_nodes.items.len = write_index;
    }

    fn removeActiveSignalTextNodeDescriptorsInTarget(self: *HostEnv, roc_host: *abi.RocHost, target: HostStructuralReplacementTarget) void {
        var write_index: usize = 0;
        self.recordStreamNodesScanned(self.active_stream.signal_text_nodes.items.len);
        for (self.active_stream.signal_text_nodes.items, 0..) |desc, read_index| {
            if (self.scopeIsInReplacementTarget(desc.scope_id, target)) {
                var removed = desc;
                self.active_stream.clearSignalTextNodeIndex(removed.elem_id, read_index);
                self.releaseActiveSignalRecord(removed.signal.record);
                self.deinitActiveSignalTextNodeDesc(roc_host, &removed);
                continue;
            }
            self.active_stream.signal_text_nodes.items[write_index] = desc;
            self.active_stream.updateSignalTextNodeIndex(desc.elem_id, write_index);
            write_index += 1;
        }
        self.active_stream.signal_text_nodes.items.len = write_index;
    }

    fn removeActiveStaticTextAttrDescriptorsInTarget(self: *HostEnv, target: HostStructuralReplacementTarget) void {
        const allocator = self.gpa.allocator();
        var write_index: usize = 0;
        self.recordStreamNodesScanned(self.active_stream.static_text_attrs.items.len);
        for (self.active_stream.static_text_attrs.items, 0..) |desc, read_index| {
            if (self.elemIdInReplacementTarget(&self.active_stream, desc.elem_id, target)) {
                self.active_stream.clearStaticTextAttrIndex(desc.elem_id, desc.field, read_index);
                allocator.free(desc.value);
                continue;
            }
            self.active_stream.static_text_attrs.items[write_index] = desc;
            self.active_stream.updateStaticTextAttrIndex(desc.elem_id, desc.field, write_index);
            write_index += 1;
        }
        self.active_stream.static_text_attrs.items.len = write_index;
    }

    fn removeActiveSignalTextAttrDescriptorsInTarget(self: *HostEnv, roc_host: *abi.RocHost, target: HostStructuralReplacementTarget) void {
        var write_index: usize = 0;
        self.recordStreamNodesScanned(self.active_stream.signal_text_attrs.items.len);
        for (self.active_stream.signal_text_attrs.items, 0..) |desc, read_index| {
            if (self.elemIdInReplacementTarget(&self.active_stream, desc.elem_id, target)) {
                var removed = desc;
                self.active_stream.clearSignalTextAttrIndex(removed.elem_id, removed.field, read_index);
                self.releaseActiveSignalRecord(removed.signal.record);
                self.deinitActiveSignalTextAttrDesc(roc_host, &removed);
                continue;
            }
            self.active_stream.signal_text_attrs.items[write_index] = desc;
            self.active_stream.updateSignalTextAttrIndex(desc.elem_id, desc.field, write_index);
            write_index += 1;
        }
        self.active_stream.signal_text_attrs.items.len = write_index;
    }

    fn removeActiveStaticBoolAttrDescriptorsInTarget(self: *HostEnv, target: HostStructuralReplacementTarget) void {
        var write_index: usize = 0;
        self.recordStreamNodesScanned(self.active_stream.static_bool_attrs.items.len);
        for (self.active_stream.static_bool_attrs.items, 0..) |desc, read_index| {
            if (self.elemIdInReplacementTarget(&self.active_stream, desc.elem_id, target)) {
                self.active_stream.clearStaticBoolAttrIndex(desc.elem_id, desc.field, read_index);
                continue;
            }
            self.active_stream.static_bool_attrs.items[write_index] = desc;
            self.active_stream.updateStaticBoolAttrIndex(desc.elem_id, desc.field, write_index);
            write_index += 1;
        }
        self.active_stream.static_bool_attrs.items.len = write_index;
    }

    fn removeActiveSignalBoolAttrDescriptorsInTarget(self: *HostEnv, roc_host: *abi.RocHost, target: HostStructuralReplacementTarget) void {
        var write_index: usize = 0;
        self.recordStreamNodesScanned(self.active_stream.signal_bool_attrs.items.len);
        for (self.active_stream.signal_bool_attrs.items, 0..) |desc, read_index| {
            if (self.elemIdInReplacementTarget(&self.active_stream, desc.elem_id, target)) {
                var removed = desc;
                self.active_stream.clearSignalBoolAttrIndex(removed.elem_id, removed.field, read_index);
                self.releaseActiveSignalRecord(removed.signal.record);
                self.deinitActiveSignalBoolAttrDesc(roc_host, &removed);
                continue;
            }
            self.active_stream.signal_bool_attrs.items[write_index] = desc;
            self.active_stream.updateSignalBoolAttrIndex(desc.elem_id, desc.field, write_index);
            write_index += 1;
        }
        self.active_stream.signal_bool_attrs.items.len = write_index;
    }

    fn removeActiveOnChangeDescriptorsInTarget(self: *HostEnv, roc_host: *abi.RocHost, target: HostStructuralReplacementTarget) void {
        var write_index: usize = 0;
        self.recordStreamNodesScanned(self.active_stream.on_changes.items.len);
        for (self.active_stream.on_changes.items) |desc| {
            if (self.scopeIsInReplacementTarget(desc.scope_id, target)) {
                var removed = desc;
                self.releaseActiveSignalRecord(removed.signal.record);
                self.deinitActiveOnChangeDesc(roc_host, &removed);
                continue;
            }
            self.active_stream.on_changes.items[write_index] = desc;
            write_index += 1;
        }
        self.active_stream.on_changes.items.len = write_index;
    }

    fn removeActiveCleanupDescriptorsInTarget(self: *HostEnv, target: HostStructuralReplacementTarget) void {
        const allocator = self.gpa.allocator();
        var write_index: usize = 0;
        self.recordStreamNodesScanned(self.active_stream.cleanups.items.len);
        for (self.active_stream.cleanups.items) |desc| {
            if (self.scopeIsInReplacementTarget(desc.scope_id, target)) {
                allocator.free(desc.name);
                continue;
            }
            self.active_stream.cleanups.items[write_index] = desc;
            write_index += 1;
        }
        self.active_stream.cleanups.items.len = write_index;
    }

    fn removeActiveEventDescriptorsInTarget(self: *HostEnv, roc_host: *abi.RocHost, target: HostStructuralReplacementTarget) void {
        if (self.active_events.items.len != self.active_stream.events.items.len) {
            if (self.active_stream.events.items.len != 0) failHost("active event descriptor table is out of sync with active events");
        }

        var write_index: usize = 0;
        self.recordStreamNodesScanned(self.active_stream.events.items.len);
        for (self.active_stream.events.items, 0..) |desc, event_index| {
            if (self.elemIdInReplacementTarget(&self.active_stream, desc.elem_id, target)) {
                if (desc.owns_payload_tag or desc.owns_payload_drop or desc.owns_transform) {
                    failHost("active event descriptor retained ownership outside the active event table");
                }
                self.active_stream.clearEventIndex(desc.elem_id, desc.kind, event_index);
                if (self.active_events.items.len != 0) {
                    self.deinitActiveEventDesc(roc_host, self.active_events.items[event_index]);
                }
                continue;
            }

            self.active_stream.events.items[write_index] = desc;
            self.active_stream.updateEventIndex(desc.elem_id, desc.kind, write_index);
            if (self.active_events.items.len != 0) {
                self.active_events.items[write_index] = self.active_events.items[event_index];
            }
            write_index += 1;
        }
        self.active_stream.events.items.len = write_index;
        if (self.active_events.items.len != 0) self.active_events.items.len = write_index;
    }

    fn removeActiveScopeSiteDescriptorsInTarget(self: *HostEnv, target: HostStructuralReplacementTarget) void {
        const allocator = self.gpa.allocator();
        var write_index: usize = 0;
        self.recordStreamNodesScanned(self.active_stream.scope_sites.items.len);
        for (self.active_stream.scope_sites.items) |desc| {
            if (self.scopeIsInReplacementTarget(desc.scope_id, target)) {
                allocator.free(desc.binder_bindings);
                continue;
            }
            self.active_stream.scope_sites.items[write_index] = desc;
            write_index += 1;
        }
        self.active_stream.scope_sites.items.len = write_index;
    }

    fn removeActiveStateDescriptorsInTarget(self: *HostEnv, roc_host: *abi.RocHost, target: HostStructuralReplacementTarget) void {
        var write_index: usize = 0;
        self.recordStreamNodesScanned(self.active_stream.states.items.len);
        for (self.active_stream.states.items) |desc| {
            if (self.streamNodeIdInReplacementTarget(&self.active_stream, desc.node_id, target)) {
                self.pending_roc_metrics.closure_releases += 3;
                abi.decrefErasedCallable(desc.initial, roc_host);
                abi.decrefErasedCallable(desc.eq, roc_host);
                abi.decrefErasedCallable(desc.drop, roc_host);
                continue;
            }
            self.active_stream.states.items[write_index] = desc;
            write_index += 1;
        }
        self.active_stream.states.items.len = write_index;
    }

    fn removeActiveWhenDescriptorsInTarget(self: *HostEnv, roc_host: *abi.RocHost, target: HostStructuralReplacementTarget) void {
        var write_index: usize = 0;
        self.recordStreamNodesScanned(self.active_stream.whens.items.len);
        for (self.active_stream.whens.items) |desc| {
            if (self.streamNodeIdInReplacementTarget(&self.active_stream, desc.node_id, target)) {
                var removed = desc;
                self.releaseActiveSignalRecord(removed.condition.record);
                removed.cached_value.deinit(roc_host, &self.pending_roc_metrics);
                removed.condition.deinit(self.gpa.allocator(), roc_host, &self.pending_roc_metrics);
                self.pending_roc_metrics.closure_releases += 1;
                abi.decrefErasedCallable(removed.read, roc_host);
                abi.decrefElem(removed.when_false, roc_host);
                abi.decrefElem(removed.when_true, roc_host);
                continue;
            }
            self.active_stream.whens.items[write_index] = desc;
            write_index += 1;
        }
        self.active_stream.whens.items.len = write_index;
    }

    fn removeActiveEachDescriptorsInTarget(self: *HostEnv, roc_host: *abi.RocHost, target: HostStructuralReplacementTarget) void {
        var write_index: usize = 0;
        self.recordStreamNodesScanned(self.active_stream.eaches.items.len);
        for (self.active_stream.eaches.items) |desc| {
            if (self.streamNodeIdInReplacementTarget(&self.active_stream, desc.node_id, target)) {
                var removed = desc;
                self.releaseActiveSignalRecord(removed.items.record);
                removed.cached_value.deinit(roc_host, &self.pending_roc_metrics);
                removed.items.deinit(self.gpa.allocator(), roc_host, &self.pending_roc_metrics);
                self.pending_roc_metrics.closure_releases += 8;
                abi.decrefErasedCallable(removed.items_to_values, roc_host);
                abi.decrefErasedCallable(removed.key_hash, roc_host);
                abi.decrefErasedCallable(removed.key_of, roc_host);
                abi.decrefErasedCallable(removed.key_eq, roc_host);
                abi.decrefErasedCallable(removed.key_drop, roc_host);
                abi.decrefErasedCallable(removed.item_eq, roc_host);
                abi.decrefErasedCallable(removed.item_drop, roc_host);
                abi.decrefErasedCallable(removed.row, roc_host);
                continue;
            }
            self.active_stream.eaches.items[write_index] = desc;
            write_index += 1;
        }
        self.active_stream.eaches.items.len = write_index;
    }

    fn removeActiveNonRenderDescriptorsInTarget(self: *HostEnv, roc_host: *abi.RocHost, target: HostStructuralReplacementTarget) void {
        self.removeActiveStaticTextAttrDescriptorsInTarget(target);
        self.removeActiveSignalTextAttrDescriptorsInTarget(roc_host, target);
        self.removeActiveStaticBoolAttrDescriptorsInTarget(target);
        self.removeActiveSignalBoolAttrDescriptorsInTarget(roc_host, target);
        self.removeActiveOnChangeDescriptorsInTarget(roc_host, target);
        self.removeActiveCleanupDescriptorsInTarget(target);
        self.removeActiveEventDescriptorsInTarget(roc_host, target);
        self.removeActiveStateDescriptorsInTarget(roc_host, target);
        self.removeActiveWhenDescriptorsInTarget(roc_host, target);
        self.removeActiveEachDescriptorsInTarget(roc_host, target);
        self.removeActiveScopeSiteDescriptorsInTarget(target);
        self.removeActiveElementDescriptorsInTarget(target);
        self.removeActiveTextNodeDescriptorsInTarget(target);
        self.removeActiveSignalTextNodeDescriptorsInTarget(roc_host, target);
    }

    fn adjustActiveScopeSiteRenderInsertIndices(self: *HostEnv, replace_index: usize, removed_render_count: usize, replacement_render_count: usize) void {
        for (self.active_stream.scope_sites.items) |*desc| {
            desc.render_insert_index = HostEnv.adjustedRenderInsertIndex(desc.render_insert_index, replace_index, removed_render_count, replacement_render_count);
        }
    }

    fn appendReplacementEventsMoved(self: *HostEnv, replacement: *HostNodeDescriptorStream) void {
        const allocator = self.gpa.allocator();
        if (self.active_events.items.len != self.active_stream.events.items.len) {
            if (self.active_stream.events.items.len != 0) failHost("active event descriptor table is out of sync before replacement event splice");
        }

        const event_base = self.active_stream.events.items.len;
        for (replacement.events.items, 0..) |*desc, offset| {
            if (!desc.owns_payload_tag or !desc.owns_payload_drop or !desc.owns_transform) {
                failHost("replacement event descriptor did not own its retained payload");
            }
            self.active_stream.recordEventIndex(allocator, desc.elem_id, desc.kind, event_base + offset);
            self.active_events.append(allocator, .{
                .target_node_id = desc.target_node_id,
                .payload_kind = desc.payload_kind,
                .payload_tag = desc.payload_tag,
                .payload_drop = desc.payload_drop,
                .transform = desc.transform,
            }) catch std.process.exit(1);
            desc.owns_payload_tag = false;
            desc.owns_payload_drop = false;
            desc.owns_transform = false;
        }

        self.active_stream.events.appendSlice(allocator, replacement.events.items) catch std.process.exit(1);
        replacement.events.items.len = 0;
    }

    fn appendReplacementNonRenderDescriptorsMoved(self: *HostEnv, replacement: *HostNodeDescriptorStream, render_insert_offset: usize) void {
        const allocator = self.gpa.allocator();

        const element_base = self.active_stream.elements.items.len;
        for (replacement.elements.items, 0..) |desc, offset| {
            self.active_stream.recordElementIndex(allocator, desc.elem_id, element_base + offset);
        }
        self.active_stream.elements.appendSlice(allocator, replacement.elements.items) catch std.process.exit(1);
        replacement.elements.items.len = 0;

        const text_node_base = self.active_stream.text_nodes.items.len;
        for (replacement.text_nodes.items, 0..) |desc, offset| {
            self.active_stream.recordTextNodeIndex(allocator, desc.elem_id, text_node_base + offset);
        }
        self.active_stream.text_nodes.appendSlice(allocator, replacement.text_nodes.items) catch std.process.exit(1);
        replacement.text_nodes.items.len = 0;

        const signal_text_node_base = self.active_stream.signal_text_nodes.items.len;
        for (replacement.signal_text_nodes.items, 0..) |desc, offset| {
            self.active_stream.recordSignalTextNodeIndex(allocator, desc.elem_id, signal_text_node_base + offset);
        }
        for (replacement.signal_text_nodes.items) |desc| {
            self.retainActiveSignalRecord(desc.signal.record);
        }
        self.active_stream.signal_text_nodes.appendSlice(allocator, replacement.signal_text_nodes.items) catch std.process.exit(1);
        replacement.signal_text_nodes.items.len = 0;

        const static_text_attr_base = self.active_stream.static_text_attrs.items.len;
        for (replacement.static_text_attrs.items, 0..) |desc, offset| {
            self.active_stream.recordStaticTextAttrIndex(allocator, desc.elem_id, desc.field, static_text_attr_base + offset);
        }
        self.active_stream.static_text_attrs.appendSlice(allocator, replacement.static_text_attrs.items) catch std.process.exit(1);
        replacement.static_text_attrs.items.len = 0;

        const signal_text_attr_base = self.active_stream.signal_text_attrs.items.len;
        for (replacement.signal_text_attrs.items, 0..) |desc, offset| {
            self.active_stream.recordSignalTextAttrIndex(allocator, desc.elem_id, desc.field, signal_text_attr_base + offset);
        }
        for (replacement.signal_text_attrs.items) |desc| {
            self.retainActiveSignalRecord(desc.signal.record);
        }
        self.active_stream.signal_text_attrs.appendSlice(allocator, replacement.signal_text_attrs.items) catch std.process.exit(1);
        replacement.signal_text_attrs.items.len = 0;

        const static_bool_attr_base = self.active_stream.static_bool_attrs.items.len;
        for (replacement.static_bool_attrs.items, 0..) |desc, offset| {
            self.active_stream.recordStaticBoolAttrIndex(allocator, desc.elem_id, desc.field, static_bool_attr_base + offset);
        }
        self.active_stream.static_bool_attrs.appendSlice(allocator, replacement.static_bool_attrs.items) catch std.process.exit(1);
        replacement.static_bool_attrs.items.len = 0;

        const signal_bool_attr_base = self.active_stream.signal_bool_attrs.items.len;
        for (replacement.signal_bool_attrs.items, 0..) |desc, offset| {
            self.active_stream.recordSignalBoolAttrIndex(allocator, desc.elem_id, desc.field, signal_bool_attr_base + offset);
        }
        for (replacement.signal_bool_attrs.items) |desc| {
            self.retainActiveSignalRecord(desc.signal.record);
        }
        self.active_stream.signal_bool_attrs.appendSlice(allocator, replacement.signal_bool_attrs.items) catch std.process.exit(1);
        replacement.signal_bool_attrs.items.len = 0;

        for (replacement.on_changes.items) |desc| {
            self.retainActiveSignalRecord(desc.signal.record);
        }
        self.active_stream.on_changes.appendSlice(allocator, replacement.on_changes.items) catch std.process.exit(1);
        replacement.on_changes.items.len = 0;

        self.active_stream.cleanups.appendSlice(allocator, replacement.cleanups.items) catch std.process.exit(1);
        replacement.cleanups.items.len = 0;

        self.appendReplacementEventsMoved(replacement);

        for (replacement.scope_sites.items) |*desc| {
            desc.render_insert_index += render_insert_offset;
        }
        self.active_stream.scope_sites.appendSlice(allocator, replacement.scope_sites.items) catch std.process.exit(1);
        replacement.scope_sites.items.len = 0;

        self.active_stream.states.appendSlice(allocator, replacement.states.items) catch std.process.exit(1);
        replacement.states.items.len = 0;

        for (replacement.whens.items) |desc| {
            self.retainActiveSignalRecord(desc.condition.record);
        }
        self.active_stream.whens.appendSlice(allocator, replacement.whens.items) catch std.process.exit(1);
        replacement.whens.items.len = 0;

        for (replacement.eaches.items) |desc| {
            self.retainActiveSignalRecord(desc.items.record);
        }
        self.active_stream.eaches.appendSlice(allocator, replacement.eaches.items) catch std.process.exit(1);
        replacement.eaches.items.len = 0;
    }

    fn rebuildActiveStreamSignalRecordTable(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
        self.active_stream.signal_records_by_token.items.len = 0;

        for (self.active_stream.signal_text_nodes.items) |desc| {
            self.active_stream.rememberSignalRecordTree(allocator, desc.signal.record);
        }
        for (self.active_stream.signal_text_attrs.items) |desc| {
            self.active_stream.rememberSignalRecordTree(allocator, desc.signal.record);
        }
        for (self.active_stream.signal_bool_attrs.items) |desc| {
            self.active_stream.rememberSignalRecordTree(allocator, desc.signal.record);
        }
        for (self.active_stream.on_changes.items) |desc| {
            self.active_stream.rememberSignalRecordTree(allocator, desc.signal.record);
        }
        for (self.active_stream.whens.items) |desc| {
            self.active_stream.rememberSignalRecordTree(allocator, desc.condition.record);
        }
        for (self.active_stream.eaches.items) |desc| {
            self.active_stream.rememberSignalRecordTree(allocator, desc.items.record);
        }
    }

    fn validateActiveRenderDescriptorIntegrity(self: *HostEnv) void {
        for (self.active_stream.render_nodes.items) |node| {
            const found = switch (node.kind) {
                .element => findElementDesc(&self.active_stream, node.elem_id) != null,
                .text => findTextNodeDesc(&self.active_stream, node.elem_id) != null,
                .signal_text => findSignalTextNodeDesc(&self.active_stream, node.elem_id) != null,
            };
            if (!found) {
                var message: [128]u8 = undefined;
                const rendered = std.fmt.bufPrint(
                    &message,
                    "active render node {d} with kind {s} has no matching descriptor",
                    .{ node.elem_id, @tagName(node.kind) },
                ) catch "active render node has no matching descriptor";
                failHost(rendered);
            }
        }
    }

    fn spliceActiveStreamReplacingTarget(self: *HostEnv, roc_host: *abi.RocHost, target: HostStructuralReplacementTarget, render_insert_index: usize, replacement: *HostNodeDescriptorStream) HostStructuralSplice {
        const allocator = self.gpa.allocator();
        if (render_insert_index > self.active_stream.render_nodes.items.len) failHost("structural replacement render insertion point is outside the active stream");

        var removed_elem_ids: std.ArrayListUnmanaged(u64) = .empty;
        errdefer removed_elem_ids.deinit(allocator);
        var touched_parent_ids: std.ArrayListUnmanaged(u64) = .empty;
        errdefer touched_parent_ids.deinit(allocator);

        var removed_render_start: ?usize = null;
        var removed_render_count: usize = 0;
        var target_range_closed = false;

        self.recordStreamNodesScanned(self.active_stream.render_nodes.items.len);
        for (self.active_stream.render_nodes.items, 0..) |node, index| {
            if (self.renderNodeInReplacementTarget(&self.active_stream, node, target)) {
                if (target_range_closed) failHost("structural replacement render target is not contiguous");
                if (removed_render_start == null) removed_render_start = index;
                removed_render_count += 1;
                removed_elem_ids.append(allocator, node.elem_id) catch std.process.exit(1);
            } else if (removed_render_start != null) {
                target_range_closed = true;
            }
        }

        self.recordStreamNodesScanned(self.active_stream.render_nodes.items.len);
        for (self.active_stream.render_nodes.items) |node| {
            if (!self.renderNodeInReplacementTarget(&self.active_stream, node, target)) continue;
            const parent_elem_id = renderNodeParentElemId(&self.active_stream, node);
            if (u64SliceContains(removed_elem_ids.items, parent_elem_id)) continue;
            appendUniqueU64(allocator, &touched_parent_ids, parent_elem_id);
        }

        const render_start = removed_render_start orelse render_insert_index;
        if (removed_render_count != 0 and render_start != render_insert_index) {
            failHost("structural replacement render target did not start at its explicit insertion point");
        }

        const replacement_render_count = replacement.render_nodes.items.len;
        const on_change_count = replacement.on_changes.items.len;
        const replacement_elem_ids = allocator.alloc(u64, replacement_render_count) catch std.process.exit(1);
        errdefer allocator.free(replacement_elem_ids);
        for (replacement.render_nodes.items, 0..) |node, index| {
            replacement_elem_ids[index] = node.elem_id;
        }

        self.removeActiveNonRenderDescriptorsInTarget(roc_host, target);
        self.adjustActiveScopeSiteRenderInsertIndices(render_insert_index, removed_render_count, replacement_render_count);
        const on_change_start = self.active_stream.on_changes.items.len;
        const replacement_on_change_indices = allocator.alloc(usize, on_change_count) catch std.process.exit(1);
        errdefer allocator.free(replacement_on_change_indices);
        for (replacement_on_change_indices, 0..) |*index, offset| {
            index.* = on_change_start + offset;
        }
        self.appendReplacementNonRenderDescriptorsMoved(replacement, render_insert_index);

        self.active_stream.render_nodes.replaceRange(allocator, render_start, removed_render_count, replacement.render_nodes.items) catch std.process.exit(1);
        replacement.render_nodes.items.len = 0;
        self.validateActiveRenderDescriptorIntegrity();
        self.rebuildActiveStreamSignalRecordTable();
        self.rebuildActiveSinkSignalRoutesFromStream(&self.active_stream);

        return .{
            .removed_elem_ids = removed_elem_ids.toOwnedSlice(allocator) catch std.process.exit(1),
            .touched_parent_ids = touched_parent_ids.toOwnedSlice(allocator) catch std.process.exit(1),
            .replacement_elem_ids = replacement_elem_ids,
            .replacement_on_change_indices = replacement_on_change_indices,
        };
    }

    fn spliceActiveStreamReplacingScope(self: *HostEnv, roc_host: *abi.RocHost, replaced_scope_id: u64, render_insert_index: usize, replacement: *HostNodeDescriptorStream) HostStructuralSplice {
        return self.spliceActiveStreamReplacingTarget(roc_host, .{ .scope = replaced_scope_id }, render_insert_index, replacement);
    }

    fn applyDirtyEachRowScopeSplices(self: *HostEnv, roc_host: *abi.RocHost, site: HostNodeScopeSiteDesc, each: HostNodeEachDesc, diff: HostKeyedRowDiffResult, dirty_source_node_ids: []const u64) CommandCounts {
        const allocator = self.gpa.allocator();
        var removed_elem_ids: std.ArrayListUnmanaged(u64) = .empty;
        defer removed_elem_ids.deinit(allocator);
        var touched_parent_ids: std.ArrayListUnmanaged(u64) = .empty;
        defer touched_parent_ids.deinit(allocator);
        var replacement_elem_ids: std.ArrayListUnmanaged(u64) = .empty;
        defer replacement_elem_ids.deinit(allocator);
        var replacement_on_change_indices: std.ArrayListUnmanaged(usize) = .empty;
        defer replacement_on_change_indices.deinit(allocator);
        var spliced_any = false;

        for (diff.removed_scope_ids) |removed_scope_id| {
            var empty_stream: HostNodeDescriptorStream = .{};
            const render_insert_index = self.firstRenderIndexInScopeSubtree(&self.active_stream, removed_scope_id) orelse site.render_insert_index;
            const splice = self.spliceActiveStreamReplacingScope(roc_host, removed_scope_id, render_insert_index, &empty_stream);
            defer splice.deinit(self.gpa.allocator());

            removed_elem_ids.appendSlice(allocator, splice.removed_elem_ids) catch std.process.exit(1);
            for (splice.touched_parent_ids) |parent_id| {
                appendUniqueU64(allocator, &touched_parent_ids, parent_id);
            }
            replacement_elem_ids.appendSlice(allocator, splice.replacement_elem_ids) catch std.process.exit(1);
            replacement_on_change_indices.appendSlice(allocator, splice.replacement_on_change_indices) catch std.process.exit(1);
            spliced_any = true;
        }

        for (diff.scope_ids, diff.row_items_changed, 0..) |row_scope_id, row_item_changed, row_index| {
            if (!row_item_changed and !self.scopeSubtreeHasDirtyStructuralSource(&self.active_stream, row_scope_id, dirty_source_node_ids)) {
                continue;
            }

            var row_stream: HostNodeDescriptorStream = .{};
            defer row_stream.deinit(self.gpa.allocator(), roc_host, &self.pending_roc_metrics);
            self.collectActiveEachSingleRowDescriptors(roc_host, &row_stream, site, each, row_scope_id, dirty_source_node_ids);

            const render_insert_index = self.renderInsertIndexForEachRow(site, diff.scope_ids, row_index);
            const splice = self.spliceActiveStreamReplacingScope(roc_host, row_scope_id, render_insert_index, &row_stream);
            defer splice.deinit(self.gpa.allocator());

            removed_elem_ids.appendSlice(allocator, splice.removed_elem_ids) catch std.process.exit(1);
            for (splice.touched_parent_ids) |parent_id| {
                appendUniqueU64(allocator, &touched_parent_ids, parent_id);
            }
            replacement_elem_ids.appendSlice(allocator, splice.replacement_elem_ids) catch std.process.exit(1);
            replacement_on_change_indices.appendSlice(allocator, splice.replacement_on_change_indices) catch std.process.exit(1);
            spliced_any = true;
        }

        if (!spliced_any) return .{};

        const merged_splice = HostStructuralSplice{
            .removed_elem_ids = removed_elem_ids.toOwnedSlice(allocator) catch std.process.exit(1),
            .touched_parent_ids = touched_parent_ids.toOwnedSlice(allocator) catch std.process.exit(1),
            .replacement_elem_ids = replacement_elem_ids.toOwnedSlice(allocator) catch std.process.exit(1),
            .replacement_on_change_indices = replacement_on_change_indices.toOwnedSlice(allocator) catch std.process.exit(1),
        };
        defer merged_splice.deinit(allocator);
        const target = HostStructuralReplacementTarget{ .each_site = .{ .parent_scope_id = site.scope_id, .site_ordinal = site.ordinal } };
        return applySplicedStructuralNodeDescriptorTarget(self, roc_host, merged_splice, .{
            .removed = target,
            .replacement = target,
        });
    }

    fn copyActiveScopeSubtreeDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, root_scope_id: u64) void {
        const allocator = self.gpa.allocator();
        const previous = &self.active_stream;
        const previous_render_base = self.firstRenderIndexInScopeSubtree(previous, root_scope_id);
        const next_render_base = stream.render_nodes.items.len;
        var copied_elem_ids: std.ArrayListUnmanaged(u64) = .empty;
        defer copied_elem_ids.deinit(allocator);

        for (previous.render_nodes.items) |node| {
            const node_scope_id = HostEnv.renderNodeScopeId(previous, node);
            if (!self.scopeIsDescendantOrSelf(node_scope_id, root_scope_id)) continue;

            copied_elem_ids.append(allocator, node.elem_id) catch std.process.exit(1);
            switch (node.kind) {
                .element => {
                    const desc = findElementDesc(previous, node.elem_id) orelse failMissingRenderDescriptor("copyActiveScopeSubtreeDescriptors", node);
                    _ = stream.appendElement(allocator, desc.elem_id, desc.parent_elem_id, desc.scope_id, desc.tag);
                },
                .text => {
                    const desc = findTextNodeDesc(previous, node.elem_id) orelse failMissingRenderDescriptor("copyActiveScopeSubtreeDescriptors", node);
                    stream.appendTextNode(allocator, desc.elem_id, desc.parent_elem_id, desc.scope_id, desc.value);
                },
                .signal_text => {
                    const desc = findSignalTextNodeDesc(previous, node.elem_id) orelse failMissingRenderDescriptor("copyActiveScopeSubtreeDescriptors", node);
                    const signal = desc.signal.cloneRetained(allocator, &self.pending_roc_metrics);
                    stream.appendSignalTextNode(allocator, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.parent_elem_id, desc.scope_id, signal, desc.read);
                    stream.signal_text_nodes.items[stream.signal_text_nodes.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(desc.cached_value, &self.pending_roc_metrics);
                },
            }
        }

        for (previous.static_text_attrs.items) |desc| {
            if (!u64SliceContains(copied_elem_ids.items, desc.elem_id)) continue;
            stream.appendStaticTextAttr(allocator, desc.elem_id, desc.field, desc.value);
        }
        for (previous.signal_text_attrs.items) |desc| {
            if (!u64SliceContains(copied_elem_ids.items, desc.elem_id)) continue;
            const signal = desc.signal.cloneRetained(allocator, &self.pending_roc_metrics);
            stream.appendSignalTextAttr(allocator, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.field, signal, desc.read);
            stream.signal_text_attrs.items[stream.signal_text_attrs.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(desc.cached_value, &self.pending_roc_metrics);
        }
        for (previous.static_bool_attrs.items) |desc| {
            if (!u64SliceContains(copied_elem_ids.items, desc.elem_id)) continue;
            stream.appendStaticBoolAttr(allocator, desc.elem_id, desc.field, desc.value);
        }
        for (previous.signal_bool_attrs.items) |desc| {
            if (!u64SliceContains(copied_elem_ids.items, desc.elem_id)) continue;
            const signal = desc.signal.cloneRetained(allocator, &self.pending_roc_metrics);
            stream.appendSignalBoolAttr(allocator, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.field, signal, desc.read);
            stream.signal_bool_attrs.items[stream.signal_bool_attrs.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(desc.cached_value, &self.pending_roc_metrics);
        }
        for (previous.on_changes.items) |desc| {
            if (!self.scopeIsDescendantOrSelf(desc.scope_id, root_scope_id)) continue;
            const signal = desc.signal.cloneRetained(allocator, &self.pending_roc_metrics);
            stream.appendOnChange(allocator, roc_host, &self.pending_roc_metrics, desc.scope_id, signal, desc.to_cmd);
            stream.on_changes.items[stream.on_changes.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(desc.cached_value, &self.pending_roc_metrics);
        }
        for (previous.cleanups.items) |desc| {
            if (!self.scopeIsDescendantOrSelf(desc.scope_id, root_scope_id)) continue;
            stream.appendCleanup(allocator, desc.scope_id, desc.name);
        }
        for (previous.events.items, 0..) |desc, event_index| {
            if (!u64SliceContains(copied_elem_ids.items, desc.elem_id)) continue;
            const payload_drop = if (desc.owns_payload_drop) desc.payload_drop else self.activeEventPayloadDropByIndex(event_index);
            const transform = if (desc.owns_transform) desc.transform else self.activeEventTransformByIndex(event_index);
            const payload_tag = if (desc.owns_payload_tag) desc.payload_tag else self.activeEventPayloadTagByIndex(event_index);
            stream.appendEventWithBorrowedPayloadTag(allocator, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.kind, desc.binder_token, desc.target_node_id, desc.payload_kind, payload_tag, payload_drop, transform);
        }

        for (previous.scope_sites.items) |desc| {
            if (!self.scopeIsDescendantOrSelf(desc.scope_id, root_scope_id)) continue;
            const render_insert_index = if (previous_render_base) |render_base| blk: {
                if (desc.render_insert_index < render_base) failHost("copied scope site insertion point precedes its scope subtree");
                break :blk next_render_base + (desc.render_insert_index - render_base);
            } else next_render_base;
            stream.appendScopeSiteAt(allocator, desc.node_id, desc.scope_id, desc.ordinal, desc.parent_elem_id, render_insert_index, desc.kind, desc.binder_bindings);
        }
        for (previous.states.items) |desc| {
            if (!self.streamNodeIdInScopeSubtree(previous, desc.node_id, root_scope_id)) continue;
            stream.appendState(allocator, roc_host, &self.pending_roc_metrics, desc.node_id, desc.initial, desc.eq, desc.drop);
        }
        for (previous.whens.items) |desc| {
            if (!self.streamNodeIdInScopeSubtree(previous, desc.node_id, root_scope_id)) continue;
            const condition = desc.condition.cloneRetained(allocator, &self.pending_roc_metrics);
            stream.appendWhen(allocator, roc_host, &self.pending_roc_metrics, desc.node_id, condition, desc.read, desc.when_false, desc.when_true);
            stream.whens.items[stream.whens.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(desc.cached_value, &self.pending_roc_metrics);
        }
        for (previous.eaches.items) |desc| {
            if (!self.streamNodeIdInScopeSubtree(previous, desc.node_id, root_scope_id)) continue;
            const items = desc.items.cloneRetained(allocator, &self.pending_roc_metrics);
            stream.appendEach(allocator, roc_host, &self.pending_roc_metrics, desc.node_id, items, desc.items_to_values, desc.key_hash, desc.key_of, desc.key_eq, desc.key_drop, desc.item_eq, desc.item_drop, desc.row);
            stream.eaches.items[stream.eaches.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(desc.cached_value, &self.pending_roc_metrics);
        }
    }

    fn collectActiveElemDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, elem: abi.Elem, scope_id: u64, parent_elem_id: u64, ordinal: *u64, dom_ordinal: *u64, binder_stack: *std.ArrayListUnmanaged(HostBinderBinding), dirty_source_node_ids: []const u64) void {
        self.validateScopeId(scope_id);

        const allocator = self.gpa.allocator();
        switch (elem.tag) {
            .Element => {
                const payload = elem.payload.element;
                const elem_id = self.internDomIdentity(scope_id, dom_ordinal.*);
                dom_ordinal.* += 1;
                _ = stream.appendElement(allocator, elem_id, parent_elem_id, scope_id, payload.tag.asSlice());
                for (payload.attrs.items()) |attr| {
                    self.collectNodeAttrDescriptor(roc_host, stream, elem_id, attr, binder_stack.items);
                }
                for (payload.children.items()) |child| {
                    self.collectActiveElemDescriptors(roc_host, stream, child, scope_id, elem_id, ordinal, dom_ordinal, binder_stack, dirty_source_node_ids);
                }
            },
            .Text => {
                const elem_id = self.internDomIdentity(scope_id, dom_ordinal.*);
                dom_ordinal.* += 1;
                stream.appendTextNode(allocator, elem_id, parent_elem_id, scope_id, elem.payload.text.asSlice());
            },
            .TextSignal => {
                const elem_id = self.internDomIdentity(scope_id, dom_ordinal.*);
                dom_ordinal.* += 1;
                const text_signal = elem.payload.text_signal;
                const signal = self.bindNodeSignal(allocator, stream, text_signal.signal.*, binder_stack.items);
                stream.appendSignalTextNode(allocator, roc_host, &self.pending_roc_metrics, elem_id, parent_elem_id, scope_id, signal, text_signal.read);
            },
            .Cleanup => {
                stream.appendCleanup(allocator, scope_id, elem.payload.cleanup.cleanup.asSlice());
            },
            .OnChange => {
                const payload = elem.payload.on_change;
                const signal = self.bindNodeSignal(allocator, stream, payload.signal.*, binder_stack.items);
                stream.appendOnChange(allocator, roc_host, &self.pending_roc_metrics, scope_id, signal, payload.to_cmd);
            },
            .State => {
                const site_ordinal = ordinal.*;
                const node_id = self.internNodeIdentity(scope_id, site_ordinal);
                ordinal.* += 1;
                stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .state, binder_stack.items);
                stream.appendState(allocator, roc_host, &self.pending_roc_metrics, node_id, elem.payload.state.initial, elem.payload.state.eq, elem.payload.state.drop);
                self.ensureStateFromDesc(roc_host, stream.states.items[stream.states.items.len - 1]);
                binder_stack.append(allocator, .{ .token = elem.payload.state.binder, .node_id = node_id }) catch std.process.exit(1);
                self.collectActiveElemDescriptors(roc_host, stream, elem.payload.state.child.*, scope_id, parent_elem_id, ordinal, dom_ordinal, binder_stack, dirty_source_node_ids);
                _ = binder_stack.pop() orelse unreachable;
            },
            .Component => {
                const site_ordinal = ordinal.*;
                const node_id = self.internNodeIdentity(scope_id, site_ordinal);
                ordinal.* += 1;
                stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .component, binder_stack.items);
                const component_scope_id = self.internComponentScope(scope_id, site_ordinal);
                var component_ordinal: u64 = 0;
                var component_dom_ordinal: u64 = 0;
                self.collectActiveElemDescriptors(roc_host, stream, elem.payload.component.child.*, component_scope_id, parent_elem_id, &component_ordinal, &component_dom_ordinal, binder_stack, dirty_source_node_ids);
            },
            .When => {
                const site_ordinal = ordinal.*;
                const node_id = self.internNodeIdentity(scope_id, site_ordinal);
                ordinal.* += 1;
                stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .when, binder_stack.items);
                const condition_binding = self.bindNodeSignal(allocator, stream, elem.payload.when.condition.*, binder_stack.items);
                stream.appendWhen(allocator, roc_host, &self.pending_roc_metrics, node_id, condition_binding, elem.payload.when.read, elem.payload.when.when_false.*, elem.payload.when.when_true.*);

                const when_index = stream.whens.items.len - 1;
                const when_desc = &stream.whens.items[when_index];
                const condition = evalHostSignalBinding(self, roc_host, &when_desc.condition);
                const active_branch: HostScopeBranch = if (callErasedHostValueToBool(roc_host, when_desc.read, condition)) .true_branch else .false_branch;
                when_desc.cached_value.replace(roc_host, &self.pending_roc_metrics, condition, hostSignalBindingEqCallable(self, &when_desc.condition), hostSignalBindingDropCallable(self, &when_desc.condition));
                if (self.activeWhenBranchScopeId(scope_id, site_ordinal, active_branch.opposite())) |inactive_scope_id| {
                    self.disposeScopeSubtree(roc_host, inactive_scope_id);
                }
                const branch_scope_id = self.internWhenBranchScope(scope_id, site_ordinal, active_branch);
                var branch_ordinal: u64 = 0;
                const branch_elem = switch (active_branch) {
                    .true_branch => elem.payload.when.when_true.*,
                    .false_branch => elem.payload.when.when_false.*,
                };
                var branch_dom_ordinal: u64 = 0;
                self.collectActiveElemDescriptors(roc_host, stream, branch_elem, branch_scope_id, parent_elem_id, &branch_ordinal, &branch_dom_ordinal, binder_stack, dirty_source_node_ids);
            },
            .Each => {
                const site_ordinal = ordinal.*;
                const node_id = self.internNodeIdentity(scope_id, site_ordinal);
                ordinal.* += 1;
                stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .each, binder_stack.items);
                const items_binding = self.bindNodeSignal(allocator, stream, elem.payload.each.items.*, binder_stack.items);
                stream.appendEach(
                    allocator,
                    roc_host,
                    &self.pending_roc_metrics,
                    node_id,
                    items_binding,
                    elem.payload.each.items_to_values,
                    elem.payload.each.key_hash,
                    elem.payload.each.key_of,
                    elem.payload.each.key_eq,
                    elem.payload.each.key_drop,
                    elem.payload.each.item_eq,
                    elem.payload.each.item_drop,
                    elem.payload.each.row,
                );
                const each_index = stream.eaches.items.len - 1;
                const each_desc = stream.eaches.items[stream.eaches.items.len - 1];

                const items_value = evalHostSignalBinding(self, roc_host, &stream.eaches.items[each_index].items);
                const items = callErasedHostValueToHostValueList(roc_host, each_desc.items_to_values, items_value);
                defer items.decref(roc_host);
                stream.eaches.items[each_index].cached_value.replace(roc_host, &self.pending_roc_metrics, items_value, hostSignalBindingEqCallable(self, &stream.eaches.items[each_index].items), hostSignalBindingDropCallable(self, &stream.eaches.items[each_index].items));
                const item_values = items.items();

                const keys = allocator.alloc(HostValue, item_values.len) catch std.process.exit(1);
                defer allocator.free(keys);

                for (item_values, 0..) |item, index| {
                    keys[index] = callErasedHostValueToHostValue(roc_host, each_desc.key_of, item);
                }

                const diff = self.syncEachRowScopes(roc_host, scope_id, site_ordinal, keys, item_values, each_desc.key_hash, each_desc.key_eq, each_desc.key_drop, each_desc.item_eq, each_desc.item_drop);
                defer diff.deinit(allocator);

                for (diff.scope_ids, diff.row_items_changed) |row_scope_id, row_item_changed| {
                    if (!row_item_changed and !self.scopeSubtreeHasDirtyStructuralSource(&self.active_stream, row_scope_id, dirty_source_node_ids)) {
                        self.copyActiveScopeSubtreeDescriptors(roc_host, stream, row_scope_id);
                        continue;
                    }

                    const row_values = self.eachRowScopeValues(row_scope_id);
                    const row_elem = callErasedHostValueHostValueToElem(roc_host, each_desc.row, row_values.key, row_values.item);
                    defer abi.decrefElem(row_elem, roc_host);

                    var row_ordinal: u64 = 0;
                    var row_dom_ordinal: u64 = 0;
                    self.collectActiveElemDescriptors(roc_host, stream, row_elem, row_scope_id, parent_elem_id, &row_ordinal, &row_dom_ordinal, binder_stack, dirty_source_node_ids);
                }
            },
        }
    }

    fn collectActiveElemRootDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, root: abi.Elem, dirty_source_node_ids: []const u64) void {
        const root_scope_id = self.internRootScope();
        const allocator = self.gpa.allocator();
        var binder_stack: std.ArrayListUnmanaged(HostBinderBinding) = .empty;
        defer binder_stack.deinit(allocator);
        var ordinal: u64 = 0;
        var dom_ordinal: u64 = 0;
        self.collectActiveElemDescriptors(roc_host, stream, root, root_scope_id, 0, &ordinal, &dom_ordinal, &binder_stack, dirty_source_node_ids);
    }

    fn clearScopes(self: *HostEnv) void {
        if (self.roc_host) |roc_host| {
            for (self.scopes.items) |*scope| {
                if (!scope.active) continue;
                scope.step.deinit(roc_host, &self.pending_roc_metrics);
            }
        } else if (self.scopes.items.len != 0) {
            failHost("host scope table cannot release keys without a Roc host");
        }
        self.scopes.items.len = 0;
    }

    fn deinit(self: *HostEnv) void {
        const allocator = self.gpa.allocator();

        self.clearActiveSignalRoutes();
        self.active_source_signal_routes.deinit(allocator);
        self.active_text_signal_routes.deinit(allocator);
        self.active_bool_signal_routes.deinit(allocator);
        self.active_change_signal_routes.deinit(allocator);
        self.active_structural_signal_routes.deinit(allocator);
        self.clearActiveSignalGraph();
        self.active_signal_graph.deinit(allocator);
        self.active_stream.deinit(allocator, self.roc_host.?, &self.pending_roc_metrics);
        self.clearActiveEvents();
        self.active_events.deinit(allocator);

        self.clearPendingTasks();
        self.pending_tasks.deinit(allocator);

        for (self.cleanup_events.items) |name| {
            allocator.free(name);
        }
        self.cleanup_events.deinit(allocator);

        if (self.root_elem) |root| {
            abi.decrefElem(root, self.roc_host.?);
            self.root_elem = null;
        }

        for (self.dom_elements.items) |*elem| {
            elem.deinit(allocator);
        }
        self.dom_elements.deinit(allocator);
        self.clearEventDescriptors();
        self.event_descriptors.deinit(allocator);
        self.clearSignalEventRoutes();
        self.signal_event_routes.deinit(allocator);
        self.clearSignalDescriptors();
        self.signal_descriptors.deinit(allocator);
        self.clearSignalRoutes();
        self.signal_routes.deinit(allocator);
        self.clearSignalDependents();
        self.signal_dependents.deinit(allocator);
        self.clearSignalCache();
        self.signal_cache.deinit(allocator);
        self.clearStates();
        self.states.deinit(allocator);
        self.clearScopes();
        self.scopes.deinit(allocator);
        self.node_identities.deinit(allocator);
        self.dom_identities.deinit(allocator);

        freeSpecCommands(allocator, self.test_state.commands);

        for (self.host_values.items) |slot| {
            switch (slot) {
                .vacant => {},
                .occupied => failHost("host value registry still owned a typed cell at shutdown"),
            }
        }
        self.host_values.deinit(allocator);
        self.test_host_value_kinds.deinit(allocator);

        for (self.roc_allocations.items) |alloc| {
            allocator.rawFree(alloc.ptr[0..alloc.total_size], alloc.alignment, @returnAddress());
        }
        self.roc_allocations.deinit(allocator);
    }

    fn implicitRole(elem: *const DomElement) ?[]const u8 {
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

    fn accessibleName(elem: *const DomElement) []const u8 {
        if (elem.label) |label| return label;
        if (elem.text) |text| return text;
        if (elem.value) |value| return value;
        return "";
    }

    fn locatorMatches(self: *HostEnv, elem: *const DomElement, locator: Locator) bool {
        _ = self;
        return switch (locator.kind) {
            .none => false,
            .role_name => blk: {
                const role = HostEnv.implicitRole(elem) orelse break :blk false;
                const expected_role = locator.role orelse break :blk false;
                const expected_name = locator.name orelse break :blk false;
                break :blk std.mem.eql(u8, role, expected_role) and std.mem.eql(u8, HostEnv.accessibleName(elem), expected_name);
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

    fn findElementByLocator(self: *HostEnv, locator: Locator, line_num: usize) ?*DomElement {
        var found: ?*DomElement = null;
        var match_count: usize = 0;
        for (self.dom_elements.items) |*elem| {
            if (!elem.active) continue;
            if (!self.locatorMatches(elem, locator)) continue;
            match_count += 1;
            if (found == null) found = elem;
        }

        if (match_count > 1) {
            var buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}: locator matched {d} elements\n", .{ line_num, match_count }) catch "TEST FAILED: ambiguous locator\n";
            writeStderr(msg);
            if (self.test_state.verbose) self.dumpDom();
            return null;
        }
        if (found == null and self.test_state.verbose) self.dumpDom();
        return found;
    }

    fn countElementsByLocator(self: *HostEnv, locator: Locator) usize {
        var match_count: usize = 0;
        for (self.dom_elements.items) |*elem| {
            if (!elem.active) continue;
            if (!self.locatorMatches(elem, locator)) continue;
            match_count += 1;
        }
        return match_count;
    }

    fn dumpDom(self: *HostEnv) void {
        for (self.dom_elements.items, 0..) |elem, idx| {
            var dbg_buf: [512]u8 = undefined;
            const dbg_msg = std.fmt.bufPrint(&dbg_buf, "[DEBUG] elem[{d}] tag=\"{s}\" text=\"{s}\" value=\"{s}\" parent={?d} children={d} active={} updates={d}\n", .{
                idx,
                elem.tag,
                elem.text orelse "(null)",
                elem.value orelse "(null)",
                elem.parent_id,
                elem.children.items.len,
                elem.active,
                elem.text_update_count + elem.value_update_count + elem.checked_update_count + elem.disabled_update_count,
            }) catch "";
            writeStderr(dbg_msg);
        }
    }
};

fn zeroRuntimeMetrics() RuntimeMetrics {
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
        .nodes_recomputed = 0,
        .patches_emitted = 0,
        .propagation_prunes = 0,
        .recompute_batches = 0,
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

var current_host: ?*HostEnv = null;
var current_roc_host: ?*abi.RocHost = null;

fn hostFromRocHost(roc_host: *abi.RocHost) *HostEnv {
    return @ptrCast(@alignCast(roc_host.env));
}

fn currentRocHost() *abi.RocHost {
    return current_roc_host orelse @panic("signals RocHost is not initialized");
}

fn rocAllocationHeaderBytes(byte_alignment: usize) usize {
    return std.mem.alignForward(usize, @sizeOf(RocAllocationHeader), byte_alignment);
}

fn rocAllocationHeaderFromUserPtr(ptr: *anyopaque) *RocAllocationHeader {
    return @ptrFromInt(@intFromPtr(ptr) - @sizeOf(RocAllocationHeader));
}

fn rocAllocationUserPtr(alloc: RocAllocation) *anyopaque {
    return @ptrFromInt(@intFromPtr(alloc.ptr) + rocAllocationHeaderBytes(alloc.alignment.toByteUnits()));
}

fn removeRocAllocationAt(host: *HostEnv, ledger_index: usize, base_ptr: [*]u8) void {
    if (ledger_index >= host.roc_allocations.items.len) {
        failHost("Roc allocation header referenced an unknown ledger index");
    }
    const recorded = host.roc_allocations.items[ledger_index];
    if (recorded.ptr != base_ptr) {
        failHost("Roc allocation header did not match its ledger slot");
    }

    const last_index = host.roc_allocations.items.len - 1;
    _ = host.roc_allocations.swapRemove(ledger_index);
    if (ledger_index != last_index) {
        const moved = host.roc_allocations.items[ledger_index];
        rocAllocationHeaderFromUserPtr(rocAllocationUserPtr(moved)).ledger_index = ledger_index;
    }
}

fn rocAllocFn(roc_host: *abi.RocHost, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const host = hostFromRocHost(roc_host);
    const allocator = host.gpa.allocator();
    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
    const header_bytes = rocAllocationHeaderBytes(min_alignment);
    const total_size = length + header_bytes;

    const base_ptr = allocator.rawAlloc(total_size, align_enum, @returnAddress()) orelse std.process.exit(1);
    const user_ptr: *anyopaque = @ptrFromInt(@intFromPtr(base_ptr) + header_bytes);
    const ledger_index = host.roc_allocations.items.len;
    rocAllocationHeaderFromUserPtr(user_ptr).* = .{
        .total_size = total_size,
        .ledger_index = ledger_index,
    };

    host.roc_allocations.append(host.gpa.allocator(), .{
        .ptr = base_ptr,
        .total_size = total_size,
        .alignment = align_enum,
    }) catch std.process.exit(1);
    host.alloc_count += 1;
    host.pending_roc_metrics.allocs_this_event += 1;

    return user_ptr;
}

fn rocDeallocFn(roc_host: *abi.RocHost, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    const host = hostFromRocHost(roc_host);
    const allocator = host.gpa.allocator();
    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
    const header_bytes = rocAllocationHeaderBytes(min_alignment);
    const header = rocAllocationHeaderFromUserPtr(ptr);
    const total_size = header.total_size;
    const ledger_index = header.ledger_index;
    const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(ptr) - header_bytes);

    removeRocAllocationAt(host, ledger_index, base_ptr);
    host.dealloc_count += 1;
    host.pending_roc_metrics.deallocs_this_event += 1;

    allocator.rawFree(base_ptr[0..total_size], align_enum, @returnAddress());
}

fn rocReallocFn(roc_host: *abi.RocHost, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const host = hostFromRocHost(roc_host);
    const allocator = host.gpa.allocator();
    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
    const header_bytes = rocAllocationHeaderBytes(min_alignment);
    const old_header = rocAllocationHeaderFromUserPtr(ptr);
    const old_total_size = old_header.total_size;
    const old_ledger_index = old_header.ledger_index;
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(ptr) - header_bytes);
    const old_user_data_size = old_total_size - header_bytes;
    const new_total_size = new_length + header_bytes;

    const new_ptr = allocator.rawAlloc(new_total_size, align_enum, @returnAddress()) orelse std.process.exit(1);
    const new_user_ptr: [*]u8 = @ptrFromInt(@intFromPtr(new_ptr) + header_bytes);
    const old_user_ptr: [*]const u8 = @ptrCast(ptr);
    const copy_size = @min(old_user_data_size, new_length);
    @memcpy(new_user_ptr[0..copy_size], old_user_ptr[0..copy_size]);

    const new_ledger_index = host.roc_allocations.items.len;
    rocAllocationHeaderFromUserPtr(new_user_ptr).* = .{
        .total_size = new_total_size,
        .ledger_index = new_ledger_index,
    };
    host.roc_allocations.append(host.gpa.allocator(), .{
        .ptr = new_ptr,
        .total_size = new_total_size,
        .alignment = align_enum,
    }) catch std.process.exit(1);

    removeRocAllocationAt(host, old_ledger_index, old_base_ptr);
    host.alloc_count += 1;
    host.dealloc_count += 1;
    host.pending_roc_metrics.allocs_this_event += 1;
    host.pending_roc_metrics.deallocs_this_event += 1;

    allocator.rawFree(old_base_ptr[0..old_total_size], align_enum, @returnAddress());

    return new_user_ptr;
}

fn rocDbgFn(roc_host: *abi.RocHost, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = roc_host;
    const message = bytes[0..len];
    std.debug.print("ROC DBG: {s}\n", .{message});
}

fn rocExpectFailedFn(roc_host: *abi.RocHost, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = roc_host;
    const source_bytes = bytes[0..len];
    const trimmed = std.mem.trim(u8, source_bytes, " \t\n\r");
    std.debug.print("Expect failed: {s}\n", .{trimmed});
}

fn rocCrashedFn(roc_host: *abi.RocHost, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = roc_host;
    const message = bytes[0..len];
    writeStderr("\n\x1b[31mRoc crashed:\x1b[0m ");
    writeStderr(message);
    writeStderr("\n");
    std.process.exit(1);
}

fn hostAlloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return rocAllocFn(currentRocHost(), length, alignment);
}

fn hostDealloc(ptr: *anyopaque, alignment: usize) callconv(.c) void {
    rocDeallocFn(currentRocHost(), ptr, alignment);
}

fn hostRealloc(ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return rocReallocFn(currentRocHost(), ptr, new_length, alignment);
}

fn hostDbg(bytes: [*]const u8, len: usize) callconv(.c) void {
    rocDbgFn(currentRocHost(), bytes, len);
}

fn hostExpectFailed(bytes: [*]const u8, len: usize) callconv(.c) void {
    rocExpectFailedFn(currentRocHost(), bytes, len);
}

fn hostCrashed(bytes: [*]const u8, len: usize) callconv(.c) void {
    rocCrashedFn(currentRocHost(), bytes, len);
}

fn currentHost() *HostEnv {
    return current_host orelse @panic("signals HostEnv is not initialized");
}

fn hostValueClone(value: HostValue) callconv(.c) HostValue {
    return currentHost().cloneHostValue(value);
}

fn hostValueGet(value: HostValue) callconv(.c) abi.RocBox {
    return currentHost().getHostValue(value);
}

fn hostValueGetTagged(value: HostValue, tag: HostValueTypeTag) callconv(.c) abi.RocBox {
    const host = currentHost();
    defer host.releaseOwnedHostValueTypeTag(tag);
    return host.getTaggedHostValue(value, tag);
}

fn hostValueStore(box: abi.RocBox) callconv(.c) HostValue {
    return currentHost().storeHostValue(box);
}

fn hostValueStoreTagged(box: abi.RocBox, tag: HostValueTypeTag) callconv(.c) HostValue {
    return currentHost().storeTaggedHostValue(box, tag);
}

fn hostValueTake(value: HostValue) callconv(.c) abi.RocBox {
    return currentHost().takeHostValue(value);
}

fn hostValueTakeTagged(value: HostValue, tag: HostValueTypeTag) callconv(.c) abi.RocBox {
    const host = currentHost();
    defer host.releaseOwnedHostValueTypeTag(tag);
    return host.takeTaggedHostValue(value, tag);
}

fn failHost(message: []const u8) noreturn {
    writeStderr("HOST ERROR: ");
    writeStderr(message);
    writeStderr("\n");
    std.process.exit(1);
}

fn failMissingRenderDescriptor(context: []const u8, node: HostRenderNode) noreturn {
    var message: [192]u8 = undefined;
    const rendered = std.fmt.bufPrint(
        &message,
        "{s}: render node {d} with kind {s} has no matching descriptor",
        .{ context, node.elem_id, @tagName(node.kind) },
    ) catch "render node has no matching descriptor";
    failHost(rendered);
}

fn replaceOwnedString(allocator: std.mem.Allocator, field: *?[]const u8, value: []const u8) bool {
    if (field.*) |existing| {
        if (std.mem.eql(u8, existing, value)) return false;
        allocator.free(existing);
    }
    field.* = allocator.dupe(u8, value) catch std.process.exit(1);
    return true;
}

const CommandCounts = struct {
    total: u64 = 0,
    reset_dom: u64 = 0,
    create_element: u64 = 0,
    append_child: u64 = 0,
    set_text: u64 = 0,
    set_value: u64 = 0,
    set_checked: u64 = 0,
    set_disabled: u64 = 0,
    set_metadata: u64 = 0,
    bind_event: u64 = 0,

    fn addHostReset(self: *CommandCounts) void {
        self.total += 1;
        self.reset_dom += 1;
    }

    fn addCreateElement(self: *CommandCounts) void {
        self.total += 1;
        self.create_element += 1;
    }

    fn addAppendChild(self: *CommandCounts) void {
        self.total += 1;
        self.append_child += 1;
    }

    fn addTextField(self: *CommandCounts, field: RenderTextField) void {
        self.total += 1;
        switch (field) {
            .text => self.set_text += 1,
            .value => self.set_value += 1,
            .role, .label, .test_id => self.set_metadata += 1,
        }
    }

    fn addBoolField(self: *CommandCounts, field: RenderBoolField) void {
        self.total += 1;
        switch (field) {
            .checked => self.set_checked += 1,
            .disabled => self.set_disabled += 1,
        }
    }

    fn addEventBinding(self: *CommandCounts) void {
        self.total += 1;
        self.bind_event += 1;
    }

    fn addAll(self: *CommandCounts, other: CommandCounts) void {
        self.total += other.total;
        self.reset_dom += other.reset_dom;
        self.create_element += other.create_element;
        self.append_child += other.append_child;
        self.set_text += other.set_text;
        self.set_value += other.set_value;
        self.set_checked += other.set_checked;
        self.set_disabled += other.set_disabled;
        self.set_metadata += other.set_metadata;
        self.bind_event += other.bind_event;
    }
};

const BenchmarkStats = struct {
    init_roc_ns: u64 = 0,
    init_apply_ns: u64 = 0,
    dispatch_roc_ns: u64 = 0,
    dispatch_apply_ns: u64 = 0,
    actions: u64 = 0,
    allocs: u64 = 0,
    deallocs: u64 = 0,
    retained_alloc_delta: i64 = 0,
    commands: CommandCounts = .{},
    metrics: RuntimeMetrics = zeroRuntimeMetrics(),
};

fn addRuntimeMetrics(left: RuntimeMetrics, right: RuntimeMetrics) RuntimeMetrics {
    return .{
        .active_graph_records_rebuilt = left.active_graph_records_rebuilt + right.active_graph_records_rebuilt,
        .append_child = left.append_child + right.append_child,
        .allocs_this_event = left.allocs_this_event + right.allocs_this_event,
        .bind_event = left.bind_event + right.bind_event,
        .closure_releases = left.closure_releases + right.closure_releases,
        .closure_retains = left.closure_retains + right.closure_retains,
        .create_element = left.create_element + right.create_element,
        .deallocs_this_event = left.deallocs_this_event + right.deallocs_this_event,
        .derived_calls_into_roc = left.derived_calls_into_roc + right.derived_calls_into_roc,
        .each_key_compares = left.each_key_compares + right.each_key_compares,
        .events_processed = left.events_processed + right.events_processed,
        .nodes_recomputed = left.nodes_recomputed + right.nodes_recomputed,
        .patches_emitted = left.patches_emitted + right.patches_emitted,
        .propagation_prunes = left.propagation_prunes + right.propagation_prunes,
        .recompute_batches = left.recompute_batches + right.recompute_batches,
        .retained_alloc_delta = left.retained_alloc_delta + right.retained_alloc_delta,
        .reset_dom = left.reset_dom + right.reset_dom,
        .rows_created = left.rows_created + right.rows_created,
        .rows_removed = left.rows_removed + right.rows_removed,
        .rows_reused = left.rows_reused + right.rows_reused,
        .scopes_created = left.scopes_created + right.scopes_created,
        .scopes_disposed = left.scopes_disposed + right.scopes_disposed,
        .set_checked = left.set_checked + right.set_checked,
        .set_disabled = left.set_disabled + right.set_disabled,
        .set_metadata = left.set_metadata + right.set_metadata,
        .set_text = left.set_text + right.set_text,
        .set_value = left.set_value + right.set_value,
        .stream_nodes_scanned = left.stream_nodes_scanned + right.stream_nodes_scanned,
    };
}

fn u64MetricAsI64(value: u64) i64 {
    return std.math.cast(i64, value) orelse failHost("runtime metric exceeded signed assertion range");
}

fn runtimeMetricValue(metrics: RuntimeMetrics, name: []const u8) ?i64 {
    if (std.mem.eql(u8, name, "active_graph_records_rebuilt")) return u64MetricAsI64(metrics.active_graph_records_rebuilt);
    if (std.mem.eql(u8, name, "reset_dom")) return u64MetricAsI64(metrics.reset_dom);
    if (std.mem.eql(u8, name, "create_element")) return u64MetricAsI64(metrics.create_element);
    if (std.mem.eql(u8, name, "append_child")) return u64MetricAsI64(metrics.append_child);
    if (std.mem.eql(u8, name, "set_text")) return u64MetricAsI64(metrics.set_text);
    if (std.mem.eql(u8, name, "set_value")) return u64MetricAsI64(metrics.set_value);
    if (std.mem.eql(u8, name, "set_checked")) return u64MetricAsI64(metrics.set_checked);
    if (std.mem.eql(u8, name, "set_disabled")) return u64MetricAsI64(metrics.set_disabled);
    if (std.mem.eql(u8, name, "set_metadata")) return u64MetricAsI64(metrics.set_metadata);
    if (std.mem.eql(u8, name, "bind_event")) return u64MetricAsI64(metrics.bind_event);
    if (std.mem.eql(u8, name, "allocs_this_event")) return u64MetricAsI64(metrics.allocs_this_event);
    if (std.mem.eql(u8, name, "deallocs_this_event")) return u64MetricAsI64(metrics.deallocs_this_event);
    if (std.mem.eql(u8, name, "events_processed")) return u64MetricAsI64(metrics.events_processed);
    if (std.mem.eql(u8, name, "nodes_recomputed")) return u64MetricAsI64(metrics.nodes_recomputed);
    if (std.mem.eql(u8, name, "propagation_prunes")) return u64MetricAsI64(metrics.propagation_prunes);
    if (std.mem.eql(u8, name, "derived_calls_into_roc")) return u64MetricAsI64(metrics.derived_calls_into_roc);
    if (std.mem.eql(u8, name, "each_key_compares")) return u64MetricAsI64(metrics.each_key_compares);
    if (std.mem.eql(u8, name, "recompute_batches")) return u64MetricAsI64(metrics.recompute_batches);
    if (std.mem.eql(u8, name, "patches_emitted")) return u64MetricAsI64(metrics.patches_emitted);
    if (std.mem.eql(u8, name, "scopes_created")) return u64MetricAsI64(metrics.scopes_created);
    if (std.mem.eql(u8, name, "scopes_disposed")) return u64MetricAsI64(metrics.scopes_disposed);
    if (std.mem.eql(u8, name, "rows_reused")) return u64MetricAsI64(metrics.rows_reused);
    if (std.mem.eql(u8, name, "rows_created")) return u64MetricAsI64(metrics.rows_created);
    if (std.mem.eql(u8, name, "rows_removed")) return u64MetricAsI64(metrics.rows_removed);
    if (std.mem.eql(u8, name, "closure_retains")) return u64MetricAsI64(metrics.closure_retains);
    if (std.mem.eql(u8, name, "closure_releases")) return u64MetricAsI64(metrics.closure_releases);
    if (std.mem.eql(u8, name, "stream_nodes_scanned")) return u64MetricAsI64(metrics.stream_nodes_scanned);
    if (std.mem.eql(u8, name, "retained_alloc_delta")) return metrics.retained_alloc_delta;
    return null;
}

fn setElementTextIfChanged(host: *HostEnv, elem: *DomElement, text: []const u8) bool {
    if (replaceOwnedString(host.gpa.allocator(), &elem.text, text)) {
        elem.text_update_count += 1;
        return true;
    }
    return false;
}

fn setElementValueIfChanged(host: *HostEnv, elem: *DomElement, value: []const u8) bool {
    if (replaceOwnedString(host.gpa.allocator(), &elem.value, value)) {
        elem.value_update_count += 1;
        return true;
    }
    return false;
}

fn clearOwnedStringIfPresent(allocator: std.mem.Allocator, field: *?[]const u8) bool {
    if (field.*) |existing| {
        allocator.free(existing);
        field.* = null;
        return true;
    }
    return false;
}

fn clearElementTextIfPresent(host: *HostEnv, elem: *DomElement) bool {
    if (clearOwnedStringIfPresent(host.gpa.allocator(), &elem.text)) {
        elem.text_update_count += 1;
        return true;
    }
    return false;
}

fn clearElementValueIfPresent(host: *HostEnv, elem: *DomElement) bool {
    if (clearOwnedStringIfPresent(host.gpa.allocator(), &elem.value)) {
        elem.value_update_count += 1;
        return true;
    }
    return false;
}

fn setElementCheckedIfChanged(elem: *DomElement, checked: bool) bool {
    if (elem.checked != checked) {
        elem.checked = checked;
        elem.checked_update_count += 1;
        return true;
    }
    return false;
}

fn setElementDisabledIfChanged(elem: *DomElement, disabled: bool) bool {
    if (elem.disabled != disabled) {
        elem.disabled = disabled;
        elem.disabled_update_count += 1;
        return true;
    }
    return false;
}

fn resetSimulatedDom(host: *HostEnv) void {
    const allocator = host.gpa.allocator();
    for (host.dom_elements.items) |*elem| {
        elem.deinit(allocator);
    }
    host.dom_elements.items.len = 0;
    host.next_elem_id = 1;

    const root_tag = allocator.dupe(u8, "root") catch std.process.exit(1);
    host.dom_elements.append(allocator, DomElement.init(0, root_tag)) catch {
        allocator.free(root_tag);
        std.process.exit(1);
    };
}

fn domElementById(host: *HostEnv, id: u64) *DomElement {
    if (id >= host.dom_elements.items.len) failHost("DOM command referenced missing element");
    const elem = &host.dom_elements.items[@intCast(id)];
    if (!elem.active) {
        var message: [96]u8 = undefined;
        const rendered = std.fmt.bufPrint(&message, "DOM command referenced inactive element {d}", .{id}) catch "DOM command referenced inactive element";
        failHost(rendered);
    }
    return elem;
}

fn applyRenderTextField(host: *HostEnv, elem_id: u64, field: RenderTextField, value: []const u8) bool {
    const elem = domElementById(host, elem_id);
    return switch (field) {
        .text => setElementTextIfChanged(host, elem, value),
        .role => replaceOwnedString(host.gpa.allocator(), &elem.role, value),
        .label => replaceOwnedString(host.gpa.allocator(), &elem.label, value),
        .test_id => replaceOwnedString(host.gpa.allocator(), &elem.test_id, value),
        .value => setElementValueIfChanged(host, elem, value),
    };
}

fn applyRenderBoolField(host: *HostEnv, elem_id: u64, field: RenderBoolField, value: bool) bool {
    const elem = domElementById(host, elem_id);
    return switch (field) {
        .checked => setElementCheckedIfChanged(elem, value),
        .disabled => setElementDisabledIfChanged(elem, value),
    };
}

fn clearRenderTextField(host: *HostEnv, elem: *DomElement, field: RenderTextField) bool {
    return switch (field) {
        .text => clearElementTextIfPresent(host, elem),
        .role => clearOwnedStringIfPresent(host.gpa.allocator(), &elem.role),
        .label => clearOwnedStringIfPresent(host.gpa.allocator(), &elem.label),
        .test_id => clearOwnedStringIfPresent(host.gpa.allocator(), &elem.test_id),
        .value => clearElementValueIfPresent(host, elem),
    };
}

fn clearRenderBoolField(elem: *DomElement, field: RenderBoolField) bool {
    return switch (field) {
        .checked => setElementCheckedIfChanged(elem, false),
        .disabled => setElementDisabledIfChanged(elem, false),
    };
}

fn appendDetachedDomNode(host: *HostEnv, elem_id: u64, tag: []const u8) void {
    if (elem_id != host.dom_elements.items.len) failHost("descriptor stream elements must be dense and ordered by elem id");

    const allocator = host.gpa.allocator();
    const tag_copy = allocator.dupe(u8, tag) catch std.process.exit(1);
    host.dom_elements.append(allocator, DomElement.init(elem_id, tag_copy)) catch {
        allocator.free(tag_copy);
        std.process.exit(1);
    };
    host.next_elem_id = elem_id + 1;
}

fn appendDomNode(host: *HostEnv, elem_id: u64, parent_elem_id: u64, tag: []const u8) void {
    appendDetachedDomNode(host, elem_id, tag);
    const allocator = host.gpa.allocator();
    const parent = domElementById(host, parent_elem_id);
    const child = domElementById(host, elem_id);
    child.parent_id = parent.id;
    parent.children.append(allocator, child.id) catch std.process.exit(1);
}

fn findElementDesc(stream: *const HostNodeDescriptorStream, elem_id: u64) ?HostElementDesc {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return null;
    const index = descriptor_index.element orelse return null;
    if (index >= stream.elements.items.len) failHost("element descriptor index exceeded descriptor table");
    const desc = stream.elements.items[index];
    if (desc.elem_id != elem_id) failHost("element descriptor index pointed at the wrong elem id");
    return desc;
}

fn findTextNodeDesc(stream: *const HostNodeDescriptorStream, elem_id: u64) ?HostNodeTextNodeDesc {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return null;
    const index = descriptor_index.text_node orelse return null;
    if (index >= stream.text_nodes.items.len) failHost("text node descriptor index exceeded descriptor table");
    const desc = stream.text_nodes.items[index];
    if (desc.elem_id != elem_id) failHost("text node descriptor index pointed at the wrong elem id");
    return desc;
}

fn findSignalTextNodeDesc(stream: *const HostNodeDescriptorStream, elem_id: u64) ?HostNodeSignalTextNodeDesc {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return null;
    const index = descriptor_index.signal_text_node orelse return null;
    if (index >= stream.signal_text_nodes.items.len) failHost("signal text node descriptor index exceeded descriptor table");
    const desc = stream.signal_text_nodes.items[index];
    if (desc.elem_id != elem_id) failHost("signal text node descriptor index pointed at the wrong elem id");
    return desc;
}

fn findSignalTextNodeDescMutable(stream: *HostNodeDescriptorStream, elem_id: u64) ?*HostNodeSignalTextNodeDesc {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return null;
    const index = descriptor_index.signal_text_node orelse return null;
    if (index >= stream.signal_text_nodes.items.len) failHost("signal text node descriptor index exceeded descriptor table");
    const desc = &stream.signal_text_nodes.items[index];
    if (desc.elem_id != elem_id) failHost("signal text node descriptor index pointed at the wrong elem id");
    return desc;
}

fn streamHasTextField(stream: *const HostNodeDescriptorStream, elem_id: u64, field: RenderTextField) bool {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return false;
    if (field == .text and descriptor_index.text_node != null) return true;
    if (field == .text and descriptor_index.signal_text_node != null) return true;

    if (descriptor_index.static_text_attrs.get(field)) |attr_index| {
        if (attr_index >= stream.static_text_attrs.items.len) failHost("static text attr descriptor index exceeded descriptor table");
        const desc = stream.static_text_attrs.items[attr_index];
        if (desc.elem_id != elem_id or desc.field != field) failHost("static text attr descriptor index pointed at the wrong field");
        return true;
    }
    if (descriptor_index.signal_text_attrs.get(field)) |attr_index| {
        if (attr_index >= stream.signal_text_attrs.items.len) failHost("signal text attr descriptor index exceeded descriptor table");
        const desc = stream.signal_text_attrs.items[attr_index];
        if (desc.elem_id != elem_id or desc.field != field) failHost("signal text attr descriptor index pointed at the wrong field");
        return true;
    }
    return false;
}

fn streamHasBoolField(stream: *const HostNodeDescriptorStream, elem_id: u64, field: RenderBoolField) bool {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return false;
    if (descriptor_index.static_bool_attrs.get(field)) |attr_index| {
        if (attr_index >= stream.static_bool_attrs.items.len) failHost("static bool attr descriptor index exceeded descriptor table");
        const desc = stream.static_bool_attrs.items[attr_index];
        if (desc.elem_id != elem_id or desc.field != field) failHost("static bool attr descriptor index pointed at the wrong field");
        return true;
    }
    if (descriptor_index.signal_bool_attrs.get(field)) |attr_index| {
        if (attr_index >= stream.signal_bool_attrs.items.len) failHost("signal bool attr descriptor index exceeded descriptor table");
        const desc = stream.signal_bool_attrs.items[attr_index];
        if (desc.elem_id != elem_id or desc.field != field) failHost("signal bool attr descriptor index pointed at the wrong field");
        return true;
    }
    return false;
}

fn maxRenderElemId(stream: *const HostNodeDescriptorStream) u64 {
    var max_elem_id: u64 = 0;
    for (stream.render_nodes.items) |node| {
        max_elem_id = @max(max_elem_id, node.elem_id);
    }
    return max_elem_id;
}

fn renderNodeTag(stream: *const HostNodeDescriptorStream, node: HostRenderNode) []const u8 {
    return switch (node.kind) {
        .element => (findElementDesc(stream, node.elem_id) orelse failMissingRenderDescriptor("renderNodeTag", node)).tag,
        .text, .signal_text => "text",
    };
}

fn streamElemTag(stream: *const HostNodeDescriptorStream, elem_id: u64) []const u8 {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse failHost("elem id had no descriptor index");
    if (descriptor_index.element) |index| {
        if (index >= stream.elements.items.len) failHost("element descriptor index exceeded descriptor table");
        const desc = stream.elements.items[index];
        if (desc.elem_id != elem_id) failHost("element descriptor index pointed at the wrong elem id");
        return desc.tag;
    }
    if (descriptor_index.text_node != null or descriptor_index.signal_text_node != null) return "text";
    failHost("elem id had no render descriptor");
}

fn renderNodeParentElemId(stream: *const HostNodeDescriptorStream, node: HostRenderNode) u64 {
    return switch (node.kind) {
        .element => (findElementDesc(stream, node.elem_id) orelse failMissingRenderDescriptor("renderNodeParentElemId", node)).parent_elem_id,
        .text => (findTextNodeDesc(stream, node.elem_id) orelse failMissingRenderDescriptor("renderNodeParentElemId", node)).parent_elem_id,
        .signal_text => (findSignalTextNodeDesc(stream, node.elem_id) orelse failMissingRenderDescriptor("renderNodeParentElemId", node)).parent_elem_id,
    };
}

fn streamElemParentElemId(stream: *const HostNodeDescriptorStream, elem_id: u64) u64 {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse failHost("elem id had no descriptor index");
    if (descriptor_index.element) |index| {
        if (index >= stream.elements.items.len) failHost("element descriptor index exceeded descriptor table");
        const desc = stream.elements.items[index];
        if (desc.elem_id != elem_id) failHost("element descriptor index pointed at the wrong elem id");
        return desc.parent_elem_id;
    }
    if (descriptor_index.text_node) |index| {
        if (index >= stream.text_nodes.items.len) failHost("text node descriptor index exceeded descriptor table");
        const desc = stream.text_nodes.items[index];
        if (desc.elem_id != elem_id) failHost("text node descriptor index pointed at the wrong elem id");
        return desc.parent_elem_id;
    }
    if (descriptor_index.signal_text_node) |index| {
        if (index >= stream.signal_text_nodes.items.len) failHost("signal text node descriptor index exceeded descriptor table");
        const desc = stream.signal_text_nodes.items[index];
        if (desc.elem_id != elem_id) failHost("signal text node descriptor index pointed at the wrong elem id");
        return desc.parent_elem_id;
    }
    failHost("elem id had no render descriptor");
}

fn findDomChildIndex(elem: *const DomElement, child_id: u64) ?usize {
    for (elem.children.items, 0..) |id, index| {
        if (id == child_id) return index;
    }
    return null;
}

fn ensureDomNode(host: *HostEnv, elem_id: u64, tag: []const u8, counts: *CommandCounts) void {
    if (elem_id == 0) failHost("render descriptor cannot claim the host DOM root id");

    if (elem_id < host.dom_elements.items.len) {
        const elem = &host.dom_elements.items[@intCast(elem_id)];
        if (!elem.active) failHost("render descriptor referenced an inactive DOM identity");
        if (!std.mem.eql(u8, elem.tag, tag)) failHost("render descriptor changed the tag for an existing DOM identity");
        return;
    }

    appendDetachedDomNode(host, elem_id, tag);
    counts.addCreateElement();
}

const HostSignalEvalResult = struct {
    value: HostValue,
    changed: bool,
};

fn recordDerivedCall(host: *HostEnv) void {
    var metrics = host.pending_roc_metrics;
    metrics.derived_calls_into_roc += 1;
    host.pending_roc_metrics = metrics;
}

fn cloneCachedSignalValue(host: *HostEnv, cache_slot: *const HostSignalCacheSlot) HostValue {
    return switch (cache_slot.*) {
        .absent => failHost("cached signal expression value was requested before initialization"),
        .present => |cached| host.cloneHostValue(cached.value),
    };
}

fn replaceSignalExprCacheAndClone(host: *HostEnv, cache_slot: *HostSignalCacheSlot, roc_host: *abi.RocHost, metrics: *RuntimeMetrics, value: HostValue, eq: abi.RocErasedCallable, drop: abi.RocErasedCallable) HostValue {
    cache_slot.replace(roc_host, metrics, value, eq, drop);
    return cloneCachedSignalValue(host, cache_slot);
}

fn updateDirtySignalExprCache(host: *HostEnv, roc_host: *abi.RocHost, cache_slot: *HostSignalCacheSlot, value: HostValue) HostSignalEvalResult {
    switch (cache_slot.*) {
        .absent => failHost("dirty signal expression was evaluated before its initial value was cached"),
        .present => |*cached| {
            const values_equal = cached.valueEquals(roc_host, value);
            if (values_equal) {
                cached.dropIncoming(roc_host, value);
                recordSignalPrune(host);
                return .{ .value = host.cloneHostValue(cached.value), .changed = false };
            }

            cached.replaceValue(roc_host, value);
            return .{ .value = host.cloneHostValue(cached.value), .changed = true };
        },
    }
}

fn cloneMemoizedDirtySignalResult(host: *HostEnv, record: *HostSignalRecord, dirty_generation: u64) ?HostSignalEvalResult {
    if (record.last_dirty_generation != dirty_generation) return null;

    return switch (record.payload) {
        .ref => null,
        .const_value => |*payload| .{
            .value = cloneCachedSignalValue(host, &payload.cached_value),
            .changed = record.last_dirty_changed,
        },
        .map => |*payload| .{
            .value = cloneCachedSignalValue(host, &payload.cached_value),
            .changed = record.last_dirty_changed,
        },
        .map2 => |*payload| .{
            .value = cloneCachedSignalValue(host, &payload.cached_value),
            .changed = record.last_dirty_changed,
        },
        .combine => |*payload| .{
            .value = cloneCachedSignalValue(host, &payload.cached_value),
            .changed = record.last_dirty_changed,
        },
        .task_source => |*payload| .{
            .value = cloneCachedSignalValue(host, &payload.cached_value),
            .changed = record.last_dirty_changed,
        },
        .interval_source => |*payload| .{
            .value = cloneCachedSignalValue(host, &payload.cached_value),
            .changed = record.last_dirty_changed,
        },
    };
}

fn rememberDirtySignalResult(record: *HostSignalRecord, dirty_generation: u64, result: HostSignalEvalResult) HostSignalEvalResult {
    record.last_dirty_generation = dirty_generation;
    record.last_dirty_changed = result.changed;
    return result;
}

fn hostSignalRecordDropCallable(host: *HostEnv, record: *const HostSignalRecord) abi.RocErasedCallable {
    return switch (record.payload) {
        .ref => |node_id| host.stateDropCallable(node_id),
        .const_value => |payload| payload.drop,
        .map => |payload| payload.drop,
        .map2 => |payload| payload.drop,
        .combine => |payload| payload.drop,
        .task_source => |payload| payload.drop,
        .interval_source => |payload| payload.drop,
    };
}

fn dropHostSignalRecordValue(host: *HostEnv, roc_host: *abi.RocHost, record: *const HostSignalRecord, value: HostValue) void {
    callErasedHostValueToUnit(roc_host, hostSignalRecordDropCallable(host, record), value);
}

fn evalHostSignalRecord(host: *HostEnv, roc_host: *abi.RocHost, record: *HostSignalRecord) HostValue {
    switch (record.payload) {
        .ref => |node_id| return host.stateValueByNodeId(node_id),
        .const_value => |*payload| {
            const value = callValueInitThunk(roc_host, payload.init);
            return replaceSignalExprCacheAndClone(host, &payload.cached_value, roc_host, &host.pending_roc_metrics, value, payload.eq, payload.drop);
        },
        .map => |*payload| {
            const input = evalHostSignalRecord(host, roc_host, payload.input);
            defer dropHostSignalRecordValue(host, roc_host, payload.input, input);
            recordDerivedCall(host);
            const value = callErasedHostValueToHostValue(roc_host, payload.transform, input);
            return replaceSignalExprCacheAndClone(host, &payload.cached_value, roc_host, &host.pending_roc_metrics, value, payload.eq, payload.drop);
        },
        .map2 => |*payload| {
            const left = evalHostSignalRecord(host, roc_host, payload.left);
            defer dropHostSignalRecordValue(host, roc_host, payload.left, left);
            const right = evalHostSignalRecord(host, roc_host, payload.right);
            defer dropHostSignalRecordValue(host, roc_host, payload.right, right);
            recordDerivedCall(host);
            const value = callErasedHostValueHostValueToHostValue(roc_host, payload.transform, left, right);
            return replaceSignalExprCacheAndClone(host, &payload.cached_value, roc_host, &host.pending_roc_metrics, value, payload.eq, payload.drop);
        },
        .combine => |*payload| {
            var values: std.ArrayListUnmanaged(HostValue) = .empty;
            errdefer {
                for (payload.children, values.items) |child, value| {
                    dropHostSignalRecordValue(host, roc_host, child, value);
                }
                values.deinit(host.gpa.allocator());
            }
            for (payload.children) |child| {
                values.append(host.gpa.allocator(), evalHostSignalRecord(host, roc_host, child)) catch std.process.exit(1);
            }
            const list = HostValueList.fromSlice(values.items, roc_host);
            defer list.decref(roc_host);
            recordDerivedCall(host);
            const value = callErasedHostValueListToHostValue(roc_host, payload.transform, list);
            for (payload.children, values.items) |child, child_value| {
                dropHostSignalRecordValue(host, roc_host, child, child_value);
            }
            values.deinit(host.gpa.allocator());
            return replaceSignalExprCacheAndClone(host, &payload.cached_value, roc_host, &host.pending_roc_metrics, value, payload.eq, payload.drop);
        },
        .task_source => |*payload| {
            switch (payload.cached_value) {
                .present => return cloneCachedSignalValue(host, &payload.cached_value),
                .absent => {
                    const value = callValueInitThunk(roc_host, payload.initial);
                    return replaceSignalExprCacheAndClone(host, &payload.cached_value, roc_host, &host.pending_roc_metrics, value, payload.eq, payload.drop);
                },
            }
        },
        .interval_source => |*payload| {
            switch (payload.cached_value) {
                .present => return cloneCachedSignalValue(host, &payload.cached_value),
                .absent => {
                    const value = callValueInitThunk(roc_host, payload.initial);
                    return replaceSignalExprCacheAndClone(host, &payload.cached_value, roc_host, &host.pending_roc_metrics, value, payload.eq, payload.drop);
                },
            }
        },
    }
}

fn evalDirtyHostSignalRecord(host: *HostEnv, roc_host: *abi.RocHost, record: *HostSignalRecord, dirty_source_node_ids: []const u64, dirty_generation: u64) HostSignalEvalResult {
    if (dirty_generation == 0) failHost("dirty signal evaluation used generation 0");
    if (cloneMemoizedDirtySignalResult(host, record, dirty_generation)) |result| return result;

    switch (record.payload) {
        .ref => |node_id| return .{
            .value = host.stateValueByNodeId(node_id),
            .changed = u64SliceContains(dirty_source_node_ids, node_id),
        },
        .const_value => |*payload| return rememberDirtySignalResult(record, dirty_generation, .{
            .value = cloneCachedSignalValue(host, &payload.cached_value),
            .changed = false,
        }),
        .map => |*payload| {
            const input = evalDirtyHostSignalRecord(host, roc_host, payload.input, dirty_source_node_ids, dirty_generation);
            defer dropHostSignalRecordValue(host, roc_host, payload.input, input.value);
            if (!input.changed) {
                return rememberDirtySignalResult(record, dirty_generation, .{ .value = cloneCachedSignalValue(host, &payload.cached_value), .changed = false });
            }

            recordDerivedCall(host);
            const value = callErasedHostValueToHostValue(roc_host, payload.transform, input.value);
            return rememberDirtySignalResult(record, dirty_generation, updateDirtySignalExprCache(host, roc_host, &payload.cached_value, value));
        },
        .map2 => |*payload| {
            const left = evalDirtyHostSignalRecord(host, roc_host, payload.left, dirty_source_node_ids, dirty_generation);
            defer dropHostSignalRecordValue(host, roc_host, payload.left, left.value);
            const right = evalDirtyHostSignalRecord(host, roc_host, payload.right, dirty_source_node_ids, dirty_generation);
            defer dropHostSignalRecordValue(host, roc_host, payload.right, right.value);
            if (!left.changed and !right.changed) {
                return rememberDirtySignalResult(record, dirty_generation, .{ .value = cloneCachedSignalValue(host, &payload.cached_value), .changed = false });
            }

            recordDerivedCall(host);
            const value = callErasedHostValueHostValueToHostValue(roc_host, payload.transform, left.value, right.value);
            return rememberDirtySignalResult(record, dirty_generation, updateDirtySignalExprCache(host, roc_host, &payload.cached_value, value));
        },
        .combine => |*payload| {
            var values: std.ArrayListUnmanaged(HostValue) = .empty;
            errdefer {
                for (payload.children[0..values.items.len], values.items) |child, value| {
                    dropHostSignalRecordValue(host, roc_host, child, value);
                }
                values.deinit(host.gpa.allocator());
            }

            var any_changed = false;
            for (payload.children) |child| {
                const child_result = evalDirtyHostSignalRecord(host, roc_host, child, dirty_source_node_ids, dirty_generation);
                any_changed = any_changed or child_result.changed;
                values.append(host.gpa.allocator(), child_result.value) catch std.process.exit(1);
            }

            if (!any_changed) {
                for (payload.children, values.items) |child, value| {
                    dropHostSignalRecordValue(host, roc_host, child, value);
                }
                values.deinit(host.gpa.allocator());
                return rememberDirtySignalResult(record, dirty_generation, .{ .value = cloneCachedSignalValue(host, &payload.cached_value), .changed = false });
            }

            const list = HostValueList.fromSlice(values.items, roc_host);
            defer list.decref(roc_host);
            recordDerivedCall(host);
            const value = callErasedHostValueListToHostValue(roc_host, payload.transform, list);
            for (payload.children, values.items) |child, child_value| {
                dropHostSignalRecordValue(host, roc_host, child, child_value);
            }
            values.deinit(host.gpa.allocator());
            return rememberDirtySignalResult(record, dirty_generation, updateDirtySignalExprCache(host, roc_host, &payload.cached_value, value));
        },
        .task_source => |*payload| return rememberDirtySignalResult(record, dirty_generation, .{
            .value = cloneCachedSignalValue(host, &payload.cached_value),
            .changed = record.last_dirty_generation == dirty_generation and record.last_dirty_changed,
        }),
        .interval_source => |*payload| return rememberDirtySignalResult(record, dirty_generation, .{
            .value = cloneCachedSignalValue(host, &payload.cached_value),
            .changed = record.last_dirty_generation == dirty_generation and record.last_dirty_changed,
        }),
    }
}

fn evalHostSignalBinding(host: *HostEnv, roc_host: *abi.RocHost, signal: *HostSignalBinding) HostValue {
    return evalHostSignalRecord(host, roc_host, signal.record);
}

fn evalDirtyHostSignalBinding(host: *HostEnv, roc_host: *abi.RocHost, signal: *HostSignalBinding, dirty_source_node_ids: []const u64, dirty_generation: u64) HostSignalEvalResult {
    return evalDirtyHostSignalRecord(host, roc_host, signal.record, dirty_source_node_ids, dirty_generation);
}

fn propagateDirtyActiveSignals(host: *HostEnv, roc_host: *abi.RocHost, allocator: std.mem.Allocator, dirty_source_node_ids: []const u64, dirty_generation: u64) []u64 {
    const dirty_record_ids = host.dirtyActiveSignalRecordIdsForSources(allocator, dirty_source_node_ids);
    defer allocator.free(dirty_record_ids);
    return propagateDirtyActiveSignalRecordIds(host, roc_host, allocator, dirty_record_ids, dirty_source_node_ids, dirty_generation);
}

fn propagateDirtyActiveSignalRecordIds(host: *HostEnv, roc_host: *abi.RocHost, allocator: std.mem.Allocator, dirty_record_ids: []const u64, dirty_source_node_ids: []const u64, dirty_generation: u64) []u64 {
    var changed_record_ids: std.ArrayListUnmanaged(u64) = .empty;
    errdefer changed_record_ids.deinit(allocator);

    for (dirty_record_ids) |record_id| {
        const record = host.active_signal_graph.items[@intCast(record_id)].record;
        const result = evalDirtyHostSignalRecord(host, roc_host, record, dirty_source_node_ids, dirty_generation);
        if (result.changed) {
            changed_record_ids.append(allocator, record_id) catch std.process.exit(1);
        }
        dropHostSignalRecordValue(host, roc_host, record, result.value);
    }

    return changed_record_ids.toOwnedSlice(allocator) catch std.process.exit(1);
}

fn hostSignalRecordEqCallable(host: *HostEnv, record: *const HostSignalRecord) abi.RocErasedCallable {
    return switch (record.payload) {
        .ref => |node_id| host.stateEqCallable(node_id),
        .const_value => |payload| payload.eq,
        .map => |payload| payload.eq,
        .map2 => |payload| payload.eq,
        .combine => |payload| payload.eq,
        .task_source => |payload| payload.eq,
        .interval_source => |payload| payload.eq,
    };
}

fn hostSignalBindingEqCallable(host: *HostEnv, signal: *const HostSignalBinding) abi.RocErasedCallable {
    return hostSignalRecordEqCallable(host, signal.record);
}

fn hostSignalBindingDropCallable(host: *HostEnv, signal: *const HostSignalBinding) abi.RocErasedCallable {
    return hostSignalRecordDropCallable(host, signal.record);
}

fn recordSignalPrune(host: *HostEnv) void {
    var metrics = host.pending_roc_metrics;
    metrics.propagation_prunes += 1;
    host.pending_roc_metrics = metrics;
}

fn updateDirtySignalCache(host: *HostEnv, roc_host: *abi.RocHost, cache_slot: *HostSignalCacheSlot, value: HostValue) bool {
    switch (cache_slot.*) {
        .absent => failHost("dirty signal expression was evaluated before its initial value was cached"),
        .present => |*cached| {
            const values_equal = cached.valueEquals(roc_host, value);
            if (values_equal) {
                cached.dropIncoming(roc_host, value);
                recordSignalPrune(host);
                return false;
            }

            cache_slot.replaceValue(roc_host, value);
            return true;
        },
    }
}

fn evalSignalTextField(host: *HostEnv, roc_host: *abi.RocHost, elem_id: u64, field: RenderTextField, signal: *HostSignalBinding, read: abi.RocErasedCallable, cache_slot: *HostSignalCacheSlot) bool {
    const value = evalHostSignalBinding(host, roc_host, signal);
    const text = callErasedHostValueToStr(roc_host, read, value);
    defer text.decref(roc_host);
    const changed = applyRenderTextField(host, elem_id, field, text.asSlice());
    cache_slot.replace(roc_host, &host.pending_roc_metrics, value, hostSignalBindingEqCallable(host, signal), hostSignalBindingDropCallable(host, signal));
    return changed;
}

fn evalSignalBoolField(host: *HostEnv, roc_host: *abi.RocHost, elem_id: u64, field: RenderBoolField, signal: *HostSignalBinding, read: abi.RocErasedCallable, cache_slot: *HostSignalCacheSlot) bool {
    const value = evalHostSignalBinding(host, roc_host, signal);
    const bool_value = callErasedHostValueToBool(roc_host, read, value);
    const changed = applyRenderBoolField(host, elem_id, field, bool_value);
    cache_slot.replace(roc_host, &host.pending_roc_metrics, value, hostSignalBindingEqCallable(host, signal), hostSignalBindingDropCallable(host, signal));
    return changed;
}

fn evalDirtySignalTextField(host: *HostEnv, roc_host: *abi.RocHost, elem_id: u64, field: RenderTextField, signal: *HostSignalBinding, read: abi.RocErasedCallable, cache_slot: *HostSignalCacheSlot, dirty_source_node_ids: []const u64, dirty_generation: u64) bool {
    const result = evalDirtyHostSignalBinding(host, roc_host, signal, dirty_source_node_ids, dirty_generation);
    if (!result.changed) {
        callErasedHostValueToUnit(roc_host, hostSignalBindingDropCallable(host, signal), result.value);
        return false;
    }
    if (!updateDirtySignalCache(host, roc_host, cache_slot, result.value)) return false;
    const text = callErasedHostValueToStr(roc_host, read, result.value);
    defer text.decref(roc_host);
    return applyRenderTextField(host, elem_id, field, text.asSlice());
}

fn evalDirtySignalBoolField(host: *HostEnv, roc_host: *abi.RocHost, elem_id: u64, field: RenderBoolField, signal: *HostSignalBinding, read: abi.RocErasedCallable, cache_slot: *HostSignalCacheSlot, dirty_source_node_ids: []const u64, dirty_generation: u64) bool {
    const result = evalDirtyHostSignalBinding(host, roc_host, signal, dirty_source_node_ids, dirty_generation);
    if (!result.changed) {
        callErasedHostValueToUnit(roc_host, hostSignalBindingDropCallable(host, signal), result.value);
        return false;
    }
    if (!updateDirtySignalCache(host, roc_host, cache_slot, result.value)) return false;
    return applyRenderBoolField(host, elem_id, field, callErasedHostValueToBool(roc_host, read, result.value));
}

fn updateEffectSourceCacheSlot(host: *HostEnv, roc_host: *abi.RocHost, cache_slot: *HostSignalCacheSlot, value: HostValue, eq: abi.RocErasedCallable, drop: abi.RocErasedCallable) bool {
    switch (cache_slot.*) {
        .absent => {
            cache_slot.replace(roc_host, &host.pending_roc_metrics, value, eq, drop);
            return true;
        },
        .present => |*cached| {
            if (cached.valueEquals(roc_host, value)) {
                cached.dropIncoming(roc_host, value);
                recordSignalPrune(host);
                return false;
            }
            cached.replaceValue(roc_host, value);
            return true;
        },
    }
}

fn updateEffectSourceCache(host: *HostEnv, roc_host: *abi.RocHost, record: *HostSignalRecord, value: HostValue) bool {
    return switch (record.payload) {
        .task_source => |*payload| updateEffectSourceCacheSlot(host, roc_host, &payload.cached_value, value, payload.eq, payload.drop),
        .interval_source => |*payload| updateEffectSourceCacheSlot(host, roc_host, &payload.cached_value, value, payload.eq, payload.drop),
        .ref, .const_value, .map, .map2, .combine => failHost("effect source update targeted a non-source signal record"),
    };
}

fn applyDirtySignalBatch(host: *HostEnv, roc_host: *abi.RocHost, dirty_source_node_ids: []const u64, changed_record_ids: []const u64, dirty_generation: u64) CommandCounts {
    const dirty_structural_signals = collectDirtyStructuralSignals(host, roc_host, host.gpa.allocator(), dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer host.gpa.allocator().free(dirty_structural_signals);

    var counts = applyDirtyRenderSinks(host, roc_host, dirty_source_node_ids, changed_record_ids, dirty_generation);
    if (dirty_structural_signals.len != 0) {
        counts.addAll(applyDirtyStructuralSignalsLocally(host, roc_host, dirty_source_node_ids, dirty_structural_signals));
    }
    return counts;
}

fn dispatchEffectSourceValue(host: *HostEnv, roc_host: *abi.RocHost, record: *HostSignalRecord, value: HostValue) CommandCounts {
    if (!updateEffectSourceCache(host, roc_host, record, value)) return .{};

    host.recordDispatch();
    var metrics = host.pending_roc_metrics;
    metrics.nodes_recomputed += 1;
    host.pending_roc_metrics = metrics;
    const dirty_generation = host.nextDirtySignalGeneration();
    record.last_dirty_generation = dirty_generation;
    record.last_dirty_changed = true;

    const record_id = host.requireActiveSignalRecordId(record);
    const roots = [_]u64{record_id};
    const dirty_record_ids = host.dirtyActiveSignalRecordIdsForRoots(host.gpa.allocator(), &roots);
    defer host.gpa.allocator().free(dirty_record_ids);

    const changed_record_ids = propagateDirtyActiveSignalRecordIds(host, roc_host, host.gpa.allocator(), dirty_record_ids, &.{}, dirty_generation);
    defer host.gpa.allocator().free(changed_record_ids);
    return applyDirtySignalBatch(host, roc_host, &.{}, changed_record_ids, dirty_generation);
}

fn startTaskCommand(host: *HostEnv, roc_host: *abi.RocHost, owner_scope_id: u64, cmd: abi.__AnonStruct74) CommandCounts {
    const record = host.activeTaskRecordByToken(cmd.task_token) orelse failHost("StartTask referenced a task source that is not active");
    const task_payload = switch (record.payload) {
        .task_source => |payload| payload,
        .ref, .const_value, .map, .map2, .combine, .interval_source => unreachable,
    };
    if (!std.mem.eql(u8, task_payload.name, cmd.task_name.asSlice())) {
        failHost("StartTask task name does not match the referenced task source");
    }

    const request_value = callValueInitThunk(roc_host, cmd.request_init);
    defer callErasedHostValueToUnit(roc_host, cmd.request_drop, request_value);
    const request = callErasedHostValueToStr(roc_host, cmd.request_read, request_value);
    defer request.decref(roc_host);

    host.appendPendingTask(owner_scope_id, cmd.task_token, cmd.task_name.asSlice(), request.asSlice());

    const loading = callValueInitThunk(roc_host, task_payload.initial);
    return dispatchEffectSourceValue(host, roc_host, record, loading);
}

fn resolvePendingTask(host: *HostEnv, roc_host: *abi.RocHost, name: []const u8, payload_text: []const u8, failed: bool) CommandCounts {
    const pending_index = host.pendingTaskIndexByName(name) orelse failHost("fake task result had no matching pending request");
    var pending = host.removePendingTaskAt(pending_index);
    defer host.deinitPendingTask(&pending);

    const record = host.activeTaskRecordByName(name) orelse failHost("fake task result matched no active task source");
    const task_payload = switch (record.payload) {
        .task_source => |payload| payload,
        .ref, .const_value, .map, .map2, .combine, .interval_source => unreachable,
    };
    if (task_payload.token != pending.task_token) {
        failHost("fake task result matched a pending request for a different task source");
    }

    const payload_value = hostValueStrTagged(host, roc_host, payload_text, task_payload.payload_tag);
    defer callErasedHostValueToUnit(roc_host, task_payload.payload_drop, payload_value);
    const next = if (failed)
        callErasedHostValueToHostValue(roc_host, task_payload.failed, payload_value)
    else
        callErasedHostValueToHostValue(roc_host, task_payload.done, payload_value);
    return dispatchEffectSourceValue(host, roc_host, record, next);
}

fn tickIntervalSource(host: *HostEnv, roc_host: *abi.RocHost, period_ms: u64) CommandCounts {
    const record = host.activeIntervalRecordByPeriod(period_ms) orelse failHost("tick_interval matched no active interval source");
    const interval_payload = switch (record.payload) {
        .interval_source => |payload| payload,
        .ref, .const_value, .map, .map2, .combine, .task_source => unreachable,
    };

    const current = evalHostSignalRecord(host, roc_host, record);
    defer dropHostSignalRecordValue(host, roc_host, record, current);
    const next = callErasedHostValueToHostValue(roc_host, interval_payload.tick, current);
    return dispatchEffectSourceValue(host, roc_host, record, next);
}

fn evalOnChangeInitial(host: *HostEnv, roc_host: *abi.RocHost, desc: *HostNodeOnChangeDesc) void {
    const value = evalHostSignalBinding(host, roc_host, &desc.signal);
    desc.cached_value.replace(roc_host, &host.pending_roc_metrics, value, hostSignalBindingEqCallable(host, &desc.signal), hostSignalBindingDropCallable(host, &desc.signal));
}

fn evalDirtyOnChange(host: *HostEnv, roc_host: *abi.RocHost, desc: *HostNodeOnChangeDesc, dirty_source_node_ids: []const u64, dirty_generation: u64) CommandCounts {
    const result = evalDirtyHostSignalBinding(host, roc_host, &desc.signal, dirty_source_node_ids, dirty_generation);
    if (!result.changed) {
        callErasedHostValueToUnit(roc_host, hostSignalBindingDropCallable(host, &desc.signal), result.value);
        return .{};
    }
    if (!updateDirtySignalCache(host, roc_host, &desc.cached_value, result.value)) return .{};

    const cmd = callErasedHostValueToStartTaskCmd(roc_host, desc.to_cmd, result.value);
    defer abi.decref__AnonStruct74(cmd, roc_host);
    return startTaskCommand(host, roc_host, desc.scope_id, cmd);
}

fn sourceNodeIdsIntersect(left: []const u64, right: []const u64) bool {
    for (left) |left_id| {
        for (right) |right_id| {
            if (left_id == right_id) return true;
        }
    }
    return false;
}

fn collectDirtyStructuralSignals(host: *HostEnv, roc_host: *abi.RocHost, allocator: std.mem.Allocator, dirty_source_node_ids: []const u64, changed_record_ids: []const u64, dirty_generation: u64) []HostDirtyStructuralSignal {
    var dirty_structural_signals: std.ArrayListUnmanaged(HostDirtyStructuralSignal) = .empty;
    errdefer dirty_structural_signals.deinit(allocator);

    for (changed_record_ids) |record_id| {
        const route_index: usize = @intCast(record_id);
        if (route_index >= host.active_structural_signal_routes.items.len) continue;

        for (host.active_structural_signal_routes.items[route_index].items) |route| {
            switch (route.kind) {
                .when => {
                    const desc = &host.active_stream.whens.items[route.index];
                    const result = evalDirtyHostSignalBinding(host, roc_host, &desc.condition, dirty_source_node_ids, dirty_generation);
                    if (!result.changed) {
                        callErasedHostValueToUnit(roc_host, hostSignalBindingDropCallable(host, &desc.condition), result.value);
                        continue;
                    }
                    const active_branch: HostScopeBranch = if (callErasedHostValueToBool(roc_host, desc.read, result.value)) .true_branch else .false_branch;
                    if (updateDirtySignalCache(host, roc_host, &desc.cached_value, result.value)) {
                        dirty_structural_signals.append(allocator, .{
                            .kind = .when,
                            .node_id = desc.node_id,
                            .branch = active_branch,
                        }) catch std.process.exit(1);
                    }
                },
                .each => {
                    const desc = &host.active_stream.eaches.items[route.index];
                    const result = evalDirtyHostSignalBinding(host, roc_host, &desc.items, dirty_source_node_ids, dirty_generation);
                    if (!result.changed) {
                        callErasedHostValueToUnit(roc_host, hostSignalBindingDropCallable(host, &desc.items), result.value);
                        continue;
                    }
                    if (updateDirtySignalCache(host, roc_host, &desc.cached_value, result.value)) {
                        dirty_structural_signals.append(allocator, .{
                            .kind = .each,
                            .node_id = desc.node_id,
                        }) catch std.process.exit(1);
                    }
                },
            }
        }
    }

    return dirty_structural_signals.toOwnedSlice(allocator) catch std.process.exit(1);
}

fn applyDirtyRenderSinks(host: *HostEnv, roc_host: *abi.RocHost, dirty_source_node_ids: []const u64, changed_record_ids: []const u64, dirty_generation: u64) CommandCounts {
    var counts: CommandCounts = .{};

    for (changed_record_ids) |record_id| {
        const route_index: usize = @intCast(record_id);
        if (route_index < host.active_text_signal_routes.items.len) {
            for (host.active_text_signal_routes.items[route_index].items) |route| {
                switch (route.kind) {
                    .text_node => {
                        const desc = &host.active_stream.signal_text_nodes.items[route.index];
                        if (evalDirtySignalTextField(host, roc_host, desc.elem_id, .text, &desc.signal, desc.read, &desc.cached_value, dirty_source_node_ids, dirty_generation)) {
                            counts.addTextField(.text);
                        }
                    },
                    .text_attr => {
                        const desc = &host.active_stream.signal_text_attrs.items[route.index];
                        if (evalDirtySignalTextField(host, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value, dirty_source_node_ids, dirty_generation)) {
                            counts.addTextField(desc.field);
                        }
                    },
                }
            }
        }

        if (route_index < host.active_bool_signal_routes.items.len) {
            for (host.active_bool_signal_routes.items[route_index].items) |route| {
                const desc = &host.active_stream.signal_bool_attrs.items[route.index];
                if (evalDirtySignalBoolField(host, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value, dirty_source_node_ids, dirty_generation)) {
                    counts.addBoolField(desc.field);
                }
            }
        }

        if (route_index < host.active_change_signal_routes.items.len) {
            for (host.active_change_signal_routes.items[route_index].items) |route| {
                const desc = &host.active_stream.on_changes.items[route.index];
                counts.addAll(evalDirtyOnChange(host, roc_host, desc, dirty_source_node_ids, dirty_generation));
            }
        }
    }

    host.render_metrics.addCommandCounts(counts);
    return counts;
}

fn bindNodeEvent(host: *HostEnv, desc: HostNodeEventDesc, event_id: u64) void {
    const elem = domElementById(host, desc.elem_id);
    switch (desc.kind) {
        .click => elem.bound_click_event = event_id,
        .input => elem.bound_input_event = event_id,
        .check => elem.bound_check_event = event_id,
    }
}

const HostRequiredEventBindings = struct {
    click: ?u64 = null,
    input: ?u64 = null,
    check: ?u64 = null,
};

fn requiredEventBindingSlot(bindings: *HostRequiredEventBindings, kind: RenderEventKind) *?u64 {
    return switch (kind) {
        .click => &bindings.click,
        .input => &bindings.input,
        .check => &bindings.check,
    };
}

fn domEventBindingSlot(elem: *DomElement, kind: RenderEventKind) *?u64 {
    return switch (kind) {
        .click => &elem.bound_click_event,
        .input => &elem.bound_input_event,
        .check => &elem.bound_check_event,
    };
}

fn applyStructuralEventBindings(host: *HostEnv, stream: *const HostNodeDescriptorStream, seen: []const bool, counts: *CommandCounts) void {
    const allocator = host.gpa.allocator();
    var required = allocator.alloc(HostRequiredEventBindings, seen.len) catch std.process.exit(1);
    defer allocator.free(required);
    @memset(required, .{});

    host.recordStreamNodesScanned(stream.events.items.len);
    for (stream.events.items, 0..) |desc, index| {
        if (desc.elem_id >= seen.len or !seen[@intCast(desc.elem_id)]) {
            failHost("event descriptor referenced an element outside the structural render stream");
        }
        const event_id: u64 = @intCast(index + 1);
        const slot = requiredEventBindingSlot(&required[@intCast(desc.elem_id)], desc.kind);
        if (slot.* != null) failHost("element has duplicate event descriptors for one event kind");
        slot.* = event_id;
    }

    const kinds = [_]RenderEventKind{ .click, .input, .check };
    for (host.dom_elements.items, 0..) |*elem, index| {
        if (index == 0 or index >= seen.len or !seen[index] or !elem.active) continue;

        for (kinds) |kind| {
            const next_event_id = requiredEventBindingSlot(&required[index], kind).*;
            const current_event_id = domEventBindingSlot(elem, kind);
            if (current_event_id.* == next_event_id) continue;

            current_event_id.* = next_event_id;
            counts.addEventBinding();
        }
    }
}

fn appendUniqueU64(allocator: std.mem.Allocator, values: *std.ArrayListUnmanaged(u64), value: u64) void {
    if (u64SliceContains(values.items, value)) return;
    values.append(allocator, value) catch std.process.exit(1);
}

fn streamDirectChildren(allocator: std.mem.Allocator, stream: *const HostNodeDescriptorStream, parent_elem_id: u64) []u64 {
    var children: std.ArrayListUnmanaged(u64) = .empty;
    errdefer children.deinit(allocator);

    for (stream.render_nodes.items) |node| {
        if (renderNodeParentElemId(stream, node) == parent_elem_id) {
            children.append(allocator, node.elem_id) catch std.process.exit(1);
        }
    }

    return children.toOwnedSlice(allocator) catch std.process.exit(1);
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

fn replaceDomChildrenForStructuralParentMoves(host: *HostEnv, parent_elem_id: u64, next_child_ids: []const u64, counts: *CommandCounts) void {
    const allocator = host.gpa.allocator();
    if (parent_elem_id >= host.dom_elements.items.len) failHost("structural move referenced missing parent");
    const parent = &host.dom_elements.items[@intCast(parent_elem_id)];
    if (!parent.active) {
        var message: [128]u8 = undefined;
        const rendered = std.fmt.bufPrint(&message, "structural move referenced inactive parent {d}", .{parent_elem_id}) catch "structural move referenced inactive parent";
        failHost(rendered);
    }
    if (parent.children.items.len != next_child_ids.len) failHost("pure structural move changed child count");

    var old_child_indexes: std.AutoHashMapUnmanaged(u64, usize) = .{};
    defer old_child_indexes.deinit(allocator);
    for (parent.children.items, 0..) |child_id, index| {
        const entry = old_child_indexes.getOrPut(allocator, child_id) catch std.process.exit(1);
        if (entry.found_existing) failHost("parent child list contained duplicate element ids");
        entry.value_ptr.* = index;
    }

    const old_indexes_in_next_order = allocator.alloc(usize, next_child_ids.len) catch std.process.exit(1);
    defer allocator.free(old_indexes_in_next_order);
    for (next_child_ids, 0..) |child_id, index| {
        if (child_id >= host.dom_elements.items.len) failHost("structural move referenced missing child");
        const child = &host.dom_elements.items[@intCast(child_id)];
        if (!child.active) {
            var message: [128]u8 = undefined;
            const rendered = std.fmt.bufPrint(&message, "structural move referenced inactive child {d} under parent {d}", .{ child_id, parent_elem_id }) catch "structural move referenced inactive child";
            failHost(rendered);
        }
        if (child.parent_id == null or child.parent_id.? != parent_elem_id) failHost("pure structural move crossed parent boundary");
        old_indexes_in_next_order[index] = old_child_indexes.get(child_id) orelse failHost("pure structural move inserted a child");
    }

    const stable_scratch = allocator.alloc(usize, next_child_ids.len) catch std.process.exit(1);
    defer allocator.free(stable_scratch);
    const stable_len = stableSubsequenceLength(old_indexes_in_next_order, stable_scratch);
    const displaced_count = next_child_ids.len - stable_len;
    var displaced_index: usize = 0;
    while (displaced_index < displaced_count) : (displaced_index += 1) {
        counts.addAppendChild();
    }

    for (next_child_ids) |child_id| {
        host.dom_elements.items[@intCast(child_id)].parent_id = parent_elem_id;
    }
    parent.children.deinit(allocator);
    parent.children = .empty;
    parent.children.appendSlice(allocator, next_child_ids) catch std.process.exit(1);
}

fn replaceDomChildrenForStructuralParent(host: *HostEnv, parent_elem_id: u64, next_child_ids: []const u64, counts: *CommandCounts) void {
    const allocator = host.gpa.allocator();
    if (parent_elem_id >= host.dom_elements.items.len) failHost("structural child replacement referenced missing parent");
    const parent = &host.dom_elements.items[@intCast(parent_elem_id)];
    if (!parent.active) {
        var message: [128]u8 = undefined;
        const rendered = std.fmt.bufPrint(&message, "structural child replacement referenced inactive parent {d}", .{parent_elem_id}) catch "structural child replacement referenced inactive parent";
        failHost(rendered);
    }

    var child_list_patch_counted = false;
    for (next_child_ids, 0..) |child_id, new_index| {
        if (child_id >= host.dom_elements.items.len) failHost("structural child replacement referenced missing child");
        const child = &host.dom_elements.items[@intCast(child_id)];
        if (!child.active) {
            var message: [128]u8 = undefined;
            const rendered = std.fmt.bufPrint(&message, "structural child replacement referenced inactive child {d} under parent {d}", .{ child_id, parent_elem_id }) catch "structural child replacement referenced inactive child";
            failHost(rendered);
        }
        const old_parent_id = child.parent_id;
        const old_child_index = if (old_parent_id) |id| blk: {
            if (id >= host.dom_elements.items.len) failHost("active DOM node referenced missing parent");
            const old_parent = &host.dom_elements.items[@intCast(id)];
            if (!old_parent.active) failHost("active DOM node referenced inactive parent");
            break :blk findDomChildIndex(old_parent, child_id);
        } else null;

        if (old_parent_id == null or old_parent_id.? != parent_elem_id or old_child_index == null or old_child_index.? != new_index) {
            counts.addAppendChild();
            child_list_patch_counted = true;
        }
        child.parent_id = parent_elem_id;
    }

    if (!child_list_patch_counted and parent.children.items.len != next_child_ids.len) {
        counts.addAppendChild();
    }

    parent.children.deinit(allocator);
    parent.children = .empty;
    parent.children.appendSlice(allocator, next_child_ids) catch std.process.exit(1);
}

fn applyStructuralEventBindingsForSeen(host: *HostEnv, stream: *const HostNodeDescriptorStream, seen: []const bool, counts: *CommandCounts) void {
    const allocator = host.gpa.allocator();
    var required = allocator.alloc(HostRequiredEventBindings, seen.len) catch std.process.exit(1);
    defer allocator.free(required);
    @memset(required, .{});

    host.recordStreamNodesScanned(stream.events.items.len);
    for (stream.events.items, 0..) |desc, index| {
        if (desc.elem_id >= seen.len or !seen[@intCast(desc.elem_id)]) continue;
        const event_id: u64 = @intCast(index + 1);
        const slot = requiredEventBindingSlot(&required[@intCast(desc.elem_id)], desc.kind);
        if (slot.* != null) failHost("element has duplicate event descriptors for one event kind");
        slot.* = event_id;
    }

    const kinds = [_]RenderEventKind{ .click, .input, .check };
    for (host.dom_elements.items, 0..) |*elem, index| {
        if (index == 0 or index >= seen.len or !seen[index] or !elem.active) continue;

        for (kinds) |kind| {
            const next_event_id = requiredEventBindingSlot(&required[index], kind).*;
            const current_event_id = domEventBindingSlot(elem, kind);
            if (current_event_id.* == next_event_id) continue;

            current_event_id.* = next_event_id;
            counts.addEventBinding();
        }
    }
}

fn activeEventIdForElemKind(host: *HostEnv, elem_id: u64, kind: RenderEventKind) ?u64 {
    const descriptor_index = host.active_stream.elemDescriptorIndex(elem_id) orelse return null;
    const event_index = descriptor_index.events.get(kind) orelse return null;
    if (event_index >= host.active_stream.events.items.len) failHost("active event descriptor index exceeded descriptor table");
    const desc = host.active_stream.events.items[event_index];
    if (desc.elem_id != elem_id or desc.kind != kind) failHost("active event descriptor index pointed at the wrong event");
    return @intCast(event_index + 1);
}

fn applyStructuralEventBindingsForElem(host: *HostEnv, elem_id: u64, counts: *CommandCounts) void {
    const elem = domElementById(host, elem_id);
    const kinds = [_]RenderEventKind{ .click, .input, .check };
    for (kinds) |kind| {
        const next_event_id = activeEventIdForElemKind(host, elem_id, kind);
        const current_event_id = domEventBindingSlot(elem, kind);
        if (current_event_id.* == next_event_id) continue;

        current_event_id.* = next_event_id;
        counts.addEventBinding();
    }
}

fn applyActiveStreamTextAttrForElem(host: *HostEnv, roc_host: *abi.RocHost, elem_id: u64, field: RenderTextField, descriptor_index: HostElemDescriptorIndex, counts: *CommandCounts) void {
    if (descriptor_index.static_text_attrs.get(field)) |attr_index| {
        if (attr_index >= host.active_stream.static_text_attrs.items.len) failHost("active static text attr index exceeded descriptor table");
        const desc = host.active_stream.static_text_attrs.items[attr_index];
        if (desc.elem_id != elem_id or desc.field != field) failHost("active static text attr index pointed at the wrong field");
        if (applyRenderTextField(host, desc.elem_id, desc.field, desc.value)) {
            counts.addTextField(desc.field);
        }
    }

    if (descriptor_index.signal_text_attrs.get(field)) |attr_index| {
        if (attr_index >= host.active_stream.signal_text_attrs.items.len) failHost("active signal text attr index exceeded descriptor table");
        const desc = &host.active_stream.signal_text_attrs.items[attr_index];
        if (desc.elem_id != elem_id or desc.field != field) failHost("active signal text attr index pointed at the wrong field");
        if (evalSignalTextField(host, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value)) {
            counts.addTextField(desc.field);
        }
    }
}

fn applyActiveStreamBoolAttrForElem(host: *HostEnv, roc_host: *abi.RocHost, elem_id: u64, field: RenderBoolField, descriptor_index: HostElemDescriptorIndex, counts: *CommandCounts) void {
    if (descriptor_index.static_bool_attrs.get(field)) |attr_index| {
        if (attr_index >= host.active_stream.static_bool_attrs.items.len) failHost("active static bool attr index exceeded descriptor table");
        const desc = host.active_stream.static_bool_attrs.items[attr_index];
        if (desc.elem_id != elem_id or desc.field != field) failHost("active static bool attr index pointed at the wrong field");
        if (applyRenderBoolField(host, desc.elem_id, desc.field, desc.value)) {
            counts.addBoolField(desc.field);
        }
    }

    if (descriptor_index.signal_bool_attrs.get(field)) |attr_index| {
        if (attr_index >= host.active_stream.signal_bool_attrs.items.len) failHost("active signal bool attr index exceeded descriptor table");
        const desc = &host.active_stream.signal_bool_attrs.items[attr_index];
        if (desc.elem_id != elem_id or desc.field != field) failHost("active signal bool attr index pointed at the wrong field");
        if (evalSignalBoolField(host, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value)) {
            counts.addBoolField(desc.field);
        }
    }
}

fn applyActiveStreamFieldsForElem(host: *HostEnv, roc_host: *abi.RocHost, elem_id: u64, counts: *CommandCounts) void {
    const descriptor_index = host.active_stream.elemDescriptorIndex(elem_id) orelse failHost("active render node had no descriptor index");
    const elem = domElementById(host, elem_id);
    const text_fields = [_]RenderTextField{ .text, .role, .label, .test_id, .value };
    const bool_fields = [_]RenderBoolField{ .checked, .disabled };

    for (text_fields) |field| {
        if (!streamHasTextField(&host.active_stream, elem.id, field) and clearRenderTextField(host, elem, field)) {
            counts.addTextField(field);
        }
    }
    for (bool_fields) |field| {
        if (!streamHasBoolField(&host.active_stream, elem.id, field) and clearRenderBoolField(elem, field)) {
            counts.addBoolField(field);
        }
    }

    if (descriptor_index.text_node) |text_index| {
        if (text_index >= host.active_stream.text_nodes.items.len) failHost("active text node index exceeded descriptor table");
        const desc = host.active_stream.text_nodes.items[text_index];
        if (desc.elem_id != elem_id) failHost("active text node index pointed at the wrong elem id");
        if (applyRenderTextField(host, desc.elem_id, .text, desc.value)) {
            counts.addTextField(.text);
        }
    }

    if (descriptor_index.signal_text_node) |signal_text_index| {
        if (signal_text_index >= host.active_stream.signal_text_nodes.items.len) failHost("active signal text node index exceeded descriptor table");
        const desc = &host.active_stream.signal_text_nodes.items[signal_text_index];
        if (desc.elem_id != elem_id) failHost("active signal text node index pointed at the wrong elem id");
        if (evalSignalTextField(host, roc_host, desc.elem_id, .text, &desc.signal, desc.read, &desc.cached_value)) {
            counts.addTextField(.text);
        }
    }

    for (text_fields) |field| {
        applyActiveStreamTextAttrForElem(host, roc_host, elem_id, field, descriptor_index, counts);
    }
    for (bool_fields) |field| {
        applyActiveStreamBoolAttrForElem(host, roc_host, elem_id, field, descriptor_index, counts);
    }
}

fn applyStructuralNodeDescriptorTarget(host: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, targets: HostStructuralPatchTargets) CommandCounts {
    if (host.dom_elements.items.len == 0) failHost("structural DOM patch requested before initial DOM root creation");

    const allocator = host.gpa.allocator();
    const max_elem_id = @max(maxRenderElemId(&host.active_stream), maxRenderElemId(stream));
    const required_child_table_len: usize = @intCast(max_elem_id + 1);
    const child_table_len = @max(host.dom_elements.items.len, required_child_table_len);

    var seen = allocator.alloc(bool, child_table_len) catch std.process.exit(1);
    defer allocator.free(seen);
    @memset(seen, false);

    var touched_parents: std.ArrayListUnmanaged(u64) = .empty;
    defer touched_parents.deinit(allocator);

    var counts: CommandCounts = .{};

    host.recordStreamNodesScanned(stream.render_nodes.items.len);
    for (stream.render_nodes.items) |node| {
        if (!host.renderNodeInReplacementTarget(stream, node, targets.replacement)) continue;
        if (node.elem_id >= child_table_len) failHost("render node exceeded structural DOM patch table");

        const parent_elem_id = renderNodeParentElemId(stream, node);
        if (parent_elem_id >= child_table_len) failHost("render node referenced parent outside structural DOM patch table");

        ensureDomNode(host, node.elem_id, renderNodeTag(stream, node), &counts);
        seen[@intCast(node.elem_id)] = true;
        appendUniqueU64(allocator, &touched_parents, parent_elem_id);
    }

    host.recordStreamNodesScanned(host.active_stream.render_nodes.items.len);
    for (host.active_stream.render_nodes.items) |node| {
        if (!host.renderNodeInReplacementTarget(&host.active_stream, node, targets.removed)) continue;
        if (node.elem_id < seen.len and seen[@intCast(node.elem_id)]) continue;
        if (node.elem_id >= host.dom_elements.items.len) continue;

        const elem = &host.dom_elements.items[@intCast(node.elem_id)];
        if (!elem.active) continue;
        elem.active = false;
        elem.parent_id = null;
        elem.bound_click_event = null;
        elem.bound_input_event = null;
        elem.bound_check_event = null;
    }

    for (touched_parents.items) |parent_elem_id| {
        host.recordStreamNodesScanned(stream.render_nodes.items.len);
        const children = streamDirectChildren(allocator, stream, parent_elem_id);
        defer allocator.free(children);
        replaceDomChildrenForStructuralParent(host, parent_elem_id, children, &counts);
    }

    const text_fields = [_]RenderTextField{ .text, .role, .label, .test_id, .value };
    const bool_fields = [_]RenderBoolField{ .checked, .disabled };
    for (host.dom_elements.items, 0..) |*elem, index| {
        if (index == 0 or index >= seen.len or !seen[index] or !elem.active) continue;

        for (text_fields) |field| {
            if (!streamHasTextField(stream, elem.id, field) and clearRenderTextField(host, elem, field)) {
                counts.addTextField(field);
            }
        }
        for (bool_fields) |field| {
            if (!streamHasBoolField(stream, elem.id, field) and clearRenderBoolField(elem, field)) {
                counts.addBoolField(field);
            }
        }
    }

    host.recordStreamNodesScanned(stream.text_nodes.items.len);
    for (stream.text_nodes.items) |desc| {
        if (desc.elem_id < seen.len and seen[@intCast(desc.elem_id)] and applyRenderTextField(host, desc.elem_id, .text, desc.value)) {
            counts.addTextField(.text);
        }
    }
    host.recordStreamNodesScanned(stream.signal_text_nodes.items.len);
    for (stream.signal_text_nodes.items) |*desc| {
        if (desc.elem_id < seen.len and seen[@intCast(desc.elem_id)] and evalSignalTextField(host, roc_host, desc.elem_id, .text, &desc.signal, desc.read, &desc.cached_value)) {
            counts.addTextField(.text);
        }
    }
    host.recordStreamNodesScanned(stream.static_text_attrs.items.len);
    for (stream.static_text_attrs.items) |desc| {
        if (desc.elem_id < seen.len and seen[@intCast(desc.elem_id)] and applyRenderTextField(host, desc.elem_id, desc.field, desc.value)) {
            counts.addTextField(desc.field);
        }
    }
    host.recordStreamNodesScanned(stream.signal_text_attrs.items.len);
    for (stream.signal_text_attrs.items) |*desc| {
        if (desc.elem_id < seen.len and seen[@intCast(desc.elem_id)] and evalSignalTextField(host, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value)) {
            counts.addTextField(desc.field);
        }
    }
    host.recordStreamNodesScanned(stream.static_bool_attrs.items.len);
    for (stream.static_bool_attrs.items) |desc| {
        if (desc.elem_id < seen.len and seen[@intCast(desc.elem_id)] and applyRenderBoolField(host, desc.elem_id, desc.field, desc.value)) {
            counts.addBoolField(desc.field);
        }
    }
    host.recordStreamNodesScanned(stream.signal_bool_attrs.items.len);
    for (stream.signal_bool_attrs.items) |*desc| {
        if (desc.elem_id < seen.len and seen[@intCast(desc.elem_id)] and evalSignalBoolField(host, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value)) {
            counts.addBoolField(desc.field);
        }
    }
    host.recordStreamNodesScanned(stream.on_changes.items.len);
    for (stream.on_changes.items) |*desc| {
        if (host.scopeIsInReplacementTarget(desc.scope_id, targets.replacement)) {
            evalOnChangeInitial(host, roc_host, desc);
        }
    }

    applyStructuralEventBindingsForSeen(host, stream, seen, &counts);

    host.rebuildActiveSignalGraphFromStream(stream);
    host.render_metrics.addCommandCounts(counts);
    return counts;
}

fn applyNodeDescriptorStream(host: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream) CommandCounts {
    var counts: CommandCounts = .{};
    counts.addHostReset();
    resetSimulatedDom(host);

    for (stream.render_nodes.items) |node| {
        switch (node.kind) {
            .element => {
                const desc = findElementDesc(stream, node.elem_id) orelse failHost("render node referenced missing element descriptor");
                appendDomNode(host, desc.elem_id, desc.parent_elem_id, desc.tag);
                counts.addCreateElement();
                counts.addAppendChild();
            },
            .text => {
                const desc = findTextNodeDesc(stream, node.elem_id) orelse failHost("render node referenced missing text descriptor");
                appendDomNode(host, desc.elem_id, desc.parent_elem_id, "text");
                counts.addCreateElement();
                counts.addAppendChild();
                if (applyRenderTextField(host, desc.elem_id, .text, desc.value)) {
                    counts.addTextField(.text);
                }
            },
            .signal_text => {
                const desc = findSignalTextNodeDescMutable(stream, node.elem_id) orelse failHost("render node referenced missing signal text descriptor");
                appendDomNode(host, desc.elem_id, desc.parent_elem_id, "text");
                counts.addCreateElement();
                counts.addAppendChild();
                if (evalSignalTextField(host, roc_host, desc.elem_id, .text, &desc.signal, desc.read, &desc.cached_value)) {
                    counts.addTextField(.text);
                }
            },
        }
    }

    for (stream.static_text_attrs.items) |desc| {
        if (applyRenderTextField(host, desc.elem_id, desc.field, desc.value)) {
            counts.addTextField(desc.field);
        }
    }
    for (stream.signal_text_attrs.items) |*desc| {
        if (evalSignalTextField(host, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value)) {
            counts.addTextField(desc.field);
        }
    }
    for (stream.static_bool_attrs.items) |desc| {
        if (applyRenderBoolField(host, desc.elem_id, desc.field, desc.value)) {
            counts.addBoolField(desc.field);
        }
    }
    for (stream.signal_bool_attrs.items) |*desc| {
        if (evalSignalBoolField(host, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value)) {
            counts.addBoolField(desc.field);
        }
    }
    for (stream.on_changes.items) |*desc| {
        evalOnChangeInitial(host, roc_host, desc);
    }
    for (stream.events.items, 0..) |desc, index| {
        bindNodeEvent(host, desc, @intCast(index + 1));
        counts.addEventBinding();
    }

    host.rebuildActiveSignalGraphFromStream(stream);
    host.render_metrics.addCommandCounts(counts);
    return counts;
}

fn applySplicedStructuralNodeDescriptorTarget(host: *HostEnv, roc_host: *abi.RocHost, splice: HostStructuralSplice, targets: HostStructuralPatchTargets) CommandCounts {
    _ = targets;
    if (host.dom_elements.items.len == 0) failHost("structural DOM patch requested before initial DOM root creation");

    const allocator = host.gpa.allocator();
    var max_elem_id: u64 = @intCast(host.dom_elements.items.len - 1);
    for (splice.removed_elem_ids) |elem_id| {
        max_elem_id = @max(max_elem_id, elem_id);
    }
    for (splice.replacement_elem_ids) |elem_id| {
        max_elem_id = @max(max_elem_id, elem_id);
    }
    const required_child_table_len: usize = @intCast(max_elem_id + 1);
    const child_table_len = @max(host.dom_elements.items.len, required_child_table_len);

    var seen = allocator.alloc(bool, child_table_len) catch std.process.exit(1);
    defer allocator.free(seen);
    @memset(seen, false);

    var touched_parents: std.ArrayListUnmanaged(u64) = .empty;
    defer touched_parents.deinit(allocator);
    for (splice.touched_parent_ids) |parent_id| {
        appendUniqueU64(allocator, &touched_parents, parent_id);
    }

    var counts: CommandCounts = .{};

    host.recordStreamNodesScanned(splice.replacement_elem_ids.len);
    for (splice.replacement_elem_ids) |elem_id| {
        if (elem_id >= child_table_len) failHost("render node exceeded structural DOM patch table");

        const parent_elem_id = streamElemParentElemId(&host.active_stream, elem_id);
        if (parent_elem_id >= child_table_len) failHost("render node referenced parent outside structural DOM patch table");

        ensureDomNode(host, elem_id, streamElemTag(&host.active_stream, elem_id), &counts);
        seen[@intCast(elem_id)] = true;
        appendUniqueU64(allocator, &touched_parents, parent_elem_id);
    }

    for (splice.removed_elem_ids) |elem_id| {
        if (elem_id < seen.len and seen[@intCast(elem_id)]) continue;
        if (elem_id >= host.dom_elements.items.len) continue;

        const elem = &host.dom_elements.items[@intCast(elem_id)];
        if (!elem.active) continue;
        elem.active = false;
        elem.parent_id = null;
        elem.bound_click_event = null;
        elem.bound_input_event = null;
        elem.bound_check_event = null;
    }

    for (touched_parents.items) |parent_elem_id| {
        host.recordStreamNodesScanned(host.active_stream.render_nodes.items.len);
        const children = streamDirectChildren(allocator, &host.active_stream, parent_elem_id);
        defer allocator.free(children);
        for (children) |child_id| {
            if (child_id >= host.dom_elements.items.len or host.dom_elements.items[@intCast(child_id)].active) continue;
            const was_seen = child_id < seen.len and seen[@intCast(child_id)];
            var message: [160]u8 = undefined;
            const rendered = std.fmt.bufPrint(
                &message,
                "spliced structural parent {d} has inactive child {d}; replacement target seen={}",
                .{ parent_elem_id, child_id, was_seen },
            ) catch "spliced structural parent has inactive child";
            failHost(rendered);
        }
        replaceDomChildrenForStructuralParent(host, parent_elem_id, children, &counts);
    }

    for (splice.replacement_elem_ids) |elem_id| {
        applyActiveStreamFieldsForElem(host, roc_host, elem_id, &counts);
        applyStructuralEventBindingsForElem(host, elem_id, &counts);
    }

    host.recordStreamNodesScanned(splice.replacement_on_change_indices.len);
    for (splice.replacement_on_change_indices) |on_change_index| {
        if (on_change_index >= host.active_stream.on_changes.items.len) failHost("structural splice on_change index exceeded active descriptor stream");
        const desc = &host.active_stream.on_changes.items[on_change_index];
        evalOnChangeInitial(host, roc_host, desc);
    }

    host.render_metrics.addCommandCounts(counts);
    return counts;
}

fn applyStructuralNodeDescriptorStream(host: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream) CommandCounts {
    if (host.dom_elements.items.len == 0) failHost("structural DOM patch requested before initial DOM root creation");

    const allocator = host.gpa.allocator();
    const max_elem_id = maxRenderElemId(stream);
    const required_child_table_len: usize = @intCast(max_elem_id + 1);
    const child_table_len = @max(host.dom_elements.items.len, required_child_table_len);

    var seen = allocator.alloc(bool, child_table_len) catch std.process.exit(1);
    defer allocator.free(seen);
    @memset(seen, false);

    var next_children = allocator.alloc(std.ArrayListUnmanaged(u64), child_table_len) catch std.process.exit(1);
    defer {
        for (next_children) |*children| {
            children.deinit(allocator);
        }
        allocator.free(next_children);
    }
    for (next_children) |*children| {
        children.* = .empty;
    }

    var counts: CommandCounts = .{};

    for (stream.render_nodes.items) |node| {
        if (node.elem_id >= child_table_len) failHost("render node exceeded structural DOM patch table");

        const parent_elem_id = renderNodeParentElemId(stream, node);
        if (parent_elem_id >= child_table_len) failHost("render node referenced parent outside structural DOM patch table");

        const was_active = node.elem_id < host.dom_elements.items.len and host.dom_elements.items[@intCast(node.elem_id)].active;
        var old_parent_id: ?u64 = null;
        var old_child_index: ?usize = null;
        if (was_active) {
            const elem = host.dom_elements.items[@intCast(node.elem_id)];
            old_parent_id = elem.parent_id;
            if (old_parent_id) |parent_id| {
                if (parent_id >= host.dom_elements.items.len) failHost("active DOM node referenced missing parent");
                const old_parent = &host.dom_elements.items[@intCast(parent_id)];
                if (!old_parent.active) failHost("active DOM node referenced inactive parent");
                old_child_index = findDomChildIndex(old_parent, node.elem_id);
            }
        }

        ensureDomNode(host, node.elem_id, renderNodeTag(stream, node), &counts);
        seen[@intCast(node.elem_id)] = true;

        const new_child_index = next_children[@intCast(parent_elem_id)].items.len;
        next_children[@intCast(parent_elem_id)].append(allocator, node.elem_id) catch std.process.exit(1);

        if (!was_active or old_parent_id == null or old_parent_id.? != parent_elem_id or old_child_index == null or old_child_index.? != new_child_index) {
            counts.addAppendChild();
        }

        const elem = domElementById(host, node.elem_id);
        elem.parent_id = parent_elem_id;
    }

    for (host.dom_elements.items, 0..) |*elem, index| {
        if (index == 0) continue;
        const still_rendered = index < seen.len and seen[index];
        if (!still_rendered and elem.active) {
            elem.active = false;
            elem.parent_id = null;
            elem.bound_click_event = null;
            elem.bound_input_event = null;
            elem.bound_check_event = null;
        }
    }

    for (host.dom_elements.items, 0..) |*elem, index| {
        const accepts_children = index == 0 or (index < seen.len and seen[index] and elem.active);
        if (!accepts_children) continue;

        elem.children.deinit(allocator);
        elem.children = next_children[index];
        next_children[index] = .empty;
    }

    const text_fields = [_]RenderTextField{ .text, .role, .label, .test_id, .value };
    const bool_fields = [_]RenderBoolField{ .checked, .disabled };
    for (host.dom_elements.items, 0..) |*elem, index| {
        if (index == 0 or index >= seen.len or !seen[index] or !elem.active) continue;

        for (text_fields) |field| {
            if (!streamHasTextField(stream, elem.id, field) and clearRenderTextField(host, elem, field)) {
                counts.addTextField(field);
            }
        }
        for (bool_fields) |field| {
            if (!streamHasBoolField(stream, elem.id, field) and clearRenderBoolField(elem, field)) {
                counts.addBoolField(field);
            }
        }
    }

    for (stream.text_nodes.items) |desc| {
        if (applyRenderTextField(host, desc.elem_id, .text, desc.value)) {
            counts.addTextField(.text);
        }
    }
    for (stream.signal_text_nodes.items) |*desc| {
        if (evalSignalTextField(host, roc_host, desc.elem_id, .text, &desc.signal, desc.read, &desc.cached_value)) {
            counts.addTextField(.text);
        }
    }
    for (stream.static_text_attrs.items) |desc| {
        if (applyRenderTextField(host, desc.elem_id, desc.field, desc.value)) {
            counts.addTextField(desc.field);
        }
    }
    for (stream.signal_text_attrs.items) |*desc| {
        if (evalSignalTextField(host, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value)) {
            counts.addTextField(desc.field);
        }
    }
    for (stream.static_bool_attrs.items) |desc| {
        if (applyRenderBoolField(host, desc.elem_id, desc.field, desc.value)) {
            counts.addBoolField(desc.field);
        }
    }
    for (stream.signal_bool_attrs.items) |*desc| {
        if (evalSignalBoolField(host, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value)) {
            counts.addBoolField(desc.field);
        }
    }
    for (stream.on_changes.items) |*desc| {
        evalOnChangeInitial(host, roc_host, desc);
    }

    applyStructuralEventBindings(host, stream, seen, &counts);

    host.rebuildActiveSignalGraphFromStream(stream);
    host.render_metrics.addCommandCounts(counts);
    return counts;
}

fn dropMovedElemPayload(_: ?*anyopaque, _: *abi.RocHost) callconv(.c) void {}

fn hostValueUnit(host: *HostEnv, roc_host: *abi.RocHost) HostValue {
    const payload = abi.allocateBox(0, @alignOf(u8), false, roc_host);
    const value = host.storeHostValue(@ptrCast(payload));
    if (builtin.is_test) host.setTestHostValueKind(value, .unit);
    return value;
}

fn hostValueStr(host: *HostEnv, roc_host: *abi.RocHost, value: []const u8) HostValue {
    const payload: *RocStr = @ptrCast(@alignCast(abi.allocateBox(@sizeOf(RocStr), @alignOf(RocStr), true, roc_host)));
    payload.* = RocStr.fromSlice(value, roc_host);
    const host_value = host.storeHostValue(@ptrCast(payload));
    if (builtin.is_test) host.setTestHostValueKind(host_value, .str);
    return host_value;
}

fn hostValueStrTagged(host: *HostEnv, roc_host: *abi.RocHost, value: []const u8, tag: HostValueTypeTag) HostValue {
    const payload: *RocStr = @ptrCast(@alignCast(abi.allocateBox(@sizeOf(RocStr), @alignOf(RocStr), true, roc_host)));
    payload.* = RocStr.fromSlice(value, roc_host);
    const host_value = host.storeHostValueWithRetainedTag(@ptrCast(payload), tag);
    if (builtin.is_test) host.setTestHostValueKind(host_value, .str);
    return host_value;
}

fn hostValueBool(host: *HostEnv, roc_host: *abi.RocHost, value: bool) HostValue {
    const payload: *bool = @ptrCast(@alignCast(abi.allocateBox(@sizeOf(bool), @alignOf(bool), false, roc_host)));
    payload.* = value;
    const host_value = host.storeHostValue(@ptrCast(payload));
    if (builtin.is_test) host.setTestHostValueKind(host_value, .bool);
    return host_value;
}

fn hostValueI64(host: *HostEnv, roc_host: *abi.RocHost, value: i64) HostValue {
    const payload: *i64 = @ptrCast(@alignCast(abi.allocateBox(@sizeOf(i64), @alignOf(i64), false, roc_host)));
    payload.* = value;
    const host_value = host.storeHostValue(@ptrCast(payload));
    if (builtin.is_test) host.setTestHostValueKind(host_value, .i64);
    return host_value;
}

const ErasedUnitArgs = extern struct {};

const ErasedHostValueUnaryArgs = extern struct {
    arg0: HostValue,
};

const ErasedHostValueBinaryArgs = extern struct {
    arg0: HostValue,
    arg1: HostValue,
};

const ErasedHostValueListUnaryArgs = extern struct {
    arg0: HostValueList,
};

fn erasedCallablePayload(callable: abi.RocErasedCallable) *abi.RocErasedCallablePayload {
    if (callable == null) failHost("host attempted to call a null Roc erased callable");
    return abi.rocErasedCallablePayloadPtr(callable);
}

fn callValueInitThunk(roc_host: *abi.RocHost, callable: abi.RocErasedCallable) HostValue {
    const payload = erasedCallablePayload(callable);
    var call_args = ErasedUnitArgs{};
    var result: HostValue = undefined;
    payload.callable_fn_ptr(
        roc_host,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return result;
}

fn callErasedHostValueToHostValue(roc_host: *abi.RocHost, callable: abi.RocErasedCallable, arg0: HostValue) HostValue {
    const payload = erasedCallablePayload(callable);
    var call_args = ErasedHostValueUnaryArgs{ .arg0 = arg0 };
    var result: HostValue = undefined;
    payload.callable_fn_ptr(
        roc_host,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return result;
}

fn callErasedHostValueToStartTaskCmd(roc_host: *abi.RocHost, callable: abi.RocErasedCallable, arg0: HostValue) abi.__AnonStruct74 {
    const payload = erasedCallablePayload(callable);
    var call_args = ErasedHostValueUnaryArgs{ .arg0 = arg0 };
    var result: abi.__AnonStruct74 = undefined;
    payload.callable_fn_ptr(
        roc_host,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return result;
}

fn callErasedHostValueHostValueToHostValue(roc_host: *abi.RocHost, callable: abi.RocErasedCallable, arg0: HostValue, arg1: HostValue) HostValue {
    const payload = erasedCallablePayload(callable);
    var call_args = ErasedHostValueBinaryArgs{ .arg0 = arg0, .arg1 = arg1 };
    var result: HostValue = undefined;
    payload.callable_fn_ptr(
        roc_host,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return result;
}

fn callErasedHostValueHostValueToElem(roc_host: *abi.RocHost, callable: abi.RocErasedCallable, arg0: HostValue, arg1: HostValue) abi.Elem {
    const payload = erasedCallablePayload(callable);
    var call_args = ErasedHostValueBinaryArgs{ .arg0 = arg0, .arg1 = arg1 };
    var result: abi.Elem = undefined;
    payload.callable_fn_ptr(
        roc_host,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return result;
}

fn callErasedHostValueHostValueToBool(roc_host: *abi.RocHost, callable: abi.RocErasedCallable, arg0: HostValue, arg1: HostValue) bool {
    const payload = erasedCallablePayload(callable);
    var call_args = ErasedHostValueBinaryArgs{ .arg0 = arg0, .arg1 = arg1 };
    var result: usize = 0;
    payload.callable_fn_ptr(
        roc_host,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return (result & 0xff) != 0;
}

fn callErasedHostValueToUnit(roc_host: *abi.RocHost, callable: abi.RocErasedCallable, arg0: HostValue) void {
    const payload = erasedCallablePayload(callable);
    var call_args = ErasedHostValueUnaryArgs{ .arg0 = arg0 };
    var result: usize = 0;
    payload.callable_fn_ptr(
        roc_host,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
}

fn callErasedHostValueToStr(roc_host: *abi.RocHost, callable: abi.RocErasedCallable, arg0: HostValue) RocStr {
    const payload = erasedCallablePayload(callable);
    var call_args = ErasedHostValueUnaryArgs{ .arg0 = arg0 };
    var result: RocStr = undefined;
    payload.callable_fn_ptr(
        roc_host,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return result;
}

fn callErasedHostValueToBool(roc_host: *abi.RocHost, callable: abi.RocErasedCallable, arg0: HostValue) bool {
    const payload = erasedCallablePayload(callable);
    var call_args = ErasedHostValueUnaryArgs{ .arg0 = arg0 };
    var result: usize = 0;
    payload.callable_fn_ptr(
        roc_host,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return (result & 0xff) != 0;
}

fn callErasedHostValueToU64(roc_host: *abi.RocHost, callable: abi.RocErasedCallable, arg0: HostValue) u64 {
    const payload = erasedCallablePayload(callable);
    var call_args = ErasedHostValueUnaryArgs{ .arg0 = arg0 };
    var result: u64 = undefined;
    payload.callable_fn_ptr(
        roc_host,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return result;
}

fn callErasedHostValueToHostValueList(roc_host: *abi.RocHost, callable: abi.RocErasedCallable, arg0: HostValue) HostValueList {
    const payload = erasedCallablePayload(callable);
    var call_args = ErasedHostValueUnaryArgs{ .arg0 = arg0 };
    var result: HostValueList = undefined;
    payload.callable_fn_ptr(
        roc_host,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return result;
}

fn callErasedHostValueListToHostValue(roc_host: *abi.RocHost, callable: abi.RocErasedCallable, arg0: HostValueList) HostValue {
    const payload = erasedCallablePayload(callable);
    arg0.incref(1);
    var call_args = ErasedHostValueListUnaryArgs{ .arg0 = arg0 };
    var result: HostValue = undefined;
    payload.callable_fn_ptr(
        roc_host,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return result;
}

fn hostEventById(host: *HostEnv, event_id: u64) HostActiveEventDesc {
    if (event_id == 0 or event_id > host.active_events.items.len) {
        failHost("DOM event referenced an unknown active event");
    }
    return host.active_events.items[@intCast(event_id - 1)];
}

fn validateEventPayloadKind(desc: HostActiveEventDesc, actual_payload_kind: EventPayloadKind) void {
    if (desc.payload_kind != actual_payload_kind) {
        failHost("DOM event payload kind does not match Roc event descriptor");
    }
}

fn finishHostMetrics(host: *HostEnv) void {
    var metrics = addRuntimeMetrics(host.last_runtime_metrics, host.pending_roc_metrics);
    metrics.patches_emitted = host.render_metrics.patches_emitted;
    metrics.reset_dom = host.render_metrics.reset_dom;
    metrics.create_element = host.render_metrics.create_element;
    metrics.append_child = host.render_metrics.append_child;
    metrics.set_text = host.render_metrics.set_text;
    metrics.set_value = host.render_metrics.set_value;
    metrics.set_checked = host.render_metrics.set_checked;
    metrics.set_disabled = host.render_metrics.set_disabled;
    metrics.set_metadata = host.render_metrics.set_metadata;
    metrics.bind_event = host.render_metrics.bind_event;
    metrics.retained_alloc_delta = @as(i64, @intCast(host.alloc_count)) - @as(i64, @intCast(host.dealloc_count));
    metrics.events_processed = host.dispatch_metrics.events_processed;
    metrics.recompute_batches = host.dispatch_metrics.recompute_batches;
    host.last_runtime_metrics = metrics;
    host.pending_roc_metrics = zeroRuntimeMetrics();
}

fn applyDirtyStructuralSignalsLocally(host: *HostEnv, roc_host: *abi.RocHost, dirty_source_node_ids: []const u64, changes: []const HostDirtyStructuralSignal) CommandCounts {
    var total_counts: CommandCounts = .{};
    var applied_any = false;

    for (changes) |change| {
        var replacement_stream: HostNodeDescriptorStream = .{};
        defer replacement_stream.deinit(host.gpa.allocator(), roc_host, &host.pending_roc_metrics);
        const splice_and_targets: HostStructuralSpliceAndTargets = switch (change.kind) {
            .when => when_target: {
                const site = host.activeScopeSiteByNodeId(change.node_id, .when) orelse {
                    if (applied_any) continue;
                    failHost("dirty when structural site is not active");
                };
                const when_index = host.activeWhenIndexByNodeId(change.node_id) orelse {
                    if (applied_any) continue;
                    failHost("dirty when descriptor is not active");
                };
                const active_branch = change.branch orelse failHost("dirty when structural signal did not record its active branch");
                const replaced_scope_id = host.activeWhenBranchScopeId(site.scope_id, site.ordinal, active_branch.opposite()) orelse failHost("dirty when structural update had no active opposite branch scope");
                const when_desc = host.active_stream.whens.items[when_index];

                const replacement_scope_id = host.collectActiveWhenBranchDescriptors(roc_host, &replacement_stream, site, when_desc, active_branch, dirty_source_node_ids);
                const splice = host.spliceActiveStreamReplacingScope(roc_host, replaced_scope_id, site.render_insert_index, &replacement_stream);
                break :when_target .{
                    .splice = splice,
                    .targets = HostStructuralPatchTargets{
                        .removed = .{ .scope = replaced_scope_id },
                        .replacement = .{ .scope = replacement_scope_id },
                    },
                };
            },
            .each => each_target: {
                const site = host.activeScopeSiteByNodeId(change.node_id, .each) orelse {
                    if (applied_any) continue;
                    failHost("dirty each structural site is not active");
                };
                const each_index = host.activeEachIndexByNodeId(change.node_id) orelse {
                    if (applied_any) continue;
                    failHost("dirty each descriptor is not active");
                };
                const each_desc = host.active_stream.eaches.items[each_index];
                const each_site = HostEachSite{ .parent_scope_id = site.scope_id, .site_ordinal = site.ordinal };
                const target = HostStructuralReplacementTarget{ .each_site = each_site };
                const allocator = host.gpa.allocator();
                const old_active_rows = host.activeEachRowScopes(allocator, site.scope_id, site.ordinal);
                defer allocator.free(old_active_rows);
                const old_render_rows = host.activeEachRowScopesInRenderOrder(allocator, each_site);
                defer allocator.free(old_render_rows);
                const diff = host.syncActiveEachRowScopes(roc_host, site, each_desc);
                defer diff.deinit(allocator);

                if (old_active_rows.len == old_render_rows.len and HostEnv.eachDiffPreservesSurvivorRenderOrder(old_render_rows, diff.scope_ids)) {
                    const counts = host.applyDirtyEachRowScopeSplices(roc_host, site, each_desc, diff, dirty_source_node_ids);
                    total_counts.addAll(counts);
                    applied_any = true;
                    continue;
                }

                if (old_active_rows.len == old_render_rows.len and host.eachDiffIsPurePermutation(old_render_rows, diff, dirty_source_node_ids)) {
                    const counts = host.applyDirtyEachPermutationMoves(site, diff.scope_ids);
                    total_counts.addAll(counts);
                    applied_any = true;
                    continue;
                }

                host.collectActiveEachRowDescriptorsFromDiff(roc_host, &replacement_stream, site, each_desc, diff, dirty_source_node_ids);
                const splice = host.spliceActiveStreamReplacingTarget(
                    roc_host,
                    target,
                    site.render_insert_index,
                    &replacement_stream,
                );
                break :each_target .{
                    .splice = splice,
                    .targets = HostStructuralPatchTargets{
                        .removed = target,
                        .replacement = target,
                    },
                };
            },
        };
        defer splice_and_targets.splice.deinit(host.gpa.allocator());

        const counts = applySplicedStructuralNodeDescriptorTarget(host, roc_host, splice_and_targets.splice, splice_and_targets.targets);
        total_counts.addAll(counts);
        applied_any = true;
    }

    return total_counts;
}

fn applyDirtyWhenStructuralSignals(host: *HostEnv, roc_host: *abi.RocHost, dirty_source_node_ids: []const u64, changes: []const HostDirtyStructuralSignal) CommandCounts {
    for (changes) |change| {
        if (change.kind != .when) failHost("non-when structural change reached when-only test helper");
    }
    return applyDirtyStructuralSignalsLocally(host, roc_host, dirty_source_node_ids, changes);
}

fn renderActiveRootMeasured(host: *HostEnv, roc_host: *abi.RocHost, dirty_source_node_ids: []const u64, apply_ns: ?*u64, command_counts: ?*CommandCounts) void {
    const root = host.root_elem orelse failHost("host render requested before Roc root Elem was initialized");

    var next_stream: HostNodeDescriptorStream = .{};
    errdefer next_stream.deinit(host.gpa.allocator(), roc_host, &host.pending_roc_metrics);
    host.collectActiveElemRootDescriptors(roc_host, &next_stream, root, dirty_source_node_ids);

    const start_ns = benchmarkNowNs();
    const counts = if (host.dom_elements.items.len == 0)
        applyNodeDescriptorStream(host, roc_host, &next_stream)
    else
        applyStructuralNodeDescriptorStream(host, roc_host, &next_stream);
    const elapsed = benchmarkNowNs() - start_ns;
    if (apply_ns) |ns| ns.* += elapsed;
    if (command_counts) |total| total.addAll(counts);

    host.rebuildActiveEventsFromStream(&next_stream);
    host.active_stream.deinit(host.gpa.allocator(), roc_host, &host.pending_roc_metrics);
    host.active_stream = next_stream;
    finishHostMetrics(host);
}

fn acceptInitElemMeasured(host: *HostEnv, roc_host: *abi.RocHost, root_box: ElemBox, apply_ns: ?*u64, command_counts: ?*CommandCounts) void {
    if (host.root_elem != null) failHost("Roc root Elem initialized more than once");
    const root = root_box.*;
    host.root_elem = root;
    abi.decrefBoxWith(@ptrCast(root_box), @alignOf(abi.Elem), &dropMovedElemPayload, roc_host);
    renderActiveRootMeasured(host, roc_host, &.{}, apply_ns, command_counts);
}

fn acceptInitElem(host: *HostEnv, roc_host: *abi.RocHost, root_box: ElemBox) void {
    acceptInitElemMeasured(host, roc_host, root_box, null, null);
}

fn dispatchRocEventMeasured(host: *HostEnv, roc_host: *abi.RocHost, event_id: u64, payload_kind: EventPayloadKind, payload: HostValue, stats: ?*BenchmarkStats) void {
    const desc = hostEventById(host, event_id);
    validateEventPayloadKind(desc, payload_kind);
    host.setHostValueTypeTag(payload, desc.payload_tag);
    defer callErasedHostValueToUnit(roc_host, desc.payload_drop, payload);

    host.recordDispatch();

    var metrics = host.pending_roc_metrics;
    metrics.nodes_recomputed += 1;
    metrics.derived_calls_into_roc += 1;
    host.pending_roc_metrics = metrics;

    const start_ns = benchmarkNowNs();
    const current = host.stateValueByNodeId(desc.target_node_id);
    defer callErasedHostValueToUnit(roc_host, host.stateDropCallable(desc.target_node_id), current);
    const next = callErasedHostValueHostValueToHostValue(roc_host, desc.transform, current, payload);
    if (stats) |s| s.dispatch_roc_ns += benchmarkNowNs() - start_ns;

    const changed = host.updateStateValue(roc_host, desc.target_node_id, next);
    if (!changed) {
        var prune_metrics = host.pending_roc_metrics;
        prune_metrics.propagation_prunes += 1;
        host.pending_roc_metrics = prune_metrics;
        finishHostMetrics(host);
        if (stats) |s| s.actions += 1;
        return;
    }

    if (stats) |s| {
        const dirty_source_node_ids = [_]u64{desc.target_node_id};
        const dirty_generation = host.nextDirtySignalGeneration();
        const changed_record_ids = propagateDirtyActiveSignals(host, roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
        defer host.gpa.allocator().free(changed_record_ids);
        const dirty_structural_signals = collectDirtyStructuralSignals(host, roc_host, host.gpa.allocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
        defer host.gpa.allocator().free(dirty_structural_signals);
        if (dirty_structural_signals.len != 0) {
            const apply_start_ns = benchmarkNowNs();
            const sink_counts = applyDirtyRenderSinks(host, roc_host, &dirty_source_node_ids, changed_record_ids, dirty_generation);
            const counts = applyDirtyStructuralSignalsLocally(host, roc_host, &dirty_source_node_ids, dirty_structural_signals);
            s.dispatch_apply_ns += benchmarkNowNs() - apply_start_ns;
            s.commands.addAll(sink_counts);
            s.commands.addAll(counts);
            finishHostMetrics(host);
        } else {
            const apply_start_ns = benchmarkNowNs();
            const counts = applyDirtyRenderSinks(host, roc_host, &dirty_source_node_ids, changed_record_ids, dirty_generation);
            s.dispatch_apply_ns += benchmarkNowNs() - apply_start_ns;
            s.commands.addAll(counts);
            finishHostMetrics(host);
        }
        s.actions += 1;
    } else {
        const dirty_source_node_ids = [_]u64{desc.target_node_id};
        const dirty_generation = host.nextDirtySignalGeneration();
        const changed_record_ids = propagateDirtyActiveSignals(host, roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
        defer host.gpa.allocator().free(changed_record_ids);
        const dirty_structural_signals = collectDirtyStructuralSignals(host, roc_host, host.gpa.allocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
        defer host.gpa.allocator().free(dirty_structural_signals);
        if (dirty_structural_signals.len != 0) {
            _ = applyDirtyRenderSinks(host, roc_host, &dirty_source_node_ids, changed_record_ids, dirty_generation);
            _ = applyDirtyStructuralSignalsLocally(host, roc_host, &dirty_source_node_ids, dirty_structural_signals);
            finishHostMetrics(host);
        } else {
            _ = applyDirtyRenderSinks(host, roc_host, &dirty_source_node_ids, changed_record_ids, dirty_generation);
            finishHostMetrics(host);
        }
    }
}

fn dispatchRocEvent(host: *HostEnv, roc_host: *abi.RocHost, event_id: u64, payload_kind: EventPayloadKind, payload: HostValue) void {
    dispatchRocEventMeasured(host, roc_host, event_id, payload_kind, payload, null);
}

fn makeSignalsRocHost(host: *HostEnv) abi.RocHost {
    if (builtin.is_test) current_host = host;
    return .{
        .env = @ptrCast(host),
        .roc_alloc = &rocAllocFn,
        .roc_dealloc = &rocDeallocFn,
        .roc_realloc = &rocReallocFn,
        .roc_dbg = &rocDbgFn,
        .roc_expect_failed = &rocExpectFailedFn,
        .roc_crashed = &rocCrashedFn,
    };
}

fn commandIsAction(cmd: SpecCommand) bool {
    return switch (cmd.cmd_type) {
        .click, .fill, .check, .uncheck, .resolve_task, .reject_task, .tick_interval => true,
        else => false,
    };
}

fn runActionCommandMeasured(host: *HostEnv, roc_host: *abi.RocHost, cmd: SpecCommand, stats: *BenchmarkStats) void {
    switch (cmd.cmd_type) {
        .click => {
            const elem = host.findElementByLocator(cmd.locator, cmd.line_num) orelse failHost("benchmark click locator did not resolve");
            if (elem.disabled) failHost("benchmark click target is disabled");
            const event_id = elem.bound_click_event orelse failHost("benchmark click target has no binding");
            dispatchRocEventMeasured(host, roc_host, event_id, .unit, hostValueUnit(host, roc_host), stats);
        },

        .fill => {
            const value = cmd.expected_text orelse "";
            const elem = host.findElementByLocator(cmd.locator, cmd.line_num) orelse failHost("benchmark fill locator did not resolve");
            if (elem.disabled) failHost("benchmark fill target is disabled");
            if (elem.bound_input_event) |event_id| {
                dispatchRocEventMeasured(host, roc_host, event_id, .str, hostValueStr(host, roc_host, value), stats);
            } else {
                _ = setElementValueIfChanged(host, elem, value);
            }
        },

        .check, .uncheck => {
            const checked = cmd.cmd_type == .check;
            const elem = host.findElementByLocator(cmd.locator, cmd.line_num) orelse failHost("benchmark check locator did not resolve");
            if (elem.disabled) failHost("benchmark check target is disabled");
            if (elem.bound_check_event) |event_id| {
                dispatchRocEventMeasured(host, roc_host, event_id, .bool, hostValueBool(host, roc_host, checked), stats);
            } else {
                _ = setElementCheckedIfChanged(elem, checked);
            }
        },

        .resolve_task, .reject_task => {
            const task_name = cmd.task_name orelse failHost("benchmark task command had no task name");
            const payload = cmd.expected_text orelse "";
            const start_ns = benchmarkNowNs();
            const counts = resolvePendingTask(host, roc_host, task_name, payload, cmd.cmd_type == .reject_task);
            stats.dispatch_apply_ns += benchmarkNowNs() - start_ns;
            stats.commands.addAll(counts);
            finishHostMetrics(host);
            stats.actions += 1;
        },

        .tick_interval => {
            const period_ms = cmd.interval_ms orelse failHost("benchmark interval command had no period");
            const start_ns = benchmarkNowNs();
            const counts = tickIntervalSource(host, roc_host, period_ms);
            stats.dispatch_apply_ns += benchmarkNowNs() - start_ns;
            stats.commands.addAll(counts);
            finishHostMetrics(host);
            stats.actions += 1;
        },

        else => {},
    }
}

fn runBenchmarkIteration(commands: []const SpecCommand, verbose: bool, stats: *BenchmarkStats) void {
    var host_env = HostEnv.init();
    host_env.test_state.verbose = verbose;

    var roc_host = makeSignalsRocHost(&host_env);
    host_env.roc_host = &roc_host;
    current_host = &host_env;
    current_roc_host = &roc_host;

    const init_start_ns = benchmarkNowNs();
    const init_result = abi.roc_ui_init();
    stats.init_roc_ns += benchmarkNowNs() - init_start_ns;
    acceptInitElemMeasured(&host_env, &roc_host, init_result, &stats.init_apply_ns, &stats.commands);

    for (commands) |cmd| {
        if (commandIsAction(cmd)) {
            runActionCommandMeasured(&host_env, &roc_host, cmd, stats);
        }
    }

    const retained_delta = @as(i64, @intCast(host_env.alloc_count)) - @as(i64, @intCast(host_env.dealloc_count));
    var iteration_metrics = host_env.last_runtime_metrics;
    iteration_metrics.retained_alloc_delta = retained_delta;
    stats.metrics = addRuntimeMetrics(stats.metrics, iteration_metrics);
    stats.allocs += @intCast(host_env.alloc_count);
    stats.deallocs += @intCast(host_env.dealloc_count);
    stats.retained_alloc_delta += retained_delta;

    host_env.deinit();
    current_host = null;
    current_roc_host = null;
}

fn printBenchmarkHeader() void {
    writeStdout("case,sample,iterations,actions,init_roc_ns,init_apply_ns,dispatch_roc_ns,dispatch_apply_ns,total_ns,allocs,deallocs,retained_alloc_delta,commands,reset_dom,create_element,append_child,set_text,set_value,set_checked,set_disabled,set_metadata,bind_event,active_graph_records_rebuilt,stream_nodes_scanned,each_key_compares,allocs_this_event,deallocs_this_event,events_processed,nodes_recomputed,propagation_prunes,derived_calls_into_roc,recompute_batches,patches_emitted,scopes_created,scopes_disposed,rows_reused,rows_created,rows_removed,closure_retains,closure_releases,metrics_retained_alloc_delta\n");
}

fn printBenchmarkRow(case_name: []const u8, sample: usize, iterations: usize, stats: BenchmarkStats) void {
    const total_ns = stats.init_roc_ns + stats.init_apply_ns + stats.dispatch_roc_ns + stats.dispatch_apply_ns;
    printStdout(
        "{s},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},",
        .{
            case_name,
            sample,
            iterations,
            stats.actions,
            stats.init_roc_ns,
            stats.init_apply_ns,
            stats.dispatch_roc_ns,
            stats.dispatch_apply_ns,
            total_ns,
            stats.allocs,
            stats.deallocs,
            stats.retained_alloc_delta,
        },
    );
    printStdout(
        "{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},",
        .{
            stats.commands.total,
            stats.commands.reset_dom,
            stats.commands.create_element,
            stats.commands.append_child,
            stats.commands.set_text,
            stats.commands.set_value,
            stats.commands.set_checked,
            stats.commands.set_disabled,
            stats.commands.set_metadata,
            stats.commands.bind_event,
        },
    );
    printStdout(
        "{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d}\n",
        .{
            stats.metrics.active_graph_records_rebuilt,
            stats.metrics.stream_nodes_scanned,
            stats.metrics.each_key_compares,
            stats.metrics.allocs_this_event,
            stats.metrics.deallocs_this_event,
            stats.metrics.events_processed,
            stats.metrics.nodes_recomputed,
            stats.metrics.propagation_prunes,
            stats.metrics.derived_calls_into_roc,
            stats.metrics.recompute_batches,
            stats.metrics.patches_emitted,
            stats.metrics.scopes_created,
            stats.metrics.scopes_disposed,
            stats.metrics.rows_reused,
            stats.metrics.rows_created,
            stats.metrics.rows_removed,
            stats.metrics.closure_retains,
            stats.metrics.closure_releases,
            stats.metrics.retained_alloc_delta,
        },
    );
}

fn runAppBenchmarks(spec_file: []const u8, case_name: []const u8, iterations: usize, samples: usize, verbose: bool) !c_int {
    var bench_gpa = std.heap.DebugAllocator(.{ .safety = true }){};
    defer _ = bench_gpa.deinit();
    const allocator = bench_gpa.allocator();
    const commands = parseTestSpecFile(allocator, spec_file) catch |err| {
        switch (err) {
            ParseError.FileNotFound => writeStderr("Error: Test spec file not found\n"),
            ParseError.InvalidFormat => writeStderr("Error: Invalid test spec format\n"),
            else => writeStderr("Error: Failed to parse test spec\n"),
        }
        return 1;
    };
    defer freeSpecCommands(allocator, commands);

    printBenchmarkHeader();
    for (0..samples) |sample| {
        var stats: BenchmarkStats = .{};
        for (0..iterations) |_| {
            runBenchmarkIteration(commands, verbose, &stats);
        }
        printBenchmarkRow(case_name, sample, iterations, stats);
    }

    return 0;
}

comptime {
    if (!builtin.is_test) {
        @export(&hostAlloc, .{ .name = "roc_alloc", .visibility = .hidden });
        @export(&hostDealloc, .{ .name = "roc_dealloc", .visibility = .hidden });
        @export(&hostRealloc, .{ .name = "roc_realloc", .visibility = .hidden });
        @export(&hostDbg, .{ .name = "roc_dbg", .visibility = .hidden });
        @export(&hostExpectFailed, .{ .name = "roc_expect_failed", .visibility = .hidden });
        @export(&hostCrashed, .{ .name = "roc_crashed", .visibility = .hidden });
        @export(&hostValueClone, .{ .name = "roc_host_value_clone", .visibility = .hidden });
        @export(&hostValueGet, .{ .name = "roc_host_value_get", .visibility = .hidden });
        @export(&hostValueGetTagged, .{ .name = "roc_host_value_get_tagged", .visibility = .hidden });
        @export(&hostValueStore, .{ .name = "roc_host_value_store", .visibility = .hidden });
        @export(&hostValueStoreTagged, .{ .name = "roc_host_value_store_tagged", .visibility = .hidden });
        @export(&hostValueTake, .{ .name = "roc_host_value_take", .visibility = .hidden });
        @export(&hostValueTakeTagged, .{ .name = "roc_host_value_take_tagged", .visibility = .hidden });

        @export(&main, .{ .name = "main" });
        if (@import("builtin").os.tag == .windows) {
            @export(&__main, .{ .name = "__main" });
        }
    }
}

fn __main() callconv(.c) void {}

fn main(argc: c_int, argv: [*][*:0]u8) callconv(.c) c_int {
    var verbose = false;
    var bench_app = false;
    var bench_name: []const u8 = "app_dispatch";
    var bench_iterations: usize = 100;
    var bench_samples: usize = 3;
    var spec_file: ?[]const u8 = null;

    var i: usize = 1;
    const arg_count: usize = @intCast(argc);
    while (i < arg_count) : (i += 1) {
        const arg = std.mem.span(argv[i]);
        if (std.mem.eql(u8, arg, "--verbose") or std.mem.eql(u8, arg, "-v")) {
            verbose = true;
        } else if (std.mem.eql(u8, arg, "--bench-app")) {
            bench_app = true;
        } else if (std.mem.eql(u8, arg, "--bench-name")) {
            i += 1;
            if (i >= arg_count) {
                writeStderr("Usage: ./app [--verbose] <test_spec.txt>\n       ./app --bench-app [--bench-name NAME] [--bench-iterations N] [--bench-samples N] <test_spec.txt>\n");
                return 1;
            }
            bench_name = std.mem.span(argv[i]);
        } else if (std.mem.eql(u8, arg, "--bench-iterations")) {
            i += 1;
            if (i >= arg_count) {
                writeStderr("Usage: ./app [--verbose] <test_spec.txt>\n       ./app --bench-app [--bench-name NAME] [--bench-iterations N] [--bench-samples N] <test_spec.txt>\n");
                return 1;
            }
            bench_iterations = std.fmt.parseInt(usize, std.mem.span(argv[i]), 10) catch {
                writeStderr("Error: Invalid --bench-iterations value\n");
                return 1;
            };
            if (bench_iterations == 0) {
                writeStderr("Error: --bench-iterations must be greater than zero\n");
                return 1;
            }
        } else if (std.mem.eql(u8, arg, "--bench-samples")) {
            i += 1;
            if (i >= arg_count) {
                writeStderr("Usage: ./app [--verbose] <test_spec.txt>\n       ./app --bench-app [--bench-name NAME] [--bench-iterations N] [--bench-samples N] <test_spec.txt>\n");
                return 1;
            }
            bench_samples = std.fmt.parseInt(usize, std.mem.span(argv[i]), 10) catch {
                writeStderr("Error: Invalid --bench-samples value\n");
                return 1;
            };
            if (bench_samples == 0) {
                writeStderr("Error: --bench-samples must be greater than zero\n");
                return 1;
            }
        } else if (arg.len > 0 and arg[0] != '-') {
            spec_file = arg;
        } else {
            writeStderr("Usage: ./app [--verbose] <test_spec.txt>\n       ./app --bench-app [--bench-name NAME] [--bench-iterations N] [--bench-samples N] <test_spec.txt>\n");
            return 1;
        }
    }

    if (spec_file == null) {
        writeStderr("Usage: ./app [--verbose] <test_spec.txt>\n       ./app --bench-app [--bench-name NAME] [--bench-iterations N] [--bench-samples N] <test_spec.txt>\n");
        return 1;
    }

    if (bench_app) {
        return runAppBenchmarks(spec_file.?, bench_name, bench_iterations, bench_samples, verbose) catch |err| {
            writeStderr("HOST ERROR: ");
            writeStderr(@errorName(err));
            writeStderr("\n");
            return 1;
        };
    }

    return platform_main(spec_file.?, verbose) catch |err| {
        writeStderr("HOST ERROR: ");
        writeStderr(@errorName(err));
        writeStderr("\n");
        return 1;
    };
}

fn platform_main(spec_file: []const u8, verbose: bool) !c_int {
    _ = base.signal_handler.installForCurrentThread(.{
        .stack_overflow = handleRocStackOverflow,
        .access_violation = handleRocAccessViolation,
        .arithmetic_error = handleRocArithmeticError,
    });

    var host_env = HostEnv.init();
    const allocator = host_env.gpa.allocator();

    host_env.test_state.commands = parseTestSpecFile(allocator, spec_file) catch |err| {
        switch (err) {
            ParseError.FileNotFound => writeStderr("Error: Test spec file not found\n"),
            ParseError.InvalidFormat => writeStderr("Error: Invalid test spec format\n"),
            else => writeStderr("Error: Failed to parse test spec\n"),
        }
        return 1;
    };
    host_env.test_state.verbose = verbose;

    var roc_host = makeSignalsRocHost(&host_env);
    host_env.roc_host = &roc_host;
    current_host = &host_env;
    current_roc_host = &roc_host;
    defer current_host = null;
    defer current_roc_host = null;
    defer host_env.deinit();

    acceptInitElem(&host_env, &roc_host, abi.roc_ui_init());

    if (verbose) {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "[INFO] UI built: {d} DOM elements, {d} recomputed nodes\n", .{
            host_env.dom_elements.items.len,
            host_env.last_runtime_metrics.nodes_recomputed,
        }) catch "";
        writeStderr(msg);
        host_env.dumpDom();
    }

    for (host_env.test_state.commands) |cmd| {
        switch (cmd.cmd_type) {
            .mark_metrics => {
                host_env.test_state.metrics_mark = host_env.last_runtime_metrics;
            },

            .click => {
                const elem = host_env.findElementByLocator(cmd.locator, cmd.line_num) orelse {
                    writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                    return 1;
                };
                if (elem.disabled) {
                    writeLocatorFailure(cmd.line_num, "target is disabled");
                    return 1;
                }
                const event_id = elem.bound_click_event orelse {
                    writeLocatorFailure(cmd.line_num, "target has no click binding");
                    return 1;
                };
                dispatchRocEvent(&host_env, &roc_host, event_id, .unit, hostValueUnit(&host_env, &roc_host));
            },

            .fill => {
                const value = cmd.expected_text orelse "";
                const elem = host_env.findElementByLocator(cmd.locator, cmd.line_num) orelse {
                    writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                    return 1;
                };
                if (elem.disabled) {
                    writeLocatorFailure(cmd.line_num, "target is disabled");
                    return 1;
                }
                if (elem.bound_input_event) |event_id| {
                    dispatchRocEvent(&host_env, &roc_host, event_id, .str, hostValueStr(&host_env, &roc_host, value));
                } else {
                    _ = setElementValueIfChanged(&host_env, elem, value);
                }
            },

            .check, .uncheck => {
                const checked = cmd.cmd_type == .check;
                const elem = host_env.findElementByLocator(cmd.locator, cmd.line_num) orelse {
                    writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                    return 1;
                };
                if (elem.disabled) {
                    writeLocatorFailure(cmd.line_num, "target is disabled");
                    return 1;
                }
                if (elem.bound_check_event) |event_id| {
                    dispatchRocEvent(&host_env, &roc_host, event_id, .bool, hostValueBool(&host_env, &roc_host, checked));
                } else {
                    _ = setElementCheckedIfChanged(elem, checked);
                }
            },

            .resolve_task, .reject_task => {
                const task_name = cmd.task_name orelse {
                    writeLocatorFailure(cmd.line_num, "task command had no task name");
                    return 1;
                };
                const payload = cmd.expected_text orelse "";
                _ = resolvePendingTask(&host_env, &roc_host, task_name, payload, cmd.cmd_type == .reject_task);
                finishHostMetrics(&host_env);
            },

            .tick_interval => {
                const period_ms = cmd.interval_ms orelse {
                    writeLocatorFailure(cmd.line_num, "interval command had no period");
                    return 1;
                };
                _ = tickIntervalSource(&host_env, &roc_host, period_ms);
                finishHostMetrics(&host_env);
            },

            .expect_visible => {
                _ = host_env.findElementByLocator(cmd.locator, cmd.line_num) orelse {
                    writeLocatorFailure(cmd.line_num, "locator did not resolve to one visible element");
                    return 1;
                };
            },

            .expect_absent => {
                const match_count = host_env.countElementsByLocator(cmd.locator);
                if (match_count != 0) {
                    writeAbsentFailure(cmd.line_num, match_count);
                    return 1;
                }
            },

            .expect_text => {
                const expected = cmd.expected_text orelse "";
                const elem = host_env.findElementByLocator(cmd.locator, cmd.line_num) orelse {
                    writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                    return 1;
                };
                const actual = elem.text orelse "";
                if (!std.mem.eql(u8, actual, expected)) {
                    writeStringMismatch(cmd.line_num, "text", expected, actual);
                    return 1;
                }
            },

            .expect_value => {
                const expected = cmd.expected_text orelse "";
                const elem = host_env.findElementByLocator(cmd.locator, cmd.line_num) orelse {
                    writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                    return 1;
                };
                const actual = elem.value orelse "";
                if (!std.mem.eql(u8, actual, expected)) {
                    writeStringMismatch(cmd.line_num, "value", expected, actual);
                    return 1;
                }
            },

            .expect_checked => {
                const expected = cmd.expected_bool orelse false;
                const elem = host_env.findElementByLocator(cmd.locator, cmd.line_num) orelse {
                    writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                    return 1;
                };
                if (elem.checked != expected) {
                    writeBoolMismatch(cmd.line_num, "checked", expected, elem.checked);
                    return 1;
                }
            },

            .expect_disabled => {
                const expected = cmd.expected_bool orelse false;
                const elem = host_env.findElementByLocator(cmd.locator, cmd.line_num) orelse {
                    writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                    return 1;
                };
                if (elem.disabled != expected) {
                    writeBoolMismatch(cmd.line_num, "disabled", expected, elem.disabled);
                    return 1;
                }
            },

            .expect_updates => {
                const expected = cmd.expected_count orelse 0;
                const elem = host_env.findElementByLocator(cmd.locator, cmd.line_num) orelse {
                    writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                    return 1;
                };
                const actual = elem.text_update_count + elem.value_update_count + elem.checked_update_count + elem.disabled_update_count;
                if (actual != expected) {
                    var buf: [512]u8 = undefined;
                    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected updates: {d}\n  Got updates:      {d}\n", .{ cmd.line_num, expected, actual }) catch "TEST FAILED\n";
                    writeStderr(msg);
                    return 1;
                }
            },

            .expect_cleanup => {
                const name = cmd.task_name orelse "";
                const expected = cmd.expected_count orelse 0;
                const actual = host_env.cleanupEventCount(name);
                if (actual != expected) {
                    var buf: [512]u8 = undefined;
                    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected cleanup \"{s}\": {d}\n  Got cleanup count:       {d}\n", .{ cmd.line_num, name, expected, actual }) catch "TEST FAILED\n";
                    writeStderr(msg);
                    return 1;
                }
            },

            .expect_pending_task => {
                const name = cmd.task_name orelse "";
                const expected = cmd.expected_count orelse 0;
                const actual = host_env.pendingTaskCountByName(name);
                if (actual != expected) {
                    var buf: [512]u8 = undefined;
                    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected pending task \"{s}\": {d}\n  Got pending task count:       {d}\n", .{ cmd.line_num, name, expected, actual }) catch "TEST FAILED\n";
                    writeStderr(msg);
                    return 1;
                }
            },

            .expect_interval => {
                const period_ms = cmd.interval_ms orelse 0;
                const expected = cmd.expected_count orelse 0;
                const actual = host_env.activeIntervalRecordCountByPeriod(period_ms);
                if (actual != expected) {
                    var buf: [512]u8 = undefined;
                    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected active interval {d}ms: {d}\n  Got active interval count:   {d}\n", .{ cmd.line_num, period_ms, expected, actual }) catch "TEST FAILED\n";
                    writeStderr(msg);
                    return 1;
                }
            },

            .expect_metric_delta => {
                const metric_name = cmd.expected_text orelse "";
                const expected = cmd.expected_metric_delta orelse 0;
                const marked = host_env.test_state.metrics_mark orelse {
                    writeMetricFailure(cmd.line_num, "mark_metrics must run before expect_metric_delta");
                    return 1;
                };
                const start = runtimeMetricValue(marked, metric_name) orelse {
                    writeUnknownMetric(cmd.line_num, metric_name);
                    return 1;
                };
                const current = runtimeMetricValue(host_env.last_runtime_metrics, metric_name) orelse {
                    writeUnknownMetric(cmd.line_num, metric_name);
                    return 1;
                };
                const actual = current - start;
                if (actual != expected) {
                    writeMetricDeltaMismatch(cmd.line_num, metric_name, expected, actual);
                    return 1;
                }
            },
        }
    }

    if (verbose) {
        writeStderr("[PASS] All tests passed\n");
    }

    return 0;
}

fn writeLocatorFailure(line_num: usize, message: []const u8) void {
    var buf: [256]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}: {s}\n", .{ line_num, message }) catch "TEST FAILED\n";
    writeStderr(msg);
}

fn writeAbsentFailure(line_num: usize, match_count: usize) void {
    var buf: [256]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}: expected no matching elements, found {d}\n", .{ line_num, match_count }) catch "TEST FAILED\n";
    writeStderr(msg);
}

fn writeStringMismatch(line_num: usize, field: []const u8, expected: []const u8, actual: []const u8) void {
    var buf: [512]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected {s}: \"{s}\"\n  Got {s}:      \"{s}\"\n", .{ line_num, field, expected, field, actual }) catch "TEST FAILED\n";
    writeStderr(msg);
}

fn writeBoolMismatch(line_num: usize, field: []const u8, expected: bool, actual: bool) void {
    var buf: [512]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected {s}: {}\n  Got {s}:      {}\n", .{ line_num, field, expected, field, actual }) catch "TEST FAILED\n";
    writeStderr(msg);
}

fn writeMetricFailure(line_num: usize, message: []const u8) void {
    var buf: [256]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}: {s}\n", .{ line_num, message }) catch "TEST FAILED\n";
    writeStderr(msg);
}

fn writeUnknownMetric(line_num: usize, metric_name: []const u8) void {
    var buf: [256]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}: unknown metric \"{s}\"\n", .{ line_num, metric_name }) catch "TEST FAILED\n";
    writeStderr(msg);
}

fn writeMetricDeltaMismatch(line_num: usize, metric_name: []const u8, expected: i64, actual: i64) void {
    var buf: [512]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected {s} delta: {d}\n  Got {s} delta:      {d}\n", .{ line_num, metric_name, expected, metric_name, actual }) catch "TEST FAILED\n";
    writeStderr(msg);
}

fn appendTestSignalDescriptor(host: *HostEnv, signal_id: u64, kind: SignalKind, source_event_ids: []const u64, input_signal_ids: []const u64) !void {
    if (!builtin.is_test) @compileError("appendTestSignalDescriptor is test-only");

    const allocator = host.gpa.allocator();
    const owned_source_states = try allocator.dupe(u64, &.{});
    errdefer allocator.free(owned_source_states);
    const owned_source_events = try allocator.dupe(u64, source_event_ids);
    errdefer allocator.free(owned_source_events);
    const owned_inputs = try allocator.dupe(u64, input_signal_ids);
    errdefer allocator.free(owned_inputs);

    try host.signal_descriptors.append(allocator, .{
        .signal_id = signal_id,
        .kind = kind,
        .source_state_ids = owned_source_states,
        .source_event_ids = owned_source_events,
        .input_signal_ids = owned_inputs,
        .rank = 0,
    });
}

fn deinitTestHostGraph(host: *HostEnv) void {
    if (!builtin.is_test) @compileError("deinitTestHostGraph is test-only");

    const allocator = host.gpa.allocator();
    host.clearActiveSignalRoutes();
    host.active_source_signal_routes.deinit(allocator);
    host.active_text_signal_routes.deinit(allocator);
    host.active_bool_signal_routes.deinit(allocator);
    host.active_change_signal_routes.deinit(allocator);
    host.active_structural_signal_routes.deinit(allocator);
    host.clearActiveSignalGraph();
    host.active_signal_graph.deinit(allocator);
    host.clearActiveEvents();
    host.active_events.deinit(allocator);
    host.clearEventDescriptors();
    host.event_descriptors.deinit(allocator);
    host.clearSignalEventRoutes();
    host.signal_event_routes.deinit(allocator);
    host.clearSignalDescriptors();
    host.signal_descriptors.deinit(allocator);
    host.clearSignalRoutes();
    host.signal_routes.deinit(allocator);
    host.clearSignalDependents();
    host.signal_dependents.deinit(allocator);
    host.clearSignalCache();
    host.signal_cache.deinit(allocator);
}

fn deinitTestHostIdentity(host: *HostEnv) void {
    if (!builtin.is_test) @compileError("deinitTestHostIdentity is test-only");

    const allocator = host.gpa.allocator();
    host.clearScopes();
    host.scopes.deinit(allocator);
    host.node_identities.deinit(allocator);
    host.dom_identities.deinit(allocator);
    for (host.host_values.items) |slot| {
        switch (slot) {
            .vacant => {},
            .occupied => failHost("test host value registry still owned a typed cell at shutdown"),
        }
    }
    host.host_values.deinit(allocator);
    host.test_host_value_kinds.deinit(allocator);
    host.roc_allocations.deinit(allocator);
}

test "signals host dirty plan deduplicates diamond join by rank" {
    var host = HostEnv.init();
    defer {
        deinitTestHostGraph(&host);
        _ = host.gpa.deinit();
    }
    const allocator = host.gpa.allocator();

    try host.event_descriptors.append(allocator, .{
        .event_id = 1,
        .payload_kind = .unit,
    });
    try appendTestSignalDescriptor(&host, 0, .source, &.{1}, &.{});
    try appendTestSignalDescriptor(&host, 1, .map, &.{}, &.{0});
    try appendTestSignalDescriptor(&host, 2, .map, &.{}, &.{0});
    try appendTestSignalDescriptor(&host, 3, .map2, &.{}, &.{ 1, 2 });

    host.rebuildSignalTopologyFromSignals();
    host.rebuildSignalEventRoutesFromSignals();

    const dirty_signal_ids = host.dirtySignalIdsForEvent(allocator, 1);
    defer allocator.free(dirty_signal_ids);

    try std.testing.expectEqualSlices(u64, &.{ 0, 1, 2, 3 }, dirty_signal_ids);
    try std.testing.expectEqual(@as(u64, 0), host.signalRank(0));
    try std.testing.expectEqual(@as(u64, 1), host.signalRank(1));
    try std.testing.expectEqual(@as(u64, 1), host.signalRank(2));
    try std.testing.expectEqual(@as(u64, 2), host.signalRank(3));
}

test "signals metrics accumulate propagation pruning counters" {
    var left = zeroRuntimeMetrics();
    left.active_graph_records_rebuilt = 7;
    left.allocs_this_event = 9;
    left.deallocs_this_event = 6;
    left.events_processed = 2;
    left.nodes_recomputed = 5;
    left.propagation_prunes = 3;
    left.derived_calls_into_roc = 4;
    left.each_key_compares = 6;
    left.recompute_batches = 2;
    left.patches_emitted = 7;
    left.create_element = 2;
    left.append_child = 3;
    left.set_text = 1;
    left.bind_event = 1;
    left.stream_nodes_scanned = 12;

    var right = zeroRuntimeMetrics();
    right.active_graph_records_rebuilt = 2;
    right.allocs_this_event = 4;
    right.deallocs_this_event = 5;
    right.events_processed = 1;
    right.nodes_recomputed = 8;
    right.propagation_prunes = 11;
    right.derived_calls_into_roc = 6;
    right.each_key_compares = 7;
    right.recompute_batches = 1;
    right.patches_emitted = 13;
    right.create_element = 5;
    right.append_child = 8;
    right.set_text = 2;
    right.bind_event = 4;
    right.stream_nodes_scanned = 5;
    right.retained_alloc_delta = -2;

    const total = addRuntimeMetrics(left, right);
    try std.testing.expectEqual(@as(u64, 9), total.active_graph_records_rebuilt);
    try std.testing.expectEqual(@as(u64, 13), total.allocs_this_event);
    try std.testing.expectEqual(@as(u64, 11), total.deallocs_this_event);
    try std.testing.expectEqual(@as(u64, 3), total.events_processed);
    try std.testing.expectEqual(@as(u64, 13), total.nodes_recomputed);
    try std.testing.expectEqual(@as(u64, 14), total.propagation_prunes);
    try std.testing.expectEqual(@as(u64, 10), total.derived_calls_into_roc);
    try std.testing.expectEqual(@as(u64, 13), total.each_key_compares);
    try std.testing.expectEqual(@as(u64, 3), total.recompute_batches);
    try std.testing.expectEqual(@as(u64, 20), total.patches_emitted);
    try std.testing.expectEqual(@as(u64, 7), total.create_element);
    try std.testing.expectEqual(@as(u64, 11), total.append_child);
    try std.testing.expectEqual(@as(u64, 3), total.set_text);
    try std.testing.expectEqual(@as(u64, 5), total.bind_event);
    try std.testing.expectEqual(@as(u64, 17), total.stream_nodes_scanned);
    try std.testing.expectEqual(@as(i64, -2), total.retained_alloc_delta);
}

test "signals host assigns explicit active graph record ids" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const label = testNodeStableStrMapExpr(&roc_host, testNodeRefExpr(state_token));
    const root = testNodeStateWithTokenAndInitial(
        &roc_host,
        state_token,
        testHostValueI64(1),
        testNodeTextSignal(&roc_host, label),
    );
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    _ = applyNodeDescriptorStream(&host, &roc_host, &stream);
    host.active_stream = stream;

    try std.testing.expectEqual(@as(usize, 2), host.active_signal_graph.items.len);
    try std.testing.expectEqual(@as(u64, 2), host.pending_roc_metrics.active_graph_records_rebuilt);

    const first_record = host.active_signal_graph.items[0].record;
    const second_record = host.active_signal_graph.items[1].record;
    try std.testing.expectEqual(@as(?u64, 0), first_record.active_graph_id);
    try std.testing.expectEqual(@as(?u64, 1), second_record.active_graph_id);
    try std.testing.expectEqual(@as(u64, 0), host.requireActiveSignalRecordId(first_record));
    try std.testing.expectEqual(@as(u64, 1), host.requireActiveSignalRecordId(second_record));

    host.clearActiveSignalGraph();

    try std.testing.expectEqual(@as(usize, 0), host.active_signal_graph.items.len);
    try std.testing.expectEqual(@as(?u64, null), first_record.active_graph_id);
    try std.testing.expectEqual(@as(?u64, null), second_record.active_graph_id);
}

test "signals host allocation ledger updates moved header indexes" {
    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const first = rocAllocFn(&roc_host, 8, 8) orelse return error.OutOfMemory;
    const middle = rocAllocFn(&roc_host, 16, 8) orelse return error.OutOfMemory;
    const last = rocAllocFn(&roc_host, 24, 8) orelse return error.OutOfMemory;

    try std.testing.expectEqual(@as(usize, 3), host.roc_allocations.items.len);
    try std.testing.expectEqual(@as(usize, 2), rocAllocationHeaderFromUserPtr(last).ledger_index);

    rocDeallocFn(&roc_host, middle, 8);

    try std.testing.expectEqual(@as(usize, 2), host.roc_allocations.items.len);
    try std.testing.expectEqual(@as(usize, 1), rocAllocationHeaderFromUserPtr(last).ledger_index);
    try std.testing.expectEqual(@as(u64, 3), host.pending_roc_metrics.allocs_this_event);
    try std.testing.expectEqual(@as(u64, 1), host.pending_roc_metrics.deallocs_this_event);

    const grown = rocReallocFn(&roc_host, first, 32, 8) orelse return error.OutOfMemory;

    try std.testing.expectEqual(@as(usize, 2), host.roc_allocations.items.len);
    try std.testing.expectEqual(@as(u64, 4), host.alloc_count);
    try std.testing.expectEqual(@as(u64, 2), host.dealloc_count);
    try std.testing.expectEqual(@as(u64, 4), host.pending_roc_metrics.allocs_this_event);
    try std.testing.expectEqual(@as(u64, 2), host.pending_roc_metrics.deallocs_this_event);

    rocDeallocFn(&roc_host, last, 8);
    rocDeallocFn(&roc_host, grown, 8);

    try std.testing.expectEqual(@as(usize, 0), host.roc_allocations.items.len);
    try std.testing.expectEqual(@as(u64, 4), host.alloc_count);
    try std.testing.expectEqual(@as(u64, 4), host.dealloc_count);
    try std.testing.expectEqual(@as(u64, 4), host.pending_roc_metrics.allocs_this_event);
    try std.testing.expectEqual(@as(u64, 4), host.pending_roc_metrics.deallocs_this_event);
}

const TestErasedI64Capture = extern struct {
    amount: i64,
};

const TestErasedBinderCapture = extern struct {
    condition_binder: HostBinderToken,
};

const TestErasedHostValueCapture = extern struct {
    value: HostValue,
};

var test_erased_callable_drop_count: u64 = 0;
var test_row_elem_call_count: u64 = 0;

fn testCapturePtrAs(comptime T: type, capture_ptr: ?[*]u8) *T {
    return @ptrCast(@alignCast(capture_ptr orelse unreachable));
}

fn testErasedArgsAs(comptime T: type, args: ?[*]const u8) *align(1) const T {
    return @as(*align(1) const T, @ptrCast(args orelse unreachable));
}

fn writeTestErasedResult(comptime T: type, ret: ?[*]u8, value: T) void {
    @as(*align(1) T, @ptrCast(ret orelse unreachable)).* = value;
}

fn writeTestErasedCallable(
    comptime Capture: type,
    roc_host: *abi.RocHost,
    callable_fn_ptr: abi.RocErasedCallableFn,
    on_drop: ?abi.RocErasedCallableOnDrop,
    capture: Capture,
) abi.RocErasedCallable {
    comptime {
        if (@alignOf(Capture) > abi.roc_erased_callable_capture_alignment) {
            @compileError("signals test erased-callable capture alignment exceeds Roc ABI alignment");
        }
    }
    const callable = abi.rocErasedCallableAllocate(roc_host, callable_fn_ptr, on_drop, @sizeOf(Capture));
    testCapturePtrAs(Capture, abi.rocErasedCallableCapturePtr(callable)).* = capture;
    return callable;
}

fn testCurrentRocHost() *abi.RocHost {
    return currentHost().roc_host orelse failHost("test HostValue helper requires an active Roc host");
}

fn testHostValueUnit() HostValue {
    const host = currentHost();
    return hostValueUnit(host, testCurrentRocHost());
}

fn testHostValueStr(roc_host: *abi.RocHost, value: []const u8) HostValue {
    return hostValueStr(hostFromRocHost(roc_host), roc_host, value);
}

fn testHostValueBool(value: bool) HostValue {
    const host = currentHost();
    return hostValueBool(host, testCurrentRocHost(), value);
}

fn testHostValueI64(value: i64) HostValue {
    const host = currentHost();
    return hostValueI64(host, testCurrentRocHost(), value);
}

fn testReadHostValueI64(roc_host: *abi.RocHost, value: HostValue) i64 {
    const host = hostFromRocHost(roc_host);
    if (host.testHostValueKind(value) != .i64) @panic("test HostValue expected I64");
    const box = host.getHostValue(value);
    defer abi.decrefBox(box, roc_host);
    const payload: *const i64 = @ptrCast(@alignCast(box orelse unreachable));
    return payload.*;
}

fn testReadHostValueBool(roc_host: *abi.RocHost, value: HostValue) bool {
    const host = hostFromRocHost(roc_host);
    if (host.testHostValueKind(value) != .bool) @panic("test HostValue expected Bool");
    const box = host.getHostValue(value);
    defer abi.decrefBox(box, roc_host);
    const payload: *const bool = @ptrCast(@alignCast(box orelse unreachable));
    return payload.*;
}

fn testReadHostValueStr(roc_host: *abi.RocHost, value: HostValue) RocStr {
    const host = hostFromRocHost(roc_host);
    if (host.testHostValueKind(value) != .str) @panic("test HostValue expected Str");
    const box = host.getHostValue(value);
    defer abi.decrefBox(box, roc_host);
    const payload: *const RocStr = @ptrCast(@alignCast(box orelse unreachable));
    return payload.*;
}

fn testReadHostValueI64List(roc_host: *abi.RocHost, value: HostValue) I64List {
    const host = hostFromRocHost(roc_host);
    if (host.testHostValueKind(value) != .i64_list) @panic("test HostValue expected List(I64)");
    const box = host.getHostValue(value);
    defer abi.decrefBox(box, roc_host);
    const payload: *const I64List = @ptrCast(@alignCast(box orelse unreachable));
    return payload.*;
}

fn testDropRocStrBoxPayload(data_ptr: ?*anyopaque, roc_host: *abi.RocHost) callconv(.c) void {
    const payload: *RocStr = @ptrCast(@alignCast(data_ptr orelse return));
    payload.*.decref(roc_host);
}

fn testDropI64ListBoxPayload(data_ptr: ?*anyopaque, roc_host: *abi.RocHost) callconv(.c) void {
    const payload: *I64List = @ptrCast(@alignCast(data_ptr orelse return));
    payload.*.decref(roc_host);
}

fn testDropHostValue(roc_host: *abi.RocHost, value: HostValue) void {
    const host = hostFromRocHost(roc_host);
    const kind = host.testHostValueKind(value);
    const box = host.takeHostValue(value);
    switch (kind) {
        .unit, .i64, .bool => abi.decrefBox(box, roc_host),
        .str => abi.decrefBoxWith(box, @alignOf(RocStr), &testDropRocStrBoxPayload, roc_host),
        .i64_list => abi.decrefBoxWith(box, @alignOf(I64List), &testDropI64ListBoxPayload, roc_host),
    }
}

fn testHostValueDropCallable(roc_host: *abi.RocHost) abi.RocErasedCallable {
    return writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testDropHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
}

fn testHostValueEqCallable(roc_host: *abi.RocHost) abi.RocErasedCallable {
    return writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testHostValueEqErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
}

fn testHostValueHashCallable(roc_host: *abi.RocHost) abi.RocErasedCallable {
    return writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testHostValueHashErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
}

fn testReadStrCallable(roc_host: *abi.RocHost) abi.RocErasedCallable {
    return writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testReadStrHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
}

fn testReadBoolCallable(roc_host: *abi.RocHost) abi.RocErasedCallable {
    return writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testReadBoolHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
}

fn testItemsToValuesCallable(roc_host: *abi.RocHost) abi.RocErasedCallable {
    return writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testI64ListToHostValuesCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
}

fn testUnaryHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    const capture = testCapturePtrAs(TestErasedI64Capture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedHostValueUnaryArgs, args);
    const input = testReadHostValueI64(roc_host, call_args.arg0);
    writeTestErasedResult(HostValue, ret, hostValueI64(hostFromRocHost(roc_host), roc_host, input + capture.amount));
}

fn testHostValueHashErasedCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = capture_ptr;
    const call_args = testErasedArgsAs(ErasedHostValueUnaryArgs, args);
    const value = testReadHostValueI64(roc_host, call_args.arg0);
    writeTestErasedResult(u64, ret, @intCast(value));
}

fn testUnaryIdentityHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = capture_ptr;
    const call_args = testErasedArgsAs(ErasedHostValueUnaryArgs, args);
    writeTestErasedResult(HostValue, ret, hostFromRocHost(roc_host).cloneHostValue(call_args.arg0));
}

fn testBinaryHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    const capture = testCapturePtrAs(TestErasedI64Capture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedHostValueBinaryArgs, args);
    const left = testReadHostValueI64(roc_host, call_args.arg0);
    const right = testReadHostValueI64(roc_host, call_args.arg1);
    writeTestErasedResult(HostValue, ret, hostValueI64(hostFromRocHost(roc_host), roc_host, left + right + capture.amount));
}

fn testUnitIncrementHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = capture_ptr;
    const call_args = testErasedArgsAs(ErasedHostValueBinaryArgs, args);
    const current = testReadHostValueI64(roc_host, call_args.arg0);
    if (hostFromRocHost(roc_host).testHostValueKind(call_args.arg1) != .unit) @panic("test unit event callable expected unit payload");
    writeTestErasedResult(HostValue, ret, hostValueI64(hostFromRocHost(roc_host), roc_host, current + 1));
}

fn testInitialHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = args;
    const capture = testCapturePtrAs(TestErasedHostValueCapture, capture_ptr);
    writeTestErasedResult(HostValue, ret, hostFromRocHost(roc_host).cloneHostValue(capture.value));
}

fn testBinaryElemCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    const capture = testCapturePtrAs(TestErasedI64Capture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedHostValueBinaryArgs, args);
    const left = testReadHostValueI64(roc_host, call_args.arg0);
    const right = testReadHostValueI64(roc_host, call_args.arg1);
    var text_buffer: [64]u8 = undefined;
    const text = std.fmt.bufPrint(&text_buffer, "row-{d}", .{left + right + capture.amount}) catch @panic("test row Elem callable could not format text");
    writeTestErasedResult(abi.Elem, ret, testNodeText(roc_host, text));
}

fn testStatefulRowElemCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    test_row_elem_call_count += 1;
    const capture = testCapturePtrAs(TestErasedI64Capture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedHostValueBinaryArgs, args);
    const key = testReadHostValueI64(roc_host, call_args.arg0);
    const item = testReadHostValueI64(roc_host, call_args.arg1);
    var text_buffer: [64]u8 = undefined;
    const text = std.fmt.bufPrint(&text_buffer, "row-{d}-{d}", .{ key, item + capture.amount }) catch @panic("test stateful row Elem callable could not format text");
    writeTestErasedResult(abi.Elem, ret, testNodeState(roc_host, testNodeText(roc_host, text)));
}

fn testStatefulRowButtonElemCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    test_row_elem_call_count += 1;
    const capture = testCapturePtrAs(TestErasedI64Capture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedHostValueBinaryArgs, args);
    const key = testReadHostValueI64(roc_host, call_args.arg0);
    const item = testReadHostValueI64(roc_host, call_args.arg1);
    var text_buffer: [64]u8 = undefined;
    const text = std.fmt.bufPrint(&text_buffer, "row-action-{d}-{d}", .{ key, item + capture.amount }) catch @panic("test stateful row button Elem callable could not format text");
    const token = newTestBinderToken(roc_host);
    const attrs = [_]abi.NodeAttr{
        testNodeStaticTextAttr(roc_host, .text, text),
        testNodeEventAttr(roc_host, .click, token, .unit),
    };
    const button = testElementWith(roc_host, "button", &attrs, &.{});
    writeTestErasedResult(abi.Elem, ret, testNodeStateWithToken(roc_host, token, button));
}

fn testNestedWhenRowElemCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    test_row_elem_call_count += 1;
    const capture = testCapturePtrAs(TestErasedBinderCapture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedHostValueBinaryArgs, args);
    const key = testReadHostValueI64(roc_host, call_args.arg0);
    const item = testReadHostValueI64(roc_host, call_args.arg1);

    var true_text_buffer: [64]u8 = undefined;
    var false_text_buffer: [64]u8 = undefined;
    const true_text = std.fmt.bufPrint(&true_text_buffer, "row-{d}-{d}-true", .{ key, item }) catch @panic("test nested row true text format failed");
    const false_text = std.fmt.bufPrint(&false_text_buffer, "row-{d}-{d}-false", .{ key, item }) catch @panic("test nested row false text format failed");
    const row = abi.Elem{
        .payload = .{
            .when = .{
                .condition = boxTestNodeSignalExpr(roc_host, testNodeRefExpr(capture.condition_binder)),
                .read = testReadBoolCallable(roc_host),
                .when_false = boxTestElem(roc_host, testNodeText(roc_host, false_text)),
                .when_true = boxTestElem(roc_host, testNodeText(roc_host, true_text)),
            },
        },
        .tag = .When,
    };
    writeTestErasedResult(abi.Elem, ret, row);
}

fn testHostValueEqErasedCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = capture_ptr;
    const call_args = testErasedArgsAs(ErasedHostValueBinaryArgs, args);
    const host = hostFromRocHost(roc_host);
    const left_kind = host.testHostValueKind(call_args.arg0);
    const right_kind = host.testHostValueKind(call_args.arg1);
    const is_equal = if (left_kind != right_kind) false else switch (left_kind) {
        .unit => true,
        .i64 => testReadHostValueI64(roc_host, call_args.arg0) == testReadHostValueI64(roc_host, call_args.arg1),
        .bool => testReadHostValueBool(roc_host, call_args.arg0) == testReadHostValueBool(roc_host, call_args.arg1),
        .str => blk: {
            const left = testReadHostValueStr(roc_host, call_args.arg0);
            const right = testReadHostValueStr(roc_host, call_args.arg1);
            break :blk std.mem.eql(u8, left.asSlice(), right.asSlice());
        },
        .i64_list => blk: {
            const left = testReadHostValueI64List(roc_host, call_args.arg0);
            const right = testReadHostValueI64List(roc_host, call_args.arg1);
            break :blk std.mem.eql(i64, left.items(), right.items());
        },
    };
    writeTestErasedResult(bool, ret, is_equal);
}

fn testStableStrHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = args;
    _ = capture_ptr;
    writeTestErasedResult(HostValue, ret, hostValueStr(hostFromRocHost(roc_host), roc_host, "stable"));
}

fn testStableI64HostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = args;
    const capture = testCapturePtrAs(TestErasedI64Capture, capture_ptr);
    writeTestErasedResult(HostValue, ret, hostValueI64(hostFromRocHost(roc_host), roc_host, capture.amount));
}

fn testStableBoolHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = args;
    _ = capture_ptr;
    writeTestErasedResult(HostValue, ret, hostValueBool(hostFromRocHost(roc_host), roc_host, true));
}

fn testAlwaysEqualHostValueCallable(_: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = args;
    _ = capture_ptr;
    writeTestErasedResult(bool, ret, true);
}

fn testNeverEqualHostValueCallable(_: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = args;
    _ = capture_ptr;
    writeTestErasedResult(bool, ret, false);
}

fn testErasedCallableOnDrop(_: ?[*]u8, _: *abi.RocHost) callconv(.c) void {
    test_erased_callable_drop_count += 1;
}

fn testBinderCaptureOnDrop(capture_ptr: ?[*]u8, roc_host: *abi.RocHost) callconv(.c) void {
    test_erased_callable_drop_count += 1;
    const capture = testCapturePtrAs(TestErasedBinderCapture, capture_ptr);
    abi.decrefBox(@ptrCast(capture.condition_binder), roc_host);
}

fn testDropHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = ret;
    _ = capture_ptr;
    const call_args = testErasedArgsAs(ErasedHostValueUnaryArgs, args);
    testDropHostValue(roc_host, call_args.arg0);
}

fn testReadStrHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = capture_ptr;
    const call_args = testErasedArgsAs(ErasedHostValueUnaryArgs, args);
    var value = testReadHostValueStr(roc_host, call_args.arg0);
    value.incref(1);
    writeTestErasedResult(RocStr, ret, value);
}

fn testReadBoolHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = capture_ptr;
    const call_args = testErasedArgsAs(ErasedHostValueUnaryArgs, args);
    writeTestErasedResult(bool, ret, testReadHostValueBool(roc_host, call_args.arg0));
}

fn testI64ListToHostValuesCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = capture_ptr;
    const host = hostFromRocHost(roc_host);
    const call_args = testErasedArgsAs(ErasedHostValueUnaryArgs, args);
    const source = testReadHostValueI64List(roc_host, call_args.arg0);
    const source_items = source.items();
    const result = HostValueList.allocate(source_items.len, roc_host);
    if (source_items.len > 0) {
        const dest = result.elements_ptr orelse unreachable;
        for (source_items, 0..) |item, index| {
            dest[index] = hostValueI64(host, roc_host, item);
        }
    }
    writeTestErasedResult(HostValueList, ret, result);
}

fn testHostValueCaptureOnDrop(capture_ptr: ?[*]u8, roc_host: *abi.RocHost) callconv(.c) void {
    test_erased_callable_drop_count += 1;
    const capture = testCapturePtrAs(TestErasedHostValueCapture, capture_ptr);
    testDropHostValue(roc_host, capture.value);
}

fn expectHostValueI64(value: HostValue, expected: i64) !void {
    const roc_host = testCurrentRocHost();
    try std.testing.expectEqual(expected, testReadHostValueI64(roc_host, value));
    testDropHostValue(roc_host, value);
}

test "signals host invokes erased HostValue thunks with ABI argument layouts" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    {
        const unary = writeTestErasedCallable(
            TestErasedI64Capture,
            &roc_host,
            &testUnaryHostValueCallable,
            &testErasedCallableOnDrop,
            .{ .amount = 5 },
        );
        defer abi.decrefErasedCallable(unary, &roc_host);

        const input = testHostValueI64(37);
        defer testDropHostValue(&roc_host, input);
        const result = callErasedHostValueToHostValue(&roc_host, unary, input);
        try expectHostValueI64(result, 42);
    }

    {
        const binary = writeTestErasedCallable(
            TestErasedI64Capture,
            &roc_host,
            &testBinaryHostValueCallable,
            &testErasedCallableOnDrop,
            .{ .amount = 3 },
        );
        defer abi.decrefErasedCallable(binary, &roc_host);

        const left = testHostValueI64(10);
        defer testDropHostValue(&roc_host, left);
        const right = testHostValueI64(29);
        defer testDropHostValue(&roc_host, right);
        const result = callErasedHostValueHostValueToHostValue(&roc_host, binary, left, right);
        try expectHostValueI64(result, 42);
    }

    {
        const row = writeTestErasedCallable(
            TestErasedI64Capture,
            &roc_host,
            &testBinaryElemCallable,
            &testErasedCallableOnDrop,
            .{ .amount = 3 },
        );
        defer abi.decrefErasedCallable(row, &roc_host);

        const left = testHostValueI64(10);
        defer testDropHostValue(&roc_host, left);
        const right = testHostValueI64(29);
        defer testDropHostValue(&roc_host, right);
        const result = callErasedHostValueHostValueToElem(&roc_host, row, left, right);
        defer abi.decrefElem(result, &roc_host);

        try std.testing.expectEqual(abi.ElemTag.Text, result.tag);
        try std.testing.expectEqualStrings("row-42", result.payload.text.asSlice());
    }

    {
        const eq = writeTestErasedCallable(
            TestErasedI64Capture,
            &roc_host,
            &testHostValueEqErasedCallable,
            &testErasedCallableOnDrop,
            .{ .amount = 0 },
        );
        defer abi.decrefErasedCallable(eq, &roc_host);

        const equal_left = testHostValueI64(42);
        defer testDropHostValue(&roc_host, equal_left);
        const equal_right = testHostValueI64(42);
        defer testDropHostValue(&roc_host, equal_right);
        try std.testing.expect(callErasedHostValueHostValueToBool(&roc_host, eq, equal_left, equal_right));

        const unequal_left = testHostValueI64(41);
        defer testDropHostValue(&roc_host, unequal_left);
        const unequal_right = testHostValueI64(42);
        defer testDropHostValue(&roc_host, unequal_right);
        try std.testing.expect(!callErasedHostValueHostValueToBool(&roc_host, eq, unequal_left, unequal_right));
    }

    try std.testing.expectEqual(@as(u64, 4), test_erased_callable_drop_count);
}

test "signals host interns scopes and node identities from explicit paths" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    const root = host.internRootScope();
    try std.testing.expectEqual(@as(u64, 0), root);
    try std.testing.expectEqual(root, host.internRootScope());

    const true_branch = host.internWhenBranchScope(root, 2, .true_branch);
    try std.testing.expectEqual(true_branch, host.internWhenBranchScope(root, 2, .true_branch));

    const false_branch = host.internWhenBranchScope(root, 2, .false_branch);
    try std.testing.expect(false_branch != true_branch);

    const nested_true_branch = host.internWhenBranchScope(true_branch, 2, .true_branch);
    try std.testing.expect(nested_true_branch != true_branch);

    const key_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        &roc_host,
        &testHostValueEqErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    defer abi.decrefErasedCallable(key_eq, &roc_host);

    const initial_keys = [_]HostValue{ testHostValueI64(10), testHostValueI64(11) };
    const initial_rows = syncTestEachRowScopes(&host, &roc_host, root, 7, &initial_keys, &initial_keys, key_eq, key_eq);
    defer freeKeyedRowDiff(&host, initial_rows);
    const row_a = initial_rows.scope_ids[0];
    const row_b = initial_rows.scope_ids[1];
    try std.testing.expect(row_b != row_a);

    const same_keys = [_]HostValue{testHostValueI64(10)};
    const same_rows = syncTestEachRowScopes(&host, &roc_host, root, 7, &same_keys, &same_keys, key_eq, key_eq);
    defer freeKeyedRowDiff(&host, same_rows);
    try std.testing.expectEqual(row_a, same_rows.scope_ids[0]);

    const other_site_keys = [_]HostValue{testHostValueI64(10)};
    const other_site_rows = syncTestEachRowScopes(&host, &roc_host, root, 8, &other_site_keys, &other_site_keys, key_eq, key_eq);
    defer freeKeyedRowDiff(&host, other_site_rows);
    const same_key_other_site = other_site_rows.scope_ids[0];
    try std.testing.expect(same_key_other_site != row_a);

    const root_state = host.internNodeIdentity(root, 0);
    try std.testing.expectEqual(root_state, host.internNodeIdentity(root, 0));

    const row_state = host.internNodeIdentity(row_a, 0);
    try std.testing.expect(row_state != root_state);

    const row_next_state = host.internNodeIdentity(row_a, 1);
    try std.testing.expect(row_next_state != row_state);
}

test "signals host disposal retires scope subtree identities" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    const key_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        &roc_host,
        &testHostValueEqErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    defer abi.decrefErasedCallable(key_eq, &roc_host);

    const root = host.internRootScope();
    const row = createTestEachRowScope(&host, &roc_host, root, 3, testHostValueI64(10), testHostValueI64(10), key_eq, key_eq);
    const branch = host.internWhenBranchScope(row, 4, .true_branch);
    const row_state = host.internNodeIdentity(row, 0);
    const branch_state = host.internNodeIdentity(branch, 0);

    host.disposeScopeSubtree(&roc_host, row);
    try std.testing.expectEqual(@as(u64, 2), host.pending_roc_metrics.scopes_disposed);
    try std.testing.expect(!host.scopes.items[@intCast(row)].active);
    try std.testing.expect(!host.scopes.items[@intCast(branch)].active);
    try std.testing.expect(!host.node_identities.items[@intCast(row_state)].active);
    try std.testing.expect(!host.node_identities.items[@intCast(branch_state)].active);

    const recreated_row = createTestEachRowScope(&host, &roc_host, root, 3, testHostValueI64(10), testHostValueI64(10), key_eq, key_eq);
    try std.testing.expect(recreated_row != row);

    const recreated_state = host.internNodeIdentity(recreated_row, 0);
    try std.testing.expect(recreated_state != row_state);
}

test "signals host patches dirty leaf sinks without descriptor rebuild" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const root = testNodeStateWithTokenAndInitial(
        &roc_host,
        state_token,
        testHostValueStr(&roc_host, "first"),
        testNodeTextSignal(&roc_host, testNodeRefExpr(state_token)),
    );
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    const initial_counts = applyNodeDescriptorStream(&host, &roc_host, &stream);
    host.active_stream = stream;

    try std.testing.expectEqual(@as(u64, 1), initial_counts.reset_dom);
    try std.testing.expectEqual(@as(u64, 1), initial_counts.set_text);
    try std.testing.expectEqual(@as(usize, 2), host.dom_elements.items.len);
    try std.testing.expectEqualStrings("first", host.dom_elements.items[1].text.?);

    const state_id = host.active_stream.scope_sites.items[0].node_id;
    const state_index = host.stateIndexByNodeId(state_id) orelse unreachable;
    testDropHostValue(&roc_host, host.states.items[state_index].cell.value);
    host.states.items[state_index].cell.value = testHostValueStr(&roc_host, "second");
    host.states.items[state_index].version += 1;

    const dirty_source_node_ids = [_]u64{state_id};
    const dirty_generation = host.nextDirtySignalGeneration();
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
    defer host.gpa.allocator().free(changed_record_ids);
    const dirty_structural_signals = collectDirtyStructuralSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer host.gpa.allocator().free(dirty_structural_signals);
    try std.testing.expectEqual(@as(usize, 0), dirty_structural_signals.len);

    const patch_start = host.render_metrics.patches_emitted;
    const patch_counts = applyDirtyRenderSinks(&host, &roc_host, &dirty_source_node_ids, changed_record_ids, dirty_generation);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.reset_dom);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.create_element);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.append_child);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.set_text);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.total);
    try std.testing.expectEqual(patch_start + 1, host.render_metrics.patches_emitted);
    try std.testing.expectEqual(@as(usize, 2), host.dom_elements.items.len);
    try std.testing.expectEqualStrings("second", host.dom_elements.items[1].text.?);

    const unchanged_generation = host.nextDirtySignalGeneration();
    const unchanged_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, unchanged_generation);
    defer host.gpa.allocator().free(unchanged_record_ids);
    const unchanged_counts = applyDirtyRenderSinks(&host, &roc_host, &dirty_source_node_ids, unchanged_record_ids, unchanged_generation);
    try std.testing.expectEqual(@as(u64, 0), unchanged_counts.total);
}

test "signals host prunes dirty leaf sink when retained map equality is unchanged" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const stable_label = testNodeStableStrMapExpr(&roc_host, testNodeRefExpr(state_token));
    const root = testNodeStateWithTokenAndInitial(
        &roc_host,
        state_token,
        testHostValueI64(1),
        testNodeTextSignal(&roc_host, stable_label),
    );
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    const initial_counts = applyNodeDescriptorStream(&host, &roc_host, &stream);
    host.active_stream = stream;

    try std.testing.expectEqual(@as(u64, 1), initial_counts.set_text);
    try std.testing.expectEqualStrings("stable", host.dom_elements.items[1].text.?);

    const state_id = host.active_stream.scope_sites.items[0].node_id;
    const state_index = host.stateIndexByNodeId(state_id) orelse unreachable;
    testDropHostValue(&roc_host, host.states.items[state_index].cell.value);
    host.states.items[state_index].cell.value = testHostValueI64(2);
    host.states.items[state_index].version += 1;

    const dirty_source_node_ids = [_]u64{state_id};
    const prune_start = host.pending_roc_metrics.propagation_prunes;
    const dirty_generation = host.nextDirtySignalGeneration();
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
    defer host.gpa.allocator().free(changed_record_ids);
    const patch_counts = applyDirtyRenderSinks(&host, &roc_host, &dirty_source_node_ids, changed_record_ids, dirty_generation);

    try std.testing.expectEqual(@as(u64, 0), patch_counts.total);
    try std.testing.expectEqual(prune_start + 1, host.pending_roc_metrics.propagation_prunes);
    try std.testing.expectEqualStrings("stable", host.dom_elements.items[1].text.?);
}

test "signals host evaluates shared dirty record once per batch" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const shared_label = testNodeStableStrMapExpr(&roc_host, testNodeRefExpr(state_token));
    abi.increfNodeSignalExpr(shared_label, 1);
    const children = [_]abi.Elem{
        testNodeTextSignal(&roc_host, shared_label),
        testNodeTextSignal(&roc_host, shared_label),
    };
    const root = testNodeStateWithTokenAndInitial(
        &roc_host,
        state_token,
        testHostValueI64(1),
        testElement(&roc_host, &children),
    );
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    const initial_counts = applyNodeDescriptorStream(&host, &roc_host, &stream);
    host.active_stream = stream;

    try std.testing.expectEqual(@as(u64, 2), initial_counts.set_text);
    try std.testing.expectEqualStrings("stable", host.dom_elements.items[2].text.?);
    try std.testing.expectEqualStrings("stable", host.dom_elements.items[3].text.?);
    try std.testing.expect(host.active_stream.signal_text_nodes.items[0].signal.record == host.active_stream.signal_text_nodes.items[1].signal.record);
    try std.testing.expectEqual(@as(usize, 2), host.active_signal_graph.items.len);

    const shared_record = host.active_stream.signal_text_nodes.items[0].signal.record;
    const shared_record_id = host.requireActiveSignalRecordId(shared_record);
    const source_record = switch (shared_record.payload) {
        .map => |payload| payload.input,
        else => failHost("shared label test expected a map signal record"),
    };
    const source_record_id = host.requireActiveSignalRecordId(source_record);
    try std.testing.expectEqual(@as(u64, 0), host.activeSignalRank(source_record_id));
    try std.testing.expectEqual(@as(u64, 1), host.activeSignalRank(shared_record_id));
    const expected_dependents = [_]u64{shared_record_id};
    try std.testing.expectEqualSlices(u64, &expected_dependents, host.dependentActiveSignalRecordIds(source_record_id));

    const state_id = host.active_stream.scope_sites.items[0].node_id;
    try std.testing.expectEqual(@as(usize, @intCast(state_id + 1)), host.active_source_signal_routes.items.len);
    const expected_source_routes = [_]u64{source_record_id};
    try std.testing.expectEqualSlices(u64, &expected_source_routes, host.active_source_signal_routes.items[@intCast(state_id)].items);
    try std.testing.expectEqual(@as(usize, 2), host.active_text_signal_routes.items[@intCast(shared_record_id)].items.len);

    const state_index = host.stateIndexByNodeId(state_id) orelse unreachable;
    testDropHostValue(&roc_host, host.states.items[state_index].cell.value);
    host.states.items[state_index].cell.value = testHostValueI64(2);
    host.states.items[state_index].version += 1;

    const dirty_source_node_ids = [_]u64{state_id};
    const prune_start = host.pending_roc_metrics.propagation_prunes;
    const derived_start = host.pending_roc_metrics.derived_calls_into_roc;
    const dirty_generation = host.nextDirtySignalGeneration();
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
    defer host.gpa.allocator().free(changed_record_ids);
    const patch_counts = applyDirtyRenderSinks(&host, &roc_host, &dirty_source_node_ids, changed_record_ids, dirty_generation);

    try std.testing.expectEqual(@as(u64, 0), patch_counts.total);
    try std.testing.expectEqual(prune_start + 1, host.pending_roc_metrics.propagation_prunes);
    try std.testing.expectEqual(derived_start + 1, host.pending_roc_metrics.derived_calls_into_roc);
}

test "signals host skips parent transform when dirty child output is unchanged" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const stable_count = testNodeStableI64MapExpr(&roc_host, testNodeRefExpr(state_token), 42);
    const parent_label = testNodeStableStrMapExpr(&roc_host, stable_count);
    const root = testNodeStateWithTokenAndInitial(
        &roc_host,
        state_token,
        testHostValueI64(1),
        testNodeTextSignal(&roc_host, parent_label),
    );
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    const initial_counts = applyNodeDescriptorStream(&host, &roc_host, &stream);
    host.active_stream = stream;

    try std.testing.expectEqual(@as(u64, 1), initial_counts.set_text);
    try std.testing.expectEqualStrings("stable", host.dom_elements.items[1].text.?);

    const state_id = host.active_stream.scope_sites.items[0].node_id;
    const state_index = host.stateIndexByNodeId(state_id) orelse unreachable;
    testDropHostValue(&roc_host, host.states.items[state_index].cell.value);
    host.states.items[state_index].cell.value = testHostValueI64(2);
    host.states.items[state_index].version += 1;

    const dirty_source_node_ids = [_]u64{state_id};
    const prune_start = host.pending_roc_metrics.propagation_prunes;
    const derived_start = host.pending_roc_metrics.derived_calls_into_roc;
    const dirty_generation = host.nextDirtySignalGeneration();
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
    defer host.gpa.allocator().free(changed_record_ids);
    const patch_counts = applyDirtyRenderSinks(&host, &roc_host, &dirty_source_node_ids, changed_record_ids, dirty_generation);

    try std.testing.expectEqual(@as(u64, 0), patch_counts.total);
    try std.testing.expectEqual(prune_start + 1, host.pending_roc_metrics.propagation_prunes);
    try std.testing.expectEqual(derived_start + 1, host.pending_roc_metrics.derived_calls_into_roc);
    try std.testing.expectEqualStrings("stable", host.dom_elements.items[1].text.?);
}

test "signals host prunes dirty combine output through cache-owned equality" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const initial_items = [_]HostValue{ testHostValueI64(1), testHostValueI64(2) };
    const combine = testNodeCombineExpr(&roc_host, &.{});
    var stream: HostNodeDescriptorStream = .{};
    defer stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);
    var binding = host.bindNodeSignal(host.gpa.allocator(), &stream, combine, &.{});
    defer binding.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);
    var cache: HostSignalCacheSlot = .absent;
    cache.replace(&roc_host, &host.pending_roc_metrics, testHostValueI64List(&roc_host, &initial_items), hostSignalBindingEqCallable(&host, &binding), hostSignalBindingDropCallable(&host, &binding));
    defer cache.deinit(&roc_host, &host.pending_roc_metrics);
    abi.decrefNodeSignalExpr(combine, &roc_host);

    const dirty_items = [_]HostValue{ testHostValueI64(3), testHostValueI64(4) };
    const prune_start = host.pending_roc_metrics.propagation_prunes;
    try std.testing.expect(!updateDirtySignalCache(&host, &roc_host, &cache, testHostValueI64List(&roc_host, &dirty_items)));
    try std.testing.expectEqual(prune_start + 1, host.pending_roc_metrics.propagation_prunes);
}

test "signals host marks dirty structural sources for structural patching" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const when_elem: abi.Elem = .{
        .payload = .{
            .when = .{
                .condition = boxTestNodeSignalExpr(&roc_host, testNodeRefExpr(state_token)),
                .read = testReadBoolCallable(&roc_host),
                .when_false = boxTestElem(&roc_host, testNodeText(&roc_host, "false branch")),
                .when_true = boxTestElem(&roc_host, testNodeText(&roc_host, "true branch")),
            },
        },
        .tag = .When,
    };
    const root = testNodeStateWithTokenAndInitial(&roc_host, state_token, testHostValueBool(true), when_elem);
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    const initial_counts = applyNodeDescriptorStream(&host, &roc_host, &stream);
    host.active_stream = stream;

    try std.testing.expectEqual(@as(u64, 1), initial_counts.reset_dom);
    const state_id = host.active_stream.scope_sites.items[0].node_id;
    const state_index = host.stateIndexByNodeId(state_id) orelse unreachable;
    testDropHostValue(&roc_host, host.states.items[state_index].cell.value);
    host.states.items[state_index].cell.value = testHostValueBool(false);
    host.states.items[state_index].version += 1;
    const dirty_source_node_ids = [_]u64{state_id};
    const dirty_generation = host.nextDirtySignalGeneration();
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
    defer host.gpa.allocator().free(changed_record_ids);
    const dirty_structural_signals = collectDirtyStructuralSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer host.gpa.allocator().free(dirty_structural_signals);
    try std.testing.expectEqual(@as(usize, 1), dirty_structural_signals.len);
    try std.testing.expectEqual(HostActiveStructuralSignalKind.when, dirty_structural_signals[0].kind);
    try std.testing.expectEqual(HostScopeBranch.false_branch, dirty_structural_signals[0].branch.?);

    const patch_counts = applyDirtyWhenStructuralSignals(&host, &roc_host, &dirty_source_node_ids, dirty_structural_signals);

    try std.testing.expectEqual(@as(u64, 0), patch_counts.reset_dom);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.create_element);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.append_child);
    try std.testing.expect(activeTextElementId(&host, "true branch") == null);
    try std.testing.expect(activeTextElementId(&host, "false branch") != null);
}

test "signals host prunes structural render when retained condition equality is unchanged" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const condition = testNodeStableBoolMapExpr(&roc_host, testNodeRefExpr(state_token));
    const when_elem: abi.Elem = .{
        .payload = .{
            .when = .{
                .condition = boxTestNodeSignalExpr(&roc_host, condition),
                .read = testReadBoolCallable(&roc_host),
                .when_false = boxTestElem(&roc_host, testNodeText(&roc_host, "false branch")),
                .when_true = boxTestElem(&roc_host, testNodeText(&roc_host, "true branch")),
            },
        },
        .tag = .When,
    };
    const root = testNodeStateWithTokenAndInitial(&roc_host, state_token, testHostValueI64(1), when_elem);
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    _ = applyNodeDescriptorStream(&host, &roc_host, &stream);
    host.active_stream = stream;

    try std.testing.expect(activeTextElementId(&host, "true branch") != null);

    const state_id = host.active_stream.scope_sites.items[0].node_id;
    const state_index = host.stateIndexByNodeId(state_id) orelse unreachable;
    testDropHostValue(&roc_host, host.states.items[state_index].cell.value);
    host.states.items[state_index].cell.value = testHostValueI64(2);
    host.states.items[state_index].version += 1;

    const dirty_source_node_ids = [_]u64{state_id};
    const prune_start = host.pending_roc_metrics.propagation_prunes;

    const dirty_generation = host.nextDirtySignalGeneration();
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
    defer host.gpa.allocator().free(changed_record_ids);
    const dirty_structural_signals = collectDirtyStructuralSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer host.gpa.allocator().free(dirty_structural_signals);
    try std.testing.expectEqual(@as(usize, 0), dirty_structural_signals.len);
    try std.testing.expectEqual(prune_start + 1, host.pending_roc_metrics.propagation_prunes);
    try std.testing.expect(activeTextElementId(&host, "true branch") != null);
    try std.testing.expect(activeTextElementId(&host, "false branch") == null);
}

test "signals host structural patch reorders keyed row DOM without recreating survivors" {
    test_erased_callable_drop_count = 0;
    test_row_elem_call_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const initial_items = [_]HostValue{ testHostValueI64(1), testHostValueI64(2), testHostValueI64(3) };
    const initial_children = [_]abi.Elem{
        testNodeEachWithItems(&roc_host, &initial_items),
    };
    const initial_root = testElementWith(&roc_host, "section", &.{}, &initial_children);
    defer abi.decrefElem(initial_root, &roc_host);

    var initial_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &initial_stream, initial_root, &.{});
    const initial_counts = applyNodeDescriptorStream(&host, &roc_host, &initial_stream);
    host.active_stream = initial_stream;

    try std.testing.expectEqual(@as(u64, 3), test_row_elem_call_count);
    try std.testing.expectEqual(@as(u64, 1), initial_counts.reset_dom);
    try std.testing.expectEqual(@as(u64, 4), initial_counts.create_element);
    try std.testing.expectEqual(@as(usize, 5), host.dom_elements.items.len);

    const section_id = host.active_stream.elements.items[0].elem_id;
    const row_1_id = activeTextElementId(&host, "row-1-1") orelse unreachable;
    const row_2_id = activeTextElementId(&host, "row-2-2") orelse unreachable;
    const row_3_id = activeTextElementId(&host, "row-3-3") orelse unreachable;
    try std.testing.expectEqualSlices(u64, &.{ row_1_id, row_2_id, row_3_id }, host.dom_elements.items[@intCast(section_id)].children.items);

    const reordered_items = [_]HostValue{ testHostValueI64(3), testHostValueI64(1), testHostValueI64(2) };
    const reordered_children = [_]abi.Elem{
        testNodeEachWithItems(&roc_host, &reordered_items),
    };
    const reordered_root = testElementWith(&roc_host, "section", &.{}, &reordered_children);
    defer abi.decrefElem(reordered_root, &roc_host);

    var reordered_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &reordered_stream, reordered_root, &.{});
    const patch_counts = applyStructuralNodeDescriptorStream(&host, &roc_host, &reordered_stream);
    host.active_stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);
    host.active_stream = reordered_stream;

    try std.testing.expectEqual(@as(u64, 3), test_row_elem_call_count);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.reset_dom);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.create_element);
    try std.testing.expect(patch_counts.append_child >= 2);
    try std.testing.expectEqual(@as(usize, 5), host.dom_elements.items.len);
    try std.testing.expectEqual(row_1_id, activeTextElementId(&host, "row-1-1") orelse unreachable);
    try std.testing.expectEqual(row_2_id, activeTextElementId(&host, "row-2-2") orelse unreachable);
    try std.testing.expectEqual(row_3_id, activeTextElementId(&host, "row-3-3") orelse unreachable);
    try std.testing.expectEqualSlices(u64, &.{ row_3_id, row_1_id, row_2_id }, host.dom_elements.items[@intCast(section_id)].children.items);

    const changed_items = [_]HostValue{ testHostValueI64(2), testHostValueI64(4) };
    const changed_children = [_]abi.Elem{
        testNodeEachWithItems(&roc_host, &changed_items),
    };
    const changed_root = testElementWith(&roc_host, "section", &.{}, &changed_children);
    defer abi.decrefElem(changed_root, &roc_host);

    var changed_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &changed_stream, changed_root, &.{});
    const changed_counts = applyStructuralNodeDescriptorStream(&host, &roc_host, &changed_stream);
    host.active_stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);
    host.active_stream = changed_stream;

    try std.testing.expectEqual(@as(u64, 4), test_row_elem_call_count);
    const row_4_id = activeTextElementId(&host, "row-4-4") orelse unreachable;
    try std.testing.expectEqual(@as(u64, 0), changed_counts.reset_dom);
    try std.testing.expectEqual(@as(u64, 1), changed_counts.create_element);
    try std.testing.expectEqual(row_2_id, activeTextElementId(&host, "row-2-2") orelse unreachable);
    try std.testing.expect(activeTextElementId(&host, "row-1-1") == null);
    try std.testing.expect(activeTextElementId(&host, "row-3-3") == null);
    try std.testing.expectEqualSlices(u64, &.{ row_2_id, row_4_id }, host.dom_elements.items[@intCast(section_id)].children.items);
}

test "signals host dirty each append patches only changed row" {
    test_erased_callable_drop_count = 0;
    test_row_elem_call_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const row_count = 24;
    const state_token = newTestBinderToken(&roc_host);
    const each = testNodeEachWithSignalAndRow(&roc_host, testNodeRefExpr(state_token), &testStatefulRowElemCallable);
    const children = [_]abi.Elem{each};
    const section = testElementWith(&roc_host, "section", &.{}, &children);

    var initial_items: [row_count]HostValue = undefined;
    for (&initial_items, 0..) |*item, index| {
        item.* = testHostValueI64(@intCast(index + 1));
    }
    const root = testNodeStateWithTokenAndInitial(&roc_host, state_token, testHostValueI64List(&roc_host, &initial_items), section);
    defer abi.decrefElem(root, &roc_host);

    var initial_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &initial_stream, root, &.{});
    _ = applyNodeDescriptorStream(&host, &roc_host, &initial_stream);
    host.active_stream = initial_stream;

    try std.testing.expectEqual(@as(u64, row_count), test_row_elem_call_count);
    try std.testing.expect(activeTextElementId(&host, "row-24-24") != null);

    const state_id = host.active_stream.scope_sites.items[0].node_id;
    const state_index = host.stateIndexByNodeId(state_id) orelse unreachable;

    var next_items: [row_count + 1]HostValue = undefined;
    for (&next_items, 0..) |*item, index| {
        item.* = testHostValueI64(@intCast(index + 1));
    }
    testDropHostValue(&roc_host, host.states.items[state_index].cell.value);
    host.states.items[state_index].cell.value = testHostValueI64List(&roc_host, &next_items);
    host.states.items[state_index].version += 1;

    const dirty_source_node_ids = [_]u64{state_id};
    const dirty_generation = host.nextDirtySignalGeneration();
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
    defer host.gpa.allocator().free(changed_record_ids);
    const dirty_structural_signals = collectDirtyStructuralSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer host.gpa.allocator().free(dirty_structural_signals);

    try std.testing.expectEqual(@as(usize, 1), dirty_structural_signals.len);
    try std.testing.expectEqual(HostActiveStructuralSignalKind.each, dirty_structural_signals[0].kind);

    const rows_reused_start = host.pending_roc_metrics.rows_reused;
    const rows_created_start = host.pending_roc_metrics.rows_created;
    const rows_removed_start = host.pending_roc_metrics.rows_removed;
    const row_call_start = test_row_elem_call_count;
    const patch_start = host.render_metrics.patches_emitted;
    const graph_rebuild_start = host.pending_roc_metrics.active_graph_records_rebuilt;

    const patch_counts = applyDirtyStructuralSignalsLocally(&host, &roc_host, &dirty_source_node_ids, dirty_structural_signals);

    try std.testing.expectEqual(@as(u64, row_count), host.pending_roc_metrics.rows_reused - rows_reused_start);
    try std.testing.expectEqual(@as(u64, 1), host.pending_roc_metrics.rows_created - rows_created_start);
    try std.testing.expectEqual(@as(u64, 0), host.pending_roc_metrics.rows_removed - rows_removed_start);
    try std.testing.expectEqual(@as(u64, 0), host.pending_roc_metrics.active_graph_records_rebuilt - graph_rebuild_start);
    try std.testing.expectEqual(row_call_start + 1, test_row_elem_call_count);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.create_element);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.append_child);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.set_text);
    try std.testing.expectEqual(@as(u64, 3), patch_counts.total);
    try std.testing.expectEqual(patch_start + 3, host.render_metrics.patches_emitted);
    try std.testing.expect(activeTextElementId(&host, "row-25-25") != null);
}

test "signals host dirty each reorder moves rows without recollecting bodies" {
    test_erased_callable_drop_count = 0;
    test_row_elem_call_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const each = testNodeEachWithSignalAndRow(&roc_host, testNodeRefExpr(state_token), &testStatefulRowElemCallable);
    const children = [_]abi.Elem{each};
    const section = testElementWith(&roc_host, "section", &.{}, &children);

    const initial_items = [_]HostValue{ testHostValueI64(1), testHostValueI64(2), testHostValueI64(3) };
    const root = testNodeStateWithTokenAndInitial(&roc_host, state_token, testHostValueI64List(&roc_host, &initial_items), section);
    defer abi.decrefElem(root, &roc_host);

    var initial_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &initial_stream, root, &.{});
    _ = applyNodeDescriptorStream(&host, &roc_host, &initial_stream);
    host.active_stream = initial_stream;

    try std.testing.expectEqual(@as(u64, 3), test_row_elem_call_count);
    const section_id = host.active_stream.elements.items[0].elem_id;
    const row_1_id = activeTextElementId(&host, "row-1-1") orelse unreachable;
    const row_2_id = activeTextElementId(&host, "row-2-2") orelse unreachable;
    const row_3_id = activeTextElementId(&host, "row-3-3") orelse unreachable;
    try std.testing.expectEqualSlices(u64, &.{ row_1_id, row_2_id, row_3_id }, host.dom_elements.items[@intCast(section_id)].children.items);

    const state_id = host.active_stream.scope_sites.items[0].node_id;
    const state_index = host.stateIndexByNodeId(state_id) orelse unreachable;

    const reordered_items = [_]HostValue{ testHostValueI64(3), testHostValueI64(1), testHostValueI64(2) };
    testDropHostValue(&roc_host, host.states.items[state_index].cell.value);
    host.states.items[state_index].cell.value = testHostValueI64List(&roc_host, &reordered_items);
    host.states.items[state_index].version += 1;

    const dirty_source_node_ids = [_]u64{state_id};
    const dirty_generation = host.nextDirtySignalGeneration();
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
    defer host.gpa.allocator().free(changed_record_ids);
    const dirty_structural_signals = collectDirtyStructuralSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer host.gpa.allocator().free(dirty_structural_signals);

    try std.testing.expectEqual(@as(usize, 1), dirty_structural_signals.len);
    try std.testing.expectEqual(HostActiveStructuralSignalKind.each, dirty_structural_signals[0].kind);

    const rows_reused_start = host.pending_roc_metrics.rows_reused;
    const rows_created_start = host.pending_roc_metrics.rows_created;
    const rows_removed_start = host.pending_roc_metrics.rows_removed;
    const row_call_start = test_row_elem_call_count;
    const patch_start = host.render_metrics.patches_emitted;
    const graph_rebuild_start = host.pending_roc_metrics.active_graph_records_rebuilt;

    const patch_counts = applyDirtyStructuralSignalsLocally(&host, &roc_host, &dirty_source_node_ids, dirty_structural_signals);

    try std.testing.expectEqual(@as(u64, 3), host.pending_roc_metrics.rows_reused - rows_reused_start);
    try std.testing.expectEqual(@as(u64, 0), host.pending_roc_metrics.rows_created - rows_created_start);
    try std.testing.expectEqual(@as(u64, 0), host.pending_roc_metrics.rows_removed - rows_removed_start);
    try std.testing.expectEqual(@as(u64, 0), host.pending_roc_metrics.active_graph_records_rebuilt - graph_rebuild_start);
    try std.testing.expectEqual(row_call_start, test_row_elem_call_count);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.create_element);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.append_child);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.set_text);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.total);
    try std.testing.expectEqual(patch_start + 1, host.render_metrics.patches_emitted);
    try std.testing.expectEqualSlices(u64, &.{ row_3_id, row_1_id, row_2_id }, host.dom_elements.items[@intCast(section_id)].children.items);
}

test "signals host updates nested when without rebuilding unchanged row" {
    test_erased_callable_drop_count = 0;
    test_row_elem_call_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const items = [_]HostValue{testHostValueI64(1)};
    const children = [_]abi.Elem{
        testNodeEachWithNestedWhenRows(&roc_host, &items, state_token),
    };
    const section = testElementWith(&roc_host, "section", &.{}, &children);
    const root = testNodeStateWithTokenAndInitial(&roc_host, state_token, testHostValueBool(true), section);
    defer abi.decrefElem(root, &roc_host);

    var initial_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &initial_stream, root, &.{});
    _ = applyNodeDescriptorStream(&host, &roc_host, &initial_stream);
    host.active_stream = initial_stream;

    try std.testing.expectEqual(@as(u64, 1), test_row_elem_call_count);
    try std.testing.expect(activeTextElementId(&host, "row-1-1-true") != null);
    try std.testing.expect(activeTextElementId(&host, "row-1-1-false") == null);

    const state_id = host.active_stream.scope_sites.items[0].node_id;
    const state_index = host.stateIndexByNodeId(state_id) orelse unreachable;
    testDropHostValue(&roc_host, host.states.items[state_index].cell.value);
    host.states.items[state_index].cell.value = testHostValueBool(false);
    host.states.items[state_index].version += 1;

    const dirty_source_node_ids = [_]u64{state_id};
    const dirty_generation = host.nextDirtySignalGeneration();
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
    defer host.gpa.allocator().free(changed_record_ids);
    const dirty_structural_signals = collectDirtyStructuralSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer host.gpa.allocator().free(dirty_structural_signals);
    try std.testing.expectEqual(@as(usize, 1), dirty_structural_signals.len);
    try std.testing.expectEqual(HostActiveStructuralSignalKind.when, dirty_structural_signals[0].kind);

    const graph_rebuild_start = host.pending_roc_metrics.active_graph_records_rebuilt;
    _ = applyDirtyWhenStructuralSignals(&host, &roc_host, &dirty_source_node_ids, dirty_structural_signals);

    try std.testing.expectEqual(@as(u64, 0), host.pending_roc_metrics.active_graph_records_rebuilt - graph_rebuild_start);
    try std.testing.expectEqual(@as(u64, 1), test_row_elem_call_count);
    try std.testing.expect(activeTextElementId(&host, "row-1-1-true") == null);
    try std.testing.expect(activeTextElementId(&host, "row-1-1-false") != null);
}

test "signals host structural patch clears fields absent from reused DOM node" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const initial_attrs = [_]abi.NodeAttr{
        testNodeStaticTextAttr(&roc_host, .label, "Initial label"),
        testNodeStaticBoolAttr(.disabled, true),
    };
    const initial_root = testElementWith(&roc_host, "section", &initial_attrs, &.{});
    defer abi.decrefElem(initial_root, &roc_host);

    var initial_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &initial_stream, initial_root, &.{});
    _ = applyNodeDescriptorStream(&host, &roc_host, &initial_stream);
    host.active_stream = initial_stream;

    const section_id = host.active_stream.elements.items[0].elem_id;
    try std.testing.expectEqualStrings("Initial label", host.dom_elements.items[@intCast(section_id)].label.?);
    try std.testing.expect(host.dom_elements.items[@intCast(section_id)].disabled);

    const next_root = testElementWith(&roc_host, "section", &.{}, &.{});
    defer abi.decrefElem(next_root, &roc_host);

    var next_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &next_stream, next_root, &.{});
    const patch_counts = applyStructuralNodeDescriptorStream(&host, &roc_host, &next_stream);
    host.active_stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);
    host.active_stream = next_stream;

    try std.testing.expectEqual(@as(u64, 0), patch_counts.reset_dom);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.create_element);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.set_metadata);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.set_disabled);
    try std.testing.expect(host.dom_elements.items[@intCast(section_id)].label == null);
    try std.testing.expect(!host.dom_elements.items[@intCast(section_id)].disabled);
}

test "signals host structural patch binds only changed event slots" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const initial_button_attrs = [_]abi.NodeAttr{
        testNodeStaticTextAttr(&roc_host, .text, "Submit"),
        testNodeEventAttr(&roc_host, .click, state_token, .unit),
    };
    const initial_children = [_]abi.Elem{
        testElementWith(&roc_host, "button", &initial_button_attrs, &.{}),
    };
    const initial_section = testElementWith(&roc_host, "section", &.{}, &initial_children);
    const initial_root = testNodeStateWithToken(&roc_host, state_token, initial_section);
    defer abi.decrefElem(initial_root, &roc_host);

    var initial_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &initial_stream, initial_root, &.{});
    const initial_counts = applyNodeDescriptorStream(&host, &roc_host, &initial_stream);
    host.active_stream = initial_stream;

    try std.testing.expectEqual(@as(u64, 1), initial_counts.bind_event);
    const button_id = host.active_stream.elements.items[1].elem_id;
    try std.testing.expectEqual(@as(?u64, 1), host.dom_elements.items[@intCast(button_id)].bound_click_event);

    const same_button_attrs = [_]abi.NodeAttr{
        testNodeStaticTextAttr(&roc_host, .text, "Submit"),
        testNodeEventAttr(&roc_host, .click, state_token, .unit),
    };
    const same_children = [_]abi.Elem{
        testElementWith(&roc_host, "button", &same_button_attrs, &.{}),
    };
    const same_section = testElementWith(&roc_host, "section", &.{}, &same_children);
    const same_root = testNodeStateWithToken(&roc_host, cloneTestBinderToken(state_token), same_section);
    defer abi.decrefElem(same_root, &roc_host);

    var same_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &same_stream, same_root, &.{});
    const same_counts = applyStructuralNodeDescriptorStream(&host, &roc_host, &same_stream);
    host.active_stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);
    host.active_stream = same_stream;

    try std.testing.expectEqual(@as(u64, 0), same_counts.bind_event);
    try std.testing.expectEqual(@as(?u64, 1), host.dom_elements.items[@intCast(button_id)].bound_click_event);

    const removed_button_attrs = [_]abi.NodeAttr{
        testNodeStaticTextAttr(&roc_host, .text, "Submit"),
    };
    const removed_children = [_]abi.Elem{
        testElementWith(&roc_host, "button", &removed_button_attrs, &.{}),
    };
    const removed_section = testElementWith(&roc_host, "section", &.{}, &removed_children);
    const removed_root = testNodeStateWithToken(&roc_host, cloneTestBinderToken(state_token), removed_section);
    defer abi.decrefElem(removed_root, &roc_host);

    var removed_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &removed_stream, removed_root, &.{});
    const removed_counts = applyStructuralNodeDescriptorStream(&host, &roc_host, &removed_stream);
    host.active_stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);
    host.active_stream = removed_stream;

    try std.testing.expectEqual(@as(u64, 1), removed_counts.bind_event);
    try std.testing.expect(host.dom_elements.items[@intCast(button_id)].bound_click_event == null);
}

test "signals host structural patch shifts moved row event ids only" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const initial_items = [_]HostValue{ testHostValueI64(1), testHostValueI64(2) };
    const initial_children = [_]abi.Elem{
        testNodeEachWithItemsAndRow(&roc_host, &initial_items, &testStatefulRowButtonElemCallable),
    };
    const initial_root = testElementWith(&roc_host, "section", &.{}, &initial_children);
    defer abi.decrefElem(initial_root, &roc_host);

    var initial_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &initial_stream, initial_root, &.{});
    const initial_counts = applyNodeDescriptorStream(&host, &roc_host, &initial_stream);
    host.active_stream = initial_stream;

    try std.testing.expectEqual(@as(u64, 2), initial_counts.bind_event);
    const row_1_button_id = activeTextElementId(&host, "row-action-1-1") orelse unreachable;
    const row_2_button_id = activeTextElementId(&host, "row-action-2-2") orelse unreachable;
    try std.testing.expectEqual(@as(?u64, 1), host.dom_elements.items[@intCast(row_1_button_id)].bound_click_event);
    try std.testing.expectEqual(@as(?u64, 2), host.dom_elements.items[@intCast(row_2_button_id)].bound_click_event);

    const reordered_items = [_]HostValue{ testHostValueI64(2), testHostValueI64(1) };
    const reordered_children = [_]abi.Elem{
        testNodeEachWithItemsAndRow(&roc_host, &reordered_items, &testStatefulRowButtonElemCallable),
    };
    const reordered_root = testElementWith(&roc_host, "section", &.{}, &reordered_children);
    defer abi.decrefElem(reordered_root, &roc_host);

    var reordered_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &reordered_stream, reordered_root, &.{});
    const reordered_counts = applyStructuralNodeDescriptorStream(&host, &roc_host, &reordered_stream);
    host.active_stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);
    host.active_stream = reordered_stream;

    try std.testing.expectEqual(@as(u64, 0), reordered_counts.create_element);
    try std.testing.expectEqual(@as(u64, 2), reordered_counts.bind_event);
    try std.testing.expectEqual(row_1_button_id, activeTextElementId(&host, "row-action-1-1") orelse unreachable);
    try std.testing.expectEqual(row_2_button_id, activeTextElementId(&host, "row-action-2-2") orelse unreachable);
    try std.testing.expectEqual(@as(?u64, 2), host.dom_elements.items[@intCast(row_1_button_id)].bound_click_event);
    try std.testing.expectEqual(@as(?u64, 1), host.dom_elements.items[@intCast(row_2_button_id)].bound_click_event);

    const same_reordered_items = [_]HostValue{ testHostValueI64(2), testHostValueI64(1) };
    const same_reordered_children = [_]abi.Elem{
        testNodeEachWithItemsAndRow(&roc_host, &same_reordered_items, &testStatefulRowButtonElemCallable),
    };
    const same_reordered_root = testElementWith(&roc_host, "section", &.{}, &same_reordered_children);
    defer abi.decrefElem(same_reordered_root, &roc_host);

    var same_reordered_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &same_reordered_stream, same_reordered_root, &.{});
    const same_reordered_counts = applyStructuralNodeDescriptorStream(&host, &roc_host, &same_reordered_stream);
    host.active_stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);
    host.active_stream = same_reordered_stream;

    try std.testing.expectEqual(@as(u64, 0), same_reordered_counts.create_element);
    try std.testing.expectEqual(@as(u64, 0), same_reordered_counts.bind_event);
    try std.testing.expectEqual(@as(?u64, 2), host.dom_elements.items[@intCast(row_1_button_id)].bound_click_event);
    try std.testing.expectEqual(@as(?u64, 1), host.dom_elements.items[@intCast(row_2_button_id)].bound_click_event);
}

fn freeKeyedRowDiff(host: *HostEnv, diff: HostKeyedRowDiffResult) void {
    diff.deinit(host.gpa.allocator());
}

fn syncTestEachRowScopes(host: *HostEnv, roc_host: *abi.RocHost, parent_scope_id: u64, site_ordinal: u64, keys: []const HostValue, items: []const HostValue, key_eq: abi.RocErasedCallable, item_eq: abi.RocErasedCallable) HostKeyedRowDiffResult {
    const allocator = host.gpa.allocator();
    const key_values = allocator.alloc(HostValue, keys.len) catch std.process.exit(1);
    defer allocator.free(key_values);
    const item_values = allocator.alloc(HostValue, items.len) catch std.process.exit(1);
    defer allocator.free(item_values);

    for (keys, key_values) |key, *dest| {
        dest.* = host.cloneHostValue(key);
    }
    for (items, item_values) |item, *dest| {
        dest.* = host.cloneHostValue(item);
    }
    for (keys) |key| {
        testDropHostValue(roc_host, key);
    }
    if (keys.ptr != items.ptr) {
        for (items) |item| {
            testDropHostValue(roc_host, item);
        }
    }

    const key_drop = testHostValueDropCallable(roc_host);
    defer abi.decrefErasedCallable(key_drop, roc_host);
    const key_hash = testHostValueHashCallable(roc_host);
    defer abi.decrefErasedCallable(key_hash, roc_host);
    const item_drop = testHostValueDropCallable(roc_host);
    defer abi.decrefErasedCallable(item_drop, roc_host);
    return host.syncEachRowScopes(roc_host, parent_scope_id, site_ordinal, key_values, item_values, key_hash, key_eq, key_drop, item_eq, item_drop);
}

fn createTestEachRowScope(host: *HostEnv, roc_host: *abi.RocHost, parent_scope_id: u64, site_ordinal: u64, key: HostValue, item: HostValue, key_eq: abi.RocErasedCallable, item_eq: abi.RocErasedCallable) u64 {
    const key_drop = testHostValueDropCallable(roc_host);
    defer abi.decrefErasedCallable(key_drop, roc_host);
    const item_drop = testHostValueDropCallable(roc_host);
    defer abi.decrefErasedCallable(item_drop, roc_host);
    return host.createEachRowScope(parent_scope_id, site_ordinal, key, item, key_eq, key_drop, item_eq, item_drop);
}

fn boxTestElem(roc_host: *abi.RocHost, elem: abi.Elem) *abi.Elem {
    const raw = abi.allocateBox(@sizeOf(abi.Elem), @alignOf(abi.Elem), true, roc_host);
    const boxed: *abi.Elem = @ptrCast(@alignCast(raw));
    boxed.* = elem;
    return boxed;
}

fn boxTestNodeSignalExpr(roc_host: *abi.RocHost, expr: abi.NodeSignalExpr) *abi.NodeSignalExpr {
    const raw = abi.allocateBox(@sizeOf(abi.NodeSignalExpr), @alignOf(abi.NodeSignalExpr), true, roc_host);
    const boxed: *abi.NodeSignalExpr = @ptrCast(@alignCast(raw));
    boxed.* = expr;
    return boxed;
}

fn newTestBinderToken(roc_host: *abi.RocHost) HostBinderToken {
    const token: *u64 = @ptrCast(@alignCast(abi.allocateBox(@sizeOf(u64), @alignOf(u64), false, roc_host)));
    token.* = 0;
    return token;
}

fn cloneTestBinderToken(token: HostBinderToken) HostBinderToken {
    abi.increfBox(@ptrCast(token), 1);
    return token;
}

fn newTestSignalToken(roc_host: *abi.RocHost) HostSignalToken {
    const token: *u64 = @ptrCast(@alignCast(abi.allocateBox(@sizeOf(u64), @alignOf(u64), false, roc_host)));
    token.* = 0;
    return token;
}

fn newTestHostValueTypeTag(roc_host: *abi.RocHost) *u64 {
    const tag: *u64 = @ptrCast(@alignCast(abi.allocateBox(@sizeOf(u64), @alignOf(u64), false, roc_host)));
    tag.* = 0;
    return tag;
}

fn testNodeConstExpr(roc_host: *abi.RocHost, value: HostValue) abi.NodeSignalExpr {
    const eq = testHostValueEqCallable(roc_host);
    const drop = testHostValueDropCallable(roc_host);
    return .{
        .payload = .{ .const_value = .{
            ._0 = newTestSignalToken(roc_host),
            ._1 = testHostValueInitialThunk(roc_host, value),
            ._2 = eq,
            ._3 = drop,
        } },
        .tag = .ConstValue,
    };
}

fn testNodeRefExpr(binder_token: HostBinderToken) abi.NodeSignalExpr {
    return .{
        .payload = .{ .ref = cloneTestBinderToken(binder_token) },
        .tag = .Ref,
    };
}

fn testNodeMapExpr(roc_host: *abi.RocHost, input: abi.NodeSignalExpr) abi.NodeSignalExpr {
    const transform = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testUnaryHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 1 },
    );
    const eq = testHostValueEqCallable(roc_host);
    const drop = testHostValueDropCallable(roc_host);
    return .{
        .payload = .{
            .map = .{
                ._0 = newTestSignalToken(roc_host),
                ._1 = boxTestNodeSignalExpr(roc_host, input),
                ._2 = transform,
                ._3 = eq,
                ._4 = drop,
            },
        },
        .tag = .Map,
    };
}

fn testNodeStableStrMapExpr(roc_host: *abi.RocHost, input: abi.NodeSignalExpr) abi.NodeSignalExpr {
    const transform = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testStableStrHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const eq = testHostValueEqCallable(roc_host);
    const drop = testHostValueDropCallable(roc_host);
    return .{
        .payload = .{
            .map = .{
                ._0 = newTestSignalToken(roc_host),
                ._1 = boxTestNodeSignalExpr(roc_host, input),
                ._2 = transform,
                ._3 = eq,
                ._4 = drop,
            },
        },
        .tag = .Map,
    };
}

fn testNodeStableI64MapExpr(roc_host: *abi.RocHost, input: abi.NodeSignalExpr, value: i64) abi.NodeSignalExpr {
    const transform = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testStableI64HostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = value },
    );
    const eq = testHostValueEqCallable(roc_host);
    const drop = testHostValueDropCallable(roc_host);
    return .{
        .payload = .{
            .map = .{
                ._0 = newTestSignalToken(roc_host),
                ._1 = boxTestNodeSignalExpr(roc_host, input),
                ._2 = transform,
                ._3 = eq,
                ._4 = drop,
            },
        },
        .tag = .Map,
    };
}

fn testNodeStableBoolMapExpr(roc_host: *abi.RocHost, input: abi.NodeSignalExpr) abi.NodeSignalExpr {
    const transform = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testStableBoolHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const eq = testHostValueEqCallable(roc_host);
    const drop = testHostValueDropCallable(roc_host);
    return .{
        .payload = .{
            .map = .{
                ._0 = newTestSignalToken(roc_host),
                ._1 = boxTestNodeSignalExpr(roc_host, input),
                ._2 = transform,
                ._3 = eq,
                ._4 = drop,
            },
        },
        .tag = .Map,
    };
}

fn testNodeCombineExpr(roc_host: *abi.RocHost, children: []const abi.NodeSignalExpr) abi.NodeSignalExpr {
    const transform = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testUnaryIdentityHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const eq = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testAlwaysEqualHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const drop = testHostValueDropCallable(roc_host);
    return .{
        .payload = .{
            .combine = .{
                ._0 = newTestSignalToken(roc_host),
                ._1 = abi.RocList(abi.NodeSignalExpr).fromSlice(children, roc_host),
                ._2 = transform,
                ._3 = eq,
                ._4 = drop,
            },
        },
        .tag = .Combine,
    };
}

fn testNodeText(roc_host: *abi.RocHost, text: []const u8) abi.Elem {
    return .{
        .payload = .{ .text = RocStr.fromSlice(text, roc_host) },
        .tag = .Text,
    };
}

fn testNodeTextSignal(roc_host: *abi.RocHost, signal: abi.NodeSignalExpr) abi.Elem {
    return .{
        .payload = .{ .text_signal = .{
            .read = testReadStrCallable(roc_host),
            .signal = boxTestNodeSignalExpr(roc_host, signal),
        } },
        .tag = .TextSignal,
    };
}

fn testNodeStaticTextAttr(roc_host: *abi.RocHost, field: RenderTextField, value: []const u8) abi.NodeAttr {
    return .{
        .payload = .{
            .static_text = .{
                .field = @intFromEnum(field),
                .value = RocStr.fromSlice(value, roc_host),
            },
        },
        .tag = .StaticText,
    };
}

fn testNodeSignalTextAttr(roc_host: *abi.RocHost, field: RenderTextField, signal: abi.NodeSignalExpr) abi.NodeAttr {
    return .{
        .payload = .{
            .signal_text = .{
                .field = @intFromEnum(field),
                .read = testReadStrCallable(roc_host),
                .signal = boxTestNodeSignalExpr(roc_host, signal),
            },
        },
        .tag = .SignalText,
    };
}

fn testNodeStaticBoolAttr(field: RenderBoolField, value: bool) abi.NodeAttr {
    return .{
        .payload = .{
            .static_bool = .{
                .field = @intFromEnum(field),
                .value = value,
            },
        },
        .tag = .StaticBool,
    };
}

fn testNodeSignalBoolAttr(roc_host: *abi.RocHost, field: RenderBoolField, signal: abi.NodeSignalExpr) abi.NodeAttr {
    return .{
        .payload = .{
            .signal_bool = .{
                .field = @intFromEnum(field),
                .read = testReadBoolCallable(roc_host),
                .signal = boxTestNodeSignalExpr(roc_host, signal),
            },
        },
        .tag = .SignalBool,
    };
}

fn testNodeEventAttr(roc_host: *abi.RocHost, kind: RenderEventKind, binder_token: HostBinderToken, payload_kind: EventPayloadKind) abi.NodeAttr {
    const transform = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testBinaryHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const payload_drop = testHostValueDropCallable(roc_host);
    return .{
        .payload = .{
            .on_event = .{
                .kind = @intFromEnum(kind),
                .msg = .{
                    .binder = cloneTestBinderToken(binder_token),
                    .payload_bool_tag = newTestHostValueTypeTag(roc_host),
                    .payload_drop = payload_drop,
                    .payload_kind = @intFromEnum(payload_kind),
                    .payload_str_tag = newTestHostValueTypeTag(roc_host),
                    .payload_unit_tag = newTestHostValueTypeTag(roc_host),
                    .transform = transform,
                },
            },
        },
        .tag = .OnEvent,
    };
}

fn testNodeUnitIncrementEventAttr(roc_host: *abi.RocHost, kind: RenderEventKind, binder_token: HostBinderToken) abi.NodeAttr {
    const transform = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testUnitIncrementHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const payload_drop = testHostValueDropCallable(roc_host);
    return .{
        .payload = .{
            .on_event = .{
                .kind = @intFromEnum(kind),
                .msg = .{
                    .binder = cloneTestBinderToken(binder_token),
                    .payload_bool_tag = newTestHostValueTypeTag(roc_host),
                    .payload_drop = payload_drop,
                    .payload_kind = @intFromEnum(EventPayloadKind.unit),
                    .payload_str_tag = newTestHostValueTypeTag(roc_host),
                    .payload_unit_tag = newTestHostValueTypeTag(roc_host),
                    .transform = transform,
                },
            },
        },
        .tag = .OnEvent,
    };
}

fn testElementWith(roc_host: *abi.RocHost, tag: []const u8, attrs: []const abi.NodeAttr, children: []const abi.Elem) abi.Elem {
    return .{
        .payload = .{
            .element = .{
                .attrs = abi.RocList(abi.NodeAttr).fromSlice(attrs, roc_host),
                .children = abi.RocList(abi.Elem).fromSlice(children, roc_host),
                .tag = RocStr.fromSlice(tag, roc_host),
            },
        },
        .tag = .Element,
    };
}

fn testElement(roc_host: *abi.RocHost, children: []const abi.Elem) abi.Elem {
    return testElementWith(roc_host, "div", &.{}, children);
}

fn testHostValueInitialThunk(roc_host: *abi.RocHost, initial: HostValue) abi.RocErasedCallable {
    return writeTestErasedCallable(
        TestErasedHostValueCapture,
        roc_host,
        &testInitialHostValueCallable,
        &testHostValueCaptureOnDrop,
        .{ .value = initial },
    );
}

fn testNodeStateWithTokenAndInitial(roc_host: *abi.RocHost, binder_token: HostBinderToken, initial: HostValue, child: abi.Elem) abi.Elem {
    const initial_thunk = testHostValueInitialThunk(roc_host, initial);
    const eq = testHostValueEqCallable(roc_host);
    const drop = testHostValueDropCallable(roc_host);
    return .{
        .payload = .{
            .state = .{
                .binder = binder_token,
                .child = boxTestElem(roc_host, child),
                .drop = drop,
                .eq = eq,
                .initial = initial_thunk,
            },
        },
        .tag = .State,
    };
}

fn testNodeStateWithToken(roc_host: *abi.RocHost, binder_token: HostBinderToken, child: abi.Elem) abi.Elem {
    return testNodeStateWithTokenAndInitial(roc_host, binder_token, testHostValueI64(0), child);
}

fn testNodeState(roc_host: *abi.RocHost, child: abi.Elem) abi.Elem {
    return testNodeStateWithToken(roc_host, newTestBinderToken(roc_host), child);
}

fn testNodeWhen(roc_host: *abi.RocHost, when_true: abi.Elem, when_false: abi.Elem) abi.Elem {
    return .{
        .payload = .{
            .when = .{
                .condition = boxTestNodeSignalExpr(roc_host, testNodeConstExpr(roc_host, testHostValueBool(true))),
                .read = testReadBoolCallable(roc_host),
                .when_false = boxTestElem(roc_host, when_false),
                .when_true = boxTestElem(roc_host, when_true),
            },
        },
        .tag = .When,
    };
}

fn testHostValueI64List(roc_host: *abi.RocHost, items: []const HostValue) HostValue {
    const host = hostFromRocHost(roc_host);
    const values = I64List.allocate(items.len, roc_host);
    if (items.len > 0) {
        const dest = values.elements_ptr orelse unreachable;
        for (items, 0..) |item, index| {
            dest[index] = testReadHostValueI64(roc_host, item);
            testDropHostValue(roc_host, item);
        }
    }
    const payload: *I64List = @ptrCast(@alignCast(abi.allocateBox(@sizeOf(I64List), @alignOf(I64List), true, roc_host)));
    payload.* = values;
    const host_value = host.storeHostValue(@ptrCast(payload));
    host.setTestHostValueKind(host_value, .i64_list);
    return host_value;
}

fn testNodeEachWithSignalAndRow(roc_host: *abi.RocHost, signal: abi.NodeSignalExpr, row_fn: abi.RocErasedCallableFn) abi.Elem {
    const key_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testHostValueEqErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const key_of = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testUnaryHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const key_hash = testHostValueHashCallable(roc_host);
    const item_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testHostValueEqErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const key_drop = testHostValueDropCallable(roc_host);
    const item_drop = testHostValueDropCallable(roc_host);
    const items_to_values = testItemsToValuesCallable(roc_host);
    const row = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        row_fn,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    return .{
        .payload = .{
            .each = .{
                .items = boxTestNodeSignalExpr(roc_host, signal),
                .items_to_values = items_to_values,
                .key_hash = key_hash,
                .key_drop = key_drop,
                .key_eq = key_eq,
                .key_of = key_of,
                .item_drop = item_drop,
                .item_eq = item_eq,
                .row = row,
            },
        },
        .tag = .Each,
    };
}

fn testNodeEachWithItemsAndRow(roc_host: *abi.RocHost, items: []const HostValue, row_fn: abi.RocErasedCallableFn) abi.Elem {
    return testNodeEachWithSignalAndRow(roc_host, testNodeConstExpr(roc_host, testHostValueI64List(roc_host, items)), row_fn);
}

fn testNodeEachWithNestedWhenRows(roc_host: *abi.RocHost, items: []const HostValue, condition_binder: HostBinderToken) abi.Elem {
    const key_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testHostValueEqErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const key_of = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testUnaryHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const key_hash = testHostValueHashCallable(roc_host);
    const item_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testHostValueEqErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const key_drop = testHostValueDropCallable(roc_host);
    const item_drop = testHostValueDropCallable(roc_host);
    const items_to_values = testItemsToValuesCallable(roc_host);
    const row = writeTestErasedCallable(
        TestErasedBinderCapture,
        roc_host,
        &testNestedWhenRowElemCallable,
        &testBinderCaptureOnDrop,
        .{ .condition_binder = cloneTestBinderToken(condition_binder) },
    );
    return .{
        .payload = .{
            .each = .{
                .items = boxTestNodeSignalExpr(roc_host, testNodeConstExpr(roc_host, testHostValueI64List(roc_host, items))),
                .items_to_values = items_to_values,
                .key_hash = key_hash,
                .key_drop = key_drop,
                .key_eq = key_eq,
                .key_of = key_of,
                .item_drop = item_drop,
                .item_eq = item_eq,
                .row = row,
            },
        },
        .tag = .Each,
    };
}

fn testNodeEachWithItems(roc_host: *abi.RocHost, items: []const HostValue) abi.Elem {
    return testNodeEachWithItemsAndRow(roc_host, items, &testStatefulRowElemCallable);
}

fn testNodeEach(roc_host: *abi.RocHost) abi.Elem {
    return testNodeEachWithItems(roc_host, &.{});
}

fn activeTextElementId(host: *const HostEnv, text: []const u8) ?u64 {
    for (host.dom_elements.items) |elem| {
        if (!elem.active) continue;
        const elem_text = elem.text orelse continue;
        if (std.mem.eql(u8, elem_text, text)) return elem.id;
    }
    return null;
}

test "signals host keyed row diff reuses creates and removes by typed key" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    const key_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        &roc_host,
        &testHostValueEqErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    defer abi.decrefErasedCallable(key_eq, &roc_host);

    const root = host.internRootScope();

    const initial_keys = [_]HostValue{ testHostValueI64(1), testHostValueI64(2), testHostValueI64(3) };
    const initial = syncTestEachRowScopes(&host, &roc_host, root, 5, &initial_keys, &initial_keys, key_eq, key_eq);
    defer freeKeyedRowDiff(&host, initial);
    try std.testing.expectEqual(@as(u64, 0), initial.rows_reused);
    try std.testing.expectEqual(@as(u64, 3), initial.rows_created);
    try std.testing.expectEqual(@as(u64, 0), initial.rows_removed);
    try std.testing.expectEqual(@as(u64, 0), initial.row_items_unchanged);
    try std.testing.expectEqual(@as(u64, 0), initial.row_items_updated);

    const state_for_key_2 = host.internNodeIdentity(initial.scope_ids[1], 0);

    const reordered_keys = [_]HostValue{ testHostValueI64(3), testHostValueI64(1), testHostValueI64(2) };
    const reordered = syncTestEachRowScopes(&host, &roc_host, root, 5, &reordered_keys, &reordered_keys, key_eq, key_eq);
    defer freeKeyedRowDiff(&host, reordered);
    try std.testing.expectEqual(@as(u64, 3), reordered.rows_reused);
    try std.testing.expectEqual(@as(u64, 0), reordered.rows_created);
    try std.testing.expectEqual(@as(u64, 0), reordered.rows_removed);
    try std.testing.expectEqual(@as(u64, 3), reordered.row_items_unchanged);
    try std.testing.expectEqual(@as(u64, 0), reordered.row_items_updated);
    try std.testing.expectEqual(initial.scope_ids[2], reordered.scope_ids[0]);
    try std.testing.expectEqual(initial.scope_ids[0], reordered.scope_ids[1]);
    try std.testing.expectEqual(initial.scope_ids[1], reordered.scope_ids[2]);
    try std.testing.expectEqual(state_for_key_2, host.internNodeIdentity(reordered.scope_ids[2], 0));

    const changed_keys = [_]HostValue{ testHostValueI64(2), testHostValueI64(4) };
    const changed_items = [_]HostValue{ testHostValueI64(22), testHostValueI64(4) };
    const changed = syncTestEachRowScopes(&host, &roc_host, root, 5, &changed_keys, &changed_items, key_eq, key_eq);
    defer freeKeyedRowDiff(&host, changed);
    try std.testing.expectEqual(@as(u64, 1), changed.rows_reused);
    try std.testing.expectEqual(@as(u64, 1), changed.rows_created);
    try std.testing.expectEqual(@as(u64, 2), changed.rows_removed);
    try std.testing.expectEqual(@as(u64, 0), changed.row_items_unchanged);
    try std.testing.expectEqual(@as(u64, 1), changed.row_items_updated);
    try std.testing.expectEqual(initial.scope_ids[1], changed.scope_ids[0]);
    try std.testing.expect(changed.scope_ids[1] != initial.scope_ids[0]);
    try std.testing.expect(changed.scope_ids[1] != initial.scope_ids[2]);

    const reappeared_keys = [_]HostValue{ testHostValueI64(1), testHostValueI64(2), testHostValueI64(4) };
    const reappeared = syncTestEachRowScopes(&host, &roc_host, root, 5, &reappeared_keys, &reappeared_keys, key_eq, key_eq);
    defer freeKeyedRowDiff(&host, reappeared);
    try std.testing.expectEqual(@as(u64, 2), reappeared.rows_reused);
    try std.testing.expectEqual(@as(u64, 1), reappeared.rows_created);
    try std.testing.expectEqual(@as(u64, 0), reappeared.rows_removed);
    try std.testing.expectEqual(@as(u64, 1), reappeared.row_items_unchanged);
    try std.testing.expectEqual(@as(u64, 1), reappeared.row_items_updated);
    try std.testing.expect(reappeared.scope_ids[0] != initial.scope_ids[0]);
    try std.testing.expectEqual(initial.scope_ids[1], reappeared.scope_ids[1]);
    try std.testing.expectEqual(changed.scope_ids[1], reappeared.scope_ids[2]);

    try std.testing.expectEqual(@as(u64, 6), host.pending_roc_metrics.rows_reused);
    try std.testing.expectEqual(@as(u64, 5), host.pending_roc_metrics.rows_created);
    try std.testing.expectEqual(@as(u64, 2), host.pending_roc_metrics.rows_removed);
}

test "signals host keyed row diff hash probes scale linearly" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    const key_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        &roc_host,
        &testHostValueEqErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    defer abi.decrefErasedCallable(key_eq, &roc_host);

    const root = host.internRootScope();
    const row_count = 64;

    var initial_keys: [row_count]HostValue = undefined;
    for (&initial_keys, 0..) |*key, index| {
        key.* = testHostValueI64(@intCast(index + 1));
    }
    const initial = syncTestEachRowScopes(&host, &roc_host, root, 5, &initial_keys, &initial_keys, key_eq, key_eq);
    defer freeKeyedRowDiff(&host, initial);
    try std.testing.expectEqual(@as(u64, row_count), initial.rows_created);

    const compare_start = host.pending_roc_metrics.each_key_compares;

    var reordered_keys: [row_count]HostValue = undefined;
    for (&reordered_keys, 0..) |*key, index| {
        key.* = testHostValueI64(@intCast(row_count - index));
    }
    const reordered = syncTestEachRowScopes(&host, &roc_host, root, 5, &reordered_keys, &reordered_keys, key_eq, key_eq);
    defer freeKeyedRowDiff(&host, reordered);
    try std.testing.expectEqual(@as(u64, row_count), reordered.rows_reused);
    try std.testing.expectEqual(@as(u64, 0), reordered.rows_created);
    try std.testing.expectEqual(@as(u64, 0), reordered.rows_removed);

    const compare_delta = host.pending_roc_metrics.each_key_compares - compare_start;
    try std.testing.expectEqual(@as(u64, row_count * 3), compare_delta);
}

test "signals host row scopes retain key and item equality thunks" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    const key_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        &roc_host,
        &testHostValueEqErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const item_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        &roc_host,
        &testHostValueEqErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );

    const root = host.internRootScope();
    const initial_keys = [_]HostValue{testHostValueI64(1)};
    const initial_items = [_]HostValue{testHostValueI64(10)};
    const initial = syncTestEachRowScopes(&host, &roc_host, root, 5, &initial_keys, &initial_items, key_eq, item_eq);
    defer freeKeyedRowDiff(&host, initial);
    try std.testing.expectEqual(@as(u64, 1), initial.rows_created);
    const row_scope_id = initial.scope_ids[0];

    test_erased_callable_drop_count = 0;
    abi.decrefErasedCallable(key_eq, &roc_host);
    abi.decrefErasedCallable(item_eq, &roc_host);
    try std.testing.expectEqual(@as(u64, 0), test_erased_callable_drop_count);

    const incoming_key_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        &roc_host,
        &testNeverEqualHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    defer abi.decrefErasedCallable(incoming_key_eq, &roc_host);
    const incoming_item_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        &roc_host,
        &testNeverEqualHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    defer abi.decrefErasedCallable(incoming_item_eq, &roc_host);

    const next_keys = [_]HostValue{testHostValueI64(1)};
    const next_items = [_]HostValue{testHostValueI64(10)};
    const next = syncTestEachRowScopes(&host, &roc_host, root, 5, &next_keys, &next_items, incoming_key_eq, incoming_item_eq);
    defer freeKeyedRowDiff(&host, next);
    try std.testing.expectEqual(@as(u64, 1), next.rows_reused);
    try std.testing.expectEqual(@as(u64, 0), next.rows_created);
    try std.testing.expectEqual(@as(u64, 1), next.row_items_unchanged);
    try std.testing.expectEqual(row_scope_id, next.scope_ids[0]);
}

test "signals host walks Elem identity-bearing sites only" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    const nested_state = testNodeState(&roc_host, testNodeText(&roc_host, "nested"));
    const branch_true = testNodeState(&roc_host, testNodeText(&roc_host, "true"));
    const branch_false = testNodeState(&roc_host, testNodeText(&roc_host, "false"));
    const when_elem = testNodeWhen(&roc_host, branch_true, branch_false);
    const each_elem = testNodeEach(&roc_host);
    const nested_children = [_]abi.Elem{
        testNodeText(&roc_host, "ordinary text"),
        when_elem,
        each_elem,
    };
    const nested_element = testElement(&roc_host, &nested_children);
    const root_children = [_]abi.Elem{
        testNodeText(&roc_host, "root text"),
        nested_state,
        nested_element,
    };
    const root = testElement(&roc_host, &root_children);
    defer abi.decrefElem(root, &roc_host);

    host.walkElemRootIdentitySites(root);
    try std.testing.expectEqual(@as(usize, 3), host.node_identities.items.len);
    try std.testing.expectEqual(@as(u64, 0), host.node_identities.items[0].scope_id);
    try std.testing.expectEqual(@as(u64, 0), host.node_identities.items[0].ordinal);
    try std.testing.expectEqual(@as(u64, 0), host.node_identities.items[1].scope_id);
    try std.testing.expectEqual(@as(u64, 1), host.node_identities.items[1].ordinal);
    try std.testing.expectEqual(@as(u64, 0), host.node_identities.items[2].scope_id);
    try std.testing.expectEqual(@as(u64, 2), host.node_identities.items[2].ordinal);

    host.walkElemRootIdentitySites(root);
    try std.testing.expectEqual(@as(usize, 3), host.node_identities.items.len);

    const true_scope = host.walkElemWhenBranchIdentitySites(0, 1, .true_branch, when_elem.payload.when.when_true.*);
    try std.testing.expectEqual(@as(usize, 4), host.node_identities.items.len);
    try std.testing.expectEqual(true_scope, host.node_identities.items[3].scope_id);
    try std.testing.expectEqual(@as(u64, 0), host.node_identities.items[3].ordinal);

    const true_scope_again = host.walkElemWhenBranchIdentitySites(0, 1, .true_branch, when_elem.payload.when.when_true.*);
    try std.testing.expectEqual(true_scope, true_scope_again);
    try std.testing.expectEqual(@as(usize, 4), host.node_identities.items.len);

    const false_scope = host.walkElemWhenBranchIdentitySites(0, 1, .false_branch, when_elem.payload.when.when_false.*);
    try std.testing.expect(false_scope != true_scope);
    try std.testing.expectEqual(@as(usize, 5), host.node_identities.items.len);
    try std.testing.expectEqual(false_scope, host.node_identities.items[4].scope_id);
    try std.testing.expectEqual(@as(u64, 0), host.node_identities.items[4].ordinal);
}

test "signals host collects Elem descriptor stream" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    var stream: HostNodeDescriptorStream = .{};
    defer stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);

    const root_attrs = [_]abi.NodeAttr{
        testNodeStaticTextAttr(&roc_host, .role, "region"),
        testNodeStaticTextAttr(&roc_host, .label, "Dashboard"),
        testNodeSignalTextAttr(&roc_host, .value, testNodeConstExpr(&roc_host, testHostValueStr(&roc_host, "search"))),
        testNodeStaticBoolAttr(.disabled, true),
        testNodeSignalBoolAttr(&roc_host, .checked, testNodeConstExpr(&roc_host, testHostValueBool(false))),
    };
    const state_token = newTestBinderToken(&roc_host);
    const state_child_attrs = [_]abi.NodeAttr{
        testNodeStaticTextAttr(&roc_host, .test_id, "state-child"),
        testNodeSignalTextAttr(&roc_host, .value, testNodeRefExpr(state_token)),
        testNodeEventAttr(&roc_host, .click, state_token, .unit),
    };
    const state_child_children = [_]abi.Elem{
        testNodeText(&roc_host, "state child"),
        testNodeTextSignal(&roc_host, testNodeRefExpr(state_token)),
    };
    const state_child = testElementWith(&roc_host, "span", &state_child_attrs, &state_child_children);
    const state = testNodeStateWithToken(&roc_host, state_token, state_child);
    const when_elem = testNodeWhen(&roc_host, testNodeText(&roc_host, "true branch"), testNodeText(&roc_host, "false branch"));
    const each_elem = testNodeEach(&roc_host);
    const root_children = [_]abi.Elem{
        testNodeText(&roc_host, "intro"),
        testNodeTextSignal(&roc_host, testNodeConstExpr(&roc_host, testHostValueStr(&roc_host, "dynamic text"))),
        state,
        when_elem,
        each_elem,
    };
    const root = testElementWith(&roc_host, "section", &root_attrs, &root_children);
    defer abi.decrefElem(root, &roc_host);

    host.collectElemRootDescriptors(&roc_host, &stream, root);

    try std.testing.expectEqual(@as(usize, 2), stream.elements.items.len);
    try std.testing.expectEqual(@as(u64, 1), stream.elements.items[0].elem_id);
    try std.testing.expectEqual(@as(u64, 0), stream.elements.items[0].parent_elem_id);
    try std.testing.expectEqual(@as(u64, 0), stream.elements.items[0].scope_id);
    try std.testing.expectEqualStrings("section", stream.elements.items[0].tag);
    try std.testing.expectEqual(@as(u64, 4), stream.elements.items[1].elem_id);
    try std.testing.expectEqual(@as(u64, 1), stream.elements.items[1].parent_elem_id);
    try std.testing.expectEqualStrings("span", stream.elements.items[1].tag);

    try std.testing.expectEqual(@as(usize, 2), stream.text_nodes.items.len);
    try std.testing.expectEqual(@as(u64, 2), stream.text_nodes.items[0].elem_id);
    try std.testing.expectEqual(@as(u64, 1), stream.text_nodes.items[0].parent_elem_id);
    try std.testing.expectEqualStrings("intro", stream.text_nodes.items[0].value);
    try std.testing.expectEqual(@as(u64, 5), stream.text_nodes.items[1].elem_id);
    try std.testing.expectEqual(@as(u64, 4), stream.text_nodes.items[1].parent_elem_id);
    try std.testing.expectEqualStrings("state child", stream.text_nodes.items[1].value);

    try std.testing.expectEqual(@as(usize, 2), stream.signal_text_nodes.items.len);
    try std.testing.expectEqual(@as(u64, 3), stream.signal_text_nodes.items[0].elem_id);
    try std.testing.expectEqual(@as(u64, 1), stream.signal_text_nodes.items[0].parent_elem_id);
    try std.testing.expectEqual(@as(std.meta.Tag(HostSignalRecordPayload), .const_value), std.meta.activeTag(stream.signal_text_nodes.items[0].signal.record.payload));
    try std.testing.expectEqual(@as(usize, 0), stream.signal_text_nodes.items[0].signal.source_node_ids.len);
    try std.testing.expectEqual(@as(u64, 6), stream.signal_text_nodes.items[1].elem_id);
    try std.testing.expectEqual(@as(u64, 4), stream.signal_text_nodes.items[1].parent_elem_id);
    try std.testing.expectEqual(@as(std.meta.Tag(HostSignalRecordPayload), .ref), std.meta.activeTag(stream.signal_text_nodes.items[1].signal.record.payload));
    try std.testing.expectEqual(@as(usize, 1), stream.signal_text_nodes.items[1].signal.source_node_ids.len);
    try std.testing.expectEqual(stream.scope_sites.items[0].node_id, stream.signal_text_nodes.items[1].signal.source_node_ids[0]);
    try std.testing.expectEqual(@as(u64, 7), stream.next_elem_id);

    try std.testing.expectEqual(@as(usize, 3), stream.static_text_attrs.items.len);
    try std.testing.expectEqual(RenderTextField.role, stream.static_text_attrs.items[0].field);
    try std.testing.expectEqualStrings("region", stream.static_text_attrs.items[0].value);
    try std.testing.expectEqual(RenderTextField.label, stream.static_text_attrs.items[1].field);
    try std.testing.expectEqualStrings("Dashboard", stream.static_text_attrs.items[1].value);
    try std.testing.expectEqual(RenderTextField.test_id, stream.static_text_attrs.items[2].field);
    try std.testing.expectEqualStrings("state-child", stream.static_text_attrs.items[2].value);

    try std.testing.expectEqual(@as(usize, 2), stream.signal_text_attrs.items.len);
    try std.testing.expectEqual(@as(u64, 1), stream.signal_text_attrs.items[0].elem_id);
    try std.testing.expectEqual(RenderTextField.value, stream.signal_text_attrs.items[0].field);
    try std.testing.expectEqual(@as(std.meta.Tag(HostSignalRecordPayload), .const_value), std.meta.activeTag(stream.signal_text_attrs.items[0].signal.record.payload));
    try std.testing.expectEqual(@as(usize, 0), stream.signal_text_attrs.items[0].signal.source_node_ids.len);
    try std.testing.expectEqual(@as(u64, 4), stream.signal_text_attrs.items[1].elem_id);
    try std.testing.expectEqual(RenderTextField.value, stream.signal_text_attrs.items[1].field);
    try std.testing.expectEqual(@as(std.meta.Tag(HostSignalRecordPayload), .ref), std.meta.activeTag(stream.signal_text_attrs.items[1].signal.record.payload));
    try std.testing.expectEqual(@as(usize, 1), stream.signal_text_attrs.items[1].signal.source_node_ids.len);
    try std.testing.expectEqual(stream.scope_sites.items[0].node_id, stream.signal_text_attrs.items[1].signal.source_node_ids[0]);

    try std.testing.expectEqual(@as(usize, 1), stream.static_bool_attrs.items.len);
    try std.testing.expectEqual(RenderBoolField.disabled, stream.static_bool_attrs.items[0].field);
    try std.testing.expect(stream.static_bool_attrs.items[0].value);

    try std.testing.expectEqual(@as(usize, 1), stream.signal_bool_attrs.items.len);
    try std.testing.expectEqual(RenderBoolField.checked, stream.signal_bool_attrs.items[0].field);
    try std.testing.expectEqual(@as(std.meta.Tag(HostSignalRecordPayload), .const_value), std.meta.activeTag(stream.signal_bool_attrs.items[0].signal.record.payload));
    try std.testing.expectEqual(@as(usize, 0), stream.signal_bool_attrs.items[0].signal.source_node_ids.len);

    try std.testing.expectEqual(@as(usize, 1), stream.events.items.len);
    try std.testing.expectEqual(RenderEventKind.click, stream.events.items[0].kind);
    try std.testing.expect(stream.events.items[0].binder_token == state_token);
    try std.testing.expectEqual(stream.scope_sites.items[0].node_id, stream.events.items[0].target_node_id);
    try std.testing.expectEqual(EventPayloadKind.unit, stream.events.items[0].payload_kind);

    try std.testing.expectEqual(@as(usize, 3), stream.scope_sites.items.len);
    try std.testing.expectEqual(HostNodeScopeSiteKind.state, stream.scope_sites.items[0].kind);
    try std.testing.expectEqual(@as(u64, 0), stream.scope_sites.items[0].ordinal);
    try std.testing.expectEqual(@as(u64, 1), stream.scope_sites.items[0].parent_elem_id);
    try std.testing.expectEqual(@as(usize, 0), stream.scope_sites.items[0].binder_bindings.len);
    try std.testing.expectEqual(HostNodeScopeSiteKind.when, stream.scope_sites.items[1].kind);
    try std.testing.expectEqual(@as(u64, 1), stream.scope_sites.items[1].ordinal);
    try std.testing.expectEqual(@as(usize, 0), stream.scope_sites.items[1].binder_bindings.len);
    try std.testing.expectEqual(HostNodeScopeSiteKind.each, stream.scope_sites.items[2].kind);
    try std.testing.expectEqual(@as(u64, 2), stream.scope_sites.items[2].ordinal);
    try std.testing.expectEqual(@as(usize, 0), stream.scope_sites.items[2].binder_bindings.len);

    try std.testing.expectEqual(@as(usize, 1), stream.states.items.len);
    try std.testing.expectEqual(stream.scope_sites.items[0].node_id, stream.states.items[0].node_id);
    try std.testing.expectEqual(@as(usize, 1), stream.whens.items.len);
    try std.testing.expectEqual(stream.scope_sites.items[1].node_id, stream.whens.items[0].node_id);
    try std.testing.expectEqual(@as(usize, 0), stream.whens.items[0].condition.source_node_ids.len);
    try std.testing.expectEqual(@as(usize, 1), stream.eaches.items.len);
    try std.testing.expectEqual(stream.scope_sites.items[2].node_id, stream.eaches.items[0].node_id);
    try std.testing.expectEqual(@as(usize, 0), stream.eaches.items[0].items.source_node_ids.len);

    try std.testing.expectEqual(@as(usize, 3), host.node_identities.items.len);
    try std.testing.expectEqual(@as(u64, 0), host.node_identities.items[0].ordinal);
    try std.testing.expectEqual(@as(u64, 1), host.node_identities.items[1].ordinal);
    try std.testing.expectEqual(@as(u64, 2), host.node_identities.items[2].ordinal);
}

test "signals host tracks descriptor stream closure lifecycle metrics" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    var stream: HostNodeDescriptorStream = .{};

    const state_token = newTestBinderToken(&roc_host);
    const root_attrs = [_]abi.NodeAttr{
        testNodeSignalTextAttr(&roc_host, .value, testNodeMapExpr(&roc_host, testNodeConstExpr(&roc_host, testHostValueI64(41)))),
    };
    const state_child_attrs = [_]abi.NodeAttr{
        testNodeEventAttr(&roc_host, .click, state_token, .unit),
    };
    const state_child = testElementWith(&roc_host, "button", &state_child_attrs, &.{});
    const state = testNodeStateWithToken(&roc_host, state_token, state_child);
    const items = [_]HostValue{testHostValueI64(1)};
    const each = testNodeEachWithItems(&roc_host, &items);
    const root_children = [_]abi.Elem{ state, each };
    const root = testElementWith(&roc_host, "section", &root_attrs, &root_children);
    defer abi.decrefElem(root, &roc_host);

    host.collectElemRootDescriptors(&roc_host, &stream, root);

    try std.testing.expectEqual(@as(u64, 23), host.pending_roc_metrics.closure_retains);
    try std.testing.expectEqual(@as(u64, 0), host.pending_roc_metrics.closure_releases);

    stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);

    try std.testing.expectEqual(@as(u64, 23), host.pending_roc_metrics.closure_retains);
    try std.testing.expectEqual(@as(u64, 23), host.pending_roc_metrics.closure_releases);
}

test "signals host preserves explicit signal tokens across cloned descriptors" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    var stream: HostNodeDescriptorStream = .{};
    defer stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);

    const signal = testNodeMapExpr(&roc_host, testNodeConstExpr(&roc_host, testHostValueI64(41)));
    abi.increfNodeSignalExpr(signal, 1);
    const root_children = [_]abi.Elem{
        testNodeTextSignal(&roc_host, signal),
        testNodeTextSignal(&roc_host, signal),
    };
    const root = testElement(&roc_host, &root_children);
    defer abi.decrefElem(root, &roc_host);

    host.collectElemRootDescriptors(&roc_host, &stream, root);

    try std.testing.expectEqual(@as(usize, 2), stream.signal_text_nodes.items.len);
    const first = stream.signal_text_nodes.items[0].signal.record;
    const second = stream.signal_text_nodes.items[1].signal.record;
    try std.testing.expect(first == second);
    try std.testing.expectEqual(@as(std.meta.Tag(HostSignalRecordPayload), .map), std.meta.activeTag(first.payload));
    try std.testing.expectEqual(@as(std.meta.Tag(HostSignalRecordPayload), .map), std.meta.activeTag(second.payload));
    try std.testing.expect(first.payload.map.token == second.payload.map.token);
}

test "signals host retains state equality outside descriptor stream" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const root = testNodeStateWithTokenAndInitial(&roc_host, state_token, testHostValueI64(0), testNodeText(&roc_host, "state"));
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    host.active_stream = stream;

    const state_id = host.active_stream.scope_sites.items[0].node_id;
    host.active_stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);
    host.active_stream = .{};

    try std.testing.expect(!host.updateStateValue(&roc_host, state_id, testHostValueI64(0)));
    try std.testing.expect(host.updateStateValue(&roc_host, state_id, testHostValueI64(1)));
}

test "signals host dispatches through active event records outside descriptor stream" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const attrs = [_]abi.NodeAttr{
        testNodeUnitIncrementEventAttr(&roc_host, .click, state_token),
    };
    const button = testElementWith(&roc_host, "button", &attrs, &.{});
    const root = testNodeStateWithTokenAndInitial(&roc_host, state_token, testHostValueI64(0), button);
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    _ = applyNodeDescriptorStream(&host, &roc_host, &stream);
    host.rebuildActiveEventsFromStream(&stream);
    host.active_stream = stream;

    const button_id = host.active_stream.elements.items[0].elem_id;
    const event_id = host.dom_elements.items[@intCast(button_id)].bound_click_event orelse unreachable;
    const state_id = host.active_stream.scope_sites.items[0].node_id;

    host.active_stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);
    host.active_stream = .{};

    dispatchRocEvent(&host, &roc_host, event_id, .unit, testHostValueUnit());
    try expectHostValueI64(host.stateValueByNodeId(state_id), 1);
}

test "signals host keeps live allocations flat across repeated events" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const attrs = [_]abi.NodeAttr{
        testNodeUnitIncrementEventAttr(&roc_host, .click, state_token),
    };
    const button = testElementWith(&roc_host, "button", &attrs, &.{});
    const root = testNodeStateWithTokenAndInitial(&roc_host, state_token, testHostValueI64(0), button);
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    _ = applyNodeDescriptorStream(&host, &roc_host, &stream);
    host.rebuildActiveEventsFromStream(&stream);
    host.active_stream = stream;

    const button_id = host.active_stream.elements.items[0].elem_id;
    const event_id = host.dom_elements.items[@intCast(button_id)].bound_click_event orelse unreachable;
    const state_id = host.active_stream.scope_sites.items[0].node_id;

    var live_after_warmup: ?i64 = null;
    var iteration: usize = 0;
    while (iteration < 100) : (iteration += 1) {
        dispatchRocEvent(&host, &roc_host, event_id, .unit, hostValueUnit(&host, &roc_host));
        const live = @as(i64, @intCast(host.alloc_count)) - @as(i64, @intCast(host.dealloc_count));
        if (iteration == 9) {
            live_after_warmup = live;
        } else if (iteration > 9) {
            try std.testing.expectEqual(live_after_warmup.?, live);
        }
    }

    try expectHostValueI64(host.stateValueByNodeId(state_id), 100);
}

test "signals host carries binder context into Elem when branch collection" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    var stream: HostNodeDescriptorStream = .{};
    defer stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);

    const state_token = newTestBinderToken(&roc_host);
    const branch_children = [_]abi.Elem{
        testNodeTextSignal(&roc_host, testNodeRefExpr(state_token)),
    };
    const branch_true = testElementWith(&roc_host, "span", &.{}, &branch_children);
    const when_elem = testNodeWhen(&roc_host, branch_true, testNodeText(&roc_host, "hidden"));
    const state_child_children = [_]abi.Elem{
        when_elem,
    };
    const state_child = testElementWith(&roc_host, "div", &.{}, &state_child_children);
    const root = testNodeStateWithToken(&roc_host, state_token, state_child);
    defer abi.decrefElem(root, &roc_host);

    host.collectElemRootDescriptors(&roc_host, &stream, root);

    try std.testing.expectEqual(@as(usize, 2), stream.scope_sites.items.len);
    const state_site = stream.scope_sites.items[0];
    const when_site = stream.scope_sites.items[1];
    try std.testing.expectEqual(HostNodeScopeSiteKind.state, state_site.kind);
    try std.testing.expectEqual(HostNodeScopeSiteKind.when, when_site.kind);
    try std.testing.expectEqual(@as(usize, 1), when_site.binder_bindings.len);
    try std.testing.expect(when_site.binder_bindings[0].token == state_token);
    try std.testing.expectEqual(state_site.node_id, when_site.binder_bindings[0].node_id);
    try std.testing.expectEqual(@as(usize, 0), stream.signal_text_nodes.items.len);

    const branch_scope_id = host.collectElemWhenBranchDescriptors(&roc_host, &stream, when_site, .true_branch, when_elem.payload.when.when_true.*);

    try std.testing.expect(branch_scope_id != 0);
    try std.testing.expectEqual(@as(usize, 2), stream.elements.items.len);
    try std.testing.expectEqual(@as(u64, 2), stream.elements.items[1].elem_id);
    try std.testing.expectEqual(@as(u64, 1), stream.elements.items[1].parent_elem_id);
    try std.testing.expectEqual(branch_scope_id, stream.elements.items[1].scope_id);
    try std.testing.expectEqualStrings("span", stream.elements.items[1].tag);
    try std.testing.expectEqual(@as(usize, 1), stream.signal_text_nodes.items.len);
    try std.testing.expectEqual(branch_scope_id, stream.signal_text_nodes.items[0].scope_id);
    try std.testing.expectEqual(@as(usize, 1), stream.signal_text_nodes.items[0].signal.source_node_ids.len);
    try std.testing.expectEqual(state_site.node_id, stream.signal_text_nodes.items[0].signal.source_node_ids[0]);
}

test "signals host disposes inactive Elem when branch scope" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    const branch_true = testNodeState(&roc_host, testNodeText(&roc_host, "true branch"));
    const branch_false = testNodeState(&roc_host, testNodeText(&roc_host, "false branch"));
    const when_elem = testNodeWhen(&roc_host, branch_true, branch_false);
    const root_children = [_]abi.Elem{when_elem};
    const root = testElementWith(&roc_host, "section", &.{}, &root_children);
    defer abi.decrefElem(root, &roc_host);

    var true_stream: HostNodeDescriptorStream = .{};
    defer true_stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);
    host.collectElemRootDescriptors(&roc_host, &true_stream, root);
    const true_scope = host.collectElemActiveWhenBranchDescriptors(&roc_host, &true_stream, true_stream.scope_sites.items[0], true_stream.whens.items[0], .true_branch, when_elem.payload.when.when_true.*);

    try std.testing.expectEqual(@as(usize, 2), true_stream.scope_sites.items.len);
    const true_state_id = true_stream.scope_sites.items[1].node_id;
    try std.testing.expectEqual(true_scope, true_stream.scope_sites.items[1].scope_id);
    try std.testing.expect(host.scopes.items[@intCast(true_scope)].active);
    try std.testing.expect(host.node_identities.items[@intCast(true_state_id)].active);

    var false_stream: HostNodeDescriptorStream = .{};
    defer false_stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);
    host.collectElemRootDescriptors(&roc_host, &false_stream, root);
    const false_scope = host.collectElemActiveWhenBranchDescriptors(&roc_host, &false_stream, false_stream.scope_sites.items[0], false_stream.whens.items[0], .false_branch, when_elem.payload.when.when_false.*);

    try std.testing.expect(false_scope != true_scope);
    try std.testing.expect(!host.scopes.items[@intCast(true_scope)].active);
    try std.testing.expect(!host.node_identities.items[@intCast(true_state_id)].active);
    try std.testing.expect(host.scopes.items[@intCast(false_scope)].active);
    try std.testing.expectEqual(@as(u64, 1), host.pending_roc_metrics.scopes_disposed);
    const false_state_id = false_stream.scope_sites.items[1].node_id;

    var true_again_stream: HostNodeDescriptorStream = .{};
    defer true_again_stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);
    host.collectElemRootDescriptors(&roc_host, &true_again_stream, root);
    const true_again_scope = host.collectElemActiveWhenBranchDescriptors(&roc_host, &true_again_stream, true_again_stream.scope_sites.items[0], true_again_stream.whens.items[0], .true_branch, when_elem.payload.when.when_true.*);

    try std.testing.expect(true_again_scope != true_scope);
    try std.testing.expect(true_again_scope != false_scope);
    try std.testing.expect(!host.scopes.items[@intCast(false_scope)].active);
    try std.testing.expect(!host.node_identities.items[@intCast(false_state_id)].active);
    try std.testing.expect(host.scopes.items[@intCast(true_again_scope)].active);
    try std.testing.expectEqual(@as(u64, 2), host.pending_roc_metrics.scopes_disposed);
    try std.testing.expect(true_again_stream.scope_sites.items[1].node_id != true_state_id);
}

test "signals host collects Elem each row bodies by keyed scope" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    const root_children = [_]abi.Elem{
        testNodeEach(&roc_host),
    };
    const root = testElementWith(&roc_host, "section", &.{}, &root_children);
    defer abi.decrefElem(root, &roc_host);

    var initial_stream: HostNodeDescriptorStream = .{};
    defer initial_stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);
    host.collectElemRootDescriptors(&roc_host, &initial_stream, root);

    try std.testing.expectEqual(@as(usize, 1), initial_stream.scope_sites.items.len);
    try std.testing.expectEqual(HostNodeScopeSiteKind.each, initial_stream.scope_sites.items[0].kind);
    try std.testing.expectEqual(@as(usize, 1), initial_stream.eaches.items.len);

    const initial_items = [_]HostValue{ testHostValueI64(1), testHostValueI64(2), testHostValueI64(3) };
    const initial = host.collectElemEachRowDescriptors(&roc_host, &initial_stream, initial_stream.scope_sites.items[0], initial_stream.eaches.items[0], &initial_items);
    defer freeKeyedRowDiff(&host, initial);

    try std.testing.expectEqual(@as(u64, 0), initial.rows_reused);
    try std.testing.expectEqual(@as(u64, 3), initial.rows_created);
    try std.testing.expectEqual(@as(u64, 0), initial.rows_removed);
    try std.testing.expectEqual(@as(u64, 0), initial.row_items_unchanged);
    try std.testing.expectEqual(@as(u64, 0), initial.row_items_updated);
    try std.testing.expectEqual(@as(usize, 4), initial_stream.scope_sites.items.len);
    try std.testing.expectEqual(@as(usize, 3), initial_stream.states.items.len);
    try std.testing.expectEqual(@as(usize, 3), initial_stream.text_nodes.items.len);

    try std.testing.expectEqual(initial.scope_ids[0], initial_stream.scope_sites.items[1].scope_id);
    try std.testing.expectEqual(initial.scope_ids[1], initial_stream.scope_sites.items[2].scope_id);
    try std.testing.expectEqual(initial.scope_ids[2], initial_stream.scope_sites.items[3].scope_id);
    try std.testing.expectEqual(@as(u64, 0), initial_stream.scope_sites.items[1].ordinal);
    try std.testing.expectEqual(@as(u64, 0), initial_stream.scope_sites.items[2].ordinal);
    try std.testing.expectEqual(@as(u64, 0), initial_stream.scope_sites.items[3].ordinal);
    try std.testing.expectEqualStrings("row-1-1", initial_stream.text_nodes.items[0].value);
    try std.testing.expectEqualStrings("row-2-2", initial_stream.text_nodes.items[1].value);
    try std.testing.expectEqualStrings("row-3-3", initial_stream.text_nodes.items[2].value);

    const state_for_key_2 = host.internNodeIdentity(initial.scope_ids[1], 0);

    var reordered_stream: HostNodeDescriptorStream = .{};
    defer reordered_stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);
    host.collectElemRootDescriptors(&roc_host, &reordered_stream, root);

    const reordered_items = [_]HostValue{ testHostValueI64(3), testHostValueI64(1), testHostValueI64(2) };
    const reordered = host.collectElemEachRowDescriptors(&roc_host, &reordered_stream, reordered_stream.scope_sites.items[0], reordered_stream.eaches.items[0], &reordered_items);
    defer freeKeyedRowDiff(&host, reordered);

    try std.testing.expectEqual(@as(u64, 3), reordered.rows_reused);
    try std.testing.expectEqual(@as(u64, 0), reordered.rows_created);
    try std.testing.expectEqual(@as(u64, 0), reordered.rows_removed);
    try std.testing.expectEqual(@as(u64, 3), reordered.row_items_unchanged);
    try std.testing.expectEqual(@as(u64, 0), reordered.row_items_updated);
    try std.testing.expectEqual(initial.scope_ids[2], reordered.scope_ids[0]);
    try std.testing.expectEqual(initial.scope_ids[0], reordered.scope_ids[1]);
    try std.testing.expectEqual(initial.scope_ids[1], reordered.scope_ids[2]);
    try std.testing.expectEqual(state_for_key_2, host.internNodeIdentity(reordered.scope_ids[2], 0));
    try std.testing.expectEqual(state_for_key_2, reordered_stream.scope_sites.items[3].node_id);

    var changed_stream: HostNodeDescriptorStream = .{};
    defer changed_stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);
    host.collectElemRootDescriptors(&roc_host, &changed_stream, root);

    const changed_items = [_]HostValue{ testHostValueI64(2), testHostValueI64(4) };
    const changed = host.collectElemEachRowDescriptors(&roc_host, &changed_stream, changed_stream.scope_sites.items[0], changed_stream.eaches.items[0], &changed_items);
    defer freeKeyedRowDiff(&host, changed);

    try std.testing.expectEqual(@as(u64, 1), changed.rows_reused);
    try std.testing.expectEqual(@as(u64, 1), changed.rows_created);
    try std.testing.expectEqual(@as(u64, 2), changed.rows_removed);
    try std.testing.expectEqual(@as(u64, 1), changed.row_items_unchanged);
    try std.testing.expectEqual(@as(u64, 0), changed.row_items_updated);
    try std.testing.expectEqual(initial.scope_ids[1], changed.scope_ids[0]);
    try std.testing.expect(changed.scope_ids[1] != initial.scope_ids[0]);
    try std.testing.expect(changed.scope_ids[1] != initial.scope_ids[2]);
    try std.testing.expect(!host.scopes.items[@intCast(initial.scope_ids[0])].active);
    try std.testing.expect(!host.scopes.items[@intCast(initial.scope_ids[2])].active);
    try std.testing.expectEqual(state_for_key_2, host.internNodeIdentity(changed.scope_ids[0], 0));
}
