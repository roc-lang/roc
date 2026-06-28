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
const identity_table = @import("identity_table.zig");
const host_value_registry = @import("host_value_registry.zig");
const hv = @import("host_values.zig");
const engine_metrics = @import("engine_metrics.zig");
const engine_contract = @import("engine_contract.zig");
const render_cache_mod = @import("render_cache.zig");
const descriptor_stream = @import("descriptor_stream.zig");
const retained_values = @import("retained_values.zig");
const signal_records = @import("signal_records.zig");
const active_graph = @import("active_signal_graph.zig");
const scope_runtime = @import("scope_runtime.zig");
const each_runtime = @import("each_runtime.zig");

const enable_runtime_metrics = engine_metrics.enable_runtime_metrics;

pub const RenderTextField = render.TextField;
pub const RenderBoolField = render.BoolField;
pub const RenderEventKind = render.EventKind;
pub const EventPayloadKind = render.EventPayloadKind;
pub const EventPayloadAccessor = render.EventPayloadAccessor;
pub const NodeFieldCustom: u64 = 7;
const node_field_custom: u64 = NodeFieldCustom;

pub const HostValue = retained_values.HostValue;
pub const HostValueList = abi.RocListWith(HostValue, false);
pub const RuntimeMetrics = engine_metrics.RuntimeMetrics;
pub const NoMetrics = engine_metrics.NoMetrics;
pub const DispatchMetrics = engine_metrics.DispatchMetrics;
pub const zeroRuntimeMetrics = engine_metrics.zeroRuntimeMetrics;
pub const verifyRegistryOps = engine_contract.verifyRegistryOps;
pub const verifySink = engine_contract.verifySink;
pub const verifyMetrics = engine_contract.verifyMetrics;
pub const verifyCtx = engine_contract.verifyCtx;
pub const HostNodeScopeSiteKind = descriptor_stream.ScopeSiteKind;
pub const HostTextFieldDescriptorIndexes = descriptor_stream.TextFieldDescriptorIndexes;
pub const HostBoolFieldDescriptorIndexes = descriptor_stream.BoolFieldDescriptorIndexes;
pub const HostEventDescriptorIndexes = descriptor_stream.EventDescriptorIndexes;
pub const HostRenderElemIndex = descriptor_stream.RenderElemIndex;
pub const HostElemDescriptorIndex = descriptor_stream.ElemDescriptorIndex;
pub const HostScopeSiteDescriptorIndexes = descriptor_stream.ScopeSiteDescriptorIndexes;
pub const HostNodeDescriptorIndex = descriptor_stream.NodeDescriptorIndex;
pub const HostRenderNodeKind = descriptor_stream.RenderNodeKind;
pub const HostRenderNode = descriptor_stream.RenderNode;
pub const HostElementDesc = descriptor_stream.ElementDesc;
pub const HostNodeTextNodeDesc = descriptor_stream.TextNodeDesc;
pub const HostNodeStaticTextAttrDesc = descriptor_stream.StaticTextAttrDesc;
pub const HostNodeStaticCustomTextAttrDesc = descriptor_stream.StaticCustomTextAttrDesc;
pub const HostNodeStaticBoolAttrDesc = descriptor_stream.StaticBoolAttrDesc;
pub const HostNodeMountDesc = descriptor_stream.MountDesc;
pub const HostNodeCleanupDesc = descriptor_stream.CleanupDesc;
pub const HostValueCapability = retained_values.HostValueCapability;
pub const HostTextRead = retained_values.HostTextRead;
pub const HostBoolRead = retained_values.HostBoolRead;
pub const HostEventReducer = retained_values.HostEventReducer;
pub const HostTaskRequestRead = retained_values.HostTaskRequestRead;
pub const HostEachOps = retained_values.HostEachOps;
pub const HostSignalToken = retained_values.HostSignalToken;
pub const HostValueCell = retained_values.HostValueCell;
pub const retainHostCallable = retained_values.retainHostCallable;
pub const retainHostSignalToken = retained_values.retainHostSignalToken;
const releaseHostSignalToken = retained_values.releaseHostSignalToken;
const retainHostValueCapability = retained_values.retainHostValueCapability;
const releaseHostValueCapability = retained_values.releaseHostValueCapability;
const assertHostValueCapabilitiesMatch = retained_values.assertHostValueCapabilitiesMatch;
const retainHostTextRead = retained_values.retainHostTextRead;
const releaseHostTextRead = retained_values.releaseHostTextRead;
const retainHostBoolRead = retained_values.retainHostBoolRead;
const releaseHostBoolRead = retained_values.releaseHostBoolRead;
const retainHostEventReducer = retained_values.retainHostEventReducer;
const releaseHostEventReducer = retained_values.releaseHostEventReducer;
const retainHostEachOps = retained_values.retainHostEachOps;
const releaseHostEachOps = retained_values.releaseHostEachOps;
pub const HostSignalCacheSlot = signal_records.CacheSlot;
pub const HostSignalEvalResult = signal_records.EvalResult;
pub const HostSignalConstRecord = signal_records.ConstRecord;
pub const HostSignalMapRecord = signal_records.MapRecord;
pub const HostSignalMap2Record = signal_records.Map2Record;
pub const HostSignalCombineRecord = signal_records.CombineRecord;
pub const HostSignalTaskSourceRecord = signal_records.TaskSourceRecord;
pub const HostSignalIntervalSourceRecord = signal_records.IntervalSourceRecord;
pub const HostSignalRecordPayload = signal_records.Payload;
pub const HostSignalRecord = signal_records.Record;
pub const HostSignalBinding = signal_records.Binding;
pub const validateExistingSignalRecord = signal_records.validateExistingSignalRecord;
pub const appendSignalRecordSourceNodeIds = signal_records.appendSignalRecordSourceNodeIds;

const render_event_kinds = [_]RenderEventKind{ .click, .input, .check, .pointer_down, .pointer_up, .pointer_enter, .pointer_leave };

fn u64SliceContains(items: []const u64, target: u64) bool {
    for (items) |item| {
        if (item == target) return true;
    }
    return false;
}

pub fn appendUniqueU64(allocator: std.mem.Allocator, values: *std.ArrayListUnmanaged(u64), value: u64) void {
    if (u64SliceContains(values.items, value)) return;
    values.append(allocator, value) catch @panic("out of memory");
}

fn renderNodeSliceContainsElem(items: []const HostRenderNode, elem_id: u64) bool {
    for (items) |item| {
        if (item.elem_id == elem_id) return true;
    }
    return false;
}

pub const HostEachRowScopeStep = scope_runtime.EachRowScopeStep;
pub const HostScopeStep = scope_runtime.ScopeStep;
pub const HostScope = scope_runtime.Scope;
pub const deinitHostScopeStep = scope_runtime.deinitScopeStep;

fn hashEachKeyText(bytes: []const u8) u64 {
    return std.hash.Wyhash.hash(0, bytes);
}

const missing_each_row_index = each_runtime.missing_row_index;
const HostEachRowSiteIndexMap = each_runtime.SiteIndexMap;
const HostEachRowMembership = each_runtime.Membership;
const HostEachRowSite = each_runtime.Site;

// Descriptor layer
//
// The host's ingested view of the Roc `Elem` tree: one descriptor per element,
// text node, attribute, event binding, scope site, state, when, and each.
// Markup carries no identity of its own; dynamic descriptors reference shared
// signal records and event reducers by retained thunk.

pub const SignalKind = enum(u64) {
    source = 1,
    map = 2,
    map2 = 3,
};

pub const HostEventDescriptor = struct {
    event_id: u64,
    payload_kind: EventPayloadKind,
    payload_accessor: EventPayloadAccessor,
};

pub const HostActiveEventDesc = struct {
    target_node_id: u64,
    payload_kind: EventPayloadKind,
    payload_accessor: EventPayloadAccessor,
    payload_reducer: HostEventReducer,
};

pub const HostPendingTask = struct {
    request_id: u64,
    owner_scope_id: u64,
    task_token: HostSignalToken,
    task_name: []const u8,
    request: []const u8,
    active: bool,
};

pub const HostActiveInterval = struct {
    token: u64,
    source_token: HostSignalToken,
    period_ms: u64,
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

pub const HostActiveSignalGraphNode = active_graph.Node(HostSignalRecord);
pub const HostNodeIdentity = identity_table.NodeIdentity;
pub const HostDomIdentity = identity_table.DomIdentity;
pub const HostScopeBranch = scope_tree.Branch;

pub const HostActiveTextSignalSinkKind = active_graph.TextSinkKind;
pub const HostActiveTextSignalSink = active_graph.TextSink;
pub const HostActiveBoolSignalSink = active_graph.BoolSink;
pub const HostActiveChangeSignalSink = active_graph.ChangeSink;
pub const HostActiveStructuralSignalKind = active_graph.StructuralKind;
pub const HostActiveStructuralSignal = active_graph.StructuralSink;
pub const HostDirtyStructuralSignal = active_graph.DirtyStructuralSignal;

pub const HostEachSite = scope_runtime.EachSite;

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
    replacement_mount_indices: []usize,

    pub fn deinit(self: HostStructuralSplice, allocator: std.mem.Allocator) void {
        allocator.free(self.removed_elem_ids);
        allocator.free(self.touched_parent_ids);
        allocator.free(self.replacement_elem_ids);
        allocator.free(self.replacement_on_change_indices);
        allocator.free(self.replacement_mount_indices);
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
    scope_created: []bool,
    removed_scope_ids: []u64,
    rows_reused: u64,
    rows_created: u64,
    rows_removed: u64,
    row_items_unchanged: u64,
    row_items_updated: u64,

    pub fn deinit(self: HostKeyedRowDiffResult, allocator: std.mem.Allocator) void {
        allocator.free(self.scope_ids);
        allocator.free(self.row_items_changed);
        allocator.free(self.scope_created);
        allocator.free(self.removed_scope_ids);
    }
};

/// The retained key/item pair read back out of an `Ui.each` row scope. Named so
/// the native forwarder and the engine method share one type instead of each
/// declaring a distinct anonymous struct.
pub const EachRowValues = struct {
    key: HostValue,
    item: HostValue,
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

pub const HostRequiredEventBinding = struct {
    event_id: u64,
    payload_accessor: EventPayloadAccessor,
};

pub const HostRequiredNamedEventBinding = struct {
    event_id: u64,
    options: u32,
    payload_kind: EventPayloadKind,
    payload_accessor: EventPayloadAccessor,
};

pub const HostRequiredEventBindings = struct {
    click: ?HostRequiredEventBinding = null,
    input: ?HostRequiredEventBinding = null,
    check: ?HostRequiredEventBinding = null,
    pointer_down: ?HostRequiredEventBinding = null,
    pointer_up: ?HostRequiredEventBinding = null,
    pointer_enter: ?HostRequiredEventBinding = null,
    pointer_leave: ?HostRequiredEventBinding = null,
};

pub fn requiredEventBindingSlot(bindings: *HostRequiredEventBindings, kind: RenderEventKind) *?HostRequiredEventBinding {
    return switch (kind) {
        .click => &bindings.click,
        .input => &bindings.input,
        .check => &bindings.check,
        .pointer_down => &bindings.pointer_down,
        .pointer_up => &bindings.pointer_up,
        .pointer_enter => &bindings.pointer_enter,
        .pointer_leave => &bindings.pointer_leave,
    };
}

/// Identity token interned per `Ui.state` binder.
pub const HostBinderToken = *u64;

/// Binds a state binder token to the node id it resolves to within a scope.
pub const HostBinderBinding = struct {
    token: HostBinderToken,
    node_id: u64,
};

const EngineScratch = struct {
    debug_seen_render_nodes: std.ArrayListUnmanaged(bool) = .empty,
    debug_expected_children: std.ArrayListUnmanaged(u64) = .empty,
    binder_stack: std.ArrayListUnmanaged(HostBinderBinding) = .empty,
    each_keys: std.ArrayListUnmanaged(HostValue) = .empty,
    each_key_hashes: std.ArrayListUnmanaged(u64) = .empty,
    each_next_hash_heads: std.AutoHashMapUnmanaged(u64, usize) = .empty,
    each_next_hash_links: std.ArrayListUnmanaged(usize) = .empty,
    each_matched_existing: std.ArrayListUnmanaged(bool) = .empty,
    replacement_target_scopes: std.ArrayListUnmanaged(bool) = .empty,
    remove_element_indexes: std.ArrayListUnmanaged(usize) = .empty,
    remove_text_node_indexes: std.ArrayListUnmanaged(usize) = .empty,
    remove_signal_text_node_indexes: std.ArrayListUnmanaged(usize) = .empty,
    remove_static_text_attr_indexes: std.ArrayListUnmanaged(usize) = .empty,
    remove_signal_text_attr_indexes: std.ArrayListUnmanaged(usize) = .empty,
    remove_static_bool_attr_indexes: std.ArrayListUnmanaged(usize) = .empty,
    remove_signal_bool_attr_indexes: std.ArrayListUnmanaged(usize) = .empty,
    remove_event_indexes: std.ArrayListUnmanaged(usize) = .empty,
    remove_named_event_indexes: std.ArrayListUnmanaged(usize) = .empty,

    fn deinit(self: *EngineScratch, allocator: std.mem.Allocator) void {
        self.debug_seen_render_nodes.deinit(allocator);
        self.debug_expected_children.deinit(allocator);
        self.binder_stack.deinit(allocator);
        self.each_keys.deinit(allocator);
        self.each_key_hashes.deinit(allocator);
        self.each_next_hash_heads.deinit(allocator);
        self.each_next_hash_links.deinit(allocator);
        self.each_matched_existing.deinit(allocator);
        self.replacement_target_scopes.deinit(allocator);
        self.remove_element_indexes.deinit(allocator);
        self.remove_text_node_indexes.deinit(allocator);
        self.remove_signal_text_node_indexes.deinit(allocator);
        self.remove_static_text_attr_indexes.deinit(allocator);
        self.remove_signal_text_attr_indexes.deinit(allocator);
        self.remove_static_bool_attr_indexes.deinit(allocator);
        self.remove_signal_bool_attr_indexes.deinit(allocator);
        self.remove_event_indexes.deinit(allocator);
        self.remove_named_event_indexes.deinit(allocator);
        self.* = .{};
    }
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

pub const HostNodeSignalTextNodeDesc = struct {
    elem_id: u64,
    parent_elem_id: u64,
    scope_id: u64,
    signal: HostSignalBinding,
    read: HostTextRead,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostNodeSignalTextAttrDesc = struct {
    elem_id: u64,
    field: RenderTextField,
    signal: HostSignalBinding,
    read: HostTextRead,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostNodeSignalCustomTextAttrDesc = struct {
    elem_id: u64,
    name: []const u8,
    signal: HostSignalBinding,
    read: HostTextRead,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostNodeSignalBoolAttrDesc = struct {
    elem_id: u64,
    field: RenderBoolField,
    signal: HostSignalBinding,
    read: HostBoolRead,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostNodeOnChangeDesc = struct {
    scope_id: u64,
    signal: HostSignalBinding,
    to_cmd: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostNodeEventDesc = struct {
    elem_id: u64,
    kind: RenderEventKind,
    binder_token: HostBinderToken,
    target_node_id: u64,
    payload_kind: EventPayloadKind,
    payload_accessor: EventPayloadAccessor,
    payload_reducer: HostEventReducer,
    owns_payload_reducer: bool = true,
};

pub const HostNodeNamedEventDesc = struct {
    elem_id: u64,
    name: []const u8,
    options: u32,
    binder_token: HostBinderToken,
    target_node_id: u64,
    payload_kind: EventPayloadKind,
    payload_accessor: EventPayloadAccessor,
    payload_reducer: HostEventReducer,
    owns_payload_reducer: bool = true,
};

pub const HostNodeStateDesc = struct {
    node_id: u64,
    initial: abi.RocErasedCallable,
    cap: HostValueCapability,
};

pub const HostNodeWhenDesc = struct {
    node_id: u64,
    condition: HostSignalBinding,
    read: HostBoolRead,
    when_false: abi.Elem,
    when_true: abi.Elem,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostNodeEachDesc = struct {
    node_id: u64,
    items: HostSignalBinding,
    ops: HostEachOps,
    cached_value: HostSignalCacheSlot = .absent,
};

// Descriptor stream
//
// The retained stream of node descriptors plus the per-elem index that makes
// descriptor lookup O(1). Its append* methods ingest the Roc Elem tree; the
// host drives ingestion and consumes the stream to render.

pub const HostNodeDescriptorStream = struct {
    pub const RenderNode = HostRenderNode;
    pub const ElementDesc = HostElementDesc;
    pub const TextNodeDesc = HostNodeTextNodeDesc;
    pub const SignalTextNodeDesc = HostNodeSignalTextNodeDesc;

    render_nodes: std.ArrayListUnmanaged(HostRenderNode) = .empty,
    elements: std.ArrayListUnmanaged(HostElementDesc) = .empty,
    text_nodes: std.ArrayListUnmanaged(HostNodeTextNodeDesc) = .empty,
    signal_text_nodes: std.ArrayListUnmanaged(HostNodeSignalTextNodeDesc) = .empty,
    static_text_attrs: std.ArrayListUnmanaged(HostNodeStaticTextAttrDesc) = .empty,
    signal_text_attrs: std.ArrayListUnmanaged(HostNodeSignalTextAttrDesc) = .empty,
    static_custom_text_attrs: std.ArrayListUnmanaged(HostNodeStaticCustomTextAttrDesc) = .empty,
    signal_custom_text_attrs: std.ArrayListUnmanaged(HostNodeSignalCustomTextAttrDesc) = .empty,
    static_bool_attrs: std.ArrayListUnmanaged(HostNodeStaticBoolAttrDesc) = .empty,
    signal_bool_attrs: std.ArrayListUnmanaged(HostNodeSignalBoolAttrDesc) = .empty,
    on_changes: std.ArrayListUnmanaged(HostNodeOnChangeDesc) = .empty,
    mounts: std.ArrayListUnmanaged(HostNodeMountDesc) = .empty,
    cleanups: std.ArrayListUnmanaged(HostNodeCleanupDesc) = .empty,
    events: std.ArrayListUnmanaged(HostNodeEventDesc) = .empty,
    named_events: std.ArrayListUnmanaged(HostNodeNamedEventDesc) = .empty,
    scope_sites: std.ArrayListUnmanaged(HostNodeScopeSiteDesc) = .empty,
    states: std.ArrayListUnmanaged(HostNodeStateDesc) = .empty,
    whens: std.ArrayListUnmanaged(HostNodeWhenDesc) = .empty,
    eaches: std.ArrayListUnmanaged(HostNodeEachDesc) = .empty,
    signal_records_by_token: std.AutoHashMapUnmanaged(usize, *HostSignalRecord) = .{},
    signal_record_descriptor_uses_by_token: std.AutoHashMapUnmanaged(usize, usize) = .{},
    render_metadata_by_elem_id: std.AutoHashMapUnmanaged(u64, HostRenderElemIndex) = .{},
    descriptor_indexes_by_elem_id: std.ArrayListUnmanaged(HostElemDescriptorIndex) = .empty,
    descriptor_indexes_by_node_id: std.ArrayListUnmanaged(HostNodeDescriptorIndex) = .empty,
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

    pub fn ensureNodeDescriptorIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, node_id: u64) *HostNodeDescriptorIndex {
        const index: usize = @intCast(node_id);
        while (self.descriptor_indexes_by_node_id.items.len <= index) {
            self.descriptor_indexes_by_node_id.append(allocator, .{}) catch @panic("out of memory");
        }
        return &self.descriptor_indexes_by_node_id.items[index];
    }

    pub fn nodeDescriptorIndex(self: *const HostNodeDescriptorStream, node_id: u64) ?HostNodeDescriptorIndex {
        if (node_id >= self.descriptor_indexes_by_node_id.items.len) return null;
        return self.descriptor_indexes_by_node_id.items[@intCast(node_id)];
    }

    pub fn ensureRenderMetadata(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64) *HostRenderElemIndex {
        return descriptor_stream.ensureRenderMetadata(HostNodeDescriptorStream, self, allocator, elem_id);
    }

    pub fn removeRenderMetadataIfEmpty(self: *HostNodeDescriptorStream, elem_id: u64) void {
        descriptor_stream.removeRenderMetadataIfEmpty(HostNodeDescriptorStream, self, elem_id);
    }

    pub fn renderNodeIndex(self: *const HostNodeDescriptorStream, elem_id: u64) ?usize {
        return descriptor_stream.renderNodeIndex(HostNodeDescriptorStream, self, elem_id);
    }

    pub fn recordRenderNodeIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, index: usize) void {
        descriptor_stream.recordRenderNodeIndex(HostNodeDescriptorStream, self, allocator, elem_id, index);
    }

    pub fn updateRenderNodeIndex(self: *HostNodeDescriptorStream, elem_id: u64, index: usize) void {
        descriptor_stream.updateRenderNodeIndex(HostNodeDescriptorStream, self, elem_id, index);
    }

    pub fn clearRenderNodeIndex(self: *HostNodeDescriptorStream, elem_id: u64, expected: usize) void {
        descriptor_stream.clearRenderNodeIndex(HostNodeDescriptorStream, self, elem_id, expected);
    }

    pub fn ensureFirstRenderChildSlot(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, parent_elem_id: u64) *?u64 {
        return descriptor_stream.ensureFirstRenderChildSlot(HostNodeDescriptorStream, self, allocator, parent_elem_id);
    }

    pub fn ensureLastRenderChildSlot(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, parent_elem_id: u64) *?u64 {
        return descriptor_stream.ensureLastRenderChildSlot(HostNodeDescriptorStream, self, allocator, parent_elem_id);
    }

    pub fn ensureNextRenderSiblingSlot(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64) *?u64 {
        return descriptor_stream.ensureNextRenderSiblingSlot(HostNodeDescriptorStream, self, allocator, elem_id);
    }

    pub fn firstRenderChild(self: *const HostNodeDescriptorStream, parent_elem_id: u64) ?u64 {
        return descriptor_stream.firstRenderChild(HostNodeDescriptorStream, self, parent_elem_id);
    }

    pub fn lastRenderChild(self: *const HostNodeDescriptorStream, parent_elem_id: u64) ?u64 {
        return descriptor_stream.lastRenderChild(HostNodeDescriptorStream, self, parent_elem_id);
    }

    pub fn nextRenderSibling(self: *const HostNodeDescriptorStream, elem_id: u64) ?u64 {
        return descriptor_stream.nextRenderSibling(HostNodeDescriptorStream, self, elem_id);
    }

    pub fn appendRenderChild(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, parent_elem_id: u64, elem_id: u64) void {
        descriptor_stream.appendRenderChild(HostNodeDescriptorStream, self, allocator, parent_elem_id, elem_id);
    }

    pub fn clearRenderChildren(self: *HostNodeDescriptorStream, parent_elem_id: u64) void {
        descriptor_stream.clearRenderChildren(HostNodeDescriptorStream, self, parent_elem_id);
    }

    pub fn removeRenderChild(self: *HostNodeDescriptorStream, parent_elem_id: u64, elem_id: u64) void {
        descriptor_stream.removeRenderChild(HostNodeDescriptorStream, self, parent_elem_id, elem_id);
    }

    pub fn insertRenderChildren(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, parent_elem_id: u64, index: usize, elem_ids: []const u64) void {
        descriptor_stream.insertRenderChildren(HostNodeDescriptorStream, self, allocator, parent_elem_id, index, elem_ids);
    }

    pub fn replaceRenderChildrenIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, parent_elem_id: u64, elem_ids: []const u64) void {
        descriptor_stream.replaceRenderChildrenIndex(HostNodeDescriptorStream, self, allocator, parent_elem_id, elem_ids);
    }

    pub fn childInsertionIndexForRenderIndex(self: *const HostNodeDescriptorStream, parent_elem_id: u64, render_insert_index: usize) usize {
        return descriptor_stream.childInsertionIndexForRenderIndex(HostNodeDescriptorStream, self, parent_elem_id, render_insert_index);
    }

    pub fn refreshRenderIndexesFrom(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, start_index: usize, metrics: anytype) void {
        descriptor_stream.refreshRenderIndexesFrom(HostNodeDescriptorStream, self, allocator, start_index, metrics);
    }

    pub fn moveReplacementRenderChildren(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, replacement: *HostNodeDescriptorStream, elem_id: u64) void {
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

    pub fn replaceRenderRangeWithStream(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, render_start: usize, removed_nodes: []const HostRenderNode, replacement: *HostNodeDescriptorStream, metrics: anytype) void {
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
            const parent_elem_id = renderNodeParentElemId(self, node);
            if (!renderNodeSliceContainsElem(removed_nodes, parent_elem_id)) {
                self.removeRenderChild(parent_elem_id, node.elem_id);
            }
            self.clearRenderChildren(node.elem_id);
            self.clearRenderNodeIndex(node.elem_id, index);
        }

        for (replacement.render_nodes.items) |node| {
            const parent_elem_id = renderNodeParentElemId(replacement, node);
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

    pub fn recordElementIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, index: usize) void {
        descriptor_stream.setFreshIndex(&self.ensureElemDescriptorIndex(allocator, elem_id).element, index);
    }

    pub fn updateElementIndex(self: *HostNodeDescriptorStream, elem_id: u64, index: usize) void {
        descriptor_stream.updateIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].element, index);
    }

    pub fn clearElementIndex(self: *HostNodeDescriptorStream, elem_id: u64, expected: usize) void {
        descriptor_stream.clearIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].element, expected);
    }

    pub fn recordTextNodeIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, index: usize) void {
        descriptor_stream.setFreshIndex(&self.ensureElemDescriptorIndex(allocator, elem_id).text_node, index);
    }

    pub fn updateTextNodeIndex(self: *HostNodeDescriptorStream, elem_id: u64, index: usize) void {
        descriptor_stream.updateIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].text_node, index);
    }

    pub fn clearTextNodeIndex(self: *HostNodeDescriptorStream, elem_id: u64, expected: usize) void {
        descriptor_stream.clearIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].text_node, expected);
    }

    pub fn recordSignalTextNodeIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, index: usize) void {
        descriptor_stream.setFreshIndex(&self.ensureElemDescriptorIndex(allocator, elem_id).signal_text_node, index);
    }

    pub fn updateSignalTextNodeIndex(self: *HostNodeDescriptorStream, elem_id: u64, index: usize) void {
        descriptor_stream.updateIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_text_node, index);
    }

    pub fn clearSignalTextNodeIndex(self: *HostNodeDescriptorStream, elem_id: u64, expected: usize) void {
        descriptor_stream.clearIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_text_node, expected);
    }

    pub fn recordStaticTextAttrIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderTextField, index: usize) void {
        descriptor_stream.setFreshIndex(self.ensureElemDescriptorIndex(allocator, elem_id).static_text_attrs.slot(field), index);
    }

    pub fn updateStaticTextAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderTextField, index: usize) void {
        descriptor_stream.updateIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].static_text_attrs.slot(field), index);
    }

    pub fn clearStaticTextAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderTextField, expected: usize) void {
        descriptor_stream.clearIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].static_text_attrs.slot(field), expected);
    }

    pub fn recordSignalTextAttrIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderTextField, index: usize) void {
        descriptor_stream.setFreshIndex(self.ensureElemDescriptorIndex(allocator, elem_id).signal_text_attrs.slot(field), index);
    }

    pub fn updateSignalTextAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderTextField, index: usize) void {
        descriptor_stream.updateIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_text_attrs.slot(field), index);
    }

    pub fn clearSignalTextAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderTextField, expected: usize) void {
        descriptor_stream.clearIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_text_attrs.slot(field), expected);
    }

    pub fn recordStaticBoolAttrIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderBoolField, index: usize) void {
        descriptor_stream.setFreshIndex(self.ensureElemDescriptorIndex(allocator, elem_id).static_bool_attrs.slot(field), index);
    }

    pub fn updateStaticBoolAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderBoolField, index: usize) void {
        descriptor_stream.updateIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].static_bool_attrs.slot(field), index);
    }

    pub fn clearStaticBoolAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderBoolField, expected: usize) void {
        descriptor_stream.clearIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].static_bool_attrs.slot(field), expected);
    }

    pub fn recordSignalBoolAttrIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderBoolField, index: usize) void {
        descriptor_stream.setFreshIndex(self.ensureElemDescriptorIndex(allocator, elem_id).signal_bool_attrs.slot(field), index);
    }

    pub fn updateSignalBoolAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderBoolField, index: usize) void {
        descriptor_stream.updateIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_bool_attrs.slot(field), index);
    }

    pub fn clearSignalBoolAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderBoolField, expected: usize) void {
        descriptor_stream.clearIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_bool_attrs.slot(field), expected);
    }

    pub fn recordEventIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, kind: RenderEventKind, index: usize) void {
        descriptor_stream.setFreshIndex(self.ensureElemDescriptorIndex(allocator, elem_id).events.slot(kind), index);
    }

    pub fn updateEventIndex(self: *HostNodeDescriptorStream, elem_id: u64, kind: RenderEventKind, index: usize) void {
        descriptor_stream.updateIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].events.slot(kind), index);
    }

    pub fn clearEventIndex(self: *HostNodeDescriptorStream, elem_id: u64, kind: RenderEventKind, expected: usize) void {
        descriptor_stream.clearIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].events.slot(kind), expected);
    }

    pub fn recordScopeSiteIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, node_id: u64, kind: HostNodeScopeSiteKind, index: usize) void {
        descriptor_stream.setFreshIndex(self.ensureNodeDescriptorIndex(allocator, node_id).scope_sites.slot(kind), index);
    }

    pub fn updateScopeSiteIndex(self: *HostNodeDescriptorStream, node_id: u64, kind: HostNodeScopeSiteKind, index: usize) void {
        descriptor_stream.updateIndex(self.descriptor_indexes_by_node_id.items[@intCast(node_id)].scope_sites.slot(kind), index);
    }

    pub fn clearScopeSiteIndex(self: *HostNodeDescriptorStream, node_id: u64, kind: HostNodeScopeSiteKind, expected: usize) void {
        descriptor_stream.clearIndex(self.descriptor_indexes_by_node_id.items[@intCast(node_id)].scope_sites.slot(kind), expected);
    }

    pub fn recordStateIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, node_id: u64, index: usize) void {
        descriptor_stream.setFreshIndex(&self.ensureNodeDescriptorIndex(allocator, node_id).state, index);
    }

    pub fn updateStateIndex(self: *HostNodeDescriptorStream, node_id: u64, index: usize) void {
        descriptor_stream.updateIndex(&self.descriptor_indexes_by_node_id.items[@intCast(node_id)].state, index);
    }

    pub fn clearStateIndex(self: *HostNodeDescriptorStream, node_id: u64, expected: usize) void {
        descriptor_stream.clearIndex(&self.descriptor_indexes_by_node_id.items[@intCast(node_id)].state, expected);
    }

    pub fn recordWhenIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, node_id: u64, index: usize) void {
        descriptor_stream.setFreshIndex(&self.ensureNodeDescriptorIndex(allocator, node_id).when, index);
    }

    pub fn updateWhenIndex(self: *HostNodeDescriptorStream, node_id: u64, index: usize) void {
        descriptor_stream.updateIndex(&self.descriptor_indexes_by_node_id.items[@intCast(node_id)].when, index);
    }

    pub fn clearWhenIndex(self: *HostNodeDescriptorStream, node_id: u64, expected: usize) void {
        descriptor_stream.clearIndex(&self.descriptor_indexes_by_node_id.items[@intCast(node_id)].when, expected);
    }

    pub fn recordEachIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, node_id: u64, index: usize) void {
        descriptor_stream.setFreshIndex(&self.ensureNodeDescriptorIndex(allocator, node_id).each, index);
    }

    pub fn updateEachIndex(self: *HostNodeDescriptorStream, node_id: u64, index: usize) void {
        descriptor_stream.updateIndex(&self.descriptor_indexes_by_node_id.items[@intCast(node_id)].each, index);
    }

    pub fn clearEachIndex(self: *HostNodeDescriptorStream, node_id: u64, expected: usize) void {
        descriptor_stream.clearIndex(&self.descriptor_indexes_by_node_id.items[@intCast(node_id)].each, expected);
    }

    pub fn deinit(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype) void {
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

    pub fn signalRecordByToken(self: *HostNodeDescriptorStream, token: HostSignalToken) ?*HostSignalRecord {
        return self.signal_records_by_token.get(@intFromPtr(token));
    }

    pub fn rememberSignalRecord(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, record: *HostSignalRecord) void {
        const token = record.token() orelse return;
        const entry = self.signal_records_by_token.getOrPut(allocator, @intFromPtr(token)) catch @panic("out of memory");
        if (entry.found_existing) {
            if (entry.value_ptr.* != record) @panic("signal token was bound to multiple host records");
            return;
        }
        entry.value_ptr.* = record;
    }

    fn incrementSignalRecordDescriptorUse(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, record: *HostSignalRecord) void {
        const token = record.token() orelse return;
        const key = @intFromPtr(token);
        const entry = self.signal_record_descriptor_uses_by_token.getOrPut(allocator, key) catch @panic("out of memory");
        if (entry.found_existing) {
            entry.value_ptr.* += 1;
        } else {
            entry.value_ptr.* = 1;
        }
    }

    fn decrementSignalRecordDescriptorUse(self: *HostNodeDescriptorStream, record: *HostSignalRecord) void {
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

    pub fn rememberSignalRecordTree(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, record: *HostSignalRecord) void {
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

    pub fn forgetSignalRecordTree(self: *HostNodeDescriptorStream, record: *HostSignalRecord) void {
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

    pub fn appendElement(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, parent_elem_id: u64, scope_id: u64, tag: []const u8) u64 {
        self.next_elem_id += 1;

        const tag_copy = allocator.dupe(u8, tag) catch @panic("out of memory");
        const element_index = self.elements.items.len;
        const render_index = self.render_nodes.items.len;
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
        self.recordRenderNodeIndex(allocator, elem_id, render_index);
        self.appendRenderChild(allocator, parent_elem_id, elem_id);
        return elem_id;
    }

    pub fn appendTextNode(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, parent_elem_id: u64, scope_id: u64, value: []const u8) void {
        self.next_elem_id += 1;

        const value_copy = allocator.dupe(u8, value) catch @panic("out of memory");
        const text_node_index = self.text_nodes.items.len;
        const render_index = self.render_nodes.items.len;
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
        self.recordRenderNodeIndex(allocator, elem_id, render_index);
        self.appendRenderChild(allocator, parent_elem_id, elem_id);
    }

    pub fn appendSignalTextNode(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, elem_id: u64, parent_elem_id: u64, scope_id: u64, signal: HostSignalBinding, read: HostTextRead) void {
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

    pub fn appendSignalTextAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, elem_id: u64, field: RenderTextField, signal: HostSignalBinding, read: HostTextRead) void {
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

    pub fn customTextAttrDescriptorExists(self: *const HostNodeDescriptorStream, elem_id: u64, name: []const u8) bool {
        for (self.static_custom_text_attrs.items) |desc| {
            if (desc.elem_id == elem_id and std.mem.eql(u8, desc.name, name)) return true;
        }
        for (self.signal_custom_text_attrs.items) |desc| {
            if (desc.elem_id == elem_id and std.mem.eql(u8, desc.name, name)) return true;
        }
        return false;
    }

    pub fn appendStaticCustomTextAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, name: []const u8, value: []const u8) void {
        if (name.len == 0) @panic("custom text attr descriptor used an empty name");
        if (self.customTextAttrDescriptorExists(elem_id, name)) @panic("element has duplicate custom text attr descriptors");

        const name_copy = allocator.dupe(u8, name) catch @panic("out of memory");
        const value_copy = allocator.dupe(u8, value) catch {
            allocator.free(name_copy);
            @panic("out of memory");
        };
        self.static_custom_text_attrs.append(allocator, .{
            .elem_id = elem_id,
            .name = name_copy,
            .value = value_copy,
        }) catch {
            allocator.free(name_copy);
            allocator.free(value_copy);
            @panic("out of memory");
        };
    }

    pub fn appendSignalCustomTextAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, elem_id: u64, name: []const u8, signal: HostSignalBinding, read: HostTextRead) void {
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

    pub fn appendStaticBoolAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderBoolField, value: bool) void {
        const attr_index = self.static_bool_attrs.items.len;
        self.static_bool_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .value = value,
        }) catch @panic("out of memory");
        self.recordStaticBoolAttrIndex(allocator, elem_id, field, attr_index);
    }

    pub fn appendSignalBoolAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, elem_id: u64, field: RenderBoolField, signal: HostSignalBinding, read: HostBoolRead) void {
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

    pub fn appendOnChange(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, scope_id: u64, signal: HostSignalBinding, to_cmd: abi.RocErasedCallable) void {
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

    pub fn appendMount(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype, scope_id: u64, to_cmd: abi.RocErasedCallable, run_on_mount: bool) void {
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

    pub fn appendEvent(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype, elem_id: u64, kind: RenderEventKind, binder_token: HostBinderToken, target_node_id: u64, payload_kind: EventPayloadKind, payload_accessor: EventPayloadAccessor, payload_reducer: HostEventReducer) void {
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

    pub fn namedEventDescriptorExists(self: *const HostNodeDescriptorStream, elem_id: u64, name: []const u8) bool {
        for (self.named_events.items) |desc| {
            if (desc.elem_id == elem_id and std.mem.eql(u8, desc.name, name)) return true;
        }
        return false;
    }

    pub fn appendNamedEvent(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype, elem_id: u64, name: []const u8, options: u64, binder_token: HostBinderToken, target_node_id: u64, payload_kind: EventPayloadKind, payload_accessor: EventPayloadAccessor, payload_reducer: HostEventReducer) void {
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

    pub fn appendScopeSite(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, node_id: u64, scope_id: u64, ordinal: u64, parent_elem_id: u64, kind: HostNodeScopeSiteKind, binder_bindings: []const HostBinderBinding) void {
        self.appendScopeSiteAt(allocator, node_id, scope_id, ordinal, parent_elem_id, self.render_nodes.items.len, kind, binder_bindings);
    }

    pub fn appendScopeSiteAt(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, node_id: u64, scope_id: u64, ordinal: u64, parent_elem_id: u64, render_insert_index: usize, kind: HostNodeScopeSiteKind, binder_bindings: []const HostBinderBinding) void {
        const binder_copy = allocator.dupe(HostBinderBinding, binder_bindings) catch @panic("out of memory");
        const scope_site_index = self.scope_sites.items.len;
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
        self.recordScopeSiteIndex(allocator, node_id, kind, scope_site_index);
    }

    pub fn appendState(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype, node_id: u64, initial: abi.RocErasedCallable, cap: HostValueCapability) void {
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

    pub fn appendWhen(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, node_id: u64, condition: HostSignalBinding, read: HostBoolRead, when_false: abi.Elem, when_true: abi.Elem) void {
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

    pub fn appendEach(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, node_id: u64, items: HostSignalBinding, ops: HostEachOps) void {
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

// Host-agnostic readers over a descriptor stream. These operate purely on the
// stream's descriptor tables and panic on internal invariant violations, so they
// are shared by both hosts and by the engine's structural apply path.

pub fn findElementDesc(stream: *const HostNodeDescriptorStream, elem_id: u64) ?HostElementDesc {
    return descriptor_stream.findElementDesc(HostNodeDescriptorStream, stream, elem_id);
}

pub fn findTextNodeDesc(stream: *const HostNodeDescriptorStream, elem_id: u64) ?HostNodeTextNodeDesc {
    return descriptor_stream.findTextNodeDesc(HostNodeDescriptorStream, stream, elem_id);
}

pub fn findSignalTextNodeDesc(stream: *const HostNodeDescriptorStream, elem_id: u64) ?HostNodeSignalTextNodeDesc {
    return descriptor_stream.findSignalTextNodeDesc(HostNodeDescriptorStream, stream, elem_id);
}

pub fn findSignalTextNodeDescMutable(stream: *HostNodeDescriptorStream, elem_id: u64) ?*HostNodeSignalTextNodeDesc {
    return descriptor_stream.findSignalTextNodeDescMutable(HostNodeDescriptorStream, stream, elem_id);
}

pub fn streamHasTextField(stream: *const HostNodeDescriptorStream, elem_id: u64, field: RenderTextField) bool {
    return descriptor_stream.streamHasTextField(HostNodeDescriptorStream, stream, elem_id, field);
}

pub fn streamHasCustomTextAttr(stream: *const HostNodeDescriptorStream, elem_id: u64, name: []const u8) bool {
    return descriptor_stream.streamHasCustomTextAttr(HostNodeDescriptorStream, stream, elem_id, name);
}

pub fn streamHasBoolField(stream: *const HostNodeDescriptorStream, elem_id: u64, field: RenderBoolField) bool {
    return descriptor_stream.streamHasBoolField(HostNodeDescriptorStream, stream, elem_id, field);
}

pub fn maxRenderElemId(stream: *const HostNodeDescriptorStream) u64 {
    return descriptor_stream.maxRenderElemId(HostNodeDescriptorStream, stream);
}

pub fn renderNodeTag(stream: *const HostNodeDescriptorStream, node: HostRenderNode) []const u8 {
    return descriptor_stream.renderNodeTag(HostNodeDescriptorStream, stream, node);
}

pub fn streamElemTag(stream: *const HostNodeDescriptorStream, elem_id: u64) []const u8 {
    return descriptor_stream.streamElemTag(HostNodeDescriptorStream, stream, elem_id);
}

pub fn renderNodeParentElemId(stream: *const HostNodeDescriptorStream, node: HostRenderNode) u64 {
    return descriptor_stream.renderNodeParentElemId(HostNodeDescriptorStream, stream, node);
}

pub fn streamElemParentElemId(stream: *const HostNodeDescriptorStream, elem_id: u64) u64 {
    return descriptor_stream.streamElemParentElemId(HostNodeDescriptorStream, stream, elem_id);
}

pub fn streamDirectChildren(allocator: std.mem.Allocator, stream: *const HostNodeDescriptorStream, parent_elem_id: u64) []u64 {
    return descriptor_stream.streamDirectChildren(HostNodeDescriptorStream, allocator, stream, parent_elem_id);
}

pub fn renderNodeScopeId(stream: *const HostNodeDescriptorStream, node: HostRenderNode) u64 {
    return descriptor_stream.renderNodeScopeId(HostNodeDescriptorStream, stream, node);
}

pub fn elemScopeId(stream: *const HostNodeDescriptorStream, elem_id: u64) ?u64 {
    return descriptor_stream.elemScopeId(HostNodeDescriptorStream, stream, elem_id);
}

pub fn adjustedRenderInsertIndex(old_index: usize, replace_index: usize, removed_count: usize, replacement_count: usize) usize {
    return descriptor_stream.adjustedRenderInsertIndex(old_index, replace_index, removed_count, replacement_count);
}

fn sourceNodeIdsIntersect(left: []const u64, right: []const u64) bool {
    for (left) |left_id| {
        for (right) |right_id| {
            if (left_id == right_id) return true;
        }
    }
    return false;
}

// Host-agnostic signal-record construction helpers (shared by both hosts).

pub fn resolveNodeBinderRef(binder_stack: []const HostBinderBinding, token: HostBinderToken) u64 {
    var index = binder_stack.len;
    while (index > 0) {
        index -= 1;
        const binding = binder_stack[index];
        if (binding.token == token) return binding.node_id;
    }
    @panic("Node.BinderRef referenced a state binder outside the active scope");
}

pub fn renderTextFieldFromAbi(field: u64) RenderTextField {
    return switch (field) {
        @intFromEnum(RenderTextField.text) => .text,
        @intFromEnum(RenderTextField.role) => .role,
        @intFromEnum(RenderTextField.label) => .label,
        @intFromEnum(RenderTextField.test_id) => .test_id,
        @intFromEnum(RenderTextField.value) => .value,
        @intFromEnum(RenderTextField.class) => .class,
        else => @panic("Roc render text descriptor used an unknown field"),
    };
}

pub fn renderBoolFieldFromAbi(field: u64) RenderBoolField {
    return switch (field) {
        @intFromEnum(RenderBoolField.checked) => .checked,
        @intFromEnum(RenderBoolField.disabled) => .disabled,
        else => @panic("Roc render bool descriptor used an unknown field"),
    };
}

pub fn renderEventKindFromAbi(kind: u64) RenderEventKind {
    return switch (kind) {
        @intFromEnum(RenderEventKind.click) => .click,
        @intFromEnum(RenderEventKind.input) => .input,
        @intFromEnum(RenderEventKind.check) => .check,
        @intFromEnum(RenderEventKind.pointer_down) => .pointer_down,
        @intFromEnum(RenderEventKind.pointer_up) => .pointer_up,
        @intFromEnum(RenderEventKind.pointer_enter) => .pointer_enter,
        @intFromEnum(RenderEventKind.pointer_leave) => .pointer_leave,
        else => @panic("Roc render event descriptor used an unknown event kind"),
    };
}

pub fn eventPayloadKindFromAbi(payload_kind: u64) EventPayloadKind {
    return switch (payload_kind) {
        @intFromEnum(EventPayloadKind.unit) => .unit,
        @intFromEnum(EventPayloadKind.str) => .str,
        @intFromEnum(EventPayloadKind.bool) => .bool,
        @intFromEnum(EventPayloadKind.bytes) => .bytes,
        else => @panic("Roc event descriptor used an unknown payload kind"),
    };
}

pub fn eventPayloadAccessorFromAbi(payload_accessor: u64) EventPayloadAccessor {
    return switch (payload_accessor) {
        @intFromEnum(EventPayloadAccessor.none) => .none,
        @intFromEnum(EventPayloadAccessor.target_value) => .target_value,
        @intFromEnum(EventPayloadAccessor.target_checked) => .target_checked,
        @intFromEnum(EventPayloadAccessor.record_key_shift) => .record_key_shift,
        else => @panic("Roc event descriptor used an unknown payload accessor"),
    };
}

pub fn Engine(comptime Ctx: type) type {
    verifyCtx(Ctx);

    return struct {
        const Self = @This();

        pub const Context = Ctx;
        pub const Metrics = Ctx.Metrics;
        pub const RegistryOps = Ctx.RegistryOps;
        pub const HostValueRegistry = host_value_registry.Registry(HostValueCapability);
        pub const ActiveEventDesc = HostActiveEventDesc;
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
        state_indexes_by_node_id: std.ArrayListUnmanaged(?usize) = .empty,
        scopes: std.ArrayListUnmanaged(HostScope) = .empty,
        each_row_site_indexes: HostEachRowSiteIndexMap = .empty,
        each_row_sites: std.ArrayListUnmanaged(HostEachRowSite) = .empty,
        each_row_memberships_by_scope_id: std.ArrayListUnmanaged(?HostEachRowMembership) = .empty,
        node_identities: std.ArrayListUnmanaged(HostNodeIdentity) = .empty,
        dom_identities: std.ArrayListUnmanaged(HostDomIdentity) = .empty,
        active_stream: HostNodeDescriptorStream = .{},
        active_signal_graph: std.ArrayListUnmanaged(HostActiveSignalGraphNode) = .empty,
        active_source_signal_routes: active_graph.RouteTable(u64) = .empty,
        active_text_signal_routes: active_graph.RouteTable(HostActiveTextSignalSink) = .empty,
        active_bool_signal_routes: active_graph.RouteTable(HostActiveBoolSignalSink) = .empty,
        active_change_signal_routes: active_graph.RouteTable(HostActiveChangeSignalSink) = .empty,
        active_structural_signal_routes: active_graph.RouteTable(HostActiveStructuralSignal) = .empty,
        render_cache: render_cache_mod.Cache(Ctx) = .{},
        pending_tasks: std.ArrayListUnmanaged(HostPendingTask) = .empty,
        active_intervals: std.ArrayListUnmanaged(HostActiveInterval) = .empty,
        cleanup_events: std.ArrayListUnmanaged([]const u8) = .empty,
        next_task_request_id: u64 = 1,
        next_interval_token: u64 = 1,
        next_elem_id: u64 = 0,
        roc_host: ?*abi.RocHost = null,
        root_elem: ?abi.Elem = null,
        last_runtime_metrics: Metrics = Ctx.zeroMetrics(),
        pending_roc_metrics: Metrics = Ctx.zeroMetrics(),
        // Render-command accumulator (patches/create/append/remove/...) folded into
        // last_runtime_metrics at finish. Engine-owned so both hosts share it.
        render_metrics: render.Metrics = .{},
        // Dispatch counters (events processed / recompute batches) folded into
        // last_runtime_metrics at finish. Engine-owned so both hosts share it.
        dispatch_metrics: DispatchMetrics = .{},
        dirty_signal_generation: u64 = 0,
        scratch: EngineScratch = .{},

        const ActiveSignalGraphLifecycle = struct {
            engine: *Self,
            ctx: Ctx.Handle,

            pub fn ensureInterval(self: *@This(), source_token: HostSignalToken, period_ms: u64) void {
                self.engine.ensureActiveInterval(self.ctx, source_token, period_ms);
            }

            pub fn removeInterval(self: *@This(), source_token: HostSignalToken) void {
                self.engine.removeActiveIntervalBySourceToken(self.ctx, source_token);
            }

            pub fn releaseRecord(self: *@This(), record: *HostSignalRecord) void {
                record.release(Ctx.allocator(self.ctx), self.ctx, self.engine.roc_host.?, &self.engine.pending_roc_metrics);
            }
        };

        const EachRowScopeKeyLookup = struct {
            engine: *Self,

            pub fn rowKeyHash(self: *@This(), scope_id: u64) u64 {
                return self.engine.eachRowScopeKeyHash(scope_id);
            }
        };

        const ScopeIdentityDeactivation = struct {
            engine: *Self,
            ctx: Ctx.Handle,
            roc_host: *abi.RocHost,

            pub fn deactivateNode(self: *@This(), node_id: u64) void {
                self.engine.deactivateState(self.ctx, self.roc_host, node_id);
            }
        };

        pub fn init() Self {
            return .{};
        }

        pub fn deinitScratch(self: *Self, ctx: Ctx.Handle) void {
            self.scratch.deinit(Ctx.allocator(ctx));
        }

        fn scratchBinderStack(self: *Self, allocator: std.mem.Allocator, base: []const HostBinderBinding) *std.ArrayListUnmanaged(HostBinderBinding) {
            if (self.scratch.binder_stack.items.len != 0) {
                @panic("engine binder scratch was already active");
            }
            self.scratch.binder_stack.appendSlice(allocator, base) catch @panic("out of memory");
            return &self.scratch.binder_stack;
        }

        fn debugPhase(ctx: Ctx.Handle, phase: u32) void {
            if (comptime @hasDecl(Ctx, "debugPhase")) {
                Ctx.debugPhase(ctx, phase);
            }
        }

        fn pushCapabilities(ctx: Ctx.Handle, caps: []const HostValueCapability) void {
            Ctx.pushHostValueCapabilities(ctx, caps);
        }

        fn popCapabilities(ctx: Ctx.Handle) void {
            Ctx.popHostValueCapabilities(ctx);
        }

        fn callHostValueToUnitWithCapability(ctx: Ctx.Handle, roc_host: *abi.RocHost, cap: HostValueCapability, callable: abi.RocErasedCallable, value: HostValue) void {
            const caps = [_]HostValueCapability{cap};
            pushCapabilities(ctx, &caps);
            defer popCapabilities(ctx);
            erased_calls.callErasedHostValueToUnit(roc_host, callable, value);
        }

        fn callHostValueToHostValueWithCapability(ctx: Ctx.Handle, roc_host: *abi.RocHost, cap: HostValueCapability, callable: abi.RocErasedCallable, value: HostValue) HostValue {
            const caps = [_]HostValueCapability{cap};
            pushCapabilities(ctx, &caps);
            defer popCapabilities(ctx);
            return erased_calls.callErasedHostValueToHostValue(roc_host, callable, value);
        }

        fn callHostValueToStartTaskCmdWithCapability(ctx: Ctx.Handle, roc_host: *abi.RocHost, cap: HostValueCapability, callable: abi.RocErasedCallable, value: HostValue) erased_calls.StartTaskCmd {
            const caps = [_]HostValueCapability{cap};
            pushCapabilities(ctx, &caps);
            defer popCapabilities(ctx);
            return erased_calls.callErasedHostValueToStartTaskCmd(roc_host, callable, value);
        }

        fn callHostValueToStrWithCapability(ctx: Ctx.Handle, roc_host: *abi.RocHost, cap: HostValueCapability, callable: abi.RocErasedCallable, value: HostValue) abi.RocStr {
            const caps = [_]HostValueCapability{cap};
            pushCapabilities(ctx, &caps);
            defer popCapabilities(ctx);
            return erased_calls.callErasedHostValueToStr(roc_host, callable, value);
        }

        fn callHostValueToBoolWithCapability(ctx: Ctx.Handle, roc_host: *abi.RocHost, cap: HostValueCapability, callable: abi.RocErasedCallable, value: HostValue) bool {
            const caps = [_]HostValueCapability{cap};
            pushCapabilities(ctx, &caps);
            defer popCapabilities(ctx);
            return erased_calls.callErasedHostValueToBool(roc_host, callable, value);
        }

        fn callHostValueToHostValueListWithCapability(ctx: Ctx.Handle, roc_host: *abi.RocHost, cap: HostValueCapability, callable: abi.RocErasedCallable, value: HostValue) HostValueList {
            const caps = [_]HostValueCapability{cap};
            pushCapabilities(ctx, &caps);
            defer popCapabilities(ctx);
            return erased_calls.callErasedHostValueToHostValueList(roc_host, callable, value);
        }

        fn callHostValueListToHostValueWithCapability(ctx: Ctx.Handle, roc_host: *abi.RocHost, cap: HostValueCapability, callable: abi.RocErasedCallable, value: HostValueList) HostValue {
            const caps = [_]HostValueCapability{cap};
            pushCapabilities(ctx, &caps);
            defer popCapabilities(ctx);
            return erased_calls.callErasedHostValueListToHostValue(roc_host, callable, value);
        }

        fn callHostValueHostValueToBoolWithCapability(ctx: Ctx.Handle, roc_host: *abi.RocHost, cap: HostValueCapability, callable: abi.RocErasedCallable, left: HostValue, right: HostValue) bool {
            const caps = [_]HostValueCapability{cap};
            pushCapabilities(ctx, &caps);
            defer popCapabilities(ctx);
            return erased_calls.callErasedHostValueHostValueToBool(roc_host, callable, left, right);
        }

        fn callHostValueHostValueToHostValueWithCapabilities(ctx: Ctx.Handle, roc_host: *abi.RocHost, left_cap: HostValueCapability, right_cap: HostValueCapability, callable: abi.RocErasedCallable, left: HostValue, right: HostValue) HostValue {
            const caps = [_]HostValueCapability{ left_cap, right_cap };
            pushCapabilities(ctx, &caps);
            defer popCapabilities(ctx);
            return erased_calls.callErasedHostValueHostValueToHostValue(roc_host, callable, left, right);
        }

        fn callHostValueHostValueToElemWithCapabilities(ctx: Ctx.Handle, roc_host: *abi.RocHost, left_cap: HostValueCapability, right_cap: HostValueCapability, callable: abi.RocErasedCallable, left: HostValue, right: HostValue) abi.Elem {
            const caps = [_]HostValueCapability{ left_cap, right_cap };
            pushCapabilities(ctx, &caps);
            defer popCapabilities(ctx);
            return erased_calls.callErasedHostValueHostValueToElem(roc_host, callable, left, right);
        }

        pub fn recordDispatch(self: *Self) void {
            if (comptime !enable_runtime_metrics) return;
            self.dispatch_metrics.events_processed += 1;
            self.dispatch_metrics.recompute_batches += 1;
        }

        pub fn recordStreamNodesScanned(self: *Self, count: usize) void {
            self.pending_roc_metrics.bump(.stream_nodes_scanned, @intCast(count));
        }

        pub fn recordStreamNodesScannedBy(self: *Self, comptime field: RuntimeMetrics.Field, count: usize) void {
            var metrics = self.pending_roc_metrics;
            metrics.bump(.stream_nodes_scanned, @intCast(count));
            metrics.bump(field, @intCast(count));
            self.pending_roc_metrics = metrics;
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

        pub fn recordEachKeyHash(self: *Self) void {
            var metrics = self.pending_roc_metrics;
            metrics.bump(.each_key_compares, 1);
            metrics.bump(.each_key_hashes, 1);
            self.pending_roc_metrics = metrics;
        }

        pub fn recordEachKeyReuseCompare(self: *Self) void {
            var metrics = self.pending_roc_metrics;
            metrics.bump(.each_key_compares, 1);
            metrics.bump(.each_key_reuse_compares, 1);
            self.pending_roc_metrics = metrics;
        }

        pub fn recordEachKeyDuplicateCompare(self: *Self) void {
            var metrics = self.pending_roc_metrics;
            metrics.bump(.each_key_compares, 1);
            metrics.bump(.each_key_duplicate_compares, 1);
            self.pending_roc_metrics = metrics;
        }

        pub fn recordEachItemCompare(self: *Self) void {
            var metrics = self.pending_roc_metrics;
            metrics.bump(.each_item_compares, 1);
            self.pending_roc_metrics = metrics;
        }

        pub fn recordEachSync(self: *Self, key_count: usize, existing_count: usize) void {
            var metrics = self.pending_roc_metrics;
            metrics.bump(.each_syncs, 1);
            metrics.bump(.each_sync_keys, @intCast(key_count));
            metrics.bump(.each_sync_existing_rows, @intCast(existing_count));
            self.pending_roc_metrics = metrics;
        }

        pub fn deinitRenderCache(self: *Self, ctx: Ctx.Handle) void {
            self.render_cache.deinit(ctx);
        }

        pub fn hasRenderRoot(self: *const Self) bool {
            return self.render_cache.hasRoot();
        }

        pub fn resetRenderTree(self: *Self, ctx: Ctx.Handle) void {
            self.render_cache.reset(ctx);
        }

        pub fn appendRenderNode(self: *Self, ctx: Ctx.Handle, elem_id: u64, parent_elem_id: u64, tag: []const u8) void {
            self.render_cache.appendNode(ctx, elem_id, parent_elem_id, tag);
        }

        pub fn ensureRenderNode(self: *Self, ctx: Ctx.Handle, elem_id: u64, tag: []const u8, counts: *render.Counts) void {
            self.render_cache.ensureNode(ctx, elem_id, tag, counts);
        }

        pub fn removeRenderNode(self: *Self, ctx: Ctx.Handle, elem_id: u64, counts: *render.Counts) void {
            self.render_cache.removeNode(ctx, elem_id, counts);
        }

        pub fn replaceRenderChildren(self: *Self, ctx: Ctx.Handle, parent_elem_id: u64, next_child_ids: []const u64, counts: *render.Counts) void {
            self.render_cache.replaceChildren(ctx, parent_elem_id, next_child_ids, counts);
        }

        pub fn replaceRenderChildrenForMoves(self: *Self, ctx: Ctx.Handle, parent_elem_id: u64, next_child_ids: []const u64, counts: *render.Counts) void {
            self.render_cache.replaceChildrenForMoves(ctx, parent_elem_id, next_child_ids, counts);
        }

        pub fn applyRenderEventBinding(self: *Self, ctx: Ctx.Handle, elem_id: u64, kind: RenderEventKind, binding: ?HostRequiredEventBinding, counts: *render.Counts) void {
            const cache_binding: ?render_cache_mod.EventBinding = if (binding) |payload| .{
                .event_id = payload.event_id,
                .payload_accessor = payload.payload_accessor,
            } else null;
            self.render_cache.applyEventBinding(ctx, elem_id, kind, cache_binding, counts);
        }

        pub fn applyRenderNamedEventBinding(self: *Self, ctx: Ctx.Handle, elem_id: u64, name: []const u8, binding: ?HostRequiredNamedEventBinding, counts: *render.Counts) void {
            const cache_binding: ?render_cache_mod.NamedEventBinding = if (binding) |payload| .{
                .event_id = payload.event_id,
                .options = payload.options,
                .payload_kind = payload.payload_kind,
                .payload_accessor = payload.payload_accessor,
            } else null;
            self.render_cache.applyNamedEventBinding(ctx, elem_id, name, cache_binding, counts);
        }

        fn descriptorStreamNodeTag(stream: *const HostNodeDescriptorStream, node: HostRenderNode) []const u8 {
            const descriptor_index = stream.elemDescriptorIndex(node.elem_id) orelse @panic("render node had no descriptor index");
            return switch (node.kind) {
                .element => blk: {
                    const index = descriptor_index.element orelse @panic("element render node had no element descriptor");
                    if (index >= stream.elements.items.len) @panic("element descriptor index exceeded descriptor table");
                    const desc = stream.elements.items[index];
                    if (desc.elem_id != node.elem_id) @panic("element descriptor index pointed at the wrong elem id");
                    break :blk desc.tag;
                },
                .text, .signal_text => "text",
            };
        }

        fn descriptorStreamNodeParent(stream: *const HostNodeDescriptorStream, node: HostRenderNode) u64 {
            const descriptor_index = stream.elemDescriptorIndex(node.elem_id) orelse @panic("render node had no descriptor index");
            return switch (node.kind) {
                .element => blk: {
                    const index = descriptor_index.element orelse @panic("element render node had no element descriptor");
                    if (index >= stream.elements.items.len) @panic("element descriptor index exceeded descriptor table");
                    const desc = stream.elements.items[index];
                    if (desc.elem_id != node.elem_id) @panic("element descriptor index pointed at the wrong elem id");
                    break :blk desc.parent_elem_id;
                },
                .text => blk: {
                    const index = descriptor_index.text_node orelse @panic("text render node had no text descriptor");
                    if (index >= stream.text_nodes.items.len) @panic("text descriptor index exceeded descriptor table");
                    const desc = stream.text_nodes.items[index];
                    if (desc.elem_id != node.elem_id) @panic("text descriptor index pointed at the wrong elem id");
                    break :blk desc.parent_elem_id;
                },
                .signal_text => blk: {
                    const index = descriptor_index.signal_text_node orelse @panic("signal text render node had no signal text descriptor");
                    if (index >= stream.signal_text_nodes.items.len) @panic("signal text descriptor index exceeded descriptor table");
                    const desc = stream.signal_text_nodes.items[index];
                    if (desc.elem_id != node.elem_id) @panic("signal text descriptor index pointed at the wrong elem id");
                    break :blk desc.parent_elem_id;
                },
            };
        }

        pub fn debugAssertRenderCacheMatchesStream(self: *Self, ctx: Ctx.Handle, stream: *const HostNodeDescriptorStream) void {
            if (comptime builtin.mode != .Debug) return;

            const allocator = Ctx.allocator(ctx);
            self.scratch.debug_seen_render_nodes.resize(allocator, self.render_cache.nodes.items.len) catch @panic("out of memory");
            defer self.scratch.debug_seen_render_nodes.clearRetainingCapacity();
            const seen = self.scratch.debug_seen_render_nodes.items;
            @memset(seen, false);
            if (seen.len != 0) seen[0] = true;

            for (stream.render_nodes.items) |node| {
                const index: usize = @intCast(node.elem_id);
                if (index >= self.render_cache.nodes.items.len) @panic("descriptor stream referenced render cache node outside table");
                const cached = &self.render_cache.nodes.items[index];
                if (!cached.active) @panic("descriptor stream referenced inactive render cache node");
                if (cached.tag == null or !std.mem.eql(u8, cached.tag.?, descriptorStreamNodeTag(stream, node))) {
                    @panic("descriptor stream tag disagreed with render cache");
                }
                const parent_id = descriptorStreamNodeParent(stream, node);
                if (cached.parent_id == null or cached.parent_id.? != parent_id) {
                    const indexed_children = streamDirectChildren(allocator, stream, parent_id);
                    defer allocator.free(indexed_children);
                    const cache_child_count = if (parent_id < self.render_cache.nodes.items.len and self.render_cache.nodes.items[@intCast(parent_id)].active)
                        self.render_cache.nodes.items[@intCast(parent_id)].children.items.len
                    else
                        0;
                    var message: [160]u8 = undefined;
                    const rendered = std.fmt.bufPrint(
                        &message,
                        "descriptor stream parent disagreed for elem {d}: stream parent {d}, cache parent {?d}, indexed children {d}, cache children {d}",
                        .{ node.elem_id, parent_id, cached.parent_id, indexed_children.len, cache_child_count },
                    ) catch "descriptor stream parent disagreed with render cache";
                    @panic(rendered);
                }
                seen[index] = true;
            }

            for (self.render_cache.nodes.items, 0..) |cached, index| {
                if (index == 0 or !cached.active) continue;
                if (index >= seen.len or !seen[index]) @panic("active render cache node was absent from descriptor stream");
            }

            for (self.render_cache.nodes.items, 0..) |cached, parent_id| {
                if (!cached.active) continue;
                const expected_children = &self.scratch.debug_expected_children;
                expected_children.clearRetainingCapacity();
                for (stream.render_nodes.items) |node| {
                    if (descriptorStreamNodeParent(stream, node) == parent_id) {
                        expected_children.append(allocator, node.elem_id) catch @panic("out of memory");
                    }
                }
                const indexed_children = streamDirectChildren(allocator, stream, @intCast(parent_id));
                defer allocator.free(indexed_children);
                if (!std.mem.eql(u64, indexed_children, expected_children.items)) {
                    var message: [160]u8 = undefined;
                    const rendered = std.fmt.bufPrint(
                        &message,
                        "descriptor stream child index disagreed with render order for parent {d}",
                        .{parent_id},
                    ) catch "descriptor stream child index disagreed with render order";
                    @panic(rendered);
                }
                if (!std.mem.eql(u64, cached.children.items, expected_children.items)) {
                    @panic("descriptor stream child order disagreed with render cache");
                }
            }
            self.scratch.debug_expected_children.clearRetainingCapacity();
        }

        pub fn debugAssertRenderCacheMatchesSink(self: *Self, ctx: Ctx.Handle) void {
            self.render_cache.debugAssertMatchesSink(ctx);
        }

        pub fn applyRenderTextField(self: *Self, ctx: Ctx.Handle, elem_id: u64, field: RenderTextField, value: []const u8) bool {
            return self.render_cache.applyTextField(ctx, elem_id, field, value);
        }

        pub fn applyRenderTextAttr(self: *Self, ctx: Ctx.Handle, elem_id: u64, name: []const u8, value: []const u8) bool {
            return self.render_cache.applyTextAttr(ctx, elem_id, name, value);
        }

        pub fn applyRenderBoolField(self: *Self, ctx: Ctx.Handle, elem_id: u64, field: RenderBoolField, value: bool) bool {
            return self.render_cache.applyBoolField(ctx, elem_id, field, value);
        }

        pub fn clearRenderTextField(self: *Self, ctx: Ctx.Handle, elem_id: u64, field: RenderTextField) bool {
            return self.render_cache.clearTextField(ctx, elem_id, field);
        }

        pub fn clearRenderTextAttr(self: *Self, ctx: Ctx.Handle, elem_id: u64, name: []const u8) bool {
            return self.render_cache.clearTextAttr(ctx, elem_id, name);
        }

        pub fn clearRenderBoolField(self: *Self, ctx: Ctx.Handle, elem_id: u64, field: RenderBoolField) bool {
            return self.render_cache.clearBoolField(ctx, elem_id, field);
        }

        pub fn clearEventDescriptors(self: *Self) void {
            self.event_descriptors.items.len = 0;
        }

        pub fn deinitActiveEventDesc(self: *Self, roc_host: *abi.RocHost, desc: ActiveEventDesc) void {
            releaseHostEventReducer(desc.payload_reducer, roc_host, &self.pending_roc_metrics);
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

        pub fn clearSignalCache(self: *Self, ctx: Ctx.Handle) RocHostRequiredError!void {
            const roc_host = self.roc_host orelse {
                if (self.signal_cache.items.len != 0) return RocHostRequiredError.MissingRocHost;
                return;
            };
            for (self.signal_cache.items) |*slot| {
                slot.deinit(ctx, roc_host, &self.pending_roc_metrics);
            }
            self.signal_cache.items.len = 0;
        }

        pub fn clearStates(self: *Self, ctx: Ctx.Handle) RocHostRequiredError!void {
            const roc_host = self.roc_host orelse {
                for (self.states.items) |state| {
                    if (state.active) return RocHostRequiredError.MissingRocHost;
                }
                self.states.items.len = 0;
                self.state_indexes_by_node_id.items.len = 0;
                return;
            };
            for (self.states.items) |*state| {
                if (!state.active) continue;
                state.cell.deinit(ctx, roc_host, &self.pending_roc_metrics);
                state.active = false;
            }
            self.states.items.len = 0;
            self.state_indexes_by_node_id.items.len = 0;
        }

        fn ensureStateIndexSlot(self: *Self, ctx: Ctx.Handle, node_id: u64) *?usize {
            const allocator = Ctx.allocator(ctx);
            const index: usize = @intCast(node_id);
            while (self.state_indexes_by_node_id.items.len <= index) {
                self.state_indexes_by_node_id.append(allocator, null) catch @panic("out of memory");
            }
            return &self.state_indexes_by_node_id.items[index];
        }

        fn recordStateCellIndex(self: *Self, ctx: Ctx.Handle, node_id: u64, index: usize) void {
            const slot = self.ensureStateIndexSlot(ctx, node_id);
            if (slot.* != null) @panic("state cell index already existed for node id");
            slot.* = index;
        }

        fn clearStateCellIndex(self: *Self, node_id: u64, expected: usize) void {
            if (node_id >= self.state_indexes_by_node_id.items.len) @panic("state cell index clear referenced an unknown node id");
            const slot = &self.state_indexes_by_node_id.items[@intCast(node_id)];
            const existing = slot.* orelse @panic("state cell index clear missed its node id");
            if (existing != expected) @panic("state cell index clear referenced the wrong state index");
            slot.* = null;
        }

        fn clearEachRowSites(self: *Self, allocator: std.mem.Allocator) void {
            each_runtime.clearSites(allocator, &self.each_row_sites, &self.each_row_site_indexes, &self.each_row_memberships_by_scope_id);
        }

        fn ensureEachRowSiteIndex(self: *Self, allocator: std.mem.Allocator, parent_scope_id: u64, site_ordinal: u64) usize {
            return each_runtime.ensureSiteIndex(allocator, &self.each_row_sites, &self.each_row_site_indexes, parent_scope_id, site_ordinal);
        }

        fn activeEachRowSiteIndex(self: *Self, parent_scope_id: u64, site_ordinal: u64) ?usize {
            return each_runtime.activeSiteIndex(&self.each_row_site_indexes, parent_scope_id, site_ordinal);
        }

        fn appendEachRowToSiteIndex(self: *Self, allocator: std.mem.Allocator, site_index: usize, scope_id: u64, key_hash: u64) void {
            each_runtime.appendRowToSiteIndex(allocator, &self.each_row_sites, &self.each_row_memberships_by_scope_id, site_index, scope_id, key_hash);
        }

        fn removeEachRowFromSiteIndex(self: *Self, scope_id: u64, key_hash: u64) void {
            var row_keys = EachRowScopeKeyLookup{ .engine = self };
            each_runtime.removeRowFromSiteIndex(&self.each_row_sites, &self.each_row_memberships_by_scope_id, scope_id, key_hash, &row_keys);
        }

        fn replaceEachRowSiteRows(self: *Self, allocator: std.mem.Allocator, site_index: usize, scope_ids: []const u64) void {
            var row_keys = EachRowScopeKeyLookup{ .engine = self };
            each_runtime.replaceSiteRows(allocator, &self.each_row_sites, &self.each_row_memberships_by_scope_id, site_index, scope_ids, &row_keys);
        }

        pub fn deactivateState(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, node_id: u64) void {
            const state_index = self.stateIndexByNodeId(node_id) orelse return;
            const state = &self.states.items[state_index];
            state.cell.deinit(ctx, roc_host, &self.pending_roc_metrics);
            state.active = false;
            self.clearStateCellIndex(node_id, state_index);
        }

        pub fn clearScopes(self: *Self, ctx: Ctx.Handle) RocHostRequiredError!void {
            if (self.roc_host) |roc_host| {
                for (self.scopes.items) |*scope| {
                    if (!scope.active) continue;
                    deinitHostScopeStep(&scope.step, ctx, roc_host, &self.pending_roc_metrics);
                }
            } else if (self.scopes.items.len != 0) {
                return RocHostRequiredError.MissingRocHost;
            }
            self.scopes.items.len = 0;
            self.clearEachRowSites(Ctx.allocator(ctx));
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

        pub fn activeIntervalRecordByToken(self: *Self, source_token: HostSignalToken) ?*HostSignalRecord {
            var found: ?*HostSignalRecord = null;
            for (self.active_signal_graph.items) |node| {
                switch (node.record.payload) {
                    .interval_source => |payload| {
                        if (payload.token != source_token) continue;
                        if (found != null) @panic("interval token matched more than one active interval source");
                        found = node.record;
                    },
                    .ref, .const_value, .map, .map2, .combine, .task_source => {},
                }
            }
            return found;
        }

        pub fn activeIntervalSourceTokenByRuntimeToken(self: *Self, token: u64) ?HostSignalToken {
            var found: ?HostSignalToken = null;
            for (self.active_intervals.items) |interval| {
                if (!interval.active or interval.token != token) continue;
                if (found != null) @panic("runtime interval token matched more than one active interval");
                found = interval.source_token;
            }
            return found;
        }

        pub fn pendingTaskCountByName(self: *const Self, name: []const u8) u64 {
            var count: u64 = 0;
            for (self.pending_tasks.items) |task| {
                if (task.active and std.mem.eql(u8, task.task_name, name)) count += 1;
            }
            return count;
        }

        pub fn pendingTaskIndexByRequestId(self: *Self, request_id: u64) ?usize {
            var found: ?usize = null;
            for (self.pending_tasks.items, 0..) |task, index| {
                if (!task.active or task.request_id != request_id) continue;
                if (found != null) @panic("task request id matched more than one pending request");
                found = index;
            }
            return found;
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
            return active_graph.rank(HostSignalRecord, self.active_signal_graph.items, record_id);
        }

        pub fn dependentActiveSignalRecordIds(self: *Self, record_id: u64) []const u64 {
            return active_graph.dependentIds(HostSignalRecord, self.active_signal_graph.items, record_id);
        }

        pub fn appendActiveSignalAndDependents(self: *Self, allocator: std.mem.Allocator, record_ids: *std.ArrayListUnmanaged(u64), record_id: u64) void {
            active_graph.appendAndDependents(HostSignalRecord, allocator, self.active_signal_graph.items, record_ids, record_id);
        }

        pub fn sortActiveSignalRecordIdsByRank(self: *Self, record_ids: []u64) void {
            active_graph.sortRecordIdsByRank(HostSignalRecord, self.active_signal_graph.items, record_ids);
        }

        pub fn dirtyActiveSignalRecordIdsForSources(self: *Self, allocator: std.mem.Allocator, dirty_source_node_ids: []const u64) []u64 {
            return active_graph.dirtyRecordIdsForSources(
                HostSignalRecord,
                allocator,
                self.active_signal_graph.items,
                self.active_source_signal_routes.items,
                dirty_source_node_ids,
            );
        }

        pub fn dirtyActiveSignalRecordIdsForRoots(self: *Self, allocator: std.mem.Allocator, root_record_ids: []const u64) []u64 {
            return active_graph.dirtyRecordIdsForRoots(HostSignalRecord, allocator, self.active_signal_graph.items, root_record_ids);
        }

        pub fn recordDerivedCall(self: *Self) void {
            var metrics = self.pending_roc_metrics;
            metrics.bump(.derived_calls_into_roc, 1);
            self.pending_roc_metrics = metrics;
        }

        pub fn recordSignalPrune(self: *Self) void {
            var metrics = self.pending_roc_metrics;
            metrics.bump(.propagation_prunes, 1);
            self.pending_roc_metrics = metrics;
        }

        pub fn cloneCachedSignalValue(self: *Self, ctx: Ctx.Handle, cache_slot: *const HostSignalCacheSlot) HostValue {
            _ = self;
            debugPhase(ctx, 409);
            return switch (cache_slot.*) {
                .absent => @panic("cached signal expression value was requested before initialization"),
                .present => |cached| Ctx.cloneHostValue(ctx, cached.value),
            };
        }

        pub fn updateDirtySignalExprCache(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, cache_slot: *HostSignalCacheSlot, value: HostValue, cap: HostValueCapability) HostSignalEvalResult {
            switch (cache_slot.*) {
                .absent => {
                    debugPhase(ctx, 450);
                    return .{
                        .value = self.replaceSignalExprCacheAndClone(ctx, cache_slot, roc_host, value, cap),
                        .changed = true,
                    };
                },
                .present => |*cached| {
                    debugPhase(ctx, 451);
                    const values_equal = cached.valueEquals(ctx, roc_host, value);
                    if (values_equal) {
                        debugPhase(ctx, 452);
                        cached.dropIncoming(ctx, roc_host, value);
                        self.recordSignalPrune();
                        debugPhase(ctx, 453);
                        return .{ .value = Ctx.cloneHostValue(ctx, cached.value), .changed = false };
                    }

                    debugPhase(ctx, 454);
                    cached.replaceValue(ctx, roc_host, value);
                    debugPhase(ctx, 455);
                    return .{ .value = Ctx.cloneHostValue(ctx, cached.value), .changed = true };
                },
            }
        }

        pub fn updateDirtySignalCache(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, cache_slot: *HostSignalCacheSlot, value: HostValue, cap: HostValueCapability) bool {
            switch (cache_slot.*) {
                .absent => {
                    cache_slot.replace(ctx, roc_host, &self.pending_roc_metrics, value, cap);
                    return true;
                },
                .present => |*cached| {
                    const values_equal = cached.valueEquals(ctx, roc_host, value);
                    if (values_equal) {
                        cached.dropIncoming(ctx, roc_host, value);
                        self.recordSignalPrune();
                        return false;
                    }

                    cache_slot.replaceValue(ctx, roc_host, value);
                    return true;
                },
            }
        }

        pub fn cloneMemoizedDirtySignalResult(self: *Self, ctx: Ctx.Handle, record: *HostSignalRecord, dirty_generation: u64) ?HostSignalEvalResult {
            if (record.last_dirty_generation != dirty_generation) return null;

            return switch (record.payload) {
                .ref => null,
                .const_value => |*payload| .{
                    .value = self.cloneCachedSignalValue(ctx, &payload.cached_value),
                    .changed = record.last_dirty_changed,
                },
                .map => |*payload| .{
                    .value = self.cloneCachedSignalValue(ctx, &payload.cached_value),
                    .changed = record.last_dirty_changed,
                },
                .map2 => |*payload| .{
                    .value = self.cloneCachedSignalValue(ctx, &payload.cached_value),
                    .changed = record.last_dirty_changed,
                },
                .combine => |*payload| .{
                    .value = self.cloneCachedSignalValue(ctx, &payload.cached_value),
                    .changed = record.last_dirty_changed,
                },
                .task_source => |*payload| .{
                    .value = self.cloneCachedSignalValue(ctx, &payload.cached_value),
                    .changed = record.last_dirty_changed,
                },
                .interval_source => |*payload| .{
                    .value = self.cloneCachedSignalValue(ctx, &payload.cached_value),
                    .changed = record.last_dirty_changed,
                },
            };
        }

        pub fn rememberDirtySignalResult(_: *Self, record: *HostSignalRecord, dirty_generation: u64, result: HostSignalEvalResult) HostSignalEvalResult {
            record.last_dirty_generation = dirty_generation;
            record.last_dirty_changed = result.changed;
            return result;
        }

        pub fn hostSignalRecordCapability(_: *Self, ctx: Ctx.Handle, record: *const HostSignalRecord) HostValueCapability {
            return switch (record.payload) {
                .ref => |node_id| Ctx.stateCapability(ctx, node_id),
                .const_value => |payload| payload.cap,
                .map => |payload| payload.cap,
                .map2 => |payload| payload.cap,
                .combine => |payload| payload.cap,
                .task_source => |payload| payload.cap,
                .interval_source => |payload| payload.cap,
            };
        }

        pub fn hostSignalBindingCapability(self: *Self, ctx: Ctx.Handle, signal: *const HostSignalBinding) HostValueCapability {
            return self.hostSignalRecordCapability(ctx, signal.record);
        }

        pub fn dropHostSignalRecordValue(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, record: *const HostSignalRecord, value: HostValue) void {
            const cap = self.hostSignalRecordCapability(ctx, record);
            callHostValueToUnitWithCapability(ctx, roc_host, cap, hv.hostValueCapabilityDrop(cap), value);
        }

        pub fn ensureStateFromDesc(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, desc: HostNodeStateDesc) void {
            if (self.stateIndexByNodeId(desc.node_id) != null) return;

            const initial = erased_calls.callValueInitThunk(roc_host, desc.initial);
            var cell = HostValueCell.initRetained(initial, desc.cap, &self.pending_roc_metrics);
            const state_index = self.states.items.len;
            self.states.append(Ctx.allocator(ctx), .{
                .state_id = desc.node_id,
                .cell = cell,
                .version = 0,
                .active = true,
            }) catch {
                cell.deinit(ctx, roc_host, &self.pending_roc_metrics);
                @panic("out of memory");
            };
            self.recordStateCellIndex(ctx, desc.node_id, state_index);
        }

        fn signalRecordByTokenForStream(self: *Self, stream: *HostNodeDescriptorStream, token: HostSignalToken) ?*HostSignalRecord {
            if (stream.signalRecordByToken(token)) |record| return record;
            if (stream != &self.active_stream) return self.active_stream.signalRecordByToken(token);
            return null;
        }

        fn retainExistingSignalRecordForStream(self: *Self, allocator: std.mem.Allocator, stream: *HostNodeDescriptorStream, token: HostSignalToken, expected_tag: std.meta.Tag(HostSignalRecordPayload)) ?*HostSignalRecord {
            const record = self.signalRecordByTokenForStream(stream, token) orelse return null;
            validateExistingSignalRecord(record, expected_tag);
            stream.rememberSignalRecord(allocator, record);
            return record.retain();
        }

        pub fn bindNodeSignalExpr(self: *Self, allocator: std.mem.Allocator, stream: *HostNodeDescriptorStream, expr: abi.NodeSignalExpr, binder_stack: []const HostBinderBinding) *HostSignalRecord {
            return switch (expr.tag) {
                .Ref => blk: {
                    const node_id = resolveNodeBinderRef(binder_stack, expr.payload_ref());
                    break :blk HostSignalRecord.init(allocator, .{ .ref = node_id });
                },
                .ConstValue => blk: {
                    const payload = expr.payload_const_value();
                    const token = payload._0;
                    if (self.retainExistingSignalRecordForStream(allocator, stream, token, .const_value)) |record| {
                        break :blk record;
                    }

                    const record = HostSignalRecord.init(allocator, .{ .const_value = .{
                        .token = retainHostSignalToken(token),
                        .init = retainHostCallable(payload._1, &self.pending_roc_metrics),
                        .cap = retainHostValueCapability(payload._2, &self.pending_roc_metrics),
                    } });
                    stream.rememberSignalRecord(allocator, record);
                    break :blk record;
                },
                .Map => blk: {
                    const payload = expr.payload_map();
                    const token = payload._0;
                    if (self.retainExistingSignalRecordForStream(allocator, stream, token, .map)) |record| {
                        break :blk record;
                    }

                    const input = self.bindNodeSignalExpr(allocator, stream, payload._1.*, binder_stack);
                    const record = HostSignalRecord.init(allocator, .{ .map = .{
                        .token = retainHostSignalToken(token),
                        .input = input,
                        .transform = retainHostCallable(payload._2, &self.pending_roc_metrics),
                        .cap = retainHostValueCapability(payload._3, &self.pending_roc_metrics),
                    } });
                    stream.rememberSignalRecord(allocator, record);
                    break :blk record;
                },
                .Map2 => blk: {
                    const payload = expr.payload_map2();
                    const token = payload._0;
                    if (self.retainExistingSignalRecordForStream(allocator, stream, token, .map2)) |record| {
                        break :blk record;
                    }

                    const left = self.bindNodeSignalExpr(allocator, stream, payload._1.*, binder_stack);
                    const right = self.bindNodeSignalExpr(allocator, stream, payload._2.*, binder_stack);
                    const record = HostSignalRecord.init(allocator, .{ .map2 = .{
                        .token = retainHostSignalToken(token),
                        .left = left,
                        .right = right,
                        .transform = retainHostCallable(payload._3, &self.pending_roc_metrics),
                        .cap = retainHostValueCapability(payload._4, &self.pending_roc_metrics),
                    } });
                    stream.rememberSignalRecord(allocator, record);
                    break :blk record;
                },
                .Combine => blk: {
                    const payload = expr.payload_combine();
                    const token = payload._0;
                    if (self.retainExistingSignalRecordForStream(allocator, stream, token, .combine)) |record| {
                        break :blk record;
                    }

                    const source_children = payload._1.items();
                    const children = allocator.alloc(*HostSignalRecord, source_children.len) catch @panic("out of memory");
                    for (source_children, children) |child, *dest| {
                        dest.* = self.bindNodeSignalExpr(allocator, stream, child, binder_stack);
                    }
                    const record = HostSignalRecord.init(allocator, .{ .combine = .{
                        .token = retainHostSignalToken(token),
                        .children = children,
                        .transform = retainHostCallable(payload._2, &self.pending_roc_metrics),
                        .cap = retainHostValueCapability(payload._3, &self.pending_roc_metrics),
                    } });
                    stream.rememberSignalRecord(allocator, record);
                    break :blk record;
                },
                .TaskSource => blk: {
                    const payload = expr.payload_task_source();
                    const token = payload.token;
                    if (self.retainExistingSignalRecordForStream(allocator, stream, token, .task_source)) |record| {
                        break :blk record;
                    }

                    const record = HostSignalRecord.init(allocator, .{ .task_source = .{
                        .token = retainHostSignalToken(token),
                        .name = allocator.dupe(u8, payload.name.asSlice()) catch @panic("out of memory"),
                        .payload_cap = retainHostValueCapability(payload.payload_cap, &self.pending_roc_metrics),
                        .initial = retainHostCallable(payload.initial, &self.pending_roc_metrics),
                        .done = retainHostCallable(payload.done, &self.pending_roc_metrics),
                        .failed = retainHostCallable(payload.failed, &self.pending_roc_metrics),
                        .cap = retainHostValueCapability(payload.cap, &self.pending_roc_metrics),
                        .reset_on_start = payload.reset_on_start,
                    } });
                    stream.rememberSignalRecord(allocator, record);
                    break :blk record;
                },
                .IntervalSource => blk: {
                    const payload = expr.payload_interval_source();
                    const token = payload.token;
                    if (self.retainExistingSignalRecordForStream(allocator, stream, token, .interval_source)) |record| {
                        break :blk record;
                    }

                    const record = HostSignalRecord.init(allocator, .{ .interval_source = .{
                        .token = retainHostSignalToken(token),
                        .period_ms = payload.period_ms,
                        .initial = retainHostCallable(payload.initial, &self.pending_roc_metrics),
                        .tick = retainHostCallable(payload.tick, &self.pending_roc_metrics),
                        .cap = retainHostValueCapability(payload.cap, &self.pending_roc_metrics),
                    } });
                    stream.rememberSignalRecord(allocator, record);
                    break :blk record;
                },
            };
        }

        pub fn bindNodeSignal(self: *Self, allocator: std.mem.Allocator, stream: *HostNodeDescriptorStream, expr: abi.NodeSignalExpr, binder_stack: []const HostBinderBinding) HostSignalBinding {
            const record = self.bindNodeSignalExpr(allocator, stream, expr, binder_stack);
            var source_node_ids: std.ArrayListUnmanaged(u64) = .empty;
            appendSignalRecordSourceNodeIds(allocator, &source_node_ids, record);
            return .{
                .record = record,
                .source_node_ids = source_node_ids.toOwnedSlice(allocator) catch @panic("out of memory"),
            };
        }

        pub fn streamNodeIdInScopeSubtree(self: *Self, previous: *const HostNodeDescriptorStream, node_id: u64, root_scope_id: u64) bool {
            const descriptor_index = previous.nodeDescriptorIndex(node_id) orelse return false;
            const ScopeSiteSlot = struct {
                kind: HostNodeScopeSiteKind,
                index: ?usize,
            };
            const scope_site_slots = [_]ScopeSiteSlot{
                .{ .kind = .component, .index = descriptor_index.scope_sites.component },
                .{ .kind = .state, .index = descriptor_index.scope_sites.state },
                .{ .kind = .when, .index = descriptor_index.scope_sites.when },
                .{ .kind = .each, .index = descriptor_index.scope_sites.each },
            };
            for (scope_site_slots) |slot| {
                const site_index = slot.index orelse continue;
                if (site_index >= previous.scope_sites.items.len) @panic("scope site descriptor index exceeded descriptor table");
                const site = previous.scope_sites.items[site_index];
                if (site.node_id != node_id or site.kind != slot.kind) @panic("scope site descriptor index pointed at the wrong node");
                if (self.scopeIsDescendantOrSelf(site.scope_id, root_scope_id) catch @panic("scope descriptor referenced an unknown parent scope")) return true;
            }
            return false;
        }

        pub fn renderNodeInScopeSubtree(self: *Self, stream: *const HostNodeDescriptorStream, node: HostRenderNode, root_scope_id: u64) bool {
            return self.scopeIsDescendantOrSelf(renderNodeScopeId(stream, node), root_scope_id) catch @panic("scope descriptor referenced an unknown parent scope");
        }

        pub fn firstRenderIndexInScopeSubtree(self: *Self, stream: *const HostNodeDescriptorStream, root_scope_id: u64) ?usize {
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_render_scope, stream.render_nodes.items.len);
            for (stream.render_nodes.items, 0..) |node, index| {
                if (self.renderNodeInScopeSubtree(stream, node, root_scope_id)) return index;
            }
            return null;
        }

        pub fn lastRenderEndIndexInScopeSubtree(self: *Self, stream: *const HostNodeDescriptorStream, root_scope_id: u64) ?usize {
            var end_index: ?usize = null;
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_render_scope, stream.render_nodes.items.len);
            for (stream.render_nodes.items, 0..) |node, index| {
                if (self.renderNodeInScopeSubtree(stream, node, root_scope_id)) {
                    end_index = index + 1;
                }
            }
            return end_index;
        }

        pub fn scopeSubtreeHasDirtyStructuralSource(self: *Self, previous: *const HostNodeDescriptorStream, root_scope_id: u64, dirty_source_node_ids: []const u64) bool {
            if (dirty_source_node_ids.len == 0) return false;

            self.recordStreamNodesScannedBy(.stream_nodes_scanned_dirty_scope, previous.whens.items.len);
            for (previous.whens.items) |desc| {
                if (!self.streamNodeIdInScopeSubtree(previous, desc.node_id, root_scope_id)) continue;
                if (sourceNodeIdsIntersect(desc.condition.source_node_ids, dirty_source_node_ids)) return true;
            }
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_dirty_scope, previous.eaches.items.len);
            for (previous.eaches.items) |desc| {
                if (!self.streamNodeIdInScopeSubtree(previous, desc.node_id, root_scope_id)) continue;
                if (sourceNodeIdsIntersect(desc.items.source_node_ids, dirty_source_node_ids)) return true;
            }
            return false;
        }

        pub fn cloneHostSignalCacheSlot(self: *Self, ctx: Ctx.Handle, slot: HostSignalCacheSlot, metrics: anytype) HostSignalCacheSlot {
            _ = self;
            return slot.cloneRetained(ctx, metrics);
        }

        pub fn copyActiveScopeSubtreeDescriptors(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, root_scope_id: u64) void {
            const allocator = Ctx.allocator(ctx);
            const previous = &self.active_stream;
            const previous_render_base = self.firstRenderIndexInScopeSubtree(previous, root_scope_id);
            const next_render_base = stream.render_nodes.items.len;
            var copied_elem_ids: std.ArrayListUnmanaged(u64) = .empty;
            defer copied_elem_ids.deinit(allocator);

            for (previous.render_nodes.items) |node| {
                const node_scope_id = renderNodeScopeId(previous, node);
                if (!(self.scopeIsDescendantOrSelf(node_scope_id, root_scope_id) catch @panic("scope descriptor referenced an unknown parent scope"))) continue;

                copied_elem_ids.append(allocator, node.elem_id) catch @panic("out of memory");
                switch (node.kind) {
                    .element => {
                        const desc = findElementDesc(previous, node.elem_id) orelse @panic("copyActiveScopeSubtreeDescriptors: render node has no matching descriptor");
                        _ = stream.appendElement(allocator, desc.elem_id, desc.parent_elem_id, desc.scope_id, desc.tag);
                    },
                    .text => {
                        const desc = findTextNodeDesc(previous, node.elem_id) orelse @panic("copyActiveScopeSubtreeDescriptors: render node has no matching descriptor");
                        stream.appendTextNode(allocator, desc.elem_id, desc.parent_elem_id, desc.scope_id, desc.value);
                    },
                    .signal_text => {
                        const desc = findSignalTextNodeDesc(previous, node.elem_id) orelse @panic("copyActiveScopeSubtreeDescriptors: render node has no matching descriptor");
                        const signal = desc.signal.cloneRetained(allocator, &self.pending_roc_metrics);
                        stream.appendSignalTextNode(allocator, ctx, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.parent_elem_id, desc.scope_id, signal, desc.read);
                        stream.signal_text_nodes.items[stream.signal_text_nodes.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(ctx, desc.cached_value, &self.pending_roc_metrics);
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
                stream.appendSignalTextAttr(allocator, ctx, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.field, signal, desc.read);
                stream.signal_text_attrs.items[stream.signal_text_attrs.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(ctx, desc.cached_value, &self.pending_roc_metrics);
            }
            for (previous.static_custom_text_attrs.items) |desc| {
                if (!u64SliceContains(copied_elem_ids.items, desc.elem_id)) continue;
                stream.appendStaticCustomTextAttr(allocator, desc.elem_id, desc.name, desc.value);
            }
            for (previous.signal_custom_text_attrs.items) |desc| {
                if (!u64SliceContains(copied_elem_ids.items, desc.elem_id)) continue;
                const signal = desc.signal.cloneRetained(allocator, &self.pending_roc_metrics);
                stream.appendSignalCustomTextAttr(allocator, ctx, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.name, signal, desc.read);
                stream.signal_custom_text_attrs.items[stream.signal_custom_text_attrs.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(ctx, desc.cached_value, &self.pending_roc_metrics);
            }
            for (previous.static_bool_attrs.items) |desc| {
                if (!u64SliceContains(copied_elem_ids.items, desc.elem_id)) continue;
                stream.appendStaticBoolAttr(allocator, desc.elem_id, desc.field, desc.value);
            }
            for (previous.signal_bool_attrs.items) |desc| {
                if (!u64SliceContains(copied_elem_ids.items, desc.elem_id)) continue;
                const signal = desc.signal.cloneRetained(allocator, &self.pending_roc_metrics);
                stream.appendSignalBoolAttr(allocator, ctx, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.field, signal, desc.read);
                stream.signal_bool_attrs.items[stream.signal_bool_attrs.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(ctx, desc.cached_value, &self.pending_roc_metrics);
            }
            for (previous.on_changes.items) |desc| {
                if (!(self.scopeIsDescendantOrSelf(desc.scope_id, root_scope_id) catch @panic("scope descriptor referenced an unknown parent scope"))) continue;
                const signal = desc.signal.cloneRetained(allocator, &self.pending_roc_metrics);
                stream.appendOnChange(allocator, ctx, roc_host, &self.pending_roc_metrics, desc.scope_id, signal, desc.to_cmd);
                stream.on_changes.items[stream.on_changes.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(ctx, desc.cached_value, &self.pending_roc_metrics);
            }
            for (previous.mounts.items) |desc| {
                if (!(self.scopeIsDescendantOrSelf(desc.scope_id, root_scope_id) catch @panic("scope descriptor referenced an unknown parent scope"))) continue;
                stream.appendMount(allocator, roc_host, &self.pending_roc_metrics, desc.scope_id, desc.to_cmd, false);
            }
            for (previous.cleanups.items) |desc| {
                if (!(self.scopeIsDescendantOrSelf(desc.scope_id, root_scope_id) catch @panic("scope descriptor referenced an unknown parent scope"))) continue;
                stream.appendCleanup(allocator, desc.scope_id, desc.name);
            }
            for (previous.events.items, 0..) |desc, event_index| {
                if (!u64SliceContains(copied_elem_ids.items, desc.elem_id)) continue;
                const payload_reducer = if (desc.owns_payload_reducer) desc.payload_reducer else self.activeEventReducerByIndex(event_index) catch @panic("active event table is missing a retained payload reducer");
                stream.appendEvent(allocator, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.kind, desc.binder_token, desc.target_node_id, desc.payload_kind, desc.payload_accessor, payload_reducer);
            }
            for (previous.named_events.items, 0..) |desc, named_event_index| {
                if (!u64SliceContains(copied_elem_ids.items, desc.elem_id)) continue;
                const active_event_index = previous.events.items.len + named_event_index;
                const payload_reducer = if (desc.owns_payload_reducer) desc.payload_reducer else self.activeEventReducerByIndex(active_event_index) catch @panic("active named event table is missing a retained payload reducer");
                stream.appendNamedEvent(allocator, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.name, desc.options, desc.binder_token, desc.target_node_id, desc.payload_kind, desc.payload_accessor, payload_reducer);
            }

            for (previous.scope_sites.items) |desc| {
                if (!(self.scopeIsDescendantOrSelf(desc.scope_id, root_scope_id) catch @panic("scope descriptor referenced an unknown parent scope"))) continue;
                const render_insert_index = if (previous_render_base) |render_base| blk: {
                    if (desc.render_insert_index < render_base) @panic("copied scope site insertion point precedes its scope subtree");
                    break :blk next_render_base + (desc.render_insert_index - render_base);
                } else next_render_base;
                stream.appendScopeSiteAt(allocator, desc.node_id, desc.scope_id, desc.ordinal, desc.parent_elem_id, render_insert_index, desc.kind, desc.binder_bindings);
            }
            for (previous.states.items) |desc| {
                if (!self.streamNodeIdInScopeSubtree(previous, desc.node_id, root_scope_id)) continue;
                stream.appendState(allocator, roc_host, &self.pending_roc_metrics, desc.node_id, desc.initial, desc.cap);
            }
            for (previous.whens.items) |desc| {
                if (!self.streamNodeIdInScopeSubtree(previous, desc.node_id, root_scope_id)) continue;
                const condition = desc.condition.cloneRetained(allocator, &self.pending_roc_metrics);
                stream.appendWhen(allocator, ctx, roc_host, &self.pending_roc_metrics, desc.node_id, condition, desc.read, desc.when_false, desc.when_true);
                stream.whens.items[stream.whens.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(ctx, desc.cached_value, &self.pending_roc_metrics);
            }
            for (previous.eaches.items) |desc| {
                if (!self.streamNodeIdInScopeSubtree(previous, desc.node_id, root_scope_id)) continue;
                const items = desc.items.cloneRetained(allocator, &self.pending_roc_metrics);
                stream.appendEach(allocator, ctx, roc_host, &self.pending_roc_metrics, desc.node_id, items, desc.ops);
                stream.eaches.items[stream.eaches.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(ctx, desc.cached_value, &self.pending_roc_metrics);
            }
        }

        pub fn deinitPendingTask(self: *Self, ctx: Ctx.Handle, task: *HostPendingTask) void {
            const allocator = Ctx.allocator(ctx);
            releaseHostSignalToken(task.task_token, self.roc_host.?);
            allocator.free(task.task_name);
            allocator.free(task.request);
            task.* = undefined;
        }

        pub fn cancelPendingTask(self: *Self, ctx: Ctx.Handle, task: *HostPendingTask) void {
            if (task.active) {
                Ctx.sink(ctx).cancelTask(task.request_id);
            }
            self.deinitPendingTask(ctx, task);
        }

        pub fn clearPendingTasks(self: *Self, ctx: Ctx.Handle) void {
            for (self.pending_tasks.items) |*task| {
                self.cancelPendingTask(ctx, task);
            }
            self.pending_tasks.items.len = 0;
        }

        pub fn cancelPendingTasksByTaskToken(self: *Self, ctx: Ctx.Handle, task_token: HostSignalToken) void {
            var index: usize = 0;
            while (index < self.pending_tasks.items.len) {
                if (!self.pending_tasks.items[index].active or self.pending_tasks.items[index].task_token != task_token) {
                    index += 1;
                    continue;
                }

                var task = self.removePendingTaskAt(index);
                self.cancelPendingTask(ctx, &task);
            }
        }

        pub fn cancelPendingTasksInScopeSubtree(self: *Self, ctx: Ctx.Handle, scope_id: u64) void {
            var write_index: usize = 0;
            for (self.pending_tasks.items) |*task| {
                if (self.scopeIsDescendantOrSelf(task.owner_scope_id, scope_id) catch @panic("scope descriptor referenced an unknown parent scope")) {
                    self.cancelPendingTask(ctx, task);
                    continue;
                }
                self.pending_tasks.items[write_index] = task.*;
                write_index += 1;
            }
            self.pending_tasks.items.len = write_index;
        }

        pub fn appendCleanupEvent(self: *Self, ctx: Ctx.Handle, name: []const u8) void {
            const allocator = Ctx.allocator(ctx);
            const copy = allocator.dupe(u8, name) catch @panic("out of memory");
            self.cleanup_events.append(allocator, copy) catch {
                allocator.free(copy);
                @panic("out of memory");
            };
        }

        pub fn disposeScopeSubtree(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, scope_id: u64) void {
            self.validateScopeId(scope_id) catch @panic("scope id has no host scope descriptor");

            var child_index: usize = 0;
            while (child_index < self.scopes.items.len) : (child_index += 1) {
                const child = self.scopes.items[child_index];
                if (!child.active) continue;
                if (child.parent_scope_id == scope_id) {
                    self.disposeScopeSubtree(ctx, roc_host, child.scope_id);
                }
            }

            var identity_deactivation = ScopeIdentityDeactivation{ .engine = self, .ctx = ctx, .roc_host = roc_host };
            identity_table.deactivateNodesInScope(&self.node_identities, scope_id, &identity_deactivation);

            for (self.active_stream.cleanups.items) |cleanup| {
                if (cleanup.scope_id == scope_id) {
                    self.appendCleanupEvent(ctx, cleanup.name);
                }
            }

            self.cancelPendingTasksInScopeSubtree(ctx, scope_id);

            identity_table.deactivateDomsInScope(&self.dom_identities, scope_id);

            const scope = &self.scopes.items[@intCast(scope_id)];
            switch (scope.step) {
                .each_row => |row| self.removeEachRowFromSiteIndex(scope.scope_id, row.key_hash),
                .root, .component, .when_branch => {},
            }
            deinitHostScopeStep(&scope.step, ctx, roc_host, &self.pending_roc_metrics);
            scope.active = false;
            var metrics = self.pending_roc_metrics;
            metrics.bump(.scopes_disposed, 1);
            self.pending_roc_metrics = metrics;
        }

        pub fn createEachRowScope(self: *Self, ctx: Ctx.Handle, parent_scope_id: u64, site_ordinal: u64, key_hash: u64, key: HostValue, item: HostValue, key_cap: HostValueCapability, item_cap: HostValueCapability) u64 {
            self.validateScopeId(parent_scope_id) catch @panic("scope id has no host scope descriptor");

            const key_cell = HostValueCell.initRetained(key, key_cap, &self.pending_roc_metrics);
            const item_cell = HostValueCell.initRetained(item, item_cap, &self.pending_roc_metrics);
            const result = scope_tree.appendEachRow(HostEachRowScopeStep, Ctx.allocator(ctx), &self.scopes, parent_scope_id, .{
                .site_ordinal = site_ordinal,
                .key_hash = key_hash,
                .key = key_cell,
                .item = item_cell,
            }) catch @panic("scope id has no host scope descriptor");
            self.recordScopeCreated();
            const site_index = self.ensureEachRowSiteIndex(Ctx.allocator(ctx), parent_scope_id, site_ordinal);
            self.appendEachRowToSiteIndex(Ctx.allocator(ctx), site_index, result.scope_id, key_hash);
            return result.scope_id;
        }

        pub fn eachRowScopeItemEquals(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, scope_id: u64, item: HostValue) bool {
            self.validateScopeId(scope_id) catch @panic("scope id has no host scope descriptor");
            const scope = &self.scopes.items[@intCast(scope_id)];
            self.recordEachItemCompare();
            return switch (scope.step) {
                .each_row => |*row| row.item.valueEquals(ctx, roc_host, item),
                .root, .component, .when_branch => @panic("scope id does not reference an each-row scope"),
            };
        }

        pub fn replaceEachRowScopeKey(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, scope_id: u64, key_hash: u64, key: HostValue, key_cap: HostValueCapability) void {
            self.validateScopeId(scope_id) catch @panic("scope id has no host scope descriptor");
            const scope = &self.scopes.items[@intCast(scope_id)];
            switch (scope.step) {
                .each_row => {},
                .root, .component, .when_branch => @panic("scope id does not reference an each-row scope"),
            }

            scope.step.each_row.key_hash = key_hash;
            scope.step.each_row.key.replaceRetained(ctx, roc_host, &self.pending_roc_metrics, key, key_cap);
        }

        pub fn replaceEachRowScopeItemWithCapability(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, scope_id: u64, item: HostValue, item_cap: HostValueCapability) void {
            self.validateScopeId(scope_id) catch @panic("scope id has no host scope descriptor");
            const scope = &self.scopes.items[@intCast(scope_id)];
            switch (scope.step) {
                .each_row => {},
                .root, .component, .when_branch => @panic("scope id does not reference an each-row scope"),
            }

            scope.step.each_row.item.replaceRetained(ctx, roc_host, &self.pending_roc_metrics, item, item_cap);
        }

        pub fn eachRowScopeValues(self: *Self, scope_id: u64) EachRowValues {
            self.validateScopeId(scope_id) catch @panic("scope id has no host scope descriptor");
            const scope = &self.scopes.items[@intCast(scope_id)];
            return switch (scope.step) {
                .each_row => |row| .{ .key = row.key.value, .item = row.item.value },
                .root, .component, .when_branch => @panic("scope id does not reference an each-row scope"),
            };
        }

        pub fn syncEachRowScopes(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, parent_scope_id: u64, site_ordinal: u64, keys: []const HostValue, items: []const HostValue, ops: HostEachOps) HostKeyedRowDiffResult {
            self.validateScopeId(parent_scope_id) catch @panic("scope id has no host scope descriptor");
            if (keys.len != items.len) @panic("Ui.each keyed scope received mismatched key and item lists");

            const allocator = Ctx.allocator(ctx);
            const site_index = self.ensureEachRowSiteIndex(allocator, parent_scope_id, site_ordinal);
            const existing_len = self.each_row_sites.items[site_index].scope_ids.items.len;
            self.recordEachSync(keys.len, existing_len);

            const key_cap = ops.key_capability;
            const item_cap = ops.item_capability;
            self.scratch.each_key_hashes.resize(allocator, keys.len) catch @panic("out of memory");
            defer self.scratch.each_key_hashes.clearRetainingCapacity();
            const key_hashes = self.scratch.each_key_hashes.items;
            for (keys, 0..) |key, key_index| {
                key_hashes[key_index] = self.hashEachKeyValue(ctx, roc_host, ops.key_text, ops.key_capability, key);
            }

            const next_hash_heads = &self.scratch.each_next_hash_heads;
            next_hash_heads.clearRetainingCapacity();
            defer next_hash_heads.clearRetainingCapacity();

            self.scratch.each_next_hash_links.resize(allocator, keys.len) catch @panic("out of memory");
            defer self.scratch.each_next_hash_links.clearRetainingCapacity();
            const next_hash_links = self.scratch.each_next_hash_links.items;
            @memset(next_hash_links, missing_each_row_index);

            for (key_hashes, 0..) |hash, key_index| {
                if (next_hash_heads.get(hash)) |head| {
                    var previous_index = head;
                    while (previous_index != missing_each_row_index) {
                        if (self.eachKeysEqual(ctx, roc_host, ops, keys[previous_index], keys[key_index])) {
                            @panic("keyed row diff operation failed");
                        }
                        previous_index = next_hash_links[previous_index];
                    }
                }

                const entry = next_hash_heads.getOrPut(allocator, hash) catch @panic("out of memory");
                if (entry.found_existing) {
                    next_hash_links[key_index] = entry.value_ptr.*;
                }
                entry.value_ptr.* = key_index;
            }

            self.scratch.each_matched_existing.resize(allocator, existing_len) catch @panic("out of memory");
            defer self.scratch.each_matched_existing.clearRetainingCapacity();
            const matched_existing = self.scratch.each_matched_existing.items;
            @memset(matched_existing, false);

            var next_scope_ids = allocator.alloc(u64, keys.len) catch @panic("out of memory");
            errdefer allocator.free(next_scope_ids);
            var row_items_changed = allocator.alloc(bool, keys.len) catch @panic("out of memory");
            errdefer allocator.free(row_items_changed);
            var scope_created = allocator.alloc(bool, keys.len) catch @panic("out of memory");
            errdefer allocator.free(scope_created);
            var removed_scope_ids: std.ArrayListUnmanaged(u64) = .empty;
            errdefer removed_scope_ids.deinit(allocator);

            var rows_reused: u64 = 0;
            var rows_created: u64 = 0;
            var row_items_unchanged: u64 = 0;
            var row_items_updated: u64 = 0;

            for (key_hashes, keys, items, 0..) |hash, key, item, key_index| {
                var matched_scope_id: ?u64 = null;
                const site = &self.each_row_sites.items[site_index];
                if (site.hash_heads.get(hash)) |head| {
                    var existing_index = head;
                    while (existing_index != missing_each_row_index) {
                        if (existing_index < existing_len and !matched_existing[existing_index]) {
                            const scope_id = site.scope_ids.items[existing_index];
                            if (self.eachRowScopeKeyEquals(ctx, roc_host, scope_id, key)) {
                                matched_existing[existing_index] = true;
                                matched_scope_id = scope_id;
                                break;
                            }
                        }
                        existing_index = site.hash_links.items[existing_index];
                    }
                }

                if (matched_scope_id) |scope_id| {
                    next_scope_ids[key_index] = scope_id;
                    scope_created[key_index] = false;
                    rows_reused += 1;

                    const row_item_equal = self.eachRowScopeItemEquals(ctx, roc_host, scope_id, item);
                    self.replaceEachRowScopeKey(ctx, roc_host, scope_id, hash, key, key_cap);
                    self.replaceEachRowScopeItemWithCapability(ctx, roc_host, scope_id, item, item_cap);
                    if (row_item_equal) {
                        row_items_changed[key_index] = false;
                        row_items_unchanged += 1;
                    } else {
                        row_items_changed[key_index] = true;
                        row_items_updated += 1;
                    }
                } else {
                    next_scope_ids[key_index] = std.math.maxInt(u64);
                    row_items_changed[key_index] = true;
                    scope_created[key_index] = true;
                    rows_created += 1;
                }
            }

            {
                const site = &self.each_row_sites.items[site_index];
                for (site.scope_ids.items[0..existing_len], 0..) |scope_id, existing_index| {
                    if (matched_existing[existing_index]) continue;
                    removed_scope_ids.append(allocator, scope_id) catch @panic("out of memory");
                }
            }

            for (scope_created, key_hashes, keys, items, 0..) |created, hash, key, item, key_index| {
                if (!created) continue;
                next_scope_ids[key_index] = self.createEachRowScope(ctx, parent_scope_id, site_ordinal, hash, key, item, key_cap, item_cap);
            }

            for (removed_scope_ids.items) |scope_id| {
                self.disposeScopeSubtree(ctx, roc_host, scope_id);
            }

            self.replaceEachRowSiteRows(allocator, site_index, next_scope_ids);
            const removed = removed_scope_ids.toOwnedSlice(allocator) catch @panic("out of memory");
            errdefer allocator.free(removed);

            var metrics = self.pending_roc_metrics;
            metrics.bump(.rows_reused, rows_reused);
            metrics.bump(.rows_created, rows_created);
            metrics.bump(.rows_removed, @intCast(removed.len));
            self.pending_roc_metrics = metrics;

            return .{
                .scope_ids = next_scope_ids,
                .row_items_changed = row_items_changed,
                .scope_created = scope_created,
                .removed_scope_ids = removed,
                .rows_reused = rows_reused,
                .rows_created = rows_created,
                .rows_removed = @intCast(removed.len),
                .row_items_unchanged = row_items_unchanged,
                .row_items_updated = row_items_updated,
            };
        }

        pub fn syncActiveEachRowScopes(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, site: HostNodeScopeSiteDesc, each: HostNodeEachDesc) HostKeyedRowDiffResult {
            if (site.kind != .each) {
                @panic("active row sync requires an each scope site");
            }
            if (site.node_id != each.node_id) {
                @panic("active row sync received mismatched each descriptors");
            }

            const allocator = Ctx.allocator(ctx);
            const items_value = self.cloneCachedSignalValue(ctx, &each.cached_value);
            const items_cap = self.hostSignalBindingCapability(ctx, &each.items);
            assertHostValueCapabilitiesMatch(each.ops.items_capability, items_cap, "each items extension capability did not match its signal value");
            defer callHostValueToUnitWithCapability(ctx, roc_host, items_cap, hv.hostValueCapabilityDrop(items_cap), items_value);

            const items = callHostValueToHostValueListWithCapability(ctx, roc_host, each.ops.items_capability, each.ops.items_to_values, items_value);
            defer items.decref(roc_host);
            const item_values = items.items();

            const keys = allocator.alloc(HostValue, item_values.len) catch @panic("out of memory");
            defer allocator.free(keys);

            for (item_values, 0..) |item, index| {
                keys[index] = callHostValueToHostValueWithCapability(ctx, roc_host, each.ops.item_capability, each.ops.key_of, item);
            }

            return self.syncEachRowScopes(ctx, roc_host, site.scope_id, site.ordinal, keys, item_values, each.ops);
        }

        pub fn collectNodeAttrDescriptor(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, elem_id: u64, attr: abi.NodeAttr, binder_stack: []const HostBinderBinding) void {
            const allocator = Ctx.allocator(ctx);
            switch (attr.tag) {
                .StaticText => {
                    const payload = attr.payload_static_text();
                    if (payload.field == node_field_custom) {
                        stream.appendStaticCustomTextAttr(allocator, elem_id, payload.name.asSlice(), payload.value.asSlice());
                    } else {
                        if (payload.name.asSlice().len != 0) @panic("fixed text attr descriptor carried a custom name");
                        const field = renderTextFieldFromAbi(payload.field);
                        stream.appendStaticTextAttr(allocator, elem_id, field, payload.value.asSlice());
                    }
                },
                .SignalText => {
                    const payload = attr.payload_signal_text();
                    if (payload.field == node_field_custom) {
                        if (payload.name.asSlice().len == 0) @panic("custom text attr descriptor used an empty name");
                        if (stream.customTextAttrDescriptorExists(elem_id, payload.name.asSlice())) @panic("element has duplicate custom text attr descriptors");
                        const signal = self.bindNodeSignal(allocator, stream, payload.signal.*, binder_stack);
                        stream.appendSignalCustomTextAttr(allocator, ctx, roc_host, &self.pending_roc_metrics, elem_id, payload.name.asSlice(), signal, payload.read);
                    } else {
                        if (payload.name.asSlice().len != 0) @panic("fixed text attr descriptor carried a custom name");
                        const field = renderTextFieldFromAbi(payload.field);
                        const signal = self.bindNodeSignal(allocator, stream, payload.signal.*, binder_stack);
                        stream.appendSignalTextAttr(allocator, ctx, roc_host, &self.pending_roc_metrics, elem_id, field, signal, payload.read);
                    }
                },
                .StaticBool => {
                    const payload = attr.payload_static_bool();
                    const field = renderBoolFieldFromAbi(payload.field);
                    stream.appendStaticBoolAttr(allocator, elem_id, field, payload.value);
                },
                .SignalBool => {
                    const payload = attr.payload_signal_bool();
                    const field = renderBoolFieldFromAbi(payload.field);
                    const signal = self.bindNodeSignal(allocator, stream, payload.signal.*, binder_stack);
                    stream.appendSignalBoolAttr(allocator, ctx, roc_host, &self.pending_roc_metrics, elem_id, field, signal, payload.read);
                },
                .OnEvent => {
                    const payload = attr.payload_on_event();
                    const msg = payload.msg;
                    const kind = renderEventKindFromAbi(payload.kind);
                    const payload_kind = eventPayloadKindFromAbi(msg.payload_kind);
                    const payload_accessor = eventPayloadAccessorFromAbi(msg.payload_accessor);
                    const target_node_id = resolveNodeBinderRef(binder_stack, msg.binder);
                    stream.appendEvent(allocator, roc_host, &self.pending_roc_metrics, elem_id, kind, msg.binder, target_node_id, payload_kind, payload_accessor, msg.payload_reducer);
                },
                .OnNamedEvent => {
                    const payload = attr.payload_on_named_event();
                    const msg = payload.msg;
                    const payload_kind = eventPayloadKindFromAbi(msg.payload_kind);
                    const payload_accessor = eventPayloadAccessorFromAbi(msg.payload_accessor);
                    const target_node_id = resolveNodeBinderRef(binder_stack, msg.binder);
                    stream.appendNamedEvent(allocator, roc_host, &self.pending_roc_metrics, elem_id, payload.name.asSlice(), payload.options, msg.binder, target_node_id, payload_kind, payload_accessor, msg.payload_reducer);
                },
            }
        }

        pub fn collectActiveWhenBranchDescriptors(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, site: HostNodeScopeSiteDesc, when: HostNodeWhenDesc, active_branch: HostScopeBranch, dirty_source_node_ids: []const u64) u64 {
            if (site.kind != .when) {
                @panic("active branch collection requires a when scope site");
            }
            if (site.node_id != when.node_id) {
                @panic("active branch collection received mismatched when descriptors");
            }

            if (self.activeWhenBranchScopeId(site.scope_id, site.ordinal, active_branch.opposite()) catch @panic("scope id has no host scope descriptor")) |inactive_scope_id| {
                self.disposeScopeSubtree(ctx, roc_host, inactive_scope_id);
            }

            const branch_scope = self.internWhenBranchScope(Ctx.allocator(ctx), site.scope_id, site.ordinal, active_branch) catch @panic("scope id has no host scope descriptor");
            const branch_scope_id = branch_scope.scope_id;
            const allocator = Ctx.allocator(ctx);
            const binder_stack = self.scratchBinderStack(allocator, site.binder_bindings);
            defer self.scratch.binder_stack.clearRetainingCapacity();

            const branch_elem = switch (active_branch) {
                .true_branch => when.when_true,
                .false_branch => when.when_false,
            };
            var ordinal: u64 = 0;
            var dom_ordinal: u64 = 0;
            self.collectActiveElemDescriptors(ctx, roc_host, stream, branch_elem, branch_scope_id, site.parent_elem_id, &ordinal, &dom_ordinal, binder_stack, branch_scope.created, dirty_source_node_ids);
            return branch_scope_id;
        }

        pub fn collectActiveEachRowDescriptors(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, site: HostNodeScopeSiteDesc, each: HostNodeEachDesc, dirty_source_node_ids: []const u64) void {
            const allocator = Ctx.allocator(ctx);
            const diff = self.syncActiveEachRowScopes(ctx, roc_host, site, each);
            defer diff.deinit(allocator);
            self.collectActiveEachRowDescriptorsFromDiff(ctx, roc_host, stream, site, each, diff, dirty_source_node_ids);
        }

        pub fn collectActiveEachRowDescriptorsFromDiff(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, site: HostNodeScopeSiteDesc, each: HostNodeEachDesc, diff: HostKeyedRowDiffResult, dirty_source_node_ids: []const u64) void {
            const allocator = Ctx.allocator(ctx);
            const binder_stack = self.scratchBinderStack(allocator, site.binder_bindings);
            defer self.scratch.binder_stack.clearRetainingCapacity();

            for (diff.scope_ids, diff.row_items_changed, diff.scope_created) |row_scope_id, row_item_changed, row_created| {
                if (!row_item_changed and !self.scopeSubtreeHasDirtyStructuralSource(&self.active_stream, row_scope_id, dirty_source_node_ids)) {
                    self.copyActiveScopeSubtreeDescriptors(ctx, roc_host, stream, row_scope_id);
                    continue;
                }

                const row_values = self.eachRowScopeValues(row_scope_id);
                const row_elem = callHostValueHostValueToElemWithCapabilities(ctx, roc_host, each.ops.key_capability, each.ops.item_capability, each.ops.row, row_values.key, row_values.item);
                defer abi.decrefElem(row_elem, roc_host);

                var ordinal: u64 = 0;
                var dom_ordinal: u64 = 0;
                self.collectActiveElemDescriptors(ctx, roc_host, stream, row_elem, row_scope_id, site.parent_elem_id, &ordinal, &dom_ordinal, binder_stack, row_created, dirty_source_node_ids);
            }
        }

        pub fn collectActiveEachSingleRowDescriptors(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, site: HostNodeScopeSiteDesc, each: HostNodeEachDesc, row_scope_id: u64, row_created: bool, dirty_source_node_ids: []const u64) void {
            const allocator = Ctx.allocator(ctx);
            const binder_stack = self.scratchBinderStack(allocator, site.binder_bindings);
            defer self.scratch.binder_stack.clearRetainingCapacity();

            const row_values = self.eachRowScopeValues(row_scope_id);
            const row_elem = callHostValueHostValueToElemWithCapabilities(ctx, roc_host, each.ops.key_capability, each.ops.item_capability, each.ops.row, row_values.key, row_values.item);
            defer abi.decrefElem(row_elem, roc_host);

            var ordinal: u64 = 0;
            var dom_ordinal: u64 = 0;
            self.collectActiveElemDescriptors(ctx, roc_host, stream, row_elem, row_scope_id, site.parent_elem_id, &ordinal, &dom_ordinal, binder_stack, row_created, dirty_source_node_ids);
        }

        pub fn collectActiveElemDescriptors(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, elem: abi.Elem, scope_id: u64, parent_elem_id: u64, ordinal: *u64, dom_ordinal: *u64, binder_stack: *std.ArrayListUnmanaged(HostBinderBinding), scope_created: bool, dirty_source_node_ids: []const u64) void {
            self.validateScopeId(scope_id) catch @panic("scope id has no host scope descriptor");

            const allocator = Ctx.allocator(ctx);
            switch (elem.tag) {
                .Element => {
                    const payload = elem.payload_element();
                    const elem_id = self.internDomIdentity(Ctx.allocator(ctx), scope_id, dom_ordinal.*) catch @panic("scope id has no host scope descriptor");
                    dom_ordinal.* += 1;
                    _ = stream.appendElement(allocator, elem_id, parent_elem_id, scope_id, payload.tag.asSlice());
                    for (payload.attrs.items()) |attr| {
                        self.collectNodeAttrDescriptor(ctx, roc_host, stream, elem_id, attr, binder_stack.items);
                    }
                    for (payload.children.items()) |child| {
                        self.collectActiveElemDescriptors(ctx, roc_host, stream, child, scope_id, elem_id, ordinal, dom_ordinal, binder_stack, scope_created, dirty_source_node_ids);
                    }
                },
                .Text => {
                    const elem_id = self.internDomIdentity(Ctx.allocator(ctx), scope_id, dom_ordinal.*) catch @panic("scope id has no host scope descriptor");
                    dom_ordinal.* += 1;
                    stream.appendTextNode(allocator, elem_id, parent_elem_id, scope_id, elem.payload_text().asSlice());
                },
                .TextSignal => {
                    const elem_id = self.internDomIdentity(Ctx.allocator(ctx), scope_id, dom_ordinal.*) catch @panic("scope id has no host scope descriptor");
                    dom_ordinal.* += 1;
                    const text_signal = elem.payload_text_signal();
                    const signal = self.bindNodeSignal(allocator, stream, text_signal.signal.*, binder_stack.items);
                    stream.appendSignalTextNode(allocator, ctx, roc_host, &self.pending_roc_metrics, elem_id, parent_elem_id, scope_id, signal, text_signal.read);
                },
                .Cleanup => {
                    stream.appendCleanup(allocator, scope_id, elem.payload_cleanup().cleanup.asSlice());
                },
                .OnChange => {
                    const payload = elem.payload_on_change();
                    const signal = self.bindNodeSignal(allocator, stream, payload.signal.*, binder_stack.items);
                    stream.appendOnChange(allocator, ctx, roc_host, &self.pending_roc_metrics, scope_id, signal, payload.to_cmd);
                },
                .OnMount => {
                    const payload = elem.payload_on_mount();
                    stream.appendMount(allocator, roc_host, &self.pending_roc_metrics, scope_id, payload.to_cmd, scope_created);
                },
                .State => {
                    const site_ordinal = ordinal.*;
                    const node_id = self.internNodeIdentity(Ctx.allocator(ctx), scope_id, site_ordinal) catch @panic("scope id has no host scope descriptor");
                    ordinal.* += 1;
                    stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .state, binder_stack.items);
                    const state = elem.payload_state();
                    stream.appendState(allocator, roc_host, &self.pending_roc_metrics, node_id, state.initial, state.cap);
                    self.ensureStateFromDesc(ctx, roc_host, stream.states.items[stream.states.items.len - 1]);
                    binder_stack.append(allocator, .{ .token = state.binder, .node_id = node_id }) catch @panic("out of memory");
                    self.collectActiveElemDescriptors(ctx, roc_host, stream, state.child.*, scope_id, parent_elem_id, ordinal, dom_ordinal, binder_stack, scope_created, dirty_source_node_ids);
                    _ = binder_stack.pop() orelse unreachable;
                },
                .Component => {
                    const site_ordinal = ordinal.*;
                    const node_id = self.internNodeIdentity(Ctx.allocator(ctx), scope_id, site_ordinal) catch @panic("scope id has no host scope descriptor");
                    ordinal.* += 1;
                    stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .component, binder_stack.items);
                    const component_scope = self.internComponentScope(Ctx.allocator(ctx), scope_id, site_ordinal) catch @panic("scope id has no host scope descriptor");
                    const component_scope_id = component_scope.scope_id;
                    var component_ordinal: u64 = 0;
                    var component_dom_ordinal: u64 = 0;
                    self.collectActiveElemDescriptors(ctx, roc_host, stream, elem.payload_component().child.*, component_scope_id, parent_elem_id, &component_ordinal, &component_dom_ordinal, binder_stack, component_scope.created, dirty_source_node_ids);
                },
                .When => {
                    const site_ordinal = ordinal.*;
                    const node_id = self.internNodeIdentity(Ctx.allocator(ctx), scope_id, site_ordinal) catch @panic("scope id has no host scope descriptor");
                    ordinal.* += 1;
                    stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .when, binder_stack.items);
                    const when_payload = elem.payload_when();
                    const condition_binding = self.bindNodeSignal(allocator, stream, when_payload.condition.*, binder_stack.items);
                    stream.appendWhen(allocator, ctx, roc_host, &self.pending_roc_metrics, node_id, condition_binding, when_payload.read, when_payload.when_false.*, when_payload.when_true.*);

                    const when_index = stream.whens.items.len - 1;
                    const when_desc = &stream.whens.items[when_index];
                    const condition = self.evalHostSignalBinding(ctx, roc_host, &when_desc.condition);
                    const condition_cap = self.hostSignalBindingCapability(ctx, &when_desc.condition);
                    assertHostValueCapabilitiesMatch(when_desc.read.capability, condition_cap, "when read extension capability did not match its signal value");
                    const active_branch: HostScopeBranch = if (callHostValueToBoolWithCapability(ctx, roc_host, when_desc.read.capability, when_desc.read.read, condition)) .true_branch else .false_branch;
                    when_desc.cached_value.replace(ctx, roc_host, &self.pending_roc_metrics, condition, condition_cap);
                    if (self.activeWhenBranchScopeId(scope_id, site_ordinal, active_branch.opposite()) catch @panic("scope id has no host scope descriptor")) |inactive_scope_id| {
                        self.disposeScopeSubtree(ctx, roc_host, inactive_scope_id);
                    }
                    const branch_scope = self.internWhenBranchScope(Ctx.allocator(ctx), scope_id, site_ordinal, active_branch) catch @panic("scope id has no host scope descriptor");
                    const branch_scope_id = branch_scope.scope_id;
                    var branch_ordinal: u64 = 0;
                    const branch_elem = switch (active_branch) {
                        .true_branch => when_payload.when_true.*,
                        .false_branch => when_payload.when_false.*,
                    };
                    var branch_dom_ordinal: u64 = 0;
                    self.collectActiveElemDescriptors(ctx, roc_host, stream, branch_elem, branch_scope_id, parent_elem_id, &branch_ordinal, &branch_dom_ordinal, binder_stack, branch_scope.created, dirty_source_node_ids);
                },
                .Each => {
                    const site_ordinal = ordinal.*;
                    const node_id = self.internNodeIdentity(Ctx.allocator(ctx), scope_id, site_ordinal) catch @panic("scope id has no host scope descriptor");
                    ordinal.* += 1;
                    stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .each, binder_stack.items);
                    const each_payload = elem.payload_each();
                    const items_binding = self.bindNodeSignal(allocator, stream, each_payload.items.*, binder_stack.items);
                    stream.appendEach(allocator, ctx, roc_host, &self.pending_roc_metrics, node_id, items_binding, each_payload.ops);
                    const each_index = stream.eaches.items.len - 1;
                    const each_desc = stream.eaches.items[stream.eaches.items.len - 1];

                    const items_value = self.evalHostSignalBinding(ctx, roc_host, &stream.eaches.items[each_index].items);
                    const each_items_cap = self.hostSignalBindingCapability(ctx, &stream.eaches.items[each_index].items);
                    assertHostValueCapabilitiesMatch(each_desc.ops.items_capability, each_items_cap, "each items extension capability did not match its signal value");
                    const items = callHostValueToHostValueListWithCapability(ctx, roc_host, each_desc.ops.items_capability, each_desc.ops.items_to_values, items_value);
                    defer items.decref(roc_host);
                    stream.eaches.items[each_index].cached_value.replace(ctx, roc_host, &self.pending_roc_metrics, items_value, each_items_cap);
                    const item_values = items.items();

                    self.scratch.each_keys.resize(allocator, item_values.len) catch @panic("out of memory");
                    defer self.scratch.each_keys.clearRetainingCapacity();
                    const keys = self.scratch.each_keys.items;

                    for (item_values, 0..) |item, index| {
                        keys[index] = callHostValueToHostValueWithCapability(ctx, roc_host, each_desc.ops.item_capability, each_desc.ops.key_of, item);
                    }

                    const diff = self.syncEachRowScopes(ctx, roc_host, scope_id, site_ordinal, keys, item_values, each_desc.ops);
                    defer diff.deinit(allocator);

                    for (diff.scope_ids, diff.row_items_changed, diff.scope_created) |row_scope_id, row_item_changed, row_created| {
                        if (!row_item_changed and !self.scopeSubtreeHasDirtyStructuralSource(&self.active_stream, row_scope_id, dirty_source_node_ids)) {
                            self.copyActiveScopeSubtreeDescriptors(ctx, roc_host, stream, row_scope_id);
                            continue;
                        }

                        const row_values = self.eachRowScopeValues(row_scope_id);
                        const row_elem = callHostValueHostValueToElemWithCapabilities(ctx, roc_host, each_desc.ops.key_capability, each_desc.ops.item_capability, each_desc.ops.row, row_values.key, row_values.item);
                        defer abi.decrefElem(row_elem, roc_host);

                        var row_ordinal: u64 = 0;
                        var row_dom_ordinal: u64 = 0;
                        self.collectActiveElemDescriptors(ctx, roc_host, stream, row_elem, row_scope_id, parent_elem_id, &row_ordinal, &row_dom_ordinal, binder_stack, row_created, dirty_source_node_ids);
                    }
                },
            }
        }

        pub fn collectActiveElemRootDescriptors(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, root: abi.Elem, dirty_source_node_ids: []const u64) void {
            const root_scope = self.internRootScope(Ctx.allocator(ctx)) catch @panic("scope id has no host scope descriptor");
            const root_scope_id = root_scope.scope_id;
            const allocator = Ctx.allocator(ctx);
            const binder_stack = self.scratchBinderStack(allocator, &.{});
            defer self.scratch.binder_stack.clearRetainingCapacity();
            var ordinal: u64 = 0;
            var dom_ordinal: u64 = 0;
            self.collectActiveElemDescriptors(ctx, roc_host, stream, root, root_scope_id, 0, &ordinal, &dom_ordinal, binder_stack, root_scope.created, dirty_source_node_ids);
        }

        pub fn clearActiveSignalGraph(self: *Self, ctx: Ctx.Handle) void {
            const allocator = Ctx.allocator(ctx);
            if (self.roc_host == null) {
                if (self.active_signal_graph.items.len != 0) @panic("active signal graph cannot release records without a Roc host");
                self.active_signal_graph.items.len = 0;
                return;
            }
            var lifecycle = ActiveSignalGraphLifecycle{ .engine = self, .ctx = ctx };
            active_graph.clear(HostSignalRecord, allocator, &self.active_signal_graph, &lifecycle);
        }

        pub fn clearActiveIntervals(self: *Self, ctx: Ctx.Handle) void {
            const roc_host = self.roc_host orelse {
                if (self.active_intervals.items.len != 0) @panic("active intervals cannot release tokens without a Roc host");
                self.active_intervals.items.len = 0;
                return;
            };
            for (self.active_intervals.items) |interval| {
                if (interval.active) {
                    Ctx.sink(ctx).cancelInterval(interval.token);
                }
                releaseHostSignalToken(interval.source_token, roc_host);
            }
            self.active_intervals.clearRetainingCapacity();
        }

        fn markActiveIntervalsInactive(self: *Self) void {
            for (self.active_intervals.items) |*interval| {
                interval.active = false;
            }
        }

        fn activeIntervalBySourceToken(self: *Self, source_token: HostSignalToken) ?*HostActiveInterval {
            var found: ?*HostActiveInterval = null;
            for (self.active_intervals.items) |*interval| {
                if (interval.source_token != source_token) continue;
                if (found != null) @panic("interval source token matched more than one runtime interval");
                found = interval;
            }
            return found;
        }

        fn ensureActiveInterval(self: *Self, ctx: Ctx.Handle, source_token: HostSignalToken, period_ms: u64) void {
            if (self.activeIntervalBySourceToken(source_token)) |interval| {
                if (interval.period_ms != period_ms) @panic("interval source token changed period");
                interval.active = true;
                return;
            }

            if (self.next_interval_token == std.math.maxInt(u64)) @panic("host interval token overflowed");
            const token = self.next_interval_token;
            self.next_interval_token += 1;
            abi.increfBox(@ptrCast(source_token), 1);
            self.active_intervals.append(Ctx.allocator(ctx), .{
                .token = token,
                .source_token = source_token,
                .period_ms = period_ms,
                .active = true,
            }) catch {
                releaseHostSignalToken(source_token, self.roc_host.?);
                @panic("out of memory");
            };
            Ctx.sink(ctx).startInterval(token, period_ms);
        }

        fn removeActiveIntervalBySourceToken(self: *Self, ctx: Ctx.Handle, source_token: HostSignalToken) void {
            const roc_host = self.roc_host orelse @panic("active interval cannot release token without a Roc host");
            var found_index: ?usize = null;
            for (self.active_intervals.items, 0..) |interval, index| {
                if (interval.source_token != source_token) continue;
                if (found_index != null) @panic("interval source token matched more than one runtime interval");
                found_index = index;
            }
            const index = found_index orelse @panic("active interval removal missed its source token");
            const interval = self.active_intervals.items[index];
            if (interval.active) {
                Ctx.sink(ctx).cancelInterval(interval.token);
            }
            releaseHostSignalToken(interval.source_token, roc_host);
            const last_index = self.active_intervals.items.len - 1;
            if (index != last_index) {
                self.active_intervals.items[index] = self.active_intervals.items[last_index];
            }
            self.active_intervals.items.len = last_index;
        }

        fn syncActiveIntervalsFromGraph(self: *Self, ctx: Ctx.Handle) void {
            self.markActiveIntervalsInactive();
            self.pending_roc_metrics.bump(.active_intervals_synced, @intCast(self.active_signal_graph.items.len));

            for (self.active_signal_graph.items) |node| {
                switch (node.record.payload) {
                    .interval_source => |payload| self.ensureActiveInterval(ctx, payload.token, payload.period_ms),
                    .ref, .const_value, .map, .map2, .combine, .task_source => {},
                }
            }

            const roc_host = self.roc_host orelse {
                for (self.active_intervals.items) |interval| {
                    if (!interval.active) @panic("inactive interval cannot release token without a Roc host");
                }
                return;
            };

            var write_index: usize = 0;
            for (self.active_intervals.items) |interval| {
                if (!interval.active) {
                    Ctx.sink(ctx).cancelInterval(interval.token);
                    releaseHostSignalToken(interval.source_token, roc_host);
                    continue;
                }
                self.active_intervals.items[write_index] = interval;
                write_index += 1;
            }
            self.active_intervals.items.len = write_index;
        }

        pub fn clearActiveSignalRoutes(self: *Self, ctx: Ctx.Handle) void {
            active_graph.clearRoutes(
                Ctx.allocator(ctx),
                &self.active_source_signal_routes,
                &self.active_text_signal_routes,
                &self.active_bool_signal_routes,
                &self.active_change_signal_routes,
                &self.active_structural_signal_routes,
            );
        }

        pub fn clearActiveSinkSignalRoutes(self: *Self, ctx: Ctx.Handle) void {
            active_graph.clearSinkRoutes(
                Ctx.allocator(ctx),
                &self.active_text_signal_routes,
                &self.active_bool_signal_routes,
                &self.active_change_signal_routes,
                &self.active_structural_signal_routes,
            );
        }

        pub fn activeSignalRecordId(self: *Self, record: *const HostSignalRecord) ?u64 {
            return active_graph.recordId(HostSignalRecord, self.active_signal_graph.items, record);
        }

        pub fn requireActiveSignalRecordId(self: *Self, record: *const HostSignalRecord) u64 {
            return active_graph.requireRecordId(HostSignalRecord, self.active_signal_graph.items, record);
        }

        pub fn appendActiveSignalGraphNode(self: *Self, ctx: Ctx.Handle, record: *HostSignalRecord, rank: u64) u64 {
            const record_id = active_graph.appendNode(HostSignalRecord, Ctx.allocator(ctx), &self.active_signal_graph, record, rank);
            self.pending_roc_metrics.bump(.active_graph_records_rebuilt, 1);
            return record_id;
        }

        pub fn appendActiveSignalDependentId(self: *Self, ctx: Ctx.Handle, input_record_id: u64, dependent_record_id: u64) void {
            active_graph.appendDependentId(HostSignalRecord, Ctx.allocator(ctx), self.active_signal_graph.items, input_record_id, dependent_record_id);
        }

        pub fn appendActiveSourceSignalRoute(self: *Self, ctx: Ctx.Handle, source_node_id: u64, record_id: u64) void {
            active_graph.appendSourceRoute(Ctx.allocator(ctx), &self.active_source_signal_routes, self.node_identities.items.len, source_node_id, record_id);
        }

        pub fn retainActiveSignalRecord(self: *Self, ctx: Ctx.Handle, record: *HostSignalRecord) void {
            var lifecycle = ActiveSignalGraphLifecycle{ .engine = self, .ctx = ctx };
            const records_rebuilt = active_graph.retainRecord(
                HostSignalRecord,
                Ctx.allocator(ctx),
                &self.active_signal_graph,
                &self.active_source_signal_routes,
                self.node_identities.items.len,
                record,
                &lifecycle,
            );
            self.pending_roc_metrics.bump(.active_graph_records_rebuilt, records_rebuilt);
        }

        pub fn ensureActiveSourceSignalRoute(self: *Self, ctx: Ctx.Handle, source_node_id: u64) *std.ArrayListUnmanaged(u64) {
            return active_graph.ensureSourceRoute(Ctx.allocator(ctx), &self.active_source_signal_routes, self.node_identities.items.len, source_node_id);
        }

        pub fn ensureActiveTextSignalRoute(self: *Self, ctx: Ctx.Handle, record_id: u64) *std.ArrayListUnmanaged(HostActiveTextSignalSink) {
            return active_graph.ensureTextRoute(Ctx.allocator(ctx), &self.active_text_signal_routes, self.active_signal_graph.items.len, record_id);
        }

        pub fn ensureActiveBoolSignalRoute(self: *Self, ctx: Ctx.Handle, record_id: u64) *std.ArrayListUnmanaged(HostActiveBoolSignalSink) {
            return active_graph.ensureBoolRoute(Ctx.allocator(ctx), &self.active_bool_signal_routes, self.active_signal_graph.items.len, record_id);
        }

        pub fn ensureActiveChangeSignalRoute(self: *Self, ctx: Ctx.Handle, record_id: u64) *std.ArrayListUnmanaged(HostActiveChangeSignalSink) {
            return active_graph.ensureChangeRoute(Ctx.allocator(ctx), &self.active_change_signal_routes, self.active_signal_graph.items.len, record_id);
        }

        pub fn ensureActiveStructuralSignalRoute(self: *Self, ctx: Ctx.Handle, record_id: u64) *std.ArrayListUnmanaged(HostActiveStructuralSignal) {
            return active_graph.ensureStructuralRoute(Ctx.allocator(ctx), &self.active_structural_signal_routes, self.active_signal_graph.items.len, record_id);
        }

        fn appendActiveTextSignalRoute(self: *Self, ctx: Ctx.Handle, record_id: u64, route: HostActiveTextSignalSink) void {
            active_graph.appendTextRoute(Ctx.allocator(ctx), &self.active_text_signal_routes, self.active_signal_graph.items.len, record_id, route);
        }

        fn removeActiveTextSignalRoute(self: *Self, record_id: u64, kind: HostActiveTextSignalSinkKind, index: usize) void {
            active_graph.removeTextRoute(&self.active_text_signal_routes, record_id, kind, index);
        }

        fn updateActiveTextSignalRouteIndex(self: *Self, record_id: u64, kind: HostActiveTextSignalSinkKind, old_index: usize, new_index: usize) void {
            active_graph.updateTextRouteIndex(&self.active_text_signal_routes, record_id, kind, old_index, new_index);
        }

        fn appendActiveBoolSignalRoute(self: *Self, ctx: Ctx.Handle, record_id: u64, route: HostActiveBoolSignalSink) void {
            active_graph.appendBoolRoute(Ctx.allocator(ctx), &self.active_bool_signal_routes, self.active_signal_graph.items.len, record_id, route);
        }

        fn removeActiveBoolSignalRoute(self: *Self, record_id: u64, index: usize) void {
            active_graph.removeBoolRoute(&self.active_bool_signal_routes, record_id, index);
        }

        fn updateActiveBoolSignalRouteIndex(self: *Self, record_id: u64, old_index: usize, new_index: usize) void {
            active_graph.updateBoolRouteIndex(&self.active_bool_signal_routes, record_id, old_index, new_index);
        }

        fn appendActiveChangeSignalRoute(self: *Self, ctx: Ctx.Handle, record_id: u64, route: HostActiveChangeSignalSink) void {
            active_graph.appendChangeRoute(Ctx.allocator(ctx), &self.active_change_signal_routes, self.active_signal_graph.items.len, record_id, route);
        }

        fn removeActiveChangeSignalRoute(self: *Self, record_id: u64, index: usize) void {
            active_graph.removeChangeRoute(&self.active_change_signal_routes, record_id, index);
        }

        fn updateActiveChangeSignalRouteIndex(self: *Self, record_id: u64, old_index: usize, new_index: usize) void {
            active_graph.updateChangeRouteIndex(&self.active_change_signal_routes, record_id, old_index, new_index);
        }

        fn appendActiveStructuralSignalRoute(self: *Self, ctx: Ctx.Handle, record_id: u64, route: HostActiveStructuralSignal) void {
            active_graph.appendStructuralRoute(Ctx.allocator(ctx), &self.active_structural_signal_routes, self.active_signal_graph.items.len, record_id, route);
        }

        fn removeActiveStructuralSignalRoute(self: *Self, record_id: u64, kind: HostActiveStructuralSignalKind, index: usize) void {
            active_graph.removeStructuralRoute(&self.active_structural_signal_routes, record_id, kind, index);
        }

        fn updateActiveStructuralSignalRouteIndex(self: *Self, record_id: u64, kind: HostActiveStructuralSignalKind, old_index: usize, new_index: usize) void {
            active_graph.updateStructuralRouteIndex(&self.active_structural_signal_routes, record_id, kind, old_index, new_index);
        }

        pub fn rebuildActiveSinkSignalRoutesFromStream(self: *Self, ctx: Ctx.Handle, stream: *const HostNodeDescriptorStream) void {
            active_graph.rebuildSinkRoutesFromStream(
                HostSignalRecord,
                Ctx.allocator(ctx),
                self.active_signal_graph.items,
                &self.active_text_signal_routes,
                &self.active_bool_signal_routes,
                &self.active_change_signal_routes,
                &self.active_structural_signal_routes,
                stream,
            );
        }

        pub fn rebuildActiveSignalGraphFromStream(self: *Self, ctx: Ctx.Handle, stream: *const HostNodeDescriptorStream) void {
            self.clearActiveSignalRoutes(ctx);
            self.clearActiveSignalGraph(ctx);

            var lifecycle = ActiveSignalGraphLifecycle{ .engine = self, .ctx = ctx };
            const records_rebuilt = active_graph.retainStreamRecords(
                HostSignalRecord,
                Ctx.allocator(ctx),
                &self.active_signal_graph,
                &self.active_source_signal_routes,
                self.node_identities.items.len,
                stream,
                &lifecycle,
            );
            self.pending_roc_metrics.bump(.active_graph_records_rebuilt, records_rebuilt);

            self.rebuildActiveSinkSignalRoutesFromStream(ctx, stream);
            self.syncActiveIntervalsFromGraph(ctx);
        }

        pub fn releaseActiveSignalRecord(self: *Self, ctx: Ctx.Handle, record: *HostSignalRecord) void {
            var lifecycle = ActiveSignalGraphLifecycle{ .engine = self, .ctx = ctx };
            active_graph.releaseRecord(
                HostSignalRecord,
                Ctx.allocator(ctx),
                &self.active_signal_graph,
                &self.active_source_signal_routes,
                &self.active_text_signal_routes,
                &self.active_bool_signal_routes,
                &self.active_change_signal_routes,
                &self.active_structural_signal_routes,
                record,
                &lifecycle,
            );
        }

        fn deinitActiveSignalTextNodeDesc(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, desc: *HostNodeSignalTextNodeDesc) void {
            desc.cached_value.deinit(ctx, roc_host, &self.pending_roc_metrics);
            desc.signal.deinit(Ctx.allocator(ctx), ctx, roc_host, &self.pending_roc_metrics);
            releaseHostTextRead(desc.read, roc_host, &self.pending_roc_metrics);
        }

        fn deinitActiveSignalTextAttrDesc(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, desc: *HostNodeSignalTextAttrDesc) void {
            desc.cached_value.deinit(ctx, roc_host, &self.pending_roc_metrics);
            desc.signal.deinit(Ctx.allocator(ctx), ctx, roc_host, &self.pending_roc_metrics);
            releaseHostTextRead(desc.read, roc_host, &self.pending_roc_metrics);
        }

        fn deinitActiveSignalCustomTextAttrDesc(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, desc: *HostNodeSignalCustomTextAttrDesc) void {
            const allocator = Ctx.allocator(ctx);
            allocator.free(desc.name);
            desc.cached_value.deinit(ctx, roc_host, &self.pending_roc_metrics);
            desc.signal.deinit(allocator, ctx, roc_host, &self.pending_roc_metrics);
            releaseHostTextRead(desc.read, roc_host, &self.pending_roc_metrics);
        }

        fn deinitActiveSignalBoolAttrDesc(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, desc: *HostNodeSignalBoolAttrDesc) void {
            desc.cached_value.deinit(ctx, roc_host, &self.pending_roc_metrics);
            desc.signal.deinit(Ctx.allocator(ctx), ctx, roc_host, &self.pending_roc_metrics);
            releaseHostBoolRead(desc.read, roc_host, &self.pending_roc_metrics);
        }

        fn deinitActiveOnChangeDesc(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, desc: *HostNodeOnChangeDesc) void {
            desc.cached_value.deinit(ctx, roc_host, &self.pending_roc_metrics);
            desc.signal.deinit(Ctx.allocator(ctx), ctx, roc_host, &self.pending_roc_metrics);
            self.pending_roc_metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(desc.to_cmd, roc_host);
        }

        fn deinitActiveMountDesc(self: *Self, roc_host: *abi.RocHost, desc: *HostNodeMountDesc) void {
            self.pending_roc_metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(desc.to_cmd, roc_host);
        }

        pub fn scopeIsInReplacementTarget(self: *Self, scope_id: u64, target: HostStructuralReplacementTarget) bool {
            return switch (target) {
                .scope => |root_scope_id| self.scopeIsDescendantOrSelf(scope_id, root_scope_id) catch @panic("scope descriptor referenced an unknown parent scope"),
                .each_site => |site| self.scopeIsEachSiteRowDescendantOrSelf(scope_id, site) catch @panic("scope descriptor referenced an unknown parent scope"),
            };
        }

        fn buildReplacementTargetScopeSet(self: *Self, ctx: Ctx.Handle, target: HostStructuralReplacementTarget) []const bool {
            if (self.scratch.replacement_target_scopes.items.len != 0) @panic("replacement target scope scratch was already active");
            const allocator = Ctx.allocator(ctx);
            self.scratch.replacement_target_scopes.resize(allocator, self.scopes.items.len) catch @panic("out of memory");
            const target_scopes = self.scratch.replacement_target_scopes.items;
            for (self.scopes.items) |scope| {
                if (scope.scope_id >= target_scopes.len) @panic("scope descriptor id exceeded replacement target set");
                target_scopes[@intCast(scope.scope_id)] = self.scopeIsInReplacementTarget(scope.scope_id, target);
            }
            return target_scopes;
        }

        fn scopeIsInReplacementTargetSet(target_scopes: []const bool, scope_id: u64) bool {
            if (scope_id >= target_scopes.len) @panic("descriptor referenced scope outside replacement target set");
            return target_scopes[@intCast(scope_id)];
        }

        pub fn renderNodeInReplacementTarget(self: *Self, stream: *const HostNodeDescriptorStream, node: HostRenderNode, target: HostStructuralReplacementTarget) bool {
            return self.scopeIsInReplacementTarget(renderNodeScopeId(stream, node), target);
        }

        pub fn renderNodeInReplacementTargetSet(self: *Self, stream: *const HostNodeDescriptorStream, node: HostRenderNode, target_scopes: []const bool) bool {
            _ = self;
            return scopeIsInReplacementTargetSet(target_scopes, renderNodeScopeId(stream, node));
        }

        pub fn elemIdInReplacementTarget(self: *Self, stream: *const HostNodeDescriptorStream, elem_id: u64, target: HostStructuralReplacementTarget) bool {
            const scope_id = elemScopeId(stream, elem_id) orelse @panic("descriptor referenced an element outside the render stream");
            return self.scopeIsInReplacementTarget(scope_id, target);
        }

        pub fn elemIdInReplacementTargetSet(self: *Self, stream: *const HostNodeDescriptorStream, elem_id: u64, target_scopes: []const bool) bool {
            _ = self;
            const scope_id = elemScopeId(stream, elem_id) orelse @panic("descriptor referenced an element outside the render stream");
            return scopeIsInReplacementTargetSet(target_scopes, scope_id);
        }

        pub fn streamNodeIdInReplacementTarget(self: *Self, previous: *const HostNodeDescriptorStream, node_id: u64, kind: HostNodeScopeSiteKind, target: HostStructuralReplacementTarget) bool {
            const descriptor_index = previous.nodeDescriptorIndex(node_id) orelse return false;
            const site_index = descriptor_index.scope_sites.get(kind) orelse return false;
            if (site_index >= previous.scope_sites.items.len) @panic("scope site descriptor index exceeded descriptor table");
            const site = previous.scope_sites.items[site_index];
            if (site.node_id != node_id or site.kind != kind) @panic("scope site descriptor index pointed at the wrong node");
            return self.scopeIsInReplacementTarget(site.scope_id, target);
        }

        pub fn streamNodeIdInReplacementTargetSet(self: *Self, previous: *const HostNodeDescriptorStream, node_id: u64, kind: HostNodeScopeSiteKind, target_scopes: []const bool) bool {
            _ = self;
            const descriptor_index = previous.nodeDescriptorIndex(node_id) orelse return false;
            const site_index = descriptor_index.scope_sites.get(kind) orelse return false;
            if (site_index >= previous.scope_sites.items.len) @panic("scope site descriptor index exceeded descriptor table");
            const site = previous.scope_sites.items[site_index];
            if (site.node_id != node_id or site.kind != kind) @panic("scope site descriptor index pointed at the wrong node");
            return scopeIsInReplacementTargetSet(target_scopes, site.scope_id);
        }

        fn removalIndexDesc(_: void, lhs: usize, rhs: usize) bool {
            return lhs > rhs;
        }

        fn sortRemovalIndexesDescending(indexes: []usize) void {
            std.mem.sort(usize, indexes, {}, removalIndexDesc);
        }

        fn appendRemovalIndex(ctx: Ctx.Handle, indexes: *std.ArrayListUnmanaged(usize), index: ?usize) void {
            indexes.append(Ctx.allocator(ctx), index orelse return) catch @panic("out of memory");
        }

        fn appendTextFieldRemovalIndexes(ctx: Ctx.Handle, indexes: *std.ArrayListUnmanaged(usize), fields: HostTextFieldDescriptorIndexes) void {
            appendRemovalIndex(ctx, indexes, fields.text);
            appendRemovalIndex(ctx, indexes, fields.role);
            appendRemovalIndex(ctx, indexes, fields.label);
            appendRemovalIndex(ctx, indexes, fields.test_id);
            appendRemovalIndex(ctx, indexes, fields.value);
            appendRemovalIndex(ctx, indexes, fields.class);
        }

        fn appendBoolFieldRemovalIndexes(ctx: Ctx.Handle, indexes: *std.ArrayListUnmanaged(usize), fields: HostBoolFieldDescriptorIndexes) void {
            appendRemovalIndex(ctx, indexes, fields.checked);
            appendRemovalIndex(ctx, indexes, fields.disabled);
        }

        fn appendEventRemovalIndexes(ctx: Ctx.Handle, indexes: *std.ArrayListUnmanaged(usize), events: HostEventDescriptorIndexes) void {
            appendRemovalIndex(ctx, indexes, events.click);
            appendRemovalIndex(ctx, indexes, events.input);
            appendRemovalIndex(ctx, indexes, events.check);
            appendRemovalIndex(ctx, indexes, events.pointer_down);
            appendRemovalIndex(ctx, indexes, events.pointer_up);
            appendRemovalIndex(ctx, indexes, events.pointer_enter);
            appendRemovalIndex(ctx, indexes, events.pointer_leave);
        }

        fn appendNamedEventRemovalIndexes(self: *Self, ctx: Ctx.Handle, indexes: *std.ArrayListUnmanaged(usize), elem_id: u64) void {
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_remove_target, self.active_stream.named_events.items.len);
            for (self.active_stream.named_events.items, 0..) |desc, index| {
                if (desc.elem_id == elem_id) {
                    indexes.append(Ctx.allocator(ctx), index) catch @panic("out of memory");
                }
            }
        }

        fn clearElemOwnedRemovalScratch(self: *Self) void {
            self.scratch.remove_element_indexes.clearRetainingCapacity();
            self.scratch.remove_text_node_indexes.clearRetainingCapacity();
            self.scratch.remove_signal_text_node_indexes.clearRetainingCapacity();
            self.scratch.remove_static_text_attr_indexes.clearRetainingCapacity();
            self.scratch.remove_signal_text_attr_indexes.clearRetainingCapacity();
            self.scratch.remove_static_bool_attr_indexes.clearRetainingCapacity();
            self.scratch.remove_signal_bool_attr_indexes.clearRetainingCapacity();
            self.scratch.remove_event_indexes.clearRetainingCapacity();
            self.scratch.remove_named_event_indexes.clearRetainingCapacity();
        }

        fn removeActiveElementDescriptorAt(self: *Self, ctx: Ctx.Handle, index: usize) void {
            const allocator = Ctx.allocator(ctx);
            if (index >= self.active_stream.elements.items.len) @panic("element removal index exceeded descriptor table");
            const last_index = self.active_stream.elements.items.len - 1;
            const removed = self.active_stream.elements.items[index];
            self.active_stream.clearElementIndex(removed.elem_id, index);
            allocator.free(removed.tag);

            if (index != last_index) {
                const moved = self.active_stream.elements.items[last_index];
                self.active_stream.elements.items[index] = moved;
                self.active_stream.updateElementIndex(moved.elem_id, index);
            }
            self.active_stream.elements.items.len = last_index;
        }

        fn removeActiveTextNodeDescriptorAt(self: *Self, ctx: Ctx.Handle, index: usize) void {
            const allocator = Ctx.allocator(ctx);
            if (index >= self.active_stream.text_nodes.items.len) @panic("text node removal index exceeded descriptor table");
            const last_index = self.active_stream.text_nodes.items.len - 1;
            const removed = self.active_stream.text_nodes.items[index];
            self.active_stream.clearTextNodeIndex(removed.elem_id, index);
            allocator.free(removed.value);

            if (index != last_index) {
                const moved = self.active_stream.text_nodes.items[last_index];
                self.active_stream.text_nodes.items[index] = moved;
                self.active_stream.updateTextNodeIndex(moved.elem_id, index);
            }
            self.active_stream.text_nodes.items.len = last_index;
        }

        fn removeActiveSignalTextNodeDescriptorAt(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, index: usize) void {
            if (index >= self.active_stream.signal_text_nodes.items.len) @panic("signal text node removal index exceeded descriptor table");
            const last_index = self.active_stream.signal_text_nodes.items.len - 1;
            var removed = self.active_stream.signal_text_nodes.items[index];
            const removed_record_id = self.requireActiveSignalRecordId(removed.signal.record);
            self.removeActiveTextSignalRoute(removed_record_id, .text_node, index);
            self.active_stream.clearSignalTextNodeIndex(removed.elem_id, index);
            self.active_stream.forgetSignalRecordTree(removed.signal.record);
            self.releaseActiveSignalRecord(ctx, removed.signal.record);
            self.deinitActiveSignalTextNodeDesc(ctx, roc_host, &removed);

            if (index != last_index) {
                const moved = self.active_stream.signal_text_nodes.items[last_index];
                const moved_record_id = self.requireActiveSignalRecordId(moved.signal.record);
                self.active_stream.signal_text_nodes.items[index] = moved;
                self.updateActiveTextSignalRouteIndex(moved_record_id, .text_node, last_index, index);
                self.active_stream.updateSignalTextNodeIndex(moved.elem_id, index);
            }
            self.active_stream.signal_text_nodes.items.len = last_index;
        }

        fn removeActiveStaticTextAttrDescriptorAt(self: *Self, ctx: Ctx.Handle, index: usize) void {
            const allocator = Ctx.allocator(ctx);
            if (index >= self.active_stream.static_text_attrs.items.len) @panic("static text attr removal index exceeded descriptor table");
            const last_index = self.active_stream.static_text_attrs.items.len - 1;
            const removed = self.active_stream.static_text_attrs.items[index];
            self.active_stream.clearStaticTextAttrIndex(removed.elem_id, removed.field, index);
            allocator.free(removed.value);

            if (index != last_index) {
                const moved = self.active_stream.static_text_attrs.items[last_index];
                self.active_stream.static_text_attrs.items[index] = moved;
                self.active_stream.updateStaticTextAttrIndex(moved.elem_id, moved.field, index);
            }
            self.active_stream.static_text_attrs.items.len = last_index;
        }

        fn removeActiveSignalTextAttrDescriptorAt(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, index: usize) void {
            if (index >= self.active_stream.signal_text_attrs.items.len) @panic("signal text attr removal index exceeded descriptor table");
            const last_index = self.active_stream.signal_text_attrs.items.len - 1;
            var removed = self.active_stream.signal_text_attrs.items[index];
            const removed_record_id = self.requireActiveSignalRecordId(removed.signal.record);
            self.removeActiveTextSignalRoute(removed_record_id, .text_attr, index);
            self.active_stream.clearSignalTextAttrIndex(removed.elem_id, removed.field, index);
            self.active_stream.forgetSignalRecordTree(removed.signal.record);
            self.releaseActiveSignalRecord(ctx, removed.signal.record);
            self.deinitActiveSignalTextAttrDesc(ctx, roc_host, &removed);

            if (index != last_index) {
                const moved = self.active_stream.signal_text_attrs.items[last_index];
                const moved_record_id = self.requireActiveSignalRecordId(moved.signal.record);
                self.active_stream.signal_text_attrs.items[index] = moved;
                self.updateActiveTextSignalRouteIndex(moved_record_id, .text_attr, last_index, index);
                self.active_stream.updateSignalTextAttrIndex(moved.elem_id, moved.field, index);
            }
            self.active_stream.signal_text_attrs.items.len = last_index;
        }

        fn removeActiveStaticCustomTextAttrDescriptorsForRemovedElems(self: *Self, ctx: Ctx.Handle, removed_elem_ids: []const u64) void {
            const allocator = Ctx.allocator(ctx);
            var write_index: usize = 0;
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_remove_target, self.active_stream.static_custom_text_attrs.items.len);
            for (self.active_stream.static_custom_text_attrs.items) |desc| {
                if (u64SliceContains(removed_elem_ids, desc.elem_id)) {
                    allocator.free(desc.name);
                    allocator.free(desc.value);
                    continue;
                }
                self.active_stream.static_custom_text_attrs.items[write_index] = desc;
                write_index += 1;
            }
            self.active_stream.static_custom_text_attrs.items.len = write_index;
        }

        fn removeActiveSignalCustomTextAttrDescriptorsForRemovedElems(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, removed_elem_ids: []const u64) void {
            var write_index: usize = 0;
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_remove_target, self.active_stream.signal_custom_text_attrs.items.len);
            for (self.active_stream.signal_custom_text_attrs.items, 0..) |desc, read_index| {
                if (u64SliceContains(removed_elem_ids, desc.elem_id)) {
                    var removed = desc;
                    const record_id = self.requireActiveSignalRecordId(removed.signal.record);
                    self.removeActiveTextSignalRoute(record_id, .custom_text_attr, read_index);
                    self.active_stream.forgetSignalRecordTree(removed.signal.record);
                    self.releaseActiveSignalRecord(ctx, removed.signal.record);
                    self.deinitActiveSignalCustomTextAttrDesc(ctx, roc_host, &removed);
                    continue;
                }
                const record_id = self.requireActiveSignalRecordId(desc.signal.record);
                self.updateActiveTextSignalRouteIndex(record_id, .custom_text_attr, read_index, write_index);
                self.active_stream.signal_custom_text_attrs.items[write_index] = desc;
                write_index += 1;
            }
            self.active_stream.signal_custom_text_attrs.items.len = write_index;
        }

        fn removeActiveStaticBoolAttrDescriptorAt(self: *Self, index: usize) void {
            if (index >= self.active_stream.static_bool_attrs.items.len) @panic("static bool attr removal index exceeded descriptor table");
            const last_index = self.active_stream.static_bool_attrs.items.len - 1;
            const removed = self.active_stream.static_bool_attrs.items[index];
            self.active_stream.clearStaticBoolAttrIndex(removed.elem_id, removed.field, index);

            if (index != last_index) {
                const moved = self.active_stream.static_bool_attrs.items[last_index];
                self.active_stream.static_bool_attrs.items[index] = moved;
                self.active_stream.updateStaticBoolAttrIndex(moved.elem_id, moved.field, index);
            }
            self.active_stream.static_bool_attrs.items.len = last_index;
        }

        fn removeActiveSignalBoolAttrDescriptorAt(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, index: usize) void {
            if (index >= self.active_stream.signal_bool_attrs.items.len) @panic("signal bool attr removal index exceeded descriptor table");
            const last_index = self.active_stream.signal_bool_attrs.items.len - 1;
            var removed = self.active_stream.signal_bool_attrs.items[index];
            const removed_record_id = self.requireActiveSignalRecordId(removed.signal.record);
            self.removeActiveBoolSignalRoute(removed_record_id, index);
            self.active_stream.clearSignalBoolAttrIndex(removed.elem_id, removed.field, index);
            self.active_stream.forgetSignalRecordTree(removed.signal.record);
            self.releaseActiveSignalRecord(ctx, removed.signal.record);
            self.deinitActiveSignalBoolAttrDesc(ctx, roc_host, &removed);

            if (index != last_index) {
                const moved = self.active_stream.signal_bool_attrs.items[last_index];
                const moved_record_id = self.requireActiveSignalRecordId(moved.signal.record);
                self.active_stream.signal_bool_attrs.items[index] = moved;
                self.updateActiveBoolSignalRouteIndex(moved_record_id, last_index, index);
                self.active_stream.updateSignalBoolAttrIndex(moved.elem_id, moved.field, index);
            }
            self.active_stream.signal_bool_attrs.items.len = last_index;
        }

        fn removeActiveOnChangeDescriptorsInTarget(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, target_scopes: []const bool) void {
            var write_index: usize = 0;
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_remove_target, self.active_stream.on_changes.items.len);
            for (self.active_stream.on_changes.items, 0..) |desc, read_index| {
                if (scopeIsInReplacementTargetSet(target_scopes, desc.scope_id)) {
                    var removed = desc;
                    const record_id = self.requireActiveSignalRecordId(removed.signal.record);
                    self.removeActiveChangeSignalRoute(record_id, read_index);
                    self.active_stream.forgetSignalRecordTree(removed.signal.record);
                    self.releaseActiveSignalRecord(ctx, removed.signal.record);
                    self.deinitActiveOnChangeDesc(ctx, roc_host, &removed);
                    continue;
                }
                const record_id = self.requireActiveSignalRecordId(desc.signal.record);
                self.updateActiveChangeSignalRouteIndex(record_id, read_index, write_index);
                self.active_stream.on_changes.items[write_index] = desc;
                write_index += 1;
            }
            self.active_stream.on_changes.items.len = write_index;
        }

        fn removeActiveMountDescriptorsInTarget(self: *Self, roc_host: *abi.RocHost, target_scopes: []const bool) void {
            var write_index: usize = 0;
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_remove_target, self.active_stream.mounts.items.len);
            for (self.active_stream.mounts.items) |desc| {
                if (scopeIsInReplacementTargetSet(target_scopes, desc.scope_id)) {
                    var removed = desc;
                    self.deinitActiveMountDesc(roc_host, &removed);
                    continue;
                }
                self.active_stream.mounts.items[write_index] = desc;
                write_index += 1;
            }
            self.active_stream.mounts.items.len = write_index;
        }

        fn removeActiveCleanupDescriptorsInTarget(self: *Self, ctx: Ctx.Handle, target_scopes: []const bool) void {
            const allocator = Ctx.allocator(ctx);
            var write_index: usize = 0;
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_remove_target, self.active_stream.cleanups.items.len);
            for (self.active_stream.cleanups.items) |desc| {
                if (scopeIsInReplacementTargetSet(target_scopes, desc.scope_id)) {
                    allocator.free(desc.name);
                    continue;
                }
                self.active_stream.cleanups.items[write_index] = desc;
                write_index += 1;
            }
            self.active_stream.cleanups.items.len = write_index;
        }

        fn removeActiveEventDescriptorAt(self: *Self, roc_host: *abi.RocHost, index: usize) void {
            const fixed_event_count = self.active_stream.events.items.len;
            const total_event_count = fixed_event_count + self.active_stream.named_events.items.len;
            if (self.active_events.items.len != total_event_count) {
                if (total_event_count != 0) @panic("active event descriptor table is out of sync with active events");
            }
            if (index >= fixed_event_count) @panic("event removal index exceeded descriptor table");

            const last_fixed_index = fixed_event_count - 1;
            const removed = self.active_stream.events.items[index];
            if (removed.owns_payload_reducer) {
                @panic("active event descriptor retained ownership outside the active event table");
            }
            self.active_stream.clearEventIndex(removed.elem_id, removed.kind, index);
            if (self.active_events.items.len != 0) {
                self.deinitActiveEventDesc(roc_host, self.active_events.items[index]);
            }

            if (index != last_fixed_index) {
                const moved = self.active_stream.events.items[last_fixed_index];
                self.active_stream.events.items[index] = moved;
                self.active_stream.updateEventIndex(moved.elem_id, moved.kind, index);
                if (self.active_events.items.len != 0) {
                    self.active_events.items[index] = self.active_events.items[last_fixed_index];
                }
            }
            self.active_stream.events.items.len = last_fixed_index;
            if (self.active_events.items.len != 0) {
                var move_index = last_fixed_index;
                while (move_index + 1 < self.active_events.items.len) : (move_index += 1) {
                    self.active_events.items[move_index] = self.active_events.items[move_index + 1];
                }
                self.active_events.items.len -= 1;
            }
        }

        fn removeActiveNamedEventDescriptorAt(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, index: usize) void {
            const fixed_event_count = self.active_stream.events.items.len;
            const named_event_count = self.active_stream.named_events.items.len;
            const total_event_count = fixed_event_count + named_event_count;
            if (self.active_events.items.len != total_event_count) {
                if (total_event_count != 0) @panic("active named event descriptor table is out of sync with active events");
            }
            if (index >= named_event_count) @panic("named event removal index exceeded descriptor table");

            const allocator = Ctx.allocator(ctx);
            const active_index = fixed_event_count + index;
            const last_named_index = named_event_count - 1;
            const last_active_index = fixed_event_count + last_named_index;
            const removed = self.active_stream.named_events.items[index];
            if (removed.owns_payload_reducer) {
                @panic("active named event descriptor retained ownership outside the active event table");
            }
            allocator.free(removed.name);
            if (self.active_events.items.len != 0) {
                self.deinitActiveEventDesc(roc_host, self.active_events.items[active_index]);
            }

            if (index != last_named_index) {
                self.active_stream.named_events.items[index] = self.active_stream.named_events.items[last_named_index];
                if (self.active_events.items.len != 0) {
                    self.active_events.items[active_index] = self.active_events.items[last_active_index];
                }
            }
            self.active_stream.named_events.items.len = last_named_index;
            if (self.active_events.items.len != 0) self.active_events.items.len = last_active_index;
        }

        fn collectElemOwnedRemovalIndexes(self: *Self, ctx: Ctx.Handle, removed_elem_ids: []const u64) void {
            if (self.scratch.remove_element_indexes.items.len != 0 or
                self.scratch.remove_text_node_indexes.items.len != 0 or
                self.scratch.remove_signal_text_node_indexes.items.len != 0 or
                self.scratch.remove_static_text_attr_indexes.items.len != 0 or
                self.scratch.remove_signal_text_attr_indexes.items.len != 0 or
                self.scratch.remove_static_bool_attr_indexes.items.len != 0 or
                self.scratch.remove_signal_bool_attr_indexes.items.len != 0 or
                self.scratch.remove_event_indexes.items.len != 0 or
                self.scratch.remove_named_event_indexes.items.len != 0)
            {
                @panic("elem-owned removal scratch was already active");
            }

            for (removed_elem_ids) |elem_id| {
                const descriptor_index = self.active_stream.elemDescriptorIndex(elem_id) orelse @panic("removed elem id had no descriptor index");
                const has_render_descriptor = descriptor_index.element != null or descriptor_index.text_node != null or descriptor_index.signal_text_node != null;
                if (!has_render_descriptor) @panic("removed rendered elem id had no render-owned descriptor");

                appendRemovalIndex(ctx, &self.scratch.remove_element_indexes, descriptor_index.element);
                appendRemovalIndex(ctx, &self.scratch.remove_text_node_indexes, descriptor_index.text_node);
                appendRemovalIndex(ctx, &self.scratch.remove_signal_text_node_indexes, descriptor_index.signal_text_node);
                appendTextFieldRemovalIndexes(ctx, &self.scratch.remove_static_text_attr_indexes, descriptor_index.static_text_attrs);
                appendTextFieldRemovalIndexes(ctx, &self.scratch.remove_signal_text_attr_indexes, descriptor_index.signal_text_attrs);
                appendBoolFieldRemovalIndexes(ctx, &self.scratch.remove_static_bool_attr_indexes, descriptor_index.static_bool_attrs);
                appendBoolFieldRemovalIndexes(ctx, &self.scratch.remove_signal_bool_attr_indexes, descriptor_index.signal_bool_attrs);
                appendEventRemovalIndexes(ctx, &self.scratch.remove_event_indexes, descriptor_index.events);
                self.appendNamedEventRemovalIndexes(ctx, &self.scratch.remove_named_event_indexes, elem_id);
            }

            sortRemovalIndexesDescending(self.scratch.remove_element_indexes.items);
            sortRemovalIndexesDescending(self.scratch.remove_text_node_indexes.items);
            sortRemovalIndexesDescending(self.scratch.remove_signal_text_node_indexes.items);
            sortRemovalIndexesDescending(self.scratch.remove_static_text_attr_indexes.items);
            sortRemovalIndexesDescending(self.scratch.remove_signal_text_attr_indexes.items);
            sortRemovalIndexesDescending(self.scratch.remove_static_bool_attr_indexes.items);
            sortRemovalIndexesDescending(self.scratch.remove_signal_bool_attr_indexes.items);
            sortRemovalIndexesDescending(self.scratch.remove_event_indexes.items);
            sortRemovalIndexesDescending(self.scratch.remove_named_event_indexes.items);
        }

        fn removeActiveElemOwnedDescriptorsForRemovedElems(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, removed_elem_ids: []const u64) void {
            self.collectElemOwnedRemovalIndexes(ctx, removed_elem_ids);
            defer self.clearElemOwnedRemovalScratch();

            for (self.scratch.remove_static_text_attr_indexes.items) |index| {
                self.removeActiveStaticTextAttrDescriptorAt(ctx, index);
            }
            for (self.scratch.remove_signal_text_attr_indexes.items) |index| {
                self.removeActiveSignalTextAttrDescriptorAt(ctx, roc_host, index);
            }
            for (self.scratch.remove_static_bool_attr_indexes.items) |index| {
                self.removeActiveStaticBoolAttrDescriptorAt(index);
            }
            for (self.scratch.remove_signal_bool_attr_indexes.items) |index| {
                self.removeActiveSignalBoolAttrDescriptorAt(ctx, roc_host, index);
            }
            for (self.scratch.remove_event_indexes.items) |index| {
                self.removeActiveEventDescriptorAt(roc_host, index);
            }
            for (self.scratch.remove_named_event_indexes.items) |index| {
                self.removeActiveNamedEventDescriptorAt(ctx, roc_host, index);
            }
            for (self.scratch.remove_element_indexes.items) |index| {
                self.removeActiveElementDescriptorAt(ctx, index);
            }
            for (self.scratch.remove_text_node_indexes.items) |index| {
                self.removeActiveTextNodeDescriptorAt(ctx, index);
            }
            for (self.scratch.remove_signal_text_node_indexes.items) |index| {
                self.removeActiveSignalTextNodeDescriptorAt(ctx, roc_host, index);
            }
        }

        fn removeActiveScopeSiteDescriptorsInTarget(self: *Self, ctx: Ctx.Handle, target_scopes: []const bool) void {
            const allocator = Ctx.allocator(ctx);
            var write_index: usize = 0;
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_remove_target, self.active_stream.scope_sites.items.len);
            for (self.active_stream.scope_sites.items, 0..) |desc, read_index| {
                if (scopeIsInReplacementTargetSet(target_scopes, desc.scope_id)) {
                    self.active_stream.clearScopeSiteIndex(desc.node_id, desc.kind, read_index);
                    allocator.free(desc.binder_bindings);
                    continue;
                }
                self.active_stream.updateScopeSiteIndex(desc.node_id, desc.kind, write_index);
                self.active_stream.scope_sites.items[write_index] = desc;
                write_index += 1;
            }
            self.active_stream.scope_sites.items.len = write_index;
        }

        fn removeActiveStateDescriptorsInTarget(self: *Self, roc_host: *abi.RocHost, target_scopes: []const bool) void {
            var write_index: usize = 0;
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_remove_target, self.active_stream.states.items.len);
            for (self.active_stream.states.items, 0..) |desc, read_index| {
                if (self.streamNodeIdInReplacementTargetSet(&self.active_stream, desc.node_id, .state, target_scopes)) {
                    self.active_stream.clearStateIndex(desc.node_id, read_index);
                    self.pending_roc_metrics.bump(.closure_releases, 1);
                    abi.decrefErasedCallable(desc.initial, roc_host);
                    releaseHostValueCapability(desc.cap, roc_host, &self.pending_roc_metrics);
                    continue;
                }
                self.active_stream.updateStateIndex(desc.node_id, write_index);
                self.active_stream.states.items[write_index] = desc;
                write_index += 1;
            }
            self.active_stream.states.items.len = write_index;
        }

        fn removeActiveWhenDescriptorsInTarget(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, target_scopes: []const bool) void {
            var write_index: usize = 0;
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_remove_target, self.active_stream.whens.items.len);
            for (self.active_stream.whens.items, 0..) |desc, read_index| {
                if (self.streamNodeIdInReplacementTargetSet(&self.active_stream, desc.node_id, .when, target_scopes)) {
                    var removed = desc;
                    const record_id = self.requireActiveSignalRecordId(removed.condition.record);
                    self.removeActiveStructuralSignalRoute(record_id, .when, read_index);
                    self.active_stream.clearWhenIndex(removed.node_id, read_index);
                    self.active_stream.forgetSignalRecordTree(removed.condition.record);
                    self.releaseActiveSignalRecord(ctx, removed.condition.record);
                    removed.cached_value.deinit(ctx, roc_host, &self.pending_roc_metrics);
                    removed.condition.deinit(Ctx.allocator(ctx), ctx, roc_host, &self.pending_roc_metrics);
                    releaseHostBoolRead(removed.read, roc_host, &self.pending_roc_metrics);
                    abi.decrefElem(removed.when_false, roc_host);
                    abi.decrefElem(removed.when_true, roc_host);
                    continue;
                }
                const record_id = self.requireActiveSignalRecordId(desc.condition.record);
                self.updateActiveStructuralSignalRouteIndex(record_id, .when, read_index, write_index);
                self.active_stream.updateWhenIndex(desc.node_id, write_index);
                self.active_stream.whens.items[write_index] = desc;
                write_index += 1;
            }
            self.active_stream.whens.items.len = write_index;
        }

        fn removeActiveEachDescriptorsInTarget(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, target_scopes: []const bool) void {
            var write_index: usize = 0;
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_remove_target, self.active_stream.eaches.items.len);
            for (self.active_stream.eaches.items, 0..) |desc, read_index| {
                if (self.streamNodeIdInReplacementTargetSet(&self.active_stream, desc.node_id, .each, target_scopes)) {
                    var removed = desc;
                    const record_id = self.requireActiveSignalRecordId(removed.items.record);
                    self.removeActiveStructuralSignalRoute(record_id, .each, read_index);
                    self.active_stream.clearEachIndex(removed.node_id, read_index);
                    self.active_stream.forgetSignalRecordTree(removed.items.record);
                    self.releaseActiveSignalRecord(ctx, removed.items.record);
                    removed.cached_value.deinit(ctx, roc_host, &self.pending_roc_metrics);
                    removed.items.deinit(Ctx.allocator(ctx), ctx, roc_host, &self.pending_roc_metrics);
                    releaseHostEachOps(removed.ops, roc_host, &self.pending_roc_metrics);
                    continue;
                }
                const record_id = self.requireActiveSignalRecordId(desc.items.record);
                self.updateActiveStructuralSignalRouteIndex(record_id, .each, read_index, write_index);
                self.active_stream.updateEachIndex(desc.node_id, write_index);
                self.active_stream.eaches.items[write_index] = desc;
                write_index += 1;
            }
            self.active_stream.eaches.items.len = write_index;
        }

        fn removeActiveNonRenderDescriptorsInTarget(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, target_scopes: []const bool, removed_elem_ids: []const u64) void {
            self.removeActiveElemOwnedDescriptorsForRemovedElems(ctx, roc_host, removed_elem_ids);
            self.removeActiveStaticCustomTextAttrDescriptorsForRemovedElems(ctx, removed_elem_ids);
            self.removeActiveSignalCustomTextAttrDescriptorsForRemovedElems(ctx, roc_host, removed_elem_ids);
            self.removeActiveOnChangeDescriptorsInTarget(ctx, roc_host, target_scopes);
            self.removeActiveMountDescriptorsInTarget(roc_host, target_scopes);
            self.removeActiveCleanupDescriptorsInTarget(ctx, target_scopes);
            self.removeActiveStateDescriptorsInTarget(roc_host, target_scopes);
            self.removeActiveWhenDescriptorsInTarget(ctx, roc_host, target_scopes);
            self.removeActiveEachDescriptorsInTarget(ctx, roc_host, target_scopes);
            self.removeActiveScopeSiteDescriptorsInTarget(ctx, target_scopes);
        }

        fn adjustActiveScopeSiteRenderInsertIndices(self: *Self, replace_index: usize, removed_render_count: usize, replacement_render_count: usize) void {
            for (self.active_stream.scope_sites.items) |*desc| {
                desc.render_insert_index = adjustedRenderInsertIndex(desc.render_insert_index, replace_index, removed_render_count, replacement_render_count);
            }
        }

        fn appendReplacementEventsMoved(self: *Self, ctx: Ctx.Handle, replacement: *HostNodeDescriptorStream) void {
            const allocator = Ctx.allocator(ctx);
            const existing_total_event_count = self.active_stream.events.items.len + self.active_stream.named_events.items.len;
            if (self.active_events.items.len != existing_total_event_count) {
                if (existing_total_event_count != 0) @panic("active event descriptor table is out of sync before replacement event splice");
            }

            const event_base = self.active_stream.events.items.len;
            for (replacement.events.items, 0..) |*desc, offset| {
                if (!desc.owns_payload_reducer) {
                    @panic("replacement event descriptor did not own its retained payload");
                }
                self.active_stream.recordEventIndex(allocator, desc.elem_id, desc.kind, event_base + offset);
                self.active_events.insert(allocator, event_base + offset, .{
                    .target_node_id = desc.target_node_id,
                    .payload_kind = desc.payload_kind,
                    .payload_accessor = desc.payload_accessor,
                    .payload_reducer = desc.payload_reducer,
                }) catch @panic("out of memory");
                desc.owns_payload_reducer = false;
            }

            self.active_stream.events.appendSlice(allocator, replacement.events.items) catch @panic("out of memory");
            replacement.events.items.len = 0;

            for (replacement.named_events.items) |*desc| {
                if (!desc.owns_payload_reducer) {
                    @panic("replacement named event descriptor did not own its retained payload");
                }
                self.active_events.append(allocator, .{
                    .target_node_id = desc.target_node_id,
                    .payload_kind = desc.payload_kind,
                    .payload_accessor = desc.payload_accessor,
                    .payload_reducer = desc.payload_reducer,
                }) catch @panic("out of memory");
                desc.owns_payload_reducer = false;
            }

            self.active_stream.named_events.appendSlice(allocator, replacement.named_events.items) catch @panic("out of memory");
            replacement.named_events.items.len = 0;
        }

        fn appendReplacementNonRenderDescriptorsMoved(self: *Self, ctx: Ctx.Handle, replacement: *HostNodeDescriptorStream, render_insert_offset: usize) void {
            const allocator = Ctx.allocator(ctx);

            const element_base = self.active_stream.elements.items.len;
            for (replacement.elements.items, 0..) |desc, offset| {
                self.active_stream.recordElementIndex(allocator, desc.elem_id, element_base + offset);
            }
            self.active_stream.elements.appendSlice(allocator, replacement.elements.items) catch @panic("out of memory");
            replacement.elements.items.len = 0;

            const text_node_base = self.active_stream.text_nodes.items.len;
            for (replacement.text_nodes.items, 0..) |desc, offset| {
                self.active_stream.recordTextNodeIndex(allocator, desc.elem_id, text_node_base + offset);
            }
            self.active_stream.text_nodes.appendSlice(allocator, replacement.text_nodes.items) catch @panic("out of memory");
            replacement.text_nodes.items.len = 0;

            const signal_text_node_base = self.active_stream.signal_text_nodes.items.len;
            for (replacement.signal_text_nodes.items, 0..) |desc, offset| {
                self.active_stream.recordSignalTextNodeIndex(allocator, desc.elem_id, signal_text_node_base + offset);
            }
            for (replacement.signal_text_nodes.items, 0..) |desc, offset| {
                self.active_stream.rememberSignalRecordTree(allocator, desc.signal.record);
                self.retainActiveSignalRecord(ctx, desc.signal.record);
                const record_id = self.requireActiveSignalRecordId(desc.signal.record);
                self.appendActiveTextSignalRoute(ctx, record_id, .{
                    .kind = .text_node,
                    .index = signal_text_node_base + offset,
                });
            }
            self.active_stream.signal_text_nodes.appendSlice(allocator, replacement.signal_text_nodes.items) catch @panic("out of memory");
            replacement.signal_text_nodes.items.len = 0;

            const static_text_attr_base = self.active_stream.static_text_attrs.items.len;
            for (replacement.static_text_attrs.items, 0..) |desc, offset| {
                self.active_stream.recordStaticTextAttrIndex(allocator, desc.elem_id, desc.field, static_text_attr_base + offset);
            }
            self.active_stream.static_text_attrs.appendSlice(allocator, replacement.static_text_attrs.items) catch @panic("out of memory");
            replacement.static_text_attrs.items.len = 0;

            const signal_text_attr_base = self.active_stream.signal_text_attrs.items.len;
            for (replacement.signal_text_attrs.items, 0..) |desc, offset| {
                self.active_stream.recordSignalTextAttrIndex(allocator, desc.elem_id, desc.field, signal_text_attr_base + offset);
            }
            for (replacement.signal_text_attrs.items, 0..) |desc, offset| {
                self.active_stream.rememberSignalRecordTree(allocator, desc.signal.record);
                self.retainActiveSignalRecord(ctx, desc.signal.record);
                const record_id = self.requireActiveSignalRecordId(desc.signal.record);
                self.appendActiveTextSignalRoute(ctx, record_id, .{
                    .kind = .text_attr,
                    .index = signal_text_attr_base + offset,
                });
            }
            self.active_stream.signal_text_attrs.appendSlice(allocator, replacement.signal_text_attrs.items) catch @panic("out of memory");
            replacement.signal_text_attrs.items.len = 0;

            self.active_stream.static_custom_text_attrs.appendSlice(allocator, replacement.static_custom_text_attrs.items) catch @panic("out of memory");
            replacement.static_custom_text_attrs.items.len = 0;

            const signal_custom_text_attr_base = self.active_stream.signal_custom_text_attrs.items.len;
            for (replacement.signal_custom_text_attrs.items, 0..) |desc, offset| {
                self.active_stream.rememberSignalRecordTree(allocator, desc.signal.record);
                self.retainActiveSignalRecord(ctx, desc.signal.record);
                const record_id = self.requireActiveSignalRecordId(desc.signal.record);
                self.appendActiveTextSignalRoute(ctx, record_id, .{
                    .kind = .custom_text_attr,
                    .index = signal_custom_text_attr_base + offset,
                });
            }
            self.active_stream.signal_custom_text_attrs.appendSlice(allocator, replacement.signal_custom_text_attrs.items) catch @panic("out of memory");
            replacement.signal_custom_text_attrs.items.len = 0;

            const static_bool_attr_base = self.active_stream.static_bool_attrs.items.len;
            for (replacement.static_bool_attrs.items, 0..) |desc, offset| {
                self.active_stream.recordStaticBoolAttrIndex(allocator, desc.elem_id, desc.field, static_bool_attr_base + offset);
            }
            self.active_stream.static_bool_attrs.appendSlice(allocator, replacement.static_bool_attrs.items) catch @panic("out of memory");
            replacement.static_bool_attrs.items.len = 0;

            const signal_bool_attr_base = self.active_stream.signal_bool_attrs.items.len;
            for (replacement.signal_bool_attrs.items, 0..) |desc, offset| {
                self.active_stream.recordSignalBoolAttrIndex(allocator, desc.elem_id, desc.field, signal_bool_attr_base + offset);
            }
            for (replacement.signal_bool_attrs.items, 0..) |desc, offset| {
                self.active_stream.rememberSignalRecordTree(allocator, desc.signal.record);
                self.retainActiveSignalRecord(ctx, desc.signal.record);
                const record_id = self.requireActiveSignalRecordId(desc.signal.record);
                self.appendActiveBoolSignalRoute(ctx, record_id, .{
                    .index = signal_bool_attr_base + offset,
                });
            }
            self.active_stream.signal_bool_attrs.appendSlice(allocator, replacement.signal_bool_attrs.items) catch @panic("out of memory");
            replacement.signal_bool_attrs.items.len = 0;

            const on_change_base = self.active_stream.on_changes.items.len;
            for (replacement.on_changes.items, 0..) |desc, offset| {
                self.active_stream.rememberSignalRecordTree(allocator, desc.signal.record);
                self.retainActiveSignalRecord(ctx, desc.signal.record);
                const record_id = self.requireActiveSignalRecordId(desc.signal.record);
                self.appendActiveChangeSignalRoute(ctx, record_id, .{
                    .index = on_change_base + offset,
                });
            }
            self.active_stream.on_changes.appendSlice(allocator, replacement.on_changes.items) catch @panic("out of memory");
            replacement.on_changes.items.len = 0;

            self.active_stream.mounts.appendSlice(allocator, replacement.mounts.items) catch @panic("out of memory");
            replacement.mounts.items.len = 0;

            self.active_stream.cleanups.appendSlice(allocator, replacement.cleanups.items) catch @panic("out of memory");
            replacement.cleanups.items.len = 0;

            self.appendReplacementEventsMoved(ctx, replacement);

            const scope_site_base = self.active_stream.scope_sites.items.len;
            for (replacement.scope_sites.items) |*desc| {
                desc.render_insert_index += render_insert_offset;
            }
            for (replacement.scope_sites.items, 0..) |desc, offset| {
                self.active_stream.recordScopeSiteIndex(allocator, desc.node_id, desc.kind, scope_site_base + offset);
            }
            self.active_stream.scope_sites.appendSlice(allocator, replacement.scope_sites.items) catch @panic("out of memory");
            replacement.scope_sites.items.len = 0;

            const state_base = self.active_stream.states.items.len;
            for (replacement.states.items, 0..) |desc, offset| {
                self.active_stream.recordStateIndex(allocator, desc.node_id, state_base + offset);
            }
            self.active_stream.states.appendSlice(allocator, replacement.states.items) catch @panic("out of memory");
            replacement.states.items.len = 0;

            const when_base = self.active_stream.whens.items.len;
            for (replacement.whens.items, 0..) |desc, offset| {
                self.active_stream.recordWhenIndex(allocator, desc.node_id, when_base + offset);
                self.active_stream.rememberSignalRecordTree(allocator, desc.condition.record);
                self.retainActiveSignalRecord(ctx, desc.condition.record);
                const record_id = self.requireActiveSignalRecordId(desc.condition.record);
                self.appendActiveStructuralSignalRoute(ctx, record_id, .{
                    .kind = .when,
                    .index = when_base + offset,
                });
            }
            self.active_stream.whens.appendSlice(allocator, replacement.whens.items) catch @panic("out of memory");
            replacement.whens.items.len = 0;

            const each_base = self.active_stream.eaches.items.len;
            for (replacement.eaches.items, 0..) |desc, offset| {
                self.active_stream.recordEachIndex(allocator, desc.node_id, each_base + offset);
                self.active_stream.rememberSignalRecordTree(allocator, desc.items.record);
                self.retainActiveSignalRecord(ctx, desc.items.record);
                const record_id = self.requireActiveSignalRecordId(desc.items.record);
                self.appendActiveStructuralSignalRoute(ctx, record_id, .{
                    .kind = .each,
                    .index = each_base + offset,
                });
            }
            self.active_stream.eaches.appendSlice(allocator, replacement.eaches.items) catch @panic("out of memory");
            replacement.eaches.items.len = 0;
        }

        fn validateActiveRenderDescriptorIntegrity(self: *Self) void {
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
                    @panic(rendered);
                }
            }
        }

        pub fn spliceActiveStreamReplacingTarget(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, target: HostStructuralReplacementTarget, render_insert_index: usize, replacement: *HostNodeDescriptorStream) HostStructuralSplice {
            const allocator = Ctx.allocator(ctx);
            if (render_insert_index > self.active_stream.render_nodes.items.len) @panic("structural replacement render insertion point is outside the active stream");

            const target_scopes = self.buildReplacementTargetScopeSet(ctx, target);
            defer self.scratch.replacement_target_scopes.clearRetainingCapacity();

            var removed_elem_ids: std.ArrayListUnmanaged(u64) = .empty;
            errdefer removed_elem_ids.deinit(allocator);
            var touched_parent_ids: std.ArrayListUnmanaged(u64) = .empty;
            errdefer touched_parent_ids.deinit(allocator);

            var removed_render_count: usize = 0;
            var target_scan_count: usize = 0;
            var render_index = render_insert_index;
            while (render_index < self.active_stream.render_nodes.items.len) : (render_index += 1) {
                const node = self.active_stream.render_nodes.items[render_index];
                target_scan_count += 1;
                if (!self.renderNodeInReplacementTargetSet(&self.active_stream, node, target_scopes)) break;
                removed_render_count += 1;
                removed_elem_ids.append(allocator, node.elem_id) catch @panic("out of memory");
                appendUniqueU64(allocator, &touched_parent_ids, renderNodeParentElemId(&self.active_stream, node));
            }
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_splice, target_scan_count);

            var touched_parent_write_index: usize = 0;
            for (touched_parent_ids.items) |parent_elem_id| {
                if (u64SliceContains(removed_elem_ids.items, parent_elem_id)) continue;
                touched_parent_ids.items[touched_parent_write_index] = parent_elem_id;
                touched_parent_write_index += 1;
            }
            touched_parent_ids.items.len = touched_parent_write_index;

            const render_start = render_insert_index;
            const removed_render_nodes = self.active_stream.render_nodes.items[render_start..][0..removed_render_count];
            const replacement_render_count = replacement.render_nodes.items.len;
            const on_change_count = replacement.on_changes.items.len;
            const mount_count = replacement.mounts.items.len;
            const replacement_elem_ids = allocator.alloc(u64, replacement_render_count) catch @panic("out of memory");
            errdefer allocator.free(replacement_elem_ids);
            for (replacement.render_nodes.items, 0..) |node, index| {
                replacement_elem_ids[index] = node.elem_id;
            }

            self.active_stream.replaceRenderRangeWithStream(allocator, render_start, removed_render_nodes, replacement, &self.pending_roc_metrics);
            self.removeActiveNonRenderDescriptorsInTarget(ctx, roc_host, target_scopes, removed_elem_ids.items);
            self.adjustActiveScopeSiteRenderInsertIndices(render_insert_index, removed_render_count, replacement_render_count);
            const on_change_start = self.active_stream.on_changes.items.len;
            const replacement_on_change_indices = allocator.alloc(usize, on_change_count) catch @panic("out of memory");
            errdefer allocator.free(replacement_on_change_indices);
            for (replacement_on_change_indices, 0..) |*index, offset| {
                index.* = on_change_start + offset;
            }
            const mount_start = self.active_stream.mounts.items.len;
            const replacement_mount_indices = allocator.alloc(usize, mount_count) catch @panic("out of memory");
            errdefer allocator.free(replacement_mount_indices);
            for (replacement_mount_indices, 0..) |*index, offset| {
                index.* = mount_start + offset;
            }
            self.appendReplacementNonRenderDescriptorsMoved(ctx, replacement, render_insert_index);
            self.validateActiveRenderDescriptorIntegrity();

            return .{
                .removed_elem_ids = removed_elem_ids.toOwnedSlice(allocator) catch @panic("out of memory"),
                .touched_parent_ids = touched_parent_ids.toOwnedSlice(allocator) catch @panic("out of memory"),
                .replacement_elem_ids = replacement_elem_ids,
                .replacement_on_change_indices = replacement_on_change_indices,
                .replacement_mount_indices = replacement_mount_indices,
            };
        }

        pub fn spliceActiveStreamReplacingScope(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, replaced_scope_id: u64, render_insert_index: usize, replacement: *HostNodeDescriptorStream) HostStructuralSplice {
            return self.spliceActiveStreamReplacingTarget(ctx, roc_host, .{ .scope = replaced_scope_id }, render_insert_index, replacement);
        }

        pub fn replaceSignalExprCacheAndClone(self: *Self, ctx: Ctx.Handle, cache_slot: *HostSignalCacheSlot, roc_host: *abi.RocHost, value: HostValue, cap: HostValueCapability) HostValue {
            cache_slot.replace(ctx, roc_host, &self.pending_roc_metrics, value, cap);
            return self.cloneCachedSignalValue(ctx, cache_slot);
        }

        pub fn evalHostSignalRecord(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, record: *HostSignalRecord) HostValue {
            switch (record.payload) {
                .ref => |node_id| return Ctx.stateValueByNodeId(ctx, node_id),
                .const_value => |*payload| {
                    const value = erased_calls.callValueInitThunk(roc_host, payload.init);
                    return self.replaceSignalExprCacheAndClone(ctx, &payload.cached_value, roc_host, value, payload.cap);
                },
                .map => |*payload| {
                    const input = self.evalHostSignalRecord(ctx, roc_host, payload.input);
                    defer self.dropHostSignalRecordValue(ctx, roc_host, payload.input, input);
                    self.recordDerivedCall();
                    const input_cap = self.hostSignalRecordCapability(ctx, payload.input);
                    const value = callHostValueToHostValueWithCapability(ctx, roc_host, input_cap, payload.transform, input);
                    return self.replaceSignalExprCacheAndClone(ctx, &payload.cached_value, roc_host, value, payload.cap);
                },
                .map2 => |*payload| {
                    const left = self.evalHostSignalRecord(ctx, roc_host, payload.left);
                    defer self.dropHostSignalRecordValue(ctx, roc_host, payload.left, left);
                    const right = self.evalHostSignalRecord(ctx, roc_host, payload.right);
                    defer self.dropHostSignalRecordValue(ctx, roc_host, payload.right, right);
                    self.recordDerivedCall();
                    const left_cap = self.hostSignalRecordCapability(ctx, payload.left);
                    const right_cap = self.hostSignalRecordCapability(ctx, payload.right);
                    const value = callHostValueHostValueToHostValueWithCapabilities(ctx, roc_host, left_cap, right_cap, payload.transform, left, right);
                    return self.replaceSignalExprCacheAndClone(ctx, &payload.cached_value, roc_host, value, payload.cap);
                },
                .combine => |*payload| {
                    const allocator = Ctx.allocator(ctx);
                    var values: std.ArrayListUnmanaged(HostValue) = .empty;
                    errdefer {
                        for (payload.children, values.items) |child, value| {
                            self.dropHostSignalRecordValue(ctx, roc_host, child, value);
                        }
                        values.deinit(allocator);
                    }
                    for (payload.children) |child| {
                        values.append(allocator, self.evalHostSignalRecord(ctx, roc_host, child)) catch @panic("out of memory");
                    }
                    const list = HostValueList.fromSlice(values.items, roc_host);
                    defer list.decref(roc_host);
                    self.recordDerivedCall();
                    const input_cap = if (payload.children.len == 0) payload.cap else self.hostSignalRecordCapability(ctx, payload.children[0]);
                    const value = callHostValueListToHostValueWithCapability(ctx, roc_host, input_cap, payload.transform, list);
                    for (payload.children, values.items) |child, child_value| {
                        self.dropHostSignalRecordValue(ctx, roc_host, child, child_value);
                    }
                    values.deinit(allocator);
                    return self.replaceSignalExprCacheAndClone(ctx, &payload.cached_value, roc_host, value, payload.cap);
                },
                .task_source => |*payload| {
                    switch (payload.cached_value) {
                        .present => return self.cloneCachedSignalValue(ctx, &payload.cached_value),
                        .absent => {
                            const value = erased_calls.callValueInitThunk(roc_host, payload.initial);
                            return self.replaceSignalExprCacheAndClone(ctx, &payload.cached_value, roc_host, value, payload.cap);
                        },
                    }
                },
                .interval_source => |*payload| {
                    switch (payload.cached_value) {
                        .present => return self.cloneCachedSignalValue(ctx, &payload.cached_value),
                        .absent => {
                            const value = erased_calls.callValueInitThunk(roc_host, payload.initial);
                            return self.replaceSignalExprCacheAndClone(ctx, &payload.cached_value, roc_host, value, payload.cap);
                        },
                    }
                },
            }
        }

        pub fn evalHostSignalBinding(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, signal: *HostSignalBinding) HostValue {
            return self.evalHostSignalRecord(ctx, roc_host, signal.record);
        }

        pub fn evalSignalTextField(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, elem_id: u64, field: RenderTextField, signal: *HostSignalBinding, read: HostTextRead, cache_slot: *HostSignalCacheSlot) bool {
            const value = self.evalHostSignalBinding(ctx, roc_host, signal);
            const signal_cap = self.hostSignalBindingCapability(ctx, signal);
            assertHostValueCapabilitiesMatch(read.capability, signal_cap, "text read extension capability did not match its signal value");
            const text = callHostValueToStrWithCapability(ctx, roc_host, read.capability, read.read, value);
            defer text.decref(roc_host);
            const changed = self.applyRenderTextField(ctx, elem_id, field, text.asSlice());
            cache_slot.replace(ctx, roc_host, &self.pending_roc_metrics, value, signal_cap);
            return changed;
        }

        pub fn evalSignalTextAttr(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, elem_id: u64, name: []const u8, signal: *HostSignalBinding, read: HostTextRead, cache_slot: *HostSignalCacheSlot) bool {
            const value = self.evalHostSignalBinding(ctx, roc_host, signal);
            const signal_cap = self.hostSignalBindingCapability(ctx, signal);
            assertHostValueCapabilitiesMatch(read.capability, signal_cap, "text attr read extension capability did not match its signal value");
            const text = callHostValueToStrWithCapability(ctx, roc_host, read.capability, read.read, value);
            defer text.decref(roc_host);
            const changed = self.applyRenderTextAttr(ctx, elem_id, name, text.asSlice());
            cache_slot.replace(ctx, roc_host, &self.pending_roc_metrics, value, signal_cap);
            return changed;
        }

        pub fn evalSignalBoolField(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, elem_id: u64, field: RenderBoolField, signal: *HostSignalBinding, read: HostBoolRead, cache_slot: *HostSignalCacheSlot) bool {
            const value = self.evalHostSignalBinding(ctx, roc_host, signal);
            const signal_cap = self.hostSignalBindingCapability(ctx, signal);
            assertHostValueCapabilitiesMatch(read.capability, signal_cap, "bool read extension capability did not match its signal value");
            const bool_value = callHostValueToBoolWithCapability(ctx, roc_host, read.capability, read.read, value);
            const changed = self.applyRenderBoolField(ctx, elem_id, field, bool_value);
            cache_slot.replace(ctx, roc_host, &self.pending_roc_metrics, value, signal_cap);
            return changed;
        }

        pub fn evalDirtySignalTextField(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, elem_id: u64, field: RenderTextField, signal: *HostSignalBinding, read: HostTextRead, cache_slot: *HostSignalCacheSlot, dirty_source_node_ids: []const u64, dirty_generation: u64) bool {
            const result = self.evalDirtyHostSignalBinding(ctx, roc_host, signal, dirty_source_node_ids, dirty_generation);
            const cap = self.hostSignalBindingCapability(ctx, signal);
            assertHostValueCapabilitiesMatch(read.capability, cap, "dirty text read extension capability did not match its signal value");
            if (!result.changed) {
                callHostValueToUnitWithCapability(ctx, roc_host, cap, hv.hostValueCapabilityDrop(cap), result.value);
                return false;
            }
            if (!self.updateDirtySignalCache(ctx, roc_host, cache_slot, result.value, cap)) return false;
            const text = callHostValueToStrWithCapability(ctx, roc_host, read.capability, read.read, result.value);
            defer text.decref(roc_host);
            return self.applyRenderTextField(ctx, elem_id, field, text.asSlice());
        }

        pub fn evalDirtySignalTextAttr(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, elem_id: u64, name: []const u8, signal: *HostSignalBinding, read: HostTextRead, cache_slot: *HostSignalCacheSlot, dirty_source_node_ids: []const u64, dirty_generation: u64) bool {
            const result = self.evalDirtyHostSignalBinding(ctx, roc_host, signal, dirty_source_node_ids, dirty_generation);
            const cap = self.hostSignalBindingCapability(ctx, signal);
            assertHostValueCapabilitiesMatch(read.capability, cap, "dirty text attr read extension capability did not match its signal value");
            if (!result.changed) {
                callHostValueToUnitWithCapability(ctx, roc_host, cap, hv.hostValueCapabilityDrop(cap), result.value);
                return false;
            }
            if (!self.updateDirtySignalCache(ctx, roc_host, cache_slot, result.value, cap)) return false;
            const text = callHostValueToStrWithCapability(ctx, roc_host, read.capability, read.read, result.value);
            defer text.decref(roc_host);
            return self.applyRenderTextAttr(ctx, elem_id, name, text.asSlice());
        }

        pub fn evalDirtySignalBoolField(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, elem_id: u64, field: RenderBoolField, signal: *HostSignalBinding, read: HostBoolRead, cache_slot: *HostSignalCacheSlot, dirty_source_node_ids: []const u64, dirty_generation: u64) bool {
            const result = self.evalDirtyHostSignalBinding(ctx, roc_host, signal, dirty_source_node_ids, dirty_generation);
            const cap = self.hostSignalBindingCapability(ctx, signal);
            assertHostValueCapabilitiesMatch(read.capability, cap, "dirty bool read extension capability did not match its signal value");
            if (!result.changed) {
                callHostValueToUnitWithCapability(ctx, roc_host, cap, hv.hostValueCapabilityDrop(cap), result.value);
                return false;
            }
            if (!self.updateDirtySignalCache(ctx, roc_host, cache_slot, result.value, cap)) return false;
            return self.applyRenderBoolField(ctx, elem_id, field, callHostValueToBoolWithCapability(ctx, roc_host, read.capability, read.read, result.value));
        }

        pub fn evalStructuralSignalTextField(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, elem_id: u64, field: RenderTextField, signal: *HostSignalBinding, read: HostTextRead, cache_slot: *HostSignalCacheSlot, dirty_source_node_ids: []const u64, dirty_generation: u64) bool {
            if (dirty_generation != 0 and sourceNodeIdsIntersect(signal.source_node_ids, dirty_source_node_ids)) {
                return self.evalDirtySignalTextField(ctx, roc_host, elem_id, field, signal, read, cache_slot, dirty_source_node_ids, dirty_generation);
            }
            return self.evalSignalTextField(ctx, roc_host, elem_id, field, signal, read, cache_slot);
        }

        pub fn evalStructuralSignalTextAttr(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, elem_id: u64, name: []const u8, signal: *HostSignalBinding, read: HostTextRead, cache_slot: *HostSignalCacheSlot, dirty_source_node_ids: []const u64, dirty_generation: u64) bool {
            if (dirty_generation != 0 and sourceNodeIdsIntersect(signal.source_node_ids, dirty_source_node_ids)) {
                return self.evalDirtySignalTextAttr(ctx, roc_host, elem_id, name, signal, read, cache_slot, dirty_source_node_ids, dirty_generation);
            }
            return self.evalSignalTextAttr(ctx, roc_host, elem_id, name, signal, read, cache_slot);
        }

        pub fn evalStructuralSignalBoolField(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, elem_id: u64, field: RenderBoolField, signal: *HostSignalBinding, read: HostBoolRead, cache_slot: *HostSignalCacheSlot, dirty_source_node_ids: []const u64, dirty_generation: u64) bool {
            if (dirty_generation != 0 and sourceNodeIdsIntersect(signal.source_node_ids, dirty_source_node_ids)) {
                return self.evalDirtySignalBoolField(ctx, roc_host, elem_id, field, signal, read, cache_slot, dirty_source_node_ids, dirty_generation);
            }
            return self.evalSignalBoolField(ctx, roc_host, elem_id, field, signal, read, cache_slot);
        }

        pub fn evalDirtyHostSignalRecord(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, record: *HostSignalRecord, dirty_source_node_ids: []const u64, dirty_generation: u64) HostSignalEvalResult {
            if (dirty_generation == 0) @panic("dirty signal evaluation used generation 0");
            debugPhase(ctx, 400);
            if (self.cloneMemoizedDirtySignalResult(ctx, record, dirty_generation)) |result| return result;

            switch (record.payload) {
                .ref => |node_id| {
                    debugPhase(ctx, 401);
                    return .{
                        .value = Ctx.stateValueByNodeId(ctx, node_id),
                        .changed = u64SliceContains(dirty_source_node_ids, node_id),
                    };
                },
                .const_value => |*payload| {
                    if (payload.cached_value == .absent) {
                        debugPhase(ctx, 402);
                        const value = erased_calls.callValueInitThunk(roc_host, payload.init);
                        return self.rememberDirtySignalResult(record, dirty_generation, self.updateDirtySignalExprCache(ctx, roc_host, &payload.cached_value, value, payload.cap));
                    }
                    debugPhase(ctx, 403);
                    return self.rememberDirtySignalResult(record, dirty_generation, .{
                        .value = self.cloneCachedSignalValue(ctx, &payload.cached_value),
                        .changed = false,
                    });
                },
                .map => |*payload| {
                    const cache_was_absent = payload.cached_value == .absent;
                    debugPhase(ctx, 420);
                    const input = self.evalDirtyHostSignalRecord(ctx, roc_host, payload.input, dirty_source_node_ids, dirty_generation);
                    defer self.dropHostSignalRecordValue(ctx, roc_host, payload.input, input.value);
                    if (!input.changed and !cache_was_absent) {
                        debugPhase(ctx, 423);
                        return self.rememberDirtySignalResult(record, dirty_generation, .{ .value = self.cloneCachedSignalValue(ctx, &payload.cached_value), .changed = false });
                    }

                    self.recordDerivedCall();
                    debugPhase(ctx, 421);
                    const input_cap = self.hostSignalRecordCapability(ctx, payload.input);
                    const value = callHostValueToHostValueWithCapability(ctx, roc_host, input_cap, payload.transform, input.value);
                    debugPhase(ctx, 422);
                    return self.rememberDirtySignalResult(record, dirty_generation, self.updateDirtySignalExprCache(ctx, roc_host, &payload.cached_value, value, payload.cap));
                },
                .map2 => |*payload| {
                    const cache_was_absent = payload.cached_value == .absent;
                    debugPhase(ctx, 430);
                    const left = self.evalDirtyHostSignalRecord(ctx, roc_host, payload.left, dirty_source_node_ids, dirty_generation);
                    defer self.dropHostSignalRecordValue(ctx, roc_host, payload.left, left.value);
                    debugPhase(ctx, 431);
                    const right = self.evalDirtyHostSignalRecord(ctx, roc_host, payload.right, dirty_source_node_ids, dirty_generation);
                    defer self.dropHostSignalRecordValue(ctx, roc_host, payload.right, right.value);
                    if (!left.changed and !right.changed and !cache_was_absent) {
                        debugPhase(ctx, 434);
                        return self.rememberDirtySignalResult(record, dirty_generation, .{ .value = self.cloneCachedSignalValue(ctx, &payload.cached_value), .changed = false });
                    }

                    self.recordDerivedCall();
                    debugPhase(ctx, 432);
                    const left_cap = self.hostSignalRecordCapability(ctx, payload.left);
                    const right_cap = self.hostSignalRecordCapability(ctx, payload.right);
                    const value = callHostValueHostValueToHostValueWithCapabilities(ctx, roc_host, left_cap, right_cap, payload.transform, left.value, right.value);
                    debugPhase(ctx, 433);
                    return self.rememberDirtySignalResult(record, dirty_generation, self.updateDirtySignalExprCache(ctx, roc_host, &payload.cached_value, value, payload.cap));
                },
                .combine => |*payload| {
                    const cache_was_absent = payload.cached_value == .absent;
                    const allocator = Ctx.allocator(ctx);
                    var values: std.ArrayListUnmanaged(HostValue) = .empty;
                    errdefer {
                        for (payload.children[0..values.items.len], values.items) |child, value| {
                            self.dropHostSignalRecordValue(ctx, roc_host, child, value);
                        }
                        values.deinit(allocator);
                    }

                    var any_changed = false;
                    for (payload.children) |child| {
                        debugPhase(ctx, 440);
                        const child_result = self.evalDirtyHostSignalRecord(ctx, roc_host, child, dirty_source_node_ids, dirty_generation);
                        any_changed = any_changed or child_result.changed;
                        values.append(allocator, child_result.value) catch @panic("out of memory");
                    }

                    if (!any_changed and !cache_was_absent) {
                        debugPhase(ctx, 443);
                        for (payload.children, values.items) |child, value| {
                            self.dropHostSignalRecordValue(ctx, roc_host, child, value);
                        }
                        values.deinit(allocator);
                        return self.rememberDirtySignalResult(record, dirty_generation, .{ .value = self.cloneCachedSignalValue(ctx, &payload.cached_value), .changed = false });
                    }

                    const list = HostValueList.fromSlice(values.items, roc_host);
                    defer list.decref(roc_host);
                    self.recordDerivedCall();
                    debugPhase(ctx, 441);
                    const input_cap = if (payload.children.len == 0) payload.cap else self.hostSignalRecordCapability(ctx, payload.children[0]);
                    const value = callHostValueListToHostValueWithCapability(ctx, roc_host, input_cap, payload.transform, list);
                    debugPhase(ctx, 442);
                    for (payload.children, values.items) |child, child_value| {
                        self.dropHostSignalRecordValue(ctx, roc_host, child, child_value);
                    }
                    values.deinit(allocator);
                    return self.rememberDirtySignalResult(record, dirty_generation, self.updateDirtySignalExprCache(ctx, roc_host, &payload.cached_value, value, payload.cap));
                },
                .task_source => |*payload| {
                    debugPhase(ctx, 410);
                    return self.rememberDirtySignalResult(record, dirty_generation, .{
                        .value = self.cloneCachedSignalValue(ctx, &payload.cached_value),
                        .changed = record.last_dirty_generation == dirty_generation and record.last_dirty_changed,
                    });
                },
                .interval_source => |*payload| {
                    debugPhase(ctx, 411);
                    return self.rememberDirtySignalResult(record, dirty_generation, .{
                        .value = self.cloneCachedSignalValue(ctx, &payload.cached_value),
                        .changed = record.last_dirty_generation == dirty_generation and record.last_dirty_changed,
                    });
                },
            }
        }

        pub fn evalDirtyHostSignalBinding(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, signal: *HostSignalBinding, dirty_source_node_ids: []const u64, dirty_generation: u64) HostSignalEvalResult {
            return self.evalDirtyHostSignalRecord(ctx, roc_host, signal.record, dirty_source_node_ids, dirty_generation);
        }

        pub fn propagateDirtyActiveSignals(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, allocator: std.mem.Allocator, dirty_source_node_ids: []const u64, dirty_generation: u64) []u64 {
            const dirty_record_ids = self.dirtyActiveSignalRecordIdsForSources(allocator, dirty_source_node_ids);
            defer allocator.free(dirty_record_ids);
            return self.propagateDirtyActiveSignalRecordIds(ctx, roc_host, allocator, dirty_record_ids, dirty_source_node_ids, dirty_generation);
        }

        pub fn propagateDirtyActiveSignalRecordIds(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, allocator: std.mem.Allocator, dirty_record_ids: []const u64, dirty_source_node_ids: []const u64, dirty_generation: u64) []u64 {
            var changed_record_ids: std.ArrayListUnmanaged(u64) = .empty;
            errdefer changed_record_ids.deinit(allocator);

            for (dirty_record_ids) |record_id| {
                const record = self.active_signal_graph.items[@intCast(record_id)].record;
                debugPhase(ctx, 331);
                const result = self.evalDirtyHostSignalRecord(ctx, roc_host, record, dirty_source_node_ids, dirty_generation);
                if (result.changed) {
                    changed_record_ids.append(allocator, record_id) catch @panic("out of memory");
                }
                debugPhase(ctx, 332);
                self.dropHostSignalRecordValue(ctx, roc_host, record, result.value);
            }

            return changed_record_ids.toOwnedSlice(allocator) catch @panic("out of memory");
        }

        pub fn collectDirtyStructuralSignals(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, allocator: std.mem.Allocator, dirty_source_node_ids: []const u64, changed_record_ids: []const u64, dirty_generation: u64) []HostDirtyStructuralSignal {
            var dirty_structural_signals: std.ArrayListUnmanaged(HostDirtyStructuralSignal) = .empty;
            errdefer dirty_structural_signals.deinit(allocator);

            for (changed_record_ids) |record_id| {
                const route_index: usize = @intCast(record_id);
                if (route_index >= self.active_structural_signal_routes.items.len) continue;

                for (self.active_structural_signal_routes.items[route_index].items) |route| {
                    switch (route.kind) {
                        .when => {
                            const desc = &self.active_stream.whens.items[route.index];
                            const result = self.evalDirtyHostSignalBinding(ctx, roc_host, &desc.condition, dirty_source_node_ids, dirty_generation);
                            const cap = self.hostSignalBindingCapability(ctx, &desc.condition);
                            assertHostValueCapabilitiesMatch(desc.read.capability, cap, "dirty when read extension capability did not match its signal value");
                            if (!result.changed) {
                                callHostValueToUnitWithCapability(ctx, roc_host, cap, hv.hostValueCapabilityDrop(cap), result.value);
                                continue;
                            }
                            const active_branch: HostScopeBranch = if (callHostValueToBoolWithCapability(ctx, roc_host, desc.read.capability, desc.read.read, result.value)) .true_branch else .false_branch;
                            if (self.updateDirtySignalCache(ctx, roc_host, &desc.cached_value, result.value, cap)) {
                                dirty_structural_signals.append(allocator, .{
                                    .kind = .when,
                                    .node_id = desc.node_id,
                                    .branch = active_branch,
                                }) catch @panic("out of memory");
                            }
                        },
                        .each => {
                            const desc = &self.active_stream.eaches.items[route.index];
                            const result = self.evalDirtyHostSignalBinding(ctx, roc_host, &desc.items, dirty_source_node_ids, dirty_generation);
                            const cap = self.hostSignalBindingCapability(ctx, &desc.items);
                            if (!result.changed) {
                                callHostValueToUnitWithCapability(ctx, roc_host, cap, hv.hostValueCapabilityDrop(cap), result.value);
                                continue;
                            }
                            if (self.updateDirtySignalCache(ctx, roc_host, &desc.cached_value, result.value, cap)) {
                                dirty_structural_signals.append(allocator, .{
                                    .kind = .each,
                                    .node_id = desc.node_id,
                                }) catch @panic("out of memory");
                            }
                        },
                    }
                }
            }

            return dirty_structural_signals.toOwnedSlice(allocator) catch @panic("out of memory");
        }

        pub fn stateIndexByNodeId(self: *Self, node_id: u64) ?usize {
            if (node_id >= self.state_indexes_by_node_id.items.len) return null;
            const state_index = self.state_indexes_by_node_id.items[@intCast(node_id)] orelse return null;
            if (state_index >= self.states.items.len) @panic("state cell index exceeded state table");
            const state = self.states.items[state_index];
            if (!state.active or state.state_id != node_id) @panic("state cell index pointed at the wrong state");
            return state_index;
        }

        pub fn stateCapability(self: *Self, node_id: u64) StateLookupError!HostValueCapability {
            const state_index = self.stateIndexByNodeId(node_id) orelse return StateLookupError.MissingActiveState;
            return self.states.items[state_index].cell.cap;
        }

        pub fn activeEventReducerByIndex(self: *Self, event_index: usize) ActiveEventLookupError!HostEventReducer {
            if (event_index >= self.active_events.items.len) return ActiveEventLookupError.MissingActiveEvent;
            return self.active_events.items[event_index].payload_reducer;
        }

        pub fn activeScopeSiteByNodeId(self: *Self, node_id: u64, kind: HostNodeScopeSiteKind) ?HostNodeScopeSiteDesc {
            const descriptor_index = self.active_stream.nodeDescriptorIndex(node_id) orelse return null;
            const scope_site_index = descriptor_index.scope_sites.get(kind) orelse return null;
            if (scope_site_index >= self.active_stream.scope_sites.items.len) @panic("active scope site index exceeded descriptor table");
            const site = self.active_stream.scope_sites.items[scope_site_index];
            if (site.node_id != node_id or site.kind != kind) @panic("active scope site index pointed at the wrong node");
            return site;
        }

        pub fn activeWhenIndexByNodeId(self: *Self, node_id: u64) ?usize {
            const descriptor_index = self.active_stream.nodeDescriptorIndex(node_id) orelse return null;
            const when_index = descriptor_index.when orelse return null;
            if (when_index >= self.active_stream.whens.items.len) @panic("active when index exceeded descriptor table");
            if (self.active_stream.whens.items[when_index].node_id != node_id) @panic("active when index pointed at the wrong node");
            return when_index;
        }

        pub fn activeEachIndexByNodeId(self: *Self, node_id: u64) ?usize {
            const descriptor_index = self.active_stream.nodeDescriptorIndex(node_id) orelse return null;
            const each_index = descriptor_index.each orelse return null;
            if (each_index >= self.active_stream.eaches.items.len) @panic("active each index exceeded descriptor table");
            if (self.active_stream.eaches.items[each_index].node_id != node_id) @panic("active each index pointed at the wrong node");
            return each_index;
        }

        pub fn recordSliceContains(records: []const *HostSignalRecord, record: *HostSignalRecord) bool {
            return active_graph.recordSliceContains(HostSignalRecord, records, record);
        }

        pub fn activeWhenBranchScopeId(self: *Self, parent_scope_id: u64, site_ordinal: u64, branch: HostScopeBranch) scope_tree.Error!?u64 {
            return scope_tree.activeWhenBranch(HostEachRowScopeStep, self.scopes.items, parent_scope_id, site_ordinal, branch);
        }

        pub fn validateScopeId(self: *Self, scope_id: u64) scope_tree.Error!void {
            return scope_tree.validate(HostEachRowScopeStep, self.scopes.items, scope_id);
        }

        pub fn internRootScope(self: *Self, allocator: std.mem.Allocator) scope_tree.Error!scope_tree.InternResult {
            const result = try scope_tree.internRoot(HostEachRowScopeStep, allocator, &self.scopes);
            if (result.created) self.recordScopeCreated();
            return result;
        }

        pub fn internComponentScope(self: *Self, allocator: std.mem.Allocator, parent_scope_id: u64, site_ordinal: u64) scope_tree.Error!scope_tree.InternResult {
            const result = try scope_tree.internComponent(HostEachRowScopeStep, allocator, &self.scopes, parent_scope_id, site_ordinal);
            if (result.created) self.recordScopeCreated();
            return result;
        }

        pub fn internWhenBranchScope(self: *Self, allocator: std.mem.Allocator, parent_scope_id: u64, site_ordinal: u64, branch: HostScopeBranch) scope_tree.Error!scope_tree.InternResult {
            const result = try scope_tree.internWhenBranch(HostEachRowScopeStep, allocator, &self.scopes, parent_scope_id, site_ordinal, branch);
            if (result.created) self.recordScopeCreated();
            return result;
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
            try self.validateScopeId(parent_scope_id);
            const site_index = self.activeEachRowSiteIndex(parent_scope_id, site_ordinal) orelse {
                return allocator.alloc(u64, 0) catch return scope_tree.Error.OutOfMemory;
            };
            return allocator.dupe(u64, self.each_row_sites.items[site_index].scope_ids.items) catch return scope_tree.Error.OutOfMemory;
        }

        pub fn eachRowScopeKeyEquals(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, scope_id: u64, key: HostValue) bool {
            self.validateScopeId(scope_id) catch @panic("scope id has no host scope descriptor");
            const scope = &self.scopes.items[@intCast(scope_id)];
            self.recordEachKeyReuseCompare();
            return switch (scope.step) {
                .each_row => |*row| row.key.valueEquals(ctx, roc_host, key),
                .root, .component, .when_branch => @panic("scope id does not reference an each-row scope"),
            };
        }

        pub fn eachRowScopeKeyValue(self: *Self, scope_id: u64) HostValue {
            self.validateScopeId(scope_id) catch @panic("scope id has no host scope descriptor");
            const scope = &self.scopes.items[@intCast(scope_id)];
            return switch (scope.step) {
                .each_row => |row| row.key.value,
                .root, .component, .when_branch => @panic("scope id does not reference an each-row scope"),
            };
        }

        pub fn eachRowScopeKeyHash(self: *Self, scope_id: u64) u64 {
            self.validateScopeId(scope_id) catch @panic("scope id has no host scope descriptor");
            const scope = &self.scopes.items[@intCast(scope_id)];
            return switch (scope.step) {
                .each_row => |row| row.key_hash,
                .root, .component, .when_branch => @panic("scope id does not reference an each-row scope"),
            };
        }

        pub fn hashEachKeyValue(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, key_text: abi.RocErasedCallable, key_cap: HostValueCapability, key: HostValue) u64 {
            self.recordEachKeyHash();
            const text = callHostValueToStrWithCapability(ctx, roc_host, key_cap, key_text, key);
            defer text.decref(roc_host);
            return hashEachKeyText(text.asSlice());
        }

        pub fn eachKeysEqual(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, ops: HostEachOps, left: HostValue, right: HostValue) bool {
            self.recordEachKeyDuplicateCompare();
            const key_cap = ops.key_capability;
            return callHostValueHostValueToBoolWithCapability(ctx, roc_host, key_cap, hv.hostValueCapabilityEq(key_cap), left, right);
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

        pub fn activeEachRowRenderSegmentsInRenderOrder(self: *Self, allocator: std.mem.Allocator, site: HostEachSite) []HostEachRowRenderSegment {
            var segments: std.ArrayListUnmanaged(HostEachRowRenderSegment) = .empty;
            errdefer segments.deinit(allocator);
            var segment_indexes_by_scope: std.AutoHashMapUnmanaged(u64, usize) = .{};
            defer segment_indexes_by_scope.deinit(allocator);

            self.recordStreamNodesScannedBy(.stream_nodes_scanned_render_scope, self.active_stream.render_nodes.items.len);
            var render_index: usize = 0;
            while (render_index < self.active_stream.render_nodes.items.len) {
                const node = self.active_stream.render_nodes.items[render_index];
                const scope_id = renderNodeScopeId(&self.active_stream, node);
                const row_scope_id = (self.eachSiteRowAncestorScopeId(scope_id, site) catch @panic("scope descriptor referenced an unknown parent scope")) orelse {
                    render_index += 1;
                    continue;
                };
                const start = render_index;
                render_index += 1;
                while (render_index < self.active_stream.render_nodes.items.len) : (render_index += 1) {
                    const next_node = self.active_stream.render_nodes.items[render_index];
                    const next_scope_id = renderNodeScopeId(&self.active_stream, next_node);
                    const next_row_scope_id = self.eachSiteRowAncestorScopeId(next_scope_id, site) catch @panic("scope descriptor referenced an unknown parent scope");
                    if (next_row_scope_id == null or next_row_scope_id.? != row_scope_id) break;
                }

                const segment_index = segments.items.len;
                segments.append(allocator, .{
                    .scope_id = row_scope_id,
                    .start = start,
                    .len = render_index - start,
                }) catch @panic("out of memory");
                const entry = segment_indexes_by_scope.getOrPut(allocator, row_scope_id) catch @panic("out of memory");
                if (entry.found_existing) @panic("each row render nodes were split across multiple segments");
                entry.value_ptr.* = segment_index;
            }

            return segments.toOwnedSlice(allocator) catch @panic("out of memory");
        }

        pub fn eachRenderSegmentScopeIds(allocator: std.mem.Allocator, segments: []const HostEachRowRenderSegment) []u64 {
            const ids = allocator.alloc(u64, segments.len) catch @panic("out of memory");
            for (segments, ids) |segment, *id| {
                id.* = segment.scope_id;
            }
            return ids;
        }

        pub fn eachDiffIsPurePermutation(self: *Self, old_render_rows: []const u64, diff: HostKeyedRowDiffResult, dirty_source_node_ids: []const u64) bool {
            if (diff.rows_created != 0 or diff.rows_removed != 0) return false;
            if (diff.scope_ids.len != old_render_rows.len) return false;
            for (diff.scope_ids, diff.row_items_changed) |scope_id, row_item_changed| {
                if (row_item_changed) return false;
                if (!u64SliceContains(old_render_rows, scope_id)) return false;
                if (self.scopeSubtreeHasDirtyStructuralSource(&self.active_stream, scope_id, dirty_source_node_ids)) return false;
            }
            return true;
        }

        pub fn applyDirtyEachPermutationMoves(self: *Self, ctx: Ctx.Handle, site: HostNodeScopeSiteDesc, next_scope_ids: []const u64) render.Counts {
            if (site.kind != .each) @panic("dirty each permutation move received a non-each site");

            const allocator = Ctx.allocator(ctx);
            const each_site = HostEachSite{ .parent_scope_id = site.scope_id, .site_ordinal = site.ordinal };
            var segments: std.ArrayListUnmanaged(HostEachRowRenderSegment) = .empty;
            defer segments.deinit(allocator);
            var segment_indexes_by_scope: std.AutoHashMapUnmanaged(u64, usize) = .{};
            defer segment_indexes_by_scope.deinit(allocator);

            var render_index: usize = 0;
            while (render_index < self.active_stream.render_nodes.items.len) {
                const node = self.active_stream.render_nodes.items[render_index];
                const scope_id = renderNodeScopeId(&self.active_stream, node);
                const row_scope_id = (self.eachSiteRowAncestorScopeId(scope_id, each_site) catch @panic("scope descriptor referenced an unknown parent scope")) orelse {
                    render_index += 1;
                    continue;
                };
                const start = render_index;
                render_index += 1;
                while (render_index < self.active_stream.render_nodes.items.len) : (render_index += 1) {
                    const next_node = self.active_stream.render_nodes.items[render_index];
                    const next_scope_id = renderNodeScopeId(&self.active_stream, next_node);
                    const next_row_scope_id = self.eachSiteRowAncestorScopeId(next_scope_id, each_site) catch @panic("scope descriptor referenced an unknown parent scope");
                    if (next_row_scope_id == null or next_row_scope_id.? != row_scope_id) break;
                }

                const segment_index = segments.items.len;
                segments.append(allocator, .{
                    .scope_id = row_scope_id,
                    .start = start,
                    .len = render_index - start,
                }) catch @panic("out of memory");
                const entry = segment_indexes_by_scope.getOrPut(allocator, row_scope_id) catch @panic("out of memory");
                if (entry.found_existing) @panic("each row render nodes were split across multiple segments");
                entry.value_ptr.* = segment_index;
            }

            if (segments.items.len != next_scope_ids.len) @panic("pure each permutation did not cover every rendered row");
            if (segments.items.len == 0) return .{};

            const region_start = segments.items[0].start;
            var expected_start = region_start;
            var total_len: usize = 0;
            for (segments.items) |segment| {
                if (segment.start != expected_start) @panic("each row render segments were not contiguous");
                expected_start += segment.len;
                total_len += segment.len;
            }

            var moves_by_scope: std.AutoHashMapUnmanaged(u64, HostEachRowRenderMove) = .{};
            defer moves_by_scope.deinit(allocator);
            const reordered_nodes = allocator.alloc(HostRenderNode, total_len) catch @panic("out of memory");
            defer allocator.free(reordered_nodes);

            var write_index: usize = 0;
            for (next_scope_ids) |scope_id| {
                const segment_index = segment_indexes_by_scope.get(scope_id) orelse @panic("pure each permutation referenced a row without render nodes");
                const segment = segments.items[segment_index];
                const next_start = region_start + write_index;
                @memcpy(reordered_nodes[write_index..][0..segment.len], self.active_stream.render_nodes.items[segment.start..][0..segment.len]);
                moves_by_scope.put(allocator, scope_id, .{
                    .old_start = segment.start,
                    .new_start = next_start,
                    .len = segment.len,
                }) catch @panic("out of memory");
                write_index += segment.len;
            }

            if (write_index != total_len) @panic("pure each permutation wrote the wrong render-node count");
            @memcpy(self.active_stream.render_nodes.items[region_start..][0..total_len], reordered_nodes);

            var reordered_region_children: std.ArrayListUnmanaged(u64) = .empty;
            defer reordered_region_children.deinit(allocator);
            for (self.active_stream.render_nodes.items[region_start..][0..total_len]) |node| {
                if (renderNodeParentElemId(&self.active_stream, node) == site.parent_elem_id) {
                    reordered_region_children.append(allocator, node.elem_id) catch @panic("out of memory");
                }
            }

            var reordered_parent_children: std.ArrayListUnmanaged(u64) = .empty;
            defer reordered_parent_children.deinit(allocator);
            var inserted_region = false;
            const region_end = region_start + total_len;
            const old_parent_children = streamDirectChildren(allocator, &self.active_stream, site.parent_elem_id);
            defer allocator.free(old_parent_children);
            for (old_parent_children) |child_id| {
                const child_render_index = self.active_stream.renderNodeIndex(child_id) orelse @panic("parent child had no render index");
                const child_in_region = child_render_index >= region_start and child_render_index < region_end;
                if (child_in_region) {
                    if (!inserted_region) {
                        reordered_parent_children.appendSlice(allocator, reordered_region_children.items) catch @panic("out of memory");
                        inserted_region = true;
                    }
                    continue;
                }
                reordered_parent_children.append(allocator, child_id) catch @panic("out of memory");
            }
            if (!inserted_region and reordered_region_children.items.len != 0) {
                reordered_parent_children.appendSlice(allocator, reordered_region_children.items) catch @panic("out of memory");
            }
            self.active_stream.replaceRenderChildrenIndex(allocator, site.parent_elem_id, reordered_parent_children.items);
            self.active_stream.refreshRenderIndexesFrom(allocator, region_start, &self.pending_roc_metrics);

            for (self.active_stream.scope_sites.items) |*scope_site| {
                const row_scope_id = (self.eachSiteRowAncestorScopeId(scope_site.scope_id, each_site) catch @panic("scope descriptor referenced an unknown parent scope")) orelse continue;
                const move = moves_by_scope.get(row_scope_id) orelse @panic("scope site referenced a row missing from pure each permutation");
                if (scope_site.render_insert_index < move.old_start) @panic("scope site insertion point preceded its row render segment");
                const offset = scope_site.render_insert_index - move.old_start;
                if (offset > move.len) @panic("scope site insertion point exceeded its row render segment");
                scope_site.render_insert_index = move.new_start + offset;
            }

            var counts: render.Counts = .{};
            const children = streamDirectChildren(allocator, &self.active_stream, site.parent_elem_id);
            defer allocator.free(children);
            self.replaceRenderChildrenForMoves(ctx, site.parent_elem_id, children, &counts);
            if (comptime enable_runtime_metrics) self.render_metrics.addCommandCounts(counts);
            return counts;
        }

        pub fn renderInsertIndexForEachRowRanges(site: HostNodeScopeSiteDesc, row_ranges: *const std.AutoHashMapUnmanaged(u64, HostEachRowRenderSegment), next_scope_ids: []const u64, row_index: usize) usize {
            if (row_index >= next_scope_ids.len) @panic("each row insertion index was requested outside the next row order");

            if (row_ranges.get(next_scope_ids[row_index])) |existing| {
                return existing.start;
            }

            var next_index = row_index + 1;
            while (next_index < next_scope_ids.len) : (next_index += 1) {
                if (row_ranges.get(next_scope_ids[next_index])) |next| {
                    return next.start;
                }
            }

            var previous_index = row_index;
            while (previous_index > 0) {
                previous_index -= 1;
                if (row_ranges.get(next_scope_ids[previous_index])) |previous| {
                    return previous.start + previous.len;
                }
            }

            return site.render_insert_index;
        }

        pub fn adjustEachRowRenderRanges(row_ranges: *std.AutoHashMapUnmanaged(u64, HostEachRowRenderSegment), replace_index: usize, removed_count: usize, replacement_count: usize) void {
            var range_iterator = row_ranges.iterator();
            while (range_iterator.next()) |entry| {
                entry.value_ptr.start = adjustedRenderInsertIndex(entry.value_ptr.start, replace_index, removed_count, replacement_count);
            }
        }

        pub fn updateEachRowRenderRange(row_ranges: *std.AutoHashMapUnmanaged(u64, HostEachRowRenderSegment), allocator: std.mem.Allocator, scope_id: u64, render_insert_index: usize, removed_count: usize, replacement_count: usize) void {
            const removed_range = row_ranges.fetchRemove(scope_id);
            const old_len = if (removed_range) |entry| entry.value.len else 0;
            if (old_len != removed_count) @panic("each row render range length did not match splice removal count");
            adjustEachRowRenderRanges(row_ranges, render_insert_index, old_len, replacement_count);
            if (replacement_count != 0) {
                row_ranges.put(allocator, scope_id, .{
                    .scope_id = scope_id,
                    .start = render_insert_index,
                    .len = replacement_count,
                }) catch @panic("out of memory");
            }
        }

        pub fn applyDirtyEachRowScopeSplices(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, site: HostNodeScopeSiteDesc, each: HostNodeEachDesc, old_render_segments: []const HostEachRowRenderSegment, diff: HostKeyedRowDiffResult, dirty_source_node_ids: []const u64, dirty_generation: u64) render.Counts {
            const allocator = Ctx.allocator(ctx);
            var row_ranges: std.AutoHashMapUnmanaged(u64, HostEachRowRenderSegment) = .{};
            defer row_ranges.deinit(allocator);
            for (old_render_segments) |segment| {
                const entry = row_ranges.getOrPut(allocator, segment.scope_id) catch @panic("out of memory");
                if (entry.found_existing) @panic("each row render range index received duplicate row scopes");
                entry.value_ptr.* = segment;
            }

            var removed_elem_ids: std.ArrayListUnmanaged(u64) = .empty;
            defer removed_elem_ids.deinit(allocator);
            var touched_parent_ids: std.ArrayListUnmanaged(u64) = .empty;
            defer touched_parent_ids.deinit(allocator);
            var replacement_elem_ids: std.ArrayListUnmanaged(u64) = .empty;
            defer replacement_elem_ids.deinit(allocator);
            var replacement_on_change_indices: std.ArrayListUnmanaged(usize) = .empty;
            defer replacement_on_change_indices.deinit(allocator);
            var replacement_mount_indices: std.ArrayListUnmanaged(usize) = .empty;
            defer replacement_mount_indices.deinit(allocator);
            var spliced_any = false;

            for (diff.removed_scope_ids) |removed_scope_id| {
                var empty_stream: HostNodeDescriptorStream = .{};
                const removed_range = row_ranges.get(removed_scope_id);
                const render_insert_index = if (removed_range) |range| range.start else site.render_insert_index;
                const splice = self.spliceActiveStreamReplacingScope(ctx, roc_host, removed_scope_id, render_insert_index, &empty_stream);
                defer splice.deinit(allocator);
                updateEachRowRenderRange(&row_ranges, allocator, removed_scope_id, render_insert_index, splice.removed_elem_ids.len, splice.replacement_elem_ids.len);

                removed_elem_ids.appendSlice(allocator, splice.removed_elem_ids) catch @panic("out of memory");
                for (splice.touched_parent_ids) |parent_id| {
                    appendUniqueU64(allocator, &touched_parent_ids, parent_id);
                }
                replacement_elem_ids.appendSlice(allocator, splice.replacement_elem_ids) catch @panic("out of memory");
                replacement_on_change_indices.appendSlice(allocator, splice.replacement_on_change_indices) catch @panic("out of memory");
                replacement_mount_indices.appendSlice(allocator, splice.replacement_mount_indices) catch @panic("out of memory");
                spliced_any = true;
            }

            for (diff.scope_ids, diff.row_items_changed, 0..) |row_scope_id, row_item_changed, row_index| {
                if (!row_item_changed and !self.scopeSubtreeHasDirtyStructuralSource(&self.active_stream, row_scope_id, dirty_source_node_ids)) {
                    continue;
                }

                var row_stream: HostNodeDescriptorStream = .{};
                defer row_stream.deinit(allocator, ctx, roc_host, &self.pending_roc_metrics);
                self.collectActiveEachSingleRowDescriptors(ctx, roc_host, &row_stream, site, each, row_scope_id, diff.scope_created[row_index], dirty_source_node_ids);

                const render_insert_index = renderInsertIndexForEachRowRanges(site, &row_ranges, diff.scope_ids, row_index);
                const splice = self.spliceActiveStreamReplacingScope(ctx, roc_host, row_scope_id, render_insert_index, &row_stream);
                defer splice.deinit(allocator);
                updateEachRowRenderRange(&row_ranges, allocator, row_scope_id, render_insert_index, splice.removed_elem_ids.len, splice.replacement_elem_ids.len);

                removed_elem_ids.appendSlice(allocator, splice.removed_elem_ids) catch @panic("out of memory");
                for (splice.touched_parent_ids) |parent_id| {
                    appendUniqueU64(allocator, &touched_parent_ids, parent_id);
                }
                replacement_elem_ids.appendSlice(allocator, splice.replacement_elem_ids) catch @panic("out of memory");
                replacement_on_change_indices.appendSlice(allocator, splice.replacement_on_change_indices) catch @panic("out of memory");
                replacement_mount_indices.appendSlice(allocator, splice.replacement_mount_indices) catch @panic("out of memory");
                spliced_any = true;
            }

            if (!spliced_any) return .{};

            const merged_splice = HostStructuralSplice{
                .removed_elem_ids = removed_elem_ids.toOwnedSlice(allocator) catch @panic("out of memory"),
                .touched_parent_ids = touched_parent_ids.toOwnedSlice(allocator) catch @panic("out of memory"),
                .replacement_elem_ids = replacement_elem_ids.toOwnedSlice(allocator) catch @panic("out of memory"),
                .replacement_on_change_indices = replacement_on_change_indices.toOwnedSlice(allocator) catch @panic("out of memory"),
                .replacement_mount_indices = replacement_mount_indices.toOwnedSlice(allocator) catch @panic("out of memory"),
            };
            defer merged_splice.deinit(allocator);
            const target = HostStructuralReplacementTarget{ .each_site = .{ .parent_scope_id = site.scope_id, .site_ordinal = site.ordinal } };
            return self.applySplicedStructuralNodeDescriptorTarget(ctx, roc_host, merged_splice, .{
                .removed = target,
                .replacement = target,
            }, dirty_source_node_ids, dirty_generation);
        }

        pub fn applyDirtyEachMixedRowSplicesAndMoves(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, site: HostNodeScopeSiteDesc, each: HostNodeEachDesc, old_render_segments: []const HostEachRowRenderSegment, diff: HostKeyedRowDiffResult, dirty_source_node_ids: []const u64, dirty_generation: u64) render.Counts {
            var counts = self.applyDirtyEachRowScopeSplices(ctx, roc_host, site, each, old_render_segments, diff, dirty_source_node_ids, dirty_generation);
            counts.addAll(self.applyDirtyEachPermutationMoves(ctx, site, diff.scope_ids));
            return counts;
        }

        pub fn evalOnChangeInitial(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, desc: *HostNodeOnChangeDesc) void {
            const value = self.evalHostSignalBinding(ctx, roc_host, &desc.signal);
            desc.cached_value.replace(ctx, roc_host, &self.pending_roc_metrics, value, self.hostSignalBindingCapability(ctx, &desc.signal));
        }

        pub fn evalMountCommand(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, desc: *HostNodeMountDesc) render.Counts {
            if (!desc.run_on_mount) return .{};
            desc.run_on_mount = false;

            const cmd = erased_calls.callUnitToStartTaskCmd(roc_host, desc.to_cmd);
            defer abi.decref__AnonStruct85(cmd, roc_host);
            return self.startTaskCommand(ctx, roc_host, desc.scope_id, cmd);
        }

        pub fn runActiveMountCommandIndices(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, indices: []const usize) render.Counts {
            var counts: render.Counts = .{};
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_mounts, indices.len);
            for (indices) |mount_index| {
                if (mount_index >= self.active_stream.mounts.items.len) @panic("mount descriptor index exceeded active descriptor stream");
                counts.addAll(self.evalMountCommand(ctx, roc_host, &self.active_stream.mounts.items[mount_index]));
            }
            return counts;
        }

        pub fn runActiveMountCommands(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost) render.Counts {
            var counts: render.Counts = .{};
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_mounts, self.active_stream.mounts.items.len);
            for (self.active_stream.mounts.items) |*desc| {
                counts.addAll(self.evalMountCommand(ctx, roc_host, desc));
            }
            return counts;
        }

        pub fn applyStructuralEventBindings(self: *Self, ctx: Ctx.Handle, stream: *const HostNodeDescriptorStream, seen: []const bool, counts: *render.Counts) void {
            const allocator = Ctx.allocator(ctx);
            var required = allocator.alloc(HostRequiredEventBindings, seen.len) catch @panic("out of memory");
            defer allocator.free(required);
            @memset(required, .{});

            self.recordStreamNodesScannedBy(.stream_nodes_scanned_events, stream.events.items.len);
            for (stream.events.items, 0..) |desc, index| {
                if (desc.elem_id >= seen.len or !seen[@intCast(desc.elem_id)]) {
                    @panic("event descriptor referenced an element outside the structural render stream");
                }
                const event_id: u64 = @intCast(index + 1);
                const slot = requiredEventBindingSlot(&required[@intCast(desc.elem_id)], desc.kind);
                if (slot.* != null) @panic("element has duplicate event descriptors for one event kind");
                slot.* = .{ .event_id = event_id, .payload_accessor = desc.payload_accessor };
            }
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_events, stream.named_events.items.len);
            for (stream.named_events.items) |desc| {
                if (desc.elem_id >= seen.len or !seen[@intCast(desc.elem_id)]) {
                    @panic("named event descriptor referenced an element outside the structural render stream");
                }
            }

            for (seen, 0..) |is_seen, index| {
                if (index == 0 or !is_seen) continue;

                for (render_event_kinds) |kind| {
                    const next_binding = requiredEventBindingSlot(&required[index], kind).*;
                    self.applyRenderEventBinding(ctx, @intCast(index), kind, next_binding, counts);
                }
                self.applyStructuralNamedEventBindingsForElem(ctx, stream, @intCast(index), counts);
            }
        }

        pub fn applyStructuralEventBindingsForSeen(self: *Self, ctx: Ctx.Handle, stream: *const HostNodeDescriptorStream, seen: []const bool, counts: *render.Counts) void {
            const allocator = Ctx.allocator(ctx);
            var required = allocator.alloc(HostRequiredEventBindings, seen.len) catch @panic("out of memory");
            defer allocator.free(required);
            @memset(required, .{});

            self.recordStreamNodesScannedBy(.stream_nodes_scanned_events, stream.events.items.len);
            for (stream.events.items, 0..) |desc, index| {
                if (desc.elem_id >= seen.len or !seen[@intCast(desc.elem_id)]) continue;
                const event_id: u64 = @intCast(index + 1);
                const slot = requiredEventBindingSlot(&required[@intCast(desc.elem_id)], desc.kind);
                if (slot.* != null) @panic("element has duplicate event descriptors for one event kind");
                slot.* = .{ .event_id = event_id, .payload_accessor = desc.payload_accessor };
            }

            for (seen, 0..) |is_seen, index| {
                if (index == 0 or !is_seen) continue;

                for (render_event_kinds) |kind| {
                    const next_binding = requiredEventBindingSlot(&required[index], kind).*;
                    self.applyRenderEventBinding(ctx, @intCast(index), kind, next_binding, counts);
                }
                self.applyStructuralNamedEventBindingsForElem(ctx, stream, @intCast(index), counts);
            }
        }

        pub fn namedEventBindingForElemName(stream: *const HostNodeDescriptorStream, elem_id: u64, name: []const u8) ?HostRequiredNamedEventBinding {
            const fixed_event_count = stream.events.items.len;
            for (stream.named_events.items, 0..) |desc, index| {
                if (desc.elem_id == elem_id and std.mem.eql(u8, desc.name, name)) {
                    return .{
                        .event_id = @intCast(fixed_event_count + index + 1),
                        .options = desc.options,
                        .payload_kind = desc.payload_kind,
                        .payload_accessor = desc.payload_accessor,
                    };
                }
            }
            return null;
        }

        pub fn applyStructuralNamedEventBindingsForElem(self: *Self, ctx: Ctx.Handle, stream: *const HostNodeDescriptorStream, elem_id: u64, counts: *render.Counts) void {
            var cache_index: usize = 0;
            while (self.render_cache.namedEventNameAt(elem_id, cache_index)) |name| {
                if (namedEventBindingForElemName(stream, elem_id, name) == null) {
                    self.applyRenderNamedEventBinding(ctx, elem_id, name, null, counts);
                    continue;
                }
                cache_index += 1;
            }

            self.recordStreamNodesScannedBy(.stream_nodes_scanned_events, stream.named_events.items.len);
            for (stream.named_events.items, 0..) |desc, index| {
                if (desc.elem_id != elem_id) continue;
                const event_id: u64 = @intCast(stream.events.items.len + index + 1);
                self.applyRenderNamedEventBinding(ctx, elem_id, desc.name, .{
                    .event_id = event_id,
                    .options = desc.options,
                    .payload_kind = desc.payload_kind,
                    .payload_accessor = desc.payload_accessor,
                }, counts);
            }
        }

        pub fn activeEventBindingForElemKind(self: *Self, elem_id: u64, kind: RenderEventKind) ?HostRequiredEventBinding {
            const descriptor_index = self.active_stream.elemDescriptorIndex(elem_id) orelse return null;
            const event_index = descriptor_index.events.get(kind) orelse return null;
            if (event_index >= self.active_stream.events.items.len) @panic("active event descriptor index exceeded descriptor table");
            const desc = self.active_stream.events.items[event_index];
            if (desc.elem_id != elem_id or desc.kind != kind) @panic("active event descriptor index pointed at the wrong event");
            return .{ .event_id = @intCast(event_index + 1), .payload_accessor = desc.payload_accessor };
        }

        pub fn applyStructuralEventBindingsForElem(self: *Self, ctx: Ctx.Handle, elem_id: u64, counts: *render.Counts) void {
            for (render_event_kinds) |kind| {
                const next_binding = self.activeEventBindingForElemKind(elem_id, kind);
                self.applyRenderEventBinding(ctx, elem_id, kind, next_binding, counts);
            }
            self.applyStructuralNamedEventBindingsForElem(ctx, &self.active_stream, elem_id, counts);
        }

        pub fn applyActiveStreamEventBindings(self: *Self, ctx: Ctx.Handle, counts: *render.Counts) void {
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_events, self.active_stream.render_nodes.items.len);
            for (self.active_stream.render_nodes.items) |node| {
                if (node.kind != .element) continue;
                self.applyStructuralEventBindingsForElem(ctx, node.elem_id, counts);
            }
        }

        pub fn applyActiveStreamTextAttrForElem(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, elem_id: u64, field: RenderTextField, descriptor_index: HostElemDescriptorIndex, counts: *render.Counts, dirty_source_node_ids: []const u64, dirty_generation: u64) void {
            if (descriptor_index.static_text_attrs.get(field)) |attr_index| {
                if (attr_index >= self.active_stream.static_text_attrs.items.len) @panic("active static text attr index exceeded descriptor table");
                const desc = self.active_stream.static_text_attrs.items[attr_index];
                if (desc.elem_id != elem_id or desc.field != field) @panic("active static text attr index pointed at the wrong field");
                if (self.applyRenderTextField(ctx, desc.elem_id, desc.field, desc.value)) {
                    counts.addTextField(desc.field);
                }
            }

            if (descriptor_index.signal_text_attrs.get(field)) |attr_index| {
                if (attr_index >= self.active_stream.signal_text_attrs.items.len) @panic("active signal text attr index exceeded descriptor table");
                const desc = &self.active_stream.signal_text_attrs.items[attr_index];
                if (desc.elem_id != elem_id or desc.field != field) @panic("active signal text attr index pointed at the wrong field");
                if (self.evalStructuralSignalTextField(ctx, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value, dirty_source_node_ids, dirty_generation)) {
                    counts.addTextField(desc.field);
                }
            }
        }

        pub fn clearRenderTextAttrsMissingFromStream(self: *Self, ctx: Ctx.Handle, stream: *const HostNodeDescriptorStream, elem_id: u64, counts: *render.Counts) void {
            var index: usize = 0;
            while (self.render_cache.customTextAttrNameAt(elem_id, index)) |name| {
                if (streamHasCustomTextAttr(stream, elem_id, name)) {
                    index += 1;
                    continue;
                }
                if (self.clearRenderTextAttr(ctx, elem_id, name)) {
                    counts.addTextAttr();
                }
            }
        }

        pub fn applyActiveStreamCustomTextAttrsForElem(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, elem_id: u64, counts: *render.Counts, dirty_source_node_ids: []const u64, dirty_generation: u64) void {
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_apply, self.active_stream.static_custom_text_attrs.items.len);
            for (self.active_stream.static_custom_text_attrs.items) |desc| {
                if (desc.elem_id != elem_id) continue;
                if (self.applyRenderTextAttr(ctx, desc.elem_id, desc.name, desc.value)) {
                    counts.addTextAttr();
                }
            }

            self.recordStreamNodesScannedBy(.stream_nodes_scanned_apply, self.active_stream.signal_custom_text_attrs.items.len);
            for (self.active_stream.signal_custom_text_attrs.items) |*desc| {
                if (desc.elem_id != elem_id) continue;
                if (self.evalStructuralSignalTextAttr(ctx, roc_host, desc.elem_id, desc.name, &desc.signal, desc.read, &desc.cached_value, dirty_source_node_ids, dirty_generation)) {
                    counts.addTextAttr();
                }
            }
        }

        pub fn applyActiveStreamBoolAttrForElem(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, elem_id: u64, field: RenderBoolField, descriptor_index: HostElemDescriptorIndex, counts: *render.Counts, dirty_source_node_ids: []const u64, dirty_generation: u64) void {
            if (descriptor_index.static_bool_attrs.get(field)) |attr_index| {
                if (attr_index >= self.active_stream.static_bool_attrs.items.len) @panic("active static bool attr index exceeded descriptor table");
                const desc = self.active_stream.static_bool_attrs.items[attr_index];
                if (desc.elem_id != elem_id or desc.field != field) @panic("active static bool attr index pointed at the wrong field");
                if (self.applyRenderBoolField(ctx, desc.elem_id, desc.field, desc.value)) {
                    counts.addBoolField(desc.field);
                }
            }

            if (descriptor_index.signal_bool_attrs.get(field)) |attr_index| {
                if (attr_index >= self.active_stream.signal_bool_attrs.items.len) @panic("active signal bool attr index exceeded descriptor table");
                const desc = &self.active_stream.signal_bool_attrs.items[attr_index];
                if (desc.elem_id != elem_id or desc.field != field) @panic("active signal bool attr index pointed at the wrong field");
                if (self.evalStructuralSignalBoolField(ctx, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value, dirty_source_node_ids, dirty_generation)) {
                    counts.addBoolField(desc.field);
                }
            }
        }

        pub fn applyActiveStreamFieldsForElem(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, elem_id: u64, counts: *render.Counts, dirty_source_node_ids: []const u64, dirty_generation: u64) void {
            const descriptor_index = self.active_stream.elemDescriptorIndex(elem_id) orelse @panic("active render node had no descriptor index");
            const text_fields = [_]RenderTextField{ .text, .role, .label, .test_id, .value, .class };
            const bool_fields = [_]RenderBoolField{ .checked, .disabled };

            for (text_fields) |field| {
                if (!streamHasTextField(&self.active_stream, elem_id, field) and self.clearRenderTextField(ctx, elem_id, field)) {
                    counts.addTextField(field);
                }
            }
            for (bool_fields) |field| {
                if (!streamHasBoolField(&self.active_stream, elem_id, field) and self.clearRenderBoolField(ctx, elem_id, field)) {
                    counts.addBoolField(field);
                }
            }
            self.clearRenderTextAttrsMissingFromStream(ctx, &self.active_stream, elem_id, counts);

            if (descriptor_index.text_node) |text_index| {
                if (text_index >= self.active_stream.text_nodes.items.len) @panic("active text node index exceeded descriptor table");
                const desc = self.active_stream.text_nodes.items[text_index];
                if (desc.elem_id != elem_id) @panic("active text node index pointed at the wrong elem id");
                if (self.applyRenderTextField(ctx, desc.elem_id, .text, desc.value)) {
                    counts.addTextField(.text);
                }
            }

            if (descriptor_index.signal_text_node) |signal_text_index| {
                if (signal_text_index >= self.active_stream.signal_text_nodes.items.len) @panic("active signal text node index exceeded descriptor table");
                const desc = &self.active_stream.signal_text_nodes.items[signal_text_index];
                if (desc.elem_id != elem_id) @panic("active signal text node index pointed at the wrong elem id");
                if (self.evalStructuralSignalTextField(ctx, roc_host, desc.elem_id, .text, &desc.signal, desc.read, &desc.cached_value, dirty_source_node_ids, dirty_generation)) {
                    counts.addTextField(.text);
                }
            }

            for (text_fields) |field| {
                self.applyActiveStreamTextAttrForElem(ctx, roc_host, elem_id, field, descriptor_index, counts, dirty_source_node_ids, dirty_generation);
            }
            self.applyActiveStreamCustomTextAttrsForElem(ctx, roc_host, elem_id, counts, dirty_source_node_ids, dirty_generation);
            for (bool_fields) |field| {
                self.applyActiveStreamBoolAttrForElem(ctx, roc_host, elem_id, field, descriptor_index, counts, dirty_source_node_ids, dirty_generation);
            }
        }

        pub fn applyStructuralNodeDescriptorTarget(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, targets: HostStructuralPatchTargets) render.Counts {
            if (!self.hasRenderRoot()) @panic("structural DOM patch requested before initial DOM root creation");

            const allocator = Ctx.allocator(ctx);
            const max_elem_id = @max(maxRenderElemId(&self.active_stream), maxRenderElemId(stream));
            const required_child_table_len: usize = @intCast(max_elem_id + 1);
            const child_table_len = required_child_table_len;

            var seen = allocator.alloc(bool, child_table_len) catch @panic("out of memory");
            defer allocator.free(seen);
            @memset(seen, false);

            var touched_parents: std.ArrayListUnmanaged(u64) = .empty;
            defer touched_parents.deinit(allocator);

            var counts: render.Counts = .{};

            self.recordStreamNodesScannedBy(.stream_nodes_scanned_apply, stream.render_nodes.items.len);
            for (stream.render_nodes.items) |node| {
                if (!self.renderNodeInReplacementTarget(stream, node, targets.replacement)) continue;
                if (node.elem_id >= child_table_len) @panic("render node exceeded structural DOM patch table");

                const parent_elem_id = renderNodeParentElemId(stream, node);
                if (parent_elem_id >= child_table_len) @panic("render node referenced parent outside structural DOM patch table");

                self.ensureRenderNode(ctx, node.elem_id, renderNodeTag(stream, node), &counts);
                seen[@intCast(node.elem_id)] = true;
                appendUniqueU64(allocator, &touched_parents, parent_elem_id);
            }

            self.recordStreamNodesScannedBy(.stream_nodes_scanned_apply, self.active_stream.render_nodes.items.len);
            for (self.active_stream.render_nodes.items) |node| {
                if (!self.renderNodeInReplacementTarget(&self.active_stream, node, targets.removed)) continue;
                if (node.elem_id < seen.len and seen[@intCast(node.elem_id)]) continue;
                self.removeRenderNode(ctx, node.elem_id, &counts);
            }

            for (touched_parents.items) |parent_elem_id| {
                const children = streamDirectChildren(allocator, stream, parent_elem_id);
                defer allocator.free(children);
                self.replaceRenderChildren(ctx, parent_elem_id, children, &counts);
            }

            const text_fields = [_]RenderTextField{ .text, .role, .label, .test_id, .value, .class };
            const bool_fields = [_]RenderBoolField{ .checked, .disabled };
            for (seen, 0..) |is_seen, index| {
                if (index == 0 or !is_seen) continue;
                const elem_id: u64 = @intCast(index);

                for (text_fields) |field| {
                    if (!streamHasTextField(stream, elem_id, field) and self.clearRenderTextField(ctx, elem_id, field)) {
                        counts.addTextField(field);
                    }
                }
                for (bool_fields) |field| {
                    if (!streamHasBoolField(stream, elem_id, field) and self.clearRenderBoolField(ctx, elem_id, field)) {
                        counts.addBoolField(field);
                    }
                }
                self.clearRenderTextAttrsMissingFromStream(ctx, stream, elem_id, &counts);
            }

            self.recordStreamNodesScannedBy(.stream_nodes_scanned_apply, stream.text_nodes.items.len);
            for (stream.text_nodes.items) |desc| {
                if (desc.elem_id < seen.len and seen[@intCast(desc.elem_id)] and self.applyRenderTextField(ctx, desc.elem_id, .text, desc.value)) {
                    counts.addTextField(.text);
                }
            }
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_apply, stream.signal_text_nodes.items.len);
            for (stream.signal_text_nodes.items) |*desc| {
                if (desc.elem_id < seen.len and seen[@intCast(desc.elem_id)] and self.evalSignalTextField(ctx, roc_host, desc.elem_id, .text, &desc.signal, desc.read, &desc.cached_value)) {
                    counts.addTextField(.text);
                }
            }
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_apply, stream.static_text_attrs.items.len);
            for (stream.static_text_attrs.items) |desc| {
                if (desc.elem_id < seen.len and seen[@intCast(desc.elem_id)] and self.applyRenderTextField(ctx, desc.elem_id, desc.field, desc.value)) {
                    counts.addTextField(desc.field);
                }
            }
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_apply, stream.signal_text_attrs.items.len);
            for (stream.signal_text_attrs.items) |*desc| {
                if (desc.elem_id < seen.len and seen[@intCast(desc.elem_id)] and self.evalSignalTextField(ctx, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value)) {
                    counts.addTextField(desc.field);
                }
            }
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_apply, stream.static_custom_text_attrs.items.len);
            for (stream.static_custom_text_attrs.items) |desc| {
                if (desc.elem_id < seen.len and seen[@intCast(desc.elem_id)] and self.applyRenderTextAttr(ctx, desc.elem_id, desc.name, desc.value)) {
                    counts.addTextAttr();
                }
            }
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_apply, stream.signal_custom_text_attrs.items.len);
            for (stream.signal_custom_text_attrs.items) |*desc| {
                if (desc.elem_id < seen.len and seen[@intCast(desc.elem_id)] and self.evalSignalTextAttr(ctx, roc_host, desc.elem_id, desc.name, &desc.signal, desc.read, &desc.cached_value)) {
                    counts.addTextAttr();
                }
            }
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_apply, stream.static_bool_attrs.items.len);
            for (stream.static_bool_attrs.items) |desc| {
                if (desc.elem_id < seen.len and seen[@intCast(desc.elem_id)] and self.applyRenderBoolField(ctx, desc.elem_id, desc.field, desc.value)) {
                    counts.addBoolField(desc.field);
                }
            }
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_apply, stream.signal_bool_attrs.items.len);
            for (stream.signal_bool_attrs.items) |*desc| {
                if (desc.elem_id < seen.len and seen[@intCast(desc.elem_id)] and self.evalSignalBoolField(ctx, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value)) {
                    counts.addBoolField(desc.field);
                }
            }
            self.recordStreamNodesScannedBy(.stream_nodes_scanned_apply, stream.on_changes.items.len);
            for (stream.on_changes.items) |*desc| {
                if (self.scopeIsInReplacementTarget(desc.scope_id, targets.replacement)) {
                    self.evalOnChangeInitial(ctx, roc_host, desc);
                }
            }

            self.applyStructuralEventBindingsForSeen(ctx, stream, seen, &counts);
            self.debugAssertRenderCacheMatchesStream(ctx, stream);
            self.debugAssertRenderCacheMatchesSink(ctx);

            self.rebuildActiveSignalGraphFromStream(ctx, stream);
            if (comptime enable_runtime_metrics) self.render_metrics.addCommandCounts(counts);
            return counts;
        }

        pub fn applyNodeDescriptorStream(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream) render.Counts {
            var counts: render.Counts = .{};
            counts.addHostReset();
            self.resetRenderTree(ctx);

            for (stream.render_nodes.items) |node| {
                switch (node.kind) {
                    .element => {
                        const desc = findElementDesc(stream, node.elem_id) orelse @panic("render node referenced missing element descriptor");
                        self.appendRenderNode(ctx, desc.elem_id, desc.parent_elem_id, desc.tag);
                        counts.addCreateElement();
                        counts.addAppendChild();
                    },
                    .text => {
                        const desc = findTextNodeDesc(stream, node.elem_id) orelse @panic("render node referenced missing text descriptor");
                        self.appendRenderNode(ctx, desc.elem_id, desc.parent_elem_id, "text");
                        counts.addCreateElement();
                        counts.addAppendChild();
                        if (self.applyRenderTextField(ctx, desc.elem_id, .text, desc.value)) {
                            counts.addTextField(.text);
                        }
                    },
                    .signal_text => {
                        const desc = findSignalTextNodeDescMutable(stream, node.elem_id) orelse @panic("render node referenced missing signal text descriptor");
                        self.appendRenderNode(ctx, desc.elem_id, desc.parent_elem_id, "text");
                        counts.addCreateElement();
                        counts.addAppendChild();
                        if (self.evalSignalTextField(ctx, roc_host, desc.elem_id, .text, &desc.signal, desc.read, &desc.cached_value)) {
                            counts.addTextField(.text);
                        }
                    },
                }
            }

            for (stream.static_text_attrs.items) |desc| {
                if (self.applyRenderTextField(ctx, desc.elem_id, desc.field, desc.value)) {
                    counts.addTextField(desc.field);
                }
            }
            for (stream.signal_text_attrs.items) |*desc| {
                if (self.evalSignalTextField(ctx, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value)) {
                    counts.addTextField(desc.field);
                }
            }
            for (stream.static_custom_text_attrs.items) |desc| {
                if (self.applyRenderTextAttr(ctx, desc.elem_id, desc.name, desc.value)) {
                    counts.addTextAttr();
                }
            }
            for (stream.signal_custom_text_attrs.items) |*desc| {
                if (self.evalSignalTextAttr(ctx, roc_host, desc.elem_id, desc.name, &desc.signal, desc.read, &desc.cached_value)) {
                    counts.addTextAttr();
                }
            }
            for (stream.static_bool_attrs.items) |desc| {
                if (self.applyRenderBoolField(ctx, desc.elem_id, desc.field, desc.value)) {
                    counts.addBoolField(desc.field);
                }
            }
            for (stream.signal_bool_attrs.items) |*desc| {
                if (self.evalSignalBoolField(ctx, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value)) {
                    counts.addBoolField(desc.field);
                }
            }
            for (stream.on_changes.items) |*desc| {
                self.evalOnChangeInitial(ctx, roc_host, desc);
            }
            for (stream.events.items, 0..) |desc, index| {
                self.applyRenderEventBinding(ctx, desc.elem_id, desc.kind, .{ .event_id = @intCast(index + 1), .payload_accessor = desc.payload_accessor }, &counts);
            }
            for (stream.named_events.items, 0..) |desc, index| {
                self.applyRenderNamedEventBinding(ctx, desc.elem_id, desc.name, .{
                    .event_id = @intCast(stream.events.items.len + index + 1),
                    .options = desc.options,
                    .payload_kind = desc.payload_kind,
                    .payload_accessor = desc.payload_accessor,
                }, &counts);
            }

            self.debugAssertRenderCacheMatchesStream(ctx, stream);
            self.debugAssertRenderCacheMatchesSink(ctx);
            self.rebuildActiveSignalGraphFromStream(ctx, stream);
            if (comptime enable_runtime_metrics) self.render_metrics.addCommandCounts(counts);
            return counts;
        }

        pub fn applySplicedStructuralNodeDescriptorTarget(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, splice: HostStructuralSplice, targets: HostStructuralPatchTargets, dirty_source_node_ids: []const u64, dirty_generation: u64) render.Counts {
            _ = targets;
            if (!self.hasRenderRoot()) @panic("structural DOM patch requested before initial DOM root creation");

            const allocator = Ctx.allocator(ctx);
            var max_elem_id: u64 = 0;
            for (splice.removed_elem_ids) |elem_id| {
                max_elem_id = @max(max_elem_id, elem_id);
            }
            for (splice.replacement_elem_ids) |elem_id| {
                max_elem_id = @max(max_elem_id, elem_id);
            }
            const required_child_table_len: usize = @intCast(max_elem_id + 1);
            const child_table_len = required_child_table_len;

            var seen = allocator.alloc(bool, child_table_len) catch @panic("out of memory");
            defer allocator.free(seen);
            @memset(seen, false);

            var touched_parents: std.ArrayListUnmanaged(u64) = .empty;
            defer touched_parents.deinit(allocator);
            for (splice.touched_parent_ids) |parent_id| {
                appendUniqueU64(allocator, &touched_parents, parent_id);
            }

            var counts: render.Counts = .{};

            self.recordStreamNodesScannedBy(.stream_nodes_scanned_splice, splice.replacement_elem_ids.len);
            for (splice.replacement_elem_ids) |elem_id| {
                if (elem_id >= child_table_len) @panic("render node exceeded structural DOM patch table");

                const parent_elem_id = streamElemParentElemId(&self.active_stream, elem_id);
                if (parent_elem_id >= child_table_len) @panic("render node referenced parent outside structural DOM patch table");

                self.ensureRenderNode(ctx, elem_id, streamElemTag(&self.active_stream, elem_id), &counts);
                seen[@intCast(elem_id)] = true;
                appendUniqueU64(allocator, &touched_parents, parent_elem_id);
            }

            for (splice.removed_elem_ids) |elem_id| {
                if (elem_id < seen.len and seen[@intCast(elem_id)]) continue;
                self.removeRenderNode(ctx, elem_id, &counts);
            }

            for (touched_parents.items) |parent_elem_id| {
                const children = streamDirectChildren(allocator, &self.active_stream, parent_elem_id);
                defer allocator.free(children);
                self.replaceRenderChildren(ctx, parent_elem_id, children, &counts);
            }

            for (splice.replacement_elem_ids) |elem_id| {
                self.applyActiveStreamFieldsForElem(ctx, roc_host, elem_id, &counts, dirty_source_node_ids, dirty_generation);
            }
            self.applyActiveStreamEventBindings(ctx, &counts);

            self.recordStreamNodesScannedBy(.stream_nodes_scanned_splice, splice.replacement_on_change_indices.len);
            for (splice.replacement_on_change_indices) |on_change_index| {
                if (on_change_index >= self.active_stream.on_changes.items.len) @panic("structural splice on_change index exceeded active descriptor stream");
                const desc = &self.active_stream.on_changes.items[on_change_index];
                self.evalOnChangeInitial(ctx, roc_host, desc);
            }

            self.debugAssertRenderCacheMatchesStream(ctx, &self.active_stream);
            self.debugAssertRenderCacheMatchesSink(ctx);
            counts.addAll(self.runActiveMountCommandIndices(ctx, roc_host, splice.replacement_mount_indices));
            if (comptime enable_runtime_metrics) self.render_metrics.addCommandCounts(counts);
            return counts;
        }

        pub fn applyStructuralNodeDescriptorStream(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream) render.Counts {
            if (!self.hasRenderRoot()) @panic("structural DOM patch requested before initial DOM root creation");

            const allocator = Ctx.allocator(ctx);
            const max_elem_id = @max(maxRenderElemId(&self.active_stream), maxRenderElemId(stream));
            const required_child_table_len: usize = @intCast(max_elem_id + 1);
            const child_table_len = required_child_table_len;

            var seen = allocator.alloc(bool, child_table_len) catch @panic("out of memory");
            defer allocator.free(seen);
            @memset(seen, false);

            var next_children = allocator.alloc(std.ArrayListUnmanaged(u64), child_table_len) catch @panic("out of memory");
            defer {
                for (next_children) |*children| {
                    children.deinit(allocator);
                }
                allocator.free(next_children);
            }
            for (next_children) |*children| {
                children.* = .empty;
            }

            var counts: render.Counts = .{};

            for (stream.render_nodes.items) |node| {
                if (node.elem_id >= child_table_len) @panic("render node exceeded structural DOM patch table");

                const parent_elem_id = renderNodeParentElemId(stream, node);
                if (parent_elem_id >= child_table_len) @panic("render node referenced parent outside structural DOM patch table");

                self.ensureRenderNode(ctx, node.elem_id, renderNodeTag(stream, node), &counts);
                seen[@intCast(node.elem_id)] = true;

                next_children[@intCast(parent_elem_id)].append(allocator, node.elem_id) catch @panic("out of memory");
            }

            for (self.active_stream.render_nodes.items) |node| {
                const still_rendered = node.elem_id < seen.len and seen[@intCast(node.elem_id)];
                if (!still_rendered) self.removeRenderNode(ctx, node.elem_id, &counts);
            }

            for (next_children, 0..) |*children, index| {
                const accepts_children = index == 0 or (index < seen.len and seen[index]);
                if (!accepts_children) continue;
                self.replaceRenderChildren(ctx, @intCast(index), children.items, &counts);
            }

            const text_fields = [_]RenderTextField{ .text, .role, .label, .test_id, .value, .class };
            const bool_fields = [_]RenderBoolField{ .checked, .disabled };
            for (seen, 0..) |is_seen, index| {
                if (index == 0 or !is_seen) continue;
                const elem_id: u64 = @intCast(index);

                for (text_fields) |field| {
                    if (!streamHasTextField(stream, elem_id, field) and self.clearRenderTextField(ctx, elem_id, field)) {
                        counts.addTextField(field);
                    }
                }
                for (bool_fields) |field| {
                    if (!streamHasBoolField(stream, elem_id, field) and self.clearRenderBoolField(ctx, elem_id, field)) {
                        counts.addBoolField(field);
                    }
                }
                self.clearRenderTextAttrsMissingFromStream(ctx, stream, elem_id, &counts);
            }

            for (stream.text_nodes.items) |desc| {
                if (self.applyRenderTextField(ctx, desc.elem_id, .text, desc.value)) {
                    counts.addTextField(.text);
                }
            }
            for (stream.signal_text_nodes.items) |*desc| {
                if (self.evalSignalTextField(ctx, roc_host, desc.elem_id, .text, &desc.signal, desc.read, &desc.cached_value)) {
                    counts.addTextField(.text);
                }
            }
            for (stream.static_text_attrs.items) |desc| {
                if (self.applyRenderTextField(ctx, desc.elem_id, desc.field, desc.value)) {
                    counts.addTextField(desc.field);
                }
            }
            for (stream.signal_text_attrs.items) |*desc| {
                if (self.evalSignalTextField(ctx, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value)) {
                    counts.addTextField(desc.field);
                }
            }
            for (stream.static_custom_text_attrs.items) |desc| {
                if (self.applyRenderTextAttr(ctx, desc.elem_id, desc.name, desc.value)) {
                    counts.addTextAttr();
                }
            }
            for (stream.signal_custom_text_attrs.items) |*desc| {
                if (self.evalSignalTextAttr(ctx, roc_host, desc.elem_id, desc.name, &desc.signal, desc.read, &desc.cached_value)) {
                    counts.addTextAttr();
                }
            }
            for (stream.static_bool_attrs.items) |desc| {
                if (self.applyRenderBoolField(ctx, desc.elem_id, desc.field, desc.value)) {
                    counts.addBoolField(desc.field);
                }
            }
            for (stream.signal_bool_attrs.items) |*desc| {
                if (self.evalSignalBoolField(ctx, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value)) {
                    counts.addBoolField(desc.field);
                }
            }
            for (stream.on_changes.items) |*desc| {
                self.evalOnChangeInitial(ctx, roc_host, desc);
            }

            self.applyStructuralEventBindings(ctx, stream, seen, &counts);
            self.debugAssertRenderCacheMatchesStream(ctx, stream);
            self.debugAssertRenderCacheMatchesSink(ctx);

            self.rebuildActiveSignalGraphFromStream(ctx, stream);
            if (comptime enable_runtime_metrics) self.render_metrics.addCommandCounts(counts);
            return counts;
        }

        pub fn applyDirtyStructuralSignalsLocally(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, dirty_source_node_ids: []const u64, dirty_generation: u64, changes: []const HostDirtyStructuralSignal) render.Counts {
            var total_counts: render.Counts = .{};
            var applied_any = false;

            for (changes) |change| {
                var replacement_stream: HostNodeDescriptorStream = .{};
                defer replacement_stream.deinit(Ctx.allocator(ctx), ctx, roc_host, &self.pending_roc_metrics);
                const splice_and_targets: HostStructuralSpliceAndTargets = switch (change.kind) {
                    .when => when_target: {
                        const site = self.activeScopeSiteByNodeId(change.node_id, .when) orelse {
                            if (applied_any) continue;
                            @panic("dirty when structural site is not active");
                        };
                        const when_index = self.activeWhenIndexByNodeId(change.node_id) orelse {
                            if (applied_any) continue;
                            @panic("dirty when descriptor is not active");
                        };
                        const active_branch = change.branch orelse @panic("dirty when structural signal did not record its active branch");
                        const replaced_scope_id = (self.activeWhenBranchScopeId(site.scope_id, site.ordinal, active_branch.opposite()) catch @panic("scope id has no host scope descriptor")) orelse @panic("dirty when structural update had no active opposite branch scope");
                        const when_desc = self.active_stream.whens.items[when_index];

                        const replacement_scope_id = self.collectActiveWhenBranchDescriptors(ctx, roc_host, &replacement_stream, site, when_desc, active_branch, dirty_source_node_ids);
                        const splice = self.spliceActiveStreamReplacingScope(ctx, roc_host, replaced_scope_id, site.render_insert_index, &replacement_stream);
                        break :when_target .{
                            .splice = splice,
                            .targets = HostStructuralPatchTargets{
                                .removed = .{ .scope = replaced_scope_id },
                                .replacement = .{ .scope = replacement_scope_id },
                            },
                        };
                    },
                    .each => each_target: {
                        const site = self.activeScopeSiteByNodeId(change.node_id, .each) orelse {
                            if (applied_any) continue;
                            @panic("dirty each structural site is not active");
                        };
                        const each_index = self.activeEachIndexByNodeId(change.node_id) orelse {
                            if (applied_any) continue;
                            @panic("dirty each descriptor is not active");
                        };
                        const each_desc = self.active_stream.eaches.items[each_index];
                        const each_site = HostEachSite{ .parent_scope_id = site.scope_id, .site_ordinal = site.ordinal };
                        const target = HostStructuralReplacementTarget{ .each_site = each_site };
                        const allocator = Ctx.allocator(ctx);
                        const old_active_rows = self.activeEachRowScopes(allocator, site.scope_id, site.ordinal) catch @panic("scope id has no host scope descriptor");
                        defer allocator.free(old_active_rows);
                        const old_render_segments = self.activeEachRowRenderSegmentsInRenderOrder(allocator, each_site);
                        defer allocator.free(old_render_segments);
                        const old_render_rows = eachRenderSegmentScopeIds(allocator, old_render_segments);
                        defer allocator.free(old_render_rows);
                        const diff = self.syncActiveEachRowScopes(ctx, roc_host, site, each_desc);
                        defer diff.deinit(allocator);

                        if (old_active_rows.len == old_render_rows.len and Self.eachDiffPreservesSurvivorRenderOrder(old_render_rows, diff.scope_ids)) {
                            const counts = self.applyDirtyEachRowScopeSplices(ctx, roc_host, site, each_desc, old_render_segments, diff, dirty_source_node_ids, dirty_generation);
                            total_counts.addAll(counts);
                            applied_any = true;
                            continue;
                        }

                        if (old_active_rows.len == old_render_rows.len and self.eachDiffIsPurePermutation(old_render_rows, diff, dirty_source_node_ids)) {
                            const counts = self.applyDirtyEachPermutationMoves(ctx, site, diff.scope_ids);
                            total_counts.addAll(counts);
                            applied_any = true;
                            continue;
                        }

                        if (old_active_rows.len == old_render_rows.len) {
                            const counts = self.applyDirtyEachMixedRowSplicesAndMoves(ctx, roc_host, site, each_desc, old_render_segments, diff, dirty_source_node_ids, dirty_generation);
                            total_counts.addAll(counts);
                            applied_any = true;
                            continue;
                        }

                        self.collectActiveEachRowDescriptorsFromDiff(ctx, roc_host, &replacement_stream, site, each_desc, diff, dirty_source_node_ids);
                        const splice = self.spliceActiveStreamReplacingTarget(
                            ctx,
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
                defer splice_and_targets.splice.deinit(Ctx.allocator(ctx));

                const counts = self.applySplicedStructuralNodeDescriptorTarget(ctx, roc_host, splice_and_targets.splice, splice_and_targets.targets, dirty_source_node_ids, dirty_generation);
                total_counts.addAll(counts);
                applied_any = true;
            }

            return total_counts;
        }

        pub fn applyDirtyWhenStructuralSignals(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, dirty_source_node_ids: []const u64, dirty_generation: u64, changes: []const HostDirtyStructuralSignal) render.Counts {
            for (changes) |change| {
                if (change.kind != .when) @panic("non-when structural change reached when-only test helper");
            }
            return self.applyDirtyStructuralSignalsLocally(ctx, roc_host, dirty_source_node_ids, dirty_generation, changes);
        }

        pub fn appendPendingTask(self: *Self, ctx: Ctx.Handle, owner_scope_id: u64, task_token: HostSignalToken, task_name: []const u8, request: []const u8) u64 {
            const allocator = Ctx.allocator(ctx);
            if (self.next_task_request_id == std.math.maxInt(u64)) @panic("host task request id overflowed");
            const request_id = self.next_task_request_id;
            self.next_task_request_id += 1;

            abi.increfBox(@ptrCast(task_token), 1);
            const task_name_copy = allocator.dupe(u8, task_name) catch @panic("out of memory");
            const request_copy = allocator.dupe(u8, request) catch {
                allocator.free(task_name_copy);
                @panic("out of memory");
            };
            self.pending_tasks.append(allocator, .{
                .request_id = request_id,
                .owner_scope_id = owner_scope_id,
                .task_token = task_token,
                .task_name = task_name_copy,
                .request = request_copy,
                .active = true,
            }) catch {
                releaseHostSignalToken(task_token, self.roc_host.?);
                allocator.free(task_name_copy);
                allocator.free(request_copy);
                @panic("out of memory");
            };
            return request_id;
        }

        pub fn pendingTaskIndexByName(self: *Self, name: []const u8) ?usize {
            var found: ?usize = null;
            for (self.pending_tasks.items, 0..) |task, index| {
                if (!task.active) continue;
                if (!std.mem.eql(u8, task.task_name, name)) continue;
                if (found != null) @panic("fake task result matched more than one pending request");
                found = index;
            }
            return found;
        }

        pub fn removePendingTaskAt(self: *Self, index: usize) HostPendingTask {
            if (index >= self.pending_tasks.items.len) @panic("pending task index is out of bounds");
            const task = self.pending_tasks.items[index];
            const last_index = self.pending_tasks.items.len - 1;
            if (index != last_index) {
                self.pending_tasks.items[index] = self.pending_tasks.items[last_index];
            }
            self.pending_tasks.items.len = last_index;
            return task;
        }

        pub fn activeTaskRecordByName(self: *Self, name: []const u8) ?*HostSignalRecord {
            var found: ?*HostSignalRecord = null;
            for (self.active_signal_graph.items) |node| {
                switch (node.record.payload) {
                    .task_source => |payload| {
                        if (!std.mem.eql(u8, payload.name, name)) continue;
                        if (found != null) @panic("fake task result matched more than one active task source");
                        found = node.record;
                    },
                    .ref, .const_value, .map, .map2, .combine, .interval_source => {},
                }
            }
            return found;
        }

        pub fn activeIntervalRecordByPeriod(self: *Self, period_ms: u64) ?*HostSignalRecord {
            var found: ?*HostSignalRecord = null;
            for (self.active_signal_graph.items) |node| {
                switch (node.record.payload) {
                    .interval_source => |payload| {
                        if (payload.period_ms != period_ms) continue;
                        if (found != null) @panic("tick_interval matched more than one active interval source");
                        found = node.record;
                    },
                    .ref, .const_value, .map, .map2, .combine, .task_source => {},
                }
            }
            return found;
        }

        pub fn rebuildActiveEventsFromStream(self: *Self, ctx: Ctx.Handle, stream: *HostNodeDescriptorStream) void {
            const allocator = Ctx.allocator(ctx);
            self.clearActiveEvents() catch @panic("active event table cannot release retained payloads without a Roc host");

            for (stream.events.items) |*desc| {
                if (!desc.owns_payload_reducer) @panic("event descriptor payload reducer ownership was already transferred");
                self.active_events.append(allocator, .{
                    .target_node_id = desc.target_node_id,
                    .payload_kind = desc.payload_kind,
                    .payload_accessor = desc.payload_accessor,
                    .payload_reducer = desc.payload_reducer,
                }) catch @panic("out of memory");
                desc.owns_payload_reducer = false;
            }
            for (stream.named_events.items) |*desc| {
                if (!desc.owns_payload_reducer) @panic("named event descriptor payload reducer ownership was already transferred");
                self.active_events.append(allocator, .{
                    .target_node_id = desc.target_node_id,
                    .payload_kind = desc.payload_kind,
                    .payload_accessor = desc.payload_accessor,
                    .payload_reducer = desc.payload_reducer,
                }) catch @panic("out of memory");
                desc.owns_payload_reducer = false;
            }
        }

        pub fn updateEffectSourceCacheSlot(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, cache_slot: *HostSignalCacheSlot, value: HostValue, cap: HostValueCapability) bool {
            switch (cache_slot.*) {
                .absent => {
                    debugPhase(ctx, 301);
                    cache_slot.replace(ctx, roc_host, &self.pending_roc_metrics, value, cap);
                    return true;
                },
                .present => |*cached| {
                    debugPhase(ctx, 310);
                    if (cached.valueEquals(ctx, roc_host, value)) {
                        debugPhase(ctx, 311);
                        cached.dropIncoming(ctx, roc_host, value);
                        self.recordSignalPrune();
                        return false;
                    }
                    debugPhase(ctx, 312);
                    cached.replaceValue(ctx, roc_host, value);
                    return true;
                },
            }
        }

        pub fn updateEffectSourceCache(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, record: *HostSignalRecord, value: HostValue) bool {
            return switch (record.payload) {
                .task_source => |*payload| self.updateEffectSourceCacheSlot(ctx, roc_host, &payload.cached_value, value, payload.cap),
                .interval_source => |*payload| self.updateEffectSourceCacheSlot(ctx, roc_host, &payload.cached_value, value, payload.cap),
                .ref, .const_value, .map, .map2, .combine => @panic("effect source update targeted a non-source signal record"),
            };
        }

        pub fn applyDirtySignalBatch(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, dirty_source_node_ids: []const u64, changed_record_ids: []const u64, dirty_generation: u64) render.Counts {
            debugPhase(ctx, 350);
            const dirty_structural_signals = self.collectDirtyStructuralSignals(ctx, roc_host, Ctx.allocator(ctx), dirty_source_node_ids, changed_record_ids, dirty_generation);
            defer Ctx.allocator(ctx).free(dirty_structural_signals);

            debugPhase(ctx, 360);
            var counts = self.applyDirtyRenderSinks(ctx, roc_host, dirty_source_node_ids, changed_record_ids, dirty_generation);
            if (dirty_structural_signals.len != 0) {
                debugPhase(ctx, 370);
                counts.addAll(self.applyDirtyStructuralSignalsLocally(ctx, roc_host, dirty_source_node_ids, dirty_generation, dirty_structural_signals));
            }
            return counts;
        }

        pub fn dispatchEffectSourceValue(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, record: *HostSignalRecord, value: HostValue) render.Counts {
            debugPhase(ctx, 300);
            if (!self.updateEffectSourceCache(ctx, roc_host, record, value)) return .{};

            self.recordDispatch();
            var metrics = self.pending_roc_metrics;
            metrics.bump(.nodes_recomputed, 1);
            self.pending_roc_metrics = metrics;
            const dirty_generation = self.nextDirtySignalGeneration();
            record.last_dirty_generation = dirty_generation;
            record.last_dirty_changed = true;

            const record_id = self.requireActiveSignalRecordId(record);
            const roots = [_]u64{record_id};
            const dirty_record_ids = self.dirtyActiveSignalRecordIdsForRoots(Ctx.allocator(ctx), &roots);
            defer Ctx.allocator(ctx).free(dirty_record_ids);

            debugPhase(ctx, 330);
            const changed_record_ids = self.propagateDirtyActiveSignalRecordIds(ctx, roc_host, Ctx.allocator(ctx), dirty_record_ids, &.{}, dirty_generation);
            defer Ctx.allocator(ctx).free(changed_record_ids);
            debugPhase(ctx, 340);
            return self.applyDirtySignalBatch(ctx, roc_host, &.{}, changed_record_ids, dirty_generation);
        }

        pub fn startTaskCommand(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, owner_scope_id: u64, cmd: erased_calls.StartTaskCmd) render.Counts {
            const record = self.activeTaskRecordByToken(cmd.task_token) orelse @panic("StartTask referenced a task source that is not active");
            const task_payload = switch (record.payload) {
                .task_source => |payload| payload,
                .ref, .const_value, .map, .map2, .combine, .interval_source => unreachable,
            };
            if (!std.mem.eql(u8, task_payload.name, cmd.task_name.asSlice())) {
                @panic("StartTask task name does not match the referenced task source");
            }

            const request_value = erased_calls.callValueInitThunk(roc_host, cmd.request_init);
            defer callHostValueToUnitWithCapability(ctx, roc_host, cmd.request_read.capability, hv.hostValueCapabilityDrop(cmd.request_read.capability), request_value);
            const request = callHostValueToStrWithCapability(ctx, roc_host, cmd.request_read.capability, cmd.request_read.read, request_value);
            defer request.decref(roc_host);

            self.cancelPendingTasksByTaskToken(ctx, cmd.task_token);
            const request_id = self.appendPendingTask(ctx, owner_scope_id, cmd.task_token, cmd.task_name.asSlice(), request.asSlice());
            Ctx.sink(ctx).startTask(request_id, cmd.task_name.asSlice(), request.asSlice());

            if (task_payload.reset_on_start) {
                const loading = erased_calls.callValueInitThunk(roc_host, task_payload.initial);
                return self.dispatchEffectSourceValue(ctx, roc_host, record, loading);
            }

            return .{};
        }

        pub fn tickIntervalSource(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, period_ms: u64) render.Counts {
            const record = self.activeIntervalRecordByPeriod(period_ms) orelse @panic("tick_interval matched no active interval source");
            const interval_payload = switch (record.payload) {
                .interval_source => |payload| payload,
                .ref, .const_value, .map, .map2, .combine, .task_source => unreachable,
            };

            const current = self.evalHostSignalRecord(ctx, roc_host, record);
            defer self.dropHostSignalRecordValue(ctx, roc_host, record, current);
            const next = callHostValueToHostValueWithCapability(ctx, roc_host, interval_payload.cap, interval_payload.tick, current);
            return self.dispatchEffectSourceValue(ctx, roc_host, record, next);
        }

        pub fn tickIntervalSourceByRuntimeToken(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, token: u64) render.Counts {
            const source_token = self.activeIntervalSourceTokenByRuntimeToken(token) orelse @panic("timer tick referenced an inactive interval token");
            const record = self.activeIntervalRecordByToken(source_token) orelse @panic("timer tick matched no active interval source");
            const interval_payload = switch (record.payload) {
                .interval_source => |payload| payload,
                .ref, .const_value, .map, .map2, .combine, .task_source => unreachable,
            };

            const current = self.evalHostSignalRecord(ctx, roc_host, record);
            defer self.dropHostSignalRecordValue(ctx, roc_host, record, current);
            const next = callHostValueToHostValueWithCapability(ctx, roc_host, interval_payload.cap, interval_payload.tick, current);
            return self.dispatchEffectSourceValue(ctx, roc_host, record, next);
        }

        pub fn evalDirtyOnChange(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, desc: *HostNodeOnChangeDesc, dirty_source_node_ids: []const u64, dirty_generation: u64) render.Counts {
            const result = self.evalDirtyHostSignalBinding(ctx, roc_host, &desc.signal, dirty_source_node_ids, dirty_generation);
            const cap = self.hostSignalBindingCapability(ctx, &desc.signal);
            if (!result.changed) {
                callHostValueToUnitWithCapability(ctx, roc_host, cap, hv.hostValueCapabilityDrop(cap), result.value);
                return .{};
            }
            if (!self.updateDirtySignalCache(ctx, roc_host, &desc.cached_value, result.value, cap)) return .{};

            const cmd = callHostValueToStartTaskCmdWithCapability(ctx, roc_host, cap, desc.to_cmd, result.value);
            defer abi.decref__AnonStruct85(cmd, roc_host);
            return self.startTaskCommand(ctx, roc_host, desc.scope_id, cmd);
        }

        pub fn applyDirtyRenderSinks(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, dirty_source_node_ids: []const u64, changed_record_ids: []const u64, dirty_generation: u64) render.Counts {
            var counts: render.Counts = .{};

            for (changed_record_ids) |record_id| {
                const route_index: usize = @intCast(record_id);
                if (route_index < self.active_text_signal_routes.items.len) {
                    for (self.active_text_signal_routes.items[route_index].items) |route| {
                        switch (route.kind) {
                            .text_node => {
                                const desc = &self.active_stream.signal_text_nodes.items[route.index];
                                if (self.evalDirtySignalTextField(ctx, roc_host, desc.elem_id, .text, &desc.signal, desc.read, &desc.cached_value, dirty_source_node_ids, dirty_generation)) {
                                    counts.addTextField(.text);
                                }
                            },
                            .text_attr => {
                                const desc = &self.active_stream.signal_text_attrs.items[route.index];
                                if (self.evalDirtySignalTextField(ctx, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value, dirty_source_node_ids, dirty_generation)) {
                                    counts.addTextField(desc.field);
                                }
                            },
                            .custom_text_attr => {
                                const desc = &self.active_stream.signal_custom_text_attrs.items[route.index];
                                if (self.evalDirtySignalTextAttr(ctx, roc_host, desc.elem_id, desc.name, &desc.signal, desc.read, &desc.cached_value, dirty_source_node_ids, dirty_generation)) {
                                    counts.addTextAttr();
                                }
                            },
                        }
                    }
                }

                if (route_index < self.active_bool_signal_routes.items.len) {
                    for (self.active_bool_signal_routes.items[route_index].items) |route| {
                        const desc = &self.active_stream.signal_bool_attrs.items[route.index];
                        if (self.evalDirtySignalBoolField(ctx, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value, dirty_source_node_ids, dirty_generation)) {
                            counts.addBoolField(desc.field);
                        }
                    }
                }

                if (route_index < self.active_change_signal_routes.items.len) {
                    for (self.active_change_signal_routes.items[route_index].items) |route| {
                        const desc = &self.active_stream.on_changes.items[route.index];
                        counts.addAll(self.evalDirtyOnChange(ctx, roc_host, desc, dirty_source_node_ids, dirty_generation));
                    }
                }
            }

            if (comptime enable_runtime_metrics) self.render_metrics.addCommandCounts(counts);
            return counts;
        }
    };
}

const VerifyCtxHost = struct {};

const VerifySink = struct {
    pub fn reset(_: VerifySink) void {}
    pub fn appendNode(_: VerifySink, _: u64, _: u64, _: []const u8) void {}
    pub fn ensureNode(_: VerifySink, _: u64, _: []const u8) void {}
    pub fn removeNode(_: VerifySink, _: u64) void {}
    pub fn replaceChildren(_: VerifySink, _: u64, _: []const u64) void {}
    pub fn replaceChildrenForMoves(_: VerifySink, _: u64, _: []const u64) void {}
    pub fn applyTextField(_: VerifySink, _: u64, _: RenderTextField, _: []const u8) void {}
    pub fn applyTextAttr(_: VerifySink, _: u64, _: []const u8, _: []const u8) void {}
    pub fn applyBoolField(_: VerifySink, _: u64, _: RenderBoolField, _: bool) void {}
    pub fn clearTextField(_: VerifySink, _: u64, _: RenderTextField) void {}
    pub fn clearTextAttr(_: VerifySink, _: u64, _: []const u8) void {}
    pub fn clearBoolField(_: VerifySink, _: u64, _: RenderBoolField) void {}
    pub fn bindEventKind(_: VerifySink, _: u64, _: RenderEventKind, _: u64, _: EventPayloadAccessor) void {}
    pub fn clearEvent(_: VerifySink, _: u64, _: RenderEventKind) void {}
    pub fn bindEventName(_: VerifySink, _: u64, _: []const u8, _: u64, _: u32, _: EventPayloadKind, _: EventPayloadAccessor) void {}
    pub fn clearEventName(_: VerifySink, _: u64, _: []const u8) void {}
    pub fn startInterval(_: VerifySink, _: u64, _: u64) void {}
    pub fn cancelInterval(_: VerifySink, _: u64) void {}
    pub fn startTask(_: VerifySink, _: u64, _: []const u8, _: []const u8) void {}
    pub fn cancelTask(_: VerifySink, _: u64) void {}
    pub fn debugAssertNode(_: VerifySink, _: u64, _: bool, _: ?[]const u8, _: ?u64, _: []const u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64) void {}
};

const VerifyCtx = struct {
    pub const Handle = *VerifyCtxHost;
    pub const RegistryOps = hv.RegistryOps();
    pub const Metrics = RuntimeMetrics;
    pub const Sink = VerifySink;

    pub fn zeroMetrics() Metrics {
        return zeroRuntimeMetrics();
    }

    pub fn allocator(_: Handle) std.mem.Allocator {
        return std.heap.page_allocator;
    }

    pub fn cloneHostValue(_: Handle, value: HostValue) HostValue {
        return value;
    }

    pub fn pushHostValueCapabilities(_: Handle, _: []const HostValueCapability) void {}

    pub fn popHostValueCapabilities(_: Handle) void {}

    pub fn stateValueByNodeId(_: Handle, _: u64) HostValue {
        return 0;
    }

    pub fn stateCapability(_: Handle, _: u64) HostValueCapability {
        return undefined;
    }

    pub fn sink(_: Handle) Sink {
        return .{};
    }
};

comptime {
    verifyCtx(VerifyCtx);
    std.debug.assert(@sizeOf(NoMetrics) == 0);
    _ = Engine(VerifyCtx);
}
