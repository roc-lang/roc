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

const RuntimeMetrics = struct {
    closure_releases: u64,
    closure_retains: u64,
    derived_calls_into_roc: u64,
    events_processed: u64,
    nodes_recomputed: u64,
    patches_emitted: u64,
    propagation_prunes: u64,
    recompute_batches: u64,
    retained_alloc_delta: i64,
    rows_created: u64,
    rows_removed: u64,
    rows_reused: u64,
    scopes_created: u64,
    scopes_disposed: u64,
};

const SignalValueDesc = struct {
    signal_id: u64,
    value: abi.NodeValue,
};

const StateValueDesc = struct {
    state_id: u64,
    value: abi.NodeValue,
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

const HostSignalEventRoute = struct {
    event_id: u64,
    signal_ids: []u64,
};

const HostValueCell = struct {
    value: abi.NodeValue,
    eq: abi.RocErasedCallable,

    fn initRetained(value: abi.NodeValue, eq: abi.RocErasedCallable, metrics: *RuntimeMetrics) HostValueCell {
        abi.increfErasedCallable(eq, 1);
        metrics.closure_retains += 1;
        return .{ .value = value, .eq = eq };
    }

    fn deinit(self: *HostValueCell, roc_host: *abi.RocHost, metrics: *RuntimeMetrics) void {
        abi.decrefNodeValue(self.value, roc_host);
        abi.decrefErasedCallable(self.eq, roc_host);
        metrics.closure_releases += 1;
        self.* = undefined;
    }

    fn valueEquals(self: *const HostValueCell, roc_host: *abi.RocHost, value: abi.NodeValue) bool {
        return callErasedNodeValueNodeValueToBool(roc_host, self.eq, self.value, value);
    }

    fn replaceValue(self: *HostValueCell, roc_host: *abi.RocHost, value: abi.NodeValue) void {
        abi.decrefNodeValue(self.value, roc_host);
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

const HostSignalCachedValue = struct {
    value: abi.NodeValue,
    eq: abi.RocErasedCallable,
};

const HostSignalCacheSlot = union(enum) {
    absent,
    present: HostSignalCachedValue,

    fn deinit(self: *HostSignalCacheSlot, roc_host: *abi.RocHost, metrics: *RuntimeMetrics) void {
        switch (self.*) {
            .absent => {},
            .present => |cached| {
                abi.decrefNodeValue(cached.value, roc_host);
                abi.decrefErasedCallable(cached.eq, roc_host);
                metrics.closure_releases += 1;
            },
        }
        self.* = .absent;
    }

    fn replace(self: *HostSignalCacheSlot, roc_host: *abi.RocHost, metrics: *RuntimeMetrics, value: abi.NodeValue, eq: abi.RocErasedCallable) void {
        self.deinit(roc_host, metrics);
        abi.increfErasedCallable(eq, 1);
        metrics.closure_retains += 1;
        self.* = .{ .present = .{ .value = value, .eq = eq } };
    }

    fn replaceValue(self: *HostSignalCacheSlot, roc_host: *abi.RocHost, value: abi.NodeValue) void {
        switch (self.*) {
            .absent => failHost("dirty signal expression was evaluated before its initial value was cached"),
            .present => |*cached| {
                abi.decrefNodeValue(cached.value, roc_host);
                cached.value = value;
            },
        }
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

const HostEachRowScopeStep = struct {
    site_ordinal: u64,
    key: abi.NodeValue,
    item: abi.NodeValue,
};

const HostScopeStep = union(enum) {
    root,
    when_branch: HostWhenBranchScopeStep,
    each_row: HostEachRowScopeStep,

    fn deinit(self: *HostScopeStep, roc_host: *abi.RocHost) void {
        switch (self.*) {
            .each_row => |row| {
                abi.decrefNodeValue(row.key, roc_host);
                abi.decrefNodeValue(row.item, roc_host);
            },
            .root, .when_branch => {},
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

const HostBinderBinding = struct {
    token: HostBinderToken,
    node_id: u64,
};

const HostNodeScopeSiteDesc = struct {
    node_id: u64,
    scope_id: u64,
    ordinal: u64,
    parent_elem_id: u64,
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
    signal: abi.NodeSignalExpr,
    source_node_ids: []u64,
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
    signal: abi.NodeSignalExpr,
    source_node_ids: []u64,
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
    signal: abi.NodeSignalExpr,
    source_node_ids: []u64,
    cached_value: HostSignalCacheSlot = .absent,
};

const HostNodeEventDesc = struct {
    elem_id: u64,
    kind: RenderEventKind,
    binder_token: HostBinderToken,
    target_node_id: u64,
    payload_kind: EventPayloadKind,
    transform: abi.RocErasedCallable,
};

const HostNodeStateDesc = struct {
    node_id: u64,
    initial: abi.RocErasedCallable,
    eq: abi.RocErasedCallable,
};

const HostNodeWhenDesc = struct {
    node_id: u64,
    condition: abi.NodeSignalExpr,
    source_node_ids: []u64,
    cached_value: HostSignalCacheSlot = .absent,
};

const HostNodeEachDesc = struct {
    node_id: u64,
    items: abi.NodeSignalExpr,
    source_node_ids: []u64,
    key_of: abi.RocErasedCallable,
    key_eq: abi.RocErasedCallable,
    item_eq: abi.RocErasedCallable,
    row: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
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
    events: std.ArrayListUnmanaged(HostNodeEventDesc) = .empty,
    scope_sites: std.ArrayListUnmanaged(HostNodeScopeSiteDesc) = .empty,
    states: std.ArrayListUnmanaged(HostNodeStateDesc) = .empty,
    whens: std.ArrayListUnmanaged(HostNodeWhenDesc) = .empty,
    eaches: std.ArrayListUnmanaged(HostNodeEachDesc) = .empty,
    next_elem_id: u64 = 1,

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
            metrics.closure_releases += nodeSignalExprCallableCount(desc.signal);
            abi.decrefNodeSignalExpr(desc.signal, roc_host);
            allocator.free(desc.source_node_ids);
        }
        self.signal_text_nodes.deinit(allocator);

        for (self.static_text_attrs.items) |desc| {
            allocator.free(desc.value);
        }
        self.static_text_attrs.deinit(allocator);

        for (self.signal_text_attrs.items) |*desc| {
            desc.cached_value.deinit(roc_host, metrics);
            metrics.closure_releases += nodeSignalExprCallableCount(desc.signal);
            abi.decrefNodeSignalExpr(desc.signal, roc_host);
            allocator.free(desc.source_node_ids);
        }
        self.signal_text_attrs.deinit(allocator);

        self.static_bool_attrs.deinit(allocator);

        for (self.signal_bool_attrs.items) |*desc| {
            desc.cached_value.deinit(roc_host, metrics);
            metrics.closure_releases += nodeSignalExprCallableCount(desc.signal);
            abi.decrefNodeSignalExpr(desc.signal, roc_host);
            allocator.free(desc.source_node_ids);
        }
        self.signal_bool_attrs.deinit(allocator);

        for (self.events.items) |desc| {
            metrics.closure_releases += 1;
            abi.decrefErasedCallable(desc.transform, roc_host);
        }
        self.events.deinit(allocator);

        for (self.scope_sites.items) |desc| {
            allocator.free(desc.binder_bindings);
        }
        self.scope_sites.deinit(allocator);

        for (self.states.items) |desc| {
            metrics.closure_releases += 2;
            abi.decrefErasedCallable(desc.initial, roc_host);
            abi.decrefErasedCallable(desc.eq, roc_host);
        }
        self.states.deinit(allocator);

        for (self.whens.items) |*desc| {
            desc.cached_value.deinit(roc_host, metrics);
            metrics.closure_releases += nodeSignalExprCallableCount(desc.condition);
            abi.decrefNodeSignalExpr(desc.condition, roc_host);
            allocator.free(desc.source_node_ids);
        }
        self.whens.deinit(allocator);

        for (self.eaches.items) |*desc| {
            desc.cached_value.deinit(roc_host, metrics);
            metrics.closure_releases += nodeSignalExprCallableCount(desc.items);
            abi.decrefNodeSignalExpr(desc.items, roc_host);
            allocator.free(desc.source_node_ids);
            metrics.closure_releases += 4;
            abi.decrefErasedCallable(desc.key_of, roc_host);
            abi.decrefErasedCallable(desc.key_eq, roc_host);
            abi.decrefErasedCallable(desc.item_eq, roc_host);
            abi.decrefErasedCallable(desc.row, roc_host);
        }
        self.eaches.deinit(allocator);

        self.* = .{};
    }

    fn appendElement(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, parent_elem_id: u64, scope_id: u64, tag: []const u8) u64 {
        self.next_elem_id += 1;

        const tag_copy = allocator.dupe(u8, tag) catch std.process.exit(1);
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
        return elem_id;
    }

    fn appendTextNode(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, parent_elem_id: u64, scope_id: u64, value: []const u8) void {
        self.next_elem_id += 1;

        const value_copy = allocator.dupe(u8, value) catch std.process.exit(1);
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
    }

    fn appendSignalTextNode(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: *RuntimeMetrics, elem_id: u64, parent_elem_id: u64, scope_id: u64, signal: abi.NodeSignalExpr, source_node_ids: []u64) void {
        self.next_elem_id += 1;

        abi.increfNodeSignalExpr(signal, 1);
        metrics.closure_retains += nodeSignalExprCallableCount(signal);
        self.render_nodes.append(allocator, .{ .elem_id = elem_id, .kind = .signal_text }) catch {
            metrics.closure_releases += nodeSignalExprCallableCount(signal);
            abi.decrefNodeSignalExpr(signal, roc_host);
            allocator.free(source_node_ids);
            std.process.exit(1);
        };
        self.signal_text_nodes.append(allocator, .{
            .elem_id = elem_id,
            .parent_elem_id = parent_elem_id,
            .scope_id = scope_id,
            .signal = signal,
            .source_node_ids = source_node_ids,
        }) catch {
            metrics.closure_releases += nodeSignalExprCallableCount(signal);
            abi.decrefNodeSignalExpr(signal, roc_host);
            allocator.free(source_node_ids);
            std.process.exit(1);
        };
    }

    fn appendStaticTextAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderTextField, value: []const u8) void {
        const value_copy = allocator.dupe(u8, value) catch std.process.exit(1);
        self.static_text_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .value = value_copy,
        }) catch {
            allocator.free(value_copy);
            std.process.exit(1);
        };
    }

    fn appendSignalTextAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: *RuntimeMetrics, elem_id: u64, field: RenderTextField, signal: abi.NodeSignalExpr, source_node_ids: []u64) void {
        abi.increfNodeSignalExpr(signal, 1);
        metrics.closure_retains += nodeSignalExprCallableCount(signal);
        self.signal_text_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .signal = signal,
            .source_node_ids = source_node_ids,
        }) catch {
            metrics.closure_releases += nodeSignalExprCallableCount(signal);
            abi.decrefNodeSignalExpr(signal, roc_host);
            allocator.free(source_node_ids);
            std.process.exit(1);
        };
    }

    fn appendStaticBoolAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderBoolField, value: bool) void {
        self.static_bool_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .value = value,
        }) catch std.process.exit(1);
    }

    fn appendSignalBoolAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: *RuntimeMetrics, elem_id: u64, field: RenderBoolField, signal: abi.NodeSignalExpr, source_node_ids: []u64) void {
        abi.increfNodeSignalExpr(signal, 1);
        metrics.closure_retains += nodeSignalExprCallableCount(signal);
        self.signal_bool_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .signal = signal,
            .source_node_ids = source_node_ids,
        }) catch {
            metrics.closure_releases += nodeSignalExprCallableCount(signal);
            abi.decrefNodeSignalExpr(signal, roc_host);
            allocator.free(source_node_ids);
            std.process.exit(1);
        };
    }

    fn appendEvent(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: *RuntimeMetrics, elem_id: u64, kind: RenderEventKind, binder_token: HostBinderToken, target_node_id: u64, payload_kind: EventPayloadKind, transform: abi.RocErasedCallable) void {
        abi.increfErasedCallable(transform, 1);
        metrics.closure_retains += 1;
        self.events.append(allocator, .{
            .elem_id = elem_id,
            .kind = kind,
            .binder_token = binder_token,
            .target_node_id = target_node_id,
            .payload_kind = payload_kind,
            .transform = transform,
        }) catch {
            metrics.closure_releases += 1;
            abi.decrefErasedCallable(transform, roc_host);
            std.process.exit(1);
        };
    }

    fn appendScopeSite(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, node_id: u64, scope_id: u64, ordinal: u64, parent_elem_id: u64, kind: HostNodeScopeSiteKind, binder_bindings: []const HostBinderBinding) void {
        const binder_copy = allocator.dupe(HostBinderBinding, binder_bindings) catch std.process.exit(1);
        self.scope_sites.append(allocator, .{
            .node_id = node_id,
            .scope_id = scope_id,
            .ordinal = ordinal,
            .parent_elem_id = parent_elem_id,
            .kind = kind,
            .binder_bindings = binder_copy,
        }) catch {
            allocator.free(binder_copy);
            std.process.exit(1);
        };
    }

    fn appendState(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: *RuntimeMetrics, node_id: u64, initial: abi.RocErasedCallable, eq: abi.RocErasedCallable) void {
        abi.increfErasedCallable(initial, 1);
        abi.increfErasedCallable(eq, 1);
        metrics.closure_retains += 2;
        self.states.append(allocator, .{
            .node_id = node_id,
            .initial = initial,
            .eq = eq,
        }) catch {
            metrics.closure_releases += 2;
            abi.decrefErasedCallable(initial, roc_host);
            abi.decrefErasedCallable(eq, roc_host);
            std.process.exit(1);
        };
    }

    fn appendWhen(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: *RuntimeMetrics, node_id: u64, condition: abi.NodeSignalExpr, source_node_ids: []u64) void {
        abi.increfNodeSignalExpr(condition, 1);
        metrics.closure_retains += nodeSignalExprCallableCount(condition);
        self.whens.append(allocator, .{
            .node_id = node_id,
            .condition = condition,
            .source_node_ids = source_node_ids,
        }) catch {
            metrics.closure_releases += nodeSignalExprCallableCount(condition);
            abi.decrefNodeSignalExpr(condition, roc_host);
            allocator.free(source_node_ids);
            std.process.exit(1);
        };
    }

    fn appendEach(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: *RuntimeMetrics, node_id: u64, items: abi.NodeSignalExpr, source_node_ids: []u64, key_of: abi.RocErasedCallable, key_eq: abi.RocErasedCallable, item_eq: abi.RocErasedCallable, row: abi.RocErasedCallable) void {
        abi.increfNodeSignalExpr(items, 1);
        abi.increfErasedCallable(key_of, 1);
        abi.increfErasedCallable(key_eq, 1);
        abi.increfErasedCallable(item_eq, 1);
        abi.increfErasedCallable(row, 1);
        metrics.closure_retains += nodeSignalExprCallableCount(items) + 4;
        self.eaches.append(allocator, .{
            .node_id = node_id,
            .items = items,
            .source_node_ids = source_node_ids,
            .key_of = key_of,
            .key_eq = key_eq,
            .item_eq = item_eq,
            .row = row,
        }) catch {
            metrics.closure_releases += nodeSignalExprCallableCount(items);
            abi.decrefNodeSignalExpr(items, roc_host);
            allocator.free(source_node_ids);
            metrics.closure_releases += 4;
            abi.decrefErasedCallable(key_of, roc_host);
            abi.decrefErasedCallable(key_eq, roc_host);
            abi.decrefErasedCallable(item_eq, roc_host);
            abi.decrefErasedCallable(row, roc_host);
            std.process.exit(1);
        };
    }
};

fn nodeSignalExprCallableCount(expr: abi.NodeSignalExpr) u64 {
    return switch (expr.tag) {
        .Ref => 0,
        .ConstValue => 2,
        .Map => 2 + nodeSignalExprCallableCount(expr.payload.map._0.*),
        .Map2 => 2 + nodeSignalExprCallableCount(expr.payload.map2._0.*) + nodeSignalExprCallableCount(expr.payload.map2._1.*),
        .Combine => blk: {
            var count: u64 = 2;
            for (expr.payload.combine._0.items()) |child| {
                count += nodeSignalExprCallableCount(child);
            }
            break :blk count;
        },
    };
}

const HostRenderTextSink = struct {
    elem_id: u64,
    field: RenderTextField,
};

const HostRenderBoolSink = struct {
    elem_id: u64,
    field: RenderBoolField,
};

const RecomputeApplyOutcome = struct {
    structural_render_required: bool,
};

const HostKeyedRowDiffResult = struct {
    scope_ids: []u64,
    row_items_changed: []bool,
    rows_reused: u64,
    rows_created: u64,
    rows_removed: u64,
    row_items_unchanged: u64,
    row_items_updated: u64,
};

const HostRenderMetrics = struct {
    patches_emitted: u64 = 0,
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

fn traceEnabled() bool {
    if (current_host) |host| {
        return host.test_state.verbose;
    }
    return false;
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
    expected_text: ?[]const u8,
    expected_count: ?u64,
    expected_bool: ?bool,
    line_num: usize,
};

fn freeSpecCommands(allocator: std.mem.Allocator, commands: []SpecCommand) void {
    for (commands) |cmd| {
        cmd.locator.deinit(allocator);
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
        } else if (std.mem.startsWith(u8, trimmed, "expect_metric_delta ")) {
            const split = try splitTrailingToken(trimmed[20..]);
            const metric_name = allocator.dupe(u8, split.head) catch return ParseError.OutOfMemory;
            const expected_count = std.fmt.parseInt(u64, split.token, 10) catch return ParseError.InvalidFormat;
            try appendSpecCommand(&commands, allocator, .expect_metric_delta, emptyLocator(), metric_name, expected_count, null, line_num);
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

const HostEnv = struct {
    gpa: std.heap.DebugAllocator(.{ .safety = true }),
    test_state: TestState,
    roc_allocations: std.ArrayListUnmanaged(RocAllocation) = .empty,
    alloc_count: usize = 0,
    dealloc_count: usize = 0,
    dom_elements: std.ArrayListUnmanaged(DomElement) = .empty,
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
    render_text_sink_routes: std.ArrayListUnmanaged(std.ArrayListUnmanaged(HostRenderTextSink)) = .empty,
    render_bool_sink_routes: std.ArrayListUnmanaged(std.ArrayListUnmanaged(HostRenderBoolSink)) = .empty,
    render_structural_signals: std.ArrayListUnmanaged(bool) = .empty,
    render_metrics: HostRenderMetrics = .{},
    dispatch_metrics: HostDispatchMetrics = .{},
    next_elem_id: u64 = 0,
    roc_host: ?*abi.RocHost = null,
    root_elem: ?abi.Elem = null,
    last_runtime_metrics: RuntimeMetrics = zeroRuntimeMetrics(),
    pending_roc_metrics: RuntimeMetrics = zeroRuntimeMetrics(),

    fn init() HostEnv {
        return .{
            .gpa = std.heap.DebugAllocator(.{ .safety = true }){},
            .test_state = TestState.init(),
        };
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

    fn clearRenderSinkRoutes(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
        for (self.render_text_sink_routes.items) |*route| {
            route.deinit(allocator);
        }
        self.render_text_sink_routes.items.len = 0;

        for (self.render_bool_sink_routes.items) |*route| {
            route.deinit(allocator);
        }
        self.render_bool_sink_routes.items.len = 0;

        self.render_structural_signals.items.len = 0;
    }

    fn resetRenderSinkRoutes(self: *HostEnv) void {
        self.clearRenderSinkRoutes();

        const allocator = self.gpa.allocator();
        const signal_count = self.signal_descriptors.items.len;
        self.render_text_sink_routes.ensureTotalCapacity(allocator, signal_count) catch std.process.exit(1);
        self.render_bool_sink_routes.ensureTotalCapacity(allocator, signal_count) catch std.process.exit(1);
        self.render_structural_signals.ensureTotalCapacity(allocator, signal_count) catch std.process.exit(1);

        var index: usize = 0;
        while (index < signal_count) : (index += 1) {
            self.render_text_sink_routes.appendAssumeCapacity(.empty);
            self.render_bool_sink_routes.appendAssumeCapacity(.empty);
            self.render_structural_signals.appendAssumeCapacity(false);
        }
    }

    fn validateRenderSinkSignalId(self: *HostEnv, signal_id: u64) usize {
        if (signal_id >= self.signal_descriptors.items.len) {
            failHost("Roc render sink descriptor referenced an unknown signal id");
        }
        const signal_index: usize = @intCast(signal_id);
        const signal = self.signal_descriptors.items[signal_index];
        if (signal.signal_id != signal_id) {
            failHost("host signal registry is not indexed by signal id");
        }
        return signal_index;
    }

    fn appendRenderTextSink(self: *HostEnv, signal_id: u64, elem_id: u64, field: RenderTextField) void {
        const signal_index = self.validateRenderSinkSignalId(signal_id);
        if (signal_index >= self.render_text_sink_routes.items.len) {
            failHost("host render text sink routes are not indexed by signal id");
        }
        self.render_text_sink_routes.items[signal_index].append(self.gpa.allocator(), .{
            .elem_id = elem_id,
            .field = field,
        }) catch std.process.exit(1);
    }

    fn appendRenderBoolSink(self: *HostEnv, signal_id: u64, elem_id: u64, field: RenderBoolField) void {
        const signal_index = self.validateRenderSinkSignalId(signal_id);
        if (signal_index >= self.render_bool_sink_routes.items.len) {
            failHost("host render bool sink routes are not indexed by signal id");
        }
        self.render_bool_sink_routes.items[signal_index].append(self.gpa.allocator(), .{
            .elem_id = elem_id,
            .field = field,
        }) catch std.process.exit(1);
    }

    fn markRenderStructuralSignal(self: *HostEnv, signal_id: u64) void {
        const signal_index = self.validateRenderSinkSignalId(signal_id);
        if (signal_index >= self.render_structural_signals.items.len) {
            failHost("host render structural signal table is not indexed by signal id");
        }
        self.render_structural_signals.items[signal_index] = true;
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

    fn recordDispatch(self: *HostEnv) void {
        self.dispatch_metrics.events_processed += 1;
        self.dispatch_metrics.recompute_batches += 1;
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

    fn stateValueByNodeId(self: *HostEnv, node_id: u64) abi.NodeValue {
        const state_index = self.stateIndexByNodeId(node_id) orelse failHost("signal referenced an unknown active state node");
        return self.states.items[state_index].cell.value;
    }

    fn ensureStateFromDesc(self: *HostEnv, roc_host: *abi.RocHost, desc: HostNodeStateDesc) void {
        if (self.stateIndexByNodeId(desc.node_id) != null) return;

        const initial = callValueInitThunk(roc_host, desc.initial);
        var cell = HostValueCell.initRetained(initial, desc.eq, &self.pending_roc_metrics);
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

    fn updateStateValue(self: *HostEnv, roc_host: *abi.RocHost, node_id: u64, value: abi.NodeValue) bool {
        const state_index = self.stateIndexByNodeId(node_id) orelse failHost("event referenced an unknown active state node");
        const state = &self.states.items[state_index];
        if (state.cell.valueEquals(roc_host, value)) {
            abi.decrefNodeValue(value, roc_host);
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
                .root, .each_row => {},
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

    fn createEachRowScope(self: *HostEnv, roc_host: *abi.RocHost, parent_scope_id: u64, site_ordinal: u64, key: abi.NodeValue, item: abi.NodeValue) u64 {
        self.validateScopeId(parent_scope_id);

        abi.increfNodeValue(key, 1);
        errdefer abi.decrefNodeValue(key, roc_host);
        abi.increfNodeValue(item, 1);
        errdefer abi.decrefNodeValue(item, roc_host);

        const scope_id: u64 = @intCast(self.scopes.items.len);
        self.scopes.append(self.gpa.allocator(), .{
            .scope_id = scope_id,
            .parent_scope_id = parent_scope_id,
            .step = .{ .each_row = .{ .site_ordinal = site_ordinal, .key = key, .item = item } },
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

        for (self.dom_identities.items) |*identity| {
            if (identity.active and identity.scope_id == scope_id) {
                self.deactivateDomElement(identity.elem_id);
                identity.active = false;
            }
        }

        const scope = &self.scopes.items[@intCast(scope_id)];
        scope.step.deinit(roc_host);
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
                .root, .when_branch => {},
            }
        }

        return ids.toOwnedSlice(allocator) catch std.process.exit(1);
    }

    fn eachRowScopeKey(self: *HostEnv, scope_id: u64) abi.NodeValue {
        self.validateScopeId(scope_id);
        const scope = self.scopes.items[@intCast(scope_id)];
        return switch (scope.step) {
            .each_row => |row| row.key,
            .root, .when_branch => failHost("scope id does not reference an each-row scope"),
        };
    }

    fn eachRowScopeItem(self: *HostEnv, scope_id: u64) abi.NodeValue {
        self.validateScopeId(scope_id);
        const scope = self.scopes.items[@intCast(scope_id)];
        return switch (scope.step) {
            .each_row => |row| row.item,
            .root, .when_branch => failHost("scope id does not reference an each-row scope"),
        };
    }

    fn replaceEachRowScopeItem(self: *HostEnv, roc_host: *abi.RocHost, scope_id: u64, item: abi.NodeValue) void {
        self.validateScopeId(scope_id);
        const scope = &self.scopes.items[@intCast(scope_id)];
        switch (scope.step) {
            .each_row => {},
            .root, .when_branch => failHost("scope id does not reference an each-row scope"),
        }

        abi.increfNodeValue(item, 1);
        abi.decrefNodeValue(scope.step.each_row.item, roc_host);
        scope.step.each_row.item = item;
    }

    fn nextKeyIsDuplicate(roc_host: *abi.RocHost, key_eq: abi.RocErasedCallable, keys: []const abi.NodeValue, key_index: usize) bool {
        const key = keys[key_index];
        for (keys[0..key_index]) |previous| {
            if (callErasedNodeValueNodeValueToBool(roc_host, key_eq, previous, key)) {
                return true;
            }
        }
        return false;
    }

    fn syncEachRowScopes(self: *HostEnv, roc_host: *abi.RocHost, parent_scope_id: u64, site_ordinal: u64, keys: []const abi.NodeValue, items: []const abi.NodeValue, key_eq: abi.RocErasedCallable, item_eq: abi.RocErasedCallable) HostKeyedRowDiffResult {
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

        var rows_reused: u64 = 0;
        var rows_created: u64 = 0;
        var row_items_unchanged: u64 = 0;
        var row_items_updated: u64 = 0;

        for (keys, items, 0..) |key, item, key_index| {
            if (HostEnv.nextKeyIsDuplicate(roc_host, key_eq, keys, key_index)) {
                failHost("Ui.each keyed scope received duplicate keys");
            }

            var matched_scope_id: ?u64 = null;
            for (existing_scope_ids, 0..) |scope_id, existing_index| {
                if (matched_existing[existing_index]) continue;
                const existing_key = self.eachRowScopeKey(scope_id);
                if (callErasedNodeValueNodeValueToBool(roc_host, key_eq, existing_key, key)) {
                    matched_existing[existing_index] = true;
                    matched_scope_id = scope_id;
                    break;
                }
            }

            if (matched_scope_id) |scope_id| {
                next_scope_ids[key_index] = scope_id;
                rows_reused += 1;
                const existing_item = self.eachRowScopeItem(scope_id);
                if (callErasedNodeValueNodeValueToBool(roc_host, item_eq, existing_item, item)) {
                    row_items_changed[key_index] = false;
                    row_items_unchanged += 1;
                } else {
                    self.replaceEachRowScopeItem(roc_host, scope_id, item);
                    row_items_changed[key_index] = true;
                    row_items_updated += 1;
                }
            } else {
                next_scope_ids[key_index] = self.createEachRowScope(roc_host, parent_scope_id, site_ordinal, key, item);
                row_items_changed[key_index] = true;
                rows_created += 1;
            }
        }

        var rows_removed: u64 = 0;
        for (existing_scope_ids, 0..) |scope_id, existing_index| {
            if (matched_existing[existing_index]) continue;
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
            .When => {
                _ = self.internNodeIdentity(scope_id, ordinal.*);
                ordinal.* += 1;
            },
            .Each => {
                _ = self.internNodeIdentity(scope_id, ordinal.*);
                ordinal.* += 1;
            },
            .Text, .TextSignal => {},
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

    fn collectNodeSignalExprSources(self: *HostEnv, allocator: std.mem.Allocator, expr: abi.NodeSignalExpr, binder_stack: []const HostBinderBinding, source_node_ids: *std.ArrayListUnmanaged(u64)) void {
        switch (expr.tag) {
            .Ref => {
                source_node_ids.append(allocator, HostEnv.resolveNodeBinderRef(binder_stack, expr.payload.ref)) catch std.process.exit(1);
            },
            .ConstValue => {},
            .Map => {
                self.collectNodeSignalExprSources(allocator, expr.payload.map._0.*, binder_stack, source_node_ids);
            },
            .Map2 => {
                self.collectNodeSignalExprSources(allocator, expr.payload.map2._0.*, binder_stack, source_node_ids);
                self.collectNodeSignalExprSources(allocator, expr.payload.map2._1.*, binder_stack, source_node_ids);
            },
            .Combine => {
                for (expr.payload.combine._0.items()) |child| {
                    self.collectNodeSignalExprSources(allocator, child, binder_stack, source_node_ids);
                }
            },
        }
    }

    fn nodeSignalExprSourceNodeIds(self: *HostEnv, allocator: std.mem.Allocator, expr: abi.NodeSignalExpr, binder_stack: []const HostBinderBinding) []u64 {
        var source_node_ids: std.ArrayListUnmanaged(u64) = .empty;
        self.collectNodeSignalExprSources(allocator, expr, binder_stack, &source_node_ids);
        return source_node_ids.toOwnedSlice(allocator) catch std.process.exit(1);
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
                const source_node_ids = self.nodeSignalExprSourceNodeIds(allocator, payload.signal.*, binder_stack);
                stream.appendSignalTextAttr(allocator, roc_host, &self.pending_roc_metrics, elem_id, field, payload.signal.*, source_node_ids);
            },
            .StaticBool => {
                const payload = attr.payload.static_bool;
                const field = HostEnv.renderBoolFieldFromAbi(payload.field);
                stream.appendStaticBoolAttr(allocator, elem_id, field, payload.value);
            },
            .SignalBool => {
                const payload = attr.payload.signal_bool;
                const field = HostEnv.renderBoolFieldFromAbi(payload.field);
                const source_node_ids = self.nodeSignalExprSourceNodeIds(allocator, payload.signal.*, binder_stack);
                stream.appendSignalBoolAttr(allocator, roc_host, &self.pending_roc_metrics, elem_id, field, payload.signal.*, source_node_ids);
            },
            .OnEvent => {
                const payload = attr.payload.on_event;
                const msg = payload.msg;
                const kind = HostEnv.renderEventKindFromAbi(payload.kind);
                const payload_kind = HostEnv.eventPayloadKindFromAbi(msg.payload_kind);
                const target_node_id = HostEnv.resolveNodeBinderRef(binder_stack, msg.binder);
                stream.appendEvent(allocator, roc_host, &self.pending_roc_metrics, elem_id, kind, msg.binder, target_node_id, payload_kind, msg.transform);
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
                const source_node_ids = self.nodeSignalExprSourceNodeIds(allocator, elem.payload.text_signal.*, binder_stack.items);
                stream.appendSignalTextNode(allocator, roc_host, &self.pending_roc_metrics, elem_id, parent_elem_id, scope_id, elem.payload.text_signal.*, source_node_ids);
            },
            .State => {
                const site_ordinal = ordinal.*;
                const node_id = self.internNodeIdentity(scope_id, site_ordinal);
                ordinal.* += 1;
                stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .state, binder_stack.items);
                stream.appendState(allocator, roc_host, &self.pending_roc_metrics, node_id, elem.payload.state.initial, elem.payload.state.eq);
                binder_stack.append(allocator, .{ .token = elem.payload.state.binder, .node_id = node_id }) catch std.process.exit(1);
                self.collectElemDescriptors(roc_host, stream, elem.payload.state.child.*, scope_id, parent_elem_id, ordinal, dom_ordinal, binder_stack);
                _ = binder_stack.pop() orelse unreachable;
            },
            .When => {
                const site_ordinal = ordinal.*;
                const node_id = self.internNodeIdentity(scope_id, site_ordinal);
                ordinal.* += 1;
                stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .when, binder_stack.items);
                const source_node_ids = self.nodeSignalExprSourceNodeIds(allocator, elem.payload.when.condition.*, binder_stack.items);
                stream.appendWhen(allocator, roc_host, &self.pending_roc_metrics, node_id, elem.payload.when.condition.*, source_node_ids);
            },
            .Each => {
                const site_ordinal = ordinal.*;
                const node_id = self.internNodeIdentity(scope_id, site_ordinal);
                ordinal.* += 1;
                stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .each, binder_stack.items);
                const source_node_ids = self.nodeSignalExprSourceNodeIds(allocator, elem.payload.each.items.*, binder_stack.items);
                stream.appendEach(
                    allocator,
                    roc_host,
                    &self.pending_roc_metrics,
                    node_id,
                    elem.payload.each.items.*,
                    source_node_ids,
                    elem.payload.each.key_of,
                    elem.payload.each.key_eq,
                    elem.payload.each.item_eq,
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
                .root, .each_row => {},
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

    fn collectElemEachRowDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, site: HostNodeScopeSiteDesc, each: HostNodeEachDesc, items: []const abi.NodeValue) HostKeyedRowDiffResult {
        if (site.kind != .each) {
            failHost("Elem row collection requires an each scope site");
        }
        if (site.node_id != each.node_id) {
            failHost("Elem row collection received mismatched each descriptors");
        }

        const allocator = self.gpa.allocator();
        const keys = allocator.alloc(abi.NodeValue, items.len) catch std.process.exit(1);
        var initialized_keys: usize = 0;
        defer {
            for (keys[0..initialized_keys]) |key| {
                abi.decrefNodeValue(key, roc_host);
            }
            allocator.free(keys);
        }

        for (items, 0..) |item, index| {
            keys[index] = callErasedNodeValueToNodeValue(roc_host, each.key_of, item);
            initialized_keys += 1;
        }

        const diff = self.syncEachRowScopes(roc_host, site.scope_id, site.ordinal, keys, items, each.key_eq, each.item_eq);

        var binder_stack: std.ArrayListUnmanaged(HostBinderBinding) = .empty;
        defer binder_stack.deinit(allocator);
        binder_stack.appendSlice(allocator, site.binder_bindings) catch std.process.exit(1);

        for (items, keys, diff.scope_ids) |item, key, row_scope_id| {
            const row_elem = callErasedNodeValueNodeValueToElem(roc_host, each.row, key, item);
            defer abi.decrefElem(row_elem, roc_host);

            var ordinal: u64 = 0;
            var dom_ordinal: u64 = 0;
            self.collectElemDescriptors(roc_host, stream, row_elem, row_scope_id, site.parent_elem_id, &ordinal, &dom_ordinal, &binder_stack);
        }

        return diff;
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
            .element => (findElementDesc(stream, node.elem_id) orelse failHost("render node referenced missing element descriptor")).scope_id,
            .text => (findTextNodeDesc(stream, node.elem_id) orelse failHost("render node referenced missing text descriptor")).scope_id,
            .signal_text => (findSignalTextNodeDesc(stream, node.elem_id) orelse failHost("render node referenced missing signal text descriptor")).scope_id,
        };
    }

    fn streamNodeIdInScopeSubtree(self: *HostEnv, previous: *const HostNodeDescriptorStream, node_id: u64, root_scope_id: u64) bool {
        for (previous.scope_sites.items) |site| {
            if (site.node_id == node_id and self.scopeIsDescendantOrSelf(site.scope_id, root_scope_id)) return true;
        }
        return false;
    }

    fn scopeSubtreeHasDirtyStructuralSource(self: *HostEnv, previous: *const HostNodeDescriptorStream, root_scope_id: u64, dirty_source_node_ids: []const u64) bool {
        if (dirty_source_node_ids.len == 0) return false;

        for (previous.whens.items) |desc| {
            if (!self.streamNodeIdInScopeSubtree(previous, desc.node_id, root_scope_id)) continue;
            if (sourceNodeIdsIntersect(desc.source_node_ids, dirty_source_node_ids)) return true;
        }
        for (previous.eaches.items) |desc| {
            if (!self.streamNodeIdInScopeSubtree(previous, desc.node_id, root_scope_id)) continue;
            if (sourceNodeIdsIntersect(desc.source_node_ids, dirty_source_node_ids)) return true;
        }
        return false;
    }

    fn cloneHostSignalCacheSlot(slot: HostSignalCacheSlot, metrics: *RuntimeMetrics) HostSignalCacheSlot {
        return switch (slot) {
            .absent => .absent,
            .present => |cached| blk: {
                abi.increfNodeValue(cached.value, 1);
                abi.increfErasedCallable(cached.eq, 1);
                metrics.closure_retains += 1;
                break :blk .{ .present = cached };
            },
        };
    }

    fn copyActiveScopeSubtreeDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, root_scope_id: u64) void {
        const allocator = self.gpa.allocator();
        const previous = &self.active_stream;
        var copied_elem_ids: std.ArrayListUnmanaged(u64) = .empty;
        defer copied_elem_ids.deinit(allocator);

        for (previous.render_nodes.items) |node| {
            const node_scope_id = HostEnv.renderNodeScopeId(previous, node);
            if (!self.scopeIsDescendantOrSelf(node_scope_id, root_scope_id)) continue;

            copied_elem_ids.append(allocator, node.elem_id) catch std.process.exit(1);
            switch (node.kind) {
                .element => {
                    const desc = findElementDesc(previous, node.elem_id) orelse failHost("render node referenced missing element descriptor");
                    _ = stream.appendElement(allocator, desc.elem_id, desc.parent_elem_id, desc.scope_id, desc.tag);
                },
                .text => {
                    const desc = findTextNodeDesc(previous, node.elem_id) orelse failHost("render node referenced missing text descriptor");
                    stream.appendTextNode(allocator, desc.elem_id, desc.parent_elem_id, desc.scope_id, desc.value);
                },
                .signal_text => {
                    const desc = findSignalTextNodeDesc(previous, node.elem_id) orelse failHost("render node referenced missing signal text descriptor");
                    const source_node_ids = allocator.dupe(u64, desc.source_node_ids) catch std.process.exit(1);
                    stream.appendSignalTextNode(allocator, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.parent_elem_id, desc.scope_id, desc.signal, source_node_ids);
                    stream.signal_text_nodes.items[stream.signal_text_nodes.items.len - 1].cached_value = HostEnv.cloneHostSignalCacheSlot(desc.cached_value, &self.pending_roc_metrics);
                },
            }
        }

        for (previous.static_text_attrs.items) |desc| {
            if (!u64SliceContains(copied_elem_ids.items, desc.elem_id)) continue;
            stream.appendStaticTextAttr(allocator, desc.elem_id, desc.field, desc.value);
        }
        for (previous.signal_text_attrs.items) |desc| {
            if (!u64SliceContains(copied_elem_ids.items, desc.elem_id)) continue;
            const source_node_ids = allocator.dupe(u64, desc.source_node_ids) catch std.process.exit(1);
            stream.appendSignalTextAttr(allocator, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.field, desc.signal, source_node_ids);
            stream.signal_text_attrs.items[stream.signal_text_attrs.items.len - 1].cached_value = HostEnv.cloneHostSignalCacheSlot(desc.cached_value, &self.pending_roc_metrics);
        }
        for (previous.static_bool_attrs.items) |desc| {
            if (!u64SliceContains(copied_elem_ids.items, desc.elem_id)) continue;
            stream.appendStaticBoolAttr(allocator, desc.elem_id, desc.field, desc.value);
        }
        for (previous.signal_bool_attrs.items) |desc| {
            if (!u64SliceContains(copied_elem_ids.items, desc.elem_id)) continue;
            const source_node_ids = allocator.dupe(u64, desc.source_node_ids) catch std.process.exit(1);
            stream.appendSignalBoolAttr(allocator, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.field, desc.signal, source_node_ids);
            stream.signal_bool_attrs.items[stream.signal_bool_attrs.items.len - 1].cached_value = HostEnv.cloneHostSignalCacheSlot(desc.cached_value, &self.pending_roc_metrics);
        }
        for (previous.events.items) |desc| {
            if (!u64SliceContains(copied_elem_ids.items, desc.elem_id)) continue;
            stream.appendEvent(allocator, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.kind, desc.binder_token, desc.target_node_id, desc.payload_kind, desc.transform);
        }

        for (previous.scope_sites.items) |desc| {
            if (!self.scopeIsDescendantOrSelf(desc.scope_id, root_scope_id)) continue;
            stream.appendScopeSite(allocator, desc.node_id, desc.scope_id, desc.ordinal, desc.parent_elem_id, desc.kind, desc.binder_bindings);
        }
        for (previous.states.items) |desc| {
            if (!self.streamNodeIdInScopeSubtree(previous, desc.node_id, root_scope_id)) continue;
            stream.appendState(allocator, roc_host, &self.pending_roc_metrics, desc.node_id, desc.initial, desc.eq);
        }
        for (previous.whens.items) |desc| {
            if (!self.streamNodeIdInScopeSubtree(previous, desc.node_id, root_scope_id)) continue;
            const source_node_ids = allocator.dupe(u64, desc.source_node_ids) catch std.process.exit(1);
            stream.appendWhen(allocator, roc_host, &self.pending_roc_metrics, desc.node_id, desc.condition, source_node_ids);
            stream.whens.items[stream.whens.items.len - 1].cached_value = HostEnv.cloneHostSignalCacheSlot(desc.cached_value, &self.pending_roc_metrics);
        }
        for (previous.eaches.items) |desc| {
            if (!self.streamNodeIdInScopeSubtree(previous, desc.node_id, root_scope_id)) continue;
            const source_node_ids = allocator.dupe(u64, desc.source_node_ids) catch std.process.exit(1);
            stream.appendEach(allocator, roc_host, &self.pending_roc_metrics, desc.node_id, desc.items, source_node_ids, desc.key_of, desc.key_eq, desc.item_eq, desc.row);
            stream.eaches.items[stream.eaches.items.len - 1].cached_value = HostEnv.cloneHostSignalCacheSlot(desc.cached_value, &self.pending_roc_metrics);
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
                const source_node_ids = self.nodeSignalExprSourceNodeIds(allocator, elem.payload.text_signal.*, binder_stack.items);
                stream.appendSignalTextNode(allocator, roc_host, &self.pending_roc_metrics, elem_id, parent_elem_id, scope_id, elem.payload.text_signal.*, source_node_ids);
            },
            .State => {
                const site_ordinal = ordinal.*;
                const node_id = self.internNodeIdentity(scope_id, site_ordinal);
                ordinal.* += 1;
                stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .state, binder_stack.items);
                stream.appendState(allocator, roc_host, &self.pending_roc_metrics, node_id, elem.payload.state.initial, elem.payload.state.eq);
                self.ensureStateFromDesc(roc_host, stream.states.items[stream.states.items.len - 1]);
                binder_stack.append(allocator, .{ .token = elem.payload.state.binder, .node_id = node_id }) catch std.process.exit(1);
                self.collectActiveElemDescriptors(roc_host, stream, elem.payload.state.child.*, scope_id, parent_elem_id, ordinal, dom_ordinal, binder_stack, dirty_source_node_ids);
                _ = binder_stack.pop() orelse unreachable;
            },
            .When => {
                const site_ordinal = ordinal.*;
                const node_id = self.internNodeIdentity(scope_id, site_ordinal);
                ordinal.* += 1;
                stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .when, binder_stack.items);
                const source_node_ids = self.nodeSignalExprSourceNodeIds(allocator, elem.payload.when.condition.*, binder_stack.items);
                stream.appendWhen(allocator, roc_host, &self.pending_roc_metrics, node_id, elem.payload.when.condition.*, source_node_ids);

                const when_index = stream.whens.items.len - 1;
                const condition = evalNodeSignalExpr(self, roc_host, elem.payload.when.condition.*, source_node_ids);
                stream.whens.items[when_index].cached_value.replace(roc_host, &self.pending_roc_metrics, condition, signalExprCacheEqCallable(self, elem.payload.when.condition.*, source_node_ids));
                const active_branch: HostScopeBranch = if (nodeValueBoolValue(condition)) .true_branch else .false_branch;
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
                const source_node_ids = self.nodeSignalExprSourceNodeIds(allocator, elem.payload.each.items.*, binder_stack.items);
                stream.appendEach(
                    allocator,
                    roc_host,
                    &self.pending_roc_metrics,
                    node_id,
                    elem.payload.each.items.*,
                    source_node_ids,
                    elem.payload.each.key_of,
                    elem.payload.each.key_eq,
                    elem.payload.each.item_eq,
                    elem.payload.each.row,
                );
                const each_index = stream.eaches.items.len - 1;
                const each_desc = stream.eaches.items[stream.eaches.items.len - 1];

                const items_value = evalNodeSignalExpr(self, roc_host, elem.payload.each.items.*, source_node_ids);
                stream.eaches.items[each_index].cached_value.replace(roc_host, &self.pending_roc_metrics, items_value, signalExprCacheEqCallable(self, elem.payload.each.items.*, source_node_ids));
                const items = nodeValueListItems(items_value);

                const keys = allocator.alloc(abi.NodeValue, items.len) catch std.process.exit(1);
                var initialized_keys: usize = 0;
                defer {
                    for (keys[0..initialized_keys]) |key| {
                        abi.decrefNodeValue(key, roc_host);
                    }
                    allocator.free(keys);
                }

                for (items, 0..) |item, index| {
                    keys[index] = callErasedNodeValueToNodeValue(roc_host, each_desc.key_of, item);
                    initialized_keys += 1;
                }

                const diff = self.syncEachRowScopes(roc_host, scope_id, site_ordinal, keys, items, each_desc.key_eq, each_desc.item_eq);
                defer {
                    allocator.free(diff.scope_ids);
                    allocator.free(diff.row_items_changed);
                }

                for (items, keys, diff.scope_ids, diff.row_items_changed) |item, key, row_scope_id, row_item_changed| {
                    if (!row_item_changed and !self.scopeSubtreeHasDirtyStructuralSource(&self.active_stream, row_scope_id, dirty_source_node_ids)) {
                        self.copyActiveScopeSubtreeDescriptors(roc_host, stream, row_scope_id);
                        continue;
                    }

                    const row_elem = callErasedNodeValueNodeValueToElem(roc_host, each_desc.row, key, item);
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
                scope.step.deinit(roc_host);
            }
        } else if (self.scopes.items.len != 0) {
            failHost("host scope table cannot release keys without a Roc host");
        }
        self.scopes.items.len = 0;
    }

    fn deinit(self: *HostEnv) void {
        const allocator = self.gpa.allocator();

        self.active_stream.deinit(allocator, self.roc_host.?, &self.pending_roc_metrics);

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
        self.clearRenderSinkRoutes();
        self.render_text_sink_routes.deinit(allocator);
        self.render_bool_sink_routes.deinit(allocator);
        self.render_structural_signals.deinit(allocator);

        freeSpecCommands(allocator, self.test_state.commands);

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
        .closure_releases = 0,
        .closure_retains = 0,
        .derived_calls_into_roc = 0,
        .events_processed = 0,
        .nodes_recomputed = 0,
        .patches_emitted = 0,
        .propagation_prunes = 0,
        .recompute_batches = 0,
        .retained_alloc_delta = 0,
        .rows_created = 0,
        .rows_removed = 0,
        .rows_reused = 0,
        .scopes_created = 0,
        .scopes_disposed = 0,
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

fn rocAllocFn(roc_host: *abi.RocHost, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const host = hostFromRocHost(roc_host);
    const allocator = host.gpa.allocator();
    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
    const size_storage_bytes = min_alignment;
    const total_size = length + size_storage_bytes;

    const base_ptr = allocator.rawAlloc(total_size, align_enum, @returnAddress()) orelse std.process.exit(1);
    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;

    host.roc_allocations.append(host.gpa.allocator(), .{
        .ptr = base_ptr,
        .total_size = total_size,
        .alignment = align_enum,
    }) catch std.process.exit(1);
    host.alloc_count += 1;

    return @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);
}

fn rocDeallocFn(roc_host: *abi.RocHost, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    const host = hostFromRocHost(roc_host);
    const allocator = host.gpa.allocator();
    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
    const size_storage_bytes = min_alignment;
    const size_ptr: *const usize = @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize));
    const total_size = size_ptr.*;
    const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(ptr) - size_storage_bytes);

    for (host.roc_allocations.items, 0..) |alloc, i| {
        if (alloc.ptr == base_ptr) {
            _ = host.roc_allocations.swapRemove(i);
            break;
        }
    }
    host.dealloc_count += 1;

    allocator.rawFree(base_ptr[0..total_size], align_enum, @returnAddress());
}

fn rocReallocFn(roc_host: *abi.RocHost, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const host = hostFromRocHost(roc_host);
    const allocator = host.gpa.allocator();
    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
    const size_storage_bytes = min_alignment;
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize));
    const old_total_size = old_size_ptr.*;
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(ptr) - size_storage_bytes);
    const old_user_data_size = old_total_size - size_storage_bytes;
    const new_total_size = new_length + size_storage_bytes;

    const new_ptr = allocator.rawAlloc(new_total_size, align_enum, @returnAddress()) orelse std.process.exit(1);
    const new_user_ptr: [*]u8 = @ptrFromInt(@intFromPtr(new_ptr) + size_storage_bytes);
    const old_user_ptr: [*]const u8 = @ptrCast(ptr);
    const copy_size = @min(old_user_data_size, new_length);
    @memcpy(new_user_ptr[0..copy_size], old_user_ptr[0..copy_size]);

    for (host.roc_allocations.items, 0..) |alloc, i| {
        if (alloc.ptr == old_base_ptr) {
            _ = host.roc_allocations.swapRemove(i);
            break;
        }
    }
    host.roc_allocations.append(host.gpa.allocator(), .{
        .ptr = new_ptr,
        .total_size = new_total_size,
        .alignment = align_enum,
    }) catch std.process.exit(1);

    allocator.rawFree(old_base_ptr[0..old_total_size], align_enum, @returnAddress());

    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;
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

fn failHost(message: []const u8) noreturn {
    writeStderr("HOST ERROR: ");
    writeStderr(message);
    writeStderr("\n");
    std.process.exit(1);
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
        .closure_releases = left.closure_releases + right.closure_releases,
        .closure_retains = left.closure_retains + right.closure_retains,
        .derived_calls_into_roc = left.derived_calls_into_roc + right.derived_calls_into_roc,
        .events_processed = left.events_processed + right.events_processed,
        .nodes_recomputed = left.nodes_recomputed + right.nodes_recomputed,
        .patches_emitted = left.patches_emitted + right.patches_emitted,
        .propagation_prunes = left.propagation_prunes + right.propagation_prunes,
        .recompute_batches = left.recompute_batches + right.recompute_batches,
        .retained_alloc_delta = left.retained_alloc_delta + right.retained_alloc_delta,
        .rows_created = left.rows_created + right.rows_created,
        .rows_removed = left.rows_removed + right.rows_removed,
        .rows_reused = left.rows_reused + right.rows_reused,
        .scopes_created = left.scopes_created + right.scopes_created,
        .scopes_disposed = left.scopes_disposed + right.scopes_disposed,
    };
}

fn u64MetricAsI64(value: u64) i64 {
    return std.math.cast(i64, value) orelse failHost("runtime metric exceeded signed assertion range");
}

fn runtimeMetricValue(metrics: RuntimeMetrics, name: []const u8) ?i64 {
    if (std.mem.eql(u8, name, "events_processed")) return u64MetricAsI64(metrics.events_processed);
    if (std.mem.eql(u8, name, "nodes_recomputed")) return u64MetricAsI64(metrics.nodes_recomputed);
    if (std.mem.eql(u8, name, "propagation_prunes")) return u64MetricAsI64(metrics.propagation_prunes);
    if (std.mem.eql(u8, name, "derived_calls_into_roc")) return u64MetricAsI64(metrics.derived_calls_into_roc);
    if (std.mem.eql(u8, name, "recompute_batches")) return u64MetricAsI64(metrics.recompute_batches);
    if (std.mem.eql(u8, name, "patches_emitted")) return u64MetricAsI64(metrics.patches_emitted);
    if (std.mem.eql(u8, name, "scopes_created")) return u64MetricAsI64(metrics.scopes_created);
    if (std.mem.eql(u8, name, "scopes_disposed")) return u64MetricAsI64(metrics.scopes_disposed);
    if (std.mem.eql(u8, name, "rows_reused")) return u64MetricAsI64(metrics.rows_reused);
    if (std.mem.eql(u8, name, "rows_created")) return u64MetricAsI64(metrics.rows_created);
    if (std.mem.eql(u8, name, "rows_removed")) return u64MetricAsI64(metrics.rows_removed);
    if (std.mem.eql(u8, name, "closure_retains")) return u64MetricAsI64(metrics.closure_retains);
    if (std.mem.eql(u8, name, "closure_releases")) return u64MetricAsI64(metrics.closure_releases);
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
    if (!elem.active) failHost("DOM command referenced inactive element");
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

fn nodeValueText(value: *const abi.NodeValue) []const u8 {
    return switch (value.tag) {
        .NvStr => value.payload.nv_str.asSlice(),
        else => failHost("render text sink expected a Str signal value"),
    };
}

fn nodeValueBoolValue(value: abi.NodeValue) bool {
    return switch (value.tag) {
        .NvBool => value.payload.nv_bool,
        else => failHost("render bool sink expected a Bool signal value"),
    };
}

fn traceHostDomReset() void {
    if (!traceEnabled()) return;
    writeStderr("[HOST CMD] ResetDom\n");
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
    for (stream.elements.items) |desc| {
        if (desc.elem_id == elem_id) return desc;
    }
    return null;
}

fn findTextNodeDesc(stream: *const HostNodeDescriptorStream, elem_id: u64) ?HostNodeTextNodeDesc {
    for (stream.text_nodes.items) |desc| {
        if (desc.elem_id == elem_id) return desc;
    }
    return null;
}

fn findSignalTextNodeDesc(stream: *const HostNodeDescriptorStream, elem_id: u64) ?HostNodeSignalTextNodeDesc {
    for (stream.signal_text_nodes.items) |desc| {
        if (desc.elem_id == elem_id) return desc;
    }
    return null;
}

fn findSignalTextNodeDescMutable(stream: *HostNodeDescriptorStream, elem_id: u64) ?*HostNodeSignalTextNodeDesc {
    for (stream.signal_text_nodes.items) |*desc| {
        if (desc.elem_id == elem_id) return desc;
    }
    return null;
}

fn streamHasTextField(stream: *const HostNodeDescriptorStream, elem_id: u64, field: RenderTextField) bool {
    if (field == .text and findTextNodeDesc(stream, elem_id) != null) return true;
    if (field == .text and findSignalTextNodeDesc(stream, elem_id) != null) return true;

    for (stream.static_text_attrs.items) |desc| {
        if (desc.elem_id == elem_id and desc.field == field) return true;
    }
    for (stream.signal_text_attrs.items) |desc| {
        if (desc.elem_id == elem_id and desc.field == field) return true;
    }
    return false;
}

fn streamHasBoolField(stream: *const HostNodeDescriptorStream, elem_id: u64, field: RenderBoolField) bool {
    for (stream.static_bool_attrs.items) |desc| {
        if (desc.elem_id == elem_id and desc.field == field) return true;
    }
    for (stream.signal_bool_attrs.items) |desc| {
        if (desc.elem_id == elem_id and desc.field == field) return true;
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
        .element => (findElementDesc(stream, node.elem_id) orelse failHost("render node referenced missing element descriptor")).tag,
        .text, .signal_text => "text",
    };
}

fn renderNodeParentElemId(stream: *const HostNodeDescriptorStream, node: HostRenderNode) u64 {
    return switch (node.kind) {
        .element => (findElementDesc(stream, node.elem_id) orelse failHost("render node referenced missing element descriptor")).parent_elem_id,
        .text => (findTextNodeDesc(stream, node.elem_id) orelse failHost("render node referenced missing text descriptor")).parent_elem_id,
        .signal_text => (findSignalTextNodeDesc(stream, node.elem_id) orelse failHost("render node referenced missing signal text descriptor")).parent_elem_id,
    };
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

fn cloneNodeValue(value: abi.NodeValue) abi.NodeValue {
    abi.increfNodeValue(value, 1);
    return value;
}

fn nodeValueListItems(value: abi.NodeValue) []const abi.NodeValue {
    return switch (value.tag) {
        .NvList => value.payload.nv_list.items(),
        else => failHost("structural list signal expected a List value"),
    };
}

fn evalNodeSignalExprWithSources(host: *HostEnv, roc_host: *abi.RocHost, expr: abi.NodeSignalExpr, source_node_ids: []const u64, source_index: *usize) abi.NodeValue {
    switch (expr.tag) {
        .Ref => {
            if (source_index.* >= source_node_ids.len) failHost("signal source table ended before all refs were evaluated");
            const node_id = source_node_ids[source_index.*];
            source_index.* += 1;
            return cloneNodeValue(host.stateValueByNodeId(node_id));
        },
        .ConstValue => return callValueInitThunk(roc_host, expr.payload.const_value._0),
        .Map => {
            const input = evalNodeSignalExprWithSources(host, roc_host, expr.payload.map._0.*, source_node_ids, source_index);
            defer abi.decrefNodeValue(input, roc_host);
            var metrics = host.pending_roc_metrics;
            metrics.derived_calls_into_roc += 1;
            host.pending_roc_metrics = metrics;
            return callErasedNodeValueToNodeValue(roc_host, expr.payload.map._1, input);
        },
        .Map2 => {
            const left = evalNodeSignalExprWithSources(host, roc_host, expr.payload.map2._0.*, source_node_ids, source_index);
            defer abi.decrefNodeValue(left, roc_host);
            const right = evalNodeSignalExprWithSources(host, roc_host, expr.payload.map2._1.*, source_node_ids, source_index);
            defer abi.decrefNodeValue(right, roc_host);
            var metrics = host.pending_roc_metrics;
            metrics.derived_calls_into_roc += 1;
            host.pending_roc_metrics = metrics;
            return callErasedNodeValueNodeValueToNodeValue(roc_host, expr.payload.map2._2, left, right);
        },
        .Combine => {
            var values: std.ArrayListUnmanaged(abi.NodeValue) = .empty;
            errdefer {
                for (values.items) |value| {
                    abi.decrefNodeValue(value, roc_host);
                }
                values.deinit(host.gpa.allocator());
            }
            for (expr.payload.combine._0.items()) |child| {
                values.append(host.gpa.allocator(), evalNodeSignalExprWithSources(host, roc_host, child, source_node_ids, source_index)) catch std.process.exit(1);
            }
            const list = abi.RocList(abi.NodeValue).fromSlice(values.items, roc_host);
            values.deinit(host.gpa.allocator());
            const list_value: abi.NodeValue = .{ .payload = .{ .nv_list = list }, .tag = .NvList };
            defer abi.decrefNodeValue(list_value, roc_host);
            var metrics = host.pending_roc_metrics;
            metrics.derived_calls_into_roc += 1;
            host.pending_roc_metrics = metrics;
            return callErasedNodeValueToNodeValue(roc_host, expr.payload.combine._1, list_value);
        },
    }
}

fn evalNodeSignalExpr(host: *HostEnv, roc_host: *abi.RocHost, expr: abi.NodeSignalExpr, source_node_ids: []const u64) abi.NodeValue {
    var source_index: usize = 0;
    const value = evalNodeSignalExprWithSources(host, roc_host, expr, source_node_ids, &source_index);
    if (source_index != source_node_ids.len) failHost("signal source table contained unused refs");
    return value;
}

fn signalExprCacheEqCallable(host: *HostEnv, expr: abi.NodeSignalExpr, source_node_ids: []const u64) abi.RocErasedCallable {
    return switch (expr.tag) {
        .Ref => {
            if (source_node_ids.len != 1) failHost("Ref signal equality requires exactly one source node id");
            return host.stateEqCallable(source_node_ids[0]);
        },
        .Map => expr.payload.map._2,
        .Map2 => expr.payload.map2._3,
        .Combine => expr.payload.combine._2,
        .ConstValue => expr.payload.const_value._1,
    };
}

fn recordSignalPrune(host: *HostEnv) void {
    var metrics = host.pending_roc_metrics;
    metrics.propagation_prunes += 1;
    host.pending_roc_metrics = metrics;
}

fn updateDirtySignalCache(host: *HostEnv, roc_host: *abi.RocHost, cache_slot: *HostSignalCacheSlot, value: abi.NodeValue) bool {
    switch (cache_slot.*) {
        .absent => failHost("dirty signal expression was evaluated before its initial value was cached"),
        .present => |cached| {
            const values_equal = callErasedNodeValueNodeValueToBool(roc_host, cached.eq, cached.value, value);
            if (values_equal) {
                abi.decrefNodeValue(value, roc_host);
                recordSignalPrune(host);
                return false;
            }

            cache_slot.replaceValue(roc_host, value);
            return true;
        },
    }
}

fn evalSignalTextField(host: *HostEnv, roc_host: *abi.RocHost, elem_id: u64, field: RenderTextField, signal: abi.NodeSignalExpr, source_node_ids: []const u64, cache_slot: *HostSignalCacheSlot) bool {
    const value = evalNodeSignalExpr(host, roc_host, signal, source_node_ids);
    const changed = applyRenderTextField(host, elem_id, field, nodeValueText(&value));
    cache_slot.replace(roc_host, &host.pending_roc_metrics, value, signalExprCacheEqCallable(host, signal, source_node_ids));
    return changed;
}

fn evalSignalBoolField(host: *HostEnv, roc_host: *abi.RocHost, elem_id: u64, field: RenderBoolField, signal: abi.NodeSignalExpr, source_node_ids: []const u64, cache_slot: *HostSignalCacheSlot) bool {
    const value = evalNodeSignalExpr(host, roc_host, signal, source_node_ids);
    const changed = applyRenderBoolField(host, elem_id, field, nodeValueBoolValue(value));
    cache_slot.replace(roc_host, &host.pending_roc_metrics, value, signalExprCacheEqCallable(host, signal, source_node_ids));
    return changed;
}

fn evalDirtySignalTextField(host: *HostEnv, roc_host: *abi.RocHost, elem_id: u64, field: RenderTextField, signal: abi.NodeSignalExpr, source_node_ids: []const u64, cache_slot: *HostSignalCacheSlot) bool {
    const value = evalNodeSignalExpr(host, roc_host, signal, source_node_ids);
    if (!updateDirtySignalCache(host, roc_host, cache_slot, value)) return false;
    return applyRenderTextField(host, elem_id, field, nodeValueText(&value));
}

fn evalDirtySignalBoolField(host: *HostEnv, roc_host: *abi.RocHost, elem_id: u64, field: RenderBoolField, signal: abi.NodeSignalExpr, source_node_ids: []const u64, cache_slot: *HostSignalCacheSlot) bool {
    const value = evalNodeSignalExpr(host, roc_host, signal, source_node_ids);
    if (!updateDirtySignalCache(host, roc_host, cache_slot, value)) return false;
    return applyRenderBoolField(host, elem_id, field, nodeValueBoolValue(value));
}

fn sourceNodeIdsIntersect(left: []const u64, right: []const u64) bool {
    for (left) |left_id| {
        for (right) |right_id| {
            if (left_id == right_id) return true;
        }
    }
    return false;
}

fn dirtyStructuralRenderRequired(host: *HostEnv, roc_host: *abi.RocHost, dirty_source_node_ids: []const u64) bool {
    for (host.active_stream.whens.items) |*desc| {
        if (!sourceNodeIdsIntersect(desc.source_node_ids, dirty_source_node_ids)) continue;
        const value = evalNodeSignalExpr(host, roc_host, desc.condition, desc.source_node_ids);
        if (updateDirtySignalCache(host, roc_host, &desc.cached_value, value)) return true;
    }
    for (host.active_stream.eaches.items) |*desc| {
        if (!sourceNodeIdsIntersect(desc.source_node_ids, dirty_source_node_ids)) continue;
        const value = evalNodeSignalExpr(host, roc_host, desc.items, desc.source_node_ids);
        if (updateDirtySignalCache(host, roc_host, &desc.cached_value, value)) return true;
    }
    return false;
}

fn applyDirtyRenderSinks(host: *HostEnv, roc_host: *abi.RocHost, dirty_source_node_ids: []const u64) CommandCounts {
    var counts: CommandCounts = .{};

    for (host.active_stream.signal_text_nodes.items) |*desc| {
        if (!sourceNodeIdsIntersect(desc.source_node_ids, dirty_source_node_ids)) continue;
        if (evalDirtySignalTextField(host, roc_host, desc.elem_id, .text, desc.signal, desc.source_node_ids, &desc.cached_value)) {
            counts.addTextField(.text);
        }
    }

    for (host.active_stream.signal_text_attrs.items) |*desc| {
        if (!sourceNodeIdsIntersect(desc.source_node_ids, dirty_source_node_ids)) continue;
        if (evalDirtySignalTextField(host, roc_host, desc.elem_id, desc.field, desc.signal, desc.source_node_ids, &desc.cached_value)) {
            counts.addTextField(desc.field);
        }
    }

    for (host.active_stream.signal_bool_attrs.items) |*desc| {
        if (!sourceNodeIdsIntersect(desc.source_node_ids, dirty_source_node_ids)) continue;
        if (evalDirtySignalBoolField(host, roc_host, desc.elem_id, desc.field, desc.signal, desc.source_node_ids, &desc.cached_value)) {
            counts.addBoolField(desc.field);
        }
    }

    host.render_metrics.patches_emitted += counts.total;
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

fn applyNodeDescriptorStream(host: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream) CommandCounts {
    var counts: CommandCounts = .{};
    counts.addHostReset();
    traceHostDomReset();
    resetSimulatedDom(host);
    host.resetRenderSinkRoutes();

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
                if (evalSignalTextField(host, roc_host, desc.elem_id, .text, desc.signal, desc.source_node_ids, &desc.cached_value)) {
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
        if (evalSignalTextField(host, roc_host, desc.elem_id, desc.field, desc.signal, desc.source_node_ids, &desc.cached_value)) {
            counts.addTextField(desc.field);
        }
    }
    for (stream.static_bool_attrs.items) |desc| {
        if (applyRenderBoolField(host, desc.elem_id, desc.field, desc.value)) {
            counts.addBoolField(desc.field);
        }
    }
    for (stream.signal_bool_attrs.items) |*desc| {
        if (evalSignalBoolField(host, roc_host, desc.elem_id, desc.field, desc.signal, desc.source_node_ids, &desc.cached_value)) {
            counts.addBoolField(desc.field);
        }
    }
    for (stream.events.items, 0..) |desc, index| {
        bindNodeEvent(host, desc, @intCast(index + 1));
        counts.addEventBinding();
    }

    host.render_metrics.patches_emitted += counts.total;
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
        if (evalSignalTextField(host, roc_host, desc.elem_id, .text, desc.signal, desc.source_node_ids, &desc.cached_value)) {
            counts.addTextField(.text);
        }
    }
    for (stream.static_text_attrs.items) |desc| {
        if (applyRenderTextField(host, desc.elem_id, desc.field, desc.value)) {
            counts.addTextField(desc.field);
        }
    }
    for (stream.signal_text_attrs.items) |*desc| {
        if (evalSignalTextField(host, roc_host, desc.elem_id, desc.field, desc.signal, desc.source_node_ids, &desc.cached_value)) {
            counts.addTextField(desc.field);
        }
    }
    for (stream.static_bool_attrs.items) |desc| {
        if (applyRenderBoolField(host, desc.elem_id, desc.field, desc.value)) {
            counts.addBoolField(desc.field);
        }
    }
    for (stream.signal_bool_attrs.items) |*desc| {
        if (evalSignalBoolField(host, roc_host, desc.elem_id, desc.field, desc.signal, desc.source_node_ids, &desc.cached_value)) {
            counts.addBoolField(desc.field);
        }
    }

    applyStructuralEventBindings(host, stream, seen, &counts);

    host.render_metrics.patches_emitted += counts.total;
    return counts;
}

fn dropMovedElemPayload(_: ?*anyopaque, _: *abi.RocHost) callconv(.c) void {}

fn nodeValueUnit() abi.NodeValue {
    return .{
        .payload = .{ .nv_unit = .{} },
        .tag = .NvUnit,
    };
}

fn nodeValueStr(roc_host: *abi.RocHost, value: []const u8) abi.NodeValue {
    return .{
        .payload = .{ .nv_str = RocStr.fromSlice(value, roc_host) },
        .tag = .NvStr,
    };
}

fn nodeValueBool(value: bool) abi.NodeValue {
    return .{
        .payload = .{ .nv_bool = value },
        .tag = .NvBool,
    };
}

fn nodeValueI64(value: i64) abi.NodeValue {
    return .{
        .payload = .{ .nv_i64 = value },
        .tag = .NvI64,
    };
}

const ErasedNodeValueUnaryArgs = extern struct {
    arg0: abi.NodeValue,
};

const ErasedNodeValueBinaryArgs = extern struct {
    arg0: abi.NodeValue,
    arg1: abi.NodeValue,
};

fn erasedCallablePayload(callable: abi.RocErasedCallable) *abi.RocErasedCallablePayload {
    if (callable == null) failHost("host attempted to call a null Roc erased callable");
    return abi.rocErasedCallablePayloadPtr(callable);
}

fn callErasedNodeValueToNodeValue(roc_host: *abi.RocHost, callable: abi.RocErasedCallable, arg0: abi.NodeValue) abi.NodeValue {
    const payload = erasedCallablePayload(callable);
    abi.increfNodeValue(arg0, 1);
    var call_args = ErasedNodeValueUnaryArgs{ .arg0 = arg0 };
    var result: abi.NodeValue = undefined;
    payload.callable_fn_ptr(
        roc_host,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return result;
}

fn callValueInitThunk(roc_host: *abi.RocHost, callable: abi.RocErasedCallable) abi.NodeValue {
    return callErasedNodeValueToNodeValue(roc_host, callable, nodeValueUnit());
}

fn callErasedNodeValueNodeValueToNodeValue(roc_host: *abi.RocHost, callable: abi.RocErasedCallable, arg0: abi.NodeValue, arg1: abi.NodeValue) abi.NodeValue {
    const payload = erasedCallablePayload(callable);
    abi.increfNodeValue(arg0, 1);
    abi.increfNodeValue(arg1, 1);
    var call_args = ErasedNodeValueBinaryArgs{ .arg0 = arg0, .arg1 = arg1 };
    var result: abi.NodeValue = undefined;
    payload.callable_fn_ptr(
        roc_host,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return result;
}

fn callErasedNodeValueNodeValueToElem(roc_host: *abi.RocHost, callable: abi.RocErasedCallable, arg0: abi.NodeValue, arg1: abi.NodeValue) abi.Elem {
    const payload = erasedCallablePayload(callable);
    abi.increfNodeValue(arg0, 1);
    abi.increfNodeValue(arg1, 1);
    var call_args = ErasedNodeValueBinaryArgs{ .arg0 = arg0, .arg1 = arg1 };
    var result: abi.Elem = undefined;
    payload.callable_fn_ptr(
        roc_host,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return result;
}

fn callErasedNodeValueNodeValueToBool(roc_host: *abi.RocHost, callable: abi.RocErasedCallable, arg0: abi.NodeValue, arg1: abi.NodeValue) bool {
    const payload = erasedCallablePayload(callable);
    abi.increfNodeValue(arg0, 1);
    abi.increfNodeValue(arg1, 1);
    var call_args = ErasedNodeValueBinaryArgs{ .arg0 = arg0, .arg1 = arg1 };
    var result: bool = undefined;
    payload.callable_fn_ptr(
        roc_host,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return result;
}

fn hostEventById(host: *HostEnv, event_id: u64) HostNodeEventDesc {
    if (event_id == 0 or event_id > host.active_stream.events.items.len) {
        failHost("DOM event referenced an unknown active event descriptor");
    }
    return host.active_stream.events.items[@intCast(event_id - 1)];
}

fn validateNodeEventPayload(desc: HostNodeEventDesc, payload: abi.NodeValue) void {
    switch (desc.payload_kind) {
        .unit => if (payload.tag != .NvUnit) failHost("click event received a non-unit payload"),
        .str => if (payload.tag != .NvStr) failHost("input event received a non-str payload"),
        .bool => if (payload.tag != .NvBool) failHost("check event received a non-bool payload"),
    }
}

fn finishHostMetrics(host: *HostEnv) void {
    var metrics = addRuntimeMetrics(host.last_runtime_metrics, host.pending_roc_metrics);
    metrics.patches_emitted = host.render_metrics.patches_emitted;
    metrics.events_processed = host.dispatch_metrics.events_processed;
    metrics.recompute_batches = host.dispatch_metrics.recompute_batches;
    host.last_runtime_metrics = metrics;
    host.pending_roc_metrics = zeroRuntimeMetrics();
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

fn dispatchRocEventMeasured(host: *HostEnv, roc_host: *abi.RocHost, event_id: u64, payload: abi.NodeValue, stats: ?*BenchmarkStats) void {
    defer abi.decrefNodeValue(payload, roc_host);

    const desc = hostEventById(host, event_id);
    validateNodeEventPayload(desc, payload);
    host.recordDispatch();

    var metrics = host.pending_roc_metrics;
    metrics.nodes_recomputed += 1;
    metrics.derived_calls_into_roc += 1;
    host.pending_roc_metrics = metrics;

    const start_ns = benchmarkNowNs();
    const current = host.stateValueByNodeId(desc.target_node_id);
    const next = callErasedNodeValueNodeValueToNodeValue(roc_host, desc.transform, current, payload);
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
        if (dirtyStructuralRenderRequired(host, roc_host, &dirty_source_node_ids)) {
            renderActiveRootMeasured(host, roc_host, &dirty_source_node_ids, &s.dispatch_apply_ns, &s.commands);
        } else {
            const apply_start_ns = benchmarkNowNs();
            const counts = applyDirtyRenderSinks(host, roc_host, &dirty_source_node_ids);
            s.dispatch_apply_ns += benchmarkNowNs() - apply_start_ns;
            s.commands.addAll(counts);
            finishHostMetrics(host);
        }
        s.actions += 1;
    } else {
        const dirty_source_node_ids = [_]u64{desc.target_node_id};
        if (dirtyStructuralRenderRequired(host, roc_host, &dirty_source_node_ids)) {
            renderActiveRootMeasured(host, roc_host, &dirty_source_node_ids, null, null);
        } else {
            _ = applyDirtyRenderSinks(host, roc_host, &dirty_source_node_ids);
            finishHostMetrics(host);
        }
    }
}

fn dispatchRocEvent(host: *HostEnv, roc_host: *abi.RocHost, event_id: u64, payload: abi.NodeValue) void {
    dispatchRocEventMeasured(host, roc_host, event_id, payload, null);
}

fn makeSignalsRocHost(host: *HostEnv) abi.RocHost {
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
        .click, .fill, .check, .uncheck => true,
        else => false,
    };
}

fn runActionCommandMeasured(host: *HostEnv, roc_host: *abi.RocHost, cmd: SpecCommand, stats: *BenchmarkStats) void {
    switch (cmd.cmd_type) {
        .click => {
            const elem = host.findElementByLocator(cmd.locator, cmd.line_num) orelse failHost("benchmark click locator did not resolve");
            if (elem.disabled) failHost("benchmark click target is disabled");
            const event_id = elem.bound_click_event orelse failHost("benchmark click target has no binding");
            dispatchRocEventMeasured(host, roc_host, event_id, nodeValueUnit(), stats);
        },

        .fill => {
            const value = cmd.expected_text orelse "";
            const elem = host.findElementByLocator(cmd.locator, cmd.line_num) orelse failHost("benchmark fill locator did not resolve");
            if (elem.disabled) failHost("benchmark fill target is disabled");
            if (elem.bound_input_event) |event_id| {
                dispatchRocEventMeasured(host, roc_host, event_id, nodeValueStr(roc_host, value), stats);
            } else {
                _ = setElementValueIfChanged(host, elem, value);
            }
        },

        .check, .uncheck => {
            const checked = cmd.cmd_type == .check;
            const elem = host.findElementByLocator(cmd.locator, cmd.line_num) orelse failHost("benchmark check locator did not resolve");
            if (elem.disabled) failHost("benchmark check target is disabled");
            if (elem.bound_check_event) |event_id| {
                dispatchRocEventMeasured(host, roc_host, event_id, nodeValueBool(checked), stats);
            } else {
                _ = setElementCheckedIfChanged(elem, checked);
            }
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
    writeStdout("case,sample,iterations,actions,init_roc_ns,init_apply_ns,dispatch_roc_ns,dispatch_apply_ns,total_ns,allocs,deallocs,retained_alloc_delta,commands,reset_dom,create_element,append_child,set_text,set_value,set_checked,set_disabled,set_metadata,bind_event,events_processed,nodes_recomputed,propagation_prunes,derived_calls_into_roc,recompute_batches,patches_emitted,scopes_created,scopes_disposed,rows_reused,rows_created,rows_removed,closure_retains,closure_releases,metrics_retained_alloc_delta\n");
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
        "{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d}\n",
        .{
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
                dispatchRocEvent(&host_env, &roc_host, event_id, nodeValueUnit());
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
                    dispatchRocEvent(&host_env, &roc_host, event_id, nodeValueStr(&roc_host, value));
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
                    dispatchRocEvent(&host_env, &roc_host, event_id, nodeValueBool(checked));
                } else {
                    _ = setElementCheckedIfChanged(elem, checked);
                }
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

            .expect_metric_delta => {
                const metric_name = cmd.expected_text orelse "";
                const expected = u64MetricAsI64(cmd.expected_count orelse 0);
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
    left.events_processed = 2;
    left.nodes_recomputed = 5;
    left.propagation_prunes = 3;
    left.derived_calls_into_roc = 4;
    left.recompute_batches = 2;
    left.patches_emitted = 7;

    var right = zeroRuntimeMetrics();
    right.events_processed = 1;
    right.nodes_recomputed = 8;
    right.propagation_prunes = 11;
    right.derived_calls_into_roc = 6;
    right.recompute_batches = 1;
    right.patches_emitted = 13;
    right.retained_alloc_delta = -2;

    const total = addRuntimeMetrics(left, right);
    try std.testing.expectEqual(@as(u64, 3), total.events_processed);
    try std.testing.expectEqual(@as(u64, 13), total.nodes_recomputed);
    try std.testing.expectEqual(@as(u64, 14), total.propagation_prunes);
    try std.testing.expectEqual(@as(u64, 10), total.derived_calls_into_roc);
    try std.testing.expectEqual(@as(u64, 3), total.recompute_batches);
    try std.testing.expectEqual(@as(u64, 20), total.patches_emitted);
    try std.testing.expectEqual(@as(i64, -2), total.retained_alloc_delta);
}

const TestErasedI64Capture = extern struct {
    amount: i64,
};

const TestErasedBinderCapture = extern struct {
    condition_binder: HostBinderToken,
};

const TestErasedNodeValueCapture = extern struct {
    value: abi.NodeValue,
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

fn testUnaryNodeValueCallable(_: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    const capture = testCapturePtrAs(TestErasedI64Capture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedNodeValueUnaryArgs, args);
    const input = switch (call_args.arg0.tag) {
        .NvI64 => call_args.arg0.payload.nv_i64,
        else => @panic("test unary NodeValue callable expected NvI64"),
    };
    writeTestErasedResult(abi.NodeValue, ret, nodeValueI64(input + capture.amount));
}

fn testUnaryIdentityNodeValueCallable(_: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = capture_ptr;
    const call_args = testErasedArgsAs(ErasedNodeValueUnaryArgs, args);
    abi.increfNodeValue(call_args.arg0, 1);
    writeTestErasedResult(abi.NodeValue, ret, call_args.arg0);
}

fn testBinaryNodeValueCallable(_: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    const capture = testCapturePtrAs(TestErasedI64Capture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedNodeValueBinaryArgs, args);
    const left = switch (call_args.arg0.tag) {
        .NvI64 => call_args.arg0.payload.nv_i64,
        else => @panic("test binary NodeValue callable expected left NvI64"),
    };
    const right = switch (call_args.arg1.tag) {
        .NvI64 => call_args.arg1.payload.nv_i64,
        else => @panic("test binary NodeValue callable expected right NvI64"),
    };
    writeTestErasedResult(abi.NodeValue, ret, nodeValueI64(left + right + capture.amount));
}

fn testInitialNodeValueCallable(_: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = args;
    const capture = testCapturePtrAs(TestErasedNodeValueCapture, capture_ptr);
    abi.increfNodeValue(capture.value, 1);
    writeTestErasedResult(abi.NodeValue, ret, capture.value);
}

fn testBinaryElemCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    const capture = testCapturePtrAs(TestErasedI64Capture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedNodeValueBinaryArgs, args);
    const left = switch (call_args.arg0.tag) {
        .NvI64 => call_args.arg0.payload.nv_i64,
        else => @panic("test row Elem callable expected left NvI64"),
    };
    const right = switch (call_args.arg1.tag) {
        .NvI64 => call_args.arg1.payload.nv_i64,
        else => @panic("test row Elem callable expected right NvI64"),
    };
    var text_buffer: [64]u8 = undefined;
    const text = std.fmt.bufPrint(&text_buffer, "row-{d}", .{left + right + capture.amount}) catch @panic("test row Elem callable could not format text");
    writeTestErasedResult(abi.Elem, ret, testNodeText(roc_host, text));
}

fn testStatefulRowElemCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    test_row_elem_call_count += 1;
    const capture = testCapturePtrAs(TestErasedI64Capture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedNodeValueBinaryArgs, args);
    const key = switch (call_args.arg0.tag) {
        .NvI64 => call_args.arg0.payload.nv_i64,
        else => @panic("test stateful row Elem callable expected key NvI64"),
    };
    const item = switch (call_args.arg1.tag) {
        .NvI64 => call_args.arg1.payload.nv_i64,
        else => @panic("test stateful row Elem callable expected item NvI64"),
    };
    var text_buffer: [64]u8 = undefined;
    const text = std.fmt.bufPrint(&text_buffer, "row-{d}-{d}", .{ key, item + capture.amount }) catch @panic("test stateful row Elem callable could not format text");
    writeTestErasedResult(abi.Elem, ret, testNodeState(roc_host, testNodeText(roc_host, text)));
}

fn testStatefulRowButtonElemCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    test_row_elem_call_count += 1;
    const capture = testCapturePtrAs(TestErasedI64Capture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedNodeValueBinaryArgs, args);
    const key = switch (call_args.arg0.tag) {
        .NvI64 => call_args.arg0.payload.nv_i64,
        else => @panic("test stateful row button Elem callable expected key NvI64"),
    };
    const item = switch (call_args.arg1.tag) {
        .NvI64 => call_args.arg1.payload.nv_i64,
        else => @panic("test stateful row button Elem callable expected item NvI64"),
    };
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
    const call_args = testErasedArgsAs(ErasedNodeValueBinaryArgs, args);
    const key = switch (call_args.arg0.tag) {
        .NvI64 => call_args.arg0.payload.nv_i64,
        else => @panic("test nested when row Elem callable expected key NvI64"),
    };
    const item = switch (call_args.arg1.tag) {
        .NvI64 => call_args.arg1.payload.nv_i64,
        else => @panic("test nested when row Elem callable expected item NvI64"),
    };

    var true_text_buffer: [64]u8 = undefined;
    var false_text_buffer: [64]u8 = undefined;
    const true_text = std.fmt.bufPrint(&true_text_buffer, "row-{d}-{d}-true", .{ key, item }) catch @panic("test nested row true text format failed");
    const false_text = std.fmt.bufPrint(&false_text_buffer, "row-{d}-{d}-false", .{ key, item }) catch @panic("test nested row false text format failed");
    const row = abi.Elem{
        .payload = .{
            .when = .{
                .condition = boxTestNodeSignalExpr(roc_host, testNodeRefExpr(capture.condition_binder)),
                .when_false = boxTestElem(roc_host, testNodeText(roc_host, false_text)),
                .when_true = boxTestElem(roc_host, testNodeText(roc_host, true_text)),
            },
        },
        .tag = .When,
    };
    writeTestErasedResult(abi.Elem, ret, row);
}

fn testNodeValueEqCallable(_: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = capture_ptr;
    const call_args = testErasedArgsAs(ErasedNodeValueBinaryArgs, args);
    const is_equal = if (call_args.arg0.tag != call_args.arg1.tag)
        false
    else switch (call_args.arg0.tag) {
        .NvBool => call_args.arg0.payload.nv_bool == call_args.arg1.payload.nv_bool,
        .NvI64 => call_args.arg0.payload.nv_i64 == call_args.arg1.payload.nv_i64,
        .NvStr => blk: {
            const left = call_args.arg0.payload.nv_str;
            const right = call_args.arg1.payload.nv_str;
            break :blk std.mem.eql(u8, left.asSlice(), right.asSlice());
        },
        .NvUnit => true,
        .NvList => false,
    };
    writeTestErasedResult(bool, ret, is_equal);
}

fn testStableStrNodeValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = args;
    _ = capture_ptr;
    writeTestErasedResult(abi.NodeValue, ret, nodeValueStr(roc_host, "stable"));
}

fn testStableBoolNodeValueCallable(_: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = args;
    _ = capture_ptr;
    writeTestErasedResult(abi.NodeValue, ret, nodeValueBool(true));
}

fn testAlwaysEqualNodeValueCallable(_: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = args;
    _ = capture_ptr;
    writeTestErasedResult(bool, ret, true);
}

fn testErasedCallableOnDrop(_: ?[*]u8, _: *abi.RocHost) callconv(.c) void {
    test_erased_callable_drop_count += 1;
}

fn testBinderCaptureOnDrop(capture_ptr: ?[*]u8, roc_host: *abi.RocHost) callconv(.c) void {
    test_erased_callable_drop_count += 1;
    const capture = testCapturePtrAs(TestErasedBinderCapture, capture_ptr);
    abi.decrefBox(@ptrCast(capture.condition_binder), roc_host);
}

fn testNodeValueCaptureOnDrop(capture_ptr: ?[*]u8, roc_host: *abi.RocHost) callconv(.c) void {
    test_erased_callable_drop_count += 1;
    const capture = testCapturePtrAs(TestErasedNodeValueCapture, capture_ptr);
    abi.decrefNodeValue(capture.value, roc_host);
}

fn expectNodeValueI64(value: abi.NodeValue, expected: i64) !void {
    try std.testing.expectEqual(abi.NodeValueTag.NvI64, value.tag);
    try std.testing.expectEqual(expected, value.payload.nv_i64);
}

test "signals host invokes erased NodeValue thunks with ABI argument layouts" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        host.roc_allocations.deinit(host.gpa.allocator());
        _ = host.gpa.deinit();
    }

    {
        const unary = writeTestErasedCallable(
            TestErasedI64Capture,
            &roc_host,
            &testUnaryNodeValueCallable,
            &testErasedCallableOnDrop,
            .{ .amount = 5 },
        );
        defer abi.decrefErasedCallable(unary, &roc_host);

        const result = callErasedNodeValueToNodeValue(&roc_host, unary, nodeValueI64(37));
        try expectNodeValueI64(result, 42);
    }

    {
        const binary = writeTestErasedCallable(
            TestErasedI64Capture,
            &roc_host,
            &testBinaryNodeValueCallable,
            &testErasedCallableOnDrop,
            .{ .amount = 3 },
        );
        defer abi.decrefErasedCallable(binary, &roc_host);

        const result = callErasedNodeValueNodeValueToNodeValue(&roc_host, binary, nodeValueI64(10), nodeValueI64(29));
        try expectNodeValueI64(result, 42);
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

        const result = callErasedNodeValueNodeValueToElem(&roc_host, row, nodeValueI64(10), nodeValueI64(29));
        defer abi.decrefElem(result, &roc_host);

        try std.testing.expectEqual(abi.ElemTag.Text, result.tag);
        try std.testing.expectEqualStrings("row-42", result.payload.text.asSlice());
    }

    {
        const eq = writeTestErasedCallable(
            TestErasedI64Capture,
            &roc_host,
            &testNodeValueEqCallable,
            &testErasedCallableOnDrop,
            .{ .amount = 0 },
        );
        defer abi.decrefErasedCallable(eq, &roc_host);

        try std.testing.expect(callErasedNodeValueNodeValueToBool(&roc_host, eq, nodeValueI64(42), nodeValueI64(42)));
        try std.testing.expect(!callErasedNodeValueNodeValueToBool(&roc_host, eq, nodeValueI64(41), nodeValueI64(42)));
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
        &testNodeValueEqCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    defer abi.decrefErasedCallable(key_eq, &roc_host);

    const initial_keys = [_]abi.NodeValue{ nodeValueI64(10), nodeValueI64(11) };
    const initial_rows = host.syncEachRowScopes(&roc_host, root, 7, &initial_keys, &initial_keys, key_eq, key_eq);
    defer freeKeyedRowDiff(&host, initial_rows);
    const row_a = initial_rows.scope_ids[0];
    const row_b = initial_rows.scope_ids[1];
    try std.testing.expect(row_b != row_a);

    const same_keys = [_]abi.NodeValue{nodeValueI64(10)};
    const same_rows = host.syncEachRowScopes(&roc_host, root, 7, &same_keys, &same_keys, key_eq, key_eq);
    defer freeKeyedRowDiff(&host, same_rows);
    try std.testing.expectEqual(row_a, same_rows.scope_ids[0]);

    const other_site_rows = host.syncEachRowScopes(&roc_host, root, 8, &same_keys, &same_keys, key_eq, key_eq);
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
        &testNodeValueEqCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    defer abi.decrefErasedCallable(key_eq, &roc_host);

    const root = host.internRootScope();
    const row = host.createEachRowScope(&roc_host, root, 3, nodeValueI64(10), nodeValueI64(10));
    const branch = host.internWhenBranchScope(row, 4, .true_branch);
    const row_state = host.internNodeIdentity(row, 0);
    const branch_state = host.internNodeIdentity(branch, 0);

    host.disposeScopeSubtree(&roc_host, row);
    try std.testing.expectEqual(@as(u64, 2), host.pending_roc_metrics.scopes_disposed);
    try std.testing.expect(!host.scopes.items[@intCast(row)].active);
    try std.testing.expect(!host.scopes.items[@intCast(branch)].active);
    try std.testing.expect(!host.node_identities.items[@intCast(row_state)].active);
    try std.testing.expect(!host.node_identities.items[@intCast(branch_state)].active);

    const recreated_row = host.createEachRowScope(&roc_host, root, 3, nodeValueI64(10), nodeValueI64(10));
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
        nodeValueStr(&roc_host, "first"),
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
    abi.decrefNodeValue(host.states.items[state_index].cell.value, &roc_host);
    host.states.items[state_index].cell.value = nodeValueStr(&roc_host, "second");
    host.states.items[state_index].version += 1;

    const dirty_source_node_ids = [_]u64{state_id};
    try std.testing.expect(!dirtyStructuralRenderRequired(&host, &roc_host, &dirty_source_node_ids));

    const patch_start = host.render_metrics.patches_emitted;
    const patch_counts = applyDirtyRenderSinks(&host, &roc_host, &dirty_source_node_ids);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.reset_dom);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.create_element);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.append_child);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.set_text);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.total);
    try std.testing.expectEqual(patch_start + 1, host.render_metrics.patches_emitted);
    try std.testing.expectEqual(@as(usize, 2), host.dom_elements.items.len);
    try std.testing.expectEqualStrings("second", host.dom_elements.items[1].text.?);

    const unchanged_counts = applyDirtyRenderSinks(&host, &roc_host, &dirty_source_node_ids);
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
        nodeValueI64(1),
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
    abi.decrefNodeValue(host.states.items[state_index].cell.value, &roc_host);
    host.states.items[state_index].cell.value = nodeValueI64(2);
    host.states.items[state_index].version += 1;

    const dirty_source_node_ids = [_]u64{state_id};
    const prune_start = host.pending_roc_metrics.propagation_prunes;
    const patch_counts = applyDirtyRenderSinks(&host, &roc_host, &dirty_source_node_ids);

    try std.testing.expectEqual(@as(u64, 0), patch_counts.total);
    try std.testing.expectEqual(prune_start + 1, host.pending_roc_metrics.propagation_prunes);
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

    const initial_items = [_]abi.NodeValue{ nodeValueI64(1), nodeValueI64(2) };
    const combine = testNodeCombineExpr(&roc_host, &.{});
    var cache: HostSignalCacheSlot = .absent;
    cache.replace(&roc_host, &host.pending_roc_metrics, nodeValueList(&roc_host, &initial_items), signalExprCacheEqCallable(&host, combine, &.{}));
    defer cache.deinit(&roc_host, &host.pending_roc_metrics);
    abi.decrefNodeSignalExpr(combine, &roc_host);

    const dirty_items = [_]abi.NodeValue{ nodeValueI64(3), nodeValueI64(4) };
    const prune_start = host.pending_roc_metrics.propagation_prunes;
    try std.testing.expect(!updateDirtySignalCache(&host, &roc_host, &cache, nodeValueList(&roc_host, &dirty_items)));
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
                .when_false = boxTestElem(&roc_host, testNodeText(&roc_host, "false branch")),
                .when_true = boxTestElem(&roc_host, testNodeText(&roc_host, "true branch")),
            },
        },
        .tag = .When,
    };
    const root = testNodeStateWithTokenAndInitial(&roc_host, state_token, nodeValueBool(true), when_elem);
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    const initial_counts = applyNodeDescriptorStream(&host, &roc_host, &stream);
    host.active_stream = stream;

    try std.testing.expectEqual(@as(u64, 1), initial_counts.reset_dom);
    const state_id = host.active_stream.scope_sites.items[0].node_id;
    const state_index = host.stateIndexByNodeId(state_id) orelse unreachable;
    abi.decrefNodeValue(host.states.items[state_index].cell.value, &roc_host);
    host.states.items[state_index].cell.value = nodeValueBool(false);
    host.states.items[state_index].version += 1;
    const dirty_source_node_ids = [_]u64{state_id};
    try std.testing.expect(dirtyStructuralRenderRequired(&host, &roc_host, &dirty_source_node_ids));

    var next_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &next_stream, root, &dirty_source_node_ids);
    const patch_counts = applyStructuralNodeDescriptorStream(&host, &roc_host, &next_stream);
    host.active_stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);
    host.active_stream = next_stream;

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
                .when_false = boxTestElem(&roc_host, testNodeText(&roc_host, "false branch")),
                .when_true = boxTestElem(&roc_host, testNodeText(&roc_host, "true branch")),
            },
        },
        .tag = .When,
    };
    const root = testNodeStateWithTokenAndInitial(&roc_host, state_token, nodeValueI64(1), when_elem);
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    _ = applyNodeDescriptorStream(&host, &roc_host, &stream);
    host.active_stream = stream;

    try std.testing.expect(activeTextElementId(&host, "true branch") != null);

    const state_id = host.active_stream.scope_sites.items[0].node_id;
    const state_index = host.stateIndexByNodeId(state_id) orelse unreachable;
    abi.decrefNodeValue(host.states.items[state_index].cell.value, &roc_host);
    host.states.items[state_index].cell.value = nodeValueI64(2);
    host.states.items[state_index].version += 1;

    const dirty_source_node_ids = [_]u64{state_id};
    const prune_start = host.pending_roc_metrics.propagation_prunes;

    try std.testing.expect(!dirtyStructuralRenderRequired(&host, &roc_host, &dirty_source_node_ids));
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

    const initial_items = [_]abi.NodeValue{ nodeValueI64(1), nodeValueI64(2), nodeValueI64(3) };
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

    const reordered_items = [_]abi.NodeValue{ nodeValueI64(3), nodeValueI64(1), nodeValueI64(2) };
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

    const changed_items = [_]abi.NodeValue{ nodeValueI64(2), nodeValueI64(4) };
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

test "signals host rebuilds unchanged row when nested structural source is dirty" {
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
    const items = [_]abi.NodeValue{nodeValueI64(1)};
    const children = [_]abi.Elem{
        testNodeEachWithNestedWhenRows(&roc_host, &items, state_token),
    };
    const section = testElementWith(&roc_host, "section", &.{}, &children);
    const root = testNodeStateWithTokenAndInitial(&roc_host, state_token, nodeValueBool(true), section);
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
    abi.decrefNodeValue(host.states.items[state_index].cell.value, &roc_host);
    host.states.items[state_index].cell.value = nodeValueBool(false);
    host.states.items[state_index].version += 1;

    const dirty_source_node_ids = [_]u64{state_id};
    try std.testing.expect(dirtyStructuralRenderRequired(&host, &roc_host, &dirty_source_node_ids));

    var next_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &next_stream, root, &dirty_source_node_ids);
    _ = applyStructuralNodeDescriptorStream(&host, &roc_host, &next_stream);
    host.active_stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);
    host.active_stream = next_stream;

    try std.testing.expectEqual(@as(u64, 2), test_row_elem_call_count);
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

    const initial_items = [_]abi.NodeValue{ nodeValueI64(1), nodeValueI64(2) };
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

    const reordered_items = [_]abi.NodeValue{ nodeValueI64(2), nodeValueI64(1) };
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

    const same_reordered_children = [_]abi.Elem{
        testNodeEachWithItemsAndRow(&roc_host, &reordered_items, &testStatefulRowButtonElemCallable),
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
    host.gpa.allocator().free(diff.scope_ids);
    host.gpa.allocator().free(diff.row_items_changed);
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

fn testNodeConstExpr(roc_host: *abi.RocHost, value: abi.NodeValue) abi.NodeSignalExpr {
    const eq = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testNodeValueEqCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    return .{
        .payload = .{ .const_value = .{
            ._0 = testNodeValueInitialThunk(roc_host, value),
            ._1 = eq,
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
        &testUnaryNodeValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 1 },
    );
    const eq = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testNodeValueEqCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    return .{
        .payload = .{
            .map = .{
                ._0 = boxTestNodeSignalExpr(roc_host, input),
                ._1 = transform,
                ._2 = eq,
            },
        },
        .tag = .Map,
    };
}

fn testNodeStableStrMapExpr(roc_host: *abi.RocHost, input: abi.NodeSignalExpr) abi.NodeSignalExpr {
    const transform = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testStableStrNodeValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const eq = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testNodeValueEqCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    return .{
        .payload = .{
            .map = .{
                ._0 = boxTestNodeSignalExpr(roc_host, input),
                ._1 = transform,
                ._2 = eq,
            },
        },
        .tag = .Map,
    };
}

fn testNodeStableBoolMapExpr(roc_host: *abi.RocHost, input: abi.NodeSignalExpr) abi.NodeSignalExpr {
    const transform = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testStableBoolNodeValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const eq = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testNodeValueEqCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    return .{
        .payload = .{
            .map = .{
                ._0 = boxTestNodeSignalExpr(roc_host, input),
                ._1 = transform,
                ._2 = eq,
            },
        },
        .tag = .Map,
    };
}

fn testNodeCombineExpr(roc_host: *abi.RocHost, children: []const abi.NodeSignalExpr) abi.NodeSignalExpr {
    const transform = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testUnaryIdentityNodeValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const eq = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testAlwaysEqualNodeValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    return .{
        .payload = .{
            .combine = .{
                ._0 = abi.RocList(abi.NodeSignalExpr).fromSlice(children, roc_host),
                ._1 = transform,
                ._2 = eq,
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
        .payload = .{ .text_signal = boxTestNodeSignalExpr(roc_host, signal) },
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
        &testBinaryNodeValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    return .{
        .payload = .{
            .on_event = .{
                .kind = @intFromEnum(kind),
                .msg = .{
                    .binder = cloneTestBinderToken(binder_token),
                    .payload_kind = @intFromEnum(payload_kind),
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

fn testNodeValueInitialThunk(roc_host: *abi.RocHost, initial: abi.NodeValue) abi.RocErasedCallable {
    return writeTestErasedCallable(
        TestErasedNodeValueCapture,
        roc_host,
        &testInitialNodeValueCallable,
        &testNodeValueCaptureOnDrop,
        .{ .value = initial },
    );
}

fn testNodeStateWithTokenAndInitial(roc_host: *abi.RocHost, binder_token: HostBinderToken, initial: abi.NodeValue, child: abi.Elem) abi.Elem {
    const initial_thunk = testNodeValueInitialThunk(roc_host, initial);
    const eq = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testNodeValueEqCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    return .{
        .payload = .{
            .state = .{
                .binder = binder_token,
                .child = boxTestElem(roc_host, child),
                .eq = eq,
                .initial = initial_thunk,
            },
        },
        .tag = .State,
    };
}

fn testNodeStateWithToken(roc_host: *abi.RocHost, binder_token: HostBinderToken, child: abi.Elem) abi.Elem {
    return testNodeStateWithTokenAndInitial(roc_host, binder_token, nodeValueI64(0), child);
}

fn testNodeState(roc_host: *abi.RocHost, child: abi.Elem) abi.Elem {
    return testNodeStateWithToken(roc_host, newTestBinderToken(roc_host), child);
}

fn testNodeWhen(roc_host: *abi.RocHost, when_true: abi.Elem, when_false: abi.Elem) abi.Elem {
    return .{
        .payload = .{
            .when = .{
                .condition = boxTestNodeSignalExpr(roc_host, testNodeConstExpr(roc_host, nodeValueBool(true))),
                .when_false = boxTestElem(roc_host, when_false),
                .when_true = boxTestElem(roc_host, when_true),
            },
        },
        .tag = .When,
    };
}

fn nodeValueList(roc_host: *abi.RocHost, items: []const abi.NodeValue) abi.NodeValue {
    return .{
        .payload = .{ .nv_list = abi.RocList(abi.NodeValue).fromSlice(items, roc_host) },
        .tag = .NvList,
    };
}

fn testNodeEachWithItemsAndRow(roc_host: *abi.RocHost, items: []const abi.NodeValue, row_fn: abi.RocErasedCallableFn) abi.Elem {
    const key_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testNodeValueEqCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const key_of = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testUnaryNodeValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const item_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testNodeValueEqCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
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
                .items = boxTestNodeSignalExpr(roc_host, testNodeConstExpr(roc_host, nodeValueList(roc_host, items))),
                .key_eq = key_eq,
                .key_of = key_of,
                .item_eq = item_eq,
                .row = row,
            },
        },
        .tag = .Each,
    };
}

fn testNodeEachWithNestedWhenRows(roc_host: *abi.RocHost, items: []const abi.NodeValue, condition_binder: HostBinderToken) abi.Elem {
    const key_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testNodeValueEqCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const key_of = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testUnaryNodeValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const item_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testNodeValueEqCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
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
                .items = boxTestNodeSignalExpr(roc_host, testNodeConstExpr(roc_host, nodeValueList(roc_host, items))),
                .key_eq = key_eq,
                .key_of = key_of,
                .item_eq = item_eq,
                .row = row,
            },
        },
        .tag = .Each,
    };
}

fn testNodeEachWithItems(roc_host: *abi.RocHost, items: []const abi.NodeValue) abi.Elem {
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
        &testNodeValueEqCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    defer abi.decrefErasedCallable(key_eq, &roc_host);

    const root = host.internRootScope();

    const initial_keys = [_]abi.NodeValue{ nodeValueI64(1), nodeValueI64(2), nodeValueI64(3) };
    const initial = host.syncEachRowScopes(&roc_host, root, 5, &initial_keys, &initial_keys, key_eq, key_eq);
    defer freeKeyedRowDiff(&host, initial);
    try std.testing.expectEqual(@as(u64, 0), initial.rows_reused);
    try std.testing.expectEqual(@as(u64, 3), initial.rows_created);
    try std.testing.expectEqual(@as(u64, 0), initial.rows_removed);
    try std.testing.expectEqual(@as(u64, 0), initial.row_items_unchanged);
    try std.testing.expectEqual(@as(u64, 0), initial.row_items_updated);

    const state_for_key_2 = host.internNodeIdentity(initial.scope_ids[1], 0);

    const reordered_keys = [_]abi.NodeValue{ nodeValueI64(3), nodeValueI64(1), nodeValueI64(2) };
    const reordered = host.syncEachRowScopes(&roc_host, root, 5, &reordered_keys, &reordered_keys, key_eq, key_eq);
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

    const changed_keys = [_]abi.NodeValue{ nodeValueI64(2), nodeValueI64(4) };
    const changed_items = [_]abi.NodeValue{ nodeValueI64(22), nodeValueI64(4) };
    const changed = host.syncEachRowScopes(&roc_host, root, 5, &changed_keys, &changed_items, key_eq, key_eq);
    defer freeKeyedRowDiff(&host, changed);
    try std.testing.expectEqual(@as(u64, 1), changed.rows_reused);
    try std.testing.expectEqual(@as(u64, 1), changed.rows_created);
    try std.testing.expectEqual(@as(u64, 2), changed.rows_removed);
    try std.testing.expectEqual(@as(u64, 0), changed.row_items_unchanged);
    try std.testing.expectEqual(@as(u64, 1), changed.row_items_updated);
    try std.testing.expectEqual(initial.scope_ids[1], changed.scope_ids[0]);
    try std.testing.expect(changed.scope_ids[1] != initial.scope_ids[0]);
    try std.testing.expect(changed.scope_ids[1] != initial.scope_ids[2]);

    const reappeared_keys = [_]abi.NodeValue{ nodeValueI64(1), nodeValueI64(2), nodeValueI64(4) };
    const reappeared = host.syncEachRowScopes(&roc_host, root, 5, &reappeared_keys, &reappeared_keys, key_eq, key_eq);
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
        testNodeSignalTextAttr(&roc_host, .value, testNodeConstExpr(&roc_host, nodeValueStr(&roc_host, "search"))),
        testNodeStaticBoolAttr(.disabled, true),
        testNodeSignalBoolAttr(&roc_host, .checked, testNodeConstExpr(&roc_host, nodeValueBool(false))),
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
        testNodeTextSignal(&roc_host, testNodeConstExpr(&roc_host, nodeValueStr(&roc_host, "dynamic text"))),
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
    try std.testing.expectEqual(abi.NodeSignalExprTag.ConstValue, stream.signal_text_nodes.items[0].signal.tag);
    try std.testing.expectEqual(@as(usize, 0), stream.signal_text_nodes.items[0].source_node_ids.len);
    try std.testing.expectEqual(@as(u64, 6), stream.signal_text_nodes.items[1].elem_id);
    try std.testing.expectEqual(@as(u64, 4), stream.signal_text_nodes.items[1].parent_elem_id);
    try std.testing.expectEqual(abi.NodeSignalExprTag.Ref, stream.signal_text_nodes.items[1].signal.tag);
    try std.testing.expectEqual(@as(usize, 1), stream.signal_text_nodes.items[1].source_node_ids.len);
    try std.testing.expectEqual(stream.scope_sites.items[0].node_id, stream.signal_text_nodes.items[1].source_node_ids[0]);
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
    try std.testing.expectEqual(abi.NodeSignalExprTag.ConstValue, stream.signal_text_attrs.items[0].signal.tag);
    try std.testing.expectEqual(@as(usize, 0), stream.signal_text_attrs.items[0].source_node_ids.len);
    try std.testing.expectEqual(@as(u64, 4), stream.signal_text_attrs.items[1].elem_id);
    try std.testing.expectEqual(RenderTextField.value, stream.signal_text_attrs.items[1].field);
    try std.testing.expectEqual(abi.NodeSignalExprTag.Ref, stream.signal_text_attrs.items[1].signal.tag);
    try std.testing.expectEqual(@as(usize, 1), stream.signal_text_attrs.items[1].source_node_ids.len);
    try std.testing.expectEqual(stream.scope_sites.items[0].node_id, stream.signal_text_attrs.items[1].source_node_ids[0]);

    try std.testing.expectEqual(@as(usize, 1), stream.static_bool_attrs.items.len);
    try std.testing.expectEqual(RenderBoolField.disabled, stream.static_bool_attrs.items[0].field);
    try std.testing.expect(stream.static_bool_attrs.items[0].value);

    try std.testing.expectEqual(@as(usize, 1), stream.signal_bool_attrs.items.len);
    try std.testing.expectEqual(RenderBoolField.checked, stream.signal_bool_attrs.items[0].field);
    try std.testing.expectEqual(abi.NodeSignalExprTag.ConstValue, stream.signal_bool_attrs.items[0].signal.tag);
    try std.testing.expectEqual(@as(usize, 0), stream.signal_bool_attrs.items[0].source_node_ids.len);

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
    try std.testing.expectEqual(@as(usize, 0), stream.whens.items[0].source_node_ids.len);
    try std.testing.expectEqual(@as(usize, 1), stream.eaches.items.len);
    try std.testing.expectEqual(stream.scope_sites.items[2].node_id, stream.eaches.items[0].node_id);
    try std.testing.expectEqual(@as(usize, 0), stream.eaches.items[0].source_node_ids.len);

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
        testNodeSignalTextAttr(&roc_host, .value, testNodeMapExpr(&roc_host, testNodeConstExpr(&roc_host, nodeValueI64(41)))),
    };
    const state_child_attrs = [_]abi.NodeAttr{
        testNodeEventAttr(&roc_host, .click, state_token, .unit),
    };
    const state_child = testElementWith(&roc_host, "button", &state_child_attrs, &.{});
    const state = testNodeStateWithToken(&roc_host, state_token, state_child);
    const items = [_]abi.NodeValue{nodeValueI64(1)};
    const each = testNodeEachWithItems(&roc_host, &items);
    const root_children = [_]abi.Elem{ state, each };
    const root = testElementWith(&roc_host, "section", &root_attrs, &root_children);
    defer abi.decrefElem(root, &roc_host);

    host.collectElemRootDescriptors(&roc_host, &stream, root);

    try std.testing.expectEqual(@as(u64, 13), host.pending_roc_metrics.closure_retains);
    try std.testing.expectEqual(@as(u64, 0), host.pending_roc_metrics.closure_releases);

    stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);

    try std.testing.expectEqual(@as(u64, 13), host.pending_roc_metrics.closure_retains);
    try std.testing.expectEqual(@as(u64, 13), host.pending_roc_metrics.closure_releases);
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
    const root = testNodeStateWithTokenAndInitial(&roc_host, state_token, nodeValueI64(0), testNodeText(&roc_host, "state"));
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    host.active_stream = stream;

    const state_id = host.active_stream.scope_sites.items[0].node_id;
    host.active_stream.deinit(host.gpa.allocator(), &roc_host, &host.pending_roc_metrics);
    host.active_stream = .{};

    try std.testing.expect(!host.updateStateValue(&roc_host, state_id, nodeValueI64(0)));
    try std.testing.expect(host.updateStateValue(&roc_host, state_id, nodeValueI64(1)));
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
    try std.testing.expectEqual(@as(usize, 1), stream.signal_text_nodes.items[0].source_node_ids.len);
    try std.testing.expectEqual(state_site.node_id, stream.signal_text_nodes.items[0].source_node_ids[0]);
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

    const initial_items = [_]abi.NodeValue{ nodeValueI64(1), nodeValueI64(2), nodeValueI64(3) };
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

    const reordered_items = [_]abi.NodeValue{ nodeValueI64(3), nodeValueI64(1), nodeValueI64(2) };
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

    const changed_items = [_]abi.NodeValue{ nodeValueI64(2), nodeValueI64(4) };
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
