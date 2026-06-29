const std = @import("std");
const scope_tree = @import("scope_tree.zig");
const signal_graph = @import("signal_graph.zig");
const render = @import("render_commands.zig");

pub fn Node(comptime Record: type) type {
    return signal_graph.Node(Record);
}

pub fn RouteTable(comptime Route: type) type {
    return std.ArrayListUnmanaged(std.ArrayListUnmanaged(Route));
}

pub const SignalKind = enum(u64) {
    source = 1,
    map = 2,
    map2 = 3,
};

pub const EventRoute = struct {
    event_id: u64,
    signal_ids: []u64,
};

pub const EventDescriptor = struct {
    event_id: u64,
    payload_kind: render.EventPayloadKind,
    payload_accessor: render.EventPayloadAccessor,
};

pub const Descriptor = struct {
    signal_id: u64,
    kind: SignalKind,
    source_state_ids: []u64,
    source_event_ids: []u64,
    input_signal_ids: []u64,
    rank: u64,
};

pub const StateRoute = struct {
    state_id: u64,
    signal_ids: []u64,
};

pub const DependentsRoute = struct {
    signal_id: u64,
    signal_ids: []u64,
};

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

pub const TextSinkKind = enum {
    text_node,
    text_attr,
    custom_text_attr,
};

pub const TextSink = struct {
    kind: TextSinkKind,
    index: usize,
};

pub const BoolSink = struct {
    index: usize,
};

pub const ChangeSink = struct {
    index: usize,
};

pub const StructuralKind = enum {
    when,
    each,
};

pub const StructuralSink = struct {
    kind: StructuralKind,
    index: usize,
};

pub const DirtyStructuralSignal = struct {
    kind: StructuralKind,
    node_id: u64,
    branch: ?scope_tree.Branch = null,
};

pub fn sourceSignalIdsForEvent(routes: []const EventRoute, event_id: u64) EventLookupError![]const u64 {
    if (event_id == 0) return EventLookupError.EventIdZero;

    const route_index = event_id - 1;
    if (route_index >= routes.len) return EventLookupError.MissingSignalEventRoute;

    const route = routes[@intCast(route_index)];
    if (route.event_id != event_id) return EventLookupError.SignalEventRouteIndexMismatch;
    return route.signal_ids;
}

pub fn eventPayloadKind(descriptors: []const EventDescriptor, event_id: u64) EventLookupError!render.EventPayloadKind {
    if (event_id == 0) return EventLookupError.EventIdZero;

    const event_index = event_id - 1;
    if (event_index >= descriptors.len) return EventLookupError.MissingEventDescriptor;

    const descriptor = descriptors[@intCast(event_index)];
    if (descriptor.event_id != event_id) return EventLookupError.EventDescriptorIndexMismatch;
    return descriptor.payload_kind;
}

pub fn signalIdsForState(routes: []const StateRoute, state_id: u64) SignalLookupError![]const u64 {
    if (state_id >= routes.len) return SignalLookupError.MissingSignalRoute;

    const route = routes[@intCast(state_id)];
    if (route.state_id != state_id) return SignalLookupError.SignalRouteIndexMismatch;
    return route.signal_ids;
}

pub fn dependentSignalIdsForSignal(routes: []const DependentsRoute, signal_id: u64) SignalLookupError![]const u64 {
    if (signal_id >= routes.len) return SignalLookupError.MissingSignalDependentRoute;

    const route = routes[@intCast(signal_id)];
    if (route.signal_id != signal_id) return SignalLookupError.SignalDependentRouteIndexMismatch;
    return route.signal_ids;
}

pub fn signalRank(descriptors: []const Descriptor, signal_id: u64) SignalLookupError!u64 {
    if (signal_id >= descriptors.len) return SignalLookupError.MissingSignalDescriptor;

    const descriptor = descriptors[@intCast(signal_id)];
    if (descriptor.signal_id != signal_id) return SignalLookupError.SignalDescriptorIndexMismatch;
    return descriptor.rank;
}

pub fn appendSignalAndDependents(allocator: std.mem.Allocator, dependent_routes: []const DependentsRoute, signal_ids: *std.ArrayListUnmanaged(u64), signal_id: u64) void {
    if (!containsU64(signal_ids.items, signal_id)) {
        signal_ids.append(allocator, signal_id) catch @panic("out of memory");
    }

    var index: usize = 0;
    while (index < signal_ids.items.len) : (index += 1) {
        const current_signal_id = signal_ids.items[index];
        const dependents = dependentSignalIdsForSignal(dependent_routes, current_signal_id) catch @panic("host signal dependent route table is invalid");
        for (dependents) |dependent_signal_id| {
            if (!containsU64(signal_ids.items, dependent_signal_id)) {
                signal_ids.append(allocator, dependent_signal_id) catch @panic("out of memory");
            }
        }
    }
}

pub fn sortSignalIdsByRank(descriptors: []const Descriptor, signal_ids: []u64) void {
    var index: usize = 1;
    while (index < signal_ids.len) : (index += 1) {
        const value = signal_ids[index];
        const value_rank = signalRank(descriptors, value) catch @panic("host signal rank table is invalid");
        var insert_index = index;
        while (insert_index > 0) {
            const previous = signal_ids[insert_index - 1];
            const previous_rank = signalRank(descriptors, previous) catch @panic("host signal rank table is invalid");
            if (previous_rank < value_rank or (previous_rank == value_rank and previous < value)) break;
            signal_ids[insert_index] = previous;
            insert_index -= 1;
        }
        signal_ids[insert_index] = value;
    }
}

pub fn dirtySignalIdsForEvent(allocator: std.mem.Allocator, event_routes: []const EventRoute, dependent_routes: []const DependentsRoute, descriptors: []const Descriptor, event_id: u64) []u64 {
    var dirty_signal_ids: std.ArrayListUnmanaged(u64) = .empty;
    errdefer dirty_signal_ids.deinit(allocator);

    const source_signal_ids = sourceSignalIdsForEvent(event_routes, event_id) catch @panic("event id has no host source signal route descriptor");
    for (source_signal_ids) |signal_id| {
        appendSignalAndDependents(allocator, dependent_routes, &dirty_signal_ids, signal_id);
    }

    const signal_ids = dirty_signal_ids.toOwnedSlice(allocator) catch @panic("out of memory");
    sortSignalIdsByRank(descriptors, signal_ids);
    return signal_ids;
}

pub fn rank(comptime Record: type, nodes: []const Node(Record), record_id: u64) u64 {
    return signal_graph.rank(Record, nodes, record_id) catch @panic("active signal record id has no graph node");
}

pub fn dependentIds(comptime Record: type, nodes: []const Node(Record), record_id: u64) []const u64 {
    return signal_graph.dependentIds(Record, nodes, record_id) catch @panic("active signal record id has no dependent table");
}

pub fn appendAndDependents(comptime Record: type, allocator: std.mem.Allocator, nodes: []const Node(Record), record_ids: *std.ArrayListUnmanaged(u64), record_id: u64) void {
    signal_graph.appendReachableDependents(Record, allocator, nodes, record_ids, record_id) catch |err| switch (err) {
        error.OutOfMemory => @panic("out of memory"),
        else => @panic("active signal dependent traversal referenced an unknown record"),
    };
}

pub fn sortRecordIdsByRank(comptime Record: type, nodes: []const Node(Record), record_ids: []u64) void {
    signal_graph.sortIdsByRank(Record, nodes, record_ids) catch {
        @panic("active signal rank sort referenced an unknown record");
    };
}

pub fn dirtyRecordIdsForSources(comptime Record: type, allocator: std.mem.Allocator, nodes: []const Node(Record), source_routes: []const std.ArrayListUnmanaged(u64), dirty_source_node_ids: []const u64) []u64 {
    var dirty_record_ids: std.ArrayListUnmanaged(u64) = .empty;
    errdefer dirty_record_ids.deinit(allocator);

    for (dirty_source_node_ids) |source_node_id| {
        const route_index: usize = @intCast(source_node_id);
        if (route_index >= source_routes.len) continue;

        for (source_routes[route_index].items) |record_id| {
            appendAndDependents(Record, allocator, nodes, &dirty_record_ids, record_id);
        }
    }

    const record_ids = dirty_record_ids.toOwnedSlice(allocator) catch @panic("out of memory");
    sortRecordIdsByRank(Record, nodes, record_ids);
    return record_ids;
}

pub fn dirtyRecordIdsForRoots(comptime Record: type, allocator: std.mem.Allocator, nodes: []const Node(Record), root_record_ids: []const u64) []u64 {
    var dirty_record_ids: std.ArrayListUnmanaged(u64) = .empty;
    errdefer dirty_record_ids.deinit(allocator);

    for (root_record_ids) |record_id| {
        if (record_id >= nodes.len) @panic("dirty active signal root referenced an unknown record");
        appendAndDependents(Record, allocator, nodes, &dirty_record_ids, record_id);
    }

    const record_ids = dirty_record_ids.toOwnedSlice(allocator) catch @panic("out of memory");
    sortRecordIdsByRank(Record, nodes, record_ids);
    return record_ids;
}

pub fn recordId(comptime Record: type, nodes: []const Node(Record), record: *const Record) ?u64 {
    const record_id = record.active_graph_id orelse return null;
    if (record_id >= nodes.len) @panic("active signal record dense id exceeded the graph table");
    if (nodes[@intCast(record_id)].record != record) {
        @panic("active signal record dense id pointed at a different record");
    }
    return record_id;
}

pub fn requireRecordId(comptime Record: type, nodes: []const Node(Record), record: *const Record) u64 {
    return recordId(Record, nodes, record) orelse @panic("active signal graph referenced a record that was not registered");
}

pub fn appendNode(comptime Record: type, allocator: std.mem.Allocator, nodes: *std.ArrayListUnmanaged(Node(Record)), record: *Record, node_rank: u64) u64 {
    const record_id: u64 = @intCast(nodes.items.len);
    nodes.append(allocator, .{
        .record = record.retain(),
        .rank = node_rank,
    }) catch @panic("out of memory");
    record.active_graph_id = record_id;
    return record_id;
}

pub fn appendDependentId(comptime Record: type, allocator: std.mem.Allocator, nodes: []Node(Record), input_record_id: u64, dependent_record_id: u64) void {
    signal_graph.appendDependent(Record, allocator, nodes, input_record_id, dependent_record_id) catch |err| switch (err) {
        error.OutOfMemory => @panic("out of memory"),
        error.UnknownNode => @panic("active signal dependent referenced an unknown input record"),
        else => @panic("active signal dependent insertion missed its edge"),
    };
}

pub fn removeDependentId(comptime Record: type, allocator: std.mem.Allocator, nodes: []Node(Record), input_record_id: u64, dependent_record_id: u64) void {
    signal_graph.removeDependent(Record, allocator, nodes, input_record_id, dependent_record_id) catch |err| switch (err) {
        error.OutOfMemory => @panic("out of memory"),
        error.UnknownNode => @panic("active signal dependent removal referenced an unknown input record"),
        else => @panic("active signal dependent removal missed its edge"),
    };
}

pub fn replaceDependentId(comptime Record: type, nodes: []Node(Record), input_record_id: u64, old_dependent_id: u64, new_dependent_id: u64) void {
    signal_graph.replaceDependent(Record, nodes, input_record_id, old_dependent_id, new_dependent_id) catch |err| switch (err) {
        error.UnknownNode => @panic("active signal dependent rewrite referenced an unknown input record"),
        else => @panic("active signal dependent rewrite missed its edge"),
    };
}

pub fn clearSourceRoutes(allocator: std.mem.Allocator, source_routes: *RouteTable(u64)) void {
    clearRouteTable(u64, allocator, source_routes);
}

pub fn clearSinkRoutes(
    allocator: std.mem.Allocator,
    text_routes: *RouteTable(TextSink),
    bool_routes: *RouteTable(BoolSink),
    change_routes: *RouteTable(ChangeSink),
    structural_routes: *RouteTable(StructuralSink),
) void {
    clearRouteTable(TextSink, allocator, text_routes);
    clearRouteTable(BoolSink, allocator, bool_routes);
    clearRouteTable(ChangeSink, allocator, change_routes);
    clearRouteTable(StructuralSink, allocator, structural_routes);
}

pub fn clearRoutes(
    allocator: std.mem.Allocator,
    source_routes: *RouteTable(u64),
    text_routes: *RouteTable(TextSink),
    bool_routes: *RouteTable(BoolSink),
    change_routes: *RouteTable(ChangeSink),
    structural_routes: *RouteTable(StructuralSink),
) void {
    clearSourceRoutes(allocator, source_routes);
    clearSinkRoutes(allocator, text_routes, bool_routes, change_routes, structural_routes);
}

pub fn ensureSourceRoute(allocator: std.mem.Allocator, source_routes: *RouteTable(u64), source_node_count: usize, source_node_id: u64) *std.ArrayListUnmanaged(u64) {
    if (source_node_id >= source_node_count) @panic("active source signal route referenced an unknown source node");
    const route_index: usize = @intCast(source_node_id);
    while (source_routes.items.len <= route_index) {
        source_routes.append(allocator, .empty) catch @panic("out of memory");
    }
    return &source_routes.items[route_index];
}

pub fn appendSourceRoute(allocator: std.mem.Allocator, source_routes: *RouteTable(u64), source_node_count: usize, source_node_id: u64, record_id: u64) void {
    const route = ensureSourceRoute(allocator, source_routes, source_node_count, source_node_id);
    if (!containsU64(route.items, record_id)) {
        route.append(allocator, record_id) catch @panic("out of memory");
    }
}

pub fn removeSourceRoute(source_routes: *RouteTable(u64), source_node_id: u64, record_id: u64) void {
    if (source_node_id >= source_routes.items.len) @panic("active source signal route removal referenced an unknown source node");
    var route = &source_routes.items[@intCast(source_node_id)];
    for (route.items, 0..) |existing_id, index| {
        if (existing_id != record_id) continue;
        _ = route.swapRemove(index);
        return;
    }
    @panic("active source signal route removal missed its record");
}

pub fn replaceSourceRouteId(source_routes: *RouteTable(u64), source_node_id: u64, old_record_id: u64, new_record_id: u64) void {
    if (source_node_id >= source_routes.items.len) @panic("active source signal route rewrite referenced an unknown source node");
    const route = source_routes.items[@intCast(source_node_id)].items;
    for (route) |*existing_id| {
        if (existing_id.* != old_record_id) continue;
        existing_id.* = new_record_id;
        return;
    }
    @panic("active source signal route rewrite missed its record");
}

pub fn ensureTextRoute(allocator: std.mem.Allocator, text_routes: *RouteTable(TextSink), graph_len: usize, record_id: u64) *std.ArrayListUnmanaged(TextSink) {
    return ensureSinkRoute(TextSink, allocator, text_routes, graph_len, record_id, "active text signal route referenced an unknown signal record");
}

pub fn ensureBoolRoute(allocator: std.mem.Allocator, bool_routes: *RouteTable(BoolSink), graph_len: usize, record_id: u64) *std.ArrayListUnmanaged(BoolSink) {
    return ensureSinkRoute(BoolSink, allocator, bool_routes, graph_len, record_id, "active bool signal route referenced an unknown signal record");
}

pub fn ensureChangeRoute(allocator: std.mem.Allocator, change_routes: *RouteTable(ChangeSink), graph_len: usize, record_id: u64) *std.ArrayListUnmanaged(ChangeSink) {
    return ensureSinkRoute(ChangeSink, allocator, change_routes, graph_len, record_id, "active change signal route referenced an unknown signal record");
}

pub fn ensureStructuralRoute(allocator: std.mem.Allocator, structural_routes: *RouteTable(StructuralSink), graph_len: usize, record_id: u64) *std.ArrayListUnmanaged(StructuralSink) {
    return ensureSinkRoute(StructuralSink, allocator, structural_routes, graph_len, record_id, "active structural signal route referenced an unknown signal record");
}

pub fn removeSinkRoutesForRecordId(
    allocator: std.mem.Allocator,
    text_routes: *RouteTable(TextSink),
    bool_routes: *RouteTable(BoolSink),
    change_routes: *RouteTable(ChangeSink),
    structural_routes: *RouteTable(StructuralSink),
    record_index: usize,
    last_index: usize,
) void {
    removeRouteTableRecordId(TextSink, allocator, text_routes, record_index, last_index, "active signal graph removed a record with live text sinks");
    removeRouteTableRecordId(BoolSink, allocator, bool_routes, record_index, last_index, "active signal graph removed a record with live bool sinks");
    removeRouteTableRecordId(ChangeSink, allocator, change_routes, record_index, last_index, "active signal graph removed a record with live change sinks");
    removeRouteTableRecordId(StructuralSink, allocator, structural_routes, record_index, last_index, "active signal graph removed a record with live structural sinks");
}

pub fn appendTextRoute(allocator: std.mem.Allocator, text_routes: *RouteTable(TextSink), graph_len: usize, record_id: u64, route: TextSink) void {
    ensureTextRoute(allocator, text_routes, graph_len, record_id).append(allocator, route) catch @panic("out of memory");
}

pub fn removeTextRoute(text_routes: *RouteTable(TextSink), record_id: u64, kind: TextSinkKind, index: usize) void {
    const route_index: usize = @intCast(record_id);
    if (route_index >= text_routes.items.len) @panic("active text signal route removal referenced an unknown signal record");
    var route = &text_routes.items[route_index];
    for (route.items, 0..) |sink, sink_index| {
        if (sink.kind == kind and sink.index == index) {
            _ = route.swapRemove(sink_index);
            return;
        }
    }
    @panic("active text signal route removal missed its sink");
}

pub fn updateTextRouteIndex(text_routes: *RouteTable(TextSink), record_id: u64, kind: TextSinkKind, old_index: usize, new_index: usize) void {
    if (old_index == new_index) return;
    const route_index: usize = @intCast(record_id);
    if (route_index >= text_routes.items.len) @panic("active text signal route update referenced an unknown signal record");
    for (text_routes.items[route_index].items) |*sink| {
        if (sink.kind == kind and sink.index == old_index) {
            sink.index = new_index;
            return;
        }
    }
    @panic("active text signal route update missed its sink");
}

pub fn appendBoolRoute(allocator: std.mem.Allocator, bool_routes: *RouteTable(BoolSink), graph_len: usize, record_id: u64, route: BoolSink) void {
    ensureBoolRoute(allocator, bool_routes, graph_len, record_id).append(allocator, route) catch @panic("out of memory");
}

pub fn removeBoolRoute(bool_routes: *RouteTable(BoolSink), record_id: u64, index: usize) void {
    const route_index: usize = @intCast(record_id);
    if (route_index >= bool_routes.items.len) @panic("active bool signal route removal referenced an unknown signal record");
    var route = &bool_routes.items[route_index];
    for (route.items, 0..) |sink, sink_index| {
        if (sink.index == index) {
            _ = route.swapRemove(sink_index);
            return;
        }
    }
    @panic("active bool signal route removal missed its sink");
}

pub fn updateBoolRouteIndex(bool_routes: *RouteTable(BoolSink), record_id: u64, old_index: usize, new_index: usize) void {
    if (old_index == new_index) return;
    const route_index: usize = @intCast(record_id);
    if (route_index >= bool_routes.items.len) @panic("active bool signal route update referenced an unknown signal record");
    for (bool_routes.items[route_index].items) |*sink| {
        if (sink.index == old_index) {
            sink.index = new_index;
            return;
        }
    }
    @panic("active bool signal route update missed its sink");
}

pub fn appendChangeRoute(allocator: std.mem.Allocator, change_routes: *RouteTable(ChangeSink), graph_len: usize, record_id: u64, route: ChangeSink) void {
    ensureChangeRoute(allocator, change_routes, graph_len, record_id).append(allocator, route) catch @panic("out of memory");
}

pub fn removeChangeRoute(change_routes: *RouteTable(ChangeSink), record_id: u64, index: usize) void {
    const route_index: usize = @intCast(record_id);
    if (route_index >= change_routes.items.len) @panic("active change signal route removal referenced an unknown signal record");
    var route = &change_routes.items[route_index];
    for (route.items, 0..) |sink, sink_index| {
        if (sink.index == index) {
            _ = route.swapRemove(sink_index);
            return;
        }
    }
    @panic("active change signal route removal missed its sink");
}

pub fn updateChangeRouteIndex(change_routes: *RouteTable(ChangeSink), record_id: u64, old_index: usize, new_index: usize) void {
    if (old_index == new_index) return;
    const route_index: usize = @intCast(record_id);
    if (route_index >= change_routes.items.len) @panic("active change signal route update referenced an unknown signal record");
    for (change_routes.items[route_index].items) |*sink| {
        if (sink.index == old_index) {
            sink.index = new_index;
            return;
        }
    }
    @panic("active change signal route update missed its sink");
}

pub fn appendStructuralRoute(allocator: std.mem.Allocator, structural_routes: *RouteTable(StructuralSink), graph_len: usize, record_id: u64, route: StructuralSink) void {
    ensureStructuralRoute(allocator, structural_routes, graph_len, record_id).append(allocator, route) catch @panic("out of memory");
}

pub fn removeStructuralRoute(structural_routes: *RouteTable(StructuralSink), record_id: u64, kind: StructuralKind, index: usize) void {
    const route_index: usize = @intCast(record_id);
    if (route_index >= structural_routes.items.len) @panic("active structural signal route removal referenced an unknown signal record");
    var route = &structural_routes.items[route_index];
    for (route.items, 0..) |sink, sink_index| {
        if (sink.kind == kind and sink.index == index) {
            _ = route.swapRemove(sink_index);
            return;
        }
    }
    @panic("active structural signal route removal missed its sink");
}

pub fn updateStructuralRouteIndex(structural_routes: *RouteTable(StructuralSink), record_id: u64, kind: StructuralKind, old_index: usize, new_index: usize) void {
    if (old_index == new_index) return;
    const route_index: usize = @intCast(record_id);
    if (route_index >= structural_routes.items.len) @panic("active structural signal route update referenced an unknown signal record");
    for (structural_routes.items[route_index].items) |*sink| {
        if (sink.kind == kind and sink.index == old_index) {
            sink.index = new_index;
            return;
        }
    }
    @panic("active structural signal route update missed its sink");
}

pub fn recordSliceContains(comptime Record: type, records: []const *Record, record: *Record) bool {
    for (records) |existing| {
        if (existing == record) return true;
    }
    return false;
}

pub fn appendInputRecords(comptime Record: type, allocator: std.mem.Allocator, records: *std.ArrayListUnmanaged(*Record), record: *Record) void {
    switch (record.payload) {
        .ref, .const_value, .task_source, .interval_source => {},
        .map => |payload| appendUniqueInputRecord(Record, allocator, records, payload.input),
        .map2 => |payload| {
            appendUniqueInputRecord(Record, allocator, records, payload.left);
            appendUniqueInputRecord(Record, allocator, records, payload.right);
        },
        .combine => |payload| {
            for (payload.children) |child| {
                appendUniqueInputRecord(Record, allocator, records, child);
            }
        },
    }
}

pub fn retainRecord(
    comptime Record: type,
    allocator: std.mem.Allocator,
    nodes: *std.ArrayListUnmanaged(Node(Record)),
    source_routes: *RouteTable(u64),
    source_node_count: usize,
    record: *Record,
    hooks: anytype,
) u64 {
    if (record.active_use_count != 0) {
        record.active_use_count += 1;
        return 0;
    }

    record.active_use_count = 1;
    var node_rank: u64 = 0;
    var records_rebuilt: u64 = 0;

    switch (record.payload) {
        .ref, .const_value, .task_source, .interval_source => {},
        .map => |payload| {
            records_rebuilt += retainRecord(Record, allocator, nodes, source_routes, source_node_count, payload.input, hooks);
            const input_id = requireRecordId(Record, nodes.items, payload.input);
            node_rank = nodes.items[@intCast(input_id)].rank + 1;
        },
        .map2 => |payload| {
            records_rebuilt += retainRecord(Record, allocator, nodes, source_routes, source_node_count, payload.left, hooks);
            if (payload.right != payload.left) {
                records_rebuilt += retainRecord(Record, allocator, nodes, source_routes, source_node_count, payload.right, hooks);
            }
            const left_id = requireRecordId(Record, nodes.items, payload.left);
            const right_id = requireRecordId(Record, nodes.items, payload.right);
            node_rank = @max(
                nodes.items[@intCast(left_id)].rank,
                nodes.items[@intCast(right_id)].rank,
            ) + 1;
        },
        .combine => |payload| {
            for (payload.children, 0..) |child, index| {
                if (recordSliceContains(Record, payload.children[0..index], child)) continue;
                records_rebuilt += retainRecord(Record, allocator, nodes, source_routes, source_node_count, child, hooks);
                const child_id = requireRecordId(Record, nodes.items, child);
                node_rank = @max(node_rank, nodes.items[@intCast(child_id)].rank + 1);
            }
        },
    }

    const record_id = appendNode(Record, allocator, nodes, record, node_rank);
    records_rebuilt += 1;

    switch (record.payload) {
        .ref => |source_node_id| appendSourceRoute(allocator, source_routes, source_node_count, source_node_id, record_id),
        .const_value, .task_source => {},
        .interval_source => |payload| hooks.ensureInterval(payload.token, payload.period_ms),
        .map => |payload| appendDependentId(Record, allocator, nodes.items, requireRecordId(Record, nodes.items, payload.input), record_id),
        .map2 => |payload| {
            appendDependentId(Record, allocator, nodes.items, requireRecordId(Record, nodes.items, payload.left), record_id);
            if (payload.right != payload.left) {
                appendDependentId(Record, allocator, nodes.items, requireRecordId(Record, nodes.items, payload.right), record_id);
            }
        },
        .combine => |payload| {
            for (payload.children, 0..) |child, index| {
                if (recordSliceContains(Record, payload.children[0..index], child)) continue;
                appendDependentId(Record, allocator, nodes.items, requireRecordId(Record, nodes.items, child), record_id);
            }
        },
    }

    return records_rebuilt;
}

pub fn releaseRecord(
    comptime Record: type,
    allocator: std.mem.Allocator,
    nodes: *std.ArrayListUnmanaged(Node(Record)),
    source_routes: *RouteTable(u64),
    text_routes: *RouteTable(TextSink),
    bool_routes: *RouteTable(BoolSink),
    change_routes: *RouteTable(ChangeSink),
    structural_routes: *RouteTable(StructuralSink),
    record: *Record,
    hooks: anytype,
) void {
    if (record.active_use_count == 0) @panic("active signal graph record use count underflow");
    record.active_use_count -= 1;
    if (record.active_use_count != 0) return;

    const record_id = requireRecordId(Record, nodes.items, record);
    var input_records: std.ArrayListUnmanaged(*Record) = .empty;
    defer input_records.deinit(allocator);
    appendInputRecords(Record, allocator, &input_records, record);

    switch (record.payload) {
        .ref => |source_node_id| removeSourceRoute(source_routes, source_node_id, record_id),
        .const_value, .task_source => {},
        .interval_source => |payload| hooks.removeInterval(payload.token),
        .map, .map2, .combine => {},
    }

    for (input_records.items) |input_record| {
        removeDependentId(Record, allocator, nodes.items, requireRecordId(Record, nodes.items, input_record), record_id);
    }

    removeNode(Record, allocator, nodes, source_routes, text_routes, bool_routes, change_routes, structural_routes, record_id, record, hooks);

    for (input_records.items) |input_record| {
        releaseRecord(Record, allocator, nodes, source_routes, text_routes, bool_routes, change_routes, structural_routes, input_record, hooks);
    }
}

pub fn clear(comptime Record: type, allocator: std.mem.Allocator, nodes: *std.ArrayListUnmanaged(Node(Record)), hooks: anytype) void {
    for (nodes.items, 0..) |node, index| {
        allocator.free(node.dependents);
        const active_graph_id = node.record.active_graph_id orelse @panic("active signal graph record was missing its dense id");
        if (active_graph_id != @as(u64, @intCast(index))) @panic("active signal graph record dense id did not match its slot");
        node.record.active_graph_id = null;
        node.record.active_use_count = 0;
        hooks.releaseRecord(node.record);
    }
    nodes.items.len = 0;
}

pub fn retainStreamRecords(
    comptime Record: type,
    allocator: std.mem.Allocator,
    nodes: *std.ArrayListUnmanaged(Node(Record)),
    source_routes: *RouteTable(u64),
    source_node_count: usize,
    stream: anytype,
    hooks: anytype,
) u64 {
    var records_rebuilt: u64 = 0;

    for (stream.signal_text_nodes.items) |*desc| {
        records_rebuilt += retainRecord(Record, allocator, nodes, source_routes, source_node_count, desc.signal.record, hooks);
    }
    for (stream.signal_text_attrs.items) |*desc| {
        records_rebuilt += retainRecord(Record, allocator, nodes, source_routes, source_node_count, desc.signal.record, hooks);
    }
    for (stream.signal_custom_text_attrs.items) |*desc| {
        records_rebuilt += retainRecord(Record, allocator, nodes, source_routes, source_node_count, desc.signal.record, hooks);
    }
    for (stream.signal_bool_attrs.items) |*desc| {
        records_rebuilt += retainRecord(Record, allocator, nodes, source_routes, source_node_count, desc.signal.record, hooks);
    }
    for (stream.on_changes.items) |*desc| {
        records_rebuilt += retainRecord(Record, allocator, nodes, source_routes, source_node_count, desc.signal.record, hooks);
    }
    for (stream.whens.items) |*desc| {
        records_rebuilt += retainRecord(Record, allocator, nodes, source_routes, source_node_count, desc.condition.record, hooks);
    }
    for (stream.eaches.items) |*desc| {
        records_rebuilt += retainRecord(Record, allocator, nodes, source_routes, source_node_count, desc.items.record, hooks);
    }

    return records_rebuilt;
}

pub fn rebuildSinkRoutesFromStream(
    comptime Record: type,
    allocator: std.mem.Allocator,
    nodes: []const Node(Record),
    text_routes: *RouteTable(TextSink),
    bool_routes: *RouteTable(BoolSink),
    change_routes: *RouteTable(ChangeSink),
    structural_routes: *RouteTable(StructuralSink),
    stream: anytype,
) void {
    clearSinkRoutes(allocator, text_routes, bool_routes, change_routes, structural_routes);

    for (stream.signal_text_nodes.items, 0..) |desc, index| {
        const id = requireRecordId(Record, nodes, desc.signal.record);
        appendTextRoute(allocator, text_routes, nodes.len, id, .{
            .kind = .text_node,
            .index = index,
        });
    }
    for (stream.signal_text_attrs.items, 0..) |desc, index| {
        const id = requireRecordId(Record, nodes, desc.signal.record);
        appendTextRoute(allocator, text_routes, nodes.len, id, .{
            .kind = .text_attr,
            .index = index,
        });
    }
    for (stream.signal_custom_text_attrs.items, 0..) |desc, index| {
        const id = requireRecordId(Record, nodes, desc.signal.record);
        appendTextRoute(allocator, text_routes, nodes.len, id, .{
            .kind = .custom_text_attr,
            .index = index,
        });
    }
    for (stream.signal_bool_attrs.items, 0..) |desc, index| {
        const id = requireRecordId(Record, nodes, desc.signal.record);
        appendBoolRoute(allocator, bool_routes, nodes.len, id, .{
            .index = index,
        });
    }
    for (stream.on_changes.items, 0..) |desc, index| {
        const id = requireRecordId(Record, nodes, desc.signal.record);
        appendChangeRoute(allocator, change_routes, nodes.len, id, .{
            .index = index,
        });
    }
    for (stream.whens.items, 0..) |desc, index| {
        const id = requireRecordId(Record, nodes, desc.condition.record);
        appendStructuralRoute(allocator, structural_routes, nodes.len, id, .{
            .kind = .when,
            .index = index,
        });
    }
    for (stream.eaches.items, 0..) |desc, index| {
        const id = requireRecordId(Record, nodes, desc.items.record);
        appendStructuralRoute(allocator, structural_routes, nodes.len, id, .{
            .kind = .each,
            .index = index,
        });
    }
}

fn appendUniqueInputRecord(comptime Record: type, allocator: std.mem.Allocator, records: *std.ArrayListUnmanaged(*Record), record: *Record) void {
    if (!recordSliceContains(Record, records.items, record)) {
        records.append(allocator, record) catch @panic("out of memory");
    }
}

fn updateMovedRecordEdges(comptime Record: type, nodes: []Node(Record), source_routes: *RouteTable(u64), moved_record: *Record, old_record_id: u64, new_record_id: u64) void {
    switch (moved_record.payload) {
        .ref => |source_node_id| replaceSourceRouteId(source_routes, source_node_id, old_record_id, new_record_id),
        .const_value, .task_source, .interval_source => {},
        .map => |payload| replaceDependentId(Record, nodes, requireRecordId(Record, nodes, payload.input), old_record_id, new_record_id),
        .map2 => |payload| {
            replaceDependentId(Record, nodes, requireRecordId(Record, nodes, payload.left), old_record_id, new_record_id);
            if (payload.right != payload.left) {
                replaceDependentId(Record, nodes, requireRecordId(Record, nodes, payload.right), old_record_id, new_record_id);
            }
        },
        .combine => |payload| {
            for (payload.children, 0..) |child, index| {
                if (recordSliceContains(Record, payload.children[0..index], child)) continue;
                replaceDependentId(Record, nodes, requireRecordId(Record, nodes, child), old_record_id, new_record_id);
            }
        },
    }
}

fn removeNode(
    comptime Record: type,
    allocator: std.mem.Allocator,
    nodes: *std.ArrayListUnmanaged(Node(Record)),
    source_routes: *RouteTable(u64),
    text_routes: *RouteTable(TextSink),
    bool_routes: *RouteTable(BoolSink),
    change_routes: *RouteTable(ChangeSink),
    structural_routes: *RouteTable(StructuralSink),
    record_id: u64,
    record: *Record,
    hooks: anytype,
) void {
    const record_index: usize = @intCast(record_id);
    if (record_index >= nodes.items.len) @panic("active signal graph removal referenced an unknown record");
    if (nodes.items[record_index].record != record) @panic("active signal graph removal referenced the wrong record");
    if (nodes.items[record_index].dependents.len != 0) @panic("active signal graph removed a record with live dependents");

    allocator.free(nodes.items[record_index].dependents);
    const last_index = nodes.items.len - 1;
    removeSinkRoutesForRecordId(allocator, text_routes, bool_routes, change_routes, structural_routes, record_index, last_index);
    _ = nodes.swapRemove(record_index);
    record.active_graph_id = null;
    hooks.releaseRecord(record);

    if (record_index != last_index) {
        const moved_id: u64 = @intCast(record_index);
        const old_moved_id: u64 = @intCast(last_index);
        const moved_record = nodes.items[record_index].record;
        moved_record.active_graph_id = moved_id;
        updateMovedRecordEdges(Record, nodes.items, source_routes, moved_record, old_moved_id, moved_id);
    }
}

fn ensureSinkRoute(comptime Route: type, allocator: std.mem.Allocator, routes: *RouteTable(Route), graph_len: usize, record_id: u64, comptime unknown_record_message: []const u8) *std.ArrayListUnmanaged(Route) {
    if (record_id >= graph_len) @panic(unknown_record_message);
    const route_index: usize = @intCast(record_id);
    while (routes.items.len <= route_index) {
        routes.append(allocator, .empty) catch @panic("out of memory");
    }
    return &routes.items[route_index];
}

fn clearRouteTable(comptime Route: type, allocator: std.mem.Allocator, routes: *RouteTable(Route)) void {
    for (routes.items) |*route| {
        route.deinit(allocator);
    }
    routes.items.len = 0;
}

fn removeRouteTableRecordId(
    comptime Route: type,
    allocator: std.mem.Allocator,
    routes: *RouteTable(Route),
    record_index: usize,
    last_index: usize,
    comptime live_route_message: []const u8,
) void {
    if (routes.items.len > last_index + 1) @panic("active sink route table exceeded active signal graph length");
    if (record_index >= routes.items.len) return;

    if (routes.items[record_index].items.len != 0) @panic(live_route_message);
    routes.items[record_index].deinit(allocator);

    if (record_index != last_index and last_index < routes.items.len) {
        routes.items[record_index] = routes.items[last_index];
        routes.items[last_index] = .empty;
    } else {
        routes.items[record_index] = .empty;
    }

    if (routes.items.len == last_index + 1) {
        routes.items.len = last_index;
    }
}

fn containsU64(items: []const u64, target: u64) bool {
    for (items) |item| {
        if (item == target) return true;
    }
    return false;
}

const TestRecord = struct {
    id: u64,
};

const LifecycleTestRecord = struct {
    id: u64,
    ref_count: usize = 1,
    payload: Payload,
    active_graph_id: ?u64 = null,
    active_use_count: usize = 0,

    const MapPayload = struct {
        input: *LifecycleTestRecord,
    };

    const Map2Payload = struct {
        left: *LifecycleTestRecord,
        right: *LifecycleTestRecord,
    };

    const CombinePayload = struct {
        children: []*LifecycleTestRecord,
    };

    const IntervalPayload = struct {
        token: u64,
        period_ms: u64,
    };

    const Payload = union(enum) {
        ref: u64,
        const_value,
        map: MapPayload,
        map2: Map2Payload,
        combine: CombinePayload,
        task_source,
        interval_source: IntervalPayload,
    };

    pub fn retain(self: *LifecycleTestRecord) *LifecycleTestRecord {
        self.ref_count += 1;
        return self;
    }
};

const LifecycleTestHooks = struct {
    interval_ensures: u64 = 0,
    interval_removes: u64 = 0,
    record_releases: u64 = 0,

    pub fn ensureInterval(self: *@This(), token: u64, period_ms: u64) void {
        if (token == 0) @panic("test interval token must be explicit");
        if (period_ms == 0) @panic("test interval period must be explicit");
        self.interval_ensures += 1;
    }

    pub fn removeInterval(self: *@This(), token: u64) void {
        if (token == 0) @panic("test interval token must be explicit");
        self.interval_removes += 1;
    }

    pub fn releaseRecord(self: *@This(), record: *LifecycleTestRecord) void {
        if (record.ref_count == 0) @panic("test record release underflow");
        record.ref_count -= 1;
        self.record_releases += 1;
    }
};

const LifecycleSignalBinding = struct {
    record: *LifecycleTestRecord,
};

const LifecycleSignalDesc = struct {
    signal: LifecycleSignalBinding,
};

const LifecycleWhenDesc = struct {
    condition: LifecycleSignalBinding,
};

const LifecycleEachDesc = struct {
    items: LifecycleSignalBinding,
};

const LifecycleStream = struct {
    signal_text_nodes: std.ArrayListUnmanaged(LifecycleSignalDesc) = .empty,
    signal_text_attrs: std.ArrayListUnmanaged(LifecycleSignalDesc) = .empty,
    signal_custom_text_attrs: std.ArrayListUnmanaged(LifecycleSignalDesc) = .empty,
    signal_bool_attrs: std.ArrayListUnmanaged(LifecycleSignalDesc) = .empty,
    on_changes: std.ArrayListUnmanaged(LifecycleSignalDesc) = .empty,
    whens: std.ArrayListUnmanaged(LifecycleWhenDesc) = .empty,
    eaches: std.ArrayListUnmanaged(LifecycleEachDesc) = .empty,

    fn deinit(self: *LifecycleStream, allocator: std.mem.Allocator) void {
        self.signal_text_nodes.deinit(allocator);
        self.signal_text_attrs.deinit(allocator);
        self.signal_custom_text_attrs.deinit(allocator);
        self.signal_bool_attrs.deinit(allocator);
        self.on_changes.deinit(allocator);
        self.whens.deinit(allocator);
        self.eaches.deinit(allocator);
    }
};

test "active graph dirty roots collect reachable dependents once sorted by rank" {
    var records = [_]TestRecord{
        .{ .id = 0 },
        .{ .id = 1 },
        .{ .id = 2 },
        .{ .id = 3 },
    };
    var nodes = [_]Node(TestRecord){
        .{ .record = &records[0], .rank = 0 },
        .{ .record = &records[1], .rank = 3 },
        .{ .record = &records[2], .rank = 1 },
        .{ .record = &records[3], .rank = 2 },
    };
    defer {
        for (&nodes) |*node| {
            std.testing.allocator.free(node.dependents);
        }
    }

    try signal_graph.appendDependent(TestRecord, std.testing.allocator, &nodes, 0, 1);
    try signal_graph.appendDependent(TestRecord, std.testing.allocator, &nodes, 0, 2);
    try signal_graph.appendDependent(TestRecord, std.testing.allocator, &nodes, 2, 3);
    try signal_graph.appendDependent(TestRecord, std.testing.allocator, &nodes, 3, 1);

    const dirty_ids = dirtyRecordIdsForRoots(TestRecord, std.testing.allocator, &nodes, &.{ 0, 2 });
    defer std.testing.allocator.free(dirty_ids);

    try std.testing.expectEqualSlices(u64, &.{ 0, 2, 3, 1 }, dirty_ids);
}

test "active graph source routes collect dirty dependents sorted by rank" {
    var records = [_]TestRecord{
        .{ .id = 0 },
        .{ .id = 1 },
        .{ .id = 2 },
    };
    var nodes = [_]Node(TestRecord){
        .{ .record = &records[0], .rank = 2 },
        .{ .record = &records[1], .rank = 0 },
        .{ .record = &records[2], .rank = 1 },
    };
    defer {
        for (&nodes) |*node| {
            std.testing.allocator.free(node.dependents);
        }
    }
    try signal_graph.appendDependent(TestRecord, std.testing.allocator, &nodes, 1, 2);
    try signal_graph.appendDependent(TestRecord, std.testing.allocator, &nodes, 2, 0);

    var source_routes: RouteTable(u64) = .empty;
    defer source_routes.deinit(std.testing.allocator);
    defer clearSourceRoutes(std.testing.allocator, &source_routes);
    appendSourceRoute(std.testing.allocator, &source_routes, 4, 3, 1);
    appendSourceRoute(std.testing.allocator, &source_routes, 4, 3, 1);

    const dirty_ids = dirtyRecordIdsForSources(TestRecord, std.testing.allocator, &nodes, source_routes.items, &.{3});
    defer std.testing.allocator.free(dirty_ids);

    try std.testing.expectEqualSlices(u64, &.{ 1, 2, 0 }, dirty_ids);
}

test "legacy signal event routes expand dependents once and sort by rank" {
    const event_sources = [_]u64{1};
    const signal_one_dependents = [_]u64{ 2, 3 };
    const signal_two_dependents = [_]u64{3};

    const event_routes = [_]EventRoute{
        .{ .event_id = 1, .signal_ids = @constCast(event_sources[0..]) },
    };
    const dependent_routes = [_]DependentsRoute{
        .{ .signal_id = 0, .signal_ids = @constCast(&.{}) },
        .{ .signal_id = 1, .signal_ids = @constCast(signal_one_dependents[0..]) },
        .{ .signal_id = 2, .signal_ids = @constCast(signal_two_dependents[0..]) },
        .{ .signal_id = 3, .signal_ids = @constCast(&.{}) },
    };
    const descriptors = [_]Descriptor{
        .{ .signal_id = 0, .kind = .source, .source_state_ids = @constCast(&.{}), .source_event_ids = @constCast(&.{}), .input_signal_ids = @constCast(&.{}), .rank = 9 },
        .{ .signal_id = 1, .kind = .source, .source_state_ids = @constCast(&.{}), .source_event_ids = @constCast(&.{}), .input_signal_ids = @constCast(&.{}), .rank = 2 },
        .{ .signal_id = 2, .kind = .map, .source_state_ids = @constCast(&.{}), .source_event_ids = @constCast(&.{}), .input_signal_ids = @constCast(&.{}), .rank = 1 },
        .{ .signal_id = 3, .kind = .map2, .source_state_ids = @constCast(&.{}), .source_event_ids = @constCast(&.{}), .input_signal_ids = @constCast(&.{}), .rank = 3 },
    };

    const dirty_ids = dirtySignalIdsForEvent(std.testing.allocator, &event_routes, &dependent_routes, &descriptors, 1);
    defer std.testing.allocator.free(dirty_ids);

    try std.testing.expectEqualSlices(u64, &.{ 2, 1, 3 }, dirty_ids);
    try std.testing.expectError(EventLookupError.EventIdZero, sourceSignalIdsForEvent(&event_routes, 0));
}

test "active source routes replace and remove ids" {
    var source_routes: RouteTable(u64) = .empty;
    defer source_routes.deinit(std.testing.allocator);
    defer clearSourceRoutes(std.testing.allocator, &source_routes);

    appendSourceRoute(std.testing.allocator, &source_routes, 4, 2, 7);
    appendSourceRoute(std.testing.allocator, &source_routes, 4, 2, 7);
    try std.testing.expectEqualSlices(u64, &.{7}, source_routes.items[2].items);

    replaceSourceRouteId(&source_routes, 2, 7, 3);
    try std.testing.expectEqualSlices(u64, &.{3}, source_routes.items[2].items);

    removeSourceRoute(&source_routes, 2, 3);
    try std.testing.expectEqual(@as(usize, 0), source_routes.items[2].items.len);
}

test "active sink routes use route-specific keys" {
    var text_routes: RouteTable(TextSink) = .empty;
    var bool_routes: RouteTable(BoolSink) = .empty;
    var change_routes: RouteTable(ChangeSink) = .empty;
    var structural_routes: RouteTable(StructuralSink) = .empty;
    defer text_routes.deinit(std.testing.allocator);
    defer bool_routes.deinit(std.testing.allocator);
    defer change_routes.deinit(std.testing.allocator);
    defer structural_routes.deinit(std.testing.allocator);
    defer clearSinkRoutes(std.testing.allocator, &text_routes, &bool_routes, &change_routes, &structural_routes);

    appendTextRoute(std.testing.allocator, &text_routes, 2, 1, .{ .kind = .text_node, .index = 3 });
    appendTextRoute(std.testing.allocator, &text_routes, 2, 1, .{ .kind = .text_attr, .index = 3 });
    updateTextRouteIndex(&text_routes, 1, .text_attr, 3, 8);
    removeTextRoute(&text_routes, 1, .text_node, 3);
    try std.testing.expectEqualSlices(TextSink, &.{.{ .kind = .text_attr, .index = 8 }}, text_routes.items[1].items);

    appendBoolRoute(std.testing.allocator, &bool_routes, 2, 1, .{ .index = 4 });
    updateBoolRouteIndex(&bool_routes, 1, 4, 9);
    removeBoolRoute(&bool_routes, 1, 9);
    try std.testing.expectEqual(@as(usize, 0), bool_routes.items[1].items.len);

    appendChangeRoute(std.testing.allocator, &change_routes, 2, 1, .{ .index = 5 });
    updateChangeRouteIndex(&change_routes, 1, 5, 10);
    removeChangeRoute(&change_routes, 1, 10);
    try std.testing.expectEqual(@as(usize, 0), change_routes.items[1].items.len);

    appendStructuralRoute(std.testing.allocator, &structural_routes, 2, 1, .{ .kind = .when, .index = 6 });
    appendStructuralRoute(std.testing.allocator, &structural_routes, 2, 1, .{ .kind = .each, .index = 6 });
    updateStructuralRouteIndex(&structural_routes, 1, .each, 6, 11);
    removeStructuralRoute(&structural_routes, 1, .when, 6);
    try std.testing.expectEqualSlices(StructuralSink, &.{.{ .kind = .each, .index = 11 }}, structural_routes.items[1].items);
}

test "active sink route record removal moves last route entries" {
    var text_routes: RouteTable(TextSink) = .empty;
    var bool_routes: RouteTable(BoolSink) = .empty;
    var change_routes: RouteTable(ChangeSink) = .empty;
    var structural_routes: RouteTable(StructuralSink) = .empty;
    defer text_routes.deinit(std.testing.allocator);
    defer bool_routes.deinit(std.testing.allocator);
    defer change_routes.deinit(std.testing.allocator);
    defer structural_routes.deinit(std.testing.allocator);
    defer clearSinkRoutes(std.testing.allocator, &text_routes, &bool_routes, &change_routes, &structural_routes);

    _ = ensureTextRoute(std.testing.allocator, &text_routes, 3, 0);
    appendTextRoute(std.testing.allocator, &text_routes, 3, 1, .{ .kind = .text_attr, .index = 4 });
    appendTextRoute(std.testing.allocator, &text_routes, 3, 2, .{ .kind = .text_node, .index = 9 });

    removeSinkRoutesForRecordId(std.testing.allocator, &text_routes, &bool_routes, &change_routes, &structural_routes, 0, 2);

    try std.testing.expectEqual(@as(usize, 2), text_routes.items.len);
    try std.testing.expectEqualSlices(TextSink, &.{.{ .kind = .text_node, .index = 9 }}, text_routes.items[0].items);
    try std.testing.expectEqualSlices(TextSink, &.{.{ .kind = .text_attr, .index = 4 }}, text_routes.items[1].items);
}

test "active graph retain and release update moved record ids and routes" {
    var source_a = LifecycleTestRecord{ .id = 0, .payload = .{ .ref = 1 } };
    var source_b = LifecycleTestRecord{ .id = 1, .payload = .{ .ref = 2 } };
    var mapped = LifecycleTestRecord{ .id = 2, .payload = .{ .map = .{ .input = &source_b } } };

    var nodes: std.ArrayListUnmanaged(Node(LifecycleTestRecord)) = .empty;
    defer nodes.deinit(std.testing.allocator);

    var source_routes: RouteTable(u64) = .empty;
    var text_routes: RouteTable(TextSink) = .empty;
    var bool_routes: RouteTable(BoolSink) = .empty;
    var change_routes: RouteTable(ChangeSink) = .empty;
    var structural_routes: RouteTable(StructuralSink) = .empty;
    defer source_routes.deinit(std.testing.allocator);
    defer text_routes.deinit(std.testing.allocator);
    defer bool_routes.deinit(std.testing.allocator);
    defer change_routes.deinit(std.testing.allocator);
    defer structural_routes.deinit(std.testing.allocator);
    defer clearRoutes(std.testing.allocator, &source_routes, &text_routes, &bool_routes, &change_routes, &structural_routes);

    var hooks = LifecycleTestHooks{};
    try std.testing.expectEqual(@as(u64, 1), retainRecord(LifecycleTestRecord, std.testing.allocator, &nodes, &source_routes, 4, &source_a, &hooks));
    try std.testing.expectEqual(@as(u64, 2), retainRecord(LifecycleTestRecord, std.testing.allocator, &nodes, &source_routes, 4, &mapped, &hooks));
    try std.testing.expectEqual(@as(usize, 3), nodes.items.len);
    try std.testing.expectEqual(@as(?u64, 0), source_a.active_graph_id);
    try std.testing.expectEqual(@as(?u64, 1), source_b.active_graph_id);
    try std.testing.expectEqual(@as(?u64, 2), mapped.active_graph_id);
    try std.testing.expectEqualSlices(u64, &.{2}, nodes.items[1].dependents);
    try std.testing.expectEqualSlices(u64, &.{0}, source_routes.items[1].items);
    try std.testing.expectEqualSlices(u64, &.{1}, source_routes.items[2].items);

    releaseRecord(
        LifecycleTestRecord,
        std.testing.allocator,
        &nodes,
        &source_routes,
        &text_routes,
        &bool_routes,
        &change_routes,
        &structural_routes,
        &source_a,
        &hooks,
    );
    try std.testing.expectEqual(@as(usize, 2), nodes.items.len);
    try std.testing.expectEqual(@as(?u64, null), source_a.active_graph_id);
    try std.testing.expectEqual(@as(?u64, 1), source_b.active_graph_id);
    try std.testing.expectEqual(@as(?u64, 0), mapped.active_graph_id);
    try std.testing.expectEqual(&mapped, nodes.items[0].record);
    try std.testing.expectEqual(&source_b, nodes.items[1].record);
    try std.testing.expectEqualSlices(u64, &.{0}, nodes.items[1].dependents);
    try std.testing.expectEqualSlices(u64, &.{}, source_routes.items[1].items);
    try std.testing.expectEqualSlices(u64, &.{1}, source_routes.items[2].items);

    releaseRecord(
        LifecycleTestRecord,
        std.testing.allocator,
        &nodes,
        &source_routes,
        &text_routes,
        &bool_routes,
        &change_routes,
        &structural_routes,
        &mapped,
        &hooks,
    );
    try std.testing.expectEqual(@as(usize, 0), nodes.items.len);
    try std.testing.expectEqual(@as(?u64, null), source_b.active_graph_id);
    try std.testing.expectEqual(@as(?u64, null), mapped.active_graph_id);
    try std.testing.expectEqual(@as(usize, 1), source_a.ref_count);
    try std.testing.expectEqual(@as(usize, 1), source_b.ref_count);
    try std.testing.expectEqual(@as(usize, 1), mapped.ref_count);
    try std.testing.expectEqual(@as(u64, 3), hooks.record_releases);
}

test "active graph interval records use explicit lifecycle hooks" {
    var interval = LifecycleTestRecord{ .id = 0, .payload = .{ .interval_source = .{ .token = 7, .period_ms = 250 } } };

    var nodes: std.ArrayListUnmanaged(Node(LifecycleTestRecord)) = .empty;
    defer nodes.deinit(std.testing.allocator);

    var source_routes: RouteTable(u64) = .empty;
    var text_routes: RouteTable(TextSink) = .empty;
    var bool_routes: RouteTable(BoolSink) = .empty;
    var change_routes: RouteTable(ChangeSink) = .empty;
    var structural_routes: RouteTable(StructuralSink) = .empty;
    defer source_routes.deinit(std.testing.allocator);
    defer text_routes.deinit(std.testing.allocator);
    defer bool_routes.deinit(std.testing.allocator);
    defer change_routes.deinit(std.testing.allocator);
    defer structural_routes.deinit(std.testing.allocator);
    defer clearRoutes(std.testing.allocator, &source_routes, &text_routes, &bool_routes, &change_routes, &structural_routes);

    var hooks = LifecycleTestHooks{};
    try std.testing.expectEqual(@as(u64, 1), retainRecord(LifecycleTestRecord, std.testing.allocator, &nodes, &source_routes, 1, &interval, &hooks));
    try std.testing.expectEqual(@as(u64, 1), hooks.interval_ensures);
    try std.testing.expectEqual(@as(usize, 1), nodes.items.len);

    releaseRecord(
        LifecycleTestRecord,
        std.testing.allocator,
        &nodes,
        &source_routes,
        &text_routes,
        &bool_routes,
        &change_routes,
        &structural_routes,
        &interval,
        &hooks,
    );
    try std.testing.expectEqual(@as(usize, 0), nodes.items.len);
    try std.testing.expectEqual(@as(u64, 1), hooks.interval_removes);
    try std.testing.expectEqual(@as(u64, 1), hooks.record_releases);
}

test "active graph stream rebuild retains records and rebuilds sink routes" {
    var source = LifecycleTestRecord{ .id = 0, .payload = .{ .ref = 1 } };
    var mapped = LifecycleTestRecord{ .id = 1, .payload = .{ .map = .{ .input = &source } } };

    var stream: LifecycleStream = .{};
    defer stream.deinit(std.testing.allocator);
    stream.signal_text_nodes.append(std.testing.allocator, .{ .signal = .{ .record = &mapped } }) catch @panic("out of memory");
    stream.signal_text_attrs.append(std.testing.allocator, .{ .signal = .{ .record = &mapped } }) catch @panic("out of memory");
    stream.signal_custom_text_attrs.append(std.testing.allocator, .{ .signal = .{ .record = &source } }) catch @panic("out of memory");
    stream.signal_bool_attrs.append(std.testing.allocator, .{ .signal = .{ .record = &source } }) catch @panic("out of memory");
    stream.on_changes.append(std.testing.allocator, .{ .signal = .{ .record = &mapped } }) catch @panic("out of memory");
    stream.whens.append(std.testing.allocator, .{ .condition = .{ .record = &source } }) catch @panic("out of memory");
    stream.eaches.append(std.testing.allocator, .{ .items = .{ .record = &mapped } }) catch @panic("out of memory");

    var nodes: std.ArrayListUnmanaged(Node(LifecycleTestRecord)) = .empty;
    defer nodes.deinit(std.testing.allocator);

    var source_routes: RouteTable(u64) = .empty;
    var text_routes: RouteTable(TextSink) = .empty;
    var bool_routes: RouteTable(BoolSink) = .empty;
    var change_routes: RouteTable(ChangeSink) = .empty;
    var structural_routes: RouteTable(StructuralSink) = .empty;
    defer source_routes.deinit(std.testing.allocator);
    defer text_routes.deinit(std.testing.allocator);
    defer bool_routes.deinit(std.testing.allocator);
    defer change_routes.deinit(std.testing.allocator);
    defer structural_routes.deinit(std.testing.allocator);
    defer clearRoutes(std.testing.allocator, &source_routes, &text_routes, &bool_routes, &change_routes, &structural_routes);

    var hooks = LifecycleTestHooks{};
    const records_rebuilt = retainStreamRecords(LifecycleTestRecord, std.testing.allocator, &nodes, &source_routes, 2, &stream, &hooks);
    try std.testing.expectEqual(@as(u64, 2), records_rebuilt);
    try std.testing.expectEqual(@as(usize, 2), nodes.items.len);
    try std.testing.expectEqual(@as(?u64, 0), source.active_graph_id);
    try std.testing.expectEqual(@as(?u64, 1), mapped.active_graph_id);

    rebuildSinkRoutesFromStream(
        LifecycleTestRecord,
        std.testing.allocator,
        nodes.items,
        &text_routes,
        &bool_routes,
        &change_routes,
        &structural_routes,
        &stream,
    );

    try std.testing.expectEqualSlices(TextSink, &.{.{ .kind = .custom_text_attr, .index = 0 }}, text_routes.items[0].items);
    try std.testing.expectEqualSlices(TextSink, &.{ .{ .kind = .text_node, .index = 0 }, .{ .kind = .text_attr, .index = 0 } }, text_routes.items[1].items);
    try std.testing.expectEqualSlices(BoolSink, &.{.{ .index = 0 }}, bool_routes.items[0].items);
    try std.testing.expectEqualSlices(ChangeSink, &.{.{ .index = 0 }}, change_routes.items[1].items);
    try std.testing.expectEqualSlices(StructuralSink, &.{.{ .kind = .when, .index = 0 }}, structural_routes.items[0].items);
    try std.testing.expectEqualSlices(StructuralSink, &.{.{ .kind = .each, .index = 0 }}, structural_routes.items[1].items);

    clear(LifecycleTestRecord, std.testing.allocator, &nodes, &hooks);
    try std.testing.expectEqual(@as(?u64, null), source.active_graph_id);
    try std.testing.expectEqual(@as(?u64, null), mapped.active_graph_id);
}
