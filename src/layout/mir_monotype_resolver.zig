//! Resolves MIR monotypes into canonical ordinary-data layouts through the shared layout store.

const std = @import("std");
const layout = @import("layout.zig");
const graph_mod = @import("graph.zig");
const mir = @import("mir");

const Monotype = mir.Monotype;
const Store = @import("store.zig").Store;
const LayoutGraph = graph_mod.Graph;
const GraphField = graph_mod.Field;
const GraphNode = graph_mod.Node;
const GraphNodeId = graph_mod.NodeId;
const GraphRef = graph_mod.Ref;
const Allocator = std.mem.Allocator;

/// Resolves MIR monotypes into canonical layout ids through the shared graph interner.
pub const Resolver = struct {
    allocator: Allocator,
    monotype_store: *const Monotype.Store,
    layout_store: *Store,
    canonical_cache: std.AutoHashMap(u32, layout.Idx),
    override_cache: std.AutoHashMap(u32, layout.Idx),

    /// Create a resolver for one MIR monotype store.
    pub fn init(
        allocator: Allocator,
        monotype_store: *const Monotype.Store,
        layout_store: *Store,
    ) Resolver {
        return .{
            .allocator = allocator,
            .monotype_store = monotype_store,
            .layout_store = layout_store,
            .canonical_cache = std.AutoHashMap(u32, layout.Idx).init(allocator),
            .override_cache = std.AutoHashMap(u32, layout.Idx).init(allocator),
        };
    }

    /// Release resolver caches.
    pub fn deinit(self: *Resolver) void {
        self.canonical_cache.deinit();
        self.override_cache.deinit();
    }

    /// Drop cached override results while preserving canonical memoization.
    pub fn clearOverrideCache(self: *Resolver) void {
        self.override_cache.clearRetainingCapacity();
    }

    /// Resolve a monotype, optionally substituting specific monotypes with known layout ids.
    pub fn resolve(
        self: *Resolver,
        mono_idx: Monotype.Idx,
        overrides: ?*const std.AutoHashMap(u32, layout.Idx),
    ) Allocator.Error!layout.Idx {
        std.debug.assert(!mono_idx.isNone());
        const key: u32 = @bitCast(mono_idx);

        if (overrides) |override_map| {
            if (override_map.get(key)) |layout_idx| return layout_idx;
            if (override_map.count() != 0) {
                if (self.override_cache.get(key)) |cached| return cached;
                const resolved = try self.buildAndIntern(mono_idx, override_map);
                try self.override_cache.put(key, resolved);
                return resolved;
            }
        }

        if (self.canonical_cache.get(key)) |cached| return cached;
        const resolved = try self.buildAndIntern(mono_idx, null);
        try self.canonical_cache.put(key, resolved);
        return resolved;
    }

    fn buildAndIntern(
        self: *Resolver,
        mono_idx: Monotype.Idx,
        overrides: ?*const std.AutoHashMap(u32, layout.Idx),
    ) Allocator.Error!layout.Idx {
        var graph: LayoutGraph = .{};
        defer graph.deinit(self.allocator);

        var refs_by_mono = std.AutoHashMap(u32, GraphRef).init(self.allocator);
        defer refs_by_mono.deinit();

        var root = try self.buildRefForMonotype(mono_idx, overrides, &graph, &refs_by_mono);
        if (root == .local) {
            if (try findEquivalentRootNode(self.allocator, &graph, root.local)) |equivalent_root| {
                root = .{ .local = equivalent_root };
            }
        }

        return self.layout_store.internGraph(&graph, root);
    }

    fn buildRefForMonotype(
        self: *Resolver,
        mono_idx: Monotype.Idx,
        overrides: ?*const std.AutoHashMap(u32, layout.Idx),
        graph: *LayoutGraph,
        refs_by_mono: *std.AutoHashMap(u32, GraphRef),
    ) Allocator.Error!GraphRef {
        const mono_key: u32 = @bitCast(mono_idx);
        if (overrides) |override_map| {
            if (override_map.get(mono_key)) |layout_idx| {
                return .{ .canonical = layout_idx };
            }
        }
        if (refs_by_mono.get(mono_key)) |cached| return cached;

        const resolved_id = self.monotype_store.resolve(mono_idx);
        const resolved_key: u32 = @bitCast(resolved_id);
        if (resolved_key != mono_key) {
            if (refs_by_mono.get(resolved_key)) |cached| {
                try refs_by_mono.put(mono_key, cached);
                return cached;
            }
        }

        const resolved_ref: GraphRef = switch (resolved_id.kind) {
            .builtin => blk: {
                if (resolved_id.builtinPrim()) |p| {
                    break :blk GraphRef{ .canonical = switch (p) {
                        .str => layout.Idx.str,
                        .u8 => layout.Idx.u8,
                        .i8 => layout.Idx.i8,
                        .u16 => layout.Idx.u16,
                        .i16 => layout.Idx.i16,
                        .u32 => layout.Idx.u32,
                        .i32 => layout.Idx.i32,
                        .u64 => layout.Idx.u64,
                        .i64 => layout.Idx.i64,
                        .u128 => layout.Idx.u128,
                        .i128 => layout.Idx.i128,
                        .f32 => layout.Idx.f32,
                        .f64 => layout.Idx.f64,
                        .dec => layout.Idx.dec,
                    } };
                }
                // unit
                break :blk GraphRef{ .canonical = .zst };
            },
            .func => blk: {
                const empty_captures = try self.layout_store.getEmptyRecordLayout();
                break :blk GraphRef{ .canonical = try self.layout_store.insertLayout(layout.Layout.closure(empty_captures)) };
            },
            .box => blk: {
                const local_ref = try self.reserveLocalRef(mono_key, resolved_key, graph, refs_by_mono);
                const child_ref = try self.buildRefForMonotype(self.monotype_store.boxInner(resolved_id), overrides, graph, refs_by_mono);
                graph.setNode(local_ref.local, .{ .box = child_ref });
                break :blk local_ref;
            },
            .list => blk: {
                const local_ref = try self.reserveLocalRef(mono_key, resolved_key, graph, refs_by_mono);
                const child_ref = try self.buildRefForMonotype(self.monotype_store.listElem(resolved_id), overrides, graph, refs_by_mono);
                graph.setNode(local_ref.local, .{ .list = child_ref });
                break :blk local_ref;
            },
            .record => blk: {
                const fields = self.monotype_store.recordFields(resolved_id);
                if (fields.len == 0) break :blk .{ .canonical = .zst };

                const local_ref = try self.reserveLocalRef(mono_key, resolved_key, graph, refs_by_mono);
                try self.fillStructNodeFromFields(local_ref.local, fields, overrides, graph, refs_by_mono);
                break :blk local_ref;
            },
            .tuple => blk: {
                const elems = self.monotype_store.tupleElems(resolved_id);
                if (elems.len == 0) break :blk .{ .canonical = .zst };

                const local_ref = try self.reserveLocalRef(mono_key, resolved_key, graph, refs_by_mono);
                try self.fillStructNodeFromElems(local_ref.local, elems, overrides, graph, refs_by_mono);
                break :blk local_ref;
            },
            .tag_union => blk: {
                const tags = self.monotype_store.tagUnionTags(resolved_id);
                if (tags.len == 0) break :blk .{ .canonical = .zst };

                const local_ref = try self.reserveLocalRef(mono_key, resolved_key, graph, refs_by_mono);
                try self.fillTagUnionNode(local_ref.local, tags, overrides, graph, refs_by_mono);
                break :blk local_ref;
            },
            .rec => unreachable, // already resolved above
        };

        try self.cacheResolvedRef(mono_key, resolved_key, resolved_ref, refs_by_mono);
        return resolved_ref;
    }

    fn cacheResolvedRef(
        self: *Resolver,
        mono_key: u32,
        resolved_key: u32,
        resolved_ref: GraphRef,
        refs_by_mono: *std.AutoHashMap(u32, GraphRef),
    ) Allocator.Error!void {
        _ = self;
        try refs_by_mono.put(mono_key, resolved_ref);
        if (resolved_key != mono_key) {
            try refs_by_mono.put(resolved_key, resolved_ref);
        }
    }

    fn reserveLocalRef(
        self: *Resolver,
        mono_key: u32,
        resolved_key: u32,
        graph: *LayoutGraph,
        refs_by_mono: *std.AutoHashMap(u32, GraphRef),
    ) Allocator.Error!GraphRef {
        const node_id = try graph.reserveNode(self.allocator);
        const local_ref: GraphRef = .{ .local = node_id };
        try self.cacheResolvedRef(mono_key, resolved_key, local_ref, refs_by_mono);
        return local_ref;
    }

    fn buildStructFromElems(
        self: *Resolver,
        elems: []const Monotype.TypeId,
        overrides: ?*const std.AutoHashMap(u32, layout.Idx),
        graph: *LayoutGraph,
        refs_by_mono: *std.AutoHashMap(u32, GraphRef),
    ) Allocator.Error!GraphRef {
        if (elems.len == 0) return .{ .canonical = .zst };

        const node_id = try graph.reserveNode(self.allocator);
        try self.fillStructNodeFromElems(node_id, elems, overrides, graph, refs_by_mono);
        return .{ .local = node_id };
    }

    fn fillStructNodeFromElems(
        self: *Resolver,
        node_id: GraphNodeId,
        elems: []const Monotype.TypeId,
        overrides: ?*const std.AutoHashMap(u32, layout.Idx),
        graph: *LayoutGraph,
        refs_by_mono: *std.AutoHashMap(u32, GraphRef),
    ) Allocator.Error!void {
        var fields = std.ArrayList(GraphField).empty;
        defer fields.deinit(self.allocator);
        try fields.ensureTotalCapacity(self.allocator, elems.len);
        for (elems, 0..) |elem_idx, i| {
            fields.appendAssumeCapacity(.{
                .index = @intCast(i),
                .child = try self.buildRefForMonotype(elem_idx, overrides, graph, refs_by_mono),
            });
        }
        try self.setNode(node_id, .{ .struct_ = try graph.appendFields(self.allocator, fields.items) }, graph);
    }

    fn fillStructNodeFromFields(
        self: *Resolver,
        node_id: GraphNodeId,
        fields_slice: []const Monotype.FieldKey,
        overrides: ?*const std.AutoHashMap(u32, layout.Idx),
        graph: *LayoutGraph,
        refs_by_mono: *std.AutoHashMap(u32, GraphRef),
    ) Allocator.Error!void {
        var fields = std.ArrayList(GraphField).empty;
        defer fields.deinit(self.allocator);
        try fields.ensureTotalCapacity(self.allocator, fields_slice.len);
        for (fields_slice, 0..) |field, i| {
            fields.appendAssumeCapacity(.{
                .index = @intCast(i),
                .child = try self.buildRefForMonotype(field.ty, overrides, graph, refs_by_mono),
            });
        }
        try self.setNode(node_id, .{ .struct_ = try graph.appendFields(self.allocator, fields.items) }, graph);
    }

    fn fillTagUnionNode(
        self: *Resolver,
        node_id: GraphNodeId,
        tags: []const Monotype.TagKey,
        overrides: ?*const std.AutoHashMap(u32, layout.Idx),
        graph: *LayoutGraph,
        refs_by_mono: *std.AutoHashMap(u32, GraphRef),
    ) Allocator.Error!void {
        var variants = std.ArrayList(GraphRef).empty;
        defer variants.deinit(self.allocator);
        try variants.ensureTotalCapacity(self.allocator, tags.len);
        for (tags) |tag| {
            variants.appendAssumeCapacity(try self.buildPayloadRef(
                self.monotype_store.getIdxListItems(tag.payloads),
                overrides,
                graph,
                refs_by_mono,
            ));
        }
        try self.setNode(node_id, .{ .tag_union = try graph.appendRefs(self.allocator, variants.items) }, graph);
    }

    fn buildPayloadRef(
        self: *Resolver,
        payloads: []const Monotype.TypeId,
        overrides: ?*const std.AutoHashMap(u32, layout.Idx),
        graph: *LayoutGraph,
        refs_by_mono: *std.AutoHashMap(u32, GraphRef),
    ) Allocator.Error!GraphRef {
        if (payloads.len == 0) return .{ .canonical = .zst };
        return self.buildStructFromElems(payloads, overrides, graph, refs_by_mono);
    }

    fn setNode(
        self: *Resolver,
        node_id: GraphNodeId,
        node: GraphNode,
        graph: *LayoutGraph,
    ) Allocator.Error!void {
        _ = self;
        graph.setNode(node_id, node);
    }
};

fn findEquivalentRootNode(
    allocator: Allocator,
    graph: *const LayoutGraph,
    root: graph_mod.NodeId,
) Allocator.Error!?graph_mod.NodeId {
    if (graph.nodes.items.len <= 1) return null;

    var visited_pairs = std.AutoHashMap(u64, void).init(allocator);
    defer visited_pairs.deinit();

    for (graph.nodes.items, 0..) |_, i| {
        const candidate: graph_mod.NodeId = @enumFromInt(i);
        if (candidate == root) continue;
        if (try localNodesEquivalent(graph, root, candidate, &visited_pairs)) {
            return candidate;
        }
        visited_pairs.clearRetainingCapacity();
    }

    return null;
}

fn localNodesEquivalent(
    graph: *const LayoutGraph,
    a: graph_mod.NodeId,
    b: graph_mod.NodeId,
    visited_pairs: *std.AutoHashMap(u64, void),
) Allocator.Error!bool {
    const pair_key = (@as(u64, @intFromEnum(a)) << 32) | @as(u64, @intFromEnum(b));
    if (visited_pairs.contains(pair_key)) return true;
    try visited_pairs.put(pair_key, {});

    const node_a = graph.getNode(a);
    const node_b = graph.getNode(b);
    if (@intFromEnum(node_a) != @intFromEnum(node_b)) return false;

    return switch (node_a) {
        .pending => false,
        .box => |child_a| refsEquivalent(graph, child_a, node_b.box, visited_pairs),
        .list => |child_a| refsEquivalent(graph, child_a, node_b.list, visited_pairs),
        .closure => |child_a| refsEquivalent(graph, child_a, node_b.closure, visited_pairs),
        .struct_ => |span_a| blk: {
            const fields_a = graph.getFields(span_a);
            const fields_b = graph.getFields(node_b.struct_);
            if (fields_a.len != fields_b.len) break :blk false;
            for (fields_a, fields_b) |field_a, field_b| {
                if (field_a.index != field_b.index) break :blk false;
                if (!try refsEquivalent(graph, field_a.child, field_b.child, visited_pairs)) break :blk false;
            }
            break :blk true;
        },
        .tag_union => |span_a| blk: {
            const refs_a = graph.getRefs(span_a);
            const refs_b = graph.getRefs(node_b.tag_union);
            if (refs_a.len != refs_b.len) break :blk false;
            for (refs_a, refs_b) |ref_a, ref_b| {
                if (!try refsEquivalent(graph, ref_a, ref_b, visited_pairs)) break :blk false;
            }
            break :blk true;
        },
    };
}

fn refsEquivalent(
    graph: *const LayoutGraph,
    a: GraphRef,
    b: GraphRef,
    visited_pairs: *std.AutoHashMap(u64, void),
) Allocator.Error!bool {
    return switch (a) {
        .canonical => |layout_idx_a| switch (b) {
            .canonical => |layout_idx_b| layout_idx_a == layout_idx_b,
            .local => false,
        },
        .local => |node_id_a| switch (b) {
            .canonical => false,
            .local => |node_id_b| try localNodesEquivalent(graph, node_id_a, node_id_b, visited_pairs),
        },
    };
}
