//! Resolves MIR monotypes into canonical ordinary-data layouts through the shared layout store.

const std = @import("std");
const layout = @import("layout.zig");
const graph_mod = @import("graph.zig");
const mir = @import("mir");

const Monotype = mir.Monotype;
const Store = @import("store.zig").Store;
const LayoutGraph = graph_mod.Graph;
const GraphField = graph_mod.Field;
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
        const key = @intFromEnum(mono_idx);

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

        // Track which tag_union monotypes are currently being built (for cycle detection).
        var active_tag_unions = std.AutoHashMap(u32, void).init(self.allocator);
        defer active_tag_unions.deinit();

        var root = try self.buildRefForMonotype(mono_idx, overrides, &graph, &refs_by_mono, &active_tag_unions, false);
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
        active_tag_unions: *std.AutoHashMap(u32, void),
        inside_box: bool,
    ) Allocator.Error!GraphRef {
        const mono_key = @intFromEnum(mono_idx);
        if (overrides) |override_map| {
            if (override_map.get(mono_key)) |layout_idx| {
                return .{ .canonical = layout_idx };
            }
        }
        if (refs_by_mono.get(mono_key)) |cached| {
            // When we hit a back-edge to a tag_union that is CURRENTLY being built
            // (i.e., we're inside its variant processing) AND we're not already
            // inside an explicit Box, wrap the reference in a box node.
            // This ensures recursive types like Tree := [..., Wrapper(Tree)] get
            // box indirection for the recursive reference.
            if (!inside_box and active_tag_unions.contains(mono_key)) {
                const box_node_id = try graph.reserveNode(self.allocator);
                graph.setNode(box_node_id, .{ .box = cached });
                return GraphRef{ .local = box_node_id };
            }
            return cached;
        }

        const mono = self.monotype_store.getMonotype(mono_idx);
        const resolved_ref: GraphRef = switch (mono) {
            .recursive_placeholder => unreachable,
            .unit => GraphRef{ .canonical = .zst },
            .prim => |p| GraphRef{ .canonical = switch (p) {
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
            } },
            .func => blk: {
                const empty_captures = try self.layout_store.getEmptyRecordLayout();
                break :blk GraphRef{ .canonical = try self.layout_store.insertLayout(layout.Layout.closure(empty_captures)) };
            },
            .box => |b| blk: {
                const node_id = try graph.reserveNode(self.allocator);
                // Pass inside_box=true so the inner ref doesn't add another box
                const child_ref = try self.buildRefForMonotype(b.inner, overrides, graph, refs_by_mono, active_tag_unions, true);
                graph.setNode(node_id, .{ .box = child_ref });
                break :blk GraphRef{ .local = node_id };
            },
            .list => |l| blk: {
                const node_id = try graph.reserveNode(self.allocator);
                const child_ref = try self.buildRefForMonotype(l.elem, overrides, graph, refs_by_mono, active_tag_unions, false);
                graph.setNode(node_id, .{ .list = child_ref });
                break :blk GraphRef{ .local = node_id };
            },
            .record => |r| try self.buildStructFromFields(self.monotype_store.getFields(r.fields), overrides, graph, refs_by_mono, active_tag_unions),
            .tuple => |t| try self.buildStructFromElems(self.monotype_store.getIdxSpan(t.elems), overrides, graph, refs_by_mono, active_tag_unions),
            .tag_union => |tu| blk: {
                // Reserve a node and cache it BEFORE recursing into payloads,
                // so that recursive tag unions (e.g. Tree := [..., Wrapper(Tree)])
                // find the placeholder on re-entry instead of looping forever.
                const node_id = try graph.reserveNode(self.allocator);
                const local_ref = GraphRef{ .local = node_id };
                try refs_by_mono.put(mono_key, local_ref);

                // Mark this tag_union as active so recursive back-edges are detected.
                try active_tag_unions.put(mono_key, {});
                defer _ = active_tag_unions.remove(mono_key);

                const tags = self.monotype_store.getTags(tu.tags);
                if (tags.len == 0) {
                    graph.setNode(node_id, .{ .tag_union = .{ .start = 0, .len = 0 } });
                    break :blk GraphRef{ .canonical = .zst };
                }

                var variants = std.ArrayList(GraphRef).empty;
                defer variants.deinit(self.allocator);
                try variants.ensureTotalCapacity(self.allocator, tags.len);
                for (tags) |tag| {
                    variants.appendAssumeCapacity(try self.buildPayloadRef(
                        self.monotype_store.getIdxSpan(tag.payloads),
                        overrides,
                        graph,
                        refs_by_mono,
                        active_tag_unions,
                    ));
                }

                const span = try graph.appendRefs(self.allocator, variants.items);
                graph.setNode(node_id, .{ .tag_union = span });
                break :blk local_ref;
            },
        };

        // Only cache tag_union types in refs_by_mono for cycle detection.
        // Non-tag-union types are rebuilt fresh when encountered multiple
        // times so that self-references within recursive tag unions produce
        // back-edges at the tag_union level, not at intermediate types.
        if (mono == .tag_union) {
            if (findEquivalentMonotypeRef(self.monotype_store, mono_idx, refs_by_mono, mono_key)) |equivalent| {
                try refs_by_mono.put(mono_key, equivalent);
                return equivalent;
            }
            try refs_by_mono.put(mono_key, resolved_ref);
        }
        return resolved_ref;
    }

    fn buildStructFromElems(
        self: *Resolver,
        elems: []const Monotype.Idx,
        overrides: ?*const std.AutoHashMap(u32, layout.Idx),
        graph: *LayoutGraph,
        refs_by_mono: *std.AutoHashMap(u32, GraphRef),
        active_tag_unions: *std.AutoHashMap(u32, void),
    ) Allocator.Error!GraphRef {
        if (elems.len == 0) return .{ .canonical = .zst };

        var fields = std.ArrayList(GraphField).empty;
        defer fields.deinit(self.allocator);
        try fields.ensureTotalCapacity(self.allocator, elems.len);
        for (elems, 0..) |elem_idx, i| {
            fields.appendAssumeCapacity(.{
                .index = @intCast(i),
                .child = try self.buildRefForMonotype(elem_idx, overrides, graph, refs_by_mono, active_tag_unions, false),
            });
        }
        return self.buildStructNode(fields.items, graph);
    }

    fn buildStructFromFields(
        self: *Resolver,
        fields_slice: []const Monotype.Field,
        overrides: ?*const std.AutoHashMap(u32, layout.Idx),
        graph: *LayoutGraph,
        refs_by_mono: *std.AutoHashMap(u32, GraphRef),
        active_tag_unions: *std.AutoHashMap(u32, void),
    ) Allocator.Error!GraphRef {
        if (fields_slice.len == 0) return .{ .canonical = .zst };

        var fields = std.ArrayList(GraphField).empty;
        defer fields.deinit(self.allocator);
        try fields.ensureTotalCapacity(self.allocator, fields_slice.len);
        for (fields_slice, 0..) |field, i| {
            fields.appendAssumeCapacity(.{
                .index = @intCast(i),
                .child = try self.buildRefForMonotype(field.type_idx, overrides, graph, refs_by_mono, active_tag_unions, false),
            });
        }
        return self.buildStructNode(fields.items, graph);
    }

    fn buildStructNode(
        self: *Resolver,
        fields: []const GraphField,
        graph: *LayoutGraph,
    ) Allocator.Error!GraphRef {
        if (fields.len == 0) return .{ .canonical = .zst };

        const node_id = try graph.reserveNode(self.allocator);
        const span = try graph.appendFields(self.allocator, fields);
        graph.setNode(node_id, .{ .struct_ = span });
        return .{ .local = node_id };
    }

    fn buildPayloadRef(
        self: *Resolver,
        payloads: []const Monotype.Idx,
        overrides: ?*const std.AutoHashMap(u32, layout.Idx),
        graph: *LayoutGraph,
        refs_by_mono: *std.AutoHashMap(u32, GraphRef),
        active_tag_unions: *std.AutoHashMap(u32, void),
    ) Allocator.Error!GraphRef {
        if (payloads.len == 0) return .{ .canonical = .zst };
        return self.buildStructFromElems(payloads, overrides, graph, refs_by_mono, active_tag_unions);
    }
};

fn findEquivalentMonotypeRef(
    monotype_store: *const Monotype.Store,
    mono_idx: Monotype.Idx,
    refs_by_mono: *const std.AutoHashMap(u32, GraphRef),
    mono_key: u32,
) ?GraphRef {
    const mono = monotype_store.getMonotype(mono_idx);

    var iter = refs_by_mono.iterator();
    while (iter.next()) |entry| {
        if (entry.key_ptr.* == mono_key) continue;
        const other_idx: Monotype.Idx = @enumFromInt(entry.key_ptr.*);
        if (std.meta.eql(monotype_store.getMonotype(other_idx), mono)) {
            return entry.value_ptr.*;
        }
    }

    return null;
}

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
