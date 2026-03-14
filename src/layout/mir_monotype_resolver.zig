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

        const root = try self.buildRefForMonotype(mono_idx, overrides, &graph, &refs_by_mono);
        return self.layout_store.internGraph(&graph, root);
    }

    fn buildRefForMonotype(
        self: *Resolver,
        mono_idx: Monotype.Idx,
        overrides: ?*const std.AutoHashMap(u32, layout.Idx),
        graph: *LayoutGraph,
        refs_by_mono: *std.AutoHashMap(u32, GraphRef),
    ) Allocator.Error!GraphRef {
        const mono_key = @intFromEnum(mono_idx);
        if (overrides) |override_map| {
            if (override_map.get(mono_key)) |layout_idx| {
                return .{ .canonical = layout_idx };
            }
        }
        if (refs_by_mono.get(mono_key)) |cached| return cached;

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
                const local_ref = GraphRef{ .local = node_id };
                try refs_by_mono.put(mono_key, local_ref);
                const child_ref = try self.buildRefForMonotype(b.inner, overrides, graph, refs_by_mono);
                graph.setNode(node_id, .{ .box = child_ref });
                break :blk local_ref;
            },
            .list => |l| blk: {
                const node_id = try graph.reserveNode(self.allocator);
                const local_ref = GraphRef{ .local = node_id };
                try refs_by_mono.put(mono_key, local_ref);
                const child_ref = try self.buildRefForMonotype(l.elem, overrides, graph, refs_by_mono);
                graph.setNode(node_id, .{ .list = child_ref });
                break :blk local_ref;
            },
            .record => |r| try self.buildStructFromFields(self.monotype_store.getFields(r.fields), overrides, graph, refs_by_mono),
            .tuple => |t| try self.buildStructFromElems(self.monotype_store.getIdxSpan(t.elems), overrides, graph, refs_by_mono),
            .tag_union => |tu| try self.buildTagUnionRef(self.monotype_store.getTags(tu.tags), overrides, graph, refs_by_mono),
        };

        try refs_by_mono.put(mono_key, resolved_ref);
        return resolved_ref;
    }

    fn buildStructFromElems(
        self: *Resolver,
        elems: []const Monotype.Idx,
        overrides: ?*const std.AutoHashMap(u32, layout.Idx),
        graph: *LayoutGraph,
        refs_by_mono: *std.AutoHashMap(u32, GraphRef),
    ) Allocator.Error!GraphRef {
        if (elems.len == 0) return .{ .canonical = .zst };
        if (elems.len == 1) return self.buildRefForMonotype(elems[0], overrides, graph, refs_by_mono);

        var fields = std.ArrayList(GraphField).empty;
        defer fields.deinit(self.allocator);
        try fields.ensureTotalCapacity(self.allocator, elems.len);
        for (elems, 0..) |elem_idx, i| {
            fields.appendAssumeCapacity(.{
                .index = @intCast(i),
                .child = try self.buildRefForMonotype(elem_idx, overrides, graph, refs_by_mono),
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
    ) Allocator.Error!GraphRef {
        if (fields_slice.len == 0) return .{ .canonical = .zst };
        if (fields_slice.len == 1) return self.buildRefForMonotype(fields_slice[0].type_idx, overrides, graph, refs_by_mono);

        var fields = std.ArrayList(GraphField).empty;
        defer fields.deinit(self.allocator);
        try fields.ensureTotalCapacity(self.allocator, fields_slice.len);
        for (fields_slice, 0..) |field, i| {
            fields.appendAssumeCapacity(.{
                .index = @intCast(i),
                .child = try self.buildRefForMonotype(field.type_idx, overrides, graph, refs_by_mono),
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
        if (fields.len == 1) return fields[0].child;

        const node_id = try graph.reserveNode(self.allocator);
        const span = try graph.appendFields(self.allocator, fields);
        graph.setNode(node_id, .{ .struct_ = span });
        return .{ .local = node_id };
    }

    fn buildTagUnionRef(
        self: *Resolver,
        tags: []const Monotype.Tag,
        overrides: ?*const std.AutoHashMap(u32, layout.Idx),
        graph: *LayoutGraph,
        refs_by_mono: *std.AutoHashMap(u32, GraphRef),
    ) Allocator.Error!GraphRef {
        if (tags.len == 0) return .{ .canonical = .zst };

        if (tags.len == 1) {
            return self.buildPayloadRef(
                self.monotype_store.getIdxSpan(tags[0].payloads),
                overrides,
                graph,
                refs_by_mono,
            );
        }

        if (tags.len == 2) {
            const p0 = self.monotype_store.getIdxSpan(tags[0].payloads);
            const p1 = self.monotype_store.getIdxSpan(tags[1].payloads);
            if (p0.len == 0 and p1.len == 0) return .{ .canonical = .bool };
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
            ));
        }

        const node_id = try graph.reserveNode(self.allocator);
        const span = try graph.appendRefs(self.allocator, variants.items);
        graph.setNode(node_id, .{ .tag_union = span });
        return .{ .local = node_id };
    }

    fn buildPayloadRef(
        self: *Resolver,
        payloads: []const Monotype.Idx,
        overrides: ?*const std.AutoHashMap(u32, layout.Idx),
        graph: *LayoutGraph,
        refs_by_mono: *std.AutoHashMap(u32, GraphRef),
    ) Allocator.Error!GraphRef {
        if (payloads.len == 0) return .{ .canonical = .zst };
        if (payloads.len == 1) return self.buildRefForMonotype(payloads[0], overrides, graph, refs_by_mono);
        return self.buildStructFromElems(payloads, overrides, graph, refs_by_mono);
    }
};
