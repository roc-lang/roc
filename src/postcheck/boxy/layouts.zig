//! The committed layout of a closed Boxy type (design.md "One
//! Representation Per Type"): the layout of its row-join set's
//! representative, committed through the shared layout graph so recursive
//! nominal types box their back edges exactly as every other strategy's
//! layouts do.

const std = @import("std");
const layout = @import("layout");
const types = @import("types.zig");
const RowJoins = @import("row_joins.zig");
const Common = @import("../common.zig");

pub const primitiveLayout = Common.primitiveLayout;

const Allocator = std.mem.Allocator;
const TypeId = types.TypeId;

pub const missing_label = "#Missing";
pub const present_label = "#Present";

pub const Layouts = struct {
    allocator: Allocator,
    store: *types.Store,
    converter: types.Converter,
    joins: *const RowJoins.Joins,
    layouts: *layout.Store,
    memo: std.AutoHashMapUnmanaged(TypeId, Committed) = .empty,
    slot_memo: std.AutoHashMapUnmanaged(TypeId, TypeId) = .empty,
    /// The payload of each presence-slot type.
    slot_payloads: std.AutoHashMapUnmanaged(TypeId, TypeId) = .empty,
    /// The recursive component of each open type `recursiveComponent` has
    /// placed, or `no_component` for one in no cycle.
    components: std.AutoHashMapUnmanaged(TypeId, u32) = .empty,
    /// The members of each recursive component that a boxed slot of the
    /// component points at, by component.
    component_targets: std.ArrayList([]const TypeId) = .empty,
    /// The type variables of each recursive member asked about.
    member_vars: std.AutoHashMapUnmanaged(TypeId, []const TypeId) = .empty,

    const Committed = struct { idx: layout.Idx, digest: ?layout.GraphDigest };

    const no_component = std.math.maxInt(u32);

    /// A component a type stores inline: in a struct slot (a record field
    /// or tuple element), as a tag's payload, or as a nominal's backing.
    pub const InlineEdge = struct { child: TypeId, slot: enum { field, payload, backing } };

    pub fn deinit(self: *Layouts) void {
        self.memo.deinit(self.allocator);
        self.slot_memo.deinit(self.allocator);
        self.slot_payloads.deinit(self.allocator);
        self.components.deinit(self.allocator);
        for (self.component_targets.items) |targets| self.allocator.free(targets);
        self.component_targets.deinit(self.allocator);
        var vars = self.member_vars.valueIterator();
        while (vars.next()) |slice| self.allocator.free(slice.*);
        self.member_vars.deinit(self.allocator);
    }

    /// The components `ty` stores inline, as the committed layout graph has
    /// them: a tag with several payloads stores their tuple.
    pub fn inlineEdges(self: *Layouts, ty: TypeId, out: *std.ArrayList(InlineEdge)) Allocator.Error!void {
        switch (self.store.get(ty)) {
            .record => |record| {
                const fields = try self.allocator.dupe(types.Field, self.store.fieldSpan(record.fields));
                defer self.allocator.free(fields);
                for (fields) |field| try out.append(self.allocator, .{ .child = try self.fieldStorage(field), .slot = .field });
            },
            .tuple => |items_span| {
                const items = try self.allocator.dupe(TypeId, self.store.span(items_span));
                defer self.allocator.free(items);
                for (items) |item| try out.append(self.allocator, .{ .child = item, .slot = .field });
            },
            .tag_union => |tag_union| {
                const tags = try self.allocator.dupe(types.Tag, self.store.tagSpan(tag_union.tags));
                defer self.allocator.free(tags);
                for (tags) |tag| {
                    const payloads = try self.allocator.dupe(TypeId, self.store.span(tag.payloads));
                    defer self.allocator.free(payloads);
                    const payload = try self.tagPayload(payloads) orelse continue;
                    try out.append(self.allocator, .{ .child = payload, .slot = .payload });
                }
            },
            .nominal => |nominal| if (nominal.has_backing) {
                try out.append(self.allocator, .{ .child = try self.converter.backing(ty), .slot = .backing });
            },
            .list, .box, .primitive, .empty_record, .function, .variable => {},
        }
    }

    /// The recursive component of the open type `ty` in the graph of inline
    /// components, or null when `ty` is in no cycle. Every type of a
    /// component reaches every other, so all are open when one is.
    pub fn recursiveComponent(self: *Layouts, ty: TypeId) Allocator.Error!?u32 {
        if (self.components.get(ty)) |component| return if (component == no_component) null else component;
        if (try self.converter.representationClosed(ty)) return null;
        try self.placeComponents(ty);
        const component = self.components.get(ty).?;
        return if (component == no_component) null else component;
    }

    /// Whether the inline edge from `parent` to `edge.child` is a slot the
    /// committed layout boxes: a struct slot or a non-struct tag payload
    /// leading back into `parent`'s recursive component, as the shared
    /// layout store boxes the slot edges of a recursive component.
    pub fn boxedEdge(self: *Layouts, parent: TypeId, edge: InlineEdge) Allocator.Error!bool {
        const component = try self.recursiveComponent(parent) orelse return false;
        if (self.components.get(edge.child) != component) return false;
        return switch (edge.slot) {
            .field => true,
            .payload => switch (self.store.get(edge.child)) {
                .record, .tuple => false,
                else => true,
            },
            .backing => false,
        };
    }

    /// The members of recursive component `component` that a boxed slot
    /// points at.
    pub fn componentTargets(self: *const Layouts, component: u32) []const TypeId {
        return self.component_targets.items[component];
    }

    /// Tarjan's strongly connected components over the open types reachable
    /// from `root` through inline edges, with an explicit stack.
    fn placeComponents(self: *Layouts, root: TypeId) Allocator.Error!void {
        const Frame = struct { ty: TypeId, edges: []InlineEdge, next: usize };
        var frames: std.ArrayList(Frame) = .empty;
        defer {
            for (frames.items) |frame| self.allocator.free(frame.edges);
            frames.deinit(self.allocator);
        }
        var index_of: std.AutoHashMapUnmanaged(TypeId, u32) = .empty;
        defer index_of.deinit(self.allocator);
        var low: std.AutoHashMapUnmanaged(TypeId, u32) = .empty;
        defer low.deinit(self.allocator);
        var on_stack: std.AutoHashMapUnmanaged(TypeId, void) = .empty;
        defer on_stack.deinit(self.allocator);
        var stack: std.ArrayList(TypeId) = .empty;
        defer stack.deinit(self.allocator);
        var edges: std.ArrayList(InlineEdge) = .empty;
        defer edges.deinit(self.allocator);

        var counter: u32 = 0;
        var pending_root: ?TypeId = root;
        while (pending_root != null or frames.items.len != 0) {
            if (pending_root) |ty| {
                pending_root = null;
                try index_of.put(self.allocator, ty, counter);
                try low.put(self.allocator, ty, counter);
                counter += 1;
                try stack.append(self.allocator, ty);
                try on_stack.put(self.allocator, ty, {});
                edges.clearRetainingCapacity();
                try self.inlineEdges(ty, &edges);
                const owned = try self.allocator.dupe(InlineEdge, edges.items);
                errdefer self.allocator.free(owned);
                try frames.append(self.allocator, .{ .ty = ty, .edges = owned, .next = 0 });
                continue;
            }
            const top = &frames.items[frames.items.len - 1];
            if (top.next < top.edges.len) {
                const child = top.edges[top.next].child;
                top.next += 1;
                if (self.components.contains(child)) continue;
                if (index_of.get(child)) |child_index| {
                    if (on_stack.contains(child)) {
                        const entry = low.getPtr(top.ty).?;
                        entry.* = @min(entry.*, child_index);
                    }
                    continue;
                }
                if (try self.converter.representationClosed(child)) continue;
                pending_root = child;
                continue;
            }
            const frame = frames.pop().?;
            defer self.allocator.free(frame.edges);
            const ty_low = low.get(frame.ty).?;
            if (frames.items.len != 0) {
                const parent = low.getPtr(frames.items[frames.items.len - 1].ty).?;
                parent.* = @min(parent.*, ty_low);
            }
            if (ty_low != index_of.get(frame.ty).?) continue;
            var start = stack.items.len;
            while (true) {
                start -= 1;
                if (stack.items[start] == frame.ty) break;
            }
            const members = stack.items[start..];
            var cyclic = members.len > 1;
            if (!cyclic) for (frame.edges) |edge| {
                if (edge.child == frame.ty) cyclic = true;
            };
            const component: u32 = if (cyclic) @intCast(self.component_targets.items.len) else no_component;
            for (members) |member| {
                _ = on_stack.remove(member);
                try self.components.put(self.allocator, member, component);
            }
            if (cyclic) try self.component_targets.append(self.allocator, try self.boxedTargets(members));
            stack.shrinkRetainingCapacity(start);
        }
    }

    fn boxedTargets(self: *Layouts, members: []const TypeId) Allocator.Error![]const TypeId {
        var targets: std.ArrayList(TypeId) = .empty;
        errdefer targets.deinit(self.allocator);
        var seen: std.AutoHashMapUnmanaged(TypeId, void) = .empty;
        defer seen.deinit(self.allocator);
        var edges: std.ArrayList(InlineEdge) = .empty;
        defer edges.deinit(self.allocator);
        for (members) |member| {
            edges.clearRetainingCapacity();
            try self.inlineEdges(member, &edges);
            for (edges.items) |edge| {
                if (!try self.boxedEdge(member, edge)) continue;
                if ((try seen.getOrPut(self.allocator, edge.child)).found_existing) continue;
                try targets.append(self.allocator, edge.child);
            }
        }
        return try targets.toOwnedSlice(self.allocator);
    }

    /// The type variables the descriptor of the recursive member `ty`
    /// depends on, in a fixed order.
    pub fn memberVars(self: *Layouts, ty: TypeId) Allocator.Error![]const TypeId {
        if (self.member_vars.get(ty)) |vars| return vars;
        var vars: std.ArrayList(TypeId) = .empty;
        errdefer vars.deinit(self.allocator);
        var seen: std.AutoHashMapUnmanaged(TypeId, void) = .empty;
        defer seen.deinit(self.allocator);
        var pending: std.ArrayList(TypeId) = .empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, ty);
        try seen.put(self.allocator, ty, {});
        var cursor: usize = 0;
        while (cursor < pending.items.len) : (cursor += 1) {
            const current = pending.items[cursor];
            const before = pending.items.len;
            switch (self.store.get(current)) {
                .variable => try vars.append(self.allocator, current),
                .list => |elem| try pending.append(self.allocator, elem),
                .box => |payload| try pending.append(self.allocator, payload),
                .tuple => |items| try pending.appendSlice(self.allocator, self.store.span(items)),
                .record => |record| {
                    if (record.ext) |ext| try pending.append(self.allocator, ext);
                    const fields = try self.allocator.dupe(types.Field, self.store.fieldSpan(record.fields));
                    defer self.allocator.free(fields);
                    for (fields) |field| try pending.append(self.allocator, try self.fieldStorage(field));
                },
                .tag_union => |tag_union| {
                    if (tag_union.ext) |ext| try pending.append(self.allocator, ext);
                    const tags = try self.allocator.dupe(types.Tag, self.store.tagSpan(tag_union.tags));
                    defer self.allocator.free(tags);
                    for (tags) |tag| try pending.appendSlice(self.allocator, self.store.span(tag.payloads));
                },
                .nominal => |nominal| if (nominal.has_backing) try pending.append(self.allocator, try self.converter.backing(current)),
                .primitive, .empty_record, .function => {},
            }
            // Keep each newly reached open type once.
            var write = before;
            for (pending.items[before..]) |child| {
                if (seen.contains(child) or try self.converter.representationClosed(child)) continue;
                try seen.put(self.allocator, child, {});
                pending.items[write] = child;
                write += 1;
            }
            pending.shrinkRetainingCapacity(write);
        }
        const owned = try vars.toOwnedSlice(self.allocator);
        errdefer self.allocator.free(owned);
        try self.member_vars.put(self.allocator, ty, owned);
        return owned;
    }

    /// The presence-slot type `[#Missing, #Present(payload)]` of an
    /// optional field (design.md "Field Kinds").
    pub fn presenceSlot(self: *Layouts, payload: TypeId) Allocator.Error!TypeId {
        if (self.slot_memo.get(payload)) |slot| return slot;
        const missing = try self.store.internName(missing_label);
        const present = try self.store.internName(present_label);
        const slot = try self.store.intern(.{ .tag_union = .{ .tags = &.{
            .{ .name = missing, .payloads = &.{} },
            .{ .name = present, .payloads = &.{payload} },
        }, .ext = null } });
        try self.slot_memo.put(self.allocator, payload, slot);
        try self.slot_payloads.put(self.allocator, slot, payload);
        return slot;
    }

    /// The payload of `ty` when it is an optional field's presence slot.
    pub fn presenceSlotPayload(self: *const Layouts, ty: TypeId) ?TypeId {
        return self.slot_payloads.get(ty);
    }

    /// The stored type of a record field: its value type for an inline slot,
    /// its presence slot otherwise.
    pub fn fieldStorage(self: *Layouts, field: types.Field) Allocator.Error!TypeId {
        return switch (field.kind) {
            .inline_slot => field.ty,
            .presence_slot => try self.presenceSlot(field.ty),
        };
    }

    /// The payload type a tag variant stores: nothing, its one argument, or a
    /// tuple of its arguments.
    pub fn tagPayload(self: *Layouts, payloads: []const TypeId) Allocator.Error!?TypeId {
        return switch (payloads.len) {
            0 => null,
            1 => payloads[0],
            else => try self.store.intern(.{ .tuple = payloads }),
        };
    }

    /// The committed layout of the closed type `ty`.
    pub fn layoutOf(self: *Layouts, raw: TypeId) Allocator.Error!layout.Idx {
        if (!try self.converter.representationClosed(raw)) types.invariant("asked for the committed layout of an open type");
        const ty = self.joins.representative(raw);
        if (self.memo.get(ty)) |committed| return committed.idx;
        if (try self.directLayout(ty)) |idx| {
            try self.memo.put(self.allocator, ty, .{ .idx = idx, .digest = null });
            return idx;
        }

        var graph = layout.Graph{};
        defer graph.deinit(self.allocator);
        var nodes: std.AutoHashMapUnmanaged(TypeId, layout.GraphNodeId) = .empty;
        defer nodes.deinit(self.allocator);
        var order: std.ArrayList(TypeId) = .empty;
        defer order.deinit(self.allocator);

        // Reserve one node per reachable type that needs one; fill every
        // node once all are reserved, so back edges resolve to their node.
        const root = try self.input(&graph, &nodes, &order, ty);
        var cursor: usize = 0;
        while (cursor < order.items.len) : (cursor += 1) {
            const node_ty = order.items[cursor];
            const node = nodes.get(node_ty).?;
            const content = try self.nodeContent(&graph, &nodes, &order, node_ty);
            graph.setNode(node, content);
        }

        var commit = try self.layouts.commitGraph(&graph, root);
        defer commit.deinit(self.allocator);
        if (layout.graphInputCommitted(root)) |idx| return idx;
        var it = nodes.iterator();
        while (it.next()) |entry| {
            const index = @backingInt(entry.value_ptr.*);
            const digest: ?layout.GraphDigest = if (commit.digests.len > index) commit.digests[index] else null;
            try self.memo.put(self.allocator, entry.key_ptr.*, .{ .idx = commit.value_layouts[index], .digest = digest });
        }
        return self.memo.get(ty).?.idx;
    }

    /// A layout that needs no graph node.
    fn directLayout(self: *Layouts, ty: TypeId) Allocator.Error!?layout.Idx {
        return switch (self.store.get(ty)) {
            .primitive => |primitive| primitiveLayout(primitive),
            .empty_record => .zst,
            .function => try self.layouts.insertErasedCallable(),
            // `Box(function)` is the function's erased callable, which is
            // already an allocation of its own (design.md "Host ABI").
            .box => |payload| if (self.store.get(payload) == .function) try self.layouts.insertErasedCallable() else null,
            .variable => types.invariant("a type variable reached committed layout selection"),
            .record, .tuple, .tag_union, .list, .nominal => null,
        };
    }

    fn input(self: *Layouts, graph: *layout.Graph, nodes: *std.AutoHashMapUnmanaged(TypeId, layout.GraphNodeId), order: *std.ArrayList(TypeId), raw: TypeId) Allocator.Error!layout.GraphInput {
        const ty = self.joins.representative(raw);
        if (nodes.get(ty)) |node| return layout.localGraphInput(node);
        if (self.memo.get(ty)) |committed| {
            if (committed.digest) |digest| {
                const node = try graph.addCommitted(self.allocator, committed.idx, digest);
                try nodes.put(self.allocator, ty, node);
                return layout.localGraphInput(node);
            }
            return layout.committedGraphInput(committed.idx);
        }
        if (try self.directLayout(ty)) |idx| return layout.committedGraphInput(idx);
        const node = try graph.reserveNode(self.allocator);
        try nodes.put(self.allocator, ty, node);
        try order.append(self.allocator, ty);
        return layout.localGraphInput(node);
    }

    fn nodeContent(self: *Layouts, graph: *layout.Graph, nodes: *std.AutoHashMapUnmanaged(TypeId, layout.GraphNodeId), order: *std.ArrayList(TypeId), ty: TypeId) Allocator.Error!layout.GraphNode {
        switch (self.store.get(ty)) {
            .list => |elem| return .{ .list = try self.input(graph, nodes, order, elem) },
            .box => |payload| return .{ .box = try self.input(graph, nodes, order, payload) },
            .tuple => |items_span| {
                const items = try self.allocator.dupe(TypeId, self.store.span(items_span));
                defer self.allocator.free(items);
                const fields = try self.allocator.alloc(layout.GraphField, items.len);
                defer self.allocator.free(fields);
                for (items, fields, 0..) |item, *field, index| {
                    field.* = .{ .index = @intCast(index), .child = try self.input(graph, nodes, order, item) };
                }
                return .{ .struct_ = try graph.appendFields(self.allocator, fields) };
            },
            .record => |record| {
                if (record.ext != null) types.invariant("an open record reached committed layout selection");
                const record_fields = try self.allocator.dupe(types.Field, self.store.fieldSpan(record.fields));
                defer self.allocator.free(record_fields);
                const fields = try self.allocator.alloc(layout.GraphField, record_fields.len);
                defer self.allocator.free(fields);
                for (record_fields, fields, 0..) |record_field, *field, index| {
                    const storage = try self.fieldStorage(record_field);
                    field.* = .{ .index = @intCast(index), .child = try self.input(graph, nodes, order, storage) };
                }
                return .{ .struct_ = try graph.appendFields(self.allocator, fields) };
            },
            .tag_union => |tag_union| {
                if (tag_union.ext != null) types.invariant("an open tag union reached committed layout selection");
                const tags = try self.allocator.dupe(types.Tag, self.store.tagSpan(tag_union.tags));
                defer self.allocator.free(tags);
                const refs = try self.allocator.alloc(layout.GraphInput, tags.len);
                defer self.allocator.free(refs);
                for (tags, refs) |tag, *ref| {
                    const payloads = try self.allocator.dupe(TypeId, self.store.span(tag.payloads));
                    defer self.allocator.free(payloads);
                    ref.* = if (try self.tagPayload(payloads)) |payload|
                        try self.input(graph, nodes, order, payload)
                    else
                        layout.committedGraphInput(.zst);
                }
                return .{ .tag_union = try graph.appendRefs(self.allocator, refs) };
            },
            .nominal => |nominal| {
                if (!nominal.has_backing) types.invariant("an opaque type without a backing reached committed layout selection");
                const backing = try self.converter.backing(ty);
                return .{ .nominal = try self.input(graph, nodes, order, backing) };
            },
            .primitive, .empty_record, .function, .variable => unreachable, // `directLayout` answers these
        }
    }
};
