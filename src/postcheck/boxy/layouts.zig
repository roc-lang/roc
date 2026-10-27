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

    const Committed = struct { idx: layout.Idx, digest: ?layout.GraphDigest };

    pub fn deinit(self: *Layouts) void {
        self.memo.deinit(self.allocator);
        self.slot_memo.deinit(self.allocator);
        self.slot_payloads.deinit(self.allocator);
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
            .variable => types.invariant("a type variable reached committed layout selection"),
            .record, .tuple, .tag_union, .list, .box, .nominal => null,
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
