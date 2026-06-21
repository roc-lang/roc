//! Temporary graph representation for logical ordinary-data layouts before the
//! shared canonical layout commit.

const std = @import("std");
const Allocator = std.mem.Allocator;
const layout = @import("./layout.zig");

/// Canonical layout index reused by graph refs that already exist in the store.
pub const Idx = layout.Idx;

/// Temporary node identifier inside a not-yet-interned layout graph.
pub const NodeId = enum(u32) {
    _,

    pub const none: NodeId = @enumFromInt(std.math.maxInt(u32));
};

/// Reference to either an already-canonical layout or a local graph node.
pub const Ref = union(enum) {
    canonical: Idx,
    local: NodeId,
};

/// Public function `refKey`.
pub fn refKey(ref: Ref) u64 {
    return switch (ref) {
        .canonical => |idx| 0x8000_0000_0000_0000 | @as(u64, @intFromEnum(idx)),
        .local => |node_id| @intFromEnum(node_id),
    };
}

/// Struct field edge in a temporary layout graph.
pub const Field = struct {
    index: u16,
    child: Ref,
    /// True for unnamed nominal-record padding spacers: the field's layout
    /// supplies only its size; it occupies bytes (forced to alignment 1) but is
    /// excluded from every semantic field operation (access, equality, refcount,
    /// inspect, glue) and does not contribute its alignment to the struct.
    is_padding: bool = false,
};

/// Span into a graph's contiguous field storage.
pub const FieldSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() FieldSpan {
        return .{ .start = 0, .len = 0 };
    }
};

/// Span into a graph's contiguous ref storage.
pub const RefSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() RefSpan {
        return .{ .start = 0, .len = 0 };
    }
};

/// Temporary node shape used before interning into the canonical layout store.
pub const Node = union(enum) {
    pending: void,
    nominal: Ref,
    box: Ref,
    list: Ref,
    closure: Ref,
    erased_callable: void,
    struct_: FieldSpan,
    tag_union: RefSpan,
};

/// Mutable builder for a recursive ordinary-data layout graph.
pub const Graph = struct {
    nodes: std.ArrayListUnmanaged(Node) = .empty,
    fields: std.ArrayListUnmanaged(Field) = .empty,
    refs: std.ArrayListUnmanaged(Ref) = .empty,
    /// Struct nodes whose fields are in nominal-record declared order. They are
    /// ordinary `.struct_` nodes for every graph pass (cycle detection,
    /// refcounting, boxing); only the final commit differs, keeping declared
    /// order (repaired for padding) instead of sorting by alignment. See
    /// `field_order` and design.md "Nominal Record Field Order".
    nominal_structs: std.ArrayListUnmanaged(NodeId) = .empty,

    /// Release all graph storage.
    pub fn deinit(self: *Graph, allocator: std.mem.Allocator) void {
        self.nodes.deinit(allocator);
        self.fields.deinit(allocator);
        self.refs.deinit(allocator);
        self.nominal_structs.deinit(allocator);
    }

    /// Mark a `.struct_` node as a nominal record laid out in declared order.
    pub fn markNominalStruct(self: *Graph, allocator: std.mem.Allocator, id: NodeId) Allocator.Error!void {
        try self.nominal_structs.append(allocator, id);
    }

    /// Whether `id` was marked as a nominal-record struct node.
    pub fn isNominalStruct(self: *const Graph, id: NodeId) bool {
        for (self.nominal_structs.items) |marked| {
            if (marked == id) return true;
        }
        return false;
    }

    /// Reserve a local node id before its final shape is known.
    pub fn reserveNode(self: *Graph, allocator: std.mem.Allocator) Allocator.Error!NodeId {
        const id: NodeId = @enumFromInt(self.nodes.items.len);
        try self.nodes.append(allocator, .pending);
        return id;
    }

    /// Fill in a previously reserved node.
    pub fn setNode(self: *Graph, id: NodeId, node: Node) void {
        self.nodes.items[@intFromEnum(id)] = node;
    }

    /// Append a field slice and return a stable span to it.
    pub fn appendFields(self: *Graph, allocator: std.mem.Allocator, fields: []const Field) Allocator.Error!FieldSpan {
        if (fields.len == 0) return .empty();

        const start: u32 = @intCast(self.fields.items.len);
        try self.fields.appendSlice(allocator, fields);
        return .{
            .start = start,
            .len = @intCast(fields.len),
        };
    }

    /// Append a ref slice and return a stable span to it.
    pub fn appendRefs(self: *Graph, allocator: std.mem.Allocator, refs: []const Ref) Allocator.Error!RefSpan {
        if (refs.len == 0) return .empty();

        const start: u32 = @intCast(self.refs.items.len);
        try self.refs.appendSlice(allocator, refs);
        return .{
            .start = start,
            .len = @intCast(refs.len),
        };
    }

    /// Fetch a local node by id.
    pub fn getNode(self: *const Graph, id: NodeId) Node {
        return self.nodes.items[@intFromEnum(id)];
    }

    /// Resolve a field span into a slice.
    pub fn getFields(self: *const Graph, span: FieldSpan) []const Field {
        if (span.len == 0) return &.{};
        return self.fields.items[span.start..][0..span.len];
    }

    /// Resolve a ref span into a slice.
    pub fn getRefs(self: *const Graph, span: RefSpan) []const Ref {
        if (span.len == 0) return &.{};
        return self.refs.items[span.start..][0..span.len];
    }
};
