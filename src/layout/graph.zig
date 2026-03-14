const std = @import("std");
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

/// Struct field edge in a temporary layout graph.
pub const Field = struct {
    index: u16,
    child: Ref,
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
    box: Ref,
    list: Ref,
    closure: Ref,
    struct_: FieldSpan,
    tag_union: RefSpan,
};

/// Mutable builder for a recursive ordinary-data layout graph.
pub const Graph = struct {
    nodes: std.ArrayListUnmanaged(Node) = .empty,
    fields: std.ArrayListUnmanaged(Field) = .empty,
    refs: std.ArrayListUnmanaged(Ref) = .empty,

    /// Release all graph storage.
    pub fn deinit(self: *Graph, allocator: std.mem.Allocator) void {
        self.nodes.deinit(allocator);
        self.fields.deinit(allocator);
        self.refs.deinit(allocator);
    }

    /// Reserve a local node id before its final shape is known.
    pub fn reserveNode(self: *Graph, allocator: std.mem.Allocator) !NodeId {
        const id: NodeId = @enumFromInt(self.nodes.items.len);
        try self.nodes.append(allocator, .pending);
        return id;
    }

    /// Fill in a previously reserved node.
    pub fn setNode(self: *Graph, id: NodeId, node: Node) void {
        self.nodes.items[@intFromEnum(id)] = node;
    }

    /// Append a field slice and return a stable span to it.
    pub fn appendFields(self: *Graph, allocator: std.mem.Allocator, fields: []const Field) !FieldSpan {
        if (fields.len == 0) return .empty();

        const start: u32 = @intCast(self.fields.items.len);
        try self.fields.appendSlice(allocator, fields);
        return .{
            .start = start,
            .len = @intCast(fields.len),
        };
    }

    /// Append a ref slice and return a stable span to it.
    pub fn appendRefs(self: *Graph, allocator: std.mem.Allocator, refs: []const Ref) !RefSpan {
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
