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

/// Span into a graph's contiguous field storage plus the explicit policy the
/// store uses when committing those fields.
pub const FieldSpan = extern struct {
    start: u32,
    len: u16,
    order: FieldOrder = .structural,

    pub const FieldOrder = enum(u8) {
        structural,
        declared,
    };

    pub fn empty() FieldSpan {
        return .{ .start = 0, .len = 0, .order = .structural };
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

/// Structural identity of a graph node as computed by the store's recursive
/// graph analysis. Equal digests describe the same (possibly infinite) runtime
/// representation.
pub const Digest = [32]u8;

/// An already committed layout together with the digest its node carried when
/// it was committed. Unlike a bare canonical ref, this leaf stays transparent
/// to recursion analysis: an unrolled copy of a recursive node that points at
/// it digests exactly as if the committed subgraph had been expanded again.
pub const Committed = struct {
    idx: Idx,
    digest: u32,
};

/// Temporary node shape used before interning into the canonical layout store.
pub const Node = union(enum) {
    pending: void,
    committed: Committed,
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
    digests: std.ArrayListUnmanaged(Digest) = .empty,

    /// Release all graph storage.
    pub fn deinit(self: *Graph, allocator: std.mem.Allocator) void {
        self.digests.deinit(allocator);
        self.nodes.deinit(allocator);
        self.fields.deinit(allocator);
        self.refs.deinit(allocator);
    }

    /// Mark a field span as declaration-ordered. Only nominal records with an
    /// unnamed padding field may select this policy.
    pub fn declaredOrder(self: *const Graph, span: FieldSpan) FieldSpan {
        var has_padding = false;
        for (self.getFields(span)) |field| {
            has_padding = has_padding or field.is_padding;
        }
        std.debug.assert(has_padding);
        var declared = span;
        declared.order = .declared;
        return declared;
    }

    /// Reserve a local node id before its final shape is known.
    pub fn reserveNode(self: *Graph, allocator: std.mem.Allocator) Allocator.Error!NodeId {
        const id: NodeId = @enumFromInt(self.nodes.items.len);
        try self.nodes.append(allocator, .pending);
        return id;
    }

    /// Add a leaf node for an already committed layout whose graph digest was
    /// recorded by the commit that produced it.
    pub fn addCommitted(self: *Graph, allocator: std.mem.Allocator, idx: Idx, digest: Digest) Allocator.Error!NodeId {
        const digest_index: u32 = @intCast(self.digests.items.len);
        try self.digests.append(allocator, digest);
        const id: NodeId = @enumFromInt(self.nodes.items.len);
        try self.nodes.append(allocator, .{ .committed = .{ .idx = idx, .digest = digest_index } });
        return id;
    }

    /// Digest recorded for a committed leaf node.
    pub fn committedDigest(self: *const Graph, committed: Committed) Digest {
        return self.digests.items[committed.digest];
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
            .order = .structural,
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
