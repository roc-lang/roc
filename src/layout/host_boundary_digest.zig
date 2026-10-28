//! Digest of the binary contract a platform entrypoint shares with its host.
//!
//! Hot reload keeps every loaded image alive, so a value the host holds can be
//! produced by one image and consumed by another. Two images may share a value
//! only when they agree on the layout of everything reachable from it. Every
//! erased callable has the same layout, so layouts alone cannot express what
//! an erased callable's code reads and returns. This digest covers both: the
//! committed layout of each value crossing the boundary, and the argument and
//! result layouts of every erased callable reachable from those values,
//! recursively.
//!
//! The walk runs over the lowering stage's own boundary types through a
//! `Graph` adapter, since only that stage knows the function type each erased
//! callable was erased from. An erased callable's captures are excluded: only
//! the image that packed a callable ever reads its captures.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");

const layout_mod = @import("layout.zig");
const Store = @import("store.zig").Store;

const Allocator = std.mem.Allocator;
const Idx = layout_mod.Idx;

/// A host boundary digest.
pub const Digest = [32]u8;

/// Hash `layout_idx`'s complete layout graph into `hasher`.
pub fn updateLayoutFingerprint(
    allocator: Allocator,
    hasher: *base.Sha256,
    layouts: *const Store,
    layout_idx: Idx,
) Allocator.Error!void {
    var ctx = LayoutHashContext.init(allocator, layouts);
    defer ctx.deinit();
    try ctx.hashIdx(hasher, layout_idx);
}

const LayoutHashContext = struct {
    layouts: *const Store,
    seen: collections.DenseMap(Idx, u32),
    next_seen: u32 = 0,

    fn init(allocator: Allocator, layouts: *const Store) LayoutHashContext {
        return .{
            .layouts = layouts,
            .seen = collections.DenseMap(Idx, u32).init(allocator),
        };
    }

    fn deinit(self: *LayoutHashContext) void {
        self.seen.deinit();
    }

    fn hashIdx(
        self: *LayoutHashContext,
        hasher: *base.Sha256,
        idx: Idx,
    ) Allocator.Error!void {
        if (idx == Idx.none) {
            updateHashBytes(hasher, "layout-none");
            return;
        }

        if (self.seen.get(idx)) |seen_index| {
            updateHashBytes(hasher, "layout-ref");
            updateHashU32(hasher, seen_index);
            return;
        }

        const seen_index = self.next_seen;
        self.next_seen += 1;
        try self.seen.put(idx, seen_index);

        const layout_val = self.layouts.getLayout(idx);
        const size_align = self.layouts.layoutSizeAlign(layout_val);
        updateHashBytes(hasher, "layout-node");
        updateHashU32(hasher, seen_index);
        updateHashU32(hasher, @intCast(@backingInt(layout_val.tag)));
        updateHashU32(hasher, @intCast(size_align.size));
        updateHashU32(hasher, @intCast(size_align.alignment.toByteUnits()));
        updateHashBool(hasher, self.layouts.layoutContainsRefcounted(layout_val));

        switch (layout_val.tag) {
            .scalar => {
                const scalar = layout_val.getScalar();
                updateHashU32(hasher, @intCast(@backingInt(scalar.tag)));
                switch (scalar.tag) {
                    .int => updateHashU32(hasher, @intCast(@backingInt(scalar.getInt()))),
                    .frac => updateHashU32(hasher, @intCast(@backingInt(scalar.getFrac()))),
                    .vector => updateHashU32(hasher, @intCast(@backingInt(scalar.getVector()))),
                    .str, .opaque_ptr => {},
                }
            },
            .box, .list, .ptr => try self.hashIdx(hasher, layout_val.getIdx()),
            // An erased callable's signature is hashed by the boundary walk,
            // which knows the function type the callable was erased from.
            .box_of_zst, .erased_box, .list_of_zst, .erased_callable, .zst, .hole => {},
            .closure => try self.hashIdx(hasher, layout_val.getClosure().captures_layout_idx),
            .struct_ => {
                const info = self.layouts.getStructInfo(layout_val);
                updateHashU32(hasher, @intCast(info.alignment.toByteUnits()));
                updateHashU32(hasher, info.size());
                updateHashU32(hasher, @intCast(info.fields.len));
                for (0..info.fields.len) |i| {
                    const field = info.fields.get(i);
                    updateHashU32(hasher, @intCast(field.index));
                    updateHashBool(hasher, field.is_padding);
                    try self.hashIdx(hasher, field.layout);
                }
            },
            .tag_union => {
                const info = self.layouts.getTagUnionInfo(layout_val);
                updateHashU32(hasher, @intCast(info.alignment.toByteUnits()));
                updateHashU32(hasher, info.size());
                updateHashU32(hasher, info.discriminant_offset);
                updateHashU32(hasher, @intCast(info.data.discriminant_size));
                updateHashU32(hasher, @intCast(info.variants.len));
                for (0..info.variants.len) |i| {
                    const variant = info.variants.get(i);
                    try self.hashIdx(hasher, variant.payload_layout);
                }
            },
        }
    }
};

/// How a boundary type node reaches erased callables.
pub fn Shape(comptime Node: type) type {
    return union(enum) {
        /// The node reaches no erased callable.
        leaf,
        /// The node is laid out as this other node, such as a named type
        /// laid out as its backing.
        transparent: Node,
        /// The node's edges are its children. Each edge's position is the
        /// child's index in the order the layout's own field and variant
        /// indices refer to, so a position and the layout fingerprint
        /// together name one storage slot.
        container,
        /// The node is an erased callable. Its edges are the callable's ABI
        /// arguments in order followed by its ABI result, each with the
        /// layout that value is passed at.
        erased,
    };
}

/// One child of a container node, or one ABI value of an erased callable or
/// an entrypoint.
pub fn Edge(comptime Node: type) type {
    return struct {
        node: Node,
        position: u32,
        /// The layout the value is passed at; set for ABI values only.
        layout: ?Idx = null,
    };
}

/// A digest walk over boundary nodes of type `Node`. `Graph` provides
/// `fn shape(self: Graph, node: Node, edges: *std.ArrayList(Edge(Node))) Error!Shape(Node)`,
/// appending a container's or erased callable's edges to `edges`.
pub fn Walk(comptime Node: type, comptime Graph: type, comptime Error: type) type {
    return struct {
        const Self = @This();
        const NodeEdge = Edge(Node);

        const Frame = struct {
            /// Null for the entrypoint signature, which no node names.
            node: ?Node,
            erased: bool,
            edges_start: usize,
            edges_len: usize,
            next: usize = 0,
            hasher: base.Sha256,
            reaches_erased: bool = false,
            /// The shallowest active frame a cycle in this frame's subtree
            /// refers back to. A frame whose subtree refers above itself has
            /// a digest that depends on its ancestors, so it is not memoized.
            shallowest_ref: usize,
        };

        allocator: Allocator,
        layouts: *const Store,
        graph: Graph,
        /// Completed nodes whose digest depends on no ancestor; null for a
        /// node that reaches no erased callable.
        memo: std.AutoHashMap(Node, ?Digest),
        /// Depth of every node with an active frame.
        active: std.AutoHashMap(Node, usize),
        frames: std.ArrayList(Frame) = .empty,
        edges: std.ArrayList(NodeEdge) = .empty,

        pub fn init(allocator: Allocator, layouts: *const Store, graph: Graph) Self {
            return .{
                .allocator = allocator,
                .layouts = layouts,
                .graph = graph,
                .memo = std.AutoHashMap(Node, ?Digest).init(allocator),
                .active = std.AutoHashMap(Node, usize).init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.edges.deinit(self.allocator);
            self.frames.deinit(self.allocator);
            self.active.deinit();
            self.memo.deinit();
        }

        /// The digest of an entrypoint taking and returning `values`: its
        /// arguments in order followed by its result, each with its layout.
        pub fn signature(self: *Self, values: []const NodeEdge) Error!Digest {
            std.debug.assert(self.frames.items.len == 0 and self.edges.items.len == 0);
            const edges_start = self.edges.items.len;
            try self.edges.appendSlice(self.allocator, values);
            try self.pushFrame(null, true, edges_start);

            var delivered: ?Child = null;
            while (true) {
                const top = self.frames.items.len - 1;
                if (delivered) |child| {
                    self.acceptChild(top, child);
                    delivered = null;
                }
                const frame = &self.frames.items[top];
                if (frame.next == frame.edges_len) {
                    const finished = try self.popFrame();
                    if (self.frames.items.len == 0) return finished.digest orelse unreachable; // an erased frame always reaches an erased callable
                    delivered = finished;
                    continue;
                }

                const edge = self.edges.items[frame.edges_start + frame.next];
                if (frame.erased) {
                    updateHashBytes(&frame.hasher, "abi-value");
                    try updateLayoutFingerprint(self.allocator, &frame.hasher, self.layouts, edge.layout.?);
                }
                delivered = try self.visit(edge.node);
            }
        }

        /// A finished child: its digest, and the shallowest active frame its
        /// digest refers back to.
        const Child = struct {
            digest: ?Digest,
            shallowest_ref: usize,
        };

        /// The finished digest of `start`, or null after pushing its frame.
        fn visit(self: *Self, start: Node) Error!?Child {
            var node = start;
            while (true) {
                if (self.memo.get(node)) |digest| return .{ .digest = digest, .shallowest_ref = std.math.maxInt(usize) };
                if (self.active.get(node)) |depth| {
                    var hasher = base.Sha256.init(.{});
                    updateHashBytes(&hasher, "cycle");
                    updateHashU32(&hasher, @intCast(self.frames.items.len - depth));
                    return .{ .digest = hasher.finalResult(), .shallowest_ref = depth };
                }
                const edges_start = self.edges.items.len;
                switch (try self.graph.shape(node, &self.edges)) {
                    .leaf => {
                        std.debug.assert(self.edges.items.len == edges_start);
                        return .{ .digest = null, .shallowest_ref = std.math.maxInt(usize) };
                    },
                    .transparent => |next| {
                        std.debug.assert(self.edges.items.len == edges_start);
                        node = next;
                    },
                    .container => {
                        try self.pushFrame(node, false, edges_start);
                        return null;
                    },
                    .erased => {
                        try self.pushFrame(node, true, edges_start);
                        return null;
                    },
                }
            }
        }

        fn pushFrame(self: *Self, node: ?Node, erased: bool, edges_start: usize) Error!void {
            const depth = self.frames.items.len;
            var hasher = base.Sha256.init(.{});
            updateHashBytes(&hasher, if (erased) "erased" else "container");
            try self.frames.append(self.allocator, .{
                .node = node,
                .erased = erased,
                .edges_start = edges_start,
                .edges_len = self.edges.items.len - edges_start,
                .hasher = hasher,
                .reaches_erased = erased,
                .shallowest_ref = std.math.maxInt(usize),
            });
            if (node) |active_node| try self.active.put(active_node, depth);
        }

        fn acceptChild(self: *Self, top: usize, child: Child) void {
            const frame = &self.frames.items[top];
            const edge = self.edges.items[frame.edges_start + frame.next];
            frame.next += 1;
            frame.shallowest_ref = @min(frame.shallowest_ref, child.shallowest_ref);
            if (child.digest) |digest| {
                frame.reaches_erased = true;
                updateHashU32(&frame.hasher, edge.position);
                frame.hasher.update(&digest);
            } else if (frame.erased) {
                updateHashBytes(&frame.hasher, "no-erased");
            }
        }

        fn popFrame(self: *Self) Error!Child {
            var frame = self.frames.pop().?;
            const depth = self.frames.items.len;
            self.edges.shrinkRetainingCapacity(frame.edges_start);
            const digest: ?Digest = if (frame.reaches_erased) frame.hasher.finalResult() else null;
            if (frame.node) |node| {
                _ = self.active.remove(node);
                // Only a subtree referring to no ancestor has a digest that
                // is the same wherever the node is reached.
                if (frame.shallowest_ref >= depth) try self.memo.put(node, digest);
            }
            return .{ .digest = digest, .shallowest_ref = if (frame.shallowest_ref >= depth) std.math.maxInt(usize) else frame.shallowest_ref };
        }
    };
}

fn updateHashU32(hasher: *base.Sha256, value: u32) void {
    var buf: [4]u8 = undefined;
    std.mem.writeInt(u32, &buf, value, .little);
    hasher.update(&buf);
}

fn updateHashBool(hasher: *base.Sha256, value: bool) void {
    hasher.update(if (value) "\x01" else "\x00");
}

fn updateHashBytes(hasher: *base.Sha256, bytes: []const u8) void {
    var len_buf: [8]u8 = undefined;
    std.mem.writeInt(u64, &len_buf, @intCast(bytes.len), .little);
    hasher.update(&len_buf);
    hasher.update(bytes);
}

/// A boundary graph for tests: each node is a leaf, an alias of another
/// node, a container of other nodes, or an erased callable taking and
/// returning other nodes at given layouts.
const TestGraph = struct {
    nodes: []const TestNode,

    const AbiValue = struct { node: u32, layout: Idx = .u64 };

    const TestNode = union(enum) {
        leaf,
        alias: u32,
        container: []const u32,
        erased: []const AbiValue,
    };

    pub fn shape(self: TestGraph, node: u32, edges: *std.ArrayList(Edge(u32))) Allocator.Error!Shape(u32) {
        switch (self.nodes[node]) {
            .leaf => return .leaf,
            .alias => |target| return .{ .transparent = target },
            .container => |children| {
                for (children, 0..) |child, index| try edges.append(std.testing.allocator, .{ .node = child, .position = @intCast(index) });
                return .container;
            },
            .erased => |abi| {
                for (abi, 0..) |value, index| try edges.append(std.testing.allocator, .{ .node = value.node, .position = @intCast(index), .layout = value.layout });
                return .erased;
            },
        }
    }
};

fn testDigest(store: *const Store, nodes: []const TestGraph.TestNode, root: u32) Allocator.Error!Digest {
    var walk = Walk(u32, TestGraph, Allocator.Error).init(std.testing.allocator, store, .{ .nodes = nodes });
    defer walk.deinit();
    return try walk.signature(&.{.{ .node = root, .position = 0, .layout = .u64 }});
}

fn expectDistinct(lhs: Digest, rhs: Digest) error{TestUnexpectedResult}!void {
    try std.testing.expect(!std.meta.eql(lhs, rhs));
}

test "host boundary digest covers a state reached only through nested erased callables" {
    var store = try Store.init(std.testing.allocator, .u64);
    defer store.deinit();
    const larger_state = try store.putStructFields(&.{ .{ .index = 0, .layout = .u64 }, .{ .index = 1, .layout = .u64 } });

    // A boxed `(State -> U64) -> U64`: node 0's ABI takes node 1 and returns
    // a leaf, and node 1's ABI takes the state and returns a leaf.
    const small = [_]TestGraph.TestNode{
        .{ .erased = &.{ .{ .node = 1, .layout = .opaque_ptr }, .{ .node = 2 } } },
        .{ .erased = &.{ .{ .node = 2 }, .{ .node = 2 } } },
        .leaf,
    };
    const larger = [_]TestGraph.TestNode{
        .{ .erased = &.{ .{ .node = 1, .layout = .opaque_ptr }, .{ .node = 2 } } },
        .{ .erased = &.{ .{ .node = 2, .layout = larger_state }, .{ .node = 2 } } },
        .leaf,
    };
    try std.testing.expectEqual(try testDigest(&store, &small, 0), try testDigest(&store, &small, 0));
    try expectDistinct(try testDigest(&store, &small, 0), try testDigest(&store, &larger, 0));
}

test "host boundary digest distinguishes which slot holds which erased callable" {
    var store = try Store.init(std.testing.allocator, .u64);
    defer store.deinit();

    const unary: TestGraph.TestNode = .{ .erased = &.{ .{ .node = 3 }, .{ .node = 3 } } };
    const binary: TestGraph.TestNode = .{ .erased = &.{ .{ .node = 3 }, .{ .node = 3 }, .{ .node = 3 } } };
    const first = [_]TestGraph.TestNode{ .{ .container = &.{ 1, 2 } }, unary, binary, .leaf };
    const swapped = [_]TestGraph.TestNode{ .{ .container = &.{ 2, 1 } }, unary, binary, .leaf };
    try expectDistinct(try testDigest(&store, &first, 0), try testDigest(&store, &swapped, 0));
}

test "host boundary digest sees through transparent nodes and ignores callable-free children" {
    var store = try Store.init(std.testing.allocator, .u64);
    defer store.deinit();

    const callable: TestGraph.TestNode = .{ .erased = &.{ .{ .node = 2 }, .{ .node = 2 } } };
    const direct = [_]TestGraph.TestNode{ .{ .container = &.{ 2, 1 } }, callable, .leaf };
    const aliased = [_]TestGraph.TestNode{ .{ .container = &.{ 2, 3 } }, callable, .leaf, .{ .alias = 1 } };
    const nested_leaf = [_]TestGraph.TestNode{ .{ .container = &.{ 4, 1 } }, callable, .leaf, .leaf, .{ .container = &.{ 2, 3 } } };
    try std.testing.expectEqual(try testDigest(&store, &direct, 0), try testDigest(&store, &aliased, 0));
    try std.testing.expectEqual(try testDigest(&store, &direct, 0), try testDigest(&store, &nested_leaf, 0));
}

test "host boundary digest terminates on recursive types and depends on where they recur" {
    var store = try Store.init(std.testing.allocator, .u64);
    defer store.deinit();

    // A recursive state whose erased callable takes the state itself, and
    // the same shape whose callable takes an unrelated state instead.
    const self_reference = [_]TestGraph.TestNode{
        .{ .container = &.{ 1, 2 } },
        .{ .erased = &.{ .{ .node = 0 }, .{ .node = 2 } } },
        .leaf,
    };
    const other_reference = [_]TestGraph.TestNode{
        .{ .container = &.{ 1, 2 } },
        .{ .erased = &.{ .{ .node = 3 }, .{ .node = 2 } } },
        .leaf,
        .{ .container = &.{ 2, 2 } },
    };
    const first = try testDigest(&store, &self_reference, 0);
    try std.testing.expectEqual(first, try testDigest(&store, &self_reference, 0));
    try expectDistinct(first, try testDigest(&store, &other_reference, 0));
}
