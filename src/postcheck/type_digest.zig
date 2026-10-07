//! Reusable structural type-digest graph reduction.
//! Nodes contain scalar content digests and explicit ordered child edges. Discovery is
//! owned by the producer; this reducer resolves acyclic nodes once and reduces
//! cyclic components by bisimulation before hashing positions ordered by label.
const std = @import("std");
const TypeDigestHasher = @import("base").TypeDigestHasher;
const Common = @import("common.zig");
const Allocator = std.mem.Allocator;
/// SHA-256 structural digest.
pub const Identity = [32]u8;
const Node = struct {
    scalar: Identity = undefined,
    child_start: u32 = 0,
    child_len: u32 = 0,
    index: u32 = unvisited,
    lowlink: u32 = unvisited,
    on_stack: bool = false,
    digest: ?Identity = null,
};
const unvisited = std.math.maxInt(u32);
const no_position = std.math.maxInt(u32);
const Frame = struct { node: u32, next_child: u32 };
/// Scratch graph. Reset discards every result but retains all buffer capacity.
pub const Graph = struct {
    gpa: Allocator,
    nodes: std.ArrayList(Node) = .empty,
    children: std.ArrayList(u32) = .empty,
    frames: std.ArrayList(Frame) = .empty,
    scc_stack: std.ArrayList(u32) = .empty,
    members: std.ArrayList(u32) = .empty,
    positions: std.ArrayList(u32) = .empty,
    labels: std.ArrayList(Identity) = .empty,
    next_labels: std.ArrayList(Identity) = .empty,
    sorted_labels: std.ArrayList(Identity) = .empty,
    ranks: std.ArrayList(u32) = .empty,
    representatives: std.ArrayList(u32) = .empty,
    distinct_labels: std.AutoHashMap(Identity, u32),
    unfoldings: std.AutoHashMap(Identity, Identity),

    /// Create empty scratch buffers.
    pub fn init(gpa: Allocator) Graph {
        return .{ .gpa = gpa, .distinct_labels = std.AutoHashMap(Identity, u32).init(gpa), .unfoldings = std.AutoHashMap(Identity, Identity).init(gpa) };
    }
    /// Release retained buffers.
    pub fn deinit(self: *Graph) void {
        inline for (.{ "nodes", "children", "frames", "scc_stack", "members", "positions", "labels", "next_labels", "sorted_labels", "ranks", "representatives" }) |field| @field(self, field).deinit(self.gpa);
        self.distinct_labels.deinit();
        self.unfoldings.deinit();
    }
    /// Start a new request, including after allocation failure.
    pub fn reset(self: *Graph) void {
        inline for (.{ "nodes", "children", "frames", "scc_stack", "members", "positions", "labels", "next_labels", "sorted_labels", "ranks", "representatives" }) |field| @field(self, field).clearRetainingCapacity();
        self.distinct_labels.clearRetainingCapacity();
        self.unfoldings.clearRetainingCapacity();
    }
    /// Reserve a node before discovering its children.
    pub fn addNode(self: *Graph) Allocator.Error!u32 {
        const node: u32 = @intCast(self.nodes.items.len);
        try self.nodes.append(self.gpa, .{});
        return node;
    }
    /// Begin the ordered encoding of a reserved node.
    pub fn beginNode(self: *Graph, node: u32) void {
        self.nodes.items[node].child_start = @intCast(self.children.items.len);
    }
    /// Finish the ordered encoding of a reserved node.
    pub fn endNode(self: *Graph, node: u32) void {
        const entry = &self.nodes.items[node];
        entry.child_len = @intCast(self.children.items.len - entry.child_start);
    }
    /// Record the producer's complete scalar content digest for a node.
    pub fn setScalar(self: *Graph, node: u32, scalar: Identity) void {
        self.nodes.items[node].scalar = scalar;
    }
    /// Append one child edge.
    pub fn putChild(self: *Graph, node: u32) Allocator.Error!void {
        try self.children.append(self.gpa, node);
    }
    /// Resolve the graph and return the requested node's digest.
    pub fn resolve(self: *Graph, root: u32) Allocator.Error!Identity {
        try self.resolveAll();
        return self.nodes.items[root].digest orelse unreachable;
    }
    fn childrenOf(self: *const Graph, node: u32) []const u32 {
        const entry = self.nodes.items[node];
        return self.children.items[entry.child_start .. entry.child_start + entry.child_len];
    }

    /// Iterative Tarjan discovery over every node. Components pop in
    /// reverse topological order, so every component a node reaches is
    /// resolved before the node's own.
    fn resolveAll(self: *Graph) Allocator.Error!void {
        const frames = &self.frames;
        const scc_stack = &self.scc_stack;
        const members = &self.members;
        var next_index: u32 = 0;
        try self.positions.resize(self.gpa, self.nodes.items.len);
        @memset(self.positions.items, no_position);

        for (0..self.nodes.items.len) |start_index| {
            const start: u32 = @intCast(start_index);
            if (self.nodes.items[start].index != unvisited) continue;
            try self.visit(start, &next_index, scc_stack);
            try frames.append(self.gpa, .{ .node = start, .next_child = 0 });
            while (frames.items.len != 0) {
                const frame = &frames.items[frames.items.len - 1];
                const node = frame.node;
                const children = self.childrenOf(node);
                if (frame.next_child < children.len) {
                    const target = children[frame.next_child];
                    frame.next_child += 1;
                    const target_entry = &self.nodes.items[target];
                    if (target_entry.index == unvisited) {
                        try self.visit(target, &next_index, scc_stack);
                        try frames.append(self.gpa, .{ .node = target, .next_child = 0 });
                    } else if (target_entry.on_stack) {
                        const entry = &self.nodes.items[node];
                        entry.lowlink = @min(entry.lowlink, target_entry.index);
                    }
                    continue;
                }
                _ = frames.pop();
                const entry = self.nodes.items[node];
                if (frames.items.len != 0) {
                    const parent = &self.nodes.items[frames.items[frames.items.len - 1].node];
                    parent.lowlink = @min(parent.lowlink, entry.lowlink);
                }
                if (entry.lowlink == entry.index) {
                    members.clearRetainingCapacity();
                    while (true) {
                        const member = scc_stack.pop() orelse
                            Common.invariant("type digest reduction popped past its component root");
                        self.nodes.items[member].on_stack = false;
                        try members.append(self.gpa, member);
                        if (member == node) break;
                    }
                    try self.resolveComponent(members.items);
                }
            }
        }
    }

    fn visit(self: *Graph, node: u32, next_index: *u32, scc_stack: *std.ArrayList(u32)) Allocator.Error!void {
        const entry = &self.nodes.items[node];
        entry.index = next_index.*;
        entry.lowlink = next_index.*;
        entry.on_stack = true;
        next_index.* += 1;
        try scc_stack.append(self.gpa, node);
    }

    fn resolveComponent(self: *Graph, members: []const u32) Allocator.Error!void {
        if (members.len == 1 and !self.hasSelfEdge(members[0])) {
            const node = members[0];
            const key = self.unfoldingKey(node);
            const digest = self.unfoldings.get(key) orelse key;
            self.finalize(node, digest);
            return;
        }
        try self.resolveCyclicComponent(members);
    }

    fn hasSelfEdge(self: *const Graph, node: u32) bool {
        for (self.childrenOf(node)) |child| if (child == node) return true;
        return false;
    }

    fn childDigest(self: *const Graph, child: u32) Identity {
        return self.nodes.items[child].digest orelse
            Common.invariant("type digest reduction read a child digest before resolving it");
    }

    /// The node's encoding with every child as its finished digest.
    fn unfoldingKey(self: *const Graph, node: u32) Identity {
        var hasher = TypeDigestHasher.init();
        writeBytes(&hasher, "roc.lambda-mono.type.v2");
        writeBytes(&hasher, "node");
        hasher.update(&self.nodes.items[node].scalar);
        for (self.childrenOf(node)) |child| {
            writeBytes(&hasher, "child");
            hasher.update(&self.childDigest(child));
        }
        return hasher.finalResult();
    }

    fn finalize(self: *Graph, node: u32, digest: Identity) void {
        const entry = &self.nodes.items[node];
        if (entry.digest != null) Common.invariant("type digest reduction resolved a type twice");
        entry.digest = digest;
    }

    /// Reduce one cyclic component by bisimulation refinement over content
    /// labels, order the reduced positions by their final labels, and digest
    /// every member as its position in that one group rendering.
    fn resolveCyclicComponent(self: *Graph, members: []const u32) Allocator.Error!void {
        const member_count = members.len;
        const position_of_node = self.positions.items;
        defer for (members) |node| {
            position_of_node[node] = no_position;
        };
        for (members, 0..) |node, pos| position_of_node[node] = @intCast(pos);

        // Refine to the stable bisimulation partition. Every member starts
        // from its content label: its encoding with every out-of-component
        // child as a finished digest and every in-component child as a bare
        // marker. Each round relabels a member by its own label followed by
        // the labels of its ordered in-component children until a round
        // stops separating members. A label is a pure function of the
        // member's unfolding to the current depth, so two members carry
        // equal labels exactly when they are bisimilar, and the labels are
        // identical for bisimilar positions of any two knots regardless of
        // how many nodes either knot uses.
        try self.labels.resize(self.gpa, member_count);
        try self.next_labels.resize(self.gpa, member_count);
        var labels = self.labels.items;
        var next_labels = self.next_labels.items;
        const distinct_labels = &self.distinct_labels;
        distinct_labels.clearRetainingCapacity();
        for (members, 0..) |node, pos| {
            var hasher = TypeDigestHasher.init();
            writeBytes(&hasher, "label");
            hasher.update(&self.nodes.items[node].scalar);
            for (self.childrenOf(node)) |child| {
                if (position_of_node[child] != no_position) {
                    writeBytes(&hasher, "in-component");
                } else {
                    writeBytes(&hasher, "child");
                    hasher.update(&self.childDigest(child));
                }
            }
            labels[pos] = hasher.finalResult();
            try distinct_labels.put(labels[pos], no_position);
        }
        var label_count: u32 = distinct_labels.count();
        while (true) {
            distinct_labels.clearRetainingCapacity();
            for (members, 0..) |node, pos| {
                var hasher = TypeDigestHasher.init();
                hasher.update(&labels[pos]);
                for (self.childrenOf(node)) |target| {
                    const target_pos = position_of_node[target];
                    if (target_pos == no_position) continue;
                    hasher.update(&labels[target_pos]);
                }
                next_labels[pos] = hasher.finalResult();
                try distinct_labels.put(next_labels[pos], no_position);
            }
            const next_count: u32 = distinct_labels.count();
            const stable = next_count == label_count;
            std.mem.swap([]Identity, &labels, &next_labels);
            label_count = next_count;
            if (stable) break;
        }

        // Group order: the reduced positions sorted by label. Labels are
        // intrinsic to the infinite type, so the order is too.
        const block_count = label_count;
        try self.sorted_labels.resize(self.gpa, block_count);
        const sorted_labels = self.sorted_labels.items;
        {
            var it = distinct_labels.keyIterator();
            var next: usize = 0;
            while (it.next()) |label| : (next += 1) sorted_labels[next] = label.*;
            std.debug.assert(next == block_count);
        }
        std.mem.sort(Identity, sorted_labels, {}, struct {
            fn lessThan(_: void, lhs: Identity, rhs: Identity) bool {
                return std.mem.order(u8, &lhs, &rhs) == .lt;
            }
        }.lessThan);
        for (sorted_labels, 0..) |label, rank| {
            const slot = distinct_labels.getPtr(label) orelse unreachable;
            slot.* = @intCast(rank);
        }
        try self.ranks.resize(self.gpa, member_count);
        const rank_of_member = self.ranks.items;
        try self.representatives.resize(self.gpa, block_count);
        const block_rep = self.representatives.items;
        @memset(block_rep, no_position);
        for (members, 0..) |node, pos| {
            const rank = distinct_labels.get(labels[pos]) orelse unreachable;
            rank_of_member[pos] = rank;
            // The partition is stable, so any member represents its position.
            if (block_rep[rank] == no_position) block_rep[rank] = node;
        }

        // One group rendering: every reduced position in label order with
        // in-component references written as ranks. Every member then
        // digests as its rank against the rendering's digest.
        var group_hasher = TypeDigestHasher.init();
        writeBytes(&group_hasher, "roc.lambda-mono.type.v2");
        writeBytes(&group_hasher, "group");
        writeU32(&group_hasher, block_count);
        for (block_rep) |rep| {
            writeBytes(&group_hasher, "position");
            group_hasher.update(&self.nodes.items[rep].scalar);
            for (self.childrenOf(rep)) |child| {
                if (position_of_node[child] != no_position) {
                    writeBytes(&group_hasher, "rank");
                    writeU32(&group_hasher, rank_of_member[position_of_node[child]]);
                } else {
                    writeBytes(&group_hasher, "child");
                    group_hasher.update(&self.childDigest(child));
                }
            }
        }
        const group_digest = group_hasher.finalResult();
        for (members, 0..) |node, pos| {
            var hasher = TypeDigestHasher.init();
            writeBytes(&hasher, "group-member");
            hasher.update(&group_digest);
            writeU32(&hasher, rank_of_member[pos]);
            self.finalize(node, hasher.finalResult());
        }
        // A rolled-out copy of any position unfolds to the same one-step
        // encoding, so it folds to that position's digest.
        for (members) |node| {
            const digest = self.nodes.items[node].digest orelse unreachable;
            try self.unfoldings.put(self.unfoldingKey(node), digest);
        }
    }
};

fn writeBytes(hasher: *TypeDigestHasher, bytes: []const u8) void {
    writeU32(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

fn encodeU32(buffer: *[4]u8, value: u32) void {
    buffer[0] = @truncate(value);
    buffer[1] = @truncate(value >> 8);
    buffer[2] = @truncate(value >> 16);
    buffer[3] = @truncate(value >> 24);
}

fn writeU32(hasher: *TypeDigestHasher, value: u32) void {
    var buffer: [4]u8 = undefined;
    encodeU32(&buffer, value);
    hasher.update(&buffer);
}
