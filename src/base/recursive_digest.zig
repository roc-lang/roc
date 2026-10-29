//! Canonical digests of possibly recursive type graphs.
//!
//! Every type identity the compiler derives from a graph of type nodes
//! (Monotype and Lambda Mono type digests, procedure identities, recursive
//! layout keys) is reduced here, so each one digests a type as its infinite
//! unfolding rather than as whichever graph a program happened to draw for
//! it. Two programs draw the same recursive type as different graphs: one
//! may tie a single knot, another may roll the knot out several levels, or
//! tie one copy whose references lead into a second, separately tied copy.
//! The digest must not see any of that.
//!
//! A producer describes each node by its content (a digest of its
//! encoding with every child as a bare slot) and its ordered children, each
//! either another node of the graph or a digest settled earlier. The
//! reduction decomposes the graph into strongly connected components, which
//! resolve in reverse topological order, so every child outside a component
//! has its digest when the component resolves:
//!
//! - An acyclic node digests its one-step unfolding: its content with its
//!   children's digests. When that unfolding is the unfolding of a position
//!   of a recursive group settled earlier, the node is a rolled-out copy of
//!   that position and takes its digest.
//! - A cyclic component that refers into a settled recursive group is first
//!   paired coinductively against the group's recorded member shapes. Every
//!   member of a component reaches every other, so either every member is
//!   equivalent to a position of that group or none is; when the pairing
//!   succeeds, the component spells the group's unfolding and its members
//!   take the group's digests.
//! - Any other cyclic component is refined to its coarsest bisimulation
//!   partition, its reduced positions are ordered by their final labels, the
//!   group is rendered once with in-group references written as ranks, and
//!   every member digests as its rank against that rendering. No child
//!   outside such a component is equivalent to one of its members (that
//!   child would lie in a settled group the component pairs with), so the
//!   refinement sees exactly the reduced group whatever graph spelled it,
//!   and the labels, their order, and the rendering are functions of the
//!   infinite type alone.
//!
//! `Settled` keeps every resolved group's member shapes and unfoldings, so
//! these guarantees hold across separate reductions that share it.

const std = @import("std");
const TypeDigestHasher = @import("TypeDigestHasher.zig");
const invariant = @import("invariant.zig").invariant;
const Allocator = std.mem.Allocator;

/// Canonical digest of one type.
pub const Identity = [TypeDigestHasher.digest_length]u8;

/// One ordered child of a node.
pub const Child = union(enum) {
    /// Another node of the same graph.
    node: u32,
    /// A type whose digest was settled before this reduction.
    settled: Identity,
};

/// How a node's digest was settled.
pub const Resolution = enum(u8) {
    /// Not part of a cycle, and not a copy of a recursive position.
    acyclic,
    /// Not part of a cycle in this graph, but a rolled-out copy of a
    /// position of a settled recursive group.
    folded,
    /// A member of a cyclic component.
    cyclic,
};

/// Work counters for one reduction.
pub const Stats = struct {
    /// Node encodings replayed while reducing cyclic components: labels,
    /// the group rendering, and the unfolding index.
    group_encodings: u64 = 0,
};

const no_position = std.math.maxInt(u32);
const unvisited = std.math.maxInt(u32);

/// Everything reductions sharing one digest domain have settled: the
/// unfolding of every recursive position, the member shapes of every
/// recursive group, and the result of every component shape reduced. All of
/// it is content addressed, so it never needs rollback.
pub const Settled = struct {
    /// Owns everything settled.
    gpa: Allocator,
    /// Separates this digest family from every other.
    domain: []const u8,
    /// One-step unfolding of every recursive position, mapped to the
    /// position's digest.
    unfoldings: std.AutoHashMapUnmanaged(Identity, Identity) = .empty,
    /// Group and rank of every recursive position's digest.
    member_of: std.AutoHashMapUnmanaged(Identity, MemberRef) = .empty,
    groups: std.ArrayList(Group) = .empty,
    ranks: std.ArrayList(Rank) = .empty,
    rank_children: std.ArrayList(RankChild) = .empty,
    /// Digests of every cyclic component reduced so far, keyed by its shape:
    /// its members' labels and in-component references in member order. A
    /// component's resolution is a function of exactly that shape.
    shapes: std.AutoHashMapUnmanaged(Identity, ShapeDigests) = .empty,
    shape_digests: std.ArrayList(Identity) = .empty,

    const MemberRef = struct {
        group: u32,
        rank: u32,
    };

    const Group = struct {
        rank_start: u32,
        rank_count: u32,
    };

    /// One reduced position of a settled group.
    const Rank = struct {
        digest: Identity,
        content: Identity,
        child_start: u32,
        child_len: u32,
    };

    const RankChild = union(enum) {
        /// A position of the same group.
        rank: u32,
        /// A type outside the group.
        settled: Identity,
    };

    const ShapeDigests = struct {
        start: u32,
        len: u32,
    };

    pub fn init(gpa: Allocator, domain: []const u8) Settled {
        return .{ .gpa = gpa, .domain = domain };
    }

    pub fn deinit(self: *Settled) void {
        self.unfoldings.deinit(self.gpa);
        self.member_of.deinit(self.gpa);
        self.groups.deinit(self.gpa);
        self.ranks.deinit(self.gpa);
        self.rank_children.deinit(self.gpa);
        self.shapes.deinit(self.gpa);
        self.shape_digests.deinit(self.gpa);
    }

    /// Forget everything settled, keeping capacity.
    pub fn clear(self: *Settled) void {
        self.unfoldings.clearRetainingCapacity();
        self.member_of.clearRetainingCapacity();
        self.groups.clearRetainingCapacity();
        self.ranks.clearRetainingCapacity();
        self.rank_children.clearRetainingCapacity();
        self.shapes.clearRetainingCapacity();
        self.shape_digests.clearRetainingCapacity();
    }

    /// A copy owned by `gpa`.
    pub fn clone(self: *const Settled, gpa: Allocator) Allocator.Error!Settled {
        var result = Settled.init(gpa, self.domain);
        errdefer result.deinit();
        result.unfoldings = try self.unfoldings.clone(gpa);
        result.member_of = try self.member_of.clone(gpa);
        result.groups = try self.groups.clone(gpa);
        result.ranks = try self.ranks.clone(gpa);
        result.rank_children = try self.rank_children.clone(gpa);
        result.shapes = try self.shapes.clone(gpa);
        result.shape_digests = try self.shape_digests.clone(gpa);
        return result;
    }

    fn rank(self: *const Settled, group: u32, member_rank: u32) Rank {
        const entry = self.groups.items[group];
        std.debug.assert(member_rank < entry.rank_count);
        return self.ranks.items[entry.rank_start + member_rank];
    }

    fn rankChildren(self: *const Settled, entry: Rank) []const RankChild {
        return self.rank_children.items[entry.child_start..][0..entry.child_len];
    }
};

/// One graph to reduce. Reset discards every node but keeps all capacity.
pub const Graph = struct {
    gpa: Allocator,
    nodes: std.ArrayList(Node) = .empty,
    children: std.ArrayList(Child) = .empty,
    frames: std.ArrayList(Frame) = .empty,
    scc_stack: std.ArrayList(u32) = .empty,
    members: std.ArrayList(u32) = .empty,
    /// Member position of every node of the component being resolved;
    /// `no_position` everywhere else.
    positions: std.ArrayList(u32) = .empty,
    labels: std.ArrayList(Identity) = .empty,
    next_labels: std.ArrayList(Identity) = .empty,
    sorted_labels: std.ArrayList(Identity) = .empty,
    distinct_labels: std.AutoHashMapUnmanaged(Identity, u32) = .empty,
    member_ranks: std.ArrayList(u32) = .empty,
    representatives: std.ArrayList(u32) = .empty,
    block_digests: std.ArrayList(Identity) = .empty,
    unfolding_keys: std.ArrayList(Identity) = .empty,
    candidate_groups: std.ArrayList(u32) = .empty,
    paired: std.ArrayList(u32) = .empty,
    pending_pairs: std.ArrayList(Pair) = .empty,

    const Node = struct {
        content: Identity = undefined,
        child_start: u32 = 0,
        child_len: u32 = 0,
        index: u32 = unvisited,
        lowlink: u32 = unvisited,
        on_stack: bool = false,
        resolution: Resolution = undefined,
        digest: ?Identity = null,
    };

    const Frame = struct { node: u32, next_child: u32 };

    const Pair = struct { position: u32, rank: u32 };

    pub fn init(gpa: Allocator) Graph {
        return .{ .gpa = gpa };
    }

    pub fn deinit(self: *Graph) void {
        inline for (list_fields) |field| @field(self, field).deinit(self.gpa);
        self.distinct_labels.deinit(self.gpa);
    }

    /// Start a new graph, including after allocation failure.
    pub fn reset(self: *Graph) void {
        inline for (list_fields) |field| @field(self, field).clearRetainingCapacity();
        self.distinct_labels.clearRetainingCapacity();
    }

    const list_fields = .{ "nodes", "children", "frames", "scc_stack", "members", "positions", "labels", "next_labels", "sorted_labels", "member_ranks", "representatives", "block_digests", "unfolding_keys", "candidate_groups", "paired", "pending_pairs" };

    pub fn nodeCount(self: *const Graph) u32 {
        return @intCast(self.nodes.items.len);
    }

    /// Reserve a node before encoding it.
    pub fn addNode(self: *Graph) Allocator.Error!u32 {
        const node: u32 = @intCast(self.nodes.items.len);
        try self.nodes.append(self.gpa, .{});
        return node;
    }

    /// Begin a reserved node's ordered children. The children of one node
    /// are appended before the next node begins.
    pub fn beginNode(self: *Graph, node: u32) void {
        self.nodes.items[node].child_start = @intCast(self.children.items.len);
    }

    pub fn putChild(self: *Graph, child: Child) Allocator.Error!void {
        try self.children.append(self.gpa, child);
    }

    /// Finish a node with its content: the digest of its encoding with
    /// every child as a bare slot.
    pub fn endNode(self: *Graph, node: u32, content: Identity) void {
        const entry = &self.nodes.items[node];
        entry.child_len = @intCast(self.children.items.len - entry.child_start);
        entry.content = content;
    }

    /// The digest of a node of a resolved graph.
    pub fn digest(self: *const Graph, node: u32) Identity {
        return self.nodes.items[node].digest orelse
            invariant("recursive digest read a node of an unresolved graph", .{});
    }

    /// How a node of a resolved graph was settled.
    pub fn resolution(self: *const Graph, node: u32) Resolution {
        std.debug.assert(self.nodes.items[node].digest != null);
        return self.nodes.items[node].resolution;
    }

    /// Settle the digest of every node.
    pub fn resolve(self: *Graph, settled: *Settled, stats: ?*Stats) Allocator.Error!void {
        try self.positions.resize(self.gpa, self.nodes.items.len);
        @memset(self.positions.items, no_position);
        var next_index: u32 = 0;
        for (0..self.nodes.items.len) |start_index| {
            const start: u32 = @intCast(start_index);
            if (self.nodes.items[start].index != unvisited) continue;
            try self.visit(start, &next_index);
            try self.frames.append(self.gpa, .{ .node = start, .next_child = 0 });
            while (self.frames.items.len != 0) {
                const frame = &self.frames.items[self.frames.items.len - 1];
                const node = frame.node;
                const children = self.childrenOf(node);
                if (frame.next_child < children.len) {
                    const child = children[frame.next_child];
                    frame.next_child += 1;
                    const target = switch (child) {
                        .settled => continue,
                        .node => |target| target,
                    };
                    const target_entry = &self.nodes.items[target];
                    if (target_entry.index == unvisited) {
                        try self.visit(target, &next_index);
                        try self.frames.append(self.gpa, .{ .node = target, .next_child = 0 });
                    } else if (target_entry.on_stack) {
                        const entry = &self.nodes.items[node];
                        entry.lowlink = @min(entry.lowlink, target_entry.index);
                    }
                    continue;
                }
                _ = self.frames.pop();
                const entry = self.nodes.items[node];
                if (self.frames.items.len != 0) {
                    const parent = &self.nodes.items[self.frames.items[self.frames.items.len - 1].node];
                    parent.lowlink = @min(parent.lowlink, entry.lowlink);
                }
                if (entry.lowlink != entry.index) continue;
                self.members.clearRetainingCapacity();
                while (true) {
                    const member = self.scc_stack.pop() orelse
                        invariant("recursive digest popped past its component root", .{});
                    self.nodes.items[member].on_stack = false;
                    try self.members.append(self.gpa, member);
                    if (member == node) break;
                }
                try self.resolveComponent(settled, stats);
            }
        }
    }

    fn visit(self: *Graph, node: u32, next_index: *u32) Allocator.Error!void {
        const entry = &self.nodes.items[node];
        entry.index = next_index.*;
        entry.lowlink = next_index.*;
        entry.on_stack = true;
        next_index.* += 1;
        try self.scc_stack.append(self.gpa, node);
    }

    fn childrenOf(self: *const Graph, node: u32) []const Child {
        const entry = self.nodes.items[node];
        return self.children.items[entry.child_start..][0..entry.child_len];
    }

    /// The node a child names inside the component being resolved, if any.
    fn memberChild(self: *const Graph, child: Child) ?u32 {
        return switch (child) {
            .settled => null,
            .node => |target| if (self.positions.items[target] == no_position) null else target,
        };
    }

    /// The digest of a child outside the component being resolved.
    fn childDigest(self: *const Graph, child: Child) Identity {
        return switch (child) {
            .settled => |settled_digest| settled_digest,
            .node => |target| self.nodes.items[target].digest orelse
                invariant("recursive digest read a child before resolving it", .{}),
        };
    }

    fn finish(self: *Graph, node: u32, node_digest: Identity, how: Resolution) void {
        const entry = &self.nodes.items[node];
        if (entry.digest != null) invariant("recursive digest resolved a node twice", .{});
        entry.digest = node_digest;
        entry.resolution = how;
    }

    /// A node's content with every child as its digest.
    fn unfoldingKey(self: *const Graph, domain: []const u8, node: u32) Identity {
        var hasher = TypeDigestHasher.init();
        writeBytes(&hasher, domain);
        writeBytes(&hasher, "unfolding");
        hasher.update(&self.nodes.items[node].content);
        const children = self.childrenOf(node);
        writeU32(&hasher, @intCast(children.len));
        for (children) |child| hasher.update(&self.childDigest(child));
        return hasher.finalResult();
    }

    fn resolveComponent(self: *Graph, settled: *Settled, stats: ?*Stats) Allocator.Error!void {
        const members = self.members.items;
        if (members.len == 1) {
            const node = members[0];
            const self_edge = for (self.childrenOf(node)) |child| {
                if (child == .node and child.node == node) break true;
            } else false;
            if (!self_edge) {
                const unfolding = self.unfoldingKey(settled.domain, node);
                if (settled.unfoldings.get(unfolding)) |position_digest| {
                    self.finish(node, position_digest, .folded);
                } else {
                    self.finish(node, unfolding, .acyclic);
                }
                return;
            }
        }
        for (members, 0..) |node, pos| self.positions.items[node] = @intCast(pos);
        defer for (members) |node| {
            self.positions.items[node] = no_position;
        };
        try self.resolveCyclicComponent(settled, stats);
    }

    fn resolveCyclicComponent(self: *Graph, settled: *Settled, stats: ?*Stats) Allocator.Error!void {
        const members = self.members.items;
        const member_count = members.len;

        // Every member starts from its label: its content with every child
        // outside the component as its digest and every child inside it as
        // a bare marker.
        try self.labels.resize(self.gpa, member_count);
        try self.next_labels.resize(self.gpa, member_count);
        var labels = self.labels.items;
        var next_labels = self.next_labels.items;
        for (members, 0..) |node, pos| {
            var hasher = TypeDigestHasher.init();
            writeBytes(&hasher, "label");
            hasher.update(&self.nodes.items[node].content);
            for (self.childrenOf(node)) |child| {
                if (self.memberChild(child) != null) {
                    writeBytes(&hasher, "member");
                } else {
                    writeBytes(&hasher, "child");
                    hasher.update(&self.childDigest(child));
                }
            }
            labels[pos] = hasher.finalResult();
        }
        if (stats) |s| s.group_encodings += member_count;

        // Labels and in-component references determine everything below,
        // so a component of a shape reduced before takes that result.
        const shape = shape: {
            var hasher = TypeDigestHasher.init();
            writeBytes(&hasher, "shape");
            writeU32(&hasher, @intCast(member_count));
            for (members, 0..) |node, pos| {
                hasher.update(&labels[pos]);
                for (self.childrenOf(node)) |child| {
                    const target = self.memberChild(child) orelse continue;
                    writeU32(&hasher, self.positions.items[target]);
                }
                writeBytes(&hasher, "end-member");
            }
            break :shape hasher.finalResult();
        };
        if (settled.shapes.get(shape)) |recorded| {
            if (recorded.len != member_count) invariant("recursive digest shape recorded a different member count", .{});
            for (members, settled.shape_digests.items[recorded.start..][0..recorded.len]) |node, member_digest| {
                self.finish(node, member_digest, .cyclic);
            }
            return;
        }

        try self.paired.resize(self.gpa, member_count);
        if (try self.foldOntoSettledGroup(settled)) {
            try reserveShape(settled, member_count);
            for (members, self.paired.items) |node, member_rank| {
                self.finish(node, settled.rank(self.candidate_groups.items[0], member_rank).digest, .cyclic);
            }
            recordShape(settled, shape, self, members);
            return;
        }

        // Refine to the coarsest stable partition: each round relabels a
        // member by its label followed by the labels of its ordered
        // in-component children, until a round separates no members.
        const distinct_labels = &self.distinct_labels;
        distinct_labels.clearRetainingCapacity();
        for (labels) |label| try distinct_labels.put(self.gpa, label, no_position);
        var label_count: u32 = distinct_labels.count();
        while (true) {
            distinct_labels.clearRetainingCapacity();
            for (members, 0..) |node, pos| {
                var hasher = TypeDigestHasher.init();
                hasher.update(&labels[pos]);
                for (self.childrenOf(node)) |child| {
                    const target = self.memberChild(child) orelse continue;
                    hasher.update(&labels[self.positions.items[target]]);
                }
                next_labels[pos] = hasher.finalResult();
                try distinct_labels.put(self.gpa, next_labels[pos], no_position);
            }
            const next_count: u32 = distinct_labels.count();
            const stable = next_count == label_count;
            std.mem.swap([]Identity, &labels, &next_labels);
            label_count = next_count;
            if (stable) break;
        }

        // Group order: the reduced positions sorted by label.
        const block_count = label_count;
        try self.sorted_labels.resize(self.gpa, block_count);
        const sorted_labels = self.sorted_labels.items;
        {
            var it = distinct_labels.keyIterator();
            var next: usize = 0;
            while (it.next()) |label| : (next += 1) sorted_labels[next] = label.*;
            std.debug.assert(next == block_count);
        }
        std.mem.sort(Identity, sorted_labels, {}, identityLessThan);
        for (sorted_labels, 0..) |label, block_rank| {
            const slot = distinct_labels.getPtr(label) orelse unreachable; // sorted_labels holds exactly the keys of distinct_labels
            slot.* = @intCast(block_rank);
        }
        try self.member_ranks.resize(self.gpa, member_count);
        const member_ranks = self.member_ranks.items;
        try self.representatives.resize(self.gpa, block_count);
        const representatives = self.representatives.items;
        @memset(representatives, no_position);
        for (members, 0..) |node, pos| {
            const block_rank = distinct_labels.get(labels[pos]) orelse unreachable; // the final round put every member's label into distinct_labels
            member_ranks[pos] = block_rank;
            // The partition is stable, so any member represents its position.
            if (representatives[block_rank] == no_position) representatives[block_rank] = node;
        }

        // One group rendering: every reduced position in rank order with
        // in-group references written as ranks.
        var group_hasher = TypeDigestHasher.init();
        writeBytes(&group_hasher, settled.domain);
        writeBytes(&group_hasher, "group");
        writeU32(&group_hasher, block_count);
        for (representatives) |rep| {
            writeBytes(&group_hasher, "position");
            group_hasher.update(&self.nodes.items[rep].content);
            for (self.childrenOf(rep)) |child| {
                if (self.memberChild(child)) |target| {
                    writeBytes(&group_hasher, "rank");
                    writeU32(&group_hasher, member_ranks[self.positions.items[target]]);
                } else {
                    writeBytes(&group_hasher, "child");
                    group_hasher.update(&self.childDigest(child));
                }
            }
        }
        if (stats) |s| s.group_encodings += block_count;
        const group_digest = group_hasher.finalResult();
        try self.block_digests.resize(self.gpa, block_count);
        const block_digests = self.block_digests.items;
        for (block_digests, 0..) |*block_digest, block_rank| {
            var hasher = TypeDigestHasher.init();
            writeBytes(&hasher, settled.domain);
            writeBytes(&hasher, "group-member");
            hasher.update(&group_digest);
            writeU32(&hasher, @intCast(block_rank));
            block_digest.* = hasher.finalResult();
        }
        for (members, member_ranks) |node, block_rank| self.finish(node, block_digests[block_rank], .cyclic);

        // A rolled-out copy of a position unfolds to the position's one-step
        // encoding, so it folds to the position's digest.
        try self.unfolding_keys.resize(self.gpa, block_count);
        for (representatives, self.unfolding_keys.items) |rep, *key| key.* = self.unfoldingKey(settled.domain, rep);
        if (stats) |s| s.group_encodings += block_count;

        // Reserve everything before committing anything, so allocation
        // failure leaves `settled` as it was.
        const record_group = !settled.member_of.contains(block_digests[0]);
        try settled.unfoldings.ensureUnusedCapacity(settled.gpa, block_count);
        if (record_group) {
            try settled.member_of.ensureUnusedCapacity(settled.gpa, block_count);
            try settled.groups.ensureUnusedCapacity(settled.gpa, 1);
            try settled.ranks.ensureUnusedCapacity(settled.gpa, block_count);
            var child_count: usize = 0;
            for (representatives) |rep| child_count += self.childrenOf(rep).len;
            try settled.rank_children.ensureUnusedCapacity(settled.gpa, child_count);
        }
        try reserveShape(settled, member_count);

        for (self.unfolding_keys.items, block_digests) |key, block_digest| {
            const gop = settled.unfoldings.getOrPutAssumeCapacity(key);
            if (gop.found_existing) {
                // Position digests are intrinsic, so an equivalent position
                // settled earlier must agree.
                std.debug.assert(std.mem.eql(u8, gop.value_ptr, &block_digest));
            } else {
                gop.value_ptr.* = block_digest;
            }
        }
        if (record_group) {
            const group: u32 = @intCast(settled.groups.items.len);
            settled.groups.appendAssumeCapacity(.{ .rank_start = @intCast(settled.ranks.items.len), .rank_count = block_count });
            for (representatives, block_digests, 0..) |rep, block_digest, block_rank| {
                const child_start: u32 = @intCast(settled.rank_children.items.len);
                for (self.childrenOf(rep)) |child| {
                    settled.rank_children.appendAssumeCapacity(if (self.memberChild(child)) |target|
                        .{ .rank = member_ranks[self.positions.items[target]] }
                    else
                        .{ .settled = self.childDigest(child) });
                }
                settled.ranks.appendAssumeCapacity(.{
                    .digest = block_digest,
                    .content = self.nodes.items[rep].content,
                    .child_start = child_start,
                    .child_len = @intCast(settled.rank_children.items.len - child_start),
                });
                settled.member_of.putAssumeCapacityNoClobber(block_digest, .{ .group = group, .rank = @intCast(block_rank) });
            }
        }
        recordShape(settled, shape, self, members);
    }

    /// Pair the component coinductively against each settled group it
    /// refers into. On success the pairing group is `candidate_groups[0]`
    /// and `paired` holds each member's rank in it.
    fn foldOntoSettledGroup(self: *Graph, settled: *const Settled) Allocator.Error!bool {
        const members = self.members.items;
        self.candidate_groups.clearRetainingCapacity();
        for (members) |node| {
            for (self.childrenOf(node)) |child| {
                if (self.memberChild(child) != null) continue;
                const member_ref = settled.member_of.get(self.childDigest(child)) orelse continue;
                if (std.mem.findScalar(u32, self.candidate_groups.items, member_ref.group) == null) {
                    try self.candidate_groups.append(self.gpa, member_ref.group);
                }
            }
        }
        for (self.candidate_groups.items, 0..) |group, candidate| {
            const rank_count = settled.groups.items[group].rank_count;
            for (0..rank_count) |start_rank| {
                if (!try self.pairWithGroup(settled, group, @intCast(start_rank))) continue;
                self.candidate_groups.items[0] = self.candidate_groups.items[candidate];
                return true;
            }
        }
        return false;
    }

    fn pairWithGroup(self: *Graph, settled: *const Settled, group: u32, start_rank: u32) Allocator.Error!bool {
        const members = self.members.items;
        const paired = self.paired.items;
        @memset(paired, no_position);
        self.pending_pairs.clearRetainingCapacity();
        try self.pending_pairs.append(self.gpa, .{ .position = 0, .rank = start_rank });
        while (self.pending_pairs.pop()) |pair| {
            if (paired[pair.position] != no_position) {
                if (paired[pair.position] != pair.rank) return false;
                continue;
            }
            paired[pair.position] = pair.rank;
            const node = members[pair.position];
            const expected = settled.rank(group, pair.rank);
            if (!std.mem.eql(u8, &self.nodes.items[node].content, &expected.content)) return false;
            const actual_children = self.childrenOf(node);
            const expected_children = settled.rankChildren(expected);
            if (actual_children.len != expected_children.len) return false;
            for (actual_children, expected_children) |actual, expected_child| {
                if (self.memberChild(actual)) |target| {
                    switch (expected_child) {
                        .rank => |child_rank| try self.pending_pairs.append(self.gpa, .{ .position = self.positions.items[target], .rank = child_rank }),
                        .settled => return false,
                    }
                } else {
                    const actual_digest = self.childDigest(actual);
                    const expected_digest = switch (expected_child) {
                        .rank => |child_rank| settled.rank(group, child_rank).digest,
                        .settled => |settled_digest| settled_digest,
                    };
                    if (!std.mem.eql(u8, &actual_digest, &expected_digest)) return false;
                }
            }
        }
        return std.mem.findScalar(u32, paired, no_position) == null;
    }
};

fn reserveShape(settled: *Settled, member_count: usize) Allocator.Error!void {
    try settled.shapes.ensureUnusedCapacity(settled.gpa, 1);
    try settled.shape_digests.ensureUnusedCapacity(settled.gpa, member_count);
}

fn recordShape(settled: *Settled, shape: Identity, graph: *const Graph, members: []const u32) void {
    settled.shapes.putAssumeCapacityNoClobber(shape, .{
        .start = @intCast(settled.shape_digests.items.len),
        .len = @intCast(members.len),
    });
    for (members) |node| settled.shape_digests.appendAssumeCapacity(graph.digest(node));
}

fn identityLessThan(_: void, lhs: Identity, rhs: Identity) bool {
    return std.mem.order(u8, &lhs, &rhs) == .lt;
}

/// Hash a length-prefixed byte string.
pub fn writeBytes(hasher: *TypeDigestHasher, bytes: []const u8) void {
    writeU32(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

/// Hash a little-endian u32.
pub fn writeU32(hasher: *TypeDigestHasher, value: u32) void {
    const bytes: [4]u8 = .{ @truncate(value), @truncate(value >> 8), @truncate(value >> 16), @truncate(value >> 24) };
    hasher.update(&bytes);
}

fn testContent(tag: []const u8) Identity {
    var hasher = TypeDigestHasher.init();
    writeBytes(&hasher, tag);
    return hasher.finalResult();
}

/// Adds a node whose children are all nodes of `graph`.
fn testNode(graph: *Graph, node: u32, tag: []const u8, children: []const Child) Allocator.Error!void {
    graph.beginNode(node);
    for (children) |child| try graph.putChild(child);
    graph.endNode(node, testContent(tag));
}

test "a knot whose references lead into a separately tied copy digests as that copy" {
    const gpa = std.testing.allocator;
    var settled = Settled.init(gpa, "test");
    defer settled.deinit();
    var graph = Graph.init(gpa);
    defer graph.deinit();

    // c = pair(c, c); b1 = pair(b2, b1); b2 = pair(b1, c). All three are the
    // same infinite type, but {b1, b2} is a component of its own whose
    // references lead into c's.
    const c = try graph.addNode();
    const b1 = try graph.addNode();
    const b2 = try graph.addNode();
    try testNode(&graph, b1, "pair", &.{ .{ .node = b2 }, .{ .node = b1 } });
    try testNode(&graph, b2, "pair", &.{ .{ .node = b1 }, .{ .node = c } });
    try testNode(&graph, c, "pair", &.{ .{ .node = c }, .{ .node = c } });
    try graph.resolve(&settled, null);
    try std.testing.expectEqual(graph.digest(c), graph.digest(b1));
    try std.testing.expectEqual(graph.digest(c), graph.digest(b2));

    // The same type alone in a fresh reduction, with nothing settled.
    var fresh_settled = Settled.init(gpa, "test");
    defer fresh_settled.deinit();
    var fresh = Graph.init(gpa);
    defer fresh.deinit();
    const solo = try fresh.addNode();
    try testNode(&fresh, solo, "pair", &.{ .{ .node = solo }, .{ .node = solo } });
    try fresh.resolve(&fresh_settled, null);
    try std.testing.expectEqual(fresh.digest(solo), graph.digest(b1));
}

test "a rolled-out copy ending in a settled knot digests as the knot" {
    const gpa = std.testing.allocator;
    var settled = Settled.init(gpa, "test");
    defer settled.deinit();
    var graph = Graph.init(gpa);
    defer graph.deinit();

    // list = List(node); node = [Leaf, Branch(list)].
    const node = try graph.addNode();
    const list = try graph.addNode();
    try testNode(&graph, node, "union", &.{.{ .node = list }});
    try testNode(&graph, list, "list", &.{.{ .node = node }});
    try graph.resolve(&settled, null);
    const node_digest = graph.digest(node);
    const list_digest = graph.digest(list);

    // A later reduction rolls the knot out twice before reaching the
    // settled copy.
    graph.reset();
    const outer = try graph.addNode();
    const outer_list = try graph.addNode();
    const inner = try graph.addNode();
    try testNode(&graph, outer, "union", &.{.{ .node = outer_list }});
    try testNode(&graph, outer_list, "list", &.{.{ .node = inner }});
    try testNode(&graph, inner, "union", &.{.{ .settled = list_digest }});
    try graph.resolve(&settled, null);
    try std.testing.expectEqual(node_digest, graph.digest(outer));
    try std.testing.expectEqual(list_digest, graph.digest(outer_list));
    try std.testing.expectEqual(node_digest, graph.digest(inner));
    try std.testing.expectEqual(Resolution.folded, graph.resolution(inner));
}

test "a cyclic component spelling a settled group through some references pairs with it" {
    const gpa = std.testing.allocator;
    var settled = Settled.init(gpa, "test");
    defer settled.deinit();
    var graph = Graph.init(gpa);
    defer graph.deinit();

    // tree = [Leaf, Node(record)]; record = { left: tree, right: tree }.
    const tree = try graph.addNode();
    const record = try graph.addNode();
    try testNode(&graph, tree, "tree", &.{.{ .node = record }});
    try testNode(&graph, record, "record", &.{ .{ .node = tree }, .{ .node = tree } });
    try graph.resolve(&settled, null);
    const tree_digest = graph.digest(tree);
    const record_digest = graph.digest(record);

    // Its own knot through `left`, the settled copy through `right`.
    graph.reset();
    const copy_tree = try graph.addNode();
    const copy_record = try graph.addNode();
    try testNode(&graph, copy_tree, "tree", &.{.{ .node = copy_record }});
    try testNode(&graph, copy_record, "record", &.{ .{ .node = copy_tree }, .{ .settled = tree_digest } });
    try graph.resolve(&settled, null);
    try std.testing.expectEqual(tree_digest, graph.digest(copy_tree));
    try std.testing.expectEqual(record_digest, graph.digest(copy_record));
    try std.testing.expectEqual(Resolution.cyclic, graph.resolution(copy_tree));
}

test "a cyclic component that refers into an inequivalent settled group stays its own type" {
    const gpa = std.testing.allocator;
    var settled = Settled.init(gpa, "test");
    defer settled.deinit();
    var graph = Graph.init(gpa);
    defer graph.deinit();

    const tree = try graph.addNode();
    const record = try graph.addNode();
    try testNode(&graph, tree, "tree", &.{.{ .node = record }});
    try testNode(&graph, record, "record", &.{ .{ .node = tree }, .{ .node = tree } });
    try graph.resolve(&settled, null);
    const tree_digest = graph.digest(tree);

    // forest = [Empty, More(forest, tree)]: a recursive type that holds the
    // settled tree without being one.
    graph.reset();
    const forest = try graph.addNode();
    try testNode(&graph, forest, "tree", &.{ .{ .node = forest }, .{ .settled = tree_digest } });
    try graph.resolve(&settled, null);
    try std.testing.expect(!std.mem.eql(u8, &tree_digest, &graph.digest(forest)));
    try std.testing.expect(settled.member_of.get(graph.digest(forest)).?.group != settled.member_of.get(tree_digest).?.group);
}

test "a knot digests the same however far it is rolled out" {
    const gpa = std.testing.allocator;
    var digests: [3]Identity = undefined;
    for (&digests, 0..) |*out, rolls| {
        var settled = Settled.init(gpa, "test");
        defer settled.deinit();
        var graph = Graph.init(gpa);
        defer graph.deinit();
        // A cycle of `rolls + 1` list nodes over one union node per roll.
        const count: u32 = @intCast(rolls + 1);
        var nodes: [6]u32 = undefined;
        for (0..2 * count) |i| nodes[i] = try graph.addNode();
        for (0..count) |i| {
            const next = nodes[2 * ((i + 1) % count)];
            try testNode(&graph, nodes[2 * i], "union", &.{.{ .node = nodes[2 * i + 1] }});
            try testNode(&graph, nodes[2 * i + 1], "list", &.{.{ .node = next }});
        }
        try graph.resolve(&settled, null);
        out.* = graph.digest(nodes[0]);
    }
    try std.testing.expectEqual(digests[0], digests[1]);
    try std.testing.expectEqual(digests[0], digests[2]);
}
