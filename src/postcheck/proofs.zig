//! Graph-owned proof certificates and exact, allocation-free mutation recording.
//! Reverse dependencies retire only proofs reached from changed producer nodes.
const std = @import("std");
const Allocator = std.mem.Allocator;

/// Graph-owned subscriptions to checked writes. A disconnected node has no
/// subscribers. Writes visit only proofs which actually read the changed node.
pub fn Changes(comptime Node: type) type {
    return struct {
        const Self = @This();
        /// Intrusive subscription owned by a committed proof's dependency array.
        pub const Watch = struct {
            subscribed: bool = false,
            previous: ?*Watch = null,
            next: ?*Watch = null,
            context: *anyopaque,
            notify: *const fn (*anyopaque, u64) void,
        };
        records: std.ArrayList(?*Watch) = .empty,
        epoch: u64 = 0,

        /// Reserve node slots before the producer commits their identities.
        pub fn ensureTotalCapacityPrecise(self: *Self, gpa: Allocator, count: usize) Allocator.Error!void {
            try self.records.ensureTotalCapacityPrecise(gpa, count);
        }
        /// Reserve additional node slots before a potentially failing write.
        pub fn ensureUnusedCapacity(self: *Self, gpa: Allocator, count: usize) Allocator.Error!void {
            try self.records.ensureUnusedCapacity(gpa, count);
        }
        /// New nodes have no existing readers.
        pub fn appendAssumeCapacity(self: *Self) void {
            self.records.appendAssumeCapacity(null);
        }
        /// The graph's query owners must finish before its node identities end.
        pub fn clearRetainingCapacity(self: *Self) void {
            for (self.records.items) |head| std.debug.assert(head == null);
            self.records.clearRetainingCapacity();
            self.epoch = 0;
        }
        /// Release the graph after its query owners have detached.
        pub fn deinit(self: *Self, gpa: Allocator) void {
            self.clearRetainingCapacity();
            self.records.deinit(gpa);
        }
        /// Register already allocated dependency storage. No producer write allocates.
        pub fn subscribe(self: *Self, node: Node, watch: *Watch) void {
            std.debug.assert(!watch.subscribed);
            watch.subscribed = true;
            const head = &self.records.items[@backingInt(node)];
            watch.previous = null;
            watch.next = head.*;
            if (head.*) |next| next.previous = watch;
            head.* = watch;
        }
        /// Detach in constant time, including after the node column has grown.
        pub fn unsubscribe(self: *Self, node: Node, watch: *Watch) void {
            if (!watch.subscribed) return;
            watch.subscribed = false;
            if (watch.previous) |previous| previous.next = watch.next else self.records.items[@backingInt(node)] = watch.next;
            if (watch.next) |next| next.previous = watch.previous;
        }
        /// Queue invalidation only; active queries may still borrow certificates.
        pub fn mark(self: *Self, node: Node) void {
            self.epoch += 1;
            while (self.records.items[@backingInt(node)]) |watch| {
                // Detach before notifying: invalidating a proof also detaches
                // its other node dependencies, possibly on this same node.
                self.unsubscribe(node, watch);
                watch.notify(watch.context, self.epoch);
            }
        }
    };
}

/// Own completed certificates for one graph and one producer scope. Every
/// dependency has a reverse link, removed in O(1) when its proof is retired.
pub fn Cache(comptime Key: type, comptime Node: type) type {
    return struct {
        const Self = @This();
        /// A directly read normalized node or an earlier completed certificate.
        pub const Dependency = union(enum) { node: Node, proof: *Proof };
        const MutationIndex = Changes(Node);
        const Link = struct { dependency: Dependency, owner: *Proof, slot: usize, watch: MutationIndex.Watch = undefined };
        // Heap-owned so moving a body-local cache does not move notification state.
        const Notifications = struct {
            head: ?*Proof = null,
            last_epoch: u64 = 0,
            invalidations: usize = 0,
            changed_nodes: usize = 0,
            edges: usize = 0,
        };
        /// A group settled together, with its proven and reverse dependencies.
        pub const Proof = struct {
            valid: bool = true,
            pending_next: ?*Proof = null,
            notifications: *Notifications,
            changes: *MutationIndex,
            slot: usize,
            dependencies: []Link,
            keys: []Key,
            dependents: std.ArrayList(*Link) = .empty,
            fn invalidate(self: *Proof) void {
                if (!self.valid) return;
                self.valid = false;
                // The certificate remains owned until refresh, but no further
                // producer writes can make it more invalid. Unsubscribe now so
                // long mutation batches never repeatedly visit a dirty proof.
                for (self.dependencies) |*link| switch (link.dependency) {
                    .node => |node| self.changes.unsubscribe(node, &link.watch),
                    .proof => {},
                };
                self.pending_next = self.notifications.head;
                self.notifications.head = self;
                self.notifications.invalidations += 1;
            }
            fn changed(context: *anyopaque, epoch: u64) void {
                const self: *Proof = @ptrCast(@alignCast(context));
                if (self.notifications.last_epoch != epoch) {
                    self.notifications.last_epoch = epoch;
                    self.notifications.changed_nodes += 1;
                }
                self.notifications.edges += 1;
                self.invalidate();
            }
        };
        /// One settled outcome and its justification.
        pub const Proven = struct { value: u2, proof: *Proof };
        /// An outcome prepared for an atomic commit.
        pub const Entry = struct { key: Key, value: u2 };
        proven: std.AutoHashMapUnmanaged(Key, Proven) = .empty,
        changes: ?*MutationIndex = null,
        notifications: ?*Notifications = null,
        groups: std.ArrayList(*Proof) = .empty,
        pending: std.ArrayList(*Proof) = .empty,
        epoch: u64 = 0,
        invalidations: usize = 0,
        changed_nodes: usize = 0,
        invalidation_edges: usize = 0,
        dependency_bytes: usize = 0,
        dependent_capacity_bytes: usize = 0,

        /// Release certificates, tables and retained capacity.
        pub fn deinit(self: *Self, gpa: Allocator) void {
            self.clear(gpa);
            self.proven.deinit(gpa);
            if (self.notifications) |state| gpa.destroy(state);
            self.groups.deinit(gpa);
            self.pending.deinit(gpa);
        }
        /// End the producer scope and release every certificate and reverse list.
        pub fn clear(self: *Self, gpa: Allocator) void {
            for (self.groups.items) |proof| for (proof.dependencies) |*link| self.unlink(link);
            for (self.groups.items) |proof| self.freeProof(gpa, proof);
            if (self.notifications) |state| state.head = null;
            self.groups.clearRetainingCapacity();
            self.pending.clearRetainingCapacity();
            self.proven.clearRetainingCapacity();
            self.dependency_bytes = 0;
            self.dependent_capacity_bytes = 0;
        }
        fn freeProof(_: *Self, gpa: Allocator, proof: *Proof) void {
            proof.dependents.deinit(gpa);
            gpa.free(proof.dependencies);
            gpa.free(proof.keys);
            gpa.destroy(proof);
        }
        /// Drain exact producer notifications outside a query. No unrelated
        /// node or another analysis owner's mutation history is inspected.
        pub fn refresh(self: *Self, gpa: Allocator, epoch: u64, graph: anytype) void {
            const changes = graph.proofChanges();
            std.debug.assert(self.groups.items.len == 0 or self.changes == changes);
            self.changes = changes;
            self.epoch = epoch;
            const state = self.notifications orelse return;
            while (state.head) |proof| {
                state.head = proof.pending_next;
                self.pending.appendAssumeCapacity(proof);
                for (proof.dependents.items) |link| {
                    state.edges += 1;
                    link.owner.invalidate();
                }
            }
            self.invalidations = state.invalidations;
            self.changed_nodes = state.changed_nodes;
            self.invalidation_edges = state.edges;
            // Unlink everything before freeing any referenced proof.
            for (self.pending.items) |proof| for (proof.dependencies) |*link| self.unlink(link);
            for (self.pending.items) |proof| {
                for (proof.keys) |key| {
                    const removed = self.proven.remove(key);
                    std.debug.assert(removed);
                }
                _ = self.groups.swapRemove(proof.slot);
                if (proof.slot < self.groups.items.len) self.groups.items[proof.slot].slot = proof.slot;
                self.dependency_bytes -= proof.dependencies.len * @sizeOf(Link) + proof.keys.len * @sizeOf(Key);
                self.dependent_capacity_bytes -= proof.dependents.capacity * @sizeOf(*Link);
                self.freeProof(gpa, proof);
            }
            self.pending.clearRetainingCapacity();
        }
        fn unlink(self: *Self, link: *Link) void {
            switch (link.dependency) {
                .node => |node| self.changes.?.unsubscribe(node, &link.watch),
                .proof => |proof| {
                    const list = &proof.dependents;
                    _ = list.swapRemove(link.slot);
                    if (link.slot < list.items.len) list.items[link.slot].slot = link.slot;
                },
            }
        }
        /// Find a proven value after the caller has refreshed dependencies.
        pub fn get(self: *Self, key: Key) ?Proven {
            return self.proven.get(key);
        }
        /// Commit only after convergence. Allocation failure rolls back every
        /// reverse link and exposes none of the new proven.
        pub fn commit(self: *Self, gpa: Allocator, entries: []const Entry, dependencies: []const Dependency) Allocator.Error!void {
            if (entries.len == 0) return;
            try self.proven.ensureUnusedCapacity(gpa, @intCast(entries.len));
            try self.groups.ensureUnusedCapacity(gpa, 1);
            try self.pending.ensureTotalCapacity(gpa, self.groups.items.len + 1);
            if (self.notifications == null) {
                const state = try gpa.create(Notifications);
                state.* = .{};
                self.notifications = state;
            }
            const proof = try gpa.create(Proof);
            errdefer gpa.destroy(proof);
            const links = try gpa.alloc(Link, dependencies.len);
            errdefer gpa.free(links);
            const keys = try gpa.alloc(Key, entries.len);
            errdefer gpa.free(keys);
            proof.* = .{ .notifications = self.notifications.?, .changes = self.changes.?, .slot = self.groups.items.len, .dependencies = links, .keys = keys };
            var linked: usize = 0;
            errdefer for (links[0..linked]) |*link| self.unlink(link);
            for (dependencies, links) |dependency, *link| {
                link.* = .{ .dependency = dependency, .owner = proof, .slot = 0 };
                switch (dependency) {
                    .node => |node| {
                        link.watch = .{ .context = proof, .notify = Proof.changed };
                        self.changes.?.subscribe(node, &link.watch);
                    },
                    .proof => |earlier| {
                        const capacity = earlier.dependents.capacity;
                        link.slot = earlier.dependents.items.len;
                        try earlier.dependents.append(gpa, link);
                        self.dependent_capacity_bytes += (earlier.dependents.capacity - capacity) * @sizeOf(*Link);
                    },
                }
                linked += 1;
            }
            for (entries, keys) |entry, *key| {
                std.debug.assert(!self.proven.contains(entry.key));
                key.* = entry.key;
                self.proven.putAssumeCapacity(entry.key, .{ .value = entry.value, .proof = proof });
            }
            self.groups.appendAssumeCapacity(proof);
            self.dependency_bytes += links.len * @sizeOf(Link) + keys.len * @sizeOf(Key);
        }
        /// Table payload and certificate bytes, excluding allocator metadata,
        /// alignment padding and hash-table control bytes.
        pub fn retainedPayloadBytes(self: *const Self) usize {
            return self.proven.capacity() * (@sizeOf(Key) + @sizeOf(Proven)) +
                (if (self.notifications != null) @as(usize, @sizeOf(Notifications)) else 0) +
                (self.groups.capacity + self.pending.capacity) * @sizeOf(*Proof) +
                self.groups.items.len * @sizeOf(Proof) + self.dependency_bytes +
                self.dependent_capacity_bytes;
        }
    };
}

test "issue 12616 proof dependencies invalidate transitively without dropping disjoint proven" {
    const Node = enum(u32) { a, b, c };
    const Store = struct {
        changes: Changes(Node) = .{},
        /// Read exact test mutation events, independent of proof state.
        pub fn proofChanges(self: *@This()) *Changes(Node) {
            return &self.changes;
        }
    };
    const gpa = std.testing.allocator;
    const Proofs = Cache(u32, Node);
    var graph: Store = .{};
    defer graph.changes.deinit(gpa);
    try graph.changes.ensureUnusedCapacity(gpa, 3);
    for (0..3) |_| graph.changes.appendAssumeCapacity();
    var proofs: Proofs = .{};
    defer proofs.deinit(gpa);
    proofs.refresh(gpa, graph.changes.epoch, &graph);
    try proofs.commit(gpa, &.{.{ .key = 0, .value = 1 }}, &.{.{ .node = .a }});
    try proofs.commit(gpa, &.{.{ .key = 1, .value = 1 }}, &.{.{ .proof = proofs.get(0).?.proof }});
    try proofs.commit(gpa, &.{.{ .key = 2, .value = 0 }}, &.{.{ .node = .c }});
    for (0..100) |_| graph.changes.mark(.b);
    proofs.refresh(gpa, graph.changes.epoch, &graph);
    try std.testing.expectEqual(@as(usize, 3), proofs.proven.count());
    try std.testing.expectEqual(@as(usize, 0), proofs.changed_nodes);
    try std.testing.expectEqual(@as(usize, 0), proofs.invalidation_edges);
    graph.changes.mark(.a);
    proofs.refresh(gpa, graph.changes.epoch, &graph);
    try std.testing.expect(proofs.get(0) == null and proofs.get(1) == null);
    try std.testing.expectEqual(@as(u2, 0), proofs.get(2).?.value);
    try std.testing.expectEqual(@as(usize, 1), proofs.groups.items.len);
    try std.testing.expectEqual(@as(usize, 1), proofs.changed_nodes);
    proofs.clear(gpa);
    try std.testing.expectEqual(@as(usize, 0), proofs.proven.count());
    try std.testing.expectEqual(@as(usize, 0), proofs.groups.items.len);
}

test "proof observers ignore disjoint graph writes and survive cache movement and node growth" {
    const Node = enum(u32) { _ };
    const Graph = struct {
        changes: Changes(Node) = .{},
        pub fn proofChanges(self: *@This()) *Changes(Node) {
            return &self.changes;
        }
    };
    const gpa = std.testing.allocator;
    var graph: Graph = .{};
    defer graph.changes.deinit(gpa);
    try graph.changes.ensureUnusedCapacity(gpa, 1);
    const first_node: Node = @fromBackingInt(@intCast(graph.changes.records.items.len));
    graph.changes.appendAssumeCapacity();
    var cache: Cache(u32, Node) = .{};
    cache.refresh(gpa, 0, &graph);
    try cache.commit(gpa, &.{.{ .key = 1, .value = 1 }}, &.{.{ .node = first_node }});
    var moved = cache;
    cache = .{};
    defer moved.deinit(gpa);
    try graph.changes.ensureUnusedCapacity(gpa, 4096);
    for (0..4096) |_| graph.changes.appendAssumeCapacity();
    for (1..4097) |i| {
        graph.changes.mark(@fromBackingInt(@intCast(i)));
        moved.refresh(gpa, graph.changes.epoch, &graph);
        try std.testing.expectEqual(@as(u2, 1), moved.get(1).?.value);
    }
    try std.testing.expectEqual(@as(usize, 0), moved.changed_nodes);
    try std.testing.expectEqual(@as(usize, 0), moved.invalidation_edges);
    graph.changes.mark(first_node);
    moved.refresh(gpa, graph.changes.epoch, &graph);
    try std.testing.expect(moved.get(1) == null);
    try std.testing.expectEqual(@as(usize, 1), moved.changed_nodes);
    try std.testing.expectEqual(@as(usize, 1), moved.invalidation_edges);
}

fn allocationFailureFixture(gpa: Allocator) error{ OutOfMemory, TestExpectedEqual }!void {
    const Node = enum(u32) { a, b };
    const Graph = struct {
        changes: Changes(Node) = .{},
        pub fn proofChanges(self: *@This()) *Changes(Node) {
            return &self.changes;
        }
    };
    var graph: Graph = .{};
    defer graph.changes.deinit(gpa);
    try graph.changes.ensureUnusedCapacity(gpa, 2);
    graph.changes.appendAssumeCapacity();
    graph.changes.appendAssumeCapacity();
    var cache: Cache(u32, Node) = .{};
    defer cache.deinit(gpa);
    cache.refresh(gpa, 0, &graph);
    try cache.commit(gpa, &.{.{ .key = 1, .value = 1 }}, &.{.{ .node = .a }});
    try cache.commit(gpa, &.{.{ .key = 2, .value = 1 }}, &.{ .{ .node = .b }, .{ .proof = cache.get(1).?.proof } });
    graph.changes.mark(.a);
    cache.refresh(gpa, graph.changes.epoch, &graph);
    try std.testing.expectEqual(@as(usize, 0), cache.proven.count());
}

test "proof publication allocation failures detach every graph subscription" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, allocationFailureFixture, .{});
}

test "a dirty proof unsubscribes before later writes in the same batch" {
    const a = std.testing.allocator;
    const Node = enum(u32) { a, b, c };
    const Graph = struct {
        changes: Changes(Node) = .{},
        fn proofChanges(self: *@This()) *Changes(Node) {
            return &self.changes;
        }
    };
    var graph = Graph{};
    defer graph.changes.deinit(a);
    try graph.changes.ensureTotalCapacityPrecise(a, 3);
    for (0..3) |_| graph.changes.appendAssumeCapacity();
    var cache = Cache(u32, Node){};
    defer cache.deinit(a);
    cache.refresh(a, 0, &graph);
    try cache.commit(a, &.{.{ .key = 1, .value = 1 }}, &.{ .{ .node = .a }, .{ .node = .b } });
    try cache.commit(a, &.{.{ .key = 2, .value = 2 }}, &.{ .{ .node = .a }, .{ .node = .a } });
    try cache.commit(a, &.{.{ .key = 3, .value = 3 }}, &.{.{ .node = .c }});
    graph.changes.mark(.a);
    for (0..4096) |_| {
        graph.changes.mark(.a);
        graph.changes.mark(.b);
    }
    cache.refresh(a, graph.changes.epoch, &graph);
    try std.testing.expectEqual(@as(usize, 2), cache.invalidation_edges);
    try std.testing.expectEqual(@as(usize, 2), cache.invalidations);
    try std.testing.expect(cache.get(1) == null);
    try std.testing.expect(cache.get(2) == null);
    try std.testing.expect(cache.get(3) != null);
}
