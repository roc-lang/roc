//! Graph-owned proof certificates and exact, allocation-free mutation recording.
//! Reverse dependencies retire only proofs reached from changed producer nodes.
const std = @import("std");
const Allocator = std.mem.Allocator;

/// A bounded mutation index for a graph with dense u32-backed node identities.
/// Each node occurs once in a list ordered by its most recent mutation. Moving
/// it to the front records another write without accumulating mutation history.
pub fn Changes(comptime Node: type) type {
    return struct {
        const Self = @This();
        const none = std.math.maxInt(u32);
        const Record = struct { previous: u32 = none, next: u32 = none, version: u64 = 0 };
        records: std.ArrayList(Record) = .empty,
        head: u32 = none,
        epoch: u64 = 0,

        /// Reserve node slots before the producer publishes their identities.
        pub fn ensureTotalCapacityPrecise(self: *Self, gpa: Allocator, count: usize) Allocator.Error!void {
            try self.records.ensureTotalCapacityPrecise(gpa, count);
        }
        /// Reserve additional node slots before a potentially failing write.
        pub fn ensureUnusedCapacity(self: *Self, gpa: Allocator, count: usize) Allocator.Error!void {
            try self.records.ensureUnusedCapacity(gpa, count);
        }
        /// A new disconnected node changes no existing proof.
        pub fn appendAssumeCapacity(self: *Self) void {
            self.records.appendAssumeCapacity(.{});
        }
        /// Begin an unrelated graph; all old identities and observers are gone.
        pub fn clearRetainingCapacity(self: *Self) void {
            self.records.clearRetainingCapacity();
            self.head = none;
            self.epoch = 0;
        }
        /// Release the per-node index.
        pub fn deinit(self: *Self, gpa: Allocator) void {
            self.records.deinit(gpa);
        }
        /// Record a producer-authored change. This cannot allocate or fail.
        pub fn mark(self: *Self, node: Node) void {
            const index = @backingInt(node);
            const record = &self.records.items[index];
            if (record.version != 0) {
                if (record.previous == none) self.head = record.next else self.records.items[record.previous].next = record.next;
                if (record.next != none) self.records.items[record.next].previous = record.previous;
            }
            self.epoch += 1;
            record.* = .{ .next = self.head, .version = self.epoch };
            if (self.head != none) self.records.items[self.head].previous = index;
            self.head = index;
        }
        /// Visits each node changed since an observer's epoch at most once.
        pub const Iterator = struct {
            records: []const Record,
            cursor: u32,
            epoch: u64,
            /// The next changed identity; iteration stops before older nodes.
            pub fn next(self: *Iterator) ?Node {
                if (self.cursor == none) return null;
                const index = self.cursor;
                const record = self.records[index];
                if (record.version <= self.epoch) return null;
                self.cursor = record.next;
                return @fromBackingInt(index);
            }
        };
        /// Read the exact mutation suffix without changing another observer.
        pub fn since(self: *const Self, epoch: u64) Iterator {
            return .{ .records = self.records.items, .cursor = self.head, .epoch = epoch };
        }
    };
}

/// Own completed certificates for one graph and one producer scope. Every
/// dependency has a reverse link, removed in O(1) when its proof is retired.
pub fn Cache(comptime Key: type, comptime Node: type) type {
    return struct {
        const Self = @This();
        /// A directly read canonical node or an earlier completed certificate.
        pub const Dependency = union(enum) { node: Node, proof: *Proof };
        const Link = struct { dependency: Dependency, owner: *Proof, slot: usize };
        /// A group settled together, with its facts and reverse dependencies.
        pub const Proof = struct {
            valid: bool = true,
            slot: usize,
            dependencies: []Link,
            keys: []Key,
            dependents: std.ArrayList(*Link) = .empty,
        };
        /// One settled outcome and its justification.
        pub const Fact = struct { value: u2, proof: *Proof };
        /// An outcome prepared for atomic publication.
        pub const Entry = struct { key: Key, value: u2 };
        facts: std.AutoHashMapUnmanaged(Key, Fact) = .empty,
        watchers: std.AutoHashMapUnmanaged(Node, std.ArrayList(*Link)) = .empty,
        groups: std.ArrayList(*Proof) = .empty,
        pending: std.ArrayList(*Proof) = .empty,
        epoch: u64 = 0,
        invalidations: usize = 0,
        changed_nodes: usize = 0,
        invalidation_edges: usize = 0,
        dependency_bytes: usize = 0,
        dependent_capacity_bytes: usize = 0,
        watcher_capacity_bytes: usize = 0,

        /// Release certificates, tables and retained capacity.
        pub fn deinit(self: *Self, gpa: Allocator) void {
            self.clear(gpa);
            self.facts.deinit(gpa);
            self.watchers.deinit(gpa);
            self.groups.deinit(gpa);
            self.pending.deinit(gpa);
        }
        /// End the producer scope and release every certificate and reverse list.
        pub fn clear(self: *Self, gpa: Allocator) void {
            for (self.groups.items) |proof| self.freeProof(gpa, proof);
            var lists = self.watchers.valueIterator();
            while (lists.next()) |list| list.deinit(gpa);
            self.groups.clearRetainingCapacity();
            self.pending.clearRetainingCapacity();
            self.watchers.clearRetainingCapacity();
            self.facts.clearRetainingCapacity();
            self.dependency_bytes = 0;
            self.dependent_capacity_bytes = 0;
            self.watcher_capacity_bytes = 0;
        }
        fn freeProof(_: *Self, gpa: Allocator, proof: *Proof) void {
            proof.dependents.deinit(gpa);
            gpa.free(proof.dependencies);
            gpa.free(proof.keys);
            gpa.destroy(proof);
        }
        fn invalidate(self: *Self, proof: *Proof) void {
            if (!proof.valid) return;
            proof.valid = false;
            self.pending.appendAssumeCapacity(proof);
            self.invalidations += 1;
        }
        /// Call only outside a query: a running recorder may borrow proofs.
        /// Work is proportional to changed nodes and affected proof edges.
        pub fn refresh(self: *Self, gpa: Allocator, epoch: u64, graph: anytype) void {
            if (self.epoch == epoch) return;
            if (self.groups.items.len == 0) {
                self.epoch = epoch;
                return;
            }
            var changes = graph.analysisChangesSince(self.epoch);
            self.epoch = epoch;
            while (changes.next()) |node| {
                self.changed_nodes += 1;
                if (self.watchers.get(node)) |links| for (links.items) |link| {
                    self.invalidation_edges += 1;
                    self.invalidate(link.owner);
                };
            }
            var cursor: usize = 0;
            while (cursor < self.pending.items.len) : (cursor += 1) {
                for (self.pending.items[cursor].dependents.items) |link| {
                    self.invalidation_edges += 1;
                    self.invalidate(link.owner);
                }
            }
            // Unlink everything before freeing any referenced proof.
            for (self.pending.items) |proof| for (proof.dependencies) |*link| self.unlink(gpa, link);
            for (self.pending.items) |proof| {
                for (proof.keys) |key| {
                    const removed = self.facts.remove(key);
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
        fn unlink(self: *Self, gpa: Allocator, link: *Link) void {
            const list = switch (link.dependency) {
                .node => |node| self.watchers.getPtr(node).?,
                .proof => |proof| &proof.dependents,
            };
            _ = list.swapRemove(link.slot);
            if (link.slot < list.items.len) list.items[link.slot].slot = link.slot;
            if (link.dependency == .node and list.items.len == 0) {
                self.watcher_capacity_bytes -= list.capacity * @sizeOf(*Link);
                list.deinit(gpa);
                const removed = self.watchers.remove(link.dependency.node);
                std.debug.assert(removed);
            }
        }
        /// Find a fact after the caller has refreshed dependencies.
        pub fn get(self: *Self, key: Key) ?Fact {
            return self.facts.get(key);
        }
        /// Publish only after convergence. Allocation failure rolls back every
        /// reverse link and exposes none of the new facts.
        pub fn publish(self: *Self, gpa: Allocator, entries: []const Entry, dependencies: []const Dependency) Allocator.Error!void {
            if (entries.len == 0) return;
            try self.facts.ensureUnusedCapacity(gpa, @intCast(entries.len));
            try self.groups.ensureUnusedCapacity(gpa, 1);
            try self.pending.ensureTotalCapacity(gpa, self.groups.items.len + 1);
            const proof = try gpa.create(Proof);
            errdefer gpa.destroy(proof);
            const links = try gpa.alloc(Link, dependencies.len);
            errdefer gpa.free(links);
            const keys = try gpa.alloc(Key, entries.len);
            errdefer gpa.free(keys);
            proof.* = .{ .slot = self.groups.items.len, .dependencies = links, .keys = keys };
            var linked: usize = 0;
            errdefer for (links[0..linked]) |*link| self.unlink(gpa, link);
            for (dependencies, links) |dependency, *link| {
                const list = switch (dependency) {
                    .node => |node| blk: {
                        const entry = try self.watchers.getOrPut(gpa, node);
                        if (!entry.found_existing) entry.value_ptr.* = .empty;
                        break :blk entry.value_ptr;
                    },
                    .proof => |earlier| &earlier.dependents,
                };
                const capacity = list.capacity;
                link.* = .{ .dependency = dependency, .owner = proof, .slot = list.items.len };
                list.append(gpa, link) catch |err| {
                    if (dependency == .node and list.items.len == 0) {
                        list.deinit(gpa);
                        _ = self.watchers.remove(dependency.node);
                    }
                    return err;
                };
                const growth = (list.capacity - capacity) * @sizeOf(*Link);
                switch (dependency) {
                    .node => self.watcher_capacity_bytes += growth,
                    .proof => self.dependent_capacity_bytes += growth,
                }
                linked += 1;
            }
            for (entries, keys) |entry, *key| {
                std.debug.assert(!self.facts.contains(entry.key));
                key.* = entry.key;
                self.facts.putAssumeCapacity(entry.key, .{ .value = entry.value, .proof = proof });
            }
            self.groups.appendAssumeCapacity(proof);
            self.dependency_bytes += links.len * @sizeOf(Link) + keys.len * @sizeOf(Key);
        }
        /// Table payload and certificate bytes, excluding allocator metadata,
        /// alignment padding and hash-table control bytes.
        pub fn retainedPayloadBytes(self: *const Self) usize {
            return self.facts.capacity() * (@sizeOf(Key) + @sizeOf(Fact)) +
                self.watchers.capacity() * (@sizeOf(Node) + @sizeOf(std.ArrayList(*Link))) +
                (self.groups.capacity + self.pending.capacity) * @sizeOf(*Proof) +
                self.groups.items.len * @sizeOf(Proof) + self.dependency_bytes +
                self.dependent_capacity_bytes + self.watcher_capacity_bytes;
        }
    };
}

test "issue 12616 proof dependencies invalidate transitively without dropping disjoint facts" {
    const Node = enum(u32) { a, b, c };
    const Store = struct {
        changes: Changes(Node) = .{},
        /// Read exact test mutation events, independent of proof state.
        pub fn analysisChangesSince(self: *const @This(), epoch: u64) Changes(Node).Iterator {
            return self.changes.since(epoch);
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
    try proofs.publish(gpa, &.{.{ .key = 0, .value = 1 }}, &.{.{ .node = .a }});
    try proofs.publish(gpa, &.{.{ .key = 1, .value = 1 }}, &.{.{ .proof = proofs.get(0).?.proof }});
    try proofs.publish(gpa, &.{.{ .key = 2, .value = 0 }}, &.{.{ .node = .c }});
    for (0..100) |_| graph.changes.mark(.b);
    proofs.refresh(gpa, graph.changes.epoch, &graph);
    try std.testing.expectEqual(@as(usize, 3), proofs.facts.count());
    try std.testing.expectEqual(@as(usize, 1), proofs.changed_nodes);
    try std.testing.expectEqual(@as(usize, 0), proofs.invalidation_edges);
    graph.changes.mark(.a);
    proofs.refresh(gpa, graph.changes.epoch, &graph);
    try std.testing.expect(proofs.get(0) == null and proofs.get(1) == null);
    try std.testing.expectEqual(@as(u2, 0), proofs.get(2).?.value);
    try std.testing.expectEqual(@as(usize, 1), proofs.groups.items.len);
    try std.testing.expectEqual(@as(usize, 2), proofs.changed_nodes);
    proofs.clear(gpa);
    try std.testing.expectEqual(@as(usize, 0), proofs.facts.count());
    try std.testing.expectEqual(@as(usize, 0), proofs.groups.items.len);
}
