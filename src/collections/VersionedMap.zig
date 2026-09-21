//! Independent environment versions sharing one indexed active view. Forking
//! copies no bindings; switching versions undoes and replays only their changes.
//! A family is single-threaded and owns its storage until its last handle dies.
const std = @import("std");
const DenseMap = @import("DenseMap.zig").DenseMap;
const Allocator = std.mem.Allocator;

/// Versioned bindings indexed directly by compiler-owned integer IDs.
pub fn VersionedDenseMap(comptime K: type, comptime V: type) type {
    return VersionedMap(K, V, DenseMap(K, usize));
}

/// Versioned bindings whose keys have structural rather than dense identity.
pub fn VersionedHashMap(comptime K: type, comptime V: type) type {
    return VersionedMap(K, V, std.AutoHashMap(K, usize));
}

fn VersionedMap(comptime K: type, comptime V: type, comptime Index: type) type {
    return struct {
        const Self = @This();
        const Slot = struct { key: K, value: ?V = null, position: usize = 0 };
        const Change = struct {
            parent: ?usize,
            depth: usize,
            slot: usize,
            position: usize,
            before: ?V,
            after: ?V,
            redo_next: ?usize = null,
        };
        const Store = struct {
            allocator: Allocator,
            refs: usize = 1,
            index: Index,
            slots: std.ArrayList(Slot) = .empty,
            active: std.ArrayList(usize) = .empty,
            changes: std.ArrayList(Change) = .empty,
            current: ?usize = null,
            restore_credits: usize = 0,
            /// Deterministic work count, also used by the complexity tests.
            replayed_changes: usize = 0,

            fn depth(self: *const Store, version: ?usize) usize {
                return if (version) |id| self.changes.items[id].depth else 0;
            }

            fn apply(self: *Store, change: Change, forward: bool) void {
                const before = if (forward) change.before else change.after;
                const after = if (forward) change.after else change.before;
                const slot = &self.slots.items[change.slot];
                if (before == null and after != null) {
                    // Reverse a swap removal at its original position, keeping
                    // iteration order identical every time this version is read.
                    const end = self.active.items.len;
                    self.active.appendAssumeCapacity(change.slot);
                    if (change.position != end) {
                        const displaced = self.active.items[change.position];
                        self.active.items[end] = displaced;
                        self.slots.items[displaced].position = end;
                        self.active.items[change.position] = change.slot;
                    }
                    slot.position = change.position;
                } else if (before != null and after == null) {
                    std.debug.assert(self.active.items[change.position] == change.slot);
                    _ = self.active.swapRemove(change.position);
                    if (change.position < self.active.items.len) {
                        self.slots.items[self.active.items[change.position]].position = change.position;
                    }
                }
                slot.value = after;
            }

            fn activate(self: *Store, version: ?usize) void {
                var left = self.current;
                var right = version;
                var redo: ?usize = null;
                while (left != right) {
                    if (self.depth(left) >= self.depth(right)) {
                        const change = self.changes.items[left.?];
                        self.apply(change, false);
                        if (@import("builtin").is_test) self.replayed_changes += 1;
                        left = change.parent;
                    } else {
                        const change = &self.changes.items[right.?];
                        change.redo_next = redo;
                        redo = right;
                        right = change.parent;
                    }
                }
                while (redo) |id| {
                    const change = self.changes.items[id];
                    self.apply(change, true);
                    if (@import("builtin").is_test) self.replayed_changes += 1;
                    redo = change.redo_next;
                }
                self.current = version;
            }

            fn recordChange(self: *Store, slot_id: usize, value: ?V) ?usize {
                const slot = self.slots.items[slot_id];
                const id = self.changes.items.len;
                const change_ = Change{
                    .parent = self.current,
                    .depth = self.depth(self.current) + 1,
                    .slot = slot_id,
                    .position = if (slot.value == null) self.active.items.len else slot.position,
                    .before = slot.value,
                    .after = value,
                };
                if (self.refs == 1) {
                    // No other handle can observe the old state. Make this
                    // version the base without retaining an undo history.
                    self.changes.clearRetainingCapacity();
                    self.apply(change_, true);
                    self.current = null;
                    return null;
                }
                self.changes.appendAssumeCapacity(change_);
                self.apply(change_, true);
                self.current = id;
                return id;
            }
        };

        allocator: Allocator,
        store: ?*Store = null,
        version: ?usize = null,

        pub fn init(allocator: Allocator) Self {
            return .{ .allocator = allocator };
        }

        pub fn deinit(self: *Self) void {
            if (self.store) |store| {
                store.refs -= 1;
                if (store.refs == 0) {
                    store.index.deinit();
                    store.slots.deinit(store.allocator);
                    store.active.deinit(store.allocator);
                    store.changes.deinit(store.allocator);
                    store.allocator.destroy(store);
                }
            }
            self.* = undefined;
        }

        /// Both handles remain independently mutable, including the parent.
        pub fn fork(self: *const Self) Self {
            if (self.store) |store| store.refs += 1;
            return self.*;
        }

        fn activeStore(self: *const Self) ?*Store {
            const store = self.store orelse return null;
            store.activate(self.version);
            return store;
        }

        pub fn get(self: *const Self, key: K) ?V {
            const store = self.activeStore() orelse return null;
            const id = store.index.get(key) orelse return null;
            return store.slots.items[id].value;
        }

        pub fn contains(self: *const Self, key: K) bool {
            return self.get(key) != null;
        }

        pub fn count(self: *const Self) usize {
            const store = self.activeStore() orelse return 0;
            return store.active.items.len;
        }

        /// Reserve a second change for allocation-free cleanup of this write.
        /// Cleanup calls restore(), not put(), so retained snapshots remain
        /// independent even when an enclosing operation fails allocation.
        pub fn put(self: *Self, key: K, value: V) Allocator.Error!void {
            return self.putWithCleanup(key, value, true);
        }

        /// Durable memo entries have no temporary-binding cleanup obligation.
        pub fn putPermanent(self: *Self, key: K, value: V) Allocator.Error!void {
            return self.putWithCleanup(key, value, false);
        }

        fn putWithCleanup(self: *Self, key: K, value: V, comptime cleanup: bool) Allocator.Error!void {
            if (self.store == null) {
                const store = try self.allocator.create(Store);
                store.* = .{ .allocator = self.allocator, .index = Index.init(self.allocator) };
                self.store = store;
            }
            const store = self.activeStore().?;
            try store.changes.ensureUnusedCapacity(store.allocator, store.restore_credits + @intFromBool(store.refs != 1) + @intFromBool(cleanup));
            try store.slots.ensureUnusedCapacity(store.allocator, 1);
            try store.active.ensureTotalCapacity(store.allocator, store.slots.items.len + 1);
            const entry = try store.index.getOrPut(key);
            if (!entry.found_existing) {
                entry.value_ptr.* = store.slots.items.len;
                store.slots.appendAssumeCapacity(.{ .key = key });
            }
            self.version = store.recordChange(entry.value_ptr.*, value);
            store.restore_credits += @intFromBool(cleanup);
        }

        /// Undo a temporary binding installed with put(). Each installation
        /// permits one restore; restoring never allocates or discards versions.
        pub fn restore(self: *Self, key: K, previous: ?V) void {
            const store = self.activeStore().?;
            const id = store.index.get(key).?;
            // Saving a binding does not imply the operation changed it before
            // failure. An unchanged binding needs no cleanup reservation.
            if (std.meta.eql(store.slots.items[id].value, previous)) return;
            std.debug.assert(store.restore_credits > 0);
            self.version = store.recordChange(id, previous);
            store.restore_credits -= 1;
        }

        /// Remove a temporary binding; the matching put reserved its cleanup.
        pub fn remove(self: *Self, key: K) bool {
            if (!self.contains(key)) return false;
            self.restore(key, null);
            return true;
        }

        pub const Entry = struct { key_ptr: *const K, value_ptr: *const V };
        pub const Iterator = struct {
            map: *const Self,
            position: usize = 0,
            key: K = undefined,
            value: V = undefined,

            pub fn next(self: *Iterator) ?Entry {
                const store = self.map.activeStore() orelse return null;
                if (self.position == store.active.items.len) return null;
                const slot = store.slots.items[store.active.items[self.position]];
                self.position += 1;
                self.key = slot.key;
                self.value = slot.value.?;
                return .{ .key_ptr = &self.key, .value_ptr = &self.value };
            }
        };

        pub fn iterator(self: *const Self) Iterator {
            return .{ .map = self };
        }

        pub const KeyIterator = struct {
            inner: Iterator,
            pub fn next(self: *KeyIterator) ?*const K {
                return (self.inner.next() orelse return null).key_ptr;
            }
        };

        pub fn keyIterator(self: *const Self) KeyIterator {
            return .{ .inner = self.iterator() };
        }
    };
}

test "versioned maps preserve siblings, parent mutations, removals, and iteration" {
    const Map = VersionedDenseMap(u32, u32);
    var parent = Map.init(std.testing.allocator);
    defer parent.deinit();
    try parent.put(1, 10);
    try parent.put(2, 20);
    var first = parent.fork();
    defer first.deinit();
    var second = parent.fork();
    defer second.deinit();
    try first.put(1, 11);
    try second.put(1, 12);
    try parent.put(1, 13);
    try first.put(2, 21);
    try std.testing.expect(first.remove(2));
    for (0..3) |_| {
        try std.testing.expectEqual(@as(?u32, 11), first.get(1));
        try std.testing.expectEqual(@as(?u32, null), first.get(2));
        try std.testing.expectEqual(@as(?u32, 12), second.get(1));
        try std.testing.expectEqual(@as(?u32, 20), second.get(2));
        try std.testing.expectEqual(@as(?u32, 13), parent.get(1));
        var iter = parent.iterator();
        try std.testing.expectEqual(1, iter.next().?.key_ptr.*);
        _ = first.get(1);
        try std.testing.expectEqual(2, iter.next().?.key_ptr.*);
        try std.testing.expect(iter.next() == null);
    }
}

test "versioned map forks and branch switching touch changes rather than inherited bindings" {
    const Map = VersionedDenseMap(u32, u32);
    var parent = Map.init(std.testing.allocator);
    defer parent.deinit();
    for (0..8192) |i| try parent.put(@intCast(i), @intCast(i));
    const inherited_changes = parent.store.?.changes.items.len;
    const before = parent.store.?.replayed_changes;
    for (0..1024) |i| {
        var child = parent.fork();
        defer child.deinit();
        try std.testing.expectEqual(inherited_changes + i, parent.store.?.changes.items.len);
        try child.put(0, 12345);
        try std.testing.expectEqual(@as(?u32, 12345), child.get(0));
        try std.testing.expectEqual(@as(?u32, 0), parent.get(0));
    }
    try std.testing.expectEqual(@as(usize, 1024), parent.store.?.replayed_changes - before);
}

test "unshared memo tables discard history without weakening later snapshots" {
    const Map = VersionedDenseMap(u32, u32);
    var parent = Map.init(std.testing.allocator);
    defer parent.deinit();
    for (0..1000) |i| try parent.putPermanent(@intCast(i), @intCast(i));
    try std.testing.expectEqual(@as(usize, 0), parent.store.?.changes.capacity);
    {
        var child = parent.fork();
        defer child.deinit();
        try child.putPermanent(1, 999);
        try std.testing.expectEqual(@as(?u32, 1), parent.get(1));
        try std.testing.expectEqual(@as(?u32, 999), child.get(1));
    }
    try parent.putPermanent(2, 222);
    try std.testing.expectEqual(@as(usize, 0), parent.store.?.changes.items.len);
    var snapshot = parent.fork();
    defer snapshot.deinit();
    try parent.putPermanent(2, 333);
    try std.testing.expectEqual(@as(?u32, 222), snapshot.get(2));
    try std.testing.expectEqual(@as(?u32, 333), parent.get(2));
}

test "versioned map restores middle removals in retained iteration order" {
    const Map = VersionedDenseMap(u32, u32);
    var parent = Map.init(std.testing.allocator);
    defer parent.deinit();
    for (0..4) |i| try parent.put(@intCast(i), @intCast(i + 10));
    var child = parent.fork();
    defer child.deinit();
    try child.put(1, 11);
    try std.testing.expect(child.remove(1));
    var removed = child.fork();
    defer removed.deinit();
    try child.put(1, 99);
    for (0..4) |_| {
        var parent_iter = parent.iterator();
        var child_iter = child.iterator();
        var removed_iter = removed.iterator();
        for ([_]u32{ 0, 1, 2, 3 }, [_]u32{ 0, 3, 2, 1 }) |parent_key, child_key| {
            try std.testing.expectEqual(parent_key, parent_iter.next().?.key_ptr.*);
            try std.testing.expectEqual(child_key, child_iter.next().?.key_ptr.*);
        }
        for ([_]u32{ 0, 3, 2 }) |key| {
            _ = parent.get(1);
            try std.testing.expectEqual(key, removed_iter.next().?.key_ptr.*);
        }
        try std.testing.expect(removed_iter.next() == null);
    }
}

test "versioned maps match independent copies across branching mutations" {
    inline for (.{ VersionedDenseMap(u32, u32), VersionedHashMap(u32, u32) }) |Map| {
        var versions: [12]Map = @splat(Map.init(std.testing.allocator));
        defer for (&versions) |*version| version.deinit();
        var expected: [12][16]?u32 = @splat(@splat(null));
        var random = std.Random.DefaultPrng.init(11322);
        for (0..2000) |_| {
            const target = random.random().uintLessThan(usize, versions.len);
            const key = random.random().uintLessThan(u32, 16);
            switch (random.random().uintLessThan(u8, 3)) {
                0 => {
                    const source = random.random().uintLessThan(usize, versions.len);
                    if (source != target) {
                        versions[target].deinit();
                        versions[target] = versions[source].fork();
                        expected[target] = expected[source];
                    }
                },
                1 => {
                    const value = random.random().int(u32);
                    try versions[target].put(key, value);
                    expected[target][key] = value;
                },
                2 => {
                    // Each temporary installation owns its cleanup reservation.
                    try versions[target].put(key, 0);
                    _ = versions[target].remove(key);
                    expected[target][key] = null;
                },
                else => unreachable,
            }
            for (&versions, expected) |*version, entries| {
                var count: usize = 0;
                for (entries, 0..) |value, index| {
                    try std.testing.expectEqual(value, version.get(@intCast(index)));
                    count += @intFromBool(value != null);
                }
                try std.testing.expectEqual(count, version.count());
            }
        }
    }
}

test "versioned map allocation failures and reserved cleanup preserve snapshots" {
    inline for (.{ VersionedDenseMap(u32, u32), VersionedHashMap(u32, u32) }) |Map| {
        const Scenario = struct {
            fn run(allocator: Allocator) (Allocator.Error || error{ TestUnexpectedResult, TestExpectedEqual })!void {
                var parent = Map.init(allocator);
                defer parent.deinit();
                try parent.put(1, 10);
                var child = parent.fork();
                defer child.deinit();
                child.put(1, 20) catch |err| {
                    try std.testing.expectEqual(@as(?u32, 10), child.get(1));
                    return err;
                };
                for (2..34) |index| {
                    const key: u32 = @intCast(index * 64);
                    child.put(key, key) catch |err| {
                        try std.testing.expectEqual(@as(?u32, null), child.get(key));
                        try std.testing.expectEqual(@as(?u32, 10), parent.get(1));
                        try std.testing.expectEqual(@as(?u32, 20), child.get(1));
                        return err;
                    };
                }
                var snapshot = child.fork();
                defer snapshot.deinit();
                const store = child.store.?;
                const saved_allocator = store.allocator;
                store.allocator = std.testing.failing_allocator;
                defer store.allocator = saved_allocator;
                child.restore(1, 10);
                try std.testing.expectEqual(@as(?u32, 10), child.get(1));
                try std.testing.expectEqual(@as(?u32, 20), snapshot.get(1));
                try std.testing.expectEqual(@as(?u32, 10), parent.get(1));
            }
        };
        try Scenario.run(std.testing.allocator);
        try std.testing.checkAllAllocationFailures(std.testing.allocator, Scenario.run, .{});
    }
}
