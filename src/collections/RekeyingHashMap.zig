//! Linear-probing hash map for indexes whose keys are repeatedly removed and
//! reinserted under new hashes.
//!
//! Backward-shift deletion closes each removed key's probe cluster immediately,
//! so lookup cost depends on the current entries rather than historical rekeys.

const std = @import("std");

const Allocator = std.mem.Allocator;

/// An open-addressed map whose backward-shift deletion leaves no tombstones.
pub fn RekeyingHashMap(
    comptime K: type,
    comptime V: type,
    comptime Context: type,
    comptime max_load_percentage: u64,
) type {
    if (max_load_percentage == 0 or max_load_percentage >= 100) {
        @compileError("max_load_percentage must be between 0 and 100");
    }

    return struct {
        const Self = @This();
        const minimum_capacity = 8;

        /// Whether deletion leaves occupied metadata in the probe cluster.
        pub const removal_leaves_tombstones = false;

        pub const KV = struct {
            key: K,
            value: V,
        };

        const Entry = struct {
            hash: u64,
            key: K,
            value: V,
        };

        allocator: Allocator,
        context: Context,
        slots: []?Entry,
        size: usize,

        pub fn init(allocator: Allocator, context: Context) Self {
            return .{
                .allocator = allocator,
                .context = context,
                .slots = &.{},
                .size = 0,
            };
        }

        pub fn deinit(self: *Self) void {
            if (self.slots.len != 0) self.allocator.free(self.slots);
            self.* = undefined;
        }

        pub fn count(self: *const Self) usize {
            return self.size;
        }

        pub fn capacity(self: *const Self) usize {
            return self.slots.len;
        }

        pub fn ensureTotalCapacity(self: *Self, expected_count: usize) Allocator.Error!void {
            if (expected_count <= usableCount(self.slots.len)) return;

            var new_capacity: usize = if (self.slots.len == 0) minimum_capacity else self.slots.len;
            while (expected_count > usableCount(new_capacity)) {
                new_capacity = std.math.mul(usize, new_capacity, 2) catch return error.OutOfMemory;
            }
            try self.resize(new_capacity);
        }

        pub fn putNoClobber(self: *Self, key: K, value: V) Allocator.Error!void {
            const required_count = std.math.add(usize, self.size, 1) catch return error.OutOfMemory;
            try self.ensureTotalCapacity(required_count);
            self.putAssumeCapacityNoClobber(key, value);
        }

        pub fn putAssumeCapacityNoClobber(self: *Self, key: K, value: V) void {
            std.debug.assert(self.size < usableCount(self.slots.len));
            std.debug.assert(self.get(key) == null);
            self.insertEntry(.{
                .hash = self.context.hash(key),
                .key = key,
                .value = value,
            });
        }

        pub fn get(self: *const Self, key: K) ?V {
            return self.getAdapted(key, self.context);
        }

        pub fn getAdapted(self: *const Self, key: anytype, context: anytype) ?V {
            const index = self.getIndexAdapted(key, context) orelse return null;
            return self.slots[index].?.value;
        }

        pub fn fetchRemove(self: *Self, key: K) ?KV {
            const index = self.getIndexAdapted(key, self.context) orelse return null;
            const removed = self.slots[index].?;
            self.removeAt(index);
            return .{ .key = removed.key, .value = removed.value };
        }

        fn usableCount(capacity_: usize) usize {
            const load: usize = max_load_percentage;
            return capacity_ / 100 * load + capacity_ % 100 * load / 100;
        }

        fn slotIndex(hash: u64, mask: usize) usize {
            return @as(usize, @truncate(hash)) & mask;
        }

        fn probeDistance(ideal: usize, actual: usize, mask: usize) usize {
            return (actual -% ideal) & mask;
        }

        fn getIndexAdapted(self: *const Self, key: anytype, context: anytype) ?usize {
            if (self.size == 0) return null;

            const hash = context.hash(key);
            const mask = self.slots.len - 1;
            var index = slotIndex(hash, mask);
            var remaining = self.slots.len;
            while (remaining != 0) : (remaining -= 1) {
                const entry = self.slots[index] orelse return null;
                if (entry.hash == hash and context.eql(key, entry.key)) return index;
                index = (index + 1) & mask;
            }
            return null;
        }

        fn insertEntry(self: *Self, entry: Entry) void {
            const mask = self.slots.len - 1;
            var index = slotIndex(entry.hash, mask);
            while (self.slots[index] != null) {
                index = (index + 1) & mask;
            }
            self.slots[index] = entry;
            self.size += 1;
        }

        fn removeAt(self: *Self, removed_index: usize) void {
            const mask = self.slots.len - 1;
            var hole = removed_index;
            var scan = (hole + 1) & mask;

            while (self.slots[scan]) |entry| {
                const ideal = slotIndex(entry.hash, mask);
                if (probeDistance(ideal, hole, mask) < probeDistance(ideal, scan, mask)) {
                    self.slots[hole] = entry;
                    hole = scan;
                }
                scan = (scan + 1) & mask;
            }

            self.slots[hole] = null;
            self.size -= 1;
        }

        fn resize(self: *Self, new_capacity: usize) Allocator.Error!void {
            std.debug.assert(std.math.isPowerOfTwo(new_capacity));
            std.debug.assert(new_capacity > self.size);

            const new_slots = try self.allocator.alloc(?Entry, new_capacity);
            @memset(new_slots, null);

            const old_slots = self.slots;
            const old_size = self.size;
            self.slots = new_slots;
            self.size = 0;
            for (old_slots) |slot| {
                if (slot) |entry| self.insertEntry(entry);
            }
            std.debug.assert(self.size == old_size);
            if (old_slots.len != 0) self.allocator.free(old_slots);
        }
    };
}

const TestContext = struct {
    pub fn hash(_: TestContext, key: u32) u64 {
        return key;
    }

    pub fn eql(_: TestContext, left: u32, right: u32) bool {
        return left == right;
    }
};

const CollisionContext = struct {
    pub fn hash(_: CollisionContext, _: u32) u64 {
        return 7;
    }

    pub fn eql(_: CollisionContext, left: u32, right: u32) bool {
        return left == right;
    }
};

const ClusteredContext = struct {
    pub fn hash(_: ClusteredContext, key: u32) u64 {
        return key % 17;
    }

    pub fn eql(_: ClusteredContext, left: u32, right: u32) bool {
        return left == right;
    }
};

const WideLookupContext = struct {
    pub fn hash(_: WideLookupContext, key: u64) u64 {
        return key;
    }

    pub fn eql(_: WideLookupContext, left: u64, right: u32) bool {
        return left == right;
    }
};

test "rekeying hash map preserves wrapped collision clusters after deletion" {
    const Map = RekeyingHashMap(u32, u32, CollisionContext, 80);
    var map = Map.init(std.testing.allocator, .{});
    defer map.deinit();

    for (1..6) |key| try map.putNoClobber(@intCast(key), @intCast(key * 10));
    try std.testing.expectEqual(@as(usize, 8), map.capacity());

    try std.testing.expectEqual(@as(u32, 10), map.fetchRemove(1).?.value);
    try std.testing.expectEqual(@as(u32, 30), map.fetchRemove(3).?.value);
    try std.testing.expectEqual(@as(u32, 20), map.get(2).?);
    try std.testing.expectEqual(@as(u32, 40), map.get(4).?);
    try std.testing.expectEqual(@as(u32, 50), map.get(5).?);
    try std.testing.expectEqual(@as(?u32, null), map.get(1));
    try std.testing.expectEqual(@as(?u32, null), map.get(3));
}

test "rekeying hash map grows and supports adapted lookup" {
    const Map = RekeyingHashMap(u32, u32, TestContext, 80);
    var map = Map.init(std.testing.allocator, .{});
    defer map.deinit();

    for (0..100) |key| try map.putNoClobber(@intCast(key), @intCast(key * 2));
    try std.testing.expectEqual(@as(usize, 100), map.count());
    try std.testing.expectEqual(@as(u32, 84), map.getAdapted(@as(u64, 42), WideLookupContext{}).?);

    for (0..100) |key| {
        try std.testing.expectEqual(@as(u32, @intCast(key * 2)), map.fetchRemove(@intCast(key)).?.value);
    }
    try std.testing.expectEqual(@as(usize, 0), map.count());
}

test "rekeying hash map matches an oracle through repeated collision churn" {
    const Map = RekeyingHashMap(u32, u32, ClusteredContext, 80);
    var map = Map.init(std.testing.allocator, .{});
    defer map.deinit();

    var oracle: [128]?u32 = @splat(null);
    var expected_count: usize = 0;
    var prng = std.Random.DefaultPrng.init(0x11103);
    const random = prng.random();

    for (0..10_000) |step| {
        const key = random.intRangeLessThan(u32, 0, @intCast(oracle.len));
        const index: usize = @intCast(key);
        switch (random.intRangeLessThan(u8, 0, 3)) {
            0 => try std.testing.expectEqual(oracle[index], map.get(key)),
            1 => {
                if (oracle[index] == null) {
                    const value: u32 = @intCast(step);
                    try map.putNoClobber(key, value);
                    oracle[index] = value;
                    expected_count += 1;
                }
            },
            2 => {
                const removed = map.fetchRemove(key);
                if (oracle[index]) |expected| {
                    try std.testing.expectEqual(expected, removed.?.value);
                    oracle[index] = null;
                    expected_count -= 1;
                } else {
                    try std.testing.expectEqual(@as(?Map.KV, null), removed);
                }
            },
            else => unreachable,
        }

        try std.testing.expectEqual(expected_count, map.count());
        for (oracle, 0..) |expected, expected_key| {
            try std.testing.expectEqual(expected, map.get(@intCast(expected_key)));
        }
    }
}
