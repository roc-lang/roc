//! A multimap whose buckets are chains threaded through one shared node list,
//! so a key holding several values owns no list of its own.
const std = @import("std");
const Allocator = std.mem.Allocator;

/// Values grouped by key, each group in insertion order. `Context` is the
/// stateless hash map context for `K`.
pub fn ChainedBuckets(comptime K: type, comptime V: type, comptime Context: type) type {
    return struct {
        const Self = @This();
        /// Node positions are program-size counts, so `u32` holds them;
        /// `append` refuses growth past that range.
        const end: u32 = std.math.maxInt(u32);
        const Chain = struct { head: u32, tail: u32 };
        const Node = struct { value: V, next: u32 };

        chains: std.HashMapUnmanaged(K, Chain, Context, std.hash_map.default_max_load_percentage) = .empty,
        nodes: std.ArrayList(Node) = .empty,

        pub const empty: Self = .{};

        /// One key's values, oldest first. A value appended to the same key
        /// while the iterator is live is visited too.
        pub const Iterator = struct {
            buckets: *const Self,
            index: u32,

            /// Whether no value remains; a fresh iterator is empty exactly
            /// when its key is absent.
            pub fn isEmpty(self: Iterator) bool {
                return self.index == end;
            }

            pub fn next(self: *Iterator) ?V {
                if (self.index == end) return null;
                const node = self.buckets.nodes.items[self.index];
                self.index = node.next;
                return node.value;
            }
        };

        pub fn deinit(self: *Self, allocator: Allocator) void {
            self.chains.deinit(allocator);
            self.nodes.deinit(allocator);
            self.* = undefined;
        }

        pub fn clearRetainingCapacity(self: *Self) void {
            self.chains.clearRetainingCapacity();
            self.nodes.clearRetainingCapacity();
        }

        /// The number of distinct keys.
        pub fn count(self: *const Self) u32 {
            return self.chains.count();
        }

        pub fn contains(self: *const Self, key: K) bool {
            return self.chains.contains(key);
        }

        pub fn get(self: *const Self, key: K) Iterator {
            const chain = self.chains.get(key) orelse return .{ .buckets = self, .index = end };
            return .{ .buckets = self, .index = chain.head };
        }

        pub fn keyIterator(self: *const Self) std.HashMapUnmanaged(K, Chain, Context, std.hash_map.default_max_load_percentage).KeyIterator {
            return self.chains.keyIterator();
        }

        /// Room for `values` more values under at most `keys` new keys, so the
        /// matching `appendAssumeCapacity` calls cannot fail.
        pub fn ensureUnusedCapacity(self: *Self, allocator: Allocator, keys: u32, values: usize) Allocator.Error!void {
            const needed = std.math.add(usize, self.nodes.items.len, values) catch return error.OutOfMemory;
            if (needed > end) return error.OutOfMemory;
            try self.nodes.ensureUnusedCapacity(allocator, values);
            try self.chains.ensureUnusedCapacity(allocator, keys);
        }

        pub fn append(self: *Self, allocator: Allocator, key: K, value: V) Allocator.Error!void {
            try self.ensureUnusedCapacity(allocator, 1, 1);
            self.appendAssumeCapacity(key, value);
        }

        pub fn appendAssumeCapacity(self: *Self, key: K, value: V) void {
            const index: u32 = @intCast(self.nodes.items.len);
            self.nodes.appendAssumeCapacity(.{ .value = value, .next = end });
            const entry = self.chains.getOrPutAssumeCapacity(key);
            if (entry.found_existing) {
                self.nodes.items[entry.value_ptr.tail].next = index;
                entry.value_ptr.tail = index;
            } else {
                entry.value_ptr.* = .{ .head = index, .tail = index };
            }
        }
    };
}

/// `ChainedBuckets` with the automatic hash map context for `K`.
pub fn AutoChainedBuckets(comptime K: type, comptime V: type) type {
    return ChainedBuckets(K, V, std.hash_map.AutoContext(K));
}

test "chained buckets keep each key's values in insertion order" {
    const allocator = std.testing.allocator;
    var buckets: AutoChainedBuckets(u64, u32) = .empty;
    defer buckets.deinit(allocator);

    try buckets.append(allocator, 7, 1);
    try buckets.append(allocator, 9, 2);
    try buckets.append(allocator, 7, 3);
    try buckets.append(allocator, 7, 4);

    var sevens = buckets.get(7);
    try std.testing.expectEqual(@as(?u32, 1), sevens.next());
    try std.testing.expectEqual(@as(?u32, 3), sevens.next());
    try std.testing.expectEqual(@as(?u32, 4), sevens.next());
    try std.testing.expectEqual(@as(?u32, null), sevens.next());

    var nines = buckets.get(9);
    try std.testing.expectEqual(@as(?u32, 2), nines.next());
    try std.testing.expectEqual(@as(?u32, null), nines.next());

    var missing = buckets.get(11);
    try std.testing.expectEqual(@as(?u32, null), missing.next());
    try std.testing.expectEqual(@as(u32, 2), buckets.count());

    buckets.clearRetainingCapacity();
    try std.testing.expectEqual(@as(u32, 0), buckets.count());
    try std.testing.expect(!buckets.contains(7));
    try buckets.append(allocator, 7, 5);
    var reused = buckets.get(7);
    try std.testing.expectEqual(@as(?u32, 5), reused.next());
    try std.testing.expectEqual(@as(?u32, null), reused.next());
}
