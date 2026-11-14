//! Items grouped by a key, iterated group by group.
//!
//! `CsrGroups` is for dense keys: a count per key, a prefix sum, and one
//! scatter put every group in one contiguous range, so walking a group is a
//! sequential read and no per-item link exists. The per-key start table is the
//! only initialized table.
//!
//! `SortedGroups` is for sparse keys over a large domain: a flat list of
//! (key, item) pairs sorted by key, whose groups are runs. It has no per-key
//! table at all, so its cost scales with the items recorded, not the domain.
//!
//! Both keep the items of one group in insertion order.

const std = @import("std");
const storage_work = @import("storage_work.zig");
const WriteFirstSlice = @import("WriteFirstSlice.zig").WriteFirstSlice;

const Allocator = std.mem.Allocator;

/// Items of type `Item` grouped by dense `u32` keys below a known count.
pub fn CsrGroups(comptime Item: type) type {
    return struct {
        const Self = @This();

        /// Group `k` is `items[starts[k]..starts[k + 1]]`.
        starts: []u32 = &.{},
        items: []Item = &.{},

        /// Free the groups.
        pub fn deinit(self: *Self, gpa: Allocator) void {
            gpa.free(self.starts);
            gpa.free(self.items);
            self.* = .{};
        }

        /// The number of keys.
        pub fn keyCount(self: *const Self) usize {
            return if (self.starts.len == 0) 0 else self.starts.len - 1;
        }

        /// The items of group `key`, in insertion order.
        pub inline fn group(self: *const Self, key: usize) []Item {
            return self.items[self.starts[key]..self.starts[key + 1]];
        }

        /// Collects (key, item) pairs, then builds the groups.
        pub const Builder = struct {
            pairs: std.ArrayListUnmanaged(struct { key: u32, item: Item }) = .empty,
            key_count: usize,

            /// A builder for keys below `key_count`.
            pub fn init(key_count: usize) Builder {
                return .{ .key_count = key_count };
            }

            /// Free the collected pairs.
            pub fn deinit(self: *Builder, gpa: Allocator) void {
                self.pairs.deinit(gpa);
                self.pairs = .empty;
            }

            /// Record `item` in group `key`.
            pub fn add(self: *Builder, gpa: Allocator, key: u32, item: Item) Allocator.Error!void {
                std.debug.assert(key < self.key_count);
                try self.pairs.append(gpa, .{ .key = key, .item = item });
            }

            /// The groups of every pair recorded.
            pub fn build(self: *const Builder, gpa: Allocator) Allocator.Error!Self {
                return self.buildOrdered(gpa, false);
            }

            /// Contiguous groups in reverse insertion order, matching head-push chains.
            pub fn buildReversed(self: *const Builder, gpa: Allocator) Allocator.Error!Self {
                return self.buildOrdered(gpa, true);
            }

            fn buildOrdered(self: *const Builder, gpa: Allocator, comptime reverse: bool) Allocator.Error!Self {
                if (self.pairs.items.len > std.math.maxInt(u32)) return error.OutOfMemory;
                const starts = try zeroedStarts(gpa, self.key_count);
                errdefer gpa.free(starts);
                for (self.pairs.items) |pair| starts[pair.key + 1] += 1;
                prefixSum(starts);
                var items = try WriteFirstSlice(Item).init(gpa, self.pairs.items.len);
                errdefer items.deinit(gpa);
                const cursor = try gpa.dupe(u32, if (reverse) starts[1..] else starts[0..self.key_count]);
                defer gpa.free(cursor);
                for (self.pairs.items) |pair| {
                    if (reverse) cursor[pair.key] -= 1;
                    items.set(cursor[pair.key], pair.item);
                    if (!reverse) cursor[pair.key] += 1;
                }
                return .{ .starts = starts, .items = items.finish(gpa) };
            }
        };

        /// Group source indices directly, without materializing key-item pairs.
        /// `keyOf` returns null for source entries that belong to no group.
        pub fn fromIndices(gpa: Allocator, key_count: usize, item_count: usize, context: anytype, comptime keyOf: anytype, comptime reverse: bool) Allocator.Error!Self {
            if (Item != u32) @compileError("indexed groups store source u32 indices");
            if (item_count > std.math.maxInt(u32)) return error.OutOfMemory;
            const starts = try zeroedStarts(gpa, key_count);
            errdefer gpa.free(starts);
            for (0..item_count) |i| if (keyOf(context, i)) |key| {
                std.debug.assert(key < key_count);
                starts[key + 1] += 1;
            };
            prefixSum(starts);
            var items = try WriteFirstSlice(u32).init(gpa, starts[key_count]);
            errdefer items.deinit(gpa);
            const cursor = try gpa.dupe(u32, if (reverse) starts[1..] else starts[0..key_count]);
            defer gpa.free(cursor);
            for (0..item_count) |i| if (keyOf(context, i)) |key| {
                if (reverse) cursor[key] -= 1;
                items.set(cursor[key], @intCast(i));
                if (!reverse) cursor[key] += 1;
            };
            return .{ .starts = starts, .items = items.finish(gpa) };
        }

        /// Reverse adjacency with source IDs ordered by source traversal.
        pub fn transpose(gpa: Allocator, source_starts: []const u32, targets: []const u32) Allocator.Error!Self {
            if (Item != u32) @compileError("adjacency groups store u32 nodes");
            std.debug.assert(source_starts.len != 0);
            const count = source_starts.len - 1;
            if (targets.len > std.math.maxInt(u32)) return error.OutOfMemory;
            const starts = try zeroedStarts(gpa, count);
            errdefer gpa.free(starts);
            for (targets) |target| starts[target + 1] += 1;
            prefixSum(starts);
            var items = try WriteFirstSlice(u32).init(gpa, targets.len);
            errdefer items.deinit(gpa);
            const cursor = try gpa.dupe(u32, starts[0..count]);
            defer gpa.free(cursor);
            for (0..count) |source| {
                for (targets[source_starts[source]..source_starts[source + 1]]) |target| {
                    items.set(cursor[target], @intCast(source));
                    cursor[target] += 1;
                }
            }
            return .{ .starts = starts, .items = items.finish(gpa) };
        }

        /// Group the indices of `keys` by their key: item `i` is in group
        /// `keys[i]`, and an entry of `absent` belongs to no group. Counts
        /// straight from the key column, with no pair list.
        pub fn fromKeyColumn(gpa: Allocator, key_count: usize, keys: []const u32, absent: u32) Allocator.Error!Self {
            if (Item != u32) @compileError("a key column groups u32 item indices");
            if (keys.len > std.math.maxInt(u32)) return error.OutOfMemory;
            const starts = try zeroedStarts(gpa, key_count);
            errdefer gpa.free(starts);
            for (keys) |key| {
                if (key == absent) continue;
                std.debug.assert(key < key_count);
                starts[key + 1] += 1;
            }
            prefixSum(starts);
            const present = starts[key_count];
            var items = try WriteFirstSlice(u32).init(gpa, present);
            errdefer items.deinit(gpa);
            const cursor = try gpa.dupe(u32, starts[0..key_count]);
            defer gpa.free(cursor);
            for (keys, 0..) |key, item| {
                if (key == absent) continue;
                items.set(cursor[key], @intCast(item));
                cursor[key] += 1;
            }
            return .{ .starts = starts, .items = items.finish(gpa) };
        }
    };
}

fn zeroedStarts(gpa: Allocator, key_count: usize) Allocator.Error![]u32 {
    if (key_count > std.math.maxInt(u32)) return error.OutOfMemory;
    const len = std.math.add(usize, key_count, 1) catch return error.OutOfMemory;
    const starts = try gpa.alloc(u32, len);
    @memset(starts, 0);
    storage_work.add(.bytes_filled, starts.len * @sizeOf(u32));
    return starts;
}

fn prefixSum(starts: []u32) void {
    for (1..starts.len) |i| starts[i] += starts[i - 1];
}

/// Items of type `Item` grouped by sparse `u32` keys.
pub fn SortedGroups(comptime Item: type) type {
    return struct {
        const Self = @This();

        /// One recorded item.
        pub const Pair = struct { key: u32, item: Item };

        pairs: std.ArrayListUnmanaged(Pair) = .empty,
        /// Whether the pairs arrived in nondecreasing key order.
        in_order: bool = true,

        /// Free the pairs.
        pub fn deinit(self: *Self, gpa: Allocator) void {
            self.pairs.deinit(gpa);
            self.* = .{};
        }

        /// Forget every pair, keeping capacity.
        pub fn clearRetainingCapacity(self: *Self) void {
            self.pairs.clearRetainingCapacity();
            self.in_order = true;
        }

        /// Record `item` in group `key`.
        pub fn add(self: *Self, gpa: Allocator, key: u32, item: Item) Allocator.Error!void {
            if (self.pairs.items.len != 0 and self.pairs.items[self.pairs.items.len - 1].key > key) self.in_order = false;
            try self.pairs.append(gpa, .{ .key = key, .item = item });
        }

        /// Sort the pairs by key, keeping insertion order within each group.
        /// Pairs that arrived in key order are not sorted again.
        pub fn finish(self: *Self) void {
            if (self.in_order) return;
            std.mem.sort(Pair, self.pairs.items, {}, struct {
                fn lessThan(_: void, a: Pair, b: Pair) bool {
                    return a.key < b.key;
                }
            }.lessThan);
            self.in_order = true;
        }

        /// One group: its key and its pairs.
        pub const Run = struct { key: u32, pairs: []const Pair };

        /// Iterate the groups in key order. Requires `finish`.
        pub fn runs(self: *const Self) RunIterator {
            std.debug.assert(self.in_order);
            return .{ .pairs = self.pairs.items };
        }

        /// The pairs of group `key`. Requires `finish`.
        pub fn group(self: *const Self, key: u32) []const Pair {
            std.debug.assert(self.in_order);
            const items = self.pairs.items;
            const start = std.sort.lowerBound(Pair, items, key, struct {
                fn order(k: u32, pair: Pair) std.math.Order {
                    return std.math.order(k, pair.key);
                }
            }.order);
            var end = start;
            while (end < items.len and items[end].key == key) end += 1;
            return items[start..end];
        }

        pub const RunIterator = struct {
            pairs: []const Pair,

            pub fn next(self: *RunIterator) ?Run {
                if (self.pairs.len == 0) return null;
                const key = self.pairs[0].key;
                var end: usize = 1;
                while (end < self.pairs.len and self.pairs[end].key == key) end += 1;
                const run: Run = .{ .key = key, .pairs = self.pairs[0..end] };
                self.pairs = self.pairs[end..];
                return run;
            }
        };
    };
}

const testing = std.testing;

test "csr groups: empty, single and duplicate keys keep insertion order" {
    {
        var builder = CsrGroups(u32).Builder.init(0);
        defer builder.deinit(testing.allocator);
        var groups = try builder.build(testing.allocator);
        defer groups.deinit(testing.allocator);
        try testing.expectEqual(@as(usize, 0), groups.keyCount());
    }
    var builder = CsrGroups(u32).Builder.init(4);
    defer builder.deinit(testing.allocator);
    try builder.add(testing.allocator, 2, 10);
    try builder.add(testing.allocator, 0, 11);
    try builder.add(testing.allocator, 2, 12);
    try builder.add(testing.allocator, 2, 13);
    var groups = try builder.build(testing.allocator);
    defer groups.deinit(testing.allocator);
    try testing.expectEqualSlices(u32, &.{11}, groups.group(0));
    try testing.expectEqualSlices(u32, &.{}, groups.group(1));
    try testing.expectEqualSlices(u32, &.{ 10, 12, 13 }, groups.group(2));
    try testing.expectEqualSlices(u32, &.{}, groups.group(3));
}

test "csr groups from a key column skip absent keys" {
    const absent = std.math.maxInt(u32);
    var groups = try CsrGroups(u32).fromKeyColumn(testing.allocator, 3, &.{ 1, absent, 0, 1, 2, absent }, absent);
    defer groups.deinit(testing.allocator);
    try testing.expectEqualSlices(u32, &.{2}, groups.group(0));
    try testing.expectEqualSlices(u32, &.{ 0, 3 }, groups.group(1));
    try testing.expectEqualSlices(u32, &.{4}, groups.group(2));
}

test "sorted groups: runs in key order with insertion order inside each" {
    var groups: SortedGroups(u8) = .{};
    defer groups.deinit(testing.allocator);
    try groups.add(testing.allocator, 900_000, 1);
    try groups.add(testing.allocator, 5, 2);
    try groups.add(testing.allocator, 900_000, 3);
    try groups.add(testing.allocator, 5, 4);
    try testing.expect(!groups.in_order);
    groups.finish();
    var runs = groups.runs();
    const first = runs.next().?;
    try testing.expectEqual(@as(u32, 5), first.key);
    try testing.expectEqual(@as(u8, 2), first.pairs[0].item);
    try testing.expectEqual(@as(u8, 4), first.pairs[1].item);
    const second = runs.next().?;
    try testing.expectEqual(@as(u32, 900_000), second.key);
    try testing.expectEqual(@as(u8, 1), second.pairs[0].item);
    try testing.expectEqual(@as(u8, 3), second.pairs[1].item);
    try testing.expectEqual(@as(?SortedGroups(u8).Run, null), runs.next());
    try testing.expectEqual(@as(usize, 0), groups.group(6).len);
    try testing.expectEqual(@as(usize, 2), groups.group(5).len);
}

test "sorted groups skip the sort for pairs that arrive in key order" {
    var groups: SortedGroups(u32) = .{};
    defer groups.deinit(testing.allocator);
    for (0..10) |i| try groups.add(testing.allocator, @intCast(i / 3), @intCast(i));
    try testing.expect(groups.in_order);
    groups.finish();
    try testing.expectEqual(@as(usize, 3), groups.group(1).len);
    try testing.expectEqual(@as(usize, 1), groups.group(3).len);
}

test "both groupings agree with a chain oracle on random inputs" {
    var prng = std.Random.DefaultPrng.init(0x5eed);
    const random = prng.random();
    for (0..50) |_| {
        const key_count = random.intRangeAtMost(u32, 1, 40);
        const item_count = random.intRangeAtMost(usize, 0, 200);
        // The oracle: index-chained bucket lists, walked from head to tail.
        const heads = try testing.allocator.alloc(u32, key_count);
        defer testing.allocator.free(heads);
        const tails = try testing.allocator.alloc(u32, key_count);
        defer testing.allocator.free(tails);
        @memset(heads, std.math.maxInt(u32));
        const next = try testing.allocator.alloc(u32, item_count);
        defer testing.allocator.free(next);
        const keys = try testing.allocator.alloc(u32, item_count);
        defer testing.allocator.free(keys);

        var csr = CsrGroups(u32).Builder.init(key_count);
        defer csr.deinit(testing.allocator);
        var sorted: SortedGroups(u32) = .{};
        defer sorted.deinit(testing.allocator);
        for (0..item_count) |item| {
            const key = random.uintLessThan(u32, key_count);
            keys[item] = key;
            next[item] = std.math.maxInt(u32);
            if (heads[key] == std.math.maxInt(u32)) heads[key] = @intCast(item) else next[tails[key]] = @intCast(item);
            tails[key] = @intCast(item);
            try csr.add(testing.allocator, key, @intCast(item));
            try sorted.add(testing.allocator, key, @intCast(item));
        }
        var csr_groups = try csr.build(testing.allocator);
        defer csr_groups.deinit(testing.allocator);
        var reversed_groups = try csr.buildReversed(testing.allocator);
        defer reversed_groups.deinit(testing.allocator);
        var column_groups = try CsrGroups(u32).fromKeyColumn(testing.allocator, key_count, keys, std.math.maxInt(u32));
        defer column_groups.deinit(testing.allocator);
        sorted.finish();
        for (0..key_count) |key| {
            var expected: std.ArrayListUnmanaged(u32) = .empty;
            defer expected.deinit(testing.allocator);
            var cursor = heads[key];
            while (cursor != std.math.maxInt(u32)) : (cursor = next[cursor]) try expected.append(testing.allocator, cursor);
            try testing.expectEqualSlices(u32, expected.items, csr_groups.group(key));
            try testing.expectEqualSlices(u32, expected.items, column_groups.group(key));
            const run = sorted.group(@intCast(key));
            try testing.expectEqual(expected.items.len, run.len);
            for (expected.items, run) |want, pair| try testing.expectEqual(want, pair.item);
            std.mem.reverse(u32, expected.items);
            try testing.expectEqualSlices(u32, expected.items, reversed_groups.group(key));
        }
    }
}

test "csr rejects an unrepresentable key domain" {
    const builder = CsrGroups(u32).Builder.init(std.math.maxInt(usize));
    try testing.expectError(error.OutOfMemory, builder.build(testing.allocator));
}

fn exerciseGroupAllocationFailures(gpa: Allocator) (Allocator.Error || error{TestExpectedEqual})!void {
    var builder = CsrGroups(u32).Builder.init(3);
    defer builder.deinit(gpa);
    try builder.add(gpa, 1, 7);
    try builder.add(gpa, 1, 9);
    var groups = try builder.build(gpa);
    defer groups.deinit(gpa);
    try testing.expectEqualSlices(u32, &.{ 7, 9 }, groups.group(1));
}

test "csr construction releases allocations on failure" {
    try testing.checkAllAllocationFailures(testing.allocator, exerciseGroupAllocationFailures, .{});
}
