//! A builder for creating sorted arrays with binary search lookups.
//!
//! SortedArrayBuilder provides a way to build key-value mappings that are
//! optimized for small sizes and deterministic serialization. It maintains
//! a sorted array internally, enabling efficient binary search lookups.
//!
//! Features:
//! - Supports both string and numeric key types
//! - Maintains sorted order for deterministic serialization
//! - Efficient binary search lookups O(log n)
//! - Zero-copy deserialization
//! - Minimal memory overhead

const std = @import("std");
const Allocator = std.mem.Allocator;

const CompactWriter = @import("CompactWriter.zig");
const SafeList = @import("safe_list.zig").SafeList;

/// A builder for creating sorted arrays directly without using hash maps
/// This is more efficient when we know we won't have duplicates
/// Uses extern struct to guarantee consistent field layout across optimization levels.
pub fn SortedArrayBuilder(comptime K: type, comptime V: type) type {
    return extern struct {
        entries: SafeList(Entry) = .{},
        sorted: bool = true,
        _padding: [7]u8 = .{ 0, 0, 0, 0, 0, 0, 0 }, // Padding after bool for alignment

        const Self = @This();

        // Entry is extern when both K and V are extern-compatible (not slices)
        const is_extern_compatible = @typeInfo(K) != .pointer and @typeInfo(V) != .pointer;
        pub const Entry = if (is_extern_compatible) extern struct {
            key: K,
            value: V,

            fn lessThan(_: void, a: @This(), b: @This()) bool {
                if (@typeInfo(K) == .int or @typeInfo(K) == .@"enum") {
                    return a.key < b.key;
                } else {
                    @compileError("Unsupported key type for extern SortedArrayBuilder Entry");
                }
            }
        } else struct {
            key: K,
            value: V,

            fn lessThan(_: void, a: @This(), b: @This()) bool {
                if (K == []const u8) {
                    return std.mem.lessThan(u8, a.key, b.key);
                } else if (@typeInfo(K) == .int or @typeInfo(K) == .@"enum") {
                    return a.key < b.key;
                } else {
                    @compileError("Unsupported key type for SortedArrayBuilder");
                }
            }
        };

        pub fn init() Self {
            return .{};
        }

        pub fn deinit(self: *Self, allocator: Allocator) void {
            if (K == []const u8) {
                // Free string keys
                for (self.entries.items.toSlice()) |entry| {
                    allocator.free(entry.key);
                }
            }
            self.entries.deinit(allocator);
        }

        /// Add a key-value pair
        pub fn put(self: *Self, allocator: Allocator, key: K, value: V) !void {
            const new_key = if (K == []const u8) try allocator.dupe(u8, key) else key;

            // Check if we need to maintain sorted order
            const entries_slice = self.entries.items.toSlice();
            if (self.sorted and entries_slice.len > 0) {
                const last = entries_slice[entries_slice.len - 1];
                if (K == []const u8) {
                    self.sorted = std.mem.lessThan(u8, last.key, new_key);
                } else {
                    self.sorted = last.key < new_key;
                }
            }

            _ = try self.entries.append(allocator, .{ .key = new_key, .value = value });
        }

        /// Get value by key (requires sorting first if not already sorted)
        pub fn get(self: *Self, allocator: Allocator, key: K) ?V {
            self.ensureSorted(allocator);

            const entries_slice = self.entries.items.toSlice();
            var left: usize = 0;
            var right: usize = entries_slice.len;

            while (left < right) {
                const mid = left + (right - left) / 2;
                const mid_key = entries_slice[mid].key;

                const cmp = if (K == []const u8)
                    std.mem.order(u8, mid_key, key)
                else if (mid_key == key)
                    std.math.Order.eq
                else if (mid_key < key)
                    std.math.Order.lt
                else
                    std.math.Order.gt;

                switch (cmp) {
                    .eq => return entries_slice[mid].value,
                    .lt => left = mid + 1,
                    .gt => right = mid,
                }
            }
            return null;
        }

        /// Ensure the array is sorted and deduplicate any duplicate entries
        pub fn ensureSorted(self: *Self, allocator: Allocator) void {
            if (!self.sorted) {
                std.sort.pdq(Entry, self.entries.items.toSlice(), {}, Entry.lessThan);
                self.sorted = true;
            }

            // Always deduplicate when ensureSorted is called
            // This ensures we handle cases where detectDuplicates was called first
            self.deduplicateAndReport(allocator);
        }

        /// Check for duplicates, report them, and remove duplicates keeping the last occurrence
        fn deduplicateAndReport(self: *Self, allocator: Allocator) void {
            const entries_slice = self.entries.items.toSlice();
            if (entries_slice.len <= 1) return;

            // First pass: detect and report duplicates
            var i: usize = 1;
            while (i < entries_slice.len) {
                const prev_entry = entries_slice[i - 1];
                const curr_entry = entries_slice[i];
                const is_duplicate = if (K == []const u8)
                    std.mem.eql(u8, prev_entry.key, curr_entry.key)
                else
                    prev_entry.key == curr_entry.key;

                if (is_duplicate) {
                    // Note: Duplicate detection is handled by caller via detectDuplicates() method
                }
                i += 1;
            }

            // Second pass: deduplicate by keeping last occurrence
            const mutable_slice = self.entries.items.toSlice();
            var write_index: usize = 0;
            for (mutable_slice, 0..) |entry, read_index| {
                var should_keep = true;

                // Look ahead to see if there's a duplicate later
                for (mutable_slice[read_index + 1 ..]) |future_entry| {
                    const is_duplicate = if (K == []const u8)
                        std.mem.eql(u8, entry.key, future_entry.key)
                    else
                        entry.key == future_entry.key;

                    if (is_duplicate) {
                        should_keep = false;
                        break;
                    }
                }

                if (should_keep) {
                    if (write_index != read_index) {
                        mutable_slice[write_index] = entry;
                    }
                    write_index += 1;
                } else {
                    // Free the string key that we're not keeping
                    if (K == []const u8) {
                        allocator.free(entry.key);
                    }
                }
            }

            // Update the length to reflect deduplicated entries
            self.entries.items.setLen(write_index);
        }

        /// Detect duplicates without modifying the array - returns list of duplicate keys
        pub fn detectDuplicates(self: *Self, allocator: Allocator) ![]K {
            var duplicates = std.array_list.Managed(K).init(allocator);

            const entries_slice = self.entries.items.toSlice();
            if (entries_slice.len <= 1) return duplicates.toOwnedSlice();

            // Ensure sorted first
            if (!self.sorted) {
                std.sort.pdq(Entry, self.entries.items.toSlice(), {}, Entry.lessThan);
                self.sorted = true;
            }

            var reported_keys = if (K == []const u8)
                std.StringHashMap(void).init(allocator)
            else
                std.AutoHashMap(K, void).init(allocator);
            defer reported_keys.deinit();

            const sorted_entries = self.entries.items.toSlice();
            var i: usize = 1;
            while (i < sorted_entries.len) {
                const prev_entry = sorted_entries[i - 1];
                const curr_entry = sorted_entries[i];
                const is_duplicate = if (K == []const u8)
                    std.mem.eql(u8, prev_entry.key, curr_entry.key)
                else
                    prev_entry.key == curr_entry.key;

                if (is_duplicate) {
                    // Report duplicate only once per unique key
                    const result = try reported_keys.getOrPut(curr_entry.key);
                    if (!result.found_existing) {
                        try duplicates.append(curr_entry.key);
                    }
                }
                i += 1;
            }

            return duplicates.toOwnedSlice();
        }

        /// Relocate pointers after memory movement
        pub fn relocate(self: *Self, offset: isize) void {
            // SafeList has its own relocate method
            self.entries.relocate(offset);
        }

        /// Get the number of entries
        pub fn count(self: *const Self) usize {
            return self.entries.len();
        }

        /// Check if there are no duplicates (assumes sorted)
        pub fn isDeduplicated(self: *const Self) bool {
            const entries = self.entries.items.toSlice();
            if (entries.len <= 1) return true;

            for (1..entries.len) |i| {
                const is_duplicate = if (K == []const u8)
                    std.mem.eql(u8, entries[i - 1].key, entries[i].key)
                else
                    entries[i - 1].key == entries[i].key;

                if (is_duplicate) {
                    return false;
                }
            }

            return true;
        }

        /// Serialized representation of SortedArrayBuilder
        /// Since SortedArrayBuilder is now extern, Serialized is just an alias.
        pub const Serialized = SortedArrayBuilder(K, V);

        /// Serialize a SortedArrayBuilder into this Serialized struct, appending data to the writer
        pub fn serialize(
            self: *Self,
            source: *const SortedArrayBuilder(K, V),
            allocator: Allocator,
            writer: *CompactWriter,
        ) Allocator.Error!void {
            // Serialize the entries SafeList
            try self.entries.items.serialize(&source.entries.items, allocator, writer);
            self.sorted = source.sorted;
            self._padding = .{ 0, 0, 0, 0, 0, 0, 0 };
        }

        /// Deserialize this SortedArrayBuilder in place
        pub fn deserialize(self: *Self, offset: i64) *SortedArrayBuilder(K, V) {
            // SafeSlice has its own deserialize method
            self.entries.items.deserialize(offset);
            return self;
        }
    };
}

test "SortedArrayBuilder maintains sorted order when added in order" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var builder = SortedArrayBuilder([]const u8, u16).init();
    defer builder.deinit(allocator);

    // Add items in sorted order
    try builder.put(allocator, "aaa", 1);
    try builder.put(allocator, "bbb", 2);
    try builder.put(allocator, "ccc", 3);
    try builder.put(allocator, "ddd", 4);

    // Should still be marked as sorted
    try testing.expect(builder.sorted);

    // Get should work without sorting
    try testing.expectEqual(@as(?u16, 1), builder.get(allocator, "aaa"));
    try testing.expectEqual(@as(?u16, 4), builder.get(allocator, "ddd"));
}

test "SortedArrayBuilder detectDuplicates sorts if unsorted" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var builder = SortedArrayBuilder(u32, u16).init();
    defer builder.deinit(allocator);

    // Add some entries with duplicates
    try builder.put(allocator, 100, 1);
    try builder.put(allocator, 200, 2);
    try builder.put(allocator, 100, 3); // duplicate of 100
    try builder.put(allocator, 300, 4);
    try builder.put(allocator, 200, 5); // duplicate of 200

    // Detect duplicates before sorting/deduplicating
    const duplicates = try builder.detectDuplicates(allocator);
    defer allocator.free(duplicates);

    // Verify we found the expected duplicates
    try testing.expectEqual(@as(usize, 2), duplicates.len);
    try testing.expectEqual(@as(u32, 100), duplicates[0]);
    try testing.expectEqual(@as(u32, 200), duplicates[1]);

    // After detection, normal operations work as expected
    builder.ensureSorted(allocator);
    try testing.expectEqual(@as(usize, 3), builder.count()); // Deduplicated
    try testing.expectEqual(@as(?u16, 3), builder.get(allocator, 100)); // Last value kept
    try testing.expectEqual(@as(?u16, 5), builder.get(allocator, 200)); // Last value kept
}

test "SortedArrayBuilder handles duplicates with string keys" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var builder = SortedArrayBuilder([]const u8, u32).init();
    defer builder.deinit(allocator);

    // Add duplicate keys with different values
    try builder.put(allocator, "apple", 10);
    try builder.put(allocator, "banana", 20);
    try builder.put(allocator, "apple", 30); // duplicate key
    try builder.put(allocator, "cherry", 40);
    try builder.put(allocator, "banana", 50); // duplicate key

    // Initial count includes duplicates
    try testing.expectEqual(@as(usize, 5), builder.count());

    // Force sorting and deduplication
    builder.ensureSorted(allocator);

    // Count should be reduced after deduplication
    try testing.expectEqual(@as(usize, 3), builder.count());

    // Should return the last value for each key (keeping last occurrence)
    try testing.expectEqual(@as(?u32, 30), builder.get(allocator, "apple"));
    try testing.expectEqual(@as(?u32, 50), builder.get(allocator, "banana"));
    try testing.expectEqual(@as(?u32, 40), builder.get(allocator, "cherry"));
}

test "SortedArrayBuilder handles duplicates with numeric keys" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var builder = SortedArrayBuilder(u32, []const u8).init();
    defer builder.deinit(allocator);

    // Add duplicate keys with different values
    try builder.put(allocator, 100, "first");
    try builder.put(allocator, 200, "second");
    try builder.put(allocator, 100, "updated_first"); // duplicate key
    try builder.put(allocator, 300, "third");

    // Initial count includes duplicates
    try testing.expectEqual(@as(usize, 4), builder.count());

    // Force sorting and deduplication
    builder.ensureSorted(allocator);

    // Count should be reduced after deduplication
    try testing.expectEqual(@as(usize, 3), builder.count());

    // Should return the last value for each key
    try testing.expectEqual(@as(?[]const u8, "updated_first"), builder.get(allocator, 100));
    try testing.expectEqual(@as(?[]const u8, "second"), builder.get(allocator, 200));
    try testing.expectEqual(@as(?[]const u8, "third"), builder.get(allocator, 300));
}

test "SortedArrayBuilder no duplicates case" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var builder = SortedArrayBuilder([]const u8, u16).init();
    defer builder.deinit(allocator);

    // Add unique keys only
    try builder.put(allocator, "unique1", 1);
    try builder.put(allocator, "unique2", 2);
    try builder.put(allocator, "unique3", 3);

    const original_count = builder.count();

    // Force sorting (should not change count)
    builder.ensureSorted(allocator);

    // Count should remain the same
    try testing.expectEqual(original_count, builder.count());

    // All values should be retrievable
    try testing.expectEqual(@as(?u16, 1), builder.get(allocator, "unique1"));
    try testing.expectEqual(@as(?u16, 2), builder.get(allocator, "unique2"));
    try testing.expectEqual(@as(?u16, 3), builder.get(allocator, "unique3"));
}
