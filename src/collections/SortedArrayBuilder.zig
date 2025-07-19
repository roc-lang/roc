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

/// A builder for creating sorted arrays directly without using hash maps
/// This is more efficient when we know we won't have duplicates
pub fn SortedArrayBuilder(comptime K: type, comptime V: type) type {
    return struct {
        entries: std.ArrayListUnmanaged(Entry) = .{},
        sorted: bool = true,

        const Self = @This();

        pub const Entry = struct {
            key: K,
            value: V,

            fn lessThan(_: void, a: Entry, b: Entry) bool {
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
                for (self.entries.items) |entry| {
                    allocator.free(entry.key);
                }
            }
            self.entries.deinit(allocator);
        }

        /// Add a key-value pair
        pub fn put(self: *Self, allocator: Allocator, key: K, value: V) !void {
            const new_key = if (K == []const u8) try allocator.dupe(u8, key) else key;

            // Check if we need to maintain sorted order
            if (self.sorted and self.entries.items.len > 0) {
                const last = self.entries.items[self.entries.items.len - 1];
                if (K == []const u8) {
                    self.sorted = std.mem.lessThan(u8, last.key, new_key);
                } else {
                    self.sorted = last.key < new_key;
                }
            }

            try self.entries.append(allocator, .{ .key = new_key, .value = value });
        }

        /// Get value by key (requires sorting first if not already sorted)
        pub fn get(self: *Self, allocator: Allocator, key: K) ?V {
            self.ensureSorted(allocator);

            var left: usize = 0;
            var right: usize = self.entries.items.len;

            while (left < right) {
                const mid = left + (right - left) / 2;
                const mid_key = self.entries.items[mid].key;

                const cmp = if (K == []const u8)
                    std.mem.order(u8, mid_key, key)
                else if (mid_key == key)
                    std.math.Order.eq
                else if (mid_key < key)
                    std.math.Order.lt
                else
                    std.math.Order.gt;

                switch (cmp) {
                    .eq => return self.entries.items[mid].value,
                    .lt => left = mid + 1,
                    .gt => right = mid,
                }
            }
            return null;
        }

        /// Ensure the array is sorted
        pub fn ensureSorted(self: *Self, allocator: Allocator) void {
            _ = allocator;
            if (!self.sorted) {
                std.sort.pdq(Entry, self.entries.items, {}, Entry.lessThan);
                self.sorted = true;
            }
        }

        /// Get the number of entries
        pub fn count(self: *const Self) usize {
            return self.entries.items.len;
        }

        /// Serialize directly (must be sorted first)
        pub fn serializeInto(self: *const Self, allocator: Allocator, buffer: []u8) ![]u8 {
            _ = allocator; // Not needed for serialization, but kept for API consistency
            // Note: caller must ensure the array is sorted before serialization
            if (!self.sorted) {
                @panic("SortedArrayBuilder must be sorted before serialization");
            }

            var offset: usize = 0;

            // Write count
            if (buffer.len < @sizeOf(u32)) return error.BufferTooSmall;
            std.mem.writeInt(u32, buffer[0..4], @intCast(self.entries.items.len), .little);
            offset += @sizeOf(u32);

            // Write entries
            for (self.entries.items) |entry| {
                if (K == []const u8) {
                    // String key: write length then data
                    if (offset + @sizeOf(u32) > buffer.len) return error.BufferTooSmall;
                    std.mem.writeInt(u32, buffer[offset..][0..4], @intCast(entry.key.len), .little);
                    offset += @sizeOf(u32);

                    if (offset + entry.key.len > buffer.len) return error.BufferTooSmall;
                    @memcpy(buffer[offset..][0..entry.key.len], entry.key);
                    offset += entry.key.len;
                } else {
                    // Numeric key
                    if (offset + @sizeOf(K) > buffer.len) return error.BufferTooSmall;
                    @memcpy(buffer[offset..][0..@sizeOf(K)], std.mem.asBytes(&entry.key));
                    offset += @sizeOf(K);
                }

                // Write value
                if (@sizeOf(V) > 0) {
                    if (offset + @sizeOf(V) > buffer.len) return error.BufferTooSmall;
                    @memcpy(buffer[offset..][0..@sizeOf(V)], std.mem.asBytes(&entry.value));
                    offset += @sizeOf(V);
                }
            }

            return buffer[0..offset];
        }

        /// Get serialized size
        pub fn serializedSize(self: *const Self) usize {
            var size: usize = @sizeOf(u32); // count

            for (self.entries.items) |entry| {
                if (K == []const u8) {
                    size += @sizeOf(u32) + entry.key.len; // key length + key data
                } else {
                    size += @sizeOf(K); // key
                }
                size += @sizeOf(V); // value
            }

            return size;
        }

        /// Deserialize from buffer
        pub fn deserializeFrom(buffer: []const u8, allocator: Allocator) !Self {
            if (buffer.len < @sizeOf(u32)) return error.BufferTooSmall;

            var offset: usize = 0;

            // Read count
            const entry_count = std.mem.readInt(u32, buffer[offset..][0..4], .little);
            offset += @sizeOf(u32);

            // Create builder with capacity
            var self = Self.init();
            try self.entries.ensureTotalCapacity(allocator, entry_count);

            // Read entries
            for (0..entry_count) |_| {
                // Read key based on type
                const key = if (K == []const u8) blk: {
                    if (offset + @sizeOf(u32) > buffer.len) return error.BufferTooSmall;
                    const key_len = std.mem.readInt(u32, buffer[offset..][0..4], .little);
                    offset += @sizeOf(u32);

                    if (offset + key_len > buffer.len) return error.BufferTooSmall;
                    const key_data = buffer[offset..][0..key_len];
                    offset += key_len;

                    // Allocate and copy the key
                    const key_copy = try allocator.dupe(u8, key_data);
                    break :blk key_copy;
                } else blk: {
                    if (offset + @sizeOf(K) > buffer.len) return error.BufferTooSmall;
                    const key = std.mem.bytesToValue(K, buffer[offset..][0..@sizeOf(K)]);
                    offset += @sizeOf(K);
                    break :blk key;
                };

                // Read value
                const value = if (@sizeOf(V) > 0) blk: {
                    if (offset + @sizeOf(V) > buffer.len) return error.BufferTooSmall;
                    const val = std.mem.bytesToValue(V, buffer[offset..][0..@sizeOf(V)]);
                    offset += @sizeOf(V);
                    break :blk val;
                } else {};

                self.entries.appendAssumeCapacity(.{ .key = key, .value = value });
            }

            // Mark as sorted since serialized data should be sorted
            self.sorted = true;

            return self;
        }
    };
}

test "SortedArrayBuilder basic operations" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var builder = SortedArrayBuilder([]const u8, u32).init();
    defer builder.deinit(allocator);

    // Add items in random order
    try builder.put(allocator, "zebra", 100);
    try builder.put(allocator, "apple", 50);
    try builder.put(allocator, "banana", 150);
    try builder.put(allocator, "cherry", 30);

    // Test count
    try testing.expectEqual(@as(usize, 4), builder.count());

    // Test get (forces sorting)
    try testing.expectEqual(@as(?u32, 50), builder.get(allocator, "apple"));
    try testing.expectEqual(@as(?u32, 150), builder.get(allocator, "banana"));
    try testing.expectEqual(@as(?u32, 30), builder.get(allocator, "cherry"));
    try testing.expectEqual(@as(?u32, 100), builder.get(allocator, "zebra"));
    try testing.expectEqual(@as(?u32, null), builder.get(allocator, "missing"));

    // Test serialization
    const size = builder.serializedSize();
    const buffer = try allocator.alloc(u8, size);
    defer allocator.free(buffer);

    const serialized = try builder.serializeInto(allocator, buffer);
    try testing.expectEqual(size, serialized.len);

    // Verify serialized data starts with count
    const count = std.mem.readInt(u32, serialized[0..4], .little);
    try testing.expectEqual(@as(u32, 4), count);
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
