//! Collections optimized for small sizes and zero-cost deserialization.
//! Uses sorted arrays with binary search for lookups.
//!
//! This module provides:
//! - SortedArrayBuilder: A builder for creating sorted arrays directly
//! - FrozenInternMap: Maps from u32 intern indices to values
//! - BuildableFrozenInternMap: A mutable version that can be frozen
//!
//! These are designed for data that is built during compilation and then frozen,
//! never to be modified again. The sorted array representation provides:
//! - Deterministic serialization
//! - Zero deserialization overhead (can be memory-mapped directly)
//! - Efficient binary search lookups
//! - Good cache locality
//! - No wasted space or uninitialized memory

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
                } else if (@typeInfo(K) == .Int or @typeInfo(K) == .Enum) {
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
        pub fn serializeInto(self: *Self, allocator: Allocator, buffer: []u8) ![]u8 {
            self.ensureSorted(allocator);

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
    };
}

/// A frozen map from string intern indices to values
pub fn FrozenInternMap(comptime T: type) type {
    return struct {
        entries: []const Entry,

        const Self = @This();

        pub const Entry = struct {
            key: u32, // String intern index
            value: T,

            fn lessThan(_: void, a: Entry, b: Entry) bool {
                return a.key < b.key;
            }
        };

        /// Create from a builder that was using intern indices
        pub fn fromBuilder(allocator: Allocator, builder: anytype) !Self {
            const entries = try allocator.alloc(Entry, builder.entries.items.len);
            @memcpy(entries, builder.entries.items);
            return Self{ .entries = entries };
        }

        /// Look up a value by its intern index
        pub fn get(self: Self, key: u32) ?T {
            if (self.entries.len == 0) return null;

            // Binary search
            var left: usize = 0;
            var right: usize = self.entries.len;

            while (left < right) {
                const mid = left + (right - left) / 2;
                const mid_key = self.entries[mid].key;

                if (mid_key == key) {
                    return self.entries[mid].value;
                } else if (mid_key < key) {
                    left = mid + 1;
                } else {
                    right = mid;
                }
            }

            return null;
        }

        /// Check if an intern index exists in the map
        pub fn contains(self: Self, key: u32) bool {
            return self.get(key) != null;
        }

        /// Get the number of entries
        pub fn count(self: Self) usize {
            return self.entries.len;
        }

        /// Iterator
        pub const Iterator = struct {
            entries: []const Entry,
            index: usize = 0,

            pub fn next(self: *Iterator) ?Entry {
                if (self.index >= self.entries.len) return null;
                const entry = self.entries[self.index];
                self.index += 1;
                return entry;
            }
        };

        pub fn iterator(self: Self) Iterator {
            return .{ .entries = self.entries };
        }

        /// Free memory
        pub fn deinit(self: *Self, allocator: Allocator) void {
            allocator.free(self.entries);
            self.entries = &.{};
        }

        /// Serialize to IovecWriter
        pub fn appendToIovecs(self: Self, writer: anytype) !void {
            // Write count
            const count_val: u32 = @intCast(self.entries.len);
            try writer.appendU32(count_val);

            // Write entries
            for (self.entries) |entry| {
                try writer.appendU32(entry.key);
                if (@sizeOf(T) > 0) {
                    try writer.appendValue(T, entry.value);
                }
            }
        }

        /// Deserialize
        pub fn deserializeFrom(reader: anytype, allocator: Allocator) !Self {
            const entry_count = try reader.readU32();

            if (entry_count == 0) {
                return Self{ .entries = &.{} };
            }

            const entries = try allocator.alloc(Entry, entry_count);
            errdefer allocator.free(entries);

            for (entries) |*entry| {
                entry.key = try reader.readU32();
                if (@sizeOf(T) > 0) {
                    entry.value = try reader.readValue(T);
                }
            }

            // Verify sorted order in debug builds
            if (std.debug.runtime_safety) {
                for (entries[1..], 0..) |entry, i| {
                    std.debug.assert(entries[i].key < entry.key);
                }
            }

            return Self{ .entries = entries };
        }

        /// Get serialized size
        pub fn serializedSize(self: Self) usize {
            return @sizeOf(u32) + // count
                self.entries.len * (@sizeOf(u32) + @sizeOf(T)); // entries
        }
    };
}

/// A buildable map that stores intern indices and can be frozen
pub fn BuildableFrozenInternMap(comptime T: type) type {
    return struct {
        entries: std.ArrayListUnmanaged(Entry) = .{},
        sorted: bool = true,

        const Self = @This();
        const Entry = FrozenInternMap(T).Entry;

        pub fn init() Self {
            return .{};
        }

        pub fn deinit(self: *Self, allocator: Allocator) void {
            self.entries.deinit(allocator);
        }

        /// Add an intern index and value
        pub fn put(self: *Self, allocator: Allocator, key: u32, value: T) !void {
            // Check if we need to maintain sorted order
            if (self.sorted and self.entries.items.len > 0) {
                const last = self.entries.items[self.entries.items.len - 1];
                self.sorted = last.key < key;
            }

            try self.entries.append(allocator, .{ .key = key, .value = value });
        }

        /// Get value by intern index (requires sorting first if not already sorted)
        pub fn get(self: *Self, key: u32) ?T {
            self.ensureSorted();

            var left: usize = 0;
            var right: usize = self.entries.items.len;

            while (left < right) {
                const mid = left + (right - left) / 2;
                const mid_key = self.entries.items[mid].key;

                if (mid_key == key) {
                    return self.entries.items[mid].value;
                } else if (mid_key < key) {
                    left = mid + 1;
                } else {
                    right = mid;
                }
            }
            return null;
        }

        /// Get value by intern index (const version, must already be sorted)
        pub fn getConst(self: *const Self, key: u32) ?T {
            if (!self.sorted) {
                @panic("BuildableFrozenInternMap must be sorted before const access");
            }

            var left: usize = 0;
            var right: usize = self.entries.items.len;

            while (left < right) {
                const mid = left + (right - left) / 2;
                const mid_key = self.entries.items[mid].key;

                if (mid_key == key) {
                    return self.entries.items[mid].value;
                } else if (mid_key < key) {
                    left = mid + 1;
                } else {
                    right = mid;
                }
            }
            return null;
        }

        /// Ensure the array is sorted
        pub fn ensureSorted(self: *Self) void {
            if (!self.sorted) {
                std.sort.pdq(Entry, self.entries.items, {}, Entry.lessThan);
                self.sorted = true;
            }
        }

        /// Get the number of entries
        pub fn count(self: *const Self) usize {
            return self.entries.items.len;
        }

        /// Check if an intern index exists
        pub fn contains(self: *Self, key: u32) bool {
            return self.get(key) != null;
        }

        /// Check if an intern index exists (const version)
        pub fn containsConst(self: *const Self, key: u32) bool {
            return self.getConst(key) != null;
        }

        /// Freeze into a FrozenInternMap
        pub fn freeze(self: *Self, allocator: Allocator) !FrozenInternMap(T) {
            self.ensureSorted();
            return try FrozenInternMap(T).fromBuilder(allocator, self);
        }

        /// Serialize
        pub fn serializeInto(self: *const Self, buffer: []u8) ![]u8 {
            if (!self.sorted) {
                @panic("BuildableFrozenInternMap must be sorted before serialization");
            }

            var offset: usize = 0;

            // Write count
            if (buffer.len < @sizeOf(u32)) return error.BufferTooSmall;
            std.mem.writeInt(u32, buffer[0..4], @intCast(self.entries.items.len), .little);
            offset += @sizeOf(u32);

            // Write entries
            for (self.entries.items) |entry| {
                if (offset + @sizeOf(u32) > buffer.len) return error.BufferTooSmall;
                std.mem.writeInt(u32, buffer[offset..][0..4], entry.key, .little);
                offset += @sizeOf(u32);

                if (@sizeOf(T) > 0) {
                    if (offset + @sizeOf(T) > buffer.len) return error.BufferTooSmall;
                    @memcpy(buffer[offset..][0..@sizeOf(T)], std.mem.asBytes(&entry.value));
                    offset += @sizeOf(T);
                }
            }

            return buffer[0..offset];
        }

        /// Get serialized size
        pub fn serializedSize(self: *const Self) usize {
            return @sizeOf(u32) + // count
                self.entries.items.len * (@sizeOf(u32) + @sizeOf(T)); // entries
        }

        /// Deserialize
        pub fn deserializeFrom(buffer: []const u8, allocator: Allocator) !Self {
            var reader = struct {
                buffer: []const u8,
                offset: usize = 0,

                pub fn readU32(self: *@This()) !u32 {
                    if (self.offset + 4 > self.buffer.len) return error.BufferTooSmall;
                    const value = std.mem.readInt(u32, self.buffer[self.offset..][0..4], .little);
                    self.offset += 4;
                    return value;
                }

                pub fn readValue(self: *@This(), comptime V: type) !V {
                    if (self.offset + @sizeOf(V) > self.buffer.len) return error.BufferTooSmall;
                    const value = std.mem.bytesToValue(V, self.buffer[self.offset..][0..@sizeOf(V)]);
                    self.offset += @sizeOf(V);
                    return value;
                }
            }{ .buffer = buffer };

            const frozen = try FrozenInternMap(T).deserializeFrom(&reader, allocator);

            // Convert frozen entries to buildable
            var self = Self.init();
            try self.entries.ensureTotalCapacity(allocator, frozen.entries.len);
            self.entries.appendSliceAssumeCapacity(frozen.entries);
            self.sorted = true;

            // Free the frozen map's allocation
            allocator.free(frozen.entries);

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
