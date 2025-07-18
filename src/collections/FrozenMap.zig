//! Read-only maps optimized for small sizes and zero-cost deserialization.
//! Uses sorted arrays with binary search for lookups.
//!
//! This module provides two map types:
//! - FrozenStringMap: Maps from strings to values
//! - FrozenU32Map: Maps from u32 indices to values
//!
//! Both are designed for maps that are built during compilation and then frozen,
//! never to be modified again. The sorted array representation provides:
//! - Deterministic serialization
//! - Zero deserialization overhead (can be memory-mapped directly)
//! - Good cache locality
//! - No wasted space or uninitialized memory

const std = @import("std");
const Allocator = std.mem.Allocator;

/// A frozen map from strings to values
pub fn FrozenStringMap(comptime T: type) type {
    return struct {
        entries: []const Entry,

        const Self = @This();

        pub const Entry = struct {
            key: []const u8, // String key
            value: T,

            fn lessThan(_: void, a: Entry, b: Entry) bool {
                return std.mem.lessThan(u8, a.key, b.key);
            }
        };

        /// Create a FrozenStringMap from a hash map
        pub fn fromHashMap(allocator: Allocator, map: anytype) !Self {
            const entries = try allocator.alloc(Entry, map.count());
            errdefer allocator.free(entries);

            var iter = map.iterator();
            var i: usize = 0;
            while (iter.next()) |entry| : (i += 1) {
                // Duplicate the key string since we need to own it
                const key_copy = try allocator.dupe(u8, entry.key_ptr.*);
                entries[i] = .{
                    .key = key_copy,
                    .value = entry.value_ptr.*,
                };
            }

            // Sort by key for binary search
            std.sort.pdq(Entry, entries, {}, Entry.lessThan);

            return Self{ .entries = entries };
        }

        /// Look up a value by its string key
        pub fn get(self: Self, key: []const u8) ?T {
            if (self.entries.len == 0) return null;

            // Binary search
            var left: usize = 0;
            var right: usize = self.entries.len;

            while (left < right) {
                const mid = left + (right - left) / 2;
                const mid_key = self.entries[mid].key;
                const cmp = std.mem.order(u8, mid_key, key);

                switch (cmp) {
                    .eq => return self.entries[mid].value,
                    .lt => left = mid + 1,
                    .gt => right = mid,
                }
            }

            return null;
        }

        /// Check if a key exists in the map
        pub fn contains(self: Self, key: []const u8) bool {
            return self.get(key) != null;
        }

        /// Get the number of entries
        pub fn count(self: Self) usize {
            return self.entries.len;
        }

        /// Iterator for the map
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

        /// Free the memory used by this map
        pub fn deinit(self: *Self, allocator: Allocator) void {
            // Free all the key strings
            for (self.entries) |entry| {
                allocator.free(entry.key);
            }
            allocator.free(self.entries);
            self.entries = &.{};
        }

        /// Serialize to an IovecWriter
        pub fn appendToIovecs(self: Self, writer: anytype) !void {
            // Write count
            const count_val: u32 = @intCast(self.entries.len);
            try writer.appendU32(count_val);

            // Write entries directly - they're already sorted
            for (self.entries) |entry| {
                // Write key length and data
                try writer.appendU32(@intCast(entry.key.len));
                try writer.appendBytes(entry.key);
                if (@sizeOf(T) > 0) {
                    try writer.appendValue(T, entry.value);
                }
            }
        }

        /// Deserialize from bytes
        pub fn deserializeFrom(reader: anytype, allocator: Allocator) !Self {
            const entry_count = try reader.readU32();

            if (entry_count == 0) {
                return Self{ .entries = &.{} };
            }

            const entries = try allocator.alloc(Entry, entry_count);
            errdefer allocator.free(entries);

            for (entries) |*entry| {
                // Read key length
                const key_len = try reader.readU32();

                // Allocate and read key
                const key = try allocator.alloc(u8, key_len);
                try reader.readBytes(key);
                entry.key = key;

                if (@sizeOf(T) > 0) {
                    entry.value = try reader.readValue(T);
                }
            }

            // Verify sorted order in debug builds
            if (std.debug.runtime_safety) {
                for (entries[1..], 0..) |entry, i| {
                    std.debug.assert(std.mem.lessThan(u8, entries[i].key, entry.key));
                }
            }

            return Self{ .entries = entries };
        }

        /// Get the serialized size in bytes
        pub fn serializedSize(self: Self) usize {
            var size: usize = @sizeOf(u32); // count
            for (self.entries) |entry| {
                size += @sizeOf(u32) + entry.key.len + @sizeOf(T); // key length + key + value
            }
            return size;
        }
    };
}

/// A frozen map from u32 indices to values
pub fn FrozenU32Map(comptime T: type) type {
    return struct {
        entries: []const Entry,

        const Self = @This();

        pub const Entry = struct {
            key: u32,
            value: T,

            fn lessThan(_: void, a: Entry, b: Entry) bool {
                return a.key < b.key;
            }
        };

        /// Create from a hash map with u32 keys
        pub fn fromHashMap(allocator: Allocator, map: anytype) !Self {
            const entries = try allocator.alloc(Entry, map.count());
            errdefer allocator.free(entries);

            var iter = map.iterator();
            var i: usize = 0;
            while (iter.next()) |entry| : (i += 1) {
                entries[i] = .{
                    .key = entry.key_ptr.*,
                    .value = entry.value_ptr.*,
                };
            }

            // Sort by key for binary search
            std.sort.pdq(Entry, entries, {}, Entry.lessThan);

            return Self{ .entries = entries };
        }

        /// Look up a value by its u32 key
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

        /// Check if a key exists
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

/// A string map that can be built up and then frozen for efficient read-only access
pub fn BuildableFrozenStringMap(comptime T: type) type {
    return struct {
        map: std.StringHashMapUnmanaged(T) = .{},
        frozen: ?FrozenStringMap(T) = null,

        const Self = @This();

        /// Initialize an empty buildable map
        pub fn init() Self {
            return .{};
        }

        /// Initialize with capacity
        pub fn initCapacity(allocator: Allocator, capacity: usize) !Self {
            var map = std.StringHashMapUnmanaged(T){};
            try map.ensureTotalCapacity(allocator, @intCast(capacity));
            return Self{ .map = map };
        }

        /// Put a key-value pair (only works before freezing)
        pub fn put(self: *Self, allocator: Allocator, key: []const u8, value: T) !void {
            std.debug.assert(self.frozen == null); // Can't modify after freezing
            // Duplicate the key since we need to own it
            const key_copy = try allocator.dupe(u8, key);
            const result = try self.map.getOrPut(allocator, key_copy);
            if (result.found_existing) {
                // Free the old key and replace
                allocator.free(result.key_ptr.*);
                result.key_ptr.* = key_copy;
            }
            result.value_ptr.* = value;
        }

        /// Freeze the map for efficient read-only access
        pub fn freeze(self: *Self, allocator: Allocator) !void {
            if (self.frozen != null) return; // Already frozen
            self.frozen = try FrozenStringMap(T).fromHashMap(allocator, self.map);
            // Clear the original map to free memory
            var iter = self.map.iterator();
            while (iter.next()) |entry| {
                allocator.free(entry.key_ptr.*);
            }
            self.map.deinit(allocator);
            self.map = .{};
        }

        /// Get a value by key
        pub fn get(self: *const Self, key: []const u8) ?T {
            if (self.frozen) |frozen| {
                return frozen.get(key);
            } else {
                return self.map.get(key);
            }
        }

        /// Check if a key exists
        pub fn contains(self: *const Self, key: []const u8) bool {
            if (self.frozen) |frozen| {
                return frozen.contains(key);
            } else {
                return self.map.contains(key);
            }
        }

        /// Get the number of entries
        pub fn count(self: *const Self) usize {
            if (self.frozen) |frozen| {
                return frozen.count();
            } else {
                return self.map.count();
            }
        }

        /// Get an iterator
        pub const Iterator = union(enum) {
            building: std.StringHashMapUnmanaged(T).Iterator,
            frozen: FrozenStringMap(T).Iterator,

            pub fn next(self: *Iterator) ?struct { key: []const u8, value: T } {
                switch (self.*) {
                    .building => |*iter| {
                        if (iter.next()) |entry| {
                            return .{ .key = entry.key_ptr.*, .value = entry.value_ptr.* };
                        }
                        return null;
                    },
                    .frozen => |*iter| {
                        if (iter.next()) |entry| {
                            return .{ .key = entry.key, .value = entry.value };
                        }
                        return null;
                    },
                }
            }
        };

        pub fn iterator(self: *const Self) Iterator {
            if (self.frozen) |frozen| {
                return .{ .frozen = frozen.iterator() };
            } else {
                return .{ .building = self.map.iterator() };
            }
        }

        /// Deinitialize
        pub fn deinit(self: *Self, allocator: Allocator) void {
            if (self.frozen) |*frozen| {
                frozen.deinit(allocator);
            } else {
                // Free all keys in the map
                var iter = self.map.iterator();
                while (iter.next()) |entry| {
                    allocator.free(entry.key_ptr.*);
                }
                self.map.deinit(allocator);
            }
            self.* = .{};
        }

        /// Serialize (must be frozen first)
        pub fn appendToIovecs(self: *const Self, writer: anytype) !void {
            if (self.frozen) |frozen| {
                try frozen.appendToIovecs(writer);
            } else {
                @panic("Must freeze before serializing");
            }
        }

        /// Get serialized size (must be frozen first)
        pub fn serializedSize(self: *const Self) usize {
            if (self.frozen) |frozen| {
                return frozen.serializedSize();
            } else {
                @panic("Must freeze before getting serialized size");
            }
        }

        /// Serialize into buffer (must be frozen first)
        pub fn serializeInto(self: *const Self, buffer: []u8) ![]u8 {
            if (self.frozen) |frozen| {
                const size = frozen.serializedSize();
                if (buffer.len < size) return error.BufferTooSmall;

                // Use a simple writer that just copies to buffer
                var offset: usize = 0;

                // Write count
                const count_val: u32 = @intCast(frozen.entries.len);
                std.mem.writeInt(u32, buffer[offset..][0..4], count_val, .little);
                offset += @sizeOf(u32);

                // Write entries
                for (frozen.entries) |entry| {
                    // Write key length
                    const key_len: u32 = @intCast(entry.key.len);
                    std.mem.writeInt(u32, buffer[offset..][0..4], key_len, .little);
                    offset += @sizeOf(u32);

                    // Write key bytes
                    @memcpy(buffer[offset..][0..entry.key.len], entry.key);
                    offset += entry.key.len;

                    // Write value
                    if (@sizeOf(T) > 0) {
                        @memcpy(buffer[offset..][0..@sizeOf(T)], std.mem.asBytes(&entry.value));
                        offset += @sizeOf(T);
                    }
                }

                return buffer[0..offset];
            } else {
                @panic("Must freeze before serializing");
            }
        }

        /// Deserialize from buffer
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

                pub fn readBytes(self: *@This(), dest: []u8) !void {
                    if (self.offset + dest.len > self.buffer.len) return error.BufferTooSmall;
                    @memcpy(dest, self.buffer[self.offset..][0..dest.len]);
                    self.offset += dest.len;
                }

                pub fn readValue(self: *@This(), comptime V: type) !V {
                    if (self.offset + @sizeOf(V) > self.buffer.len) return error.BufferTooSmall;
                    const value = std.mem.bytesToValue(V, self.buffer[self.offset..][0..@sizeOf(V)]);
                    self.offset += @sizeOf(V);
                    return value;
                }
            }{ .buffer = buffer };

            const frozen = try FrozenStringMap(T).deserializeFrom(&reader, allocator);
            return .{ .frozen = frozen };
        }

        /// Relocate for fixup cache (only works on frozen maps)
        pub fn relocate(self: *Self, offset: isize) void {
            if (self.frozen) |*frozen| {
                // Relocate the entries array pointer
                if (frozen.entries.len > 0) {
                    const old_ptr = @intFromPtr(frozen.entries.ptr);
                    const new_ptr = @as(usize, @intCast(@as(isize, @intCast(old_ptr)) + offset));
                    frozen.entries.ptr = @ptrFromInt(new_ptr);

                    // Relocate each string pointer
                    for (frozen.entries) |*entry| {
                        if (entry.key.len > 0) {
                            const old_str_ptr = @intFromPtr(entry.key.ptr);
                            const new_str_ptr = @as(usize, @intCast(@as(isize, @intCast(old_str_ptr)) + offset));
                            entry.key.ptr = @ptrFromInt(new_str_ptr);
                        }
                    }
                }
            }
        }
    };
}

test "FrozenStringMap basic operations" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Create a string hash map to convert from
    var hash_map = std.StringHashMapUnmanaged(u32){};
    defer hash_map.deinit(allocator);

    try hash_map.put(allocator, "zebra", 100);
    try hash_map.put(allocator, "apple", 50);
    try hash_map.put(allocator, "banana", 150);
    try hash_map.put(allocator, "cherry", 30);

    // Convert to frozen map
    var frozen = try FrozenStringMap(u32).fromHashMap(allocator, hash_map);
    defer frozen.deinit(allocator);

    // Test lookups
    try testing.expectEqual(@as(?u32, 50), frozen.get("apple"));
    try testing.expectEqual(@as(?u32, 150), frozen.get("banana"));
    try testing.expectEqual(@as(?u32, 30), frozen.get("cherry"));
    try testing.expectEqual(@as(?u32, 100), frozen.get("zebra"));
    try testing.expectEqual(@as(?u32, null), frozen.get("missing"));

    // Test iteration (should be sorted)
    var iter = frozen.iterator();
    try testing.expectEqualStrings("apple", iter.next().?.key);
    try testing.expectEqualStrings("banana", iter.next().?.key);
    try testing.expectEqualStrings("cherry", iter.next().?.key);
    try testing.expectEqualStrings("zebra", iter.next().?.key);
    try testing.expectEqual(@as(?FrozenStringMap(u32).Entry, null), iter.next());
}

test "FrozenStringMap with void values" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var hash_map = std.StringHashMapUnmanaged(void){};
    defer hash_map.deinit(allocator);

    try hash_map.put(allocator, "hello", {});
    try hash_map.put(allocator, "world", {});
    try hash_map.put(allocator, "test", {});

    var frozen = try FrozenStringMap(void).fromHashMap(allocator, hash_map);
    defer frozen.deinit(allocator);

    try testing.expect(frozen.contains("hello"));
    try testing.expect(frozen.contains("world"));
    try testing.expect(frozen.contains("test"));
    try testing.expect(!frozen.contains("missing"));
}

test "FrozenU32Map basic operations" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var hash_map = std.hash_map.HashMap(u32, u32, std.hash_map.AutoContext(u32), 80).init(allocator);
    defer hash_map.deinit();

    try hash_map.put(10, 100);
    try hash_map.put(5, 50);
    try hash_map.put(15, 150);
    try hash_map.put(3, 30);

    var frozen = try FrozenU32Map(u32).fromHashMap(allocator, hash_map);
    defer frozen.deinit(allocator);

    // Test lookups
    try testing.expectEqual(@as(?u32, 100), frozen.get(10));
    try testing.expectEqual(@as(?u32, 50), frozen.get(5));
    try testing.expectEqual(@as(?u32, 150), frozen.get(15));
    try testing.expectEqual(@as(?u32, 30), frozen.get(3));
    try testing.expectEqual(@as(?u32, null), frozen.get(7));

    // Test iteration (should be sorted)
    var iter = frozen.iterator();
    try testing.expectEqual(@as(u32, 3), iter.next().?.key);
    try testing.expectEqual(@as(u32, 5), iter.next().?.key);
    try testing.expectEqual(@as(u32, 10), iter.next().?.key);
    try testing.expectEqual(@as(u32, 15), iter.next().?.key);
    try testing.expectEqual(@as(?FrozenU32Map(u32).Entry, null), iter.next());
}
