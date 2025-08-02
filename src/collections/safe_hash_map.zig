//! Safe hash map collections with serialization support
//!
//! These collections provide type-safe serialization/deserialization for hash maps
//! commonly used in the Roc compiler.

const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;
const serialization = @import("serialization");

/// A type-safe string hash map with serialization support
pub fn SafeStringHashMap(comptime V: type) type {
    return struct {
        map: std.StringHashMapUnmanaged(V) = .{},

        const Self = @This();

        /// Initialize the hash map
        pub fn init() Self {
            return Self{};
        }

        /// Initialize with capacity
        pub fn initCapacity(gpa: Allocator, capacity: usize) std.mem.Allocator.Error!Self {
            var map = std.StringHashMapUnmanaged(V){};
            try map.ensureTotalCapacityContext(gpa, @intCast(capacity), std.hash_map.StringContext{});
            return Self{ .map = map };
        }

        /// Deinitialize the hash map
        pub fn deinit(self: *Self, gpa: Allocator) void {
            // Free all the keys (which are owned strings)
            var iter = self.map.iterator();
            while (iter.next()) |entry| {
                gpa.free(entry.key_ptr.*);
            }
            self.map.deinit(gpa);
        }

        /// Put a key-value pair (takes ownership of key)
        pub fn put(self: *Self, gpa: Allocator, key: []const u8, value: V) Allocator.Error!void {
            // Check if key already exists and get the old key if so
            const result = try self.map.getOrPutContext(gpa, key, std.hash_map.StringContext{});
            if (result.found_existing) {
                // Free the old key before replacing
                gpa.free(result.key_ptr.*);
                // Replace with new key
                result.key_ptr.* = try gpa.dupe(u8, key);
            } else {
                // New key, just duplicate it
                result.key_ptr.* = try gpa.dupe(u8, key);
            }
            result.value_ptr.* = value;
        }

        /// Get a value by key
        pub fn get(self: *const Self, key: []const u8) ?V {
            return self.map.getContext(key, std.hash_map.StringContext{});
        }

        /// Get the number of entries
        pub fn count(self: *const Self) usize {
            return self.map.count();
        }

        /// Check if a key exists in the map
        pub fn contains(self: *const Self, key: []const u8) bool {
            return self.map.containsContext(key, std.hash_map.StringContext{});
        }

        /// Calculate the size needed to serialize this hash map
        pub fn serializedSize(self: *const Self) usize {
            var size: usize = @sizeOf(u32); // count

            var iter = self.map.iterator();
            while (iter.next()) |entry| {
                size += @sizeOf(u32); // key length
                size += entry.key_ptr.len; // key bytes
                if (V != void) {
                    size += @sizeOf(V); // value bytes
                }
            }

            return size;
        }

        /// Deserialize a hash map from the provided buffer
        pub fn deserializeFrom(buffer: []const u8, allocator: Allocator) !Self {
            if (buffer.len < @sizeOf(u32)) return error.BufferTooSmall;

            var offset: usize = 0;

            // Read count
            const entry_count = std.mem.readInt(u32, buffer[offset..][0..4], .little);
            offset += @sizeOf(u32);

            // Create hash map with capacity
            var result = try Self.initCapacity(allocator, entry_count);
            errdefer result.deinit(allocator);

            // Read entries
            for (0..entry_count) |_| {
                // Read key length
                if (offset + @sizeOf(u32) > buffer.len) return error.BufferTooSmall;
                const key_len = std.mem.readInt(u32, buffer[offset..][0..4], .little);
                offset += @sizeOf(u32);

                // Read key bytes
                if (offset + key_len > buffer.len) return error.BufferTooSmall;
                const key = buffer[offset .. offset + key_len];
                offset += key_len;

                // Read value (if not void)
                const value = if (V != void) blk: {
                    if (offset + @sizeOf(V) > buffer.len) return error.BufferTooSmall;
                    const value_bytes = buffer[offset .. offset + @sizeOf(V)];
                    offset += @sizeOf(V);
                    break :blk std.mem.bytesAsValue(V, value_bytes).*;
                } else {};

                // Insert into map
                try result.put(allocator, key, value);
            }

            return result;
        }

        /// Get an iterator over the hash map
        pub fn iterator(self: *const Self) std.StringHashMapUnmanaged(V).Iterator {
            return self.map.iterator();
        }
    };
}

// Tests
test "SafeStringHashMap(void) basic operations" {
    const gpa = testing.allocator;

    var map = SafeStringHashMap(void).init();
    defer map.deinit(gpa);

    try testing.expectEqual(@as(usize, 0), map.count());

    try map.put(gpa, "hello", {});
    try map.put(gpa, "world", {});

    try testing.expectEqual(@as(usize, 2), map.count());
    try testing.expect(map.get("hello") != null);
    try testing.expect(map.get("world") != null);
    try testing.expect(map.get("missing") == null);
}

test "SafeStringHashMap(u16) basic operations" {
    const gpa = testing.allocator;

    var map = SafeStringHashMap(u16).init();
    defer map.deinit(gpa);

    try map.put(gpa, "foo", 42);
    try map.put(gpa, "bar", 123);

    try testing.expectEqual(@as(usize, 2), map.count());
    try testing.expectEqual(@as(u16, 42), map.get("foo").?);
    try testing.expectEqual(@as(u16, 123), map.get("bar").?);
    try testing.expect(map.get("missing") == null);
}

test "SafeStringHashMap(void) serialization round-trip" {
    const gpa = testing.allocator;

    var original = SafeStringHashMap(void).init();
    defer original.deinit(gpa);

    try original.put(gpa, "first", {});
    try original.put(gpa, "second", {});
    try original.put(gpa, "third", {});

    // Serialize
    const size = original.serializedSize();
    const buffer = try gpa.alloc(u8, size);
    defer gpa.free(buffer);

    const serialized = try original.serializeInto(buffer);
    try testing.expectEqual(size, serialized.len);

    // Deserialize
    var deserialized = try SafeStringHashMap(void).deserializeFrom(serialized, gpa);
    defer deserialized.deinit(gpa);

    // Verify
    try testing.expectEqual(original.count(), deserialized.count());
    try testing.expect(deserialized.get("first") != null);
    try testing.expect(deserialized.get("second") != null);
    try testing.expect(deserialized.get("third") != null);
    try testing.expect(deserialized.get("missing") == null);
}

test "SafeStringHashMap(u16) serialization round-trip" {
    const gpa = testing.allocator;

    var original = SafeStringHashMap(u16).init();
    defer original.deinit(gpa);

    try original.put(gpa, "alpha", 100);
    try original.put(gpa, "beta", 200);
    try original.put(gpa, "gamma", 300);

    // Serialize
    const size = original.serializedSize();
    const buffer = try gpa.alloc(u8, size);
    defer gpa.free(buffer);

    const serialized = try original.serializeInto(buffer);

    // Deserialize
    var deserialized = try SafeStringHashMap(u16).deserializeFrom(serialized, gpa);
    defer deserialized.deinit(gpa);

    // Verify
    try testing.expectEqual(original.count(), deserialized.count());
    try testing.expectEqual(@as(u16, 100), deserialized.get("alpha").?);
    try testing.expectEqual(@as(u16, 200), deserialized.get("beta").?);
    try testing.expectEqual(@as(u16, 300), deserialized.get("gamma").?);
}

test "SafeStringHashMap empty serialization" {
    const gpa = testing.allocator;

    var empty = SafeStringHashMap(u32).init();
    defer empty.deinit(gpa);

    // Serialize empty map
    const size = empty.serializedSize();
    try testing.expectEqual(@sizeOf(u32), size); // Just the count

    const buffer = try gpa.alloc(u8, size);
    defer gpa.free(buffer);

    const serialized = try empty.serializeInto(buffer);

    // Deserialize
    var deserialized = try SafeStringHashMap(u32).deserializeFrom(serialized, gpa);
    defer deserialized.deinit(gpa);

    try testing.expectEqual(@as(usize, 0), deserialized.count());
}

test "SafeStringHashMap deserialization buffer too small error" {
    const gpa = testing.allocator;

    // Buffer too small to even contain count
    var tiny_buffer: [2]u8 = undefined;
    try testing.expectError(error.BufferTooSmall, SafeStringHashMap(void).deserializeFrom(&tiny_buffer, gpa));

    // Buffer with count but insufficient data
    var partial_buffer: [8]u8 = undefined;
    std.mem.writeInt(u32, partial_buffer[0..4], 1, .little); // Claims 1 item
    std.mem.writeInt(u32, partial_buffer[4..8], 10, .little); // Claims key length 10, but no space for key
    try testing.expectError(error.BufferTooSmall, SafeStringHashMap(void).deserializeFrom(&partial_buffer, gpa));
}

test "SafeStringHashMap duplicate key handling" {
    const gpa = testing.allocator;

    var map = SafeStringHashMap(u32).init();
    defer map.deinit(gpa);

    // Insert first value
    try map.put(gpa, "test_key", 42);
    try testing.expectEqual(@as(usize, 1), map.count());
    try testing.expectEqual(@as(u32, 42), map.get("test_key").?);

    // Insert same key with different value - should replace without leaking
    try map.put(gpa, "test_key", 99);
    try testing.expectEqual(@as(usize, 1), map.count());
    try testing.expectEqual(@as(u32, 99), map.get("test_key").?);

    // Insert same key again
    try map.put(gpa, "test_key", 123);
    try testing.expectEqual(@as(usize, 1), map.count());
    try testing.expectEqual(@as(u32, 123), map.get("test_key").?);

    // Add a different key to make sure normal operation still works
    try map.put(gpa, "other_key", 456);
    try testing.expectEqual(@as(usize, 2), map.count());
    try testing.expectEqual(@as(u32, 123), map.get("test_key").?);
    try testing.expectEqual(@as(u32, 456), map.get("other_key").?);
}
