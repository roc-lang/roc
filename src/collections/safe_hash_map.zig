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




        /// Get an iterator over the hash map
        pub fn iterator(self: *const Self) std.StringHashMapUnmanaged(V).Iterator {
            return self.map.iterator();
        }

        /// Append this SafeStringHashMap to an iovec writer for serialization
        pub fn appendToIovecs(self: *const Self, writer: anytype) !usize {
            // For now, we serialize as entries only and rebuild on deserialization
            // This is a limitation we need to address later
            const start_offset = writer.getOffset();
            
            // Write count
            const entry_count: u32 = @intCast(self.map.count());
            _ = try writer.appendStruct(entry_count);
            
            // Write all entries
            var iter = self.map.iterator();
            while (iter.next()) |entry| {
                // Write key length and key data
                const key_len: u32 = @intCast(entry.key_ptr.len);
                _ = try writer.appendStruct(key_len);
                _ = try writer.appendBytes(u8, entry.key_ptr.*);
                
                // Write value (if not void)
                if (V != void) {
                    _ = try writer.appendStruct(entry.value_ptr.*);
                }
            }
            
            return start_offset;
        }

        /// Relocate all pointers in this SafeStringHashMap by the given offset
        pub fn relocate(self: *Self, offset: isize) void {
            // Since we serialize hash maps as individual entries (not the internal structure),
            // and rebuild them during deserialization, we don't need to relocate the
            // internal hash map pointers. The hash map will be empty after deserialization
            // and entries will be re-inserted.
            _ = self;
            _ = offset;
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

test "SafeStringHashMap comprehensive serialization framework test" {
    const gpa = testing.allocator;

    var map = SafeStringHashMap(u32).init();
    defer map.deinit(gpa);

    // Add various test data including edge cases
    try map.put(gpa, "key1", 0); // minimum value
    try map.put(gpa, "key2", 42);
    try map.put(gpa, "longer_key_name_test", 123);
    try map.put(gpa, "k", 0xFFFFFFFF); // maximum value, short key
    try map.put(gpa, "", 999); // empty key
    try map.put(gpa, "🦎🚀", 777); // unicode key

    // Test serialization using the testing framework
    try serialization.testing.testSerialization(SafeStringHashMap(u32), &map, gpa);
}

test "SafeStringHashMap empty map serialization framework test" {
    const gpa = testing.allocator;

    var empty_map = SafeStringHashMap(u16).init();
    defer empty_map.deinit(gpa);

    try serialization.testing.testSerialization(SafeStringHashMap(u16), &empty_map, gpa);
}

test "SafeStringHashMap void value serialization framework test" {
    const gpa = testing.allocator;

    var map = SafeStringHashMap(void).init();
    defer map.deinit(gpa);

    // Add various keys (values are void)
    try map.put(gpa, "first", {});
    try map.put(gpa, "second", {});
    try map.put(gpa, "third", {});

    // Test serialization using the testing framework
    try serialization.testing.testSerialization(SafeStringHashMap(void), &map, gpa);
}
