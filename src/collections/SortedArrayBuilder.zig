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

// Conditional import for appendToIovecs support
const base = if (@import("builtin").is_test) 
    struct { 
        const iovec_serialize = struct { 
            const IovecWriter = void; 
        }; 
    } 
else 
    @import("base");

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

        /// Ensure the array is sorted and deduplicate any duplicate entries
        pub fn ensureSorted(self: *Self, allocator: Allocator) void {
            if (!self.sorted) {
                std.sort.pdq(Entry, self.entries.items, {}, Entry.lessThan);
                self.sorted = true;

                // Check for and report duplicates, then deduplicate
                self.deduplicateAndReport(allocator);
            }
        }

        /// Check for duplicates, report them, and remove duplicates keeping the last occurrence
        fn deduplicateAndReport(self: *Self, allocator: Allocator) void {
            if (self.entries.items.len <= 1) return;

            // First pass: detect and report duplicates
            var i: usize = 1;
            while (i < self.entries.items.len) {
                const prev_entry = self.entries.items[i - 1];
                const curr_entry = self.entries.items[i];
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
            var write_index: usize = 0;
            for (self.entries.items, 0..) |entry, read_index| {
                var should_keep = true;

                // Look ahead to see if there's a duplicate later
                for (self.entries.items[read_index + 1 ..]) |future_entry| {
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
                        self.entries.items[write_index] = entry;
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
            self.entries.items.len = write_index;
        }

        /// Detect duplicates without modifying the array - returns list of duplicate keys
        pub fn detectDuplicates(self: *Self, allocator: Allocator) ![]K {
            var duplicates = std.ArrayList(K).init(allocator);

            if (self.entries.items.len <= 1) return duplicates.toOwnedSlice();

            // Ensure sorted first
            if (!self.sorted) {
                std.sort.pdq(Entry, self.entries.items, {}, Entry.lessThan);
                self.sorted = true;
            }

            var reported_keys = std.AutoHashMap(K, void).init(allocator);
            defer reported_keys.deinit();

            var i: usize = 1;
            while (i < self.entries.items.len) {
                const prev_entry = self.entries.items[i - 1];
                const curr_entry = self.entries.items[i];
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
            // Relocate the entries array pointer
            if (self.entries.items.len > 0) {
                const old_ptr = @intFromPtr(self.entries.items.ptr);
                const new_ptr = @as(usize, @intCast(@as(isize, @intCast(old_ptr)) + offset));
                self.entries.items.ptr = @ptrFromInt(new_ptr);
            }
        }

        /// Get the number of entries
        pub fn count(self: *const Self) usize {
            return self.entries.items.len;
        }

        /// Append this SortedArrayBuilder to an iovec writer for serialization
        pub fn appendToIovecs(self: *const Self, writer: anytype) !usize {
            const start_offset = writer.getOffset();
            
            // Must be sorted before serialization
            if (!self.sorted) {
                @panic("SortedArrayBuilder must be sorted before serialization");
            }
            
            // Write count
            const entry_count: u32 = @intCast(self.entries.items.len);
            try writer.appendStruct(entry_count);
            
            // Write entries directly as iovecs (zero-copy)
            for (self.entries.items) |entry| {
                if (K == []const u8) {
                    // String key: write length then data
                    const key_len: u32 = @intCast(entry.key.len);
                    try writer.appendStruct(key_len);
                    try writer.appendBytes(entry.key);
                } else {
                    // Numeric key - append directly as bytes
                    try writer.appendBytes(std.mem.asBytes(&entry.key));
                }
                
                // Write value
                if (@sizeOf(V) > 0) {
                    try writer.appendBytes(std.mem.asBytes(&entry.value));
                }
            }
            
            return start_offset;
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

test "SortedArrayBuilder detectDuplicates example usage" {
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

    // Example: Report duplicates (in real code, this would push diagnostics)
    for (duplicates) |duplicate_key| {
        // In a real compiler, you'd push a diagnostic here:
        // try cir.pushDiagnostic(CIR.Diagnostic{ .duplicate_exposed_item = ... });
        std.debug.print("Found duplicate key: {d}\n", .{duplicate_key});
    }

    // Verify we found the expected duplicates
    try testing.expectEqual(@as(usize, 2), duplicates.len);

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

