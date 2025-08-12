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

            var reported_keys = if (K == []const u8)
                std.StringHashMap(void).init(allocator)
            else
                std.AutoHashMap(K, void).init(allocator);
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
                // Skip relocation if this is a sentinel value
                // Define sentinel value locally since iovec_serialize is not available
                const EMPTY_ARRAY_SENTINEL: usize = 0xDEADBEEF;
                if (old_ptr != EMPTY_ARRAY_SENTINEL) {
                    // Handle negative offsets properly
                    if (offset >= 0) {
                        const new_ptr = old_ptr + @as(usize, @intCast(offset));
                        // Ensure proper alignment for Entry type
                        const aligned_ptr_opt = std.mem.alignPointer(@as([*]u8, @ptrFromInt(new_ptr)), @alignOf(Entry));
                        if (aligned_ptr_opt) |aligned_ptr| {
                            self.entries.items.ptr = @as([*]Entry, @ptrCast(@alignCast(aligned_ptr)));
                        } else {
                            // If we can't align properly, skip relocation
                            return;
                        }
                    } else {
                        // For negative offsets, we need to ensure we don't underflow
                        const abs_offset = @as(usize, @intCast(-offset));
                        if (old_ptr >= abs_offset) {
                            const new_ptr = old_ptr - abs_offset;
                            // Ensure proper alignment for Entry type
                            const aligned_ptr_opt = std.mem.alignPointer(@as([*]u8, @ptrFromInt(new_ptr)), @alignOf(Entry));
                            if (aligned_ptr_opt) |aligned_ptr| {
                                self.entries.items.ptr = @as([*]Entry, @ptrCast(@alignCast(aligned_ptr)));
                            } else {
                                // If we can't align properly, skip relocation
                                return;
                            }
                        }
                        // If old_ptr < abs_offset, we can't relocate safely, so skip
                    }
                }
            }
        }

        /// Get the number of entries
        pub fn count(self: *const Self) usize {
            return self.entries.items.len;
        }

        /// Check if there are no duplicates (assumes sorted)
        pub fn isDeduplicated(self: *const Self) bool {
            const entries = self.entries.items;
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

        // TODO FIXME
        // /// Append this SortedArrayBuilder to an iovec writer for serialization
        // pub fn appendToIovecs(self: *const Self, writer: *IovecWriter) !usize {

        //     // Must be sorted before serialization
        //     if (!self.sorted) {
        //         @panic("SortedArrayBuilder must be sorted before serialization");
        //     }

        //     // Create a mutable copy of self that we can modify
        //     var builder_copy = self.*;

        //     // Create a buffer for the final serialized struct
        //     const builder_copy_buffer = try writer.allocator.alloc(u8, @sizeOf(Self));

        //     // Track this allocation so it gets freed when writer is deinitialized
        //     try writer.owned_buffers.append(builder_copy_buffer);

        //     // Copy the builder to the buffer
        //     @memcpy(builder_copy_buffer, @as([*]const u8, @ptrCast(&builder_copy)));

        //     // Relocate the buffer to be relative to the writer's base
        //     const buffer_ptr = @as(*Self, @ptrCast(builder_copy_buffer.ptr));
        //     buffer_ptr.relocate(@as(isize, @intCast(@intFromPtr(writer.base_ptr)));

        //     // Append the buffer to the writer
        //     try writer.append(buffer_copy_buffer);

        //     return @sizeOf(Self);
        // }

        /// Serialized representation of SortedArrayBuilder
        pub const Serialized = struct {
            entries_offset: i64,
            entries_len: u64,
            entries_capacity: u64,
            sorted: bool,

            /// Serialize a SortedArrayBuilder into this Serialized struct, appending data to the writer
            pub fn serialize(
                self: *Serialized,
                builder: *const SortedArrayBuilder(K, V),
                allocator: Allocator,
                writer: *CompactWriter,
            ) Allocator.Error!void {
                const entries_slice = builder.entries.items;

                // Append the entries data to the writer
                const slice_ptr = try writer.appendSlice(allocator, entries_slice);

                // Store the offset, len, and capacity
                self.entries_offset = @intCast(@intFromPtr(slice_ptr.ptr));
                self.entries_len = entries_slice.len;
                self.entries_capacity = entries_slice.len;
                self.sorted = builder.sorted;
            }

            /// Deserialize this Serialized struct into a SortedArrayBuilder
            pub fn deserialize(self: *Serialized, offset: i64) *SortedArrayBuilder(K, V) {
                // SortedArrayBuilder.Serialized should be at least as big as SortedArrayBuilder
                std.debug.assert(@sizeOf(Serialized) >= @sizeOf(SortedArrayBuilder(K, V)));

                // Overwrite ourself with the deserialized version, and return our pointer after casting it to Self.
                const builder = @as(*SortedArrayBuilder(K, V), @ptrFromInt(@intFromPtr(self)));

                // Handle empty array case
                if (self.entries_len == 0) {
                    builder.* = SortedArrayBuilder(K, V){
                        .entries = .{},
                        .sorted = self.sorted,
                    };
                } else {
                    // Apply the offset to convert from serialized offset to actual pointer
                    const entries_ptr_usize: usize = @intCast(self.entries_offset + offset);
                    const entries_ptr: [*]Entry = @ptrFromInt(entries_ptr_usize);

                    builder.* = SortedArrayBuilder(K, V){
                        .entries = .{
                            .items = entries_ptr[0..@intCast(self.entries_len)],
                            .capacity = @intCast(self.entries_capacity),
                        },
                        .sorted = self.sorted,
                    };
                }

                return builder;
            }
        };
    };
}

// TODO FIXME
// test "SortedArrayBuilder basic operations" {
//     const testing = std.testing;
//     const allocator = testing.allocator;

//     var builder = SortedArrayBuilder([]const u8, u32).init();
//     defer builder.deinit(allocator);

//     // Add items in random order
//     try builder.put(allocator, "zebra", 100);
//     try builder.put(allocator, "apple", 50);
//     try builder.put(allocator, "banana", 150);
//     try builder.put(allocator, "cherry", 30);

//     // Test count
//     try testing.expectEqual(@as(usize, 4), builder.count());

//     // Test get (forces sorting)
//     try testing.expectEqual(@as(?u32, 50), builder.get(allocator, "apple"));
//     try testing.expectEqual(@as(?u32, 150), builder.get(allocator, "banana"));
//     try testing.expectEqual(@as(?u32, 30), builder.get(allocator, "cherry"));
//     try testing.expectEqual(@as(?u32, 100), builder.get(allocator, "zebra"));
//     try testing.expectEqual(@as(?u32, null), builder.get(allocator, "missing"));

//     // Test iovec serialization
//     var writer = IovecWriter.init(allocator);
//     defer writer.deinit();

//     _ = try builder.appendToIovecs(&writer);

//     // Verify we have some iovecs (basic smoke test)
//     try testing.expect(writer.iovecs.items.len > 0);
// }

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

// TODO FIXME
// test "SortedArrayBuilder detectDuplicates example usage" {
//     const testing = std.testing;
//     const allocator = testing.allocator;

//     var builder = SortedArrayBuilder(u32, u16).init();
//     defer builder.deinit(allocator);

//     // Add some entries with duplicates
//     try builder.put(allocator, 100, 1);
//     try builder.put(allocator, 200, 2);
//     try builder.put(allocator, 100, 3); // duplicate of 100
//     try builder.put(allocator, 300, 4);
//     try builder.put(allocator, 200, 5); // duplicate of 200

//     // Detect duplicates before sorting/deduplicating
//     const duplicates = try builder.detectDuplicates(allocator);
//     defer allocator.free(duplicates);

//     // Example: Report duplicates (in real code, this would push diagnostics)
//     for (duplicates) |duplicate_key| {
//         // In a real compiler, you'd push a diagnostic here:
//         // try cir.pushDiagnostic(CIR.Diagnostic{ .duplicate_exposed_item = ... });
//         std.debug.print("Found duplicate key: {d}\n", .{duplicate_key});
//     }

//     // Verify we found the expected duplicates
//     try testing.expectEqual(@as(usize, 2), duplicates.len);

//     // After detection, normal operations work as expected
//     builder.ensureSorted(allocator);
//     try testing.expectEqual(@as(usize, 3), builder.count()); // Deduplicated
//     try testing.expectEqual(@as(?u16, 3), builder.get(allocator, 100)); // Last value kept
//     try testing.expectEqual(@as(?u16, 5), builder.get(allocator, 200)); // Last value kept
// }

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
