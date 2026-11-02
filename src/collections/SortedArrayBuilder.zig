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
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;

const CompactWriter = @import("CompactWriter.zig");
const validateRelocatedSpan = @import("safe_list.zig").validateRelocatedSpan;

/// A builder for creating sorted arrays directly without using hash maps
/// This is more efficient when we know we won't have duplicates
pub fn SortedArrayBuilder(comptime K: type, comptime V: type) type {
    return struct {
        entries: std.ArrayList(Entry) = .empty,
        sorted: bool = true,
        deduplicated: bool = true,

        const Self = @This();

        // For packed structs backed by integers, get the backing integer type
        const KeyBackingInt = if (@typeInfo(K) == .@"struct" and @typeInfo(K).@"struct".backing_integer != null)
            @typeInfo(K).@"struct".backing_integer.?
        else
            void;
        const KeyHasOrder = switch (@typeInfo(K)) {
            .@"struct", .@"enum", .@"union", .@"opaque" => @hasDecl(K, "order"),
            .type,
            .void,
            .bool,
            .noreturn,
            .int,
            .float,
            .pointer,
            .array,
            .comptime_float,
            .comptime_int,
            .undefined,
            .null,
            .optional,
            .error_union,
            .error_set,
            .@"fn",
            .frame,
            .@"anyframe",
            .vector,
            .enum_literal,
            .spirv,
            => false,
        };

        pub const Entry = struct {
            key: K,
            value: V,

            fn lessThan(_: void, a: Entry, b: Entry) bool {
                return keyOrder(a.key, b.key) == .lt;
            }
        };

        fn keyOrder(a: K, b: K) std.math.Order {
            if (K == []const u8) {
                return std.mem.order(u8, a, b);
            } else if (KeyHasOrder) {
                return K.order(a, b);
            } else if (@typeInfo(K) == .int or @typeInfo(K) == .@"enum") {
                return if (a == b)
                    .eq
                else if (a < b)
                    .lt
                else
                    .gt;
            } else if (KeyBackingInt != void) {
                const a_int = @as(KeyBackingInt, @bitCast(a));
                const b_int = @as(KeyBackingInt, @bitCast(b));
                return if (a_int == b_int)
                    .eq
                else if (a_int < b_int)
                    .lt
                else
                    .gt;
            } else {
                @compileError("Unsupported key type for SortedArrayBuilder");
            }
        }

        fn keyEql(a: K, b: K) bool {
            return keyOrder(a, b) == .eq;
        }

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

        /// Clone this builder into fresh owned memory.
        pub fn clone(self: *const Self, allocator: Allocator) Allocator.Error!Self {
            var cloned = Self.init();
            errdefer cloned.deinit(allocator);
            try cloned.entries.ensureTotalCapacity(allocator, self.entries.items.len);
            for (self.entries.items) |entry| {
                const cloned_key = if (K == []const u8)
                    try allocator.dupe(u8, entry.key)
                else
                    entry.key;
                cloned.entries.appendAssumeCapacity(.{
                    .key = cloned_key,
                    .value = entry.value,
                });
            }
            cloned.sorted = self.sorted;
            cloned.deduplicated = self.deduplicated;
            return cloned;
        }

        /// Add a key-value pair
        pub fn put(self: *Self, allocator: Allocator, key: K, value: V) Allocator.Error!void {
            const new_key = if (K == []const u8) try allocator.dupe(u8, key) else key;

            // Check if we need to maintain sorted order
            if (self.sorted and self.entries.items.len > 0) {
                const last = self.entries.items[self.entries.items.len - 1];
                const is_less_than = keyOrder(last.key, new_key) == .lt;

                if (!is_less_than) {
                    self.sorted = false;
                    self.deduplicated = false; // If unsorted, may have duplicates
                }
            }

            try self.entries.append(allocator, .{ .key = new_key, .value = value });
        }

        /// Get value by key (requires sorting first if not already sorted)
        pub fn get(self: *Self, allocator: Allocator, key: K) ?V {
            self.ensureSorted(allocator);
            return self.getFinalized(key);
        }

        /// Get value by key without mutating. The builder must already be sorted
        /// and deduplicated.
        pub fn getFinalized(self: *const Self, key: K) ?V {
            if (builtin.mode == .debug) {
                std.debug.assert(self.sorted);
                std.debug.assert(self.deduplicated);
            }
            var left: usize = 0;
            var right: usize = self.entries.items.len;

            while (left < right) {
                const mid = left + (right - left) / 2;
                const mid_key = self.entries.items[mid].key;

                const cmp = keyOrder(mid_key, key);

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
            }

            // Only deduplicate if not already deduplicated
            if (!self.deduplicated) {
                self.deduplicateAndReport(allocator);
            }
        }

        /// Ensure the array is sorted, and enforce that every key is unique.
        /// This is for semantic tables where duplicate keys must have already
        /// been rejected by the producing stage instead of being silently merged.
        pub fn ensureSortedUnique(self: *Self) void {
            if (!self.sorted) {
                std.sort.pdq(Entry, self.entries.items, {}, Entry.lessThan);
                self.sorted = true;
            }

            var i: usize = 1;
            while (i < self.entries.items.len) : (i += 1) {
                if (!keyEql(self.entries.items[i - 1].key, self.entries.items[i].key)) continue;
                if (builtin.mode == .debug) {
                    collectionsInvariant("SortedArrayBuilder invariant violated: duplicate key reached unique finalization", .{});
                }
                @trap();
            }

            self.deduplicated = true;
        }

        /// Check for duplicates, report them, and remove duplicates keeping the last occurrence.
        /// Since the array is sorted, duplicates are adjacent - this is O(n).
        fn deduplicateAndReport(self: *Self, allocator: Allocator) void {
            if (self.entries.items.len <= 1) {
                self.deduplicated = true;
                return;
            }

            // Since array is sorted, duplicates are adjacent.
            // We keep the last occurrence of each key by scanning backwards from each position.
            // Single pass: for each entry, check if the NEXT entry has the same key.
            // If so, skip current entry (keep the later one).
            var write_index: usize = 0;
            const len = self.entries.items.len;

            for (0..len) |read_index| {
                const entry = self.entries.items[read_index];

                // Check if next entry has the same key (bounds check first)
                const has_duplicate_after: bool = blk: {
                    const next_index = read_index + 1;
                    if (next_index >= self.entries.items.len) break :blk false;
                    const next_entry = self.entries.items[next_index];
                    break :blk keyEql(entry.key, next_entry.key);
                };

                if (has_duplicate_after) {
                    // Skip this entry, keep the later one
                    if (K == []const u8) {
                        allocator.free(entry.key);
                    }
                } else {
                    // Keep this entry
                    if (write_index != read_index) {
                        self.entries.items[write_index] = entry;
                    }
                    write_index += 1;
                }
            }

            // Update the length to reflect deduplicated entries
            self.entries.shrinkRetainingCapacity(write_index);
            self.deduplicated = true;
        }

        /// Get the number of entries
        pub fn count(self: *const Self) usize {
            return self.entries.items.len;
        }

        /// Serialized representation of SortedArrayBuilder
        /// Uses extern struct to guarantee consistent field layout across optimization levels.
        pub const Serialized = extern struct {
            entries_offset: i64,
            entries_len: u64,
            entries_capacity: u64,
            sorted: bool,
            deduplicated: bool,

            /// One relocatable base pointer (`entries_offset`) is fixed up per
            /// `deserializeInto`, regardless of `entries_len`. Marks this as a
            /// relocatable marker, so the cache validator calls `validateRelocations`
            /// and `relocatablePointerCount` counts it.
            pub const serialized_relocatable_pointers: usize = 1;

            /// The element type whose bytes are serialized, so a layout fingerprint can
            /// reflect a change to the entry's field order/size.
            pub const SerializedElement = Entry;

            /// Reject an `(entries_offset, entries_len)` that is misaligned for `Entry` or
            /// reaches outside the `backing_len`-byte buffer, before `deserializeInto`
            /// dereferences it.
            pub fn validateRelocations(self: *const Serialized, backing_len: u64) error{CorruptArtifact}!void {
                const span_bytes = std.math.mul(u64, self.entries_len, @sizeOf(Entry)) catch return error.CorruptArtifact;
                try validateRelocatedSpan(@alignOf(Entry), self.entries_offset, span_bytes, backing_len);
            }

            /// Serialize a SortedArrayBuilder into this Serialized struct, appending data to the writer
            pub fn serialize(
                self: *Serialized,
                builder: *const SortedArrayBuilder(K, V),
                allocator: Allocator,
                writer: *CompactWriter,
            ) Allocator.Error!void {
                const entries_slice = builder.entries.items;

                // Padding inside `Entry` is zeroed so the serialized bytes are
                // deterministic; the offset is an integer so zero is representable.
                const data_offset = try writer.appendSlicePodZeroedOffset(allocator, @as([]const Entry, entries_slice));

                self.entries_offset = @intCast(data_offset);
                self.entries_len = entries_slice.len;
                self.entries_capacity = entries_slice.len;
                self.sorted = builder.sorted;
                self.deduplicated = builder.deduplicated;
            }

            /// Deserialize into a SortedArrayBuilder value (no in-place modification of cache buffer).
            /// The base_addr parameter is the base address of the serialized buffer in memory.
            /// WARNING: The returned SortedArrayBuilder points into the cache buffer and CANNOT be grown.
            pub fn deserializeInto(self: *const Serialized, base_addr: usize) SortedArrayBuilder(K, V) {
                // Handle empty array case
                if (self.entries_len == 0) {
                    return SortedArrayBuilder(K, V){
                        .entries = .empty,
                        .sorted = self.sorted,
                        .deduplicated = self.deduplicated,
                    };
                }

                // Apply the base address to convert from serialized offset to actual pointer
                const entries_ptr: [*]Entry = @ptrFromInt(base_addr +% @as(usize, @intCast(self.entries_offset)));

                return SortedArrayBuilder(K, V){
                    .entries = .{
                        .items = entries_ptr[0..@intCast(self.entries_len)],
                        .capacity = @intCast(self.entries_capacity),
                        .pointer_stability = .{},
                    },
                    .sorted = self.sorted,
                    .deduplicated = self.deduplicated,
                };
            }

            /// Deserialize into a SortedArrayBuilder that owns its entries.
            /// The returned builder can be grown and must be released with `deinit`.
            pub fn deserializeWithCopy(
                self: *const Serialized,
                base_addr: usize,
                allocator: Allocator,
            ) Allocator.Error!SortedArrayBuilder(K, V) {
                const frozen = self.deserializeInto(base_addr);
                return frozen.clone(allocator);
            }
        };
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

/// A violated compiler invariant (design.md): builds with runtime safety
/// panic with this message, and optimized builds treat it as unreachable.
inline fn collectionsInvariant(comptime fmt: []const u8, args: anytype) noreturn {
    if (std.debug.runtime_safety) std.debug.panic(fmt, args);
    unreachable; // design.md: optimized builds carry no runtime check for a violated compiler invariant
}

test "SortedArrayBuilder.Serialized: the cache validator rejects a misaligned or out-of-range offset" {
    const testing = std.testing;
    const gpa = testing.allocator;
    const validateSerializedRelocations = @import("serde_validation.zig").validateSerializedRelocations;

    const Builder = SortedArrayBuilder(u32, u32);
    // A composing header, as `ExposedItems.Serialized` and `ModuleEnv.Serialized`
    // embed the builder: the validator reaches it only by walking the fields.
    const Header = extern struct {
        items: Builder.Serialized,
    };

    var builder = Builder.init();
    defer builder.deinit(gpa);
    try builder.put(gpa, 3, 30);
    try builder.put(gpa, 1, 10);
    try builder.put(gpa, 2, 20);
    builder.ensureSorted(gpa);

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);
    const header = try writer.appendAlloc(gpa, Header);
    try header.items.serialize(&builder, gpa, &writer);

    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, writer.total_bytes);
    defer gpa.free(buffer);
    _ = try writer.writeToBuffer(buffer);
    const written: *Header = @ptrCast(@alignCast(buffer.ptr));

    try validateSerializedRelocations(Header, written, buffer.len);

    // An offset one byte past the entries is no longer aligned to `Entry`.
    var misaligned = written.*;
    misaligned.items.entries_offset += 1;
    try testing.expectError(error.CorruptArtifact, validateSerializedRelocations(Header, &misaligned, buffer.len));

    // A truncated buffer leaves the entries reaching past its end.
    try testing.expectError(error.CorruptArtifact, validateSerializedRelocations(Header, written, buffer.len - 1));

    // A corrupt length whose byte extent overflows.
    var huge = written.*;
    huge.items.entries_len = std.math.maxInt(u64);
    try testing.expectError(error.CorruptArtifact, validateSerializedRelocations(Header, &huge, buffer.len));
}
