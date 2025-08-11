//! An interner for short and likely repeated strings in in a Roc file.
//!
//! This interner deduplicates its string values because they are
//! expected to be small and often repeated since they tend to represent
//! the same value being referenced in many places. The indices assigned
//! to each interned string are serial, meaning they can be used for
//! arrays with values corresponding 1-to-1 to interned values, e.g. regions.

const std = @import("std");
const collections = @import("collections");
const serialization = @import("serialization");

const Region = @import("Region.zig");
const CompactWriter = collections.CompactWriter;

const SmallStringInterner = @This();

/// The raw underlying bytes for all strings.
/// Since strings are small, they are simply null terminated.
/// This uses only 1 byte to encode the size and is cheap to scan.
bytes: collections.SafeList(u8) = .{},
/// A hash table using linear probing to map hashes to string indices.
/// Each slot contains an Idx pointing to the start of a string in bytes.
/// A value of .unused (0) indicates an empty slot.
hash_table: collections.SafeList(Idx) = .{},
/// The current number of entries in the hash table.
entry_count: u32 = 0,
/// When true, no new entries can be added to the interner.
/// This is set after parsing is complete.
frozen: if (std.debug.runtime_safety) bool else void = if (std.debug.runtime_safety) false else {},

/// A unique index for a deduped string in this interner.
pub const Idx = enum(u32) {
    unused = 0,
    _,
};

/// Initialize a `SmallStringInterner` with the specified capacity.
pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) std.mem.Allocator.Error!SmallStringInterner {
    // TODO: tune this. Rough assumption that average small string is 4 bytes.
    const bytes_per_string = 4;

    // Calculate hash table size based on load factor of 80% (multiply by 5, divide by 4)
    const hash_table_size = @as(u32, @intCast(((capacity * 5) / 4) + 1));
    // Round up to next power of 2 for better modulo performance
    const hash_table_capacity = std.math.ceilPowerOfTwo(u32, hash_table_size) catch hash_table_size;

    var self = SmallStringInterner{
        .bytes = collections.SafeList(u8){},
        .hash_table = collections.SafeList(Idx){},
        .entry_count = 0,
    };

    // Properly initialize the bytes array to ensure clean state
    self.bytes = try collections.SafeList(u8).initCapacity(gpa, capacity * bytes_per_string);

    // Start with at least one byte to ensure Idx.unused (0) never points to valid data
    _ = try self.bytes.append(gpa, 0);

    // Initialize hash table with all zeros (Idx.unused)
    self.hash_table = try collections.SafeList(Idx).initCapacity(gpa, hash_table_capacity);
    try self.hash_table.items.ensureTotalCapacityPrecise(gpa, hash_table_capacity);
    self.hash_table.items.items.len = hash_table_capacity;
    @memset(self.hash_table.items.items, .unused);

    return self;
}

/// Free all memory consumed by this interner.
/// Will invalidate all slices referencing the interner.
pub fn deinit(self: *SmallStringInterner, gpa: std.mem.Allocator) void {
    self.bytes.deinit(gpa);
    self.hash_table.deinit(gpa);
}

/// Find a string in the hash table using linear probing.
/// Returns the Idx if found, or the slot index where it should be inserted if not found.
pub fn findStringOrSlot(self: *const SmallStringInterner, string: []const u8) struct { idx: ?Idx, slot: u64 } {
    const hash = std.hash.Fnv1a_32.hash(string);
    const table_size = self.hash_table.len();
    var slot: usize = @intCast(hash % table_size);

    while (true) {
        const idx_at_slot = self.hash_table.items.items[slot];

        if (idx_at_slot == .unused) {
            // Empty slot - string not found
            return .{ .idx = null, .slot = slot };
        }

        // Check if this slot contains our string
        const stored_idx = @intFromEnum(idx_at_slot);
        const stored_end = stored_idx + string.len;

        // If the stored string would have had to go past the end of bytes,
        // they must not be equal. Also if there isn't a null terminator
        // right where we expect, they must not be equal.
        if (stored_end < self.bytes.len() and self.bytes.items.items[stored_end] == 0) {
            // With that out of the way, we can safely compare the string contents.
            if (std.mem.eql(u8, string, self.bytes.items.items[stored_idx..stored_end])) {
                // Found the string!
                return .{ .idx = idx_at_slot, .slot = slot };
            }
        }

        // Linear probe to next slot (with wraparound)
        slot = @intCast((slot + 1) % table_size);
    }
}

/// Resize the hash table when it gets too full.
fn resizeHashTable(self: *SmallStringInterner, gpa: std.mem.Allocator) std.mem.Allocator.Error!void {
    const old_table = self.hash_table;
    const new_size: usize = @intCast(old_table.len() * 2);

    // Create new hash table initialized to zeros
    self.hash_table = try collections.SafeList(Idx).initCapacity(gpa, new_size);
    try self.hash_table.items.ensureTotalCapacityPrecise(gpa, new_size);
    self.hash_table.items.items.len = new_size;
    @memset(self.hash_table.items.items, .unused);

    // Rehash all existing entries
    for (old_table.items.items) |idx| {
        if (idx != .unused) {
            // Get the string for this index
            const string = self.getText(idx);
            const result = self.findStringOrSlot(string);
            self.hash_table.items.items[@intCast(result.slot)] = idx;
        }
    }

    @constCast(&old_table).deinit(gpa);
}

/// Add a string to this interner, returning a unique, serial index.
pub fn insert(self: *SmallStringInterner, gpa: std.mem.Allocator, string: []const u8) std.mem.Allocator.Error!Idx {
    if (std.debug.runtime_safety) {
        std.debug.assert(!self.frozen); // Should not insert into a frozen interner
    }

    // Check if we need to resize the hash table (when 80% full = entry_count * 5 >= hash_table.len() * 4)
    if (self.entry_count * 5 >= self.hash_table.len() * 4) {
        try self.resizeHashTable(gpa);
    }

    // Find the string or the slot where it should be inserted
    const result = self.findStringOrSlot(string);

    if (result.idx) |existing_idx| {
        // String already exists
        return existing_idx;
    } else {
        // String doesn't exist, add it to bytes
        const new_offset: Idx = @enumFromInt(self.bytes.len());

        _ = try self.bytes.appendSlice(gpa, string);
        _ = try self.bytes.append(gpa, 0);

        // Add to hash table
        self.hash_table.items.items[@intCast(result.slot)] = new_offset;
        self.entry_count += 1;

        return new_offset;
    }
}

/// Check if a string is already interned in this interner, used for generating unique names.
pub fn contains(self: *const SmallStringInterner, string: []const u8) bool {
    const result = self.findStringOrSlot(string);
    return result.idx != null;
}

/// Get a reference to the text for an interned string.
pub fn getText(self: *const SmallStringInterner, idx: Idx) []u8 {
    const bytes_slice = self.bytes.items.items;
    const start = @intFromEnum(idx);

    return std.mem.sliceTo(bytes_slice[start..], 0);
}

/// Freeze the interner, preventing any new entries from being added.
pub fn freeze(self: *SmallStringInterner) void {
    if (std.debug.runtime_safety) {
        self.frozen = true;
    }
}

/// Serialize this interner to the given CompactWriter. The resulting interner
/// in the writer's buffer will have offsets instead of pointers. Calling any
/// methods on it or dereferencing its internal "pointers" (which are now
/// offsets) is illegal behavior!
pub fn serialize(
    self: *const SmallStringInterner,
    allocator: std.mem.Allocator,
    writer: *CompactWriter,
) std.mem.Allocator.Error!*const SmallStringInterner {
    // First, write the struct
    const offset_self = try writer.appendAlloc(allocator, SmallStringInterner);

    // Then serialize the bytes and hash_table SafeLists and update the struct
    const serialized_bytes = try self.bytes.serialize(allocator, writer);
    const serialized_hash_table = try self.hash_table.serialize(allocator, writer);

    offset_self.* = .{
        .bytes = serialized_bytes.*,
        .hash_table = serialized_hash_table.*,
        .entry_count = self.entry_count,
        .frozen = self.frozen,
    };

    // Return the version of Self that's in the writer's buffer
    return @constCast(offset_self);
}

/// Add the given offset to the memory addresses of all pointers in `self`.
pub fn relocate(self: *SmallStringInterner, offset: isize) void {
    self.bytes.relocate(offset);
    self.hash_table.relocate(offset);
}

/// Serialized representation of a SmallStringInterner
pub const Serialized = struct {
    bytes: collections.SafeList(u8).Serialized,
    hash_table: collections.SafeList(Idx).Serialized,
    entry_count: u32,
    frozen: if (std.debug.runtime_safety) bool else void,

    /// Serialize a SmallStringInterner into this Serialized struct, appending data to the writer
    pub fn serialize(
        self: *Serialized,
        interner: *const SmallStringInterner,
        allocator: std.mem.Allocator,
        writer: *CompactWriter,
    ) std.mem.Allocator.Error!void {
        // Serialize the bytes SafeList
        try self.bytes.serialize(&interner.bytes, allocator, writer);
        // Serialize the hash_table SafeList
        try self.hash_table.serialize(&interner.hash_table, allocator, writer);
        // Copy simple values directly
        self.entry_count = interner.entry_count;
        self.frozen = interner.frozen;
    }

    /// Deserialize this Serialized struct into a SmallStringInterner
    pub fn deserialize(self: *Serialized, offset: i64) *SmallStringInterner {
        // Self.Serialized should be at least as big as Self
        std.debug.assert(@sizeOf(Serialized) >= @sizeOf(SmallStringInterner));

        // Overwrite ourself with the deserialized version, and return our pointer after casting it to Self.
        const interner = @as(*SmallStringInterner, @ptrCast(self));

        interner.* = .{
            .bytes = self.bytes.deserialize(offset).*,
            .hash_table = self.hash_table.deserialize(offset).*,
            .entry_count = self.entry_count,
            .frozen = self.frozen,
        };

        return interner;
    }
};

test "SmallStringInterner empty CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create an empty SmallStringInterner with proper initialization
    var original = try SmallStringInterner.initCapacity(gpa, 0);
    defer original.deinit(gpa);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_empty_interner.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter with arena allocator
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    _ = try original.serialize(arena_allocator, &writer);

    // Write to file
    try writer.writeGather(arena_allocator, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, @as(usize, @intCast(file_size)));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate - empty interner should still work
    // The SmallStringInterner struct is at the beginning of the buffer
    const deserialized = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify empty - bytes starts with one zero byte, hash_table should be empty
    try std.testing.expectEqual(@as(usize, 1), deserialized.bytes.len());
    try std.testing.expectEqual(@as(u32, 0), deserialized.entry_count);
}

test "SmallStringInterner basic CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create an interner with some strings
    var original = try SmallStringInterner.initCapacity(gpa, 10);
    defer original.deinit(gpa);

    // Insert test strings
    const test_strings = [_][]const u8{
        "hello",
        "world",
        "foo",
        "bar",
        "baz",
        "test string",
        "another test",
        "", // empty string
        "duplicate",
        "duplicate", // Should reuse the same index
    };

    var indices = std.ArrayList(SmallStringInterner.Idx).init(gpa);
    defer indices.deinit();

    for (test_strings) |str| {
        const idx = try original.insert(gpa, str);
        try indices.append(idx);
    }

    // Verify duplicate detection worked
    try std.testing.expectEqual(indices.items[8], indices.items[9]);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_basic_interner.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter with arena allocator
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    _ = try original.serialize(arena_allocator, &writer);

    // Write to file
    try writer.writeGather(arena_allocator, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, @as(usize, @intCast(file_size)));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify all strings are accessible and correct
    for (test_strings[0..9], 0..) |expected_str, i| {
        const idx = indices.items[i];
        const actual_str = deserialized.getText(idx);
        try std.testing.expectEqualStrings(expected_str, actual_str);
    }

    // Verify the entry count is preserved after deserialization
    try std.testing.expectEqual(@as(u32, 9), deserialized.entry_count);
}

test "SmallStringInterner with populated hashmap CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create interner and populate it
    var original = try SmallStringInterner.initCapacity(gpa, 20);
    defer original.deinit(gpa);

    // Insert many strings to ensure the hash map is well populated
    const test_data = [_]struct { str: []const u8, expected_idx: u32 }{
        .{ .str = "first", .expected_idx = 1 }, // First string starts at index 1 (after initial 0 byte)
        .{ .str = "second", .expected_idx = 7 },
        .{ .str = "third", .expected_idx = 14 },
        .{ .str = "first", .expected_idx = 1 }, // duplicate
        .{ .str = "fourth", .expected_idx = 20 },
        .{ .str = "fifth", .expected_idx = 27 },
        .{ .str = "second", .expected_idx = 7 }, // duplicate
        .{ .str = "sixth", .expected_idx = 33 },
        .{ .str = "seventh", .expected_idx = 39 },
        .{ .str = "eighth", .expected_idx = 47 },
    };

    for (test_data) |data| {
        const idx = try original.insert(gpa, data.str);
        try std.testing.expectEqual(@as(u32, data.expected_idx), @intFromEnum(idx));
    }

    // Verify the hash table is populated
    try std.testing.expect(original.entry_count > 0);
    const original_entry_count = original.entry_count;

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_hashmap_interner.dat", .{ .read = true });
    defer file.close();

    // Serialize using arena allocator
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    _ = try original.serialize(arena_allocator, &writer);

    // Write to file
    try writer.writeGather(arena_allocator, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, @as(usize, @intCast(file_size)));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify the entry count is preserved after deserialization
    try std.testing.expectEqual(original_entry_count, deserialized.entry_count);

    // But all strings should still be accessible
    // Note: Index 0 is reserved, so all indices are offset by 1
    try std.testing.expectEqualStrings("first", deserialized.getText(@enumFromInt(1)));
    try std.testing.expectEqualStrings("second", deserialized.getText(@enumFromInt(7)));
    try std.testing.expectEqualStrings("third", deserialized.getText(@enumFromInt(14)));
    try std.testing.expectEqualStrings("fourth", deserialized.getText(@enumFromInt(20)));
    try std.testing.expectEqualStrings("fifth", deserialized.getText(@enumFromInt(27)));
    try std.testing.expectEqualStrings("sixth", deserialized.getText(@enumFromInt(33)));
    try std.testing.expectEqualStrings("seventh", deserialized.getText(@enumFromInt(39)));
    try std.testing.expectEqualStrings("eighth", deserialized.getText(@enumFromInt(47)));

    // Verify the original had entries
    try std.testing.expect(original_entry_count > 0);
}

test "SmallStringInterner frozen state CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create and populate interner
    var original = try SmallStringInterner.initCapacity(gpa, 5);
    defer original.deinit(gpa);

    _ = try original.insert(gpa, "test1");
    _ = try original.insert(gpa, "test2");

    // Freeze the interner
    original.freeze();

    // Verify it's frozen
    if (std.debug.runtime_safety) {
        try std.testing.expect(original.frozen);
    }

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_frozen_interner.dat", .{ .read = true });
    defer file.close();

    // Serialize using arena allocator
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    _ = try original.serialize(arena_allocator, &writer);

    // Write to file
    try writer.writeGather(arena_allocator, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, @as(usize, @intCast(file_size)));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify frozen state is preserved
    if (std.debug.runtime_safety) {
        try std.testing.expect(deserialized.frozen);
    }

    // Verify strings are still accessible
    // Note: Index 0 is reserved for the unused marker, so strings start at index 1
    try std.testing.expectEqualStrings("test1", deserialized.getText(@enumFromInt(1)));
    try std.testing.expectEqualStrings("test2", deserialized.getText(@enumFromInt(7)));
}

test "SmallStringInterner edge cases CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Test with strings of various lengths and special characters
    var original = try SmallStringInterner.initCapacity(gpa, 15);
    defer original.deinit(gpa);

    const edge_cases = [_][]const u8{
        "", // empty string
        "a", // single char
        "ab", // two chars
        "hello world with spaces",
        "special\ncharacters\ttabs",
        "unicode: 你好世界", // UTF-8
        "very_long_string_that_is_much_longer_than_average_to_test_capacity_handling",
        "\x00embedded", // string starting with null (though this might not work)
        "end_with_space ",
        " start_with_space",
    };

    var indices = std.ArrayList(SmallStringInterner.Idx).init(gpa);
    defer indices.deinit();

    for (edge_cases) |str| {
        const idx = try original.insert(gpa, str);
        try indices.append(idx);
    }

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_edge_interner.dat", .{ .read = true });
    defer file.close();

    // Serialize using arena allocator
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    _ = try original.serialize(arena_allocator, &writer);

    // Write to file
    try writer.writeGather(arena_allocator, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, @as(usize, @intCast(file_size)));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify all edge cases
    for (edge_cases, 0..) |expected_str, i| {
        const idx = indices.items[i];
        const actual_str = deserialized.getText(idx);

        // Special case: strings starting with null byte will be truncated to empty string
        // This is a limitation of null-terminated string storage
        if (expected_str.len > 0 and expected_str[0] == '\x00') {
            try std.testing.expectEqualStrings("", actual_str);
        } else {
            try std.testing.expectEqualStrings(expected_str, actual_str);
        }
    }
}

// TODO: CompactWriter doesn't support serializing multiple independent structures
// This test needs to be redesigned to either use separate writers or a containing structure
// test "SmallStringInterner multiple interners CompactWriter roundtrip" {
//     const gpa = std.testing.allocator;

//     // Create multiple interners to test alignment and offset handling
//     var interner1 = try SmallStringInterner.initCapacity(gpa, 5);
//     defer interner1.deinit(gpa);

//     var interner2 = try SmallStringInterner.initCapacity(gpa, 5);
//     defer interner2.deinit(gpa);

//     var interner3 = try SmallStringInterner.initCapacity(gpa, 5);
//     defer interner3.deinit(gpa);

//     // Populate with different strings
//     const idx1_1 = try interner1.insert(gpa, "interner1_string1");
//     const idx1_2 = try interner1.insert(gpa, "interner1_string2");

//     const idx2_1 = try interner2.insert(gpa, "interner2_string1");
//     const idx2_2 = try interner2.insert(gpa, "interner2_string2");
//     const idx2_3 = try interner2.insert(gpa, "interner2_string3");

//     const idx3_1 = try interner3.insert(gpa, "interner3_string1");

//     // Freeze the second one
//     interner2.freeze();

//     // Create a temp file
//     var tmp_dir = std.testing.tmpDir(.{});
//     defer tmp_dir.cleanup();

//     const file = try tmp_dir.dir.createFile("test_multiple_interners.dat", .{ .read = true });
//     defer file.close();

//     // Use arena allocator for serialization
//     var arena = std.heap.ArenaAllocator.init(gpa);
//     defer arena.deinit();
//     const arena_allocator = arena.allocator();

//     // Serialize all three
//     var writer = CompactWriter{
//         .iovecs = .{},
//         .total_bytes = 0,
//         .allocated_memory = .{},
//     };
//     defer writer.deinit(arena_allocator);

//     // Track where each interner starts in the serialized data
//     const offset1: usize = 0; // First interner starts at 0
//     _ = try interner1.serialize(arena_allocator, &writer);
//     const offset1_end = writer.total_bytes;

//     const offset2 = offset1_end; // Second interner starts where first ended
//     _ = try interner2.serialize(arena_allocator, &writer);
//     const offset2_end = writer.total_bytes;

//     const offset3 = offset2_end; // Third interner starts where second ended
//     _ = try interner3.serialize(arena_allocator, &writer);

//     // Write to file
//     try writer.writeGather(arena_allocator, file);

//     // Read back
//     try file.seekTo(0);
//     const file_size = try file.getEndPos();
//     const buffer = try gpa.alignedAlloc(u8, 16, @as(usize, @intCast(file_size)));
//     defer gpa.free(buffer);

//     _ = try file.read(buffer);

//     // Cast and relocate all three
//     const deserialized1 = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr + offset1)));
//     deserialized1.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

//     const deserialized2 = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr + offset2)));
//     deserialized2.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

//     const deserialized3 = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr + offset3)));
//     deserialized3.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

//     // Verify interner 1
//     try std.testing.expectEqualStrings("interner1_string1", deserialized1.getText(idx1_1));
//     try std.testing.expectEqualStrings("interner1_string2", deserialized1.getText(idx1_2));
//     try std.testing.expectEqual(@as(u32, 2), deserialized1.entry_count);

//     // Verify interner 2 (frozen)
//     try std.testing.expectEqualStrings("interner2_string1", deserialized2.getText(idx2_1));
//     try std.testing.expectEqualStrings("interner2_string2", deserialized2.getText(idx2_2));
//     try std.testing.expectEqualStrings("interner2_string3", deserialized2.getText(idx2_3));
//     try std.testing.expectEqual(@as(u32, 3), deserialized2.entry_count);
//     if (std.debug.runtime_safety) {
//         try std.testing.expect(deserialized2.frozen);
//     }

//     // Verify interner 3
//     try std.testing.expectEqualStrings("interner3_string1", deserialized3.getText(idx3_1));
//     try std.testing.expectEqual(@as(u32, 1), deserialized3.entry_count);
// } // End of commented test
