//! An interner for identifiers in a Roc file.
//!
//! This interner deduplicates identifiers and normalizes their case.
//! When an identifier starting with an uppercase ASCII letter is interned,
//! it is converted to lowercase for storage. This optimization takes advantage
//! of the fact that uppercase and lowercase identifiers are used in
//! non-overlapping contexts in Roc (e.g., types vs values).

const std = @import("std");
const mod = @import("mod.zig");
const collections = @import("collections");

const Region = mod.Region;

const Self = @This();

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

/// Initialize an `IdentInterner` with the specified capacity.
pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) std.mem.Allocator.Error!Self {
    // TODO: tune this. Rough assumption that average small string is 4 bytes.
    const bytes_per_string = 4;

    // Calculate hash table size based on load factor of 80% (multiply by 5, divide by 4)
    const hash_table_size = @as(u32, @intCast(((capacity * 5) / 4) + 1));
    // Round up to next power of 2 for better modulo performance
    const hash_table_capacity = std.math.ceilPowerOfTwo(u32, hash_table_size) catch hash_table_size;

    var self = Self{
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
pub fn deinit(self: *Self, gpa: std.mem.Allocator) void {
    self.bytes.deinit(gpa);
    self.hash_table.deinit(gpa);
}

/// Find a string in the hash table using linear probing.
/// Returns the Idx if found, or the slot index where it should be inserted if not found.
pub fn findStringOrSlot(self: *const Self, string: []const u8) struct { idx: ?Idx, slot: u32 } {
    const hash = std.hash.Fnv1a_32.hash(string);
    const table_size = self.hash_table.len();
    var slot = hash % table_size;

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
        slot = (slot + 1) % table_size;
    }
}

/// Find a string by linearly searching through all interned strings.
/// Returns the Idx if found, or null if not found.
fn findString(self: *const Self, string: []const u8) ?Idx {
    var offset: u32 = 1; // Skip the first null byte

    while (offset < self.bytes.len()) {
        // Find the next null terminator
        const start = offset;
        while (offset < self.bytes.len() and self.bytes.items.items[offset] != 0) {
            offset += 1;
        }

        if (offset <= self.bytes.len()) {
            const stored_string = self.bytes.items.items[start..offset];
            if (std.mem.eql(u8, string, stored_string)) {
                return @enumFromInt(start);
            }
            // Move past the null terminator
            offset += 1;
        }
    }

    return null;
}

/// Convert an ASCII uppercase letter to lowercase.
fn toLowerAscii(c: u8) u8 {
    if (c >= 'A' and c <= 'Z') {
        return c + ('a' - 'A');
    }
    return c;
}

/// Convert an ASCII lowercase letter to uppercase.
fn toUpperAscii(c: u8) u8 {
    if (c >= 'a' and c <= 'z') {
        return c - ('a' - 'A');
    }
    return c;
}

/// Add a nonempty string to this interner, returning a unique, serial index.
/// If the string starts with an uppercase ASCII letter, it will be converted to lowercase internally.
/// Empty strings may not be passed to this function, and it will panic in debug builds if this happens!
/// This saves memory because it means we don't store duplicates of e.g. "Serializer" and "serializer",
/// when identifiers are always known to be either lowercase or uppercase based on context (e.g. variables
/// and record fields and packages are always lowercase, modules and types and tags are always uppercase).
pub fn insert(self: *Self, gpa: std.mem.Allocator, string: []const u8) std.mem.Allocator.Error!Idx {
    std.debug.assert(string.len > 0); // Should never be asked to intern empty strings!
    std.debug.assert(!(string[0] >= '0' and string[0] <= '9')); // Identifiers cannot start with digits!
    std.debug.assert(!self.frozen); // Should not insert into a frozen interner

    // Normalize to lowercase
    if (string[0] >= 'A' and string[0] <= 'Z') {
        // Create a normalized version with lowercase first character
        var normalized = try gpa.alloc(u8, string.len);
        defer gpa.free(normalized);
        @memcpy(normalized, string);
        normalized[0] = toLowerAscii(normalized[0]);

        // Find the normalized string using linear search
        const result = self.findStringOrSlot(normalized);
        if (result.idx) |existing_idx| {
            // String already exists
            return existing_idx;
        } else {
            // String doesn't exist, add normalized version to bytes
            const new_offset: Idx = @enumFromInt(self.bytes.len());

            _ = try self.bytes.appendSlice(gpa, normalized);
            _ = try self.bytes.append(gpa, 0);

            // Add to hash table
            self.hash_table.items.items[result.slot] = new_offset;
            self.entry_count += 1;

            return new_offset;
        }
    } else {
        // No case conversion needed
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
            self.hash_table.items.items[result.slot] = new_offset;
            self.entry_count += 1;

            return new_offset;
        }
    }
}

/// Check if a string is already interned in this interner, used for generating unique names.
/// This checks for the normalized version (lowercase first char if uppercase).
pub fn contains(self: *const Self, string: []const u8) bool {
    std.debug.assert(string.len > 0); // Should never check empty strings!
    std.debug.assert(!(string[0] >= '0' and string[0] <= '9')); // Identifiers cannot start with digits!
    // Check if we need to normalize the case
    if (string.len > 0 and string[0] >= 'A' and string[0] <= 'Z') {
        // Need to check with normalized version
        var normalized: [256]u8 = undefined; // Stack buffer for small strings
        if (string.len > normalized.len) {
            // String too long for stack buffer, just check as-is
            const result = self.findStringOrSlot(string);
            return result.idx != null;
        }
        @memcpy(normalized[0..string.len], string);
        normalized[0] = toLowerAscii(normalized[0]);
        const result = self.findStringOrSlot(normalized[0..string.len]);
        return result.idx != null;
    } else {
        const result = self.findStringOrSlot(string);
        return result.idx != null;
    }
}

/// Get a reference to the lowercase text for an interned string.
pub fn getLowercase(self: *const Self, idx: Idx) []u8 {
    const bytes_slice = self.bytes.items.items;
    return std.mem.sliceTo(bytes_slice[@intFromEnum(idx)..], 0);
}

/// Get the text for an uppercase identifier (converts first char to uppercase if lowercase).
/// Caller must free the returned slice if it was allocated.
pub fn getUppercase(self: *const Self, allocator: std.mem.Allocator, idx: Idx) std.mem.Allocator.Error![]u8 {
    const text = self.getLowercase(idx);
    std.debug.assert(text.len > 0); // Should never have empty identifiers!
    if (text[0] >= 'a' and text[0] <= 'z') {
        // Need to convert to uppercase
        const result = try allocator.alloc(u8, text.len);
        @memcpy(result, text);
        result[0] = toUpperAscii(result[0]);
        return result;
    }
    // Already uppercase or doesn't start with letter - return as-is (not owned)
    return text;
}

/// Freeze the interner, preventing any new entries from being added.
pub fn freeze(self: *Self) void {
    if (std.debug.runtime_safety) {
        self.frozen = true;
    }
}

/// Serialize this interner to the given CompactWriter. The resulting interner
/// in the writer's buffer will have offsets instead of pointers. Calling any
/// methods on it or dereferencing its internal "pointers" (which are now
/// offsets) is illegal behavior!
pub fn serialize(
    self: *const Self,
    allocator: std.mem.Allocator,
    writer: *collections.CompactWriter,
) std.mem.Allocator.Error!*const Self {
    // First, write the struct
    const offset_self = try writer.appendAlloc(allocator, Self);

    // Then serialize the bytes and hash_table SafeLists and update the struct
    offset_self.* = .{
        .bytes = (try self.bytes.serialize(allocator, writer)).*,
        .hash_table = (try self.hash_table.serialize(allocator, writer)).*,
        .entry_count = self.entry_count,
        .frozen = self.frozen,
    };

    // Return the version of Self that's in the writer's buffer
    return @constCast(offset_self);
}

/// Add the given offset to the memory addresses of all pointers in `self`.
pub fn relocate(self: *Self, offset: isize) void {
    self.bytes.relocate(offset);
    self.hash_table.relocate(offset);
}

test "IdentInterner empty CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create an empty IdentInterner
    var original = Self{};
    defer original.deinit(gpa);

    // Create a temp file
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_empty_interner.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = collections.CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate - empty interner should still work
    const deserialized = @as(*Self, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(Self))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify empty - bytes starts with one zero byte, hash_table should be empty
    try testing.expectEqual(@as(usize, 1), deserialized.bytes.len());
    try testing.expectEqual(@as(u32, 0), deserialized.entry_count);
}

test "IdentInterner basic CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create an interner with some strings
    var original = try Self.initCapacity(gpa, 10);
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

    var indices = std.ArrayList(Idx).init(gpa);
    defer indices.deinit();

    for (test_strings) |str| {
        const idx = try original.insert(gpa, str);
        try indices.append(idx);
    }

    // Verify duplicate detection worked
    try testing.expectEqual(indices.items[8], indices.items[9]);

    // Create a temp file
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_basic_interner.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = collections.CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*Self, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(Self))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify all strings are accessible and correct
    for (test_strings[0..9], 0..) |expected_str, i| {
        const idx = indices.items[i];
        const actual_str = deserialized.getLowercase(idx);
        try testing.expectEqualStrings(expected_str, actual_str);
    }

    // Verify the entry count is preserved after deserialization
    try testing.expectEqual(@as(u32, 9), deserialized.entry_count);
}

test "IdentInterner with populated hashmap CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create interner and populate it
    var original = try Self.initCapacity(gpa, 20);
    defer original.deinit(gpa);

    // Insert many strings to ensure the hash map is well populated
    const test_data = [_]struct { str: []const u8, expected_idx: u32 }{
        .{ .str = "first", .expected_idx = 0 },
        .{ .str = "second", .expected_idx = 6 },
        .{ .str = "third", .expected_idx = 13 },
        .{ .str = "first", .expected_idx = 0 }, // duplicate
        .{ .str = "fourth", .expected_idx = 19 },
        .{ .str = "fifth", .expected_idx = 26 },
        .{ .str = "second", .expected_idx = 6 }, // duplicate
        .{ .str = "sixth", .expected_idx = 32 },
        .{ .str = "seventh", .expected_idx = 38 },
        .{ .str = "eighth", .expected_idx = 46 },
    };

    for (test_data) |data| {
        const idx = try original.insert(gpa, data.str);
        try testing.expectEqual(@as(u32, data.expected_idx), @intFromEnum(idx));
    }

    // Verify the hash table is populated
    try testing.expect(original.entry_count > 0);
    const original_entry_count = original.entry_count;

    // Create a temp file
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_hashmap_interner.dat", .{ .read = true });
    defer file.close();

    // Serialize
    var writer = collections.CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*Self, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(Self))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify the entry count is preserved after deserialization
    try testing.expectEqual(original_entry_count, deserialized.entry_count);

    // But all strings should still be accessible
    try testing.expectEqualStrings("first", deserialized.getLowercase(@enumFromInt(0)));
    try testing.expectEqualStrings("second", deserialized.getLowercase(@enumFromInt(6)));
    try testing.expectEqualStrings("third", deserialized.getLowercase(@enumFromInt(13)));
    try testing.expectEqualStrings("fourth", deserialized.getLowercase(@enumFromInt(19)));
    try testing.expectEqualStrings("fifth", deserialized.getLowercase(@enumFromInt(26)));
    try testing.expectEqualStrings("sixth", deserialized.getLowercase(@enumFromInt(32)));
    try testing.expectEqualStrings("seventh", deserialized.getLowercase(@enumFromInt(38)));
    try testing.expectEqualStrings("eighth", deserialized.getLowercase(@enumFromInt(46)));

    // Log to confirm the original had entries
    if (original_entry_count == 0) {
        std.debug.panic("Test failed: original should have had entries\n", .{});
    }
}

test "IdentInterner frozen state CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create and populate interner
    var original = try Self.initCapacity(gpa, 5);
    defer original.deinit(gpa);

    _ = try original.insert(gpa, "test1");
    _ = try original.insert(gpa, "test2");

    // Freeze the interner
    original.freeze();

    // Verify it's frozen
    if (std.debug.runtime_safety) {
        try testing.expect(original.frozen);
    }

    // Create a temp file
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_frozen_interner.dat", .{ .read = true });
    defer file.close();

    // Serialize
    var writer = collections.CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*Self, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(Self))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify frozen state is preserved
    if (std.debug.runtime_safety) {
        try testing.expect(deserialized.frozen);
    }

    // Verify strings are still accessible
    try testing.expectEqualStrings("test1", deserialized.getLowercase(@enumFromInt(0)));
    try testing.expectEqualStrings("test2", deserialized.getLowercase(@enumFromInt(6)));
}

test "IdentInterner edge cases CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Test with strings of various lengths and special characters
    var original = try Self.initCapacity(gpa, 15);
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

    var indices = std.ArrayList(Idx).init(gpa);
    defer indices.deinit();

    for (edge_cases) |str| {
        const idx = try original.insert(gpa, str);
        try indices.append(idx);
    }

    // Create a temp file
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_edge_interner.dat", .{ .read = true });
    defer file.close();

    // Serialize
    var writer = collections.CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*Self, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(Self))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify all edge cases
    for (edge_cases, 0..) |expected_str, i| {
        const idx = indices.items[i];
        const actual_str = deserialized.getLowercase(idx);
        try testing.expectEqualStrings(expected_str, actual_str);
    }
}

test "IdentInterner case conversion" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var interner = try Self.initCapacity(gpa, 10);
    defer interner.deinit(gpa);

    // Test uppercase to lowercase conversion
    const idx1 = try interner.insert(gpa, "Hello");
    const idx2 = try interner.insert(gpa, "hello");
    const idx3 = try interner.insert(gpa, "World");
    const idx4 = try interner.insert(gpa, "world");

    // These should be the same due to case normalization
    try testing.expectEqual(idx1, idx2);
    try testing.expectEqual(idx3, idx4);

    // The stored text should be lowercase
    try testing.expectEqualStrings("hello", interner.getLowercase(idx1));
    try testing.expectEqualStrings("world", interner.getLowercase(idx3));

    // Test getUppercase - should convert back to uppercase
    const type_text1 = try interner.getUppercase(gpa, idx1);
    defer if (type_text1.ptr != interner.getLowercase(idx1).ptr) gpa.free(type_text1);
    try testing.expectEqualStrings("Hello", type_text1);

    const tag_text1 = try interner.getUppercase(gpa, idx3);
    defer if (tag_text1.ptr != interner.getLowercase(idx3).ptr) gpa.free(tag_text1);
    try testing.expectEqualStrings("World", tag_text1);

    // Test with already lowercase
    const idx5 = try interner.insert(gpa, "lowercase");
    try testing.expectEqualStrings("lowercase", interner.getLowercase(idx5));

    // getUppercase should still work even if already lowercase
    const type_text2 = try interner.getUppercase(gpa, idx5);
    defer if (type_text2.ptr != interner.getLowercase(idx5).ptr) gpa.free(type_text2);
    try testing.expectEqualStrings("Lowercase", type_text2);

    // Test contains with case conversion
    try testing.expect(interner.contains("Hello"));
    try testing.expect(interner.contains("hello"));
    try testing.expect(interner.contains("World"));
    try testing.expect(interner.contains("world"));
    try testing.expect(!interner.contains("NotThere"));
}

test "IdentInterner multiple interners CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create multiple interners to test alignment and offset handling
    var interner1 = try Self.initCapacity(gpa, 5);
    defer interner1.deinit(gpa);

    var interner2 = try Self.initCapacity(gpa, 5);
    defer interner2.deinit(gpa);

    var interner3 = try Self.initCapacity(gpa, 5);
    defer interner3.deinit(gpa);

    // Populate with different strings
    const idx1_1 = try interner1.insert(gpa, "interner1_string1");
    const idx1_2 = try interner1.insert(gpa, "interner1_string2");

    const idx2_1 = try interner2.insert(gpa, "interner2_string1");
    const idx2_2 = try interner2.insert(gpa, "interner2_string2");
    const idx2_3 = try interner2.insert(gpa, "interner2_string3");

    const idx3_1 = try interner3.insert(gpa, "interner3_string1");

    // Freeze the second one
    interner2.freeze();

    // Create a temp file
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_multiple_interners.dat", .{ .read = true });
    defer file.close();

    // Serialize all three
    var writer = collections.CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try interner1.serialize(gpa, &writer);
    const offset1 = writer.total_bytes - @sizeOf(Self);

    _ = try interner2.serialize(gpa, &writer);
    const offset2 = writer.total_bytes - @sizeOf(Self);

    _ = try interner3.serialize(gpa, &writer);
    const offset3 = writer.total_bytes - @sizeOf(Self);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate all three
    const deserialized1 = @as(*Self, @ptrCast(@alignCast(buffer.ptr + offset1)));
    deserialized1.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    const deserialized2 = @as(*Self, @ptrCast(@alignCast(buffer.ptr + offset2)));
    deserialized2.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    const deserialized3 = @as(*Self, @ptrCast(@alignCast(buffer.ptr + offset3)));
    deserialized3.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify interner 1
    try testing.expectEqualStrings("interner1_string1", deserialized1.getLowercase(idx1_1));
    try testing.expectEqualStrings("interner1_string2", deserialized1.getLowercase(idx1_2));
    try testing.expectEqual(@as(u32, 2), deserialized1.entry_count);

    // Verify interner 2 (frozen)
    try testing.expectEqualStrings("interner2_string1", deserialized2.getLowercase(idx2_1));
    try testing.expectEqualStrings("interner2_string2", deserialized2.getLowercase(idx2_2));
    try testing.expectEqualStrings("interner2_string3", deserialized2.getLowercase(idx2_3));
    try testing.expectEqual(@as(u32, 3), deserialized2.entry_count);
    if (std.debug.runtime_safety) {
        try testing.expect(deserialized2.frozen);
    }

    // Verify interner 3
    try testing.expectEqualStrings("interner3_string1", deserialized3.getLowercase(idx3_1));
    try testing.expectEqual(@as(u32, 1), deserialized3.entry_count);
}
