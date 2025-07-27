//! An interner for short and likely repeated strings in in a Roc file.
//!
//! This interner deduplicates its string values because they are
//! expected to be small and often repeated since they tend to represent
//! the same value being referenced in many places. The indices assigned
//! to each interned string are serial, meaning they can be used for
//! arrays with values corresponding 1-to-1 to interned values, e.g. regions.

const std = @import("std");
const mod = @import("mod.zig");
const collections = @import("collections");

const Region = mod.Region;

const Self = @This();

/// The raw underlying bytes for all strings.
/// Since strings are small, they are simply null terminated.
/// This uses only 1 byte to encode the size and is cheap to scan.
bytes: collections.SafeList(u8) = .{},
/// A deduplicated set of strings mapping to their indices in bytes.
/// Used for deduplication during insertion. May be empty after deserialization.
strings: std.StringHashMapUnmanaged(Idx) = .{},
/// When true, no new entries can be added to the interner.
/// This is set after parsing is complete.
frozen: if (std.debug.runtime_safety) bool else void = if (std.debug.runtime_safety) false else {},

/// A unique index for a deduped string in this interner.
pub const Idx = enum(u32) {
    _,
};

/// Initialize a `SmallStringInterner` with the specified capacity.
pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) std.mem.Allocator.Error!Self {
    // TODO: tune this. Rough assumption that average small string is 4 bytes.
    const bytes_per_string = 4;

    var self = Self{
        .bytes = collections.SafeList(u8){},
        .strings = std.StringHashMapUnmanaged(Idx){},
    };

    // Properly initialize the bytes array to ensure clean state
    self.bytes = try collections.SafeList(u8).initCapacity(gpa, capacity * bytes_per_string);

    try self.strings.ensureTotalCapacity(gpa, @intCast(capacity));

    return self;
}

/// Free all memory consumed by this interner.
/// Will invalidate all slices referencing the interner.
pub fn deinit(self: *Self, gpa: std.mem.Allocator) void {
    self.bytes.deinit(gpa);

    // Free all the string keys we allocated
    var iterator = self.strings.iterator();
    while (iterator.next()) |entry| {
        gpa.free(entry.key_ptr.*);
    }
    self.strings.deinit(gpa);
}

/// Add a string to this interner, returning a unique, serial index.
pub fn insert(self: *Self, gpa: std.mem.Allocator, string: []const u8) std.mem.Allocator.Error!Idx {
    if (std.debug.runtime_safety) {
        std.debug.assert(!self.frozen); // Should not insert into a frozen interner
    }

    // Check if string already exists for deduplication
    const string_offset = if (self.strings.get(string)) |existing_offset| blk: {
        break :blk existing_offset;
    } else blk: {
        // String doesn't exist, add it to bytes
        const new_offset: Idx = @enumFromInt(self.bytes.len());

        _ = try self.bytes.appendSlice(gpa, string);
        _ = try self.bytes.append(gpa, 0);

        // Add to HashMap for future deduplication
        const owned_string = try gpa.dupe(u8, string);
        try self.strings.put(gpa, owned_string, new_offset);

        break :blk new_offset;
    };

    return string_offset;
}

/// Check if a string is already interned in this interner, used for generating unique names.
pub fn contains(self: *const Self, string: []const u8) bool {
    // Check if the string exists in the interner's map
    return self.strings.get(string) != null;
}

/// Get a reference to the text for an interned string.
pub fn getText(self: *const Self, idx: Idx) []u8 {
    const bytes_slice = self.bytes.items.items;
    return std.mem.sliceTo(bytes_slice[@intFromEnum(idx)..], 0);
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
    // First, serialize the bytes SafeList
    const bytes_ptr = try self.bytes.serialize(allocator, writer);

    // Next, write the struct with an empty hash map
    const offset_self = try writer.appendAlloc(allocator, Self);
    offset_self.* = .{
        .bytes = bytes_ptr.*,
        .strings = .{}, // Always serialize an empty hash map
        .frozen = self.frozen,
    };

    // Return the version of Self that's in the writer's buffer
    return @constCast(offset_self);
}

/// Add the given offset to the memory addresses of all pointers in `self`.
pub fn relocate(self: *Self, offset: isize) void {
    // Relocate the bytes SafeList
    self.bytes.relocate(offset);

    // The strings hash map is always empty after deserialization,
    // so there's nothing to relocate there
}

test "SmallStringInterner empty CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create an empty SmallStringInterner
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
    defer writer.iovecs.deinit(gpa);

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

    // Verify empty
    try testing.expectEqual(@as(usize, 0), deserialized.bytes.len());
    try testing.expectEqual(@as(usize, 0), deserialized.strings.count());
}

test "SmallStringInterner basic CompactWriter roundtrip" {
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
    defer writer.iovecs.deinit(gpa);

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
        const actual_str = deserialized.getText(idx);
        try testing.expectEqualStrings(expected_str, actual_str);
    }

    // Verify the strings hash map is empty after deserialization
    try testing.expectEqual(@as(usize, 0), deserialized.strings.count());
}

test "SmallStringInterner with populated hashmap CompactWriter roundtrip" {
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

    // Verify the hash map is populated
    try testing.expect(original.strings.count() > 0);
    const original_hashmap_count = original.strings.count();

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
    defer writer.iovecs.deinit(gpa);

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

    // Verify the hash map is empty after deserialization
    try testing.expectEqual(@as(usize, 0), deserialized.strings.count());

    // But all strings should still be accessible
    try testing.expectEqualStrings("first", deserialized.getText(@enumFromInt(0)));
    try testing.expectEqualStrings("second", deserialized.getText(@enumFromInt(6)));
    try testing.expectEqualStrings("third", deserialized.getText(@enumFromInt(13)));
    try testing.expectEqualStrings("fourth", deserialized.getText(@enumFromInt(19)));
    try testing.expectEqualStrings("fifth", deserialized.getText(@enumFromInt(26)));
    try testing.expectEqualStrings("sixth", deserialized.getText(@enumFromInt(32)));
    try testing.expectEqualStrings("seventh", deserialized.getText(@enumFromInt(38)));
    try testing.expectEqualStrings("eighth", deserialized.getText(@enumFromInt(46)));

    // Log to confirm the original had a populated hash map
    if (original_hashmap_count == 0) {
        std.debug.panic("Test failed: original hash map should have been populated\n", .{});
    }
}

test "SmallStringInterner frozen state CompactWriter roundtrip" {
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
    defer writer.iovecs.deinit(gpa);

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
    try testing.expectEqualStrings("test1", deserialized.getText(@enumFromInt(0)));
    try testing.expectEqualStrings("test2", deserialized.getText(@enumFromInt(6)));
}

test "SmallStringInterner edge cases CompactWriter roundtrip" {
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
    defer writer.iovecs.deinit(gpa);

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
        const actual_str = deserialized.getText(idx);
        try testing.expectEqualStrings(expected_str, actual_str);
    }
}

test "SmallStringInterner multiple interners CompactWriter roundtrip" {
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
    defer writer.iovecs.deinit(gpa);

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
    try testing.expectEqualStrings("interner1_string1", deserialized1.getText(idx1_1));
    try testing.expectEqualStrings("interner1_string2", deserialized1.getText(idx1_2));
    try testing.expectEqual(@as(usize, 0), deserialized1.strings.count());

    // Verify interner 2 (frozen)
    try testing.expectEqualStrings("interner2_string1", deserialized2.getText(idx2_1));
    try testing.expectEqualStrings("interner2_string2", deserialized2.getText(idx2_2));
    try testing.expectEqualStrings("interner2_string3", deserialized2.getText(idx2_3));
    try testing.expectEqual(@as(usize, 0), deserialized2.strings.count());
    if (std.debug.runtime_safety) {
        try testing.expect(deserialized2.frozen);
    }

    // Verify interner 3
    try testing.expectEqualStrings("interner3_string1", deserialized3.getText(idx3_1));
    try testing.expectEqual(@as(usize, 0), deserialized3.strings.count());
}
