//! Strings written inline in Roc code, e.g. `x = "abc"`.

const std = @import("std");
const collections = @import("collections");
const testing = std.testing;

const CompactWriter = collections.CompactWriter;

/// The index of this string in a `Store`.
pub const Idx = enum(u32) { _ };

/// An interner for string literals.
///
/// String literals are deduplicated so that identical strings receive the same Idx.
/// This enables direct index comparison for equality checking (e.g., in exhaustiveness).
/// The deduplication uses a linear search through existing strings, which is acceptable
/// because the number of unique string literals in pattern matching is typically small.
pub const Store = struct {
    /// An Idx points to the
    /// first byte of the string. The previous
    /// 4 bytes encode it's length.
    ///          Idx of "well"
    ///           |
    ///           |
    ///           |
    /// |    3   |w|e|l|l|   5    |h|e|l|l|o|
    /// |---u32--|--u8---|--u32---|--u8-----|
    /// conceptually these are the sizes above.
    ///
    /// Note:
    /// Later we could change from fixed u32-s to variable lengthed
    /// sizes, encoded in reverse where for example,
    /// the first 7 bit would signal the length, the last bit would signal that the length
    /// continues to the previous byte
    buffer: collections.SafeList(u8) = .{},

    /// Intiizalizes a `Store` with capacity `bytes` of space.
    /// Note this specifically is the number of bytes for storing strings.
    /// The string `hello, world!` will use 14 bytes including the null terminator.
    pub fn initCapacityBytes(gpa: std.mem.Allocator, bytes: usize) std.mem.Allocator.Error!Store {
        return .{
            .buffer = try collections.SafeList(u8).initCapacity(gpa, bytes),
        };
    }

    /// Deinitialize a `Store`'s memory.
    pub fn deinit(self: *Store, gpa: std.mem.Allocator) void {
        self.buffer.deinit(gpa);
    }

    /// Insert a new string into a `Store`.
    ///
    /// Deduplicates: if an identical string already exists, returns its index.
    /// This enables direct index comparison for equality checking.
    pub fn insert(self: *Store, gpa: std.mem.Allocator, string: []const u8) std.mem.Allocator.Error!Idx {
        // Search for an existing identical string
        if (self.findExisting(string)) |existing_idx| {
            return existing_idx;
        }

        // String not found, insert it
        const str_len: u32 = @truncate(string.len);

        const str_len_bytes = std.mem.asBytes(&str_len);
        _ = try self.buffer.appendSlice(gpa, str_len_bytes);

        const string_content_start = self.buffer.len();

        _ = try self.buffer.appendSlice(gpa, string);

        return @enumFromInt(@as(u32, @intCast(string_content_start)));
    }

    /// Search for an existing string in the store and return its index if found.
    fn findExisting(self: *const Store, string: []const u8) ?Idx {
        const buffer_items = self.buffer.items.items;
        var pos: usize = 0;

        while (pos + 4 <= buffer_items.len) {
            // Read the length (4 bytes)
            const str_len = std.mem.bytesAsValue(u32, buffer_items[pos .. pos + 4]).*;
            const content_start = pos + 4;
            const content_end = content_start + str_len;

            if (content_end > buffer_items.len) break;

            // Compare with the target string
            const existing = buffer_items[content_start..content_end];
            if (std.mem.eql(u8, existing, string)) {
                return @enumFromInt(@as(u32, @intCast(content_start)));
            }

            // Move to next string
            pos = content_end;
        }

        return null;
    }

    /// Get a string literal's text from this `Store`.
    pub fn get(self: *const Store, idx: Idx) []u8 {
        const idx_u32: u32 = @intCast(@intFromEnum(idx));
        const str_len = std.mem.bytesAsValue(u32, self.buffer.items.items[idx_u32 - 4 .. idx_u32]).*;
        return self.buffer.items.items[idx_u32 .. idx_u32 + str_len];
    }

    /// Serialize this Store to the given CompactWriter. The resulting Store
    /// in the writer's buffer will have offsets instead of pointers. Calling any
    /// methods on it or dereferencing its internal "pointers" (which are now
    /// offsets) is illegal behavior!
    pub fn serialize(
        self: *const Store,
        allocator: std.mem.Allocator,
        writer: *CompactWriter,
    ) std.mem.Allocator.Error!*const Store {
        // First, write the Store struct itself
        const offset_self = try writer.appendAlloc(allocator, Store);

        // Then serialize the buffer SafeList and update the struct
        offset_self.* = .{
            .buffer = (try self.buffer.serialize(allocator, writer)).*,
        };

        return @constCast(offset_self);
    }

    /// Add the given offset to the memory addresses of all pointers in `self`.
    pub fn relocate(self: *Store, offset: isize) void {
        self.buffer.relocate(offset);
    }

    /// Serialized representation of a Store
    /// Uses extern struct to guarantee consistent field layout across optimization levels.
    pub const Serialized = extern struct {
        buffer: collections.SafeList(u8).Serialized,

        /// Serialize a Store into this Serialized struct, appending data to the writer
        pub fn serialize(
            self: *Serialized,
            store: *const Store,
            allocator: std.mem.Allocator,
            writer: *CompactWriter,
        ) std.mem.Allocator.Error!void {
            // Serialize the buffer SafeList
            try self.buffer.serialize(&store.buffer, allocator, writer);
        }

        /// Deserialize into a Store value (no in-place modification of cache buffer).
        /// The base parameter is the base address of the serialized buffer in memory.
        pub fn deserializeInto(self: *const Serialized, base: usize) Store {
            return Store{
                .buffer = self.buffer.deserializeInto(base),
            };
        }
    };
};

test "insert" {
    const gpa = std.testing.allocator;

    var interner = Store{};
    defer interner.deinit(gpa);

    const str_1 = "abc".*;
    const str_2 = "defg".*;
    const idx_1 = try interner.insert(gpa, &str_1);
    const idx_2 = try interner.insert(gpa, &str_2);

    try std.testing.expectEqualStrings("abc", interner.get(idx_1));
    try std.testing.expectEqualStrings("defg", interner.get(idx_2));
}

test "Store empty CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create an empty Store
    var original = Store{};
    defer original.deinit(gpa);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_empty_stringlit.dat", .{ .read = true });
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
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @as(usize, @intCast(file_size)));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify empty
    try std.testing.expectEqual(@as(usize, 0), deserialized.buffer.len());
}

test "Store basic CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create original store and add some strings
    var original = Store{};
    defer original.deinit(gpa);

    const idx1 = try original.insert(gpa, "hello");
    const idx2 = try original.insert(gpa, "world");
    const idx3 = try original.insert(gpa, "foo bar baz");

    // Verify original values
    try std.testing.expectEqualStrings("hello", original.get(idx1));
    try std.testing.expectEqualStrings("world", original.get(idx2));
    try std.testing.expectEqualStrings("foo bar baz", original.get(idx3));

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_basic_stringlit.dat", .{ .read = true });
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
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @as(usize, @intCast(file_size)));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify the strings are accessible
    try std.testing.expectEqualStrings("hello", deserialized.get(idx1));
    try std.testing.expectEqualStrings("world", deserialized.get(idx2));
    try std.testing.expectEqualStrings("foo bar baz", deserialized.get(idx3));
}

test "Store comprehensive CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    var original = Store{};
    defer original.deinit(gpa);

    // Test various string types
    const test_strings = [_][]const u8{
        "", // empty string
        "a", // single character
        "hello world", // simple string
        "🦎🚀✨", // emojis
        "日本語テキスト", // non-Latin script
        "\x00\x01\x02\x03", // binary data
        "line1\nline2\r\nline3", // line breaks
        "tab\tseparated\tvalues", // tabs
        "quotes: 'single' and \"double\"", // quotes
        "very long string " ** 50, // long string
    };

    var indices = std.ArrayList(Idx).empty;
    defer indices.deinit(gpa);

    for (test_strings) |str| {
        const idx = try original.insert(gpa, str);
        try indices.append(gpa, idx);
    }

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_comprehensive_stringlit.dat", .{ .read = true });
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
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @as(usize, @intCast(file_size)));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify all strings
    for (test_strings, 0..) |expected_str, i| {
        const idx = indices.items[i];
        const actual_str = deserialized.get(idx);
        try std.testing.expectEqualStrings(expected_str, actual_str);
    }
}

test "Store CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create and populate store
    var original = Store{};
    defer original.deinit(gpa);

    _ = try original.insert(gpa, "test1");
    _ = try original.insert(gpa, "test2");

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_frozen_stringlit.dat", .{ .read = true });
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
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @as(usize, @intCast(file_size)));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));
}

test "Store.Serialized roundtrip" {
    const gpa = std.testing.allocator;

    // Create original store and add some strings
    var original = Store{};
    defer original.deinit(gpa);

    const idx1 = try original.insert(gpa, "hello");
    const idx2 = try original.insert(gpa, "world");
    const idx3 = try original.insert(gpa, "foo bar baz");

    // Create a CompactWriter and arena
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_file = try tmp_dir.dir.createFile("test.compact", .{ .read = true });
    defer tmp_file.close();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_alloc);

    // Allocate and serialize using the Serialized struct
    const serialized_ptr = try writer.appendAlloc(arena_alloc, Store.Serialized);
    try serialized_ptr.serialize(&original, arena_alloc, &writer);

    // Write to file
    try writer.writeGather(arena_alloc, tmp_file);

    // Read back
    const file_size = try tmp_file.getEndPos();
    const buffer = try gpa.alloc(u8, @as(usize, @intCast(file_size)));
    defer gpa.free(buffer);
    _ = try tmp_file.pread(buffer, 0);

    // Deserialize
    const deserialized_ptr = @as(*Store.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const store = deserialized_ptr.deserializeInto(@intFromPtr(buffer.ptr));

    // Verify the strings are accessible
    try std.testing.expectEqualStrings("hello", store.get(idx1));
    try std.testing.expectEqualStrings("world", store.get(idx2));
    try std.testing.expectEqualStrings("foo bar baz", store.get(idx3));
}

test "Store edge case indices CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    var original = Store{};
    defer original.deinit(gpa);

    // The index returned points to the first byte of the string content,
    // with the length stored in the previous 4 bytes.
    // Test various scenarios that might stress the index calculation.

    // First string starts at index 4 (after its length)
    const idx1 = try original.insert(gpa, "first");
    try std.testing.expectEqual(@as(u32, 4), @intFromEnum(idx1));

    // Second string starts at 4 + 5 + 4 = 13
    const idx2 = try original.insert(gpa, "second");
    try std.testing.expectEqual(@as(u32, 13), @intFromEnum(idx2));

    // Empty string
    const idx3 = try original.insert(gpa, "");
    try std.testing.expectEqual(@as(u32, 23), @intFromEnum(idx3));

    // Very long string
    const long_str = "x" ** 1000;
    const idx4 = try original.insert(gpa, long_str);
    try std.testing.expectEqual(@as(u32, 27), @intFromEnum(idx4));

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_edge_indices_stringlit.dat", .{ .read = true });
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
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @as(usize, @intCast(file_size)));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify all strings with their exact indices
    try std.testing.expectEqualStrings("first", deserialized.get(idx1));
    try std.testing.expectEqualStrings("second", deserialized.get(idx2));
    try std.testing.expectEqualStrings("", deserialized.get(idx3));
    try std.testing.expectEqualStrings(long_str, deserialized.get(idx4));
}
