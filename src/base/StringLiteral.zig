//! Strings written inline in Roc code, e.g. `x = "abc"`.

const std = @import("std");
const collections = @import("collections");
const serialization = @import("serialization");
const testing = std.testing;

const CompactWriter = serialization.CompactWriter;

/// The index of this string in a `Store`.
pub const Idx = enum(u32) { _ };

/// An interner for string literals.
///
/// We avoid using the SmallStringInterner for string literals since
/// they are expected to be almost all unique and also larger, meaning
/// not worth the equality checking cost for depuplicating.
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
    /// When true, no new entries can be added to the store.
    /// This is set after canonicalization is complete, so that
    /// we know it's safe to serialize/deserialize the part of the interner
    /// that goes from ident to string, because we don't go from string to ident anymore.
    frozen: if (std.debug.runtime_safety) bool else void = if (std.debug.runtime_safety) false else {},

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
    /// Does not deduplicate, as string literals are expected to be large and mostly unique.
    pub fn insert(self: *Store, gpa: std.mem.Allocator, string: []const u8) std.mem.Allocator.Error!Idx {
        if (std.debug.runtime_safety) {
            std.debug.assert(!self.frozen); // Should not insert into a frozen store
        }
        const str_len: u32 = @truncate(string.len);

        const str_len_bytes = std.mem.asBytes(&str_len);
        _ = try self.buffer.appendSlice(gpa, str_len_bytes);

        const string_content_start = self.buffer.len();

        _ = try self.buffer.appendSlice(gpa, string);

        return @enumFromInt(@as(u32, @intCast(string_content_start)));
    }

    /// Get a string literal's text from this `Store`.
    pub fn get(self: *const Store, idx: Idx) []u8 {
        const idx_u32: u32 = @intCast(@intFromEnum(idx));
        const str_len = std.mem.bytesAsValue(u32, self.buffer.items.items[idx_u32 - 4 .. idx_u32]).*;
        return self.buffer.items.items[idx_u32 .. idx_u32 + str_len];
    }

    /// Freeze the store, preventing any new entries from being added.
    pub fn freeze(self: *Store) void {
        if (std.debug.runtime_safety) {
            self.frozen = true;
        }
    }

    /// Calculate the size needed to serialize this Store
    pub fn serializedSize(self: *const Store) usize {
        // Header: 4 bytes for buffer length
        // Data: buffer.items.len bytes
        const raw_size = @sizeOf(u32) + self.buffer.len();
        // Align to SERIALIZATION_ALIGNMENT to maintain alignment for subsequent data
        return std.mem.alignForward(usize, raw_size, serialization.SERIALIZATION_ALIGNMENT);
    }

    /// Deserialize a Store from the provided buffer
    pub fn deserializeFrom(buffer: []const u8, gpa: std.mem.Allocator) !Store {
        if (buffer.len < @sizeOf(u32)) return error.BufferTooSmall;

        // Read buffer length
        const buffer_len = @as(*const u32, @ptrCast(@alignCast(buffer.ptr))).*;

        const expected_size = @sizeOf(u32) + buffer_len;
        if (buffer.len < expected_size) return error.BufferTooSmall;

        // Create store with exact capacity
        var store = try Store.initCapacityBytes(gpa, buffer_len);

        // Copy buffer data
        if (buffer_len > 0) {
            const data_start = @sizeOf(u32);
            _ = try store.buffer.appendSlice(gpa, buffer[data_start .. data_start + buffer_len]);
        }

        return store;
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
            .frozen = self.frozen,
        };

        return @constCast(offset_self);
    }

    /// Add the given offset to the memory addresses of all pointers in `self`.
    pub fn relocate(self: *Store, offset: isize) void {
        self.buffer.relocate(offset);
    }

    /// Serialized representation of a Store
    pub const Serialized = struct {
        buffer: collections.SafeList(u8).Serialized,
        frozen: if (std.debug.runtime_safety) bool else void,

        /// Serialize a Store into this Serialized struct, appending data to the writer
        pub fn serialize(
            self: *Serialized,
            store: *const Store,
            allocator: std.mem.Allocator,
            writer: *CompactWriter,
        ) std.mem.Allocator.Error!void {
            // Serialize the buffer SafeList
            try self.buffer.serialize(&store.buffer, allocator, writer);
            // Copy the frozen field
            self.frozen = store.frozen;
        }

        /// Deserialize this Serialized struct into a Store
        pub fn deserialize(self: *Serialized, offset: i64) *Store {
            // Store.Serialized should be at least as big as Store
            std.debug.assert(@sizeOf(Serialized) >= @sizeOf(Store));

            // Overwrite ourself with the deserialized version, and return our pointer after casting it to Self.
            const store = @as(*Store, @ptrFromInt(@intFromPtr(self)));

            store.* = Store{
                .buffer = self.buffer.deserialize(offset).*,
                .frozen = self.frozen,
            };

            return store;
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

    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
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

    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
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

    var indices = std.ArrayList(Idx).init(gpa);
    defer indices.deinit();

    for (test_strings) |str| {
        const idx = try original.insert(gpa, str);
        try indices.append(idx);
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

    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
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
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify all strings
    for (test_strings, 0..) |expected_str, i| {
        const idx = indices.items[i];
        const actual_str = deserialized.get(idx);
        try std.testing.expectEqualStrings(expected_str, actual_str);
    }
}

test "Store frozen state CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create and populate store
    var original = Store{};
    defer original.deinit(gpa);

    _ = try original.insert(gpa, "test1");
    _ = try original.insert(gpa, "test2");

    // Freeze the store
    original.freeze();

    // Verify store is frozen
    if (std.debug.runtime_safety) {
        try std.testing.expect(original.frozen);
    }

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_frozen_stringlit.dat", .{ .read = true });
    defer file.close();

    // Serialize using arena allocator
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
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
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify frozen state is preserved
    if (std.debug.runtime_safety) {
        try std.testing.expect(deserialized.frozen);
    }
}

test "Store.Serialized roundtrip" {
    const gpa = std.testing.allocator;

    // Create original store and add some strings
    var original = Store{};
    defer original.deinit(gpa);

    const idx1 = try original.insert(gpa, "hello");
    const idx2 = try original.insert(gpa, "world");
    const idx3 = try original.insert(gpa, "foo bar baz");

    // Freeze the store in debug mode
    original.freeze();

    // Create a CompactWriter and arena
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_file = try tmp_dir.dir.createFile("test.compact", .{ .read = true });
    defer tmp_file.close();

    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
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
    const store = deserialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))));

    // Verify the strings are accessible
    try std.testing.expectEqualStrings("hello", store.get(idx1));
    try std.testing.expectEqualStrings("world", store.get(idx2));
    try std.testing.expectEqualStrings("foo bar baz", store.get(idx3));

    // Verify frozen state is preserved
    if (std.debug.runtime_safety) {
        try std.testing.expect(store.frozen);
    }
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

    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
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
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify all strings with their exact indices
    try std.testing.expectEqualStrings("first", deserialized.get(idx1));
    try std.testing.expectEqualStrings("second", deserialized.get(idx2));
    try std.testing.expectEqualStrings("", deserialized.get(idx3));
    try std.testing.expectEqualStrings(long_str, deserialized.get(idx4));
}
