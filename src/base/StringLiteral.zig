//! Strings written inline in Roc code, e.g. `x = "abc"`.

const std = @import("std");
const collections = @import("collections");
const serialization = @import("serialization");
const testing = std.testing;

/// The index of this string in a `StringLiteral.Store`.
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
    buffer: std.ArrayListUnmanaged(u8) = .{},
    /// When true, no new entries can be added to the store.
    /// This is set after canonicalization is complete, so that
    /// we know it's safe to serialize/deserialize the part of the interner
    /// that goes from ident to string, because we don't go from string to ident anymore.
    frozen: if (std.debug.runtime_safety) bool else void = if (std.debug.runtime_safety) false else {},

    /// Intiizalizes a `StringLiteral.Store` with capacity `bytes` of space.
    /// Note this specifically is the number of bytes for storing strings.
    /// The string `hello, world!` will use 14 bytes including the null terminator.
    pub fn initCapacityBytes(gpa: std.mem.Allocator, bytes: usize) std.mem.Allocator.Error!Store {
        return .{
            .buffer = try std.ArrayListUnmanaged(u8).initCapacity(gpa, bytes),
        };
    }

    /// Deinitialize a `StringLiteral.Store`'s memory.
    pub fn deinit(self: *Store, gpa: std.mem.Allocator) void {
        self.buffer.deinit(gpa);
    }

    /// Insert a new string into a `StringLiteral.Store`.
    ///
    /// Does not deduplicate, as string literals are expected to be large and mostly unique.
    pub fn insert(self: *Store, gpa: std.mem.Allocator, string: []const u8) std.mem.Allocator.Error!Idx {
        if (std.debug.runtime_safety) {
            std.debug.assert(!self.frozen); // Should not insert into a frozen store
        }
        const str_len: u32 = @truncate(string.len);

        const str_len_bytes = std.mem.asBytes(&str_len);
        try self.buffer.appendSlice(gpa, str_len_bytes);

        const string_content_start = self.buffer.items.len;

        try self.buffer.appendSlice(gpa, string);

        return @enumFromInt(@as(u32, @intCast(string_content_start)));
    }

    /// Get a string literal's text from this `Store`.
    pub fn get(self: *const Store, idx: Idx) []u8 {
        const idx_u32: u32 = @intCast(@intFromEnum(idx));
        const str_len = std.mem.bytesAsValue(u32, self.buffer.items[idx_u32 - 4 .. idx_u32]).*;
        return self.buffer.items[idx_u32 .. idx_u32 + str_len];
    }

    /// Freeze the store, preventing any new entries from being added.
    pub fn freeze(self: *Store) void {
        if (std.debug.runtime_safety) {
            self.frozen = true;
        }
    }

    /// Calculate the size needed to serialize this StringLiteral.Store
    pub fn serializedSize(self: *const Store) usize {
        // Header: 4 bytes for buffer length
        // Data: buffer.items.len bytes
        const raw_size = @sizeOf(u32) + self.buffer.items.len;
        // Align to SERIALIZATION_ALIGNMENT to maintain alignment for subsequent data
        return std.mem.alignForward(usize, raw_size, serialization.SERIALIZATION_ALIGNMENT);
    }

    /// Serialize this StringLiteral.Store into the provided buffer
    /// Buffer must be at least serializedSize() bytes
    pub fn serializeInto(self: *const Store, buffer: []u8) ![]u8 {
        const size = self.serializedSize();
        if (buffer.len < size) return error.BufferTooSmall;

        // Write buffer length
        const len_ptr = @as(*u32, @ptrCast(@alignCast(buffer.ptr)));
        len_ptr.* = @intCast(self.buffer.items.len);

        // Write buffer data
        if (self.buffer.items.len > 0) {
            @memcpy(buffer[@sizeOf(u32) .. @sizeOf(u32) + self.buffer.items.len], self.buffer.items);
        }

        // Zero out any padding bytes
        const actual_size = @sizeOf(u32) + self.buffer.items.len;
        if (actual_size < size) {
            @memset(buffer[actual_size..size], 0);
        }

        return buffer[0..size];
    }

    /// Deserialize a StringLiteral.Store from the provided buffer
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
            store.buffer.appendSliceAssumeCapacity(buffer[data_start .. data_start + buffer_len]);
        }

        return store;
    }
};

test "insert" {
    const gpa = testing.allocator;

    var interner = Store{};
    defer interner.deinit(gpa);

    const str_1 = "abc".*;
    const str_2 = "defg".*;
    const idx_1 = try interner.insert(gpa, &str_1);
    const idx_2 = try interner.insert(gpa, &str_2);

    try testing.expectEqualStrings("abc", interner.get(idx_1));
    try testing.expectEqualStrings("defg", interner.get(idx_2));
}

test "StringLiteral.Store serialization comprehensive" {
    const gpa = testing.allocator;

    var store = Store{};
    defer store.deinit(gpa);

    // Add various test strings including edge cases
    _ = try store.insert(gpa, "hello");
    _ = try store.insert(gpa, "world");
    _ = try store.insert(gpa, "test string with 🦎 unicode");
    _ = try store.insert(gpa, ""); // empty string
    _ = try store.insert(gpa, "\x00\x01\x02"); // binary data
    _ = try store.insert(gpa, "🦎🚀✨"); // emoji
    _ = try store.insert(gpa, "日本語"); // non-latin script
    _ = try store.insert(gpa, "test\n\r\t"); // control characters
    _ = try store.insert(gpa, "very very very very very very long string that exceeds normal buffer sizes and might cause issues with memory management");

    // Test serialization
    try serialization.testing.testSerialization(Store, &store, gpa);
}

test "StringLiteral.Store empty store serialization" {
    const gpa = testing.allocator;

    var empty_store = Store{};
    defer empty_store.deinit(gpa);

    try serialization.testing.testSerialization(Store, &empty_store, gpa);
}
