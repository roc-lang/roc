//! Strings written inline in Roc code, e.g. `x = "abc"`.

const std = @import("std");
const collections = @import("collections");
const serialization = @import("serialization");
const testing = std.testing;

const CompactWriter = serialization.CompactWriter;

/// The index of this string in a `StringLiteral.Store`.
pub const Idx = enum(u32) { _ };

/// An interner for string literals.
///
/// We avoid using the IdentInterner for string literals since
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

    /// Intiizalizes a `StringLiteral.Store` with capacity `bytes` of space.
    /// Note this specifically is the number of bytes for storing strings.
    /// The string `hello, world!` will use 14 bytes including the null terminator.
    pub fn initCapacityBytes(gpa: std.mem.Allocator, bytes: usize) std.mem.Allocator.Error!Store {
        return .{
            .buffer = try collections.SafeList(u8).initCapacity(gpa, bytes),
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

    /// Calculate the size needed to serialize this StringLiteral.Store
    pub fn serializedSize(self: *const Store) usize {
        // Header: 4 bytes for buffer length
        // Data: buffer.items.len bytes
        const raw_size = @sizeOf(u32) + self.buffer.len();
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
        len_ptr.* = @intCast(self.buffer.len());

        // Write buffer data
        if (self.buffer.len() > 0) {
            @memcpy(buffer[@sizeOf(u32) .. @sizeOf(u32) + self.buffer.len()], self.buffer.items.items);
        }

        // Zero out any padding bytes
        const actual_size = @sizeOf(u32) + self.buffer.len();
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
            // StringLiteral.Store.Serialized should be at least as big as StringLiteral.Store
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
