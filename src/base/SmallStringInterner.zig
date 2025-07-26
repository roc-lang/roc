//! An interner for short and likely repeated strings in in a Roc file.
//!
//! This interner deduplicates its string values because they are
//! expected to be small and often repeated since they tend to represent
//! the same value being referenced in many places. The indices assigned
//! to each interned string are serial, meaning they can be used for
//! arrays with values corresponding 1-to-1 to interned values, e.g. regions.

const std = @import("std");
const mod = @import("mod.zig");

const Region = mod.Region;

const Self = @This();

/// The raw underlying bytes for all strings.
/// Since strings are small, they are simply null terminated.
/// This uses only 1 byte to encode the size and is cheap to scan.
bytes: std.ArrayListUnmanaged(u8) = .{},
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
        .bytes = std.ArrayListUnmanaged(u8){},
        .strings = std.StringHashMapUnmanaged(Idx){},
    };

    // Properly initialize the bytes array to ensure clean state
    try self.bytes.ensureTotalCapacity(gpa, capacity * bytes_per_string);

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
        try self.bytes.ensureUnusedCapacity(gpa, string.len + 1);
        const new_offset: Idx = @enumFromInt(self.bytes.items.len);

        self.bytes.appendSliceAssumeCapacity(string);
        self.bytes.appendAssumeCapacity(0);

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
    return std.mem.sliceTo(self.bytes.items[@intFromEnum(idx)..], 0);
}

/// Freeze the interner, preventing any new entries from being added.
pub fn freeze(self: *Self) void {
    if (std.debug.runtime_safety) {
        self.frozen = true;
    }
}

/// Append this SmallStringInterner to an iovec writer for serialization
pub fn appendToIovecs(self: *const Self, writer: anytype) !usize {
    const serialization = @import("serialization");

    // Create a mutable copy of self as a regular byte buffer
    const interner_copy_buffer = try writer.allocator.alloc(u8, @sizeOf(Self));
    @memcpy(interner_copy_buffer, std.mem.asBytes(self));

    // Track this allocation so it gets freed when writer is deinitialized
    try writer.owned_buffers.append(interner_copy_buffer);

    // Get access to the struct in the buffer for easier manipulation
    const interner_copy = @as(*Self, @ptrCast(@alignCast(interner_copy_buffer.ptr)));

    // Serialize bytes array
    const bytes_offset = if (self.bytes.items.len > 0) blk: {
        const offset = try writer.appendBytes(u8, self.bytes.items);
        break :blk offset;
    } else 0;

    // Update pointer in the copy to use offset
    interner_copy.bytes.items.ptr = if (bytes_offset == 0)
        @ptrFromInt(serialization.iovec_serialize.EMPTY_ARRAY_SENTINEL)
    else
        @ptrFromInt(bytes_offset);
    interner_copy.bytes.items.len = self.bytes.items.len;

    // Clear strings hash map - it's only used for deduplication during insertion
    // and can be rebuilt on demand after deserialization
    interner_copy.strings = .{};

    // Now that all pointers have been converted to offsets, add the copy to iovecs
    const struct_offset = try writer.appendBytes(Self, interner_copy_buffer);

    return struct_offset;
}

/// Relocate all pointers in this SmallStringInterner by the given offset
pub fn relocate(self: *Self, offset: isize) void {
    const serialization = @import("serialization");

    // Relocate bytes array
    if (self.bytes.items.len > 0) {
        const old_ptr = @intFromPtr(self.bytes.items.ptr);
        // Skip relocation if this is a sentinel value
        if (old_ptr != serialization.iovec_serialize.EMPTY_ARRAY_SENTINEL) {
            const new_ptr = @as(usize, @intCast(@as(isize, @intCast(old_ptr)) + offset));
            self.bytes.items.ptr = @ptrFromInt(new_ptr);
        }
    }

    // strings hash map is empty after deserialization, so no need to relocate
}
