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
    
    // Add a deliberate failure to confirm test is running
    try testing.expectEqual(@as(usize, 1), deserialized.bytes.len());
}
