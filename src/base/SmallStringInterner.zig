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
        .bytes = try std.ArrayListUnmanaged(u8).initCapacity(gpa, capacity * bytes_per_string),
        .strings = std.StringHashMapUnmanaged(Idx){},
    };

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
    const string_offset = if (self.strings.get(string)) |existing_offset|
        existing_offset
    else blk: {
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
