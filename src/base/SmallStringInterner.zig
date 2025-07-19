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
strings: std.StringHashMapUnmanaged(StringIdx) = .{},
/// A unique ID for every string. This is fundamentally an index into bytes.
/// It also maps 1:1 with a region at the same index.
outer_indices: std.ArrayListUnmanaged(StringIdx) = .{},
regions: std.ArrayListUnmanaged(Region) = .{},
/// When true, no new entries can be added to the interner.
/// This is set after parsing is complete.
frozen: if (std.debug.runtime_safety) bool else void = if (std.debug.runtime_safety) false else {},

/// A unique index for a deduped string in this interner.
pub const Idx = enum(u32) { _ };

/// Initialize a `SmallStringInterner` with the specified capacity.
pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) std.mem.Allocator.Error!Self {
    // TODO: tune this. Rough assumption that average small string is 4 bytes.
    const bytes_per_string = 4;

    var self = Self{
        .bytes = try std.ArrayListUnmanaged(u8).initCapacity(gpa, capacity * bytes_per_string),
        .strings = std.StringHashMapUnmanaged(StringIdx){},
        .outer_indices = try std.ArrayListUnmanaged(StringIdx).initCapacity(gpa, capacity),
        .regions = try std.ArrayListUnmanaged(Region).initCapacity(gpa, capacity),
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

    self.outer_indices.deinit(gpa);
    self.regions.deinit(gpa);
}

/// Add a string to this interner, returning a unique, serial index.
pub fn insert(self: *Self, gpa: std.mem.Allocator, string: []const u8, region: Region) std.mem.Allocator.Error!Idx {
    if (std.debug.runtime_safety) {
        std.debug.assert(!self.frozen); // Should not insert into a frozen interner
    }

    // Check if string already exists for deduplication
    const string_offset = if (self.strings.get(string)) |existing_offset|
        existing_offset
    else blk: {
        // String doesn't exist, add it to bytes
        try self.bytes.ensureUnusedCapacity(gpa, string.len + 1);
        const new_offset: StringIdx = @enumFromInt(self.bytes.items.len);

        self.bytes.appendSliceAssumeCapacity(string);
        self.bytes.appendAssumeCapacity(0);

        // Add to HashMap for future deduplication
        const owned_string = try gpa.dupe(u8, string);
        try self.strings.put(gpa, owned_string, new_offset);

        break :blk new_offset;
    };

    // Always create a new serial index, even for duplicate strings
    // This maintains the 1:1 mapping between indices and regions
    return self.addOuterIdForStringIndex(gpa, string_offset, region);
}

fn addOuterIdForStringIndex(self: *Self, gpa: std.mem.Allocator, string_offset: StringIdx, region: Region) std.mem.Allocator.Error!Idx {
    if (std.debug.runtime_safety) {
        std.debug.assert(!self.frozen); // Should not add outer IDs to a frozen interner
    }
    const len: Idx = @enumFromInt(@as(u32, @truncate(self.outer_indices.items.len)));
    try self.outer_indices.append(gpa, string_offset);
    try self.regions.append(gpa, region);

    return len;
}

/// Check if two indices have the same text in constant time.
pub fn indicesHaveSameText(
    self: *const Self,
    first_idx: Idx,
    second_idx: Idx,
) bool {
    const first_string_offset = self.outer_indices.items[@as(usize, @intFromEnum(first_idx))];
    const second_string_offset = self.outer_indices.items[@as(usize, @intFromEnum(second_idx))];
    return first_string_offset == second_string_offset;
}

/// Get a reference to the text for an interned string.
pub fn getText(self: *const Self, idx: Idx) []u8 {
    const string_offset = self.outer_indices.items[@as(usize, @intFromEnum(idx))];

    return std.mem.sliceTo(self.bytes.items[@intFromEnum(string_offset)..], 0);
}

/// Get the region for an interned string.
pub fn getRegion(self: *const Self, idx: Idx) Region {
    return self.regions.items[@as(usize, @intFromEnum(idx))];
}

/// Freeze the interner, preventing any new entries from being added.
pub fn freeze(self: *Self) void {
    if (std.debug.runtime_safety) {
        self.frozen = true;
    }
}

/// TODO
pub const StringIdx = enum(u32) {
    _,
};
