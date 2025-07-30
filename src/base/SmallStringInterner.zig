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
const serialization = @import("serialization");

const Region = mod.Region;
const CompactWriter = serialization.CompactWriter;

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

/// Initialize a `SmallStringInterner` with the specified capacity.
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

/// Resize the hash table when it gets too full.
fn resizeHashTable(self: *Self, gpa: std.mem.Allocator) std.mem.Allocator.Error!void {
    const old_table = self.hash_table;
    const new_size = old_table.len() * 2;

    // Create new hash table initialized to zeros
    self.hash_table = try collections.SafeList(Idx).initCapacity(gpa, new_size);
    try self.hash_table.items.ensureTotalCapacityPrecise(gpa, new_size);
    self.hash_table.items.items.len = new_size;
    @memset(self.hash_table.items.items, .unused);

    // Rehash all existing entries
    for (old_table.items.items) |idx| {
        if (idx != .unused) {
            // Get the string for this index
            const string = self.getText(idx);
            const result = self.findStringOrSlot(string);
            self.hash_table.items.items[result.slot] = idx;
        }
    }

    @constCast(&old_table).deinit(gpa);
}

/// Add a string to this interner, returning a unique, serial index.
pub fn insert(self: *Self, gpa: std.mem.Allocator, string: []const u8) std.mem.Allocator.Error!Idx {
    if (std.debug.runtime_safety) {
        std.debug.assert(!self.frozen); // Should not insert into a frozen interner
    }

    // Check if we need to resize the hash table (when 80% full = entry_count * 5 >= hash_table.len() * 4)
    if (self.entry_count * 5 >= self.hash_table.len() * 4) {
        try self.resizeHashTable(gpa);
    }

    // Find the string or the slot where it should be inserted
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

/// Check if a string is already interned in this interner, used for generating unique names.
pub fn contains(self: *const Self, string: []const u8) bool {
    const result = self.findStringOrSlot(string);
    return result.idx != null;
}

/// Get a reference to the text for an interned string.
pub fn getText(self: *const Self, idx: Idx) []u8 {
    const bytes_slice = self.bytes.items.items;
    const start = @intFromEnum(idx);

    return std.mem.sliceTo(bytes_slice[start..], 0);
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
    writer: *CompactWriter,
) std.mem.Allocator.Error!*const Self {
    // First, write the struct
    const offset_self = try writer.appendAlloc(allocator, Self);

    // Then serialize the bytes and hash_table SafeLists and update the struct
    const serialized_bytes = try self.bytes.serialize(allocator, writer);
    const serialized_hash_table = try self.hash_table.serialize(allocator, writer);

    offset_self.* = .{
        .bytes = serialized_bytes.*,
        .hash_table = serialized_hash_table.*,
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
