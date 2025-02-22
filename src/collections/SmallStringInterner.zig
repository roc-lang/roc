//! An interner for short and likely repeated strings in in a Roc file.
//!
//! This interner deduplicates its string values because they are
//! expected to be small and often repeated since they tend to represent
//! the same value being referenced in many places. The indices assigned
//! to each interned string are serial, meaning they can be used for
//! arrays with values corresponding 1-to-1 to interned values, e.g. regions.

const std = @import("std");
const utils = @import("./utils.zig");
const Region = @import("../base/Region.zig");

const exitOnOom = utils.exitOnOom;
const fnvStringHash = utils.fnvStringHash;

const Self = @This();

/// A deduplicated list of strings
strings: std.ArrayList([]u8),
/// All string indices that have the given hash
string_indices_per_hash: std.AutoHashMap(u32, std.ArrayList(u32)),
/// All outer indices that have the given string index
outer_ids_per_string_index: std.AutoHashMap(u32, std.ArrayList(Idx)),
/// A unique ID for every string, which may or may not correspond
/// to the same underlying string
outer_indices: std.ArrayList(u32),
regions: std.ArrayList(Region),
arena: *std.heap.ArenaAllocator,

/// A unique index for a deduped string in this interner.
pub const Idx = enum(u32) { _ };

pub fn init(arena: *std.heap.ArenaAllocator) Self {
    return Self{
        .strings = std.ArrayList([]u8).init(arena.allocator()),
        .string_indices_per_hash = std.AutoHashMap(u32, std.ArrayList(u32)).init(arena.allocator()),
        .outer_ids_per_string_index = std.AutoHashMap(u32, std.ArrayList(Idx)).init(arena.allocator()),
        .outer_indices = std.ArrayList(u32).init(arena.allocator()),
        .regions = std.ArrayList(Region).init(arena.allocator()),
        .arena = arena,
    };
}

/// Add a string to this interner, returning a unique, serial index.
pub fn insert(self: *Self, string: []const u8, region: Region) Idx {
    const hash = fnvStringHash(string);

    const string_indices = self.stringIndicesForHash(hash);
    for (string_indices.items) |string_index| {
        const interned = self.strings.items[string_index];
        if (std.mem.eql(u8, string, interned)) {
            return self.addOuterIdForStringIndex(string_index, region);
        }
    }

    const copied_string = self.arena.allocator().alloc(u8, string.len) catch exitOnOom();
    std.mem.copyForwards(u8, copied_string, string);

    const strings_len: u32 = @truncate(self.strings.items.len);
    self.strings.append(copied_string) catch exitOnOom();

    return self.addOuterIdForStringIndex(strings_len, region);
}

fn stringIndicesForHash(self: *Self, hash: u32) *std.ArrayList(u32) {
    const res = self.string_indices_per_hash.getOrPut(hash) catch exitOnOom();
    if (!res.found_existing) {
        res.value_ptr.* = std.ArrayList(u32).init(self.arena.allocator());
    }

    return res.value_ptr;
}

fn addOuterIdForStringIndex(self: *Self, string_index: u32, region: Region) Idx {
    const len: Idx = @enumFromInt(@as(u32, @truncate(self.outer_indices.items.len)));
    self.outer_indices.append(string_index) catch exitOnOom();
    self.regions.append(region) catch exitOnOom();

    const res = self.outer_ids_per_string_index.getOrPut(string_index) catch exitOnOom();
    if (!res.found_existing) {
        res.value_ptr.* = std.ArrayList(Idx).init(self.arena.allocator());
    }

    res.value_ptr.append(len) catch exitOnOom();

    return len;
}

/// Check if two indices have the same text in constant time.
pub fn indicesHaveSameText(
    self: *Self,
    first_idx: Idx,
    second_idx: Idx,
) bool {
    const first_string_index = self.outer_indices.items[@as(usize, @intFromEnum(first_idx))];
    const second_string_index = self.outer_indices.items[@as(usize, @intFromEnum(second_idx))];

    return first_string_index == second_string_index;
}

/// Return a slice of all indices of strings interned with the given text.
pub fn lookup(self: *Self, string: []u8) []Idx {
    const hash = fnvStringHash(string);
    const indices_for_hash = if (self.string_indices_per_hash.get(hash)) |list|
        list.items
    else
        return &.{};

    for (indices_for_hash) |string_index| {
        if (std.mem.eql(u8, self.strings.items[string_index], string)) {
            return self.outer_ids_per_string_index.get(string_index).?.items;
        }
    }

    return &.{};
}

/// Get a reference to the text for an interned string.
pub fn getText(self: *Self, idx: Idx) []u8 {
    const string_index = self.outer_indices.items[@as(usize, @intFromEnum(idx))];
    return self.strings.items[@as(usize, string_index)];
}

/// Get the region for an interned string.
pub fn getRegion(self: *Self, idx: Idx) Region {
    return self.regions.items[@as(usize, @intFromEnum(idx))];
}
