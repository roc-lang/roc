const std = @import("std");
const cols = @import("../collections.zig");

pub const SmallStringId = struct { id: u32 };

pub const SmallStringInterner = struct {
    /// A deduplicated list of strings
    strings: std.ArrayList([]u8),
    /// All string indices that have the given hash
    string_indices_per_hash: std.AutoHashMap(u32, std.ArrayList(u32)),
    /// All outer indices that have to the given hash
    outer_ids_per_hash: std.AutoHashMap(u32, std.ArrayList(SmallStringId)),
    /// A unique for every string, which may or may not correspond
    /// to the same underlying string
    outer_indices: std.ArrayList(u32),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) SmallStringInterner {
        return SmallStringInterner{
            .strings = std.ArrayList([]u8).init(allocator),
            .string_indices_per_hash = std.AutoHashMap(u32, std.ArrayList(u32)).init(allocator),
            .outer_ids_per_hash = std.AutoHashMap(u32, std.ArrayList(SmallStringId)).init(allocator),
            .outer_indices = std.ArrayList(u32).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *SmallStringInterner) void {
        var string_indices_iter = self.string_indices_per_hash.valueIterator();
        while (string_indices_iter.next()) |index_list| {
            index_list.deinit();
        }

        var outer_ids_iter = self.outer_ids_per_hash.valueIterator();
        while (outer_ids_iter.next()) |outer_id_list| {
            outer_id_list.deinit();
        }

        self.strings.deinit();
        self.string_indices_per_hash.deinit();
        self.outer_ids_per_hash.deinit();
        self.outer_indices.deinit();
    }

    pub fn insert(self: *SmallStringInterner, string: []u8) SmallStringId {
        const hash = fnvStringHash(string);

        const string_indices = self.stringIndicesForHash(hash);
        for (string_indices.items) |string_index| {
            const interned = self.strings.items[string_index];
            if (string == interned) {
                return self.addOuterIdForHash(hash, string_index);
            }
        }

        const copied_string = self.strings.allocator.alloc(u8, string.len);
        std.mem.copyForwards(u8, copied_string, string);

        const strings_len = @as(u32, self.strings.items.len);
        self.strings.append(copied_string) catch cols.exit_on_oom;

        return self.addOuterIdForHash(hash, strings_len);
    }

    fn stringIndicesForHash(self: *SmallStringInterner, hash: u32) *cols.SafeList(u32) {
        const res = self.string_indices_per_hash.getOrPut(hash) catch cols.exit_on_oom;
        if (!res.found_existing) {
            res.value_ptr = cols.SafeList(u32).init(self.allocator);
        }

        return res.value_ptr.*;
    }

    fn addOuterIdForHash(self: *SmallStringInterner, hash: u32, string_index: u32) SmallStringId {
        const len = SmallStringId{ .id = @as(u32, self.outer_indices.items.len) };
        self.outer_indices.append(string_index) catch cols.exit_on_oom;

        const res = self.outer_ids_per_hash.getOrPut(hash) catch cols.exit_on_oom;
        if (!res.found_existing) {
            res.value_ptr = cols.SafeList(u32).init(self.allocator);
        }

        res.value_ptr.append(len) catch cols.exit_on_oom;

        return len;
    }

    pub fn idsHaveSameValue(
        self: *SmallStringInterner,
        first_id: SmallStringId,
        second_id: SmallStringId,
    ) bool {
        const first_string_index = self.outer_indices[@as(usize, first_id.id)];
        const second_string_index = self.outer_indices[@as(usize, second_id.id)];

        return first_string_index == second_string_index;
    }

    pub fn lookup(self: *SmallStringInterner, string: []u8) std.ArrayList(SmallStringId).Slice {
        const hash = fnvStringHash(string);

        if (self.outer_ids_per_hash.get(hash)) |outer_ids| {
            return outer_ids.items;
        } else {
            return &.{};
        }
    }

    pub fn get(self: *SmallStringInterner, id: SmallStringId) []u8 {
        const string_index = self.outer_indices.items[@as(usize, id.id)];
        return self.strings.items[@as(usize, string_index)];
    }
};

pub const LargeStringId = struct { id: u32 };

pub const LargeStringInterner = struct {
    // these are not deduped because equality checking on large strings becomes expensive
    // and they are pretty likely unique anyway
    strings: std.ArrayList([]u8),

    pub fn init(allocator: std.mem.Allocator) LargeStringInterner {
        return LargeStringInterner{
            .strings = std.ArrayList([]u8).init(allocator),
        };
    }

    pub fn deinit(self: *LargeStringInterner) void {
        self.strings.deinit();
    }

    pub fn insert(self: *LargeStringInterner, string: []u8) LargeStringId {
        const len = self.strings.items.len;

        const copied_string = self.strings.allocator.alloc(u8, string.len) catch cols.exit_on_oom;
        std.mem.copyForwards(u8, copied_string, string);

        self.strings.append(copied_string) catch cols.exit_on_oom;

        return LargeStringId{ .id = @as(u32, len) };
    }

    pub fn get(self: *LargeStringInterner, id: LargeStringId) []u8 {
        return self.strings.items[@as(usize, id.id)];
    }
};

/// A simple string hash.
///
/// http://isthe.com/chongo/tech/comp/fnv/#FNV-1
pub fn fnvStringHash(string: []const u8) u32 {
    const FNV_PRIME_32_BIT: u32 = 16777619;
    const OFFSET_BASIS_32_BIT: u32 = 2166136261;

    var hash = OFFSET_BASIS_32_BIT;

    for (string) |byte| {
        hash *= FNV_PRIME_32_BIT;
        hash ^= @as(u32, byte);
    }

    return hash;
}
