//! Interner for an identifier name string.
const std = @import("std");
const fnvStringHash = @import("../utils.zig").fnvStringHash;
const exit_on_oom = @import("../utils.zig").exit_on_oom;
const safe_list = @import("../safe_list.zig");

/// Index this value is stored in an interner
pub const Idx = struct { id: u32 };

/// An interner for identifier names.
pub const Interner = struct {
    /// A deduplicated list of strings
    strings: std.ArrayList([]u8),
    /// All string indices that have the given hash
    string_indices_per_hash: std.AutoHashMap(u32, std.ArrayList(u32)),
    /// All outer indices that have to the given hash
    outer_ids_per_hash: std.AutoHashMap(u32, std.ArrayList(Idx)),
    /// A unique for every string, which may or may not correspond
    /// to the same underlying string
    outer_indices: std.ArrayList(u32),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Interner {
        return Interner{
            .strings = std.ArrayList([]u8).init(allocator),
            .string_indices_per_hash = std.AutoHashMap(u32, std.ArrayList(u32)).init(allocator),
            .outer_ids_per_hash = std.AutoHashMap(u32, std.ArrayList(Idx)).init(allocator),
            .outer_indices = std.ArrayList(u32).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Interner) void {
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

    pub fn insert(self: *Interner, string: []u8) Idx {
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
        self.strings.append(copied_string) catch exit_on_oom;

        return self.addOuterIdForHash(hash, strings_len);
    }

    fn stringIndicesForHash(self: *Interner, hash: u32) *safe_list.SafeList(u32) {
        const res = self.string_indices_per_hash.getOrPut(hash) catch exit_on_oom;
        if (!res.found_existing) {
            res.value_ptr = safe_list.SafeList(u32).init(self.allocator);
        }

        return res.value_ptr.*;
    }

    fn addOuterIdForHash(self: *Interner, hash: u32, string_index: u32) Idx {
        const len = Idx{ .id = @as(u32, self.outer_indices.items.len) };
        self.outer_indices.append(string_index) catch exit_on_oom;

        const res = self.outer_ids_per_hash.getOrPut(hash) catch exit_on_oom;
        if (!res.found_existing) {
            res.value_ptr = safe_list.SafeList(u32).init(self.allocator);
        }

        res.value_ptr.append(len) catch exit_on_oom;

        return len;
    }

    pub fn idsHaveSameValue(
        self: *Interner,
        first_id: Idx,
        second_id: Idx,
    ) bool {
        const first_string_index = self.outer_indices[@as(usize, first_id.id)];
        const second_string_index = self.outer_indices[@as(usize, second_id.id)];

        return first_string_index == second_string_index;
    }

    pub fn lookup(self: *Interner, string: []u8) std.ArrayList(Idx).Slice {
        const hash = fnvStringHash(string);

        if (self.outer_ids_per_hash.get(hash)) |outer_ids| {
            return outer_ids.items;
        } else {
            return &.{};
        }
    }

    pub fn get(self: *Interner, id: Idx) []u8 {
        const string_index = self.outer_indices.items[@as(usize, id.id)];
        return self.strings.items[@as(usize, string_index)];
    }
};
