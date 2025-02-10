//! The name for any entity in a Roc file, such as an identifier or a type name.
const std = @import("std");
const utils = @import("../utils.zig");

const exitOnOom = utils.exitOnOom;
const fnvStringHash = utils.fnvStringHash;

/// The index this name is stored in an IdentName.Interner.
pub const Idx = enum(u32) { _ };

/// An interner for identifier names.
///
/// This interner deduplicates its string values because they are
/// expected to be small and often repeated since they tend to represent
/// the same value being referenced in many places. The indices assigned
/// to each interned string are serial, meaning they can be used for
/// arrays with values corresponding 1-to-1 to interned values, e.g. regions.
pub const Interner = struct {
    /// A deduplicated list of strings
    strings: std.ArrayList([]u8),
    /// All string indices that have the given hash
    string_indices_per_hash: std.AutoHashMap(u32, std.ArrayList(u32)),
    /// All outer indices that have the given string index
    outer_ids_per_string_index: std.AutoHashMap(u32, std.ArrayList(Idx)),
    /// A unique ID for every string, which may or may not correspond
    /// to the same underlying string
    outer_indices: std.ArrayList(u32),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Interner {
        return Interner{
            .strings = std.ArrayList([]u8).init(allocator),
            .string_indices_per_hash = std.AutoHashMap(u32, std.ArrayList(u32)).init(allocator),
            .outer_ids_per_string_index = std.AutoHashMap(u32, std.ArrayList(Idx)).init(allocator),
            .outer_indices = std.ArrayList(Idx).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Interner) void {
        var string_indices_iter = self.string_indices_per_hash.valueIterator();
        while (string_indices_iter.next()) |index_list| {
            index_list.deinit();
        }

        var outer_ids_iter = self.outer_ids_per_string_index.valueIterator();
        while (outer_ids_iter.next()) |outer_id_list| {
            outer_id_list.deinit();
        }

        self.strings.deinit();
        self.string_indices_per_hash.deinit();
        self.outer_ids_per_string_index.deinit();
        self.outer_indices.deinit();
    }

    /// Add an ident name to this interner, returning a unique, serial index.
    pub fn insert(self: *Interner, string: []u8) Idx {
        const hash = fnvStringHash(string);

        const string_indices = self.stringIndicesForHash(hash);
        for (string_indices.items) |string_index| {
            const interned = self.strings.items[string_index];
            if (string == interned) {
                return self.addOuterIdForStringIndex(string_index);
            }
        }

        const copied_string = self.strings.allocator.alloc(u8, string.len);
        std.mem.copyForwards(u8, copied_string, string);

        const strings_len = @as(u32, self.strings.items.len);
        self.strings.append(copied_string) catch exitOnOom();

        return self.addOuterIdForStringIndex(strings_len);
    }

    fn stringIndicesForHash(self: *Interner, hash: u32) *std.ArrayList(u32) {
        const res = self.string_indices_per_hash.getOrPut(hash) catch exitOnOom();
        if (!res.found_existing) {
            res.value_ptr = std.ArrayList(u32).init(self.allocator);
        }

        return res.value_ptr.*;
    }

    fn addOuterIdForStringIndex(self: *Interner, string_index: u32) Idx {
        const len: Idx = @enumFromInt(@as(u32, self.outer_indices.items.len));
        self.outer_indices.append(string_index) catch exitOnOom();

        const res = self.outer_ids_per_string_index.getOrPut(string_index) catch exitOnOom();
        if (!res.found_existing) {
            res.value_ptr = std.ArrayList(u32).init(self.allocator);
        }

        res.value_ptr.append(len) catch exitOnOom();

        return len;
    }

    /// Check if two ident names have the same text in constant time.
    pub fn identsHaveSameText(
        self: *Interner,
        first_idx: Idx,
        second_idx: Idx,
    ) bool {
        const first_string_index = self.outer_indices.items[@as(usize, @intFromEnum(first_idx))];
        const second_string_index = self.outer_indices.items[@as(usize, @intFromEnum(second_idx))];

        return first_string_index == second_string_index;
    }

    /// Return a slice of all indices of names interned with the given text.
    pub fn lookup(self: *Interner, string: []u8) []Idx {
        const hash = fnvStringHash(string);
        const indices_for_hash = self.string_indices_per_hash.get(hash) orelse return &.{};

        for (indices_for_hash) |string_index| {
            if (self.strings.items[string_index] == string) {
                return self.outer_ids_per_string_index.get(string_index).?;
            }
        }

        return &.{};
    }

    /// Get a reference to the text for an interned ident name.
    pub fn get(self: *Interner, idx: Idx) []u8 {
        const string_index = self.outer_indices.items[@as(usize, @intFromEnum(idx))];
        return self.strings.items[@as(usize, string_index)];
    }
};
