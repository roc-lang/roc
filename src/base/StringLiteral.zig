//! Strings written in-line in Roc code, e.g. `x = "abc"`.
const std = @import("std");
const collections = @import("../collections.zig");

const exitOnOom = collections.utils.exitOnOom;

/// The index of this string in a StringLiteral.Interner.
pub const Idx = enum(u32) { _ };

/// An interner for string literals.
///
/// We avoid using the SmallStringInterner for string literals since
/// they are expected to be almost all unique and also larger, meaning
/// not worth the equality checking cost for depuplicating.
pub const Store = struct {
    strings: std.ArrayList([]u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Store {
        return Store{
            .strings = std.ArrayList([]u8).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Store) void {
        self.strings.deinit();
    }

    pub fn insert(self: *Store, string: []u8) Idx {
        const len = self.strings.items.len;

        const copied_string = self.allocator.alloc(u8, string.len) catch exitOnOom();
        std.mem.copyForwards(u8, copied_string, string);

        self.strings.append(copied_string) catch exitOnOom();

        return @enumFromInt(@as(u32, @intCast(len)));
    }

    pub fn get(self: *Store, idx: Idx) []u8 {
        return self.strings.items[@as(usize, @intFromEnum(idx))];
    }
};
