//! Strings written in-line in Roc code, e.g. `x = "abc"`.
const std = @import("std");
const exitOnOom = @import("../utils.zig").exitOnOom;

/// The index of this string in a StringLiteral.Interner.
pub const Idx = enum(u32) { _ };

/// An interner for string literals.
///
/// As opposed to the [IdentName.Interner], this interner does not
/// deduplicate its values because they are expected to be almost all unique,
/// and also larger, meaning more expensive to do equality checking on.
pub const Interner = struct {
    strings: std.ArrayList([]u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Interner {
        return Interner{
            .strings = std.ArrayList([]u8).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Interner) void {
        self.strings.deinit();
    }

    pub fn insert(self: *Interner, string: []u8) Idx {
        const len = self.strings.items.len;

        const copied_string = self.allocator.alloc(u8, string.len) catch exitOnOom();
        std.mem.copyForwards(u8, copied_string, string);

        self.strings.append(copied_string) catch exitOnOom();

        return @enumFromInt(@as(u32, len));
    }

    pub fn get(self: *Interner, idx: Idx) []u8 {
        return self.strings.items[@as(usize, @intFromEnum(idx))];
    }
};
