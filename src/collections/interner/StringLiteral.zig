//! Interner for a string literal
const std = @import("std");
const exit_on_oom = @import("../utils.zig").exit_on_oom;

/// Index this value is stored in an interner
pub const Idx = struct { id: u32 };

/// Interner for a string literal
pub const Interner = struct {
    // these are not deduped because equality checking on large strings becomes expensive
    // and they are pretty likely unique anyway
    strings: std.ArrayList([]u8),

    pub fn init(allocator: std.mem.Allocator) Interner {
        return Interner{
            .strings = std.ArrayList([]u8).init(allocator),
        };
    }

    pub fn deinit(self: *Interner) void {
        self.strings.deinit();
    }

    pub fn insert(self: *Interner, string: []u8) Idx {
        const len = self.strings.items.len;

        const copied_string = self.strings.allocator.alloc(u8, string.len) catch exit_on_oom;
        std.mem.copyForwards(u8, copied_string, string);

        self.strings.append(copied_string) catch exit_on_oom;

        return Idx{ .id = @as(u32, len) };
    }

    pub fn get(self: *Interner, id: Idx) []u8 {
        return self.strings.items[@as(usize, id.id)];
    }
};
