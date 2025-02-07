//! Interner for a record field name.
const std = @import("std");
const IdentName = @import("IdentName.zig");

/// Index this value is stored in an interner
pub const Idx = struct { id: u32 };

/// A thin wrapper around a small string interner that
/// allows for typed IDs of record field names which can't be
/// interchanged with other interned string IDs.
pub const Interner = struct {
    names: IdentName.Interner,

    pub fn init(allocator: std.mem.Allocator) Interner {
        return Interner{ .names = IdentName.Interner.init(allocator) };
    }

    pub fn deinit(self: *Interner) void {
        self.names.deinit();
    }

    pub fn insert(self: *Interner, name: []u8) Idx {
        return Idx{ .id = self.names.insert(name).id };
    }

    pub fn get(self: *Interner, id: Idx) []u8 {
        return self.names.get(IdentName.Idx{ .id = id.id });
    }
};
