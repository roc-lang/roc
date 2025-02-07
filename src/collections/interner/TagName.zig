//! The name of a tag, e.g. `Foo` in `Foo("abc", 123)`.
const std = @import("std");
const IdentName = @import("IdentName.zig");

/// The index for a tag name in a TagName.Interner.
pub const Idx = enum(u32) { _ };

/// An interner for a tag name.
///
/// A thin wrapper around a small string interner that
/// allows for typed IDs of tag names which can't be
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
        return @enumFromInt(self.names.insert(name).id);
    }

    pub fn get(self: *Interner, id: Idx) []u8 {
        return self.names.get(@intFromEnum(id));
    }
};
