//! The name of a field in a record, e.g. `foo` in `{ foo: 123 }`.
const std = @import("std");
const IdentName = @import("IdentName.zig");
const exitOnOom = @import("../utils.zig").exitOnOom;

/// The index for a record field name in a FieldName.Interner.
pub const Idx = enum(u32) { _ };

/// An interner for a record field name.
///
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
        const name_idx = self.names.insert(name);
        return @enumFromInt(@intFromEnum(name_idx));
    }

    pub fn get(self: *Interner, idx: Idx) []u8 {
        return self.names.get(@enumFromInt(@intFromEnum(idx)));
    }
};
