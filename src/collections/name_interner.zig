const std = @import("std");
const interner = @import("./string_interner.zig");

/// A typed ID for an interned tag name.
pub const TagNameId = struct { id: u32 };

/// A thin wrapper around a small string interner that
/// allows for typed IDs of tag names which can't be
/// interchanged with other interned string IDs.
pub const TagNameInterner = struct {
    names: interner.SmallStringInterner,

    pub fn init(allocator: std.mem.Allocator) TagNameInterner {
        return TagNameInterner{ .names = interner.SmallStringInterner.init(allocator) };
    }

    pub fn deinit(self: *TagNameInterner) void {
        self.names.deinit();
    }

    pub fn insert(self: *TagNameInterner, name: []u8) TagNameId {
        return TagNameId{ .id = self.names.insert(name).id };
    }

    pub fn get(self: *TagNameInterner, id: TagNameId) []u8 {
        return self.names.get(interner.SmallStringInternerId{ .id = id.id });
    }
};

/// A typed ID for an interned record field name.
pub const FieldNameId = struct { id: u32 };

/// A thin wrapper around a small string interner that
/// allows for typed IDs of record field names which can't be
/// interchanged with other interned string IDs.
pub const FieldNameInterner = struct {
    names: interner.SmallStringInterner,

    pub fn init(allocator: std.mem.Allocator) FieldNameInterner {
        return FieldNameInterner{ .names = interner.SmallStringInterner.init(allocator) };
    }

    pub fn deinit(self: *FieldNameInterner) void {
        self.names.deinit();
    }

    pub fn insert(self: *FieldNameInterner, name: []u8) FieldNameId {
        return FieldNameId{ .id = self.names.insert(name).id };
    }

    pub fn get(self: *FieldNameInterner, id: FieldNameId) []u8 {
        return self.names.get(interner.SmallStringInternerId{ .id = id.id });
    }
};
