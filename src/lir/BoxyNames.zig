//! LIR-owned Boxy name identities, independent of literal backing offsets.

const std = @import("std");
const base = @import("base");

const Self = @This();

/// Dense identity shared by Boxy statements, descriptors, and field names.
pub const Id = enum(u32) {
    /// Names a declared nominal record's unnamed padding field, which holds
    /// no value: inspection skips it, and it corresponds only to the padding
    /// field at the same position of another descriptor.
    padding_field = std.math.maxInt(u32),
    _,
};

/// Insertion uses the shared interner. Images retain only bytes and ranges;
/// its hash index is compiler scratch and is never rebuilt by a runtime view.
interner: base.SerialStringInterner = .{},

/// Intern a spelling once in this program's Boxy identity domain.
pub fn insert(self: *Self, allocator: std.mem.Allocator, text: []const u8) std.mem.Allocator.Error!Id {
    return @fromBackingInt(@intCast(try self.interner.insert(allocator, text)));
}

/// Resolve the text used by inspection and invariant diagnostics.
pub fn get(self: *const Self, id: Id) []const u8 {
    return self.interner.getText(@backingInt(id));
}

/// Number of assigned dense identities.
pub fn count(self: *const Self) u32 {
    return self.interner.count();
}

/// Release compiler-owned storage; mapped views borrow their image storage.
pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    self.interner.deinit(allocator);
}
