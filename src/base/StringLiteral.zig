//! Strings written inline in Roc code, e.g. `x = "abc"`.

const std = @import("std");
const collections = @import("../collections.zig");

const exitOnOom = collections.utils.exitOnOom;
const testing = std.testing;

/// The index of this string in a `StringLiteral.Store`.
pub const Idx = enum(u32) { _ };

/// An interner for string literals.
///
/// We avoid using the SmallStringInterner for string literals since
/// they are expected to be almost all unique and also larger, meaning
/// not worth the equality checking cost for depuplicating.
pub const Store = struct {
    /// An Idx points to the
    /// first byte of the string. The previous
    /// 4 bytes encode it's length.
    ///          Idx of "well"
    ///           |
    ///           |
    ///           |
    /// |    3   |w|e|l|l|   5    |h|e|l|l|o|
    /// |---u32--|--u8---|--u32---|--u8-----|
    /// conceptually these are the sizes above.
    ///
    /// Note:
    /// Later we could change from fixed u32-s to variable lengthed
    /// sizes, encoded in reverse where for example,
    /// the first 7 bit would signal the length, the last bit would signal that the length
    /// continues to the previous byte
    buffer: std.ArrayListUnmanaged(u8) = .{},

    /// Deinitialize a `StringLiteral.Store`'s memory.
    pub fn deinit(self: *Store, gpa: std.mem.Allocator) void {
        self.buffer.deinit(gpa);
    }

    /// Insert a new string into a `StringLiteral.Store`.
    ///
    /// Does not deduplicate, as string literals are expected to be large and mostly unique.
    pub fn insert(self: *Store, gpa: std.mem.Allocator, string: []const u8) Idx {
        const str_len: u32 = @truncate(string.len);
        const str_len_bytes = std.mem.asBytes(&str_len);
        self.buffer.appendSlice(gpa, str_len_bytes) catch |err| exitOnOom(err);
        const str_start_idx = self.buffer.items.len;
        self.buffer.appendSlice(gpa, string) catch |err| exitOnOom(err);

        return @enumFromInt(@as(u32, @intCast(str_start_idx)));
    }

    /// Get a string literal's text from this `Store`.
    pub fn get(self: *const Store, idx: Idx) []u8 {
        const idx_u32: u32 = @intCast(@intFromEnum(idx));
        const str_len = std.mem.bytesAsValue(u32, self.buffer.items[idx_u32 - 4 .. idx_u32]).*;
        return self.buffer.items[idx_u32 .. idx_u32 + str_len];
    }
};

test "insert" {
    const gpa = testing.allocator;

    var interner = Store{};
    defer interner.deinit(gpa);

    const str_1 = "abc".*;
    const str_2 = "defg".*;
    const idx_1 = interner.insert(gpa, &str_1);
    const idx_2 = interner.insert(gpa, &str_2);

    try testing.expectEqualStrings("abc", interner.get(idx_1));
    try testing.expectEqualStrings("defg", interner.get(idx_2));
}
