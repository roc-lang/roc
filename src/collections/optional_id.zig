//! Four-byte optional typed IDs shared by persisted rows and transient columns.
//! Shared zero-based domains bias the stored value; a private domain may
//! instead reserve slot zero and store its IDs unchanged.

const std = @import("std");

/// Zero means absent; other words store the zero-based ID plus one.
pub fn OptionalId(comptime Id: type) type {
    return IndexValue(Id, false);
}

/// Zero means absent in a domain whose producer issues IDs starting at one.
pub fn ZeroSentinelId(comptime Id: type) type {
    return IndexValue(Id, true);
}

fn IndexValue(comptime Id: type, comptime reserved_zero: bool) type {
    comptime {
        const info = @typeInfo(Id);
        if (info != .@"enum" or info.@"enum".tag_type != u32)
            @compileError("optional IDs require a typed u32 index");
    }
    return enum(u32) {
        none = 0,
        _,

        const Self = @This();

        /// Wrap an ID within the owning store's bounded index domain.
        pub inline fn some(id: Id) Self {
            const raw = @backingInt(id);
            std.debug.assert(raw < std.math.maxInt(u32));
            if (reserved_zero) std.debug.assert(raw != 0);
            return @fromBackingInt(if (reserved_zero) raw else raw + 1);
        }

        /// Read the original ID, or null for absence.
        pub inline fn get(self: Self) ?Id {
            if (self == .none) return null;
            const raw = @backingInt(self);
            return @fromBackingInt(if (reserved_zero) raw else raw - 1);
        }
    };
}

test "optional IDs use one word and preserve both numbering policies" {
    const Id = enum(u32) { _ };
    const Biased = OptionalId(Id);
    const Direct = ZeroSentinelId(Id);
    try std.testing.expectEqual(@as(usize, 4), @sizeOf(Biased));
    try std.testing.expectEqual(@as(usize, 4), @sizeOf(Direct));
    try std.testing.expect(Biased.none.get() == null);
    try std.testing.expect(Direct.none.get() == null);
    const first: Id = @fromBackingInt(0);
    const one: Id = @fromBackingInt(1);
    const last: Id = @fromBackingInt(std.math.maxInt(u32) - 1);
    try std.testing.expectEqual(first, Biased.some(first).get().?);
    try std.testing.expectEqual(last, Biased.some(last).get().?);
    try std.testing.expectEqual(one, Direct.some(one).get().?);
    try std.testing.expectEqual(last, Direct.some(last).get().?);
    try std.testing.expectEqual(@as(u32, 1), @backingInt(Biased.some(first)));
    try std.testing.expectEqual(@as(u32, 1), @backingInt(Direct.some(one)));
}
