const std = @import("std");
const collections = @import("../collections.zig");

const Region = @This();

start: Position,
end: Position,

// Okay to use a non-multi list because both fields are the same size
pub const List = collections.SafeList(@This());
pub const Idx = List.Idx;
pub const Slice = List.Slice;

pub fn zero() Region {
    return Region{
        .start = Position.zero(),
        .end = Position.zero(),
    };
}

pub fn format(self: *const Region, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: std.io.AnyWriter) !void {
    if (fmt.len != 0) {
        std.fmt.invalidFmtError(fmt, self);
    }

    if ((self.start.isZero()) and (self.end.isZero())) {
        // In tests, it's super common to set all Located values to 0.
        // Also in tests, we don't want to bother printing the locations
        // because it makes failed assertions much harder to read.
        try writer.print("…", .{});
    } else {
        try writer.print("@{}-{}", .{ self.start.offset, self.end.offset });
    }
}

pub const Position = struct {
    offset: u32,

    pub fn zero() Position {
        return Position{ .offset = 0 };
    }

    pub fn isZero(self: Position) bool {
        return self.offset == 0;
    }
};
