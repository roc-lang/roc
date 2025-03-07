//! A slice of a module's source code, used for highlighting code in diagnostics.

const std = @import("std");
const collections = @import("../collections.zig");

const Region = @This();

start: Position,
end: Position,

// Okay to use a non-multi list because both fields are the same size
/// A type-safe ArrayList of Ranges's
pub const List = collections.SafeList(@This());

/// Index of the Region
pub const Idx = List.Idx;

/// Slice of the Region's
pub const Slice = List.Slice;

/// create an empty Region
pub fn zero() Region {
    return Region{
        .start = Position.zero(),
        .end = Position.zero(),
    };
}

/// string formating for a Region
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

/// One side of a [Region].
pub const Position = struct {
    offset: u32,

    pub fn zero() Position {
        return Position{ .offset = 0 };
    }

    pub fn isZero(self: Position) bool {
        return self.offset == 0;
    }
};
