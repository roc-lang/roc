//! A slice of a module's source code, used for highlighting code in diagnostics.

const std = @import("std");
const collections = @import("../collections.zig");

const Region = @This();

start: Position,
end: Position,

// Okay to use a non-multi list because both fields are the same size
/// A type-safe list of regions.
pub const List = collections.SafeList(@This());

/// An index into a list of regions.
pub const Idx = List.Idx;

/// A slice into a list of regions.
pub const Slice = List.Slice;

/// Create an empty `Region`.
pub fn zero() Region {
    return Region{
        .start = Position.zero(),
        .end = Position.zero(),
    };
}

/// Write the debug format of a region to a writer.
pub fn format(self: *const Region, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: std.io.AnyWriter) !void {
    if (fmt.len != 0) {
        std.fmt.invalidFmtError(fmt, self);
    }

    if (self.start.offset == 0 and self.end.offset == 0) {
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

    /// Create a `Position` at the start of a source file.
    pub fn zero() Position {
        return Position{ .offset = 0 };
    }
};
