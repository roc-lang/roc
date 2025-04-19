//! A slice of a module's source code, used for highlighting code in diagnostics.

const std = @import("std");
const collections = @import("../collections.zig");

const Self = @This();

start: Position,
end: Position,

// Okay to use a non-multi list because both fields are the same size
/// A type-safe list of regions.
pub const List = collections.SafeList(@This());

/// An index into a list of regions.
pub const Idx = List.Idx;

/// A slice into a list of regions.
pub const Range = List.Range;

/// Create an empty `Region`.
pub fn zero() Self {
    return Self{
        .start = Position.zero(),
        .end = Position.zero(),
    };
}

/// Returns true if the region is empty i.e. all values are zero.
pub fn isEmpty(self: Self) bool {
    return self.start.offset == 0 and self.end.offset == 0;
}

/// Write the debug format of a region to a writer.
pub fn format(self: *const Self, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: std.io.AnyWriter) !void {
    if (fmt.len != 0) {
        std.fmt.invalidFmtError(fmt, self);
    }

    try writer.print("@{}-{}", .{ self.start.offset, self.end.offset });
}

/// One side of a [Region].
pub const Position = struct {
    offset: u32,

    /// Create a `Position` at the start of a source file.
    pub fn zero() Position {
        return Position{ .offset = 0 };
    }
};
