//! A slice of a module's source code, used for highlighting code in diagnostics.

const std = @import("std");

const Self = @This();

start: u32,
len: u32,

/// Write the debug format of a span to a writer.
pub fn format(
    self: *const Self,
    writer: *std.Io.Writer,
) std.Io.Writer.Error!void {
    try writer.print("@{}-{}", .{ self.start, self.start + self.len });
}
