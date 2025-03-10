//! This is a test

const std = @import("std");

/// pub comment
pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Hello, World!\n", .{});
}
