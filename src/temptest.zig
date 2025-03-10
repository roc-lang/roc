const std = @import("std");

//! This is a test

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Hello, World!\n", .{});
}
