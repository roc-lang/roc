const std = @import("std");
const Io = std.Io;

pub fn main(init: std.process.Init) !void {
    try Io.File.stdout().writeStreamingAll(init.io, "hello world!\n");
}
