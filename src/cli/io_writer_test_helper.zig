//! Helper process for testing CLI stdout/stderr writer behavior when both
//! standard streams refer to one regular file.

const std = @import("std");
const Io = @import("CliCtx.zig").Io;

const stdout_payload = "stdout \u{2713} issue-10465\n" ** 256;
const stderr_payload = "stderr \u{2713} issue-10465\n" ** 256;

pub fn main(init: std.process.Init) !void {
    var io = Io.create(init.io);
    io.initWriters();

    try io.stdout().writeAll(stdout_payload);
    try io.stdout().flush();
    try io.stderr().writeAll(stderr_payload);
    try io.stderr().flush();
}
