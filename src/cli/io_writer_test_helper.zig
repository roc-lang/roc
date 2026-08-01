//! Helper process for testing CLI stdout/stderr writer behavior when both
//! standard streams refer to one regular file.

const std = @import("std");
const builtin = @import("builtin");
const Io = @import("CliCtx.zig").Io;

const stdout_payload = "stdout \u{2713} issue-10465\n" ** 256;
const stderr_payload = "stderr \u{2713} issue-10465\n" ** 256;

const HelperError = std.process.Args.ToSliceError || std.Io.File.OpenError || std.Io.Writer.Error || error{
    InvalidArguments,
    RedirectFailed,
};

/// Write both payloads through the CLI's standard-stream writers.
pub fn main(init: std.process.Init) HelperError!void {
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    if (args.len != 2) return error.InvalidArguments;

    const combined_file = try std.Io.Dir.cwd().createFile(init.io, args[1], .{});
    defer combined_file.close(init.io);
    try mergeStandardStreams(combined_file);

    var io = Io.create(init.io);
    io.initWriters();

    try io.stdout().writeAll(stdout_payload);
    try io.stdout().flush();
    try io.stderr().writeAll(stderr_payload);
    try io.stderr().flush();
}

fn mergeStandardStreams(combined_file: std.Io.File) error{RedirectFailed}!void {
    if (builtin.os.tag == .windows) {
        const process_parameters = std.os.windows.peb().ProcessParameters;
        process_parameters.hStdOutput = combined_file.handle;
        process_parameters.hStdError = combined_file.handle;
    } else {
        if (std.c.dup2(combined_file.handle, std.posix.STDOUT_FILENO) < 0) return error.RedirectFailed;
        if (std.c.dup2(combined_file.handle, std.posix.STDERR_FILENO) < 0) return error.RedirectFailed;
    }
}
