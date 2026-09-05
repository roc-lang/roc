//! Helper process for testing CLI stdout/stderr writer behavior when both
//! standard streams refer to one regular file.

const std = @import("std");
const builtin = @import("builtin");
const Io = @import("CliCtx.zig").Io;

fn repeatBytes(comptime bytes: []const u8, comptime count: usize) [bytes.len * count]u8 {
    var result: [bytes.len * count]u8 = undefined;
    for (0..count) |i| @memcpy(result[i * bytes.len ..][0..bytes.len], bytes);
    return result;
}

const stdout_line = "stdout \u{2713} issue-10465\n";
const stderr_line = "stderr \u{2713} issue-10465\n";
const stdout_payload = repeatBytes(stdout_line, 256);
const stderr_payload = repeatBytes(stderr_line, 256);

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
