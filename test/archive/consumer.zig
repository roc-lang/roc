//! Consumer for the static-archive test platform. Links against the archive
//! that `roc build` produced (host code + app + builtins packaged together)
//! and calls its outward-facing C API, exactly like an external build system
//! embedding a Roc platform would.
//!
//! Run with: zig build run-test-archive

const std = @import("std");

extern fn roc_run_app(n: i64) callconv(.c) i64;

pub fn main(init: std.process.Init) anyerror!void {
    _ = init;

    // The app sums an allocated 3-element list of n, doubles it via the hosted
    // Host.double! function, and adds one: 6n + 1.
    const answer = roc_run_app(20);
    if (answer != 121) {
        std.debug.print("FAILED: roc_run_app(20) returned {d}, expected 121\n", .{answer});
        return error.WrongAnswer;
    }

    std.debug.print("SUCCESS: roc_run_app(20) == 121\n", .{});
}
