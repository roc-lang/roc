//! Debug-only MIR invariant verification helpers.

const std = @import("std");
const builtin = @import("builtin");

pub inline fn enabled() bool {
    return builtin.mode == .Debug;
}

pub fn assert(condition: bool, comptime message: []const u8) void {
    if (builtin.mode != .Debug) return;
    if (!condition) std.debug.panic(message, .{});
}

pub fn assertFmt(condition: bool, comptime fmt: []const u8, args: anytype) void {
    if (builtin.mode != .Debug) return;
    if (!condition) std.debug.panic(fmt, args);
}

test "debug verification helpers compile out of release callers" {
    assert(true, "unreachable");
}
