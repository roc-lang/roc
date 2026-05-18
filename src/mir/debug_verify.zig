//! Debug-only MIR invariant verification helpers.

const std = @import("std");
const builtin = @import("builtin");

/// Public declaration.
pub inline fn enabled() bool {
    return builtin.mode == .Debug;
}

/// Public `assert` function.
pub fn assert(condition: bool, comptime message: []const u8) void {
    if (builtin.mode != .Debug) return;
    if (!condition) std.debug.panic(message, .{});
}

/// Public `assertFmt` function.
pub fn assertFmt(condition: bool, comptime fmt: []const u8, args: anytype) void {
    if (builtin.mode != .Debug) return;
    if (!condition) std.debug.panic(fmt, args);
}

/// Public `invariant` function.
pub fn invariant(condition: bool, comptime message: []const u8) void {
    if (condition) return;
    if (builtin.mode == .Debug) std.debug.panic(message, .{});
    unreachable;
}

/// Public `invariantFmt` function.
pub fn invariantFmt(condition: bool, comptime fmt: []const u8, args: anytype) void {
    if (condition) return;
    if (builtin.mode == .Debug) std.debug.panic(fmt, args);
    unreachable;
}

test "debug verification helpers compile out of release callers" {
    assert(true, "unreachable");
    invariant(true, "unreachable");
}
