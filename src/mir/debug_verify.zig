//! Debug-only MIR invariant verification helpers.

const std = @import("std");
const builtin = @import("builtin");

/// Public declaration.
pub inline fn enabled() bool {
    return builtin.mode == .Debug;
}

/// Public `assert` function.
pub inline fn assert(condition: bool, comptime message: []const u8) void {
    if (comptime builtin.mode != .Debug) return;
    if (!condition) std.debug.panic(message, .{});
}

/// Public `assertFmt` function.
pub inline fn assertFmt(condition: bool, comptime fmt: []const u8, args: anytype) void {
    if (comptime builtin.mode != .Debug) return;
    if (!condition) std.debug.panic(fmt, args);
}

/// Public `invariant` function.
pub inline fn invariant(condition: bool, comptime message: []const u8) void {
    if (comptime builtin.mode != .Debug) return;
    if (!condition) std.debug.panic(message, .{});
}

/// Public `invariantFmt` function.
pub inline fn invariantFmt(condition: bool, comptime fmt: []const u8, args: anytype) void {
    if (comptime builtin.mode != .Debug) return;
    if (!condition) std.debug.panic(fmt, args);
}

test "debug verification helpers compile out of release callers" {
    assert(true, "unreachable");
    invariant(true, "unreachable");
}
