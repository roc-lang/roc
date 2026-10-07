//! The one way the compiler handles a violated compiler invariant
//! (design.md).

const std = @import("std");

/// A violated compiler invariant: an assumption that can fail only if the
/// compiler has a bug. Builds with runtime safety panic with this message;
/// optimized builds treat the path as unreachable, so release builds carry no
/// runtime check for it.
pub inline fn invariant(comptime fmt: []const u8, args: anytype) noreturn {
    if (std.debug.runtime_safety) std.debug.panic(fmt, args);
    unreachable;
}
