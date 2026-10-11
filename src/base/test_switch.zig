//! Compiler settings that exist only so tests can compare a behavior against
//! its absence.

const builtin = @import("builtin");

/// A boolean setting that production builds fix at `production`. Test builds
/// store a real flag a test may change; every other build stores nothing, and
/// `enabled` is the comptime-known constant, so code it guards carries no
/// runtime check.
pub fn TestSwitch(comptime production: bool) type {
    return if (builtin.is_test) struct {
        value: bool = production,

        pub const on: @This() = .{ .value = true };
        pub const off: @This() = .{ .value = false };

        pub inline fn enabled(self: @This()) bool {
            return self.value;
        }
    } else struct {
        pub inline fn enabled(_: @This()) bool {
            return production;
        }
    };
}
