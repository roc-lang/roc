//! Threading compatibility layer for freestanding (WASM) targets.
//!
//! On native targets, re-exports std.Thread types directly.
//! On freestanding, provides no-op stubs so that code using
//! Mutex/Condition compiles but operates single-threaded.

const std = @import("std");
const builtin = @import("builtin");

/// Whether the target OS is freestanding (e.g. WASM). Used throughout
/// the compile module to gate threading and native OS functionality.
pub const is_freestanding = builtin.target.os.tag == .freestanding;

/// Native `std.Thread` on supported targets, empty struct on freestanding.
pub const Thread = if (!is_freestanding) std.Thread else struct {};

/// Native `std.Io.Mutex` on supported targets, no-op stub on freestanding.
pub const Mutex = if (!is_freestanding) std.Io.Mutex else struct {
    pub fn lock(_: *@This(), _: anytype) error{Canceled}!void {}
    pub fn lockUncancelable(_: *@This(), _: anytype) void {}
    pub fn unlock(_: *@This(), _: anytype) void {}
};

/// Native `std.Io.Condition` on supported targets, no-op stub on freestanding.
pub const Condition = if (!is_freestanding) std.Io.Condition else struct {
    pub fn wait(_: *@This(), _: anytype, _: anytype) error{Canceled}!void {}
    pub fn waitUncancelable(_: *@This(), _: anytype, _: anytype) void {}
    pub fn signal(_: *@This(), _: anytype) void {}
    pub fn broadcast(_: *@This(), _: anytype) void {}
};

/// Returns the number of available CPU cores, falling back to 1 on error or freestanding targets.
pub fn getCpuCount() usize {
    if (comptime is_freestanding) return 1;
    return std.Thread.getCpuCount() catch 1;
}
