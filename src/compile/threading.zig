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

/// Native `std.Thread.Mutex` on supported targets, no-op stub on freestanding.
pub const Mutex = if (!is_freestanding) std.Thread.Mutex else struct {
    pub fn lock(_: *@This()) void {}
    pub fn unlock(_: *@This()) void {}
};

/// Native `std.Thread.Condition` on supported targets, no-op stub on freestanding.
pub const Condition = if (!is_freestanding) std.Thread.Condition else struct {
    pub fn wait(_: *@This(), _: anytype) void {}
    pub fn timedWait(_: *@This(), _: anytype, _: u64) error{Timeout}!void {
        return error.Timeout;
    }
    pub fn signal(_: *@This()) void {}
    pub fn broadcast(_: *@This()) void {}
};
