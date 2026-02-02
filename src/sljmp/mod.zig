//! Cross-platform setjmp/longjmp for crash protection.
//!
//! On Unix, uses the C library's _setjmp/_longjmp.
//! On Windows, provides a custom implementation using naked functions
//! since the C library setjmp is a compiler intrinsic that can't be linked.

const std = @import("std");
const builtin = @import("builtin");

pub const JmpBuf = if (builtin.os.tag == .windows)
    @import("windows.zig").JmpBuf
else
    @import("unix.zig").JmpBuf;

pub const setjmp = if (builtin.os.tag == .windows)
    @import("windows.zig").setjmp
else
    @import("unix.zig").setjmp;

pub const longjmp = if (builtin.os.tag == .windows)
    @import("windows.zig").longjmp
else
    @import("unix.zig").longjmp;

test {
    std.testing.refAllDecls(@This());
    if (builtin.os.tag == .windows) {
        std.testing.refAllDecls(@import("windows.zig"));
    } else {
        std.testing.refAllDecls(@import("unix.zig"));
    }
}
