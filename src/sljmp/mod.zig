//! Cross-platform setjmp/longjmp for crash protection.
//!
//! On Unix, uses the C library's _setjmp/_longjmp.
//! On Windows, provides a custom implementation using naked functions
//! since the C library setjmp is a compiler intrinsic that can't be linked.
//! On unsupported platforms (wasm, freestanding), provides stub types that error at runtime.

const std = @import("std");
const builtin = @import("builtin");

/// Whether this platform supports setjmp/longjmp.
pub const supported = switch (builtin.os.tag) {
    .windows, .macos, .linux => true,
    else => false,
};

/// Buffer type for storing execution context (registers, stack pointer, etc.).
pub const JmpBuf = if (builtin.os.tag == .windows)
    @import("windows.zig").JmpBuf
else if (builtin.os.tag == .macos or builtin.os.tag == .linux)
    @import("unix.zig").JmpBuf
else
    [1]u8; // Stub for unsupported platforms

/// Saves the current execution context into the buffer. Returns 0 on initial call,
/// or the value passed to longjmp when returning via a jump.
pub const setjmp = if (builtin.os.tag == .windows)
    @import("windows.zig").setjmp
else if (builtin.os.tag == .macos or builtin.os.tag == .linux)
    @import("unix.zig").setjmp
else
    &stubSetjmp;

/// Restores execution context from the buffer, causing setjmp to return with the given value.
pub const longjmp = if (builtin.os.tag == .windows)
    @import("windows.zig").longjmp
else if (builtin.os.tag == .macos or builtin.os.tag == .linux)
    @import("unix.zig").longjmp
else
    &stubLongjmp;

fn stubSetjmp(_: *JmpBuf) callconv(.c) c_int {
    @panic("setjmp not supported on this platform");
}

fn stubLongjmp(_: *JmpBuf, _: c_int) callconv(.c) noreturn {
    @panic("longjmp not supported on this platform");
}

test {
    std.testing.refAllDecls(@This());
    if (builtin.os.tag == .windows) {
        std.testing.refAllDecls(@import("windows.zig"));
    } else if (builtin.os.tag == .macos or builtin.os.tag == .linux) {
        std.testing.refAllDecls(@import("unix.zig"));
    }
}
