//! Cross-platform setjmp/longjmp for crash protection.
//!
//! On Unix, uses the C library's _setjmp/_longjmp.
//! On Windows x86_64, provides a custom implementation using naked functions
//! since the C library setjmp is a compiler intrinsic that can't be linked.
//! On unsupported platforms (wasm, freestanding, Windows ARM64), provides stub types that error at runtime.

const std = @import("std");
const builtin = @import("builtin");

/// Whether we have a platform-specific implementation.
const has_windows_x64_impl = builtin.os.tag == .windows and builtin.cpu.arch == .x86_64;
const has_unix_impl = builtin.os.tag == .macos or builtin.os.tag == .linux;

/// Whether this platform supports setjmp/longjmp.
pub const supported = has_windows_x64_impl or has_unix_impl;

/// Buffer type for storing execution context (registers, stack pointer, etc.).
pub const JmpBuf = if (has_windows_x64_impl)
    @import("windows.zig").JmpBuf
else if (has_unix_impl)
    @import("unix.zig").JmpBuf
else
    [1]u8; // Stub for unsupported platforms

/// Saves the current execution context into the buffer. Returns 0 on initial call,
/// or the value passed to longjmp when returning via a jump.
pub const setjmp = if (has_windows_x64_impl)
    @import("windows.zig").setjmp
else if (has_unix_impl)
    @import("unix.zig").setjmp
else
    &stubSetjmp;

/// Restores execution context from the buffer, causing setjmp to return with the given value.
pub const longjmp = if (has_windows_x64_impl)
    @import("windows.zig").longjmp
else if (has_unix_impl)
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
    if (has_windows_x64_impl) {
        std.testing.refAllDecls(@import("windows.zig"));
    } else if (has_unix_impl) {
        std.testing.refAllDecls(@import("unix.zig"));
    }
}
