//! Cross-platform setjmp/longjmp for crash protection.
//!
//! On Linux, provides custom implementations using naked functions.
//! On macOS, uses the C library's _setjmp/_longjmp.
//! On Windows, provides a custom implementation using naked functions
//! since the C library setjmp is a compiler intrinsic that can't be linked.
//! On unsupported platforms (wasm, freestanding), provides stub types that error at runtime.

const std = @import("std");
const builtin = @import("builtin");

/// Whether we have a platform-specific implementation.
const has_windows_x64_impl = builtin.os.tag == .windows and builtin.cpu.arch == .x86_64;
const has_windows_aarch64_impl = builtin.os.tag == .windows and builtin.cpu.arch == .aarch64;
const has_linux_impl = builtin.os.tag == .linux and
    (builtin.cpu.arch == .x86_64 or builtin.cpu.arch == .aarch64);
const has_macos_impl = builtin.os.tag == .macos;

/// Whether this platform supports setjmp/longjmp.
pub const supported = has_windows_x64_impl or has_windows_aarch64_impl or has_linux_impl or has_macos_impl;

/// Buffer type for storing execution context (registers, stack pointer, etc.).
pub const JmpBuf = if (has_windows_x64_impl)
    @import("windows.zig").JmpBuf
else if (has_windows_aarch64_impl)
    @import("windows_aarch64.zig").JmpBuf
else if (has_linux_impl)
    @import("linux.zig").JmpBuf
else if (has_macos_impl)
    @import("unix.zig").JmpBuf
else
    [1]u8; // Stub for unsupported platforms

/// Saves the current execution context into the buffer. Returns 0 on initial call,
/// or the value passed to longjmp when returning via a jump.
pub const setjmp = if (has_windows_x64_impl)
    @import("windows.zig").setjmp
else if (has_windows_aarch64_impl)
    @import("windows_aarch64.zig").setjmp
else if (has_linux_impl)
    @import("linux.zig").setjmp
else if (has_macos_impl)
    @import("unix.zig").setjmp
else
    &stubSetjmp;

/// Restores execution context from the buffer, causing setjmp to return with the given value.
pub const longjmp = if (has_windows_x64_impl)
    @import("windows.zig").longjmp
else if (has_windows_aarch64_impl)
    @import("windows_aarch64.zig").longjmp
else if (has_linux_impl)
    @import("linux.zig").longjmp
else if (has_macos_impl)
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
    } else if (has_windows_aarch64_impl) {
        std.testing.refAllDecls(@import("windows_aarch64.zig"));
    } else if (has_linux_impl) {
        std.testing.refAllDecls(@import("linux.zig"));
    } else if (has_macos_impl) {
        std.testing.refAllDecls(@import("unix.zig"));
    }
}
