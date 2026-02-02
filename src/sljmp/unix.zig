//! Unix implementation of setjmp/longjmp using C library functions.

const builtin = @import("builtin");

/// Platform-specific jmp_buf size for Unix systems.
pub const JmpBuf = switch (builtin.os.tag) {
    .macos => [48]c_int,
    .linux => switch (builtin.cpu.arch) {
        .aarch64 => [64]c_long,
        .x86_64 => [25]c_long,
        else => @compileError("Unsupported architecture for jmp_buf"),
    },
    else => @compileError("Unsupported OS for jmp_buf"),
};

/// Link to C library's _setjmp (sigsetjmp variant that doesn't save signal mask).
pub const setjmp = @extern(*const fn (*JmpBuf) callconv(.c) c_int, .{ .name = "_setjmp" });

/// Link to C library's _longjmp.
pub const longjmp = @extern(*const fn (*JmpBuf, c_int) callconv(.c) noreturn, .{ .name = "_longjmp" });
