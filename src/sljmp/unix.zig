//! macOS implementation of setjmp/longjmp using C library functions.

const builtin = @import("builtin");

/// Platform-specific jmp_buf size.
pub const JmpBuf = switch (builtin.os.tag) {
    .macos => [48]c_int,
    else => @compileError("Unsupported OS for jmp_buf"),
};

/// Whether to use underscore-prefixed names.
/// macOS _setjmp/_longjmp don't save signal masks.
const use_underscore = true;

/// Link to C library's setjmp variant that doesn't save signal mask.
pub const setjmp = @extern(*const fn (*JmpBuf) callconv(.c) c_int, .{
    .name = if (use_underscore) "_setjmp" else "setjmp",
});

/// Link to C library's longjmp variant.
pub const longjmp = @extern(*const fn (*JmpBuf, c_int) callconv(.c) noreturn, .{
    .name = if (use_underscore) "_longjmp" else "longjmp",
});
