//! Signal handling for the Roc compiler (stack overflow, segfault, division by zero).
//!
//! This module provides a thin wrapper around the generic signal handlers in
//! builtins.handlers, configured with compiler-specific error messages.
//!
//! On POSIX systems (Linux, macOS), we use sigaltstack to set up an alternate
//! signal stack and install handlers for SIGSEGV, SIGBUS, and SIGFPE.
//!
//! On Windows, we use SetUnhandledExceptionFilter to catch various exceptions.
//!
//! Freestanding targets (like wasm32) are not supported (no signal handling available).

const std = @import("std");
const builtin = @import("builtin");
const handlers = @import("builtins").handlers;
const posix = if (builtin.os.tag != .windows and builtin.os.tag != .freestanding) std.posix else undefined;

/// Error message to display on stack overflow
const STACK_OVERFLOW_MESSAGE = "\nThe Roc compiler overflowed its stack memory and had to exit.\n\n";

/// Callback for stack overflow in the compiler
fn handleStackOverflow() noreturn {
    if (comptime builtin.os.tag == .windows) {
        // Windows: use WriteFile for signal-safe output
        const DWORD = u32;
        const HANDLE = ?*anyopaque;
        const STD_ERROR_HANDLE: DWORD = @bitCast(@as(i32, -12));

        const kernel32 = struct {
            extern "kernel32" fn GetStdHandle(nStdHandle: DWORD) callconv(.winapi) HANDLE;
            extern "kernel32" fn WriteFile(hFile: HANDLE, lpBuffer: [*]const u8, nNumberOfBytesToWrite: DWORD, lpNumberOfBytesWritten: ?*DWORD, lpOverlapped: ?*anyopaque) callconv(.winapi) i32;
            extern "kernel32" fn TerminateProcess(hProcess: HANDLE, uExitCode: c_uint) callconv(.winapi) i32;
            extern "kernel32" fn GetCurrentProcess() callconv(.winapi) HANDLE;
        };

        const stderr_handle = kernel32.GetStdHandle(STD_ERROR_HANDLE);
        var bytes_written: DWORD = 0;
        _ = kernel32.WriteFile(stderr_handle, STACK_OVERFLOW_MESSAGE.ptr, STACK_OVERFLOW_MESSAGE.len, &bytes_written, null);
        // Use TerminateProcess instead of ExitProcess: after a stack overflow the
        // stack is blown and ExitProcess's DLL cleanup can trigger a secondary crash.
        _ = kernel32.TerminateProcess(kernel32.GetCurrentProcess(), 134);
        @trap();
    } else if (comptime builtin.os.tag != .freestanding) {
        // POSIX: use raw write for signal-safety
        _ = std.c.write(posix.STDERR_FILENO, STACK_OVERFLOW_MESSAGE.ptr, STACK_OVERFLOW_MESSAGE.len);
        std.process.exit(134);
    } else {
        // WASI fallback
        std.process.exit(134);
    }
}

/// Error message to display on arithmetic error (division by zero, etc.)
const ARITHMETIC_ERROR_MESSAGE = "\nThe Roc compiler divided by zero and had to exit.\n\n";

/// Callback for arithmetic errors (division by zero) in the compiler
fn handleArithmeticError() noreturn {
    if (comptime builtin.os.tag == .windows) {
        const DWORD = u32;
        const HANDLE = ?*anyopaque;
        const STD_ERROR_HANDLE: DWORD = @bitCast(@as(i32, -12));

        const kernel32 = struct {
            extern "kernel32" fn GetStdHandle(nStdHandle: DWORD) callconv(.winapi) HANDLE;
            extern "kernel32" fn WriteFile(hFile: HANDLE, lpBuffer: [*]const u8, nNumberOfBytesToWrite: DWORD, lpNumberOfBytesWritten: ?*DWORD, lpOverlapped: ?*anyopaque) callconv(.winapi) i32;
            extern "kernel32" fn ExitProcess(uExitCode: c_uint) callconv(.winapi) noreturn;
        };

        const stderr_handle = kernel32.GetStdHandle(STD_ERROR_HANDLE);
        var bytes_written: DWORD = 0;
        _ = kernel32.WriteFile(stderr_handle, ARITHMETIC_ERROR_MESSAGE.ptr, ARITHMETIC_ERROR_MESSAGE.len, &bytes_written, null);
        kernel32.ExitProcess(136);
    } else if (comptime builtin.os.tag != .freestanding) {
        _ = std.c.write(posix.STDERR_FILENO, ARITHMETIC_ERROR_MESSAGE.ptr, ARITHMETIC_ERROR_MESSAGE.len);
        std.process.exit(136); // 128 + 8 (SIGFPE)
    } else {
        std.process.exit(136);
    }
}

/// Callback for access violation in the compiler
fn handleAccessViolation(fault_addr: usize) noreturn {
    if (comptime builtin.os.tag == .windows) {
        const DWORD = u32;
        const HANDLE = ?*anyopaque;
        const STD_ERROR_HANDLE: DWORD = @bitCast(@as(i32, -12));

        const kernel32 = struct {
            extern "kernel32" fn GetStdHandle(nStdHandle: DWORD) callconv(.winapi) HANDLE;
            extern "kernel32" fn WriteFile(hFile: HANDLE, lpBuffer: [*]const u8, nNumberOfBytesToWrite: DWORD, lpNumberOfBytesWritten: ?*DWORD, lpOverlapped: ?*anyopaque) callconv(.winapi) i32;
            extern "kernel32" fn ExitProcess(uExitCode: c_uint) callconv(.winapi) noreturn;
        };

        var addr_buf: [18]u8 = undefined;
        const addr_str = handlers.formatHex(fault_addr, &addr_buf);

        const msg1 = "\nAccess violation in the Roc compiler.\nFault address: ";
        const msg2 = "\n\nPlease report this issue at: https://github.com/roc-lang/roc/issues\n\n";
        const stderr_handle = kernel32.GetStdHandle(STD_ERROR_HANDLE);
        var bytes_written: DWORD = 0;
        _ = kernel32.WriteFile(stderr_handle, msg1.ptr, msg1.len, &bytes_written, null);
        _ = kernel32.WriteFile(stderr_handle, addr_str.ptr, @intCast(addr_str.len), &bytes_written, null);
        _ = kernel32.WriteFile(stderr_handle, msg2.ptr, msg2.len, &bytes_written, null);
        kernel32.ExitProcess(139);
    } else {
        // POSIX (and WASI fallback): use raw write for signal-safety
        const generic_msg = "\nSegmentation fault (SIGSEGV) in the Roc compiler.\nFault address: ";
        _ = std.c.write(posix.STDERR_FILENO, generic_msg.ptr, generic_msg.len);

        // Write the fault address as hex
        var addr_buf: [18]u8 = undefined;
        const addr_str = handlers.formatHex(fault_addr, &addr_buf);
        _ = std.c.write(posix.STDERR_FILENO, addr_str.ptr, addr_str.len);
        const report_msg = "\n\nPlease report this issue at: https://github.com/roc-lang/roc/issues\n\n";
        _ = std.c.write(posix.STDERR_FILENO, report_msg.ptr, report_msg.len);
        std.process.exit(139);
    }
}

/// Install signal handlers for stack overflow, segfault, and division by zero.
/// This should be called early in main() before any significant work is done.
/// Returns true if the handlers were installed successfully, false otherwise.
pub fn install() bool {
    return handlers.install(handleStackOverflow, handleAccessViolation, handleArithmeticError);
}

/// Test function that intentionally causes a stack overflow.
/// This is used to verify the handler works correctly.
pub fn triggerStackOverflowForTest() noreturn {
    // Use a recursive function that can't be tail-call optimized
    const S = struct {
        fn recurse(n: usize) usize {
            // Prevent tail-call optimization by doing work after the recursive call
            var buf: [1024]u8 = undefined;
            buf[0] = @truncate(n);
            const result = if (n == 0) 0 else recurse(n + 1);
            // Use the buffer to prevent it from being optimized away
            return result + buf[0];
        }
    };

    // This will recurse until stack overflow
    const result = S.recurse(1);

    // This should never be reached
    std.debug.print("Unexpected result: {}\n", .{result});
    std.process.exit(1);
}

test "formatHex" {
    var buf: [18]u8 = undefined;

    const zero = handlers.formatHex(0, &buf);
    try std.testing.expectEqualStrings("0x0", zero);

    const small = handlers.formatHex(0xff, &buf);
    try std.testing.expectEqualStrings("0xff", small);

    const medium = handlers.formatHex(0xdeadbeef, &buf);
    try std.testing.expectEqualStrings("0xdeadbeef", medium);
}

/// Check if we're being run as a subprocess to trigger stack overflow.
/// This is called by tests to create a child process that will crash.
/// Returns true if we should trigger the overflow (and not return).
pub fn checkAndTriggerIfSubprocess() bool {
    // Check for the special environment variable that signals we should crash
    const env_val = std.c.getenv("ROC_TEST_TRIGGER_STACK_OVERFLOW") orelse return false;

    if (std.mem.eql(u8, std.mem.span(env_val), "1")) {
        // Install handler and trigger overflow
        _ = install();
        triggerStackOverflowForTest();
        // Never returns
    }
    return false;
}

test "stack overflow handler produces helpful error message" {
    try posix_overflow_test.run();
}

// Implementation lives in a separate file so its POSIX-only `std.c` / `posix.fd_t`
// references are never semantically analyzed on Windows / freestanding targets.
const posix_overflow_test = if (builtin.os.tag != .windows and builtin.os.tag != .freestanding)
    @import("stack_overflow_posix.zig")
else
    struct {
        pub fn run() !void {
            return error.SkipZigTest;
        }
    };
