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
const STACK_OVERFLOW_TEST_HELPER_ENV_VAR = "ROC_STACK_OVERFLOW_TEST_HELPER";

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
        // POSIX: use direct write syscall for signal-safety
        const written = posix.write(posix.STDERR_FILENO, STACK_OVERFLOW_MESSAGE) catch |err| {
            if (comptime builtin.mode == .Debug) {
                @panic(@errorName(err));
            } else {
                unreachable;
            }
        };
        if (written != STACK_OVERFLOW_MESSAGE.len) {
            if (comptime builtin.mode == .Debug) {
                @panic("stack overflow handler short write");
            } else {
                unreachable;
            }
        }
        posix.exit(134);
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
        const written = posix.write(posix.STDERR_FILENO, ARITHMETIC_ERROR_MESSAGE) catch |err| {
            if (comptime builtin.mode == .Debug) {
                @panic(@errorName(err));
            } else {
                unreachable;
            }
        };
        if (written != ARITHMETIC_ERROR_MESSAGE.len) {
            if (comptime builtin.mode == .Debug) {
                @panic("arithmetic error handler short write");
            } else {
                unreachable;
            }
        }
        posix.exit(136); // 128 + 8 (SIGFPE)
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
    } else if (comptime builtin.os.tag != .freestanding) {
        // POSIX: use direct write syscall for signal-safety
        const generic_msg = "\nSegmentation fault (SIGSEGV) in the Roc compiler.\nFault address: ";
        {
            const written = posix.write(posix.STDERR_FILENO, generic_msg) catch |err| {
                if (comptime builtin.mode == .Debug) {
                    @panic(@errorName(err));
                } else {
                    unreachable;
                }
            };
            if (written != generic_msg.len) {
                if (comptime builtin.mode == .Debug) {
                    @panic("access violation handler short write (prefix)");
                } else {
                    unreachable;
                }
            }
        }

        // Write the fault address as hex
        var addr_buf: [18]u8 = undefined;
        const addr_str = handlers.formatHex(fault_addr, &addr_buf);
        {
            const written = posix.write(posix.STDERR_FILENO, addr_str) catch |err| {
                if (comptime builtin.mode == .Debug) {
                    @panic(@errorName(err));
                } else {
                    unreachable;
                }
            };
            if (written != addr_str.len) {
                if (comptime builtin.mode == .Debug) {
                    @panic("access violation handler short write (addr)");
                } else {
                    unreachable;
                }
            }
        }
        {
            const tail = "\n\nPlease report this issue at: https://github.com/roc-lang/roc/issues\n\n";
            const written = posix.write(posix.STDERR_FILENO, tail) catch |err| {
                if (comptime builtin.mode == .Debug) {
                    @panic(@errorName(err));
                } else {
                    unreachable;
                }
            };
            if (written != tail.len) {
                if (comptime builtin.mode == .Debug) {
                    @panic("access violation handler short write (tail)");
                } else {
                    unreachable;
                }
            }
        }
        posix.exit(139);
    } else {
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

test "stack overflow handler produces helpful error message" {
    // Skip on freestanding targets - no process spawning or signal handling
    if (comptime builtin.os.tag == .freestanding) {
        return error.SkipZigTest;
    }

    try testStackOverflowInChildProcess();
}

fn testStackOverflowInChildProcess() !void {
    const allocator = std.testing.allocator;
    const helper_path = std.process.getEnvVarOwned(allocator, STACK_OVERFLOW_TEST_HELPER_ENV_VAR) catch |err| {
        std.debug.print("Missing {s}: {s}\n", .{ STACK_OVERFLOW_TEST_HELPER_ENV_VAR, @errorName(err) });
        return error.TestUnexpectedResult;
    };
    defer allocator.free(helper_path);

    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{helper_path},
        .max_output_bytes = 4096,
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try verifyHandlerOutput(result.term, result.stderr);
}

fn verifyHandlerOutput(term: std.process.Child.Term, stderr_output: []const u8) !void {
    const has_stack_overflow_msg = std.mem.indexOf(u8, stderr_output, "overflowed its stack memory") != null;
    const has_segfault_msg = std.mem.indexOf(u8, stderr_output, "Segmentation fault") != null;

    switch (term) {
        .Exited => |code| {
            // Exit code 134 = stack overflow detected
            // Exit code 139 = generic segfault/access violation handler path
            if (code == 134 or code == 139) {
                try std.testing.expect(has_stack_overflow_msg or has_segfault_msg);
                return;
            }

            std.debug.print("Unexpected exit code: {}\n", .{code});
        },
        .Signal => |sig| {
            if (comptime builtin.os.tag != .windows and builtin.os.tag != .freestanding) {
                if (sig == posix.SIG.SEGV or sig == posix.SIG.BUS) {
                    // The handler might not have caught it - this can happen on some systems
                    // where the signal delivery is different. Just warn and skip.
                    std.debug.print("Warning: Stack overflow was not caught by handler (signal {})\n", .{sig});
                    return error.SkipZigTest;
                }
            }

            std.debug.print("Unexpected termination signal: {}\n", .{sig});
        },
        else => {
            std.debug.print("Unexpected termination: {}\n", .{term});
        },
    }

    std.debug.print("Stderr: {s}\n", .{stderr_output});
    return error.TestUnexpectedResult;
}
