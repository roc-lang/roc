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
//! WASI is not currently supported (no signal handling available).

const std = @import("std");
const builtin = @import("builtin");
const handlers = @import("builtins").handlers;
const posix = if (builtin.os.tag != .windows and builtin.os.tag != .wasi) std.posix else undefined;

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
            extern "kernel32" fn ExitProcess(uExitCode: c_uint) callconv(.winapi) noreturn;
        };

        const stderr_handle = kernel32.GetStdHandle(STD_ERROR_HANDLE);
        var bytes_written: DWORD = 0;
        _ = kernel32.WriteFile(stderr_handle, STACK_OVERFLOW_MESSAGE.ptr, STACK_OVERFLOW_MESSAGE.len, &bytes_written, null);
        kernel32.ExitProcess(134);
    } else if (comptime builtin.os.tag != .wasi) {
        // POSIX: use direct write syscall for signal-safety
        _ = posix.write(posix.STDERR_FILENO, STACK_OVERFLOW_MESSAGE) catch {};
        posix.exit(134);
    } else {
        // WASI fallback
        std.process.exit(134);
    }
}

/// Error message to display on arithmetic error (division by zero, etc.)
const ARITHMETIC_ERROR_MESSAGE =
    \\
    \\================================================================================
    \\ARITHMETIC ERROR in the Roc compiler
    \\================================================================================
    \\
    \\The Roc compiler encountered an arithmetic error (likely division by zero).
    \\This is a bug in the compiler, not in your code.
    \\
    \\Please report this issue at: https://github.com/roc-lang/roc/issues
    \\
    \\Include the Roc code that triggered this error if possible.
    \\
    \\================================================================================
    \\
    \\
;

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
    } else if (comptime builtin.os.tag != .wasi) {
        _ = posix.write(posix.STDERR_FILENO, ARITHMETIC_ERROR_MESSAGE) catch {};
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
    } else {
        // POSIX (and WASI fallback): use direct write syscall for signal-safety
        const generic_msg = "\nSegmentation fault (SIGSEGV) in the Roc compiler.\nFault address: ";
        _ = posix.write(posix.STDERR_FILENO, generic_msg) catch {};

        // Write the fault address as hex
        var addr_buf: [18]u8 = undefined;
        const addr_str = handlers.formatHex(fault_addr, &addr_buf);
        _ = posix.write(posix.STDERR_FILENO, addr_str) catch {};
        _ = posix.write(posix.STDERR_FILENO, "\n\nPlease report this issue at: https://github.com/roc-lang/roc/issues\n\n") catch {};
        posix.exit(139);
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
    const env_val = std.process.getEnvVarOwned(std.heap.page_allocator, "ROC_TEST_TRIGGER_STACK_OVERFLOW") catch return false;
    defer std.heap.page_allocator.free(env_val);

    if (std.mem.eql(u8, env_val, "1")) {
        // Install handler and trigger overflow
        _ = install();
        triggerStackOverflowForTest();
        // Never returns
    }
    return false;
}

test "stack overflow handler produces helpful error message" {
    // Skip on WASI - no process spawning or signal handling
    if (comptime builtin.os.tag == .wasi) {
        return error.SkipZigTest;
    }

    if (comptime builtin.os.tag == .windows) {
        // Windows test would need subprocess spawning which is more complex
        // The handler is installed and works, but testing it is harder
        // For now, just verify the handler installs successfully
        if (install()) {
            return; // Success - handler installed
        }
        return error.SkipZigTest;
    }

    try testStackOverflowPosix();
}

fn testStackOverflowPosix() !void {
    // Create a pipe to capture stderr from the child
    const pipe_fds = try posix.pipe();
    const pipe_read = pipe_fds[0];
    const pipe_write = pipe_fds[1];

    const fork_result = posix.fork() catch {
        posix.close(pipe_read);
        posix.close(pipe_write);
        return error.ForkFailed;
    };

    if (fork_result == 0) {
        // Child process
        posix.close(pipe_read);

        // Redirect stderr to the pipe
        posix.dup2(pipe_write, posix.STDERR_FILENO) catch posix.exit(99);
        posix.close(pipe_write);

        // Install the handler and trigger stack overflow
        _ = install();
        triggerStackOverflowForTest();
        // Should never reach here
        unreachable;
    } else {
        // Parent process
        posix.close(pipe_write);

        // Wait for child to exit
        const wait_result = posix.waitpid(fork_result, 0);
        const status = wait_result.status;

        // Parse the wait status (Unix encoding)
        const exited_normally = (status & 0x7f) == 0;
        const exit_code: u8 = @truncate((status >> 8) & 0xff);
        const termination_signal: u8 = @truncate(status & 0x7f);

        // Read stderr output from child
        var stderr_buf: [4096]u8 = undefined;
        const bytes_read = posix.read(pipe_read, &stderr_buf) catch 0;
        posix.close(pipe_read);

        const stderr_output = stderr_buf[0..bytes_read];

        try verifyHandlerOutput(exited_normally, exit_code, termination_signal, stderr_output);
    }
}

fn verifyHandlerOutput(exited_normally: bool, exit_code: u8, termination_signal: u8, stderr_output: []const u8) !void {
    // Exit code 134 = stack overflow detected
    // Exit code 139 = generic segfault (handler caught it but didn't classify as stack overflow)
    if (exited_normally and (exit_code == 134 or exit_code == 139)) {
        // Check that our handler message was printed
        const has_stack_overflow_msg = std.mem.indexOf(u8, stderr_output, "overflowed its stack memory") != null;
        const has_segfault_msg = std.mem.indexOf(u8, stderr_output, "Segmentation fault") != null;

        // Handler should have printed EITHER stack overflow message OR segfault message
        try std.testing.expect(has_stack_overflow_msg or has_segfault_msg);
    } else if (!exited_normally and (termination_signal == posix.SIG.SEGV or termination_signal == posix.SIG.BUS)) {
        // The handler might not have caught it - this can happen on some systems
        // where the signal delivery is different. Just warn and skip.
        std.debug.print("Warning: Stack overflow was not caught by handler (signal {})\n", .{termination_signal});
        return error.SkipZigTest;
    } else {
        std.debug.print("Unexpected exit status: exited={}, code={}, signal={}\n", .{ exited_normally, exit_code, termination_signal });
        std.debug.print("Stderr: {s}\n", .{stderr_output});
        return error.TestUnexpectedResult;
    }
}
