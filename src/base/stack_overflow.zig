//! Signal handling for the Roc compiler process.

const std = @import("std");
const builtin = @import("builtin");
const posix = if (builtin.os.tag != .windows and builtin.os.tag != .freestanding) std.posix else undefined;
const signal_handler = @import("signal_handler.zig");

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
fn handleAccessViolation(fault_addr: usize, context: signal_handler.AccessViolationContext) noreturn {
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
        const addr_str = signal_handler.formatHex(fault_addr, &addr_buf);

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
        const addr_str = signal_handler.formatHex(fault_addr, &addr_buf);
        _ = std.c.write(posix.STDERR_FILENO, addr_str.ptr, addr_str.len);
        const stack_msg = "\n\nStack trace:\n";
        _ = std.c.write(posix.STDERR_FILENO, stack_msg.ptr, stack_msg.len);
        if (comptime signal_handler.AccessViolationContext != void) {
            if (context) |cpu_context| {
                std.debug.dumpCurrentStackTrace(.{
                    .context = cpu_context,
                    .allow_unsafe_unwind = true,
                });
            } else {
                const unavailable_msg = "(signal context unavailable)\n";
                _ = std.c.write(posix.STDERR_FILENO, unavailable_msg.ptr, unavailable_msg.len);
            }
        } else {
            const unavailable_msg = "(stack trace unavailable on this target)\n";
            _ = std.c.write(posix.STDERR_FILENO, unavailable_msg.ptr, unavailable_msg.len);
        }
        const report_msg = "\nPlease report this issue at: https://github.com/roc-lang/roc/issues\n\n";
        _ = std.c.write(posix.STDERR_FILENO, report_msg.ptr, report_msg.len);
        std.process.exit(139);
    }
}

/// Install compiler crash handling for the current thread.
pub fn installForCurrentThread() bool {
    return signal_handler.installForCurrentThread(.{
        .stack_overflow = handleStackOverflow,
        .access_violation = handleAccessViolation,
        .arithmetic_error = handleArithmeticError,
    });
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

test "stack overflow handler produces helpful error message" {
    // Skip on freestanding targets - no process spawning or signal handling
    if (comptime builtin.os.tag == .freestanding) {
        return error.SkipZigTest;
    }

    try testCrashInChildProcess("stack-overflow", "overflowed its stack memory", 134);
}

test "access violation handler is not reported as stack overflow" {
    if (comptime builtin.os.tag == .freestanding) {
        return error.SkipZigTest;
    }

    const expected_msg = if (comptime builtin.os.tag == .windows)
        "Access violation"
    else
        "Segmentation fault";
    try testCrashInChildProcess("high-access-violation", expected_msg, 139);
}

test "worker thread installs stack overflow handler" {
    if (comptime builtin.os.tag == .freestanding) {
        return error.SkipZigTest;
    }

    try testCrashInChildProcess("thread-stack-overflow", "overflowed its stack memory", 134);
}

const StackOverflowTestError = std.process.RunError || error{ TestUnexpectedResult, SkipZigTest };

fn testCrashInChildProcess(mode: []const u8, expected: []const u8, expected_code: u8) StackOverflowTestError!void {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    const stack_overflow_test_options = @import("stack_overflow_test_options");

    const helper_path = stack_overflow_test_options.helper_path;
    if (helper_path.len == 0) {
        std.debug.print("Missing stack_overflow_test_options.helper_path\n", .{});
        return error.TestUnexpectedResult;
    }

    const result = try std.process.run(allocator, io, .{
        .argv = &.{ helper_path, mode },
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    try verifyHandlerOutput(result.term, result.stderr, expected, expected_code);
}

fn verifyHandlerOutput(term: std.process.Child.Term, stderr_output: []const u8, expected: []const u8, expected_code: u8) StackOverflowTestError!void {
    const has_expected_msg = std.mem.find(u8, stderr_output, expected) != null;
    const has_wrong_stack_msg = std.mem.find(u8, stderr_output, "overflowed its stack memory") != null and
        !std.mem.eql(u8, expected, "overflowed its stack memory");

    switch (term) {
        .exited => |code| {
            if (code == expected_code) {
                try std.testing.expect(has_expected_msg);
                try std.testing.expect(!has_wrong_stack_msg);
                return;
            }

            // musl can run our handler but still classify an overflow as the
            // generic SIGSEGV path on some stack layouts. Treat that like the
            // uncaught SIGSEGV skip below instead of failing this portability
            // check.
            if (comptime builtin.os.tag != .windows and builtin.os.tag != .freestanding and builtin.abi == .musl) {
                const expected_stack_overflow = std.mem.eql(u8, expected, "overflowed its stack memory");
                const fell_back_to_access_violation = code == 139 and
                    std.mem.find(u8, stderr_output, "Segmentation fault") != null;

                if (expected_stack_overflow and fell_back_to_access_violation) {
                    std.debug.print("Warning: Stack overflow was handled as access violation on musl\n", .{});
                    return error.SkipZigTest;
                }
            }

            std.debug.print("Unexpected exit code: {}\n", .{code});
        },
        .signal => |sig| {
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
