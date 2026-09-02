//! Crash reporting for the test platform hosts.
//!
//! A Roc program that faults with no handler installed dies on a bare signal,
//! and every caller—including the CLI test runner—sees only "killed by signal
//! 11" with no fault address and no distinction between a stack overflow and a
//! wild pointer. Hosts opt into `base.signal_handler` to turn that into a
//! message the reader can act on, which matters most on the platforms nobody
//! can reproduce on locally.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");

const STACK_OVERFLOW_MESSAGE = "\nThis Roc application overflowed its stack memory and crashed.\n\n";
const DIVISION_BY_ZERO_MESSAGE = "\nThis Roc application divided by zero and crashed.\n\n";

const windows = struct {
    const DWORD = u32;
    const HANDLE = ?*anyopaque;
    const STD_ERROR_HANDLE: DWORD = @bitCast(@as(i32, -12));

    const kernel32 = struct {
        extern "kernel32" fn GetStdHandle(nStdHandle: DWORD) callconv(.winapi) HANDLE;
        extern "kernel32" fn WriteFile(hFile: HANDLE, lpBuffer: [*]const u8, nNumberOfBytesToWrite: DWORD, lpNumberOfBytesWritten: ?*DWORD, lpOverlapped: ?*anyopaque) callconv(.winapi) i32;
        extern "kernel32" fn TerminateProcess(hProcess: HANDLE, uExitCode: c_uint) callconv(.winapi) i32;
        extern "kernel32" fn GetCurrentProcess() callconv(.winapi) HANDLE;
        extern "kernel32" fn ExitProcess(uExitCode: c_uint) callconv(.winapi) noreturn;
    };

    fn write(text: []const u8) void {
        const stderr_handle = kernel32.GetStdHandle(STD_ERROR_HANDLE);
        var bytes_written: DWORD = 0;
        _ = kernel32.WriteFile(stderr_handle, text.ptr, @intCast(text.len), &bytes_written, null);
    }
};

/// Callback for stack overflow in a Roc program.
fn handleRocStackOverflow() noreturn {
    if (comptime builtin.os.tag == .windows) {
        windows.write(STACK_OVERFLOW_MESSAGE);
        // TerminateProcess rather than ExitProcess: the stack is blown, and
        // ExitProcess's DLL cleanup can trigger a secondary crash.
        _ = windows.kernel32.TerminateProcess(windows.kernel32.GetCurrentProcess(), 134);
        @trap();
    } else if (comptime builtin.os.tag != .wasi) {
        std.debug.print("{s}", .{STACK_OVERFLOW_MESSAGE});
        std.process.exit(134);
    } else {
        std.process.exit(134);
    }
}

/// Callback for access violation in a Roc program.
fn handleRocAccessViolation(fault_addr: usize, _: base.signal_handler.AccessViolationContext) noreturn {
    var addr_buf: [18]u8 = undefined;
    const addr_str = base.signal_handler.formatHex(fault_addr, &addr_buf);
    const headline = "\nSegmentation fault (SIGSEGV) in this Roc program.\nFault address: ";

    if (comptime builtin.os.tag == .windows) {
        windows.write(headline);
        windows.write(addr_str);
        windows.write("\n\n");
        windows.kernel32.ExitProcess(139);
    } else {
        std.debug.print("{s}{s}\n\n", .{ headline, addr_str });
        std.process.exit(139);
    }
}

/// Callback for arithmetic errors (division by zero) in a Roc program.
fn handleRocArithmeticError() noreturn {
    if (comptime builtin.os.tag == .windows) {
        windows.write(DIVISION_BY_ZERO_MESSAGE);
        windows.kernel32.ExitProcess(136);
    } else if (comptime builtin.os.tag != .wasi) {
        std.debug.print("{s}", .{DIVISION_BY_ZERO_MESSAGE});
        std.process.exit(136); // 128 + 8 (SIGFPE)
    } else {
        std.process.exit(136);
    }
}

/// Install the Roc-program crash messages for the calling thread.
pub fn installForCurrentThread() void {
    _ = base.signal_handler.installForCurrentThread(.{
        .stack_overflow = handleRocStackOverflow,
        .access_violation = handleRocAccessViolation,
        .arithmetic_error = handleRocArithmeticError,
    });
}
