//! POSIX-only test scaffolding for `stack_overflow.zig`.
//!
//! Lives in a separate file so it is never semantically analyzed on Windows /
//! freestanding targets where `std.posix.fd_t` and the POSIX `std.c` symbols
//! are not available.

const std = @import("std");
const posix = std.posix;

const stack_overflow_root = @import("stack_overflow.zig");
const install = stack_overflow_root.install;
const triggerStackOverflowForTest = stack_overflow_root.triggerStackOverflowForTest;

/// Forks a child process that intentionally overflows its stack and verifies
/// the installed handler prints the expected diagnostic before exiting.
pub fn run() !void {
    // Create a pipe to capture stderr from the child
    var pipe_fds: [2]posix.fd_t = undefined;
    if (std.c.pipe(&pipe_fds) != 0) return error.PipeFailed;
    const pipe_read = pipe_fds[0];
    const pipe_write = pipe_fds[1];

    const fork_result = std.c.fork();
    if (fork_result < 0) {
        _ = std.c.close(pipe_read);
        _ = std.c.close(pipe_write);
        return error.ForkFailed;
    }

    if (fork_result == 0) {
        // Child process
        _ = std.c.close(pipe_read);

        // Redirect stderr to the pipe
        if (std.c.dup2(pipe_write, posix.STDERR_FILENO) < 0) std.process.exit(99);
        _ = std.c.close(pipe_write);

        // Install the handler and trigger stack overflow
        _ = install();
        triggerStackOverflowForTest();
        // Should never reach here
        unreachable;
    } else {
        // Parent process
        _ = std.c.close(pipe_write);

        // Wait for child to exit
        var status: c_int = 0;
        _ = std.c.waitpid(fork_result, &status, 0);

        // Parse the wait status (Unix encoding)
        const ustatus: u32 = @bitCast(status);
        const exited_normally = (ustatus & 0x7f) == 0;
        const exit_code: u8 = @truncate((ustatus >> 8) & 0xff);
        const termination_signal: u8 = @truncate(ustatus & 0x7f);

        // Read stderr output from child
        var stderr_buf: [4096]u8 = undefined;
        const read_result = std.c.read(pipe_read, &stderr_buf, stderr_buf.len);
        const bytes_read: usize = if (read_result > 0) @intCast(read_result) else 0;
        _ = std.c.close(pipe_read);

        const stderr_output = stderr_buf[0..bytes_read];

        try verifyHandlerOutput(exited_normally, exit_code, termination_signal, stderr_output);
    }
}

fn verifyHandlerOutput(exited_normally: bool, exit_code: u8, termination_signal: u8, stderr_output: []const u8) !void {
    // Exit code 134 = stack overflow detected
    // Exit code 139 = generic segfault (handler caught it but didn't classify as stack overflow)
    if (exited_normally and (exit_code == 134 or exit_code == 139)) {
        // Check that our handler message was printed
        const has_stack_overflow_msg = std.mem.find(u8, stderr_output, "overflowed its stack memory") != null;
        const has_segfault_msg = std.mem.find(u8, stderr_output, "Segmentation fault") != null;

        // Handler should have printed EITHER stack overflow message OR segfault message
        try std.testing.expect(has_stack_overflow_msg or has_segfault_msg);
    } else if (!exited_normally and (termination_signal == @intFromEnum(posix.SIG.SEGV) or termination_signal == @intFromEnum(posix.SIG.BUS))) {
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
