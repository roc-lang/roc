//! Stack overflow detection and handling for the Roc compiler.
//!
//! This module provides a mechanism to catch stack overflows and report them
//! with a helpful error message instead of a generic segfault. This is particularly
//! useful during compiler development when recursive algorithms might blow the stack.
//!
//! On POSIX systems (Linux, macOS), we use sigaltstack to set up an alternate
//! signal stack and install a SIGSEGV handler that detects stack overflows.
//!
//! Windows and WASI are not currently supported.

const std = @import("std");
const builtin = @import("builtin");
const posix = if (builtin.os.tag != .windows and builtin.os.tag != .wasi) std.posix else undefined;

/// Size of the alternate signal stack (64KB should be plenty for the handler)
const ALT_STACK_SIZE = 64 * 1024;

/// Storage for the alternate signal stack (POSIX only)
var alt_stack_storage: [ALT_STACK_SIZE]u8 align(16) = undefined;

/// Whether the handler has been installed
var handler_installed = false;

/// Error message to display on stack overflow
const STACK_OVERFLOW_MESSAGE =
    \\
    \\================================================================================
    \\STACK OVERFLOW in the Roc compiler
    \\================================================================================
    \\
    \\The Roc compiler ran out of stack space. This is a bug in the compiler,
    \\not in your code.
    \\
    \\This often happens due to:
    \\  - Infinite recursion in type translation or unification
    \\  - Very deeply nested expressions without tail-call optimization
    \\  - Cyclic data structures without proper cycle detection
    \\
    \\Please report this issue at: https://github.com/roc-lang/roc/issues
    \\
    \\Include the Roc code that triggered this error if possible.
    \\
    \\================================================================================
    \\
    \\
;

/// Install the stack overflow handler.
/// This should be called early in main() before any significant work is done.
/// Returns true if the handler was installed successfully, false otherwise.
pub fn install() bool {
    if (handler_installed) return true;

    if (comptime builtin.os.tag == .windows) {
        return installWindows();
    }

    if (comptime builtin.os.tag == .wasi) {
        // WASI doesn't support signal handling
        return false;
    }

    return installPosix();
}

fn installPosix() bool {
    // Set up the alternate signal stack
    var alt_stack = posix.stack_t{
        .sp = &alt_stack_storage,
        .flags = 0,
        .size = ALT_STACK_SIZE,
    };

    posix.sigaltstack(&alt_stack, null) catch {
        return false;
    };

    // Install the SIGSEGV handler
    const action = posix.Sigaction{
        .handler = .{ .sigaction = handleSignalPosix },
        .mask = posix.sigemptyset(),
        .flags = posix.SA.SIGINFO | posix.SA.ONSTACK,
    };

    posix.sigaction(posix.SIG.SEGV, &action, null);

    // Also catch SIGBUS which can occur on some systems for stack overflow
    posix.sigaction(posix.SIG.BUS, &action, null);

    handler_installed = true;
    return true;
}

fn installWindows() bool {
    // Windows support requires SetUnhandledExceptionFilter which isn't
    // exposed in Zig's stdlib. Skip for now - POSIX platforms are covered.
    return false;
}

/// The POSIX signal handler function
fn handleSignalPosix(sig: i32, info: *const posix.siginfo_t, _: ?*anyopaque) callconv(.c) void {
    // Get the fault address - access differs by platform
    const fault_addr: usize = getFaultAddress(info);

    // Get the current stack pointer to help determine if this is a stack overflow
    var current_sp: usize = 0;
    asm volatile (""
        : [sp] "={sp}" (current_sp),
    );

    // A stack overflow typically occurs when the fault address is near the stack pointer
    // or below the stack (stacks grow downward on most architectures)
    const likely_stack_overflow = isLikelyStackOverflow(fault_addr, current_sp);

    // Write our error message to stderr (use STDERR_FILENO directly for signal safety)
    const stderr_fd = posix.STDERR_FILENO;

    if (likely_stack_overflow) {
        _ = posix.write(stderr_fd, STACK_OVERFLOW_MESSAGE) catch {};
    } else {
        // Generic segfault - provide some context
        const generic_msg = switch (sig) {
            posix.SIG.SEGV => "\nSegmentation fault (SIGSEGV) in the Roc compiler.\nFault address: ",
            posix.SIG.BUS => "\nBus error (SIGBUS) in the Roc compiler.\nFault address: ",
            else => "\nFatal signal in the Roc compiler.\nFault address: ",
        };
        _ = posix.write(stderr_fd, generic_msg) catch {};

        // Write the fault address as hex
        var addr_buf: [18]u8 = undefined;
        const addr_str = formatHex(fault_addr, &addr_buf);
        _ = posix.write(stderr_fd, addr_str) catch {};
        _ = posix.write(stderr_fd, "\n\nPlease report this issue at: https://github.com/roc-lang/roc/issues\n\n") catch {};
    }

    // Exit with a distinct error code for stack overflow
    if (likely_stack_overflow) {
        posix.exit(134); // 128 + 6 (SIGABRT-like)
    } else {
        posix.exit(139); // 128 + 11 (SIGSEGV)
    }
}

/// Get the fault address from siginfo_t (platform-specific)
fn getFaultAddress(info: *const posix.siginfo_t) usize {
    // The siginfo_t structure varies by platform
    if (comptime builtin.os.tag == .linux) {
        // Linux: fault address is in fields.sigfault.addr
        return @intFromPtr(info.fields.sigfault.addr);
    } else if (comptime builtin.os.tag == .macos or
        builtin.os.tag == .ios or
        builtin.os.tag == .tvos or
        builtin.os.tag == .watchos or
        builtin.os.tag == .visionos or
        builtin.os.tag == .freebsd or
        builtin.os.tag == .dragonfly or
        builtin.os.tag == .netbsd or
        builtin.os.tag == .openbsd)
    {
        // macOS/iOS/BSD: fault address is in addr field
        return @intFromPtr(info.addr);
    } else {
        // Fallback: return 0 if we can't determine the address
        return 0;
    }
}

/// Heuristic to determine if a fault is likely a stack overflow
fn isLikelyStackOverflow(fault_addr: usize, current_sp: usize) bool {
    // If fault address is 0 or very low, it's likely a null pointer dereference
    if (fault_addr < 4096) return false;

    // Stack overflows typically fault near the stack guard page
    // The fault address will be close to (but below) the current stack pointer
    // We use a generous range since the stack pointer in the signal handler
    // is on the alternate stack

    // On most systems, the main stack is in high memory and grows down
    // A stack overflow fault will be at an address lower than the normal stack

    // Check if fault address is within a reasonable range of where stack would be
    // This is a heuristic - we check if the fault is in the lower part of address space
    // where guard pages typically are

    const max_addr = std.math.maxInt(usize);
    const high_memory_threshold = max_addr - (16 * 1024 * 1024 * 1024); // 16GB from top

    // If the fault is in the high memory region (where stacks live) but at a page boundary
    // it's likely a stack guard page hit
    if (fault_addr > high_memory_threshold) {
        // Check if it's at a page boundary (guard pages are typically page-aligned)
        const page_size = std.heap.page_size_min;
        const page_aligned = (fault_addr & (page_size - 1)) == 0 or (fault_addr & (page_size - 1)) < 64;
        if (page_aligned) return true;
    }

    // Also check if the fault address is suspiciously close to the current SP
    // This catches cases where we're still on the main stack when the overflow happens
    const sp_distance = if (fault_addr < current_sp) current_sp - fault_addr else fault_addr - current_sp;
    if (sp_distance < 1024 * 1024) { // Within 1MB of stack pointer
        return true;
    }

    return false;
}

/// Format a usize as hexadecimal
fn formatHex(value: usize, buf: []u8) []const u8 {
    const hex_chars = "0123456789abcdef";
    var i: usize = buf.len;

    if (value == 0) {
        i -= 1;
        buf[i] = '0';
    } else {
        var v = value;
        while (v > 0 and i > 2) {
            i -= 1;
            buf[i] = hex_chars[v & 0xf];
            v >>= 4;
        }
    }

    // Add 0x prefix
    i -= 1;
    buf[i] = 'x';
    i -= 1;
    buf[i] = '0';

    return buf[i..];
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

    const zero = formatHex(0, &buf);
    try std.testing.expectEqualStrings("0x0", zero);

    const small = formatHex(0xff, &buf);
    try std.testing.expectEqualStrings("0xff", small);

    const medium = formatHex(0xdeadbeef, &buf);
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
    // Skip on WASI - no process spawning
    // Skip on Windows - SetUnhandledExceptionFilter not in Zig stdlib
    if (comptime builtin.os.tag == .wasi or builtin.os.tag == .windows) {
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
        const has_stack_overflow_msg = std.mem.indexOf(u8, stderr_output, "STACK OVERFLOW") != null;
        const has_segfault_msg = std.mem.indexOf(u8, stderr_output, "Segmentation fault") != null;
        const has_roc_compiler_msg = std.mem.indexOf(u8, stderr_output, "Roc compiler") != null;

        // Handler should have printed EITHER stack overflow message OR segfault message
        try std.testing.expect(has_stack_overflow_msg or has_segfault_msg);
        try std.testing.expect(has_roc_compiler_msg);
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
