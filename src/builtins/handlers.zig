//! Generic signal handlers for stack overflow, access violation, and arithmetic errors.
//!
//! This module provides a mechanism to catch runtime errors like stack overflows,
//! access violations, and division by zero, handling them with custom callbacks
//! instead of crashing with a raw signal.
//!
//! On POSIX systems (Linux, macOS), we use sigaltstack to set up an alternate
//! signal stack and install handlers for SIGSEGV, SIGBUS, and SIGFPE.
//!
//! On Windows, we use SetUnhandledExceptionFilter to catch various exceptions.
//!
//! Freestanding targets (like wasm32) are not supported (no signal handling available).

const std = @import("std");
const builtin = @import("builtin");
const posix = if (builtin.os.tag != .windows and builtin.os.tag != .freestanding) std.posix else undefined;

// Windows types and constants
const DWORD = u32;
const LONG = i32;
const ULONG_PTR = usize;
const PVOID = ?*anyopaque;
const HANDLE = ?*anyopaque;
const BOOL = i32;

const EXCEPTION_STACK_OVERFLOW: DWORD = 0xC00000FD;
const EXCEPTION_ACCESS_VIOLATION: DWORD = 0xC0000005;
const EXCEPTION_INT_DIVIDE_BY_ZERO: DWORD = 0xC0000094;
const EXCEPTION_INT_OVERFLOW: DWORD = 0xC0000095;
const EXCEPTION_CONTINUE_SEARCH: LONG = 0;
const STD_ERROR_HANDLE: DWORD = @bitCast(@as(i32, -12));
const INVALID_HANDLE_VALUE: HANDLE = @ptrFromInt(std.math.maxInt(usize));

const EXCEPTION_RECORD = extern struct {
    ExceptionCode: DWORD,
    ExceptionFlags: DWORD,
    ExceptionRecord: ?*EXCEPTION_RECORD,
    ExceptionAddress: PVOID,
    NumberParameters: DWORD,
    ExceptionInformation: [15]ULONG_PTR,
};

const CONTEXT = extern struct {
    // We don't need the full context, just enough to make the struct valid
    data: [1232]u8, // Size varies by arch, this is x64 size
};

const EXCEPTION_POINTERS = extern struct {
    ExceptionRecord: *EXCEPTION_RECORD,
    ContextRecord: *CONTEXT,
};

const LPTOP_LEVEL_EXCEPTION_FILTER = ?*const fn (*EXCEPTION_POINTERS) callconv(.winapi) LONG;

// Windows API imports
extern "kernel32" fn SetUnhandledExceptionFilter(lpTopLevelExceptionFilter: LPTOP_LEVEL_EXCEPTION_FILTER) callconv(.winapi) LPTOP_LEVEL_EXCEPTION_FILTER;
extern "kernel32" fn GetStdHandle(nStdHandle: DWORD) callconv(.winapi) HANDLE;
extern "kernel32" fn WriteFile(hFile: HANDLE, lpBuffer: [*]const u8, nNumberOfBytesToWrite: DWORD, lpNumberOfBytesWritten: ?*DWORD, lpOverlapped: ?*anyopaque) callconv(.winapi) BOOL;
extern "kernel32" fn ExitProcess(uExitCode: c_uint) callconv(.winapi) noreturn;

/// Size of the alternate signal stack (64KB should be plenty for the handler)
const ALT_STACK_SIZE = 64 * 1024;

/// Storage for the alternate signal stack (POSIX only)
var alt_stack_storage: [ALT_STACK_SIZE]u8 align(16) = undefined;

/// Whether the handler has been installed
var handler_installed = false;

/// Callback function type for handling stack overflow
pub const StackOverflowCallback = *const fn () noreturn;

/// Callback function type for handling access violation/segfault
pub const AccessViolationCallback = *const fn (fault_addr: usize) noreturn;

/// Callback function type for handling division by zero (and other arithmetic errors)
pub const ArithmeticErrorCallback = *const fn () noreturn;

/// Stored callbacks (set during install)
var stack_overflow_callback: ?StackOverflowCallback = null;
var access_violation_callback: ?AccessViolationCallback = null;
var arithmetic_error_callback: ?ArithmeticErrorCallback = null;

/// Install signal handlers with custom callbacks.
///
/// Parameters:
/// - on_stack_overflow: Called when a stack overflow is detected. Must not return.
/// - on_access_violation: Called for other memory access violations (segfaults).
///   Receives the fault address. Must not return.
/// - on_arithmetic_error: Called for arithmetic errors like division by zero. Must not return.
///
/// Returns true if the handlers were installed successfully, false otherwise.
pub fn install(
    on_stack_overflow: StackOverflowCallback,
    on_access_violation: AccessViolationCallback,
    on_arithmetic_error: ArithmeticErrorCallback,
) bool {
    if (handler_installed) return true;

    stack_overflow_callback = on_stack_overflow;
    access_violation_callback = on_access_violation;
    arithmetic_error_callback = on_arithmetic_error;

    if (comptime builtin.os.tag == .windows) {
        return installWindows();
    }

    if (comptime builtin.os.tag == .freestanding) {
        // Freestanding targets (like wasm32) don't support signal handling
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

    // Install the SIGSEGV handler for stack overflow and access violations
    const segv_action = posix.Sigaction{
        .handler = .{ .sigaction = handleSegvSignal },
        .mask = posix.sigemptyset(),
        .flags = posix.SA.SIGINFO | posix.SA.ONSTACK,
    };

    posix.sigaction(posix.SIG.SEGV, &segv_action, null);

    // Also catch SIGBUS which can occur on some systems for stack overflow
    posix.sigaction(posix.SIG.BUS, &segv_action, null);

    // Install the SIGFPE handler for division by zero and other arithmetic errors
    const fpe_action = posix.Sigaction{
        .handler = .{ .sigaction = handleFpeSignal },
        .mask = posix.sigemptyset(),
        .flags = posix.SA.SIGINFO | posix.SA.ONSTACK,
    };

    posix.sigaction(posix.SIG.FPE, &fpe_action, null);

    handler_installed = true;
    return true;
}

fn installWindows() bool {
    _ = SetUnhandledExceptionFilter(handleExceptionWindows);
    handler_installed = true;
    return true;
}

/// Windows exception handler function
fn handleExceptionWindows(exception_info: *EXCEPTION_POINTERS) callconv(.winapi) LONG {
    const exception_code = exception_info.ExceptionRecord.ExceptionCode;

    // Check if this is a known exception type
    const is_stack_overflow = (exception_code == EXCEPTION_STACK_OVERFLOW);
    const is_access_violation = (exception_code == EXCEPTION_ACCESS_VIOLATION);
    const is_divide_by_zero = (exception_code == EXCEPTION_INT_DIVIDE_BY_ZERO);
    const is_int_overflow = (exception_code == EXCEPTION_INT_OVERFLOW);
    const is_arithmetic_error = is_divide_by_zero or is_int_overflow;

    if (!is_stack_overflow and !is_access_violation and !is_arithmetic_error) {
        // Let other handlers deal with this exception
        return EXCEPTION_CONTINUE_SEARCH;
    }

    if (is_stack_overflow) {
        if (stack_overflow_callback) |callback| {
            callback();
        }
        ExitProcess(134);
    } else if (is_arithmetic_error) {
        if (arithmetic_error_callback) |callback| {
            callback();
        }
        ExitProcess(136); // 128 + 8 (SIGFPE)
    } else {
        if (access_violation_callback) |callback| {
            // Get fault address from ExceptionInformation[1] for access violations
            const fault_addr = exception_info.ExceptionRecord.ExceptionInformation[1];
            callback(fault_addr);
        }
        ExitProcess(139);
    }
}

/// The POSIX SIGSEGV/SIGBUS signal handler function
fn handleSegvSignal(_: i32, info: *const posix.siginfo_t, _: ?*anyopaque) callconv(.c) void {
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

    if (likely_stack_overflow) {
        if (stack_overflow_callback) |callback| {
            callback();
        }
    } else {
        if (access_violation_callback) |callback| {
            callback(fault_addr);
        }
    }

    // If no callback was set, exit with appropriate code
    if (likely_stack_overflow) {
        posix.exit(134); // 128 + 6 (SIGABRT-like)
    } else {
        posix.exit(139); // 128 + 11 (SIGSEGV)
    }
}

/// The POSIX SIGFPE signal handler function (division by zero, etc.)
fn handleFpeSignal(_: i32, _: *const posix.siginfo_t, _: ?*anyopaque) callconv(.c) void {
    if (arithmetic_error_callback) |callback| {
        callback();
    }

    // If no callback was set, exit with SIGFPE code
    posix.exit(136); // 128 + 8 (SIGFPE)
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

    // If the fault address is close to the current stack pointer (within 16MB),
    // it's very likely a stack overflow. The signal handler runs on an alternate
    // stack, but the fault address should still be near where the stack was.
    const sp_distance = if (fault_addr < current_sp) current_sp - fault_addr else fault_addr - current_sp;
    if (sp_distance < 16 * 1024 * 1024) { // Within 16MB of stack pointer
        return true;
    }

    // On 64-bit systems, stacks are typically placed in high memory.
    // On macOS, the stack is around 0x16XXXXXXXX (about 6GB mark).
    // On Linux, it's typically near 0x7FFFFFFFFFFF.
    // If the fault address is in the upper half of the address space,
    // it's more likely to be a stack-related issue.
    if (comptime @sizeOf(usize) == 8) {
        // 64-bit: check if address is in upper portion of address space
        // On macOS, stacks start around 0x100000000 (4GB) and go up
        // On Linux, stacks are near 0x7FFFFFFFFFFF
        const lower_bound: usize = 0x100000000; // 4GB
        if (fault_addr > lower_bound) {
            // This is in the region where stacks typically are on 64-bit systems
            // Default to assuming it's a stack overflow for addresses in this range
            return true;
        }
    } else {
        // 32-bit: stacks are typically in the upper portion of the 4GB space
        const lower_bound: usize = 0x40000000; // 1GB
        if (fault_addr > lower_bound) {
            return true;
        }
    }

    return false;
}

/// Format a usize as hexadecimal (for use in callbacks)
pub fn formatHex(value: usize, buf: []u8) []const u8 {
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

test "formatHex" {
    var buf: [18]u8 = undefined;

    const zero = formatHex(0, &buf);
    try std.testing.expectEqualStrings("0x0", zero);

    const small = formatHex(0xff, &buf);
    try std.testing.expectEqualStrings("0xff", small);

    const medium = formatHex(0xdeadbeef, &buf);
    try std.testing.expectEqualStrings("0xdeadbeef", medium);
}
