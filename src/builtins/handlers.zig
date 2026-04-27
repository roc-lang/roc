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

// Platform-specific pthread helpers for capturing stack bounds.
// Guarded by link_libc so cross-compilation for musl targets (where libc
// is not explicitly linked) doesn't fail on the extern "c" declarations.
const pthread = if (!builtin.link_libc)
    struct {}
else if (builtin.os.tag == .macos or builtin.os.tag == .ios or
    builtin.os.tag == .tvos or builtin.os.tag == .watchos or
    builtin.os.tag == .visionos)
    struct {
        extern "c" fn pthread_self() std.c.pthread_t;
        extern "c" fn pthread_get_stackaddr_np(std.c.pthread_t) ?*anyopaque;
        extern "c" fn pthread_get_stacksize_np(std.c.pthread_t) usize;
    }
else if (builtin.os.tag == .linux)
    struct {
        extern "c" fn pthread_self() std.c.pthread_t;
        extern "c" fn pthread_getattr_np(std.c.pthread_t, *std.c.pthread_attr_t) c_int;
        extern "c" fn pthread_attr_getstack(*const std.c.pthread_attr_t, *?*anyopaque, *usize) c_int;
        extern "c" fn pthread_attr_destroy(*std.c.pthread_attr_t) c_int;
    }
else
    struct {};

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
extern "kernel32" fn TerminateProcess(hProcess: HANDLE, uExitCode: c_uint) callconv(.winapi) BOOL;
extern "kernel32" fn GetCurrentProcess() callconv(.winapi) HANDLE;

/// Size of the alternate signal stack (64KB should be plenty for the handler)
const ALT_STACK_SIZE = 64 * 1024;

/// Storage for the alternate signal stack (POSIX only)
var alt_stack_storage: [ALT_STACK_SIZE]u8 align(16) = undefined;

/// Whether the handler has been installed
var handler_installed = false;

/// Stack bounds captured at install time (POSIX only).
/// Used to reliably detect stack overflows — the alt-stack `sp` is far from
/// the main stack, so comparing against the current `sp` doesn't work.
var stack_lower: usize = 0;
var stack_upper: usize = 0;

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
    // Capture the main thread's stack bounds so the signal handler can
    // reliably distinguish stack overflows from other segfaults.
    // The handler runs on an alternate stack, so its `sp` is far from
    // the main stack — we need the real bounds instead.
    captureStackBounds();

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
        // Use TerminateProcess instead of ExitProcess: after a stack overflow the
        // stack is in a fragile state and ExitProcess's DLL-detach cleanup can
        // trigger a secondary ACCESS_VIOLATION, producing exit code 5 (0xC0000005
        // truncated to u8) instead of the intended 134.
        _ = TerminateProcess(GetCurrentProcess(), 134);
        // Use @trap() instead of unreachable: unreachable is UB in ReleaseFast,
        // while @trap() always generates a defined trap instruction.
        @trap();
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
fn handleSegvSignal(_: posix.SIG, info: *const posix.siginfo_t, context: ?*anyopaque) callconv(.c) void {
    // Get the fault address - access differs by platform
    const fault_addr: usize = getFaultAddress(info);

    // Use the original (faulting) stack pointer from the signal context rather than the
    // current sp, which points to the alt-stack and is far from the main stack.
    const faulting_sp = getContextSp(context);

    const likely_stack_overflow = isLikelyStackOverflow(fault_addr, faulting_sp);

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
        std.process.exit(134); // 128 + 6 (SIGABRT-like)
    } else {
        std.process.exit(139); // 128 + 11 (SIGSEGV)
    }
}

/// The POSIX SIGFPE signal handler function (division by zero, etc.)
fn handleFpeSignal(_: posix.SIG, _: *const posix.siginfo_t, _: ?*anyopaque) callconv(.c) void {
    if (arithmetic_error_callback) |callback| {
        callback();
    }

    // If no callback was set, exit with SIGFPE code
    std.process.exit(136); // 128 + 8 (SIGFPE)
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

/// Extract the stack pointer that was active when the signal fired from the signal
/// context (third argument to a SA_SIGINFO handler).  The handler itself runs on
/// the alt-stack, so reading `sp` via inline asm gives the alt-stack pointer —
/// which is far from the main stack and breaks the proximity heuristic.  Reading
/// the register directly from the saved context gives the correct value.
fn getContextSp(context: ?*anyopaque) usize {
    if (comptime builtin.os.tag == .linux) {
        if (context) |ctx| {
            const bytes: [*]const u8 = @ptrCast(ctx);
            if (comptime builtin.cpu.arch == .x86_64) {
                // Linux x86_64 ucontext_t layout (kernel ABI, stable since 2.6):
                //   0: uc_flags (usize)
                //   8: uc_link  (?*ucontext_t)
                //  16: uc_stack (stack_t: sp[8] flags[4] _pad[4] size[8] = 24 bytes)
                //  40: uc_mcontext: r8 r9 r10 r11 r12 r13 r14 r15 rdi rsi rbp rbx rdx rax rcx rsp rip
                const rsp_offset = 40 + 15 * 8; // 160
                return @as(*const u64, @ptrCast(@alignCast(bytes + rsp_offset))).*;
            } else if (comptime builtin.cpu.arch == .aarch64) {
                // Linux aarch64 ucontext_t layout (kernel ABI):
                //   0: uc_flags (8), uc_link (8), uc_stack (24), uc_sigmask (8), _unused (120) = 168
                // 168->176: 8-byte padding to align uc_mcontext to 16
                // 176: uc_mcontext: fault_address[8 align16], x[30×8=240], lr[8], sp[8], pc[8]
                const sp_offset = 176 + 8 + 30 * 8 + 8; // 432
                return @as(*const u64, @ptrCast(@alignCast(bytes + sp_offset))).*;
            }
        }
    }
    // Fallback (non-Linux or unrecognised arch): current sp.  On the alt-stack
    // this is wrong for stack-overflow detection; captureStackBounds + the bounds
    // check handle it on platforms where pthread is available.
    var sp: usize = 0;
    asm volatile (""
        : [sp] "={sp}" (sp),
    );
    return sp;
}

/// Try to discover the main thread's stack address range from the OS.
/// Falls back gracefully — if bounds can't be determined, `isLikelyStackOverflow`
/// uses an sp-proximity heuristic instead.
fn captureStackBounds() void {
    if (comptime !builtin.link_libc) {
        // No libc available (e.g. cross-compiled musl without explicit libc).
        // The sp-proximity fallback will be used.
    } else if (comptime builtin.os.tag == .macos or builtin.os.tag == .ios or
        builtin.os.tag == .tvos or builtin.os.tag == .watchos or
        builtin.os.tag == .visionos)
    {
        const self = pthread.pthread_self();
        const base = @intFromPtr(pthread.pthread_get_stackaddr_np(self));
        const size = pthread.pthread_get_stacksize_np(self);
        if (base > 0 and size > 0) {
            stack_upper = base;
            stack_lower = base - size;
        }
    } else if (comptime builtin.os.tag == .linux) {
        const self = pthread.pthread_self();
        var attr: std.c.pthread_attr_t = undefined;
        if (pthread.pthread_getattr_np(self, &attr) == 0) {
            var addr: ?*anyopaque = null;
            var size: usize = 0;
            if (pthread.pthread_attr_getstack(&attr, &addr, &size) == 0) {
                if (addr) |a| {
                    stack_lower = @intFromPtr(a);
                    stack_upper = stack_lower + size;
                }
            }
            _ = pthread.pthread_attr_destroy(&attr);
        }
    }
    // Other POSIX platforms (BSDs): the sp-proximity fallback will be used.
}

/// Determine if a fault is likely a stack overflow by checking whether the
/// fault address falls within (or just below) the main thread's stack region.
///
/// We capture the stack bounds at handler-install time because the signal
/// handler runs on an alternate stack whose `sp` is far from the main stack.
fn isLikelyStackOverflow(fault_addr: usize, current_sp: usize) bool {
    // Null-page dereferences are never stack overflows.
    if (fault_addr < 4096) return false;

    // If we captured the stack bounds, check whether the fault is in or
    // just below the stack region (the guard page sits right below it).
    if (stack_lower > 0 and stack_upper > 0) {
        // Allow a 64KB margin below the stack for guard pages.
        const margin = 64 * 1024;
        const effective_lower = if (stack_lower >= margin) stack_lower - margin else 0;
        return fault_addr >= effective_lower and fault_addr < stack_upper;
    }

    // Fallback when stack bounds are unavailable: check proximity to sp.
    const sp_distance = if (fault_addr < current_sp) current_sp - fault_addr else fault_addr - current_sp;
    return sp_distance < 16 * 1024 * 1024;
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
