//! Process signal handling for hosts that want crash messages.
//!
//! This module is not part of Roc builtins. Hosts may opt into it explicitly,
//! and the compiler uses it for its own process. Generated Roc code does not
//! install or call this module.

const std = @import("std");
const builtin = @import("builtin");

const supports_posix_signals = switch (builtin.os.tag) {
    .linux,
    .macos,
    .ios,
    .tvos,
    .watchos,
    .visionos,
    .freebsd,
    .dragonfly,
    .netbsd,
    .openbsd,
    => true,
    else => false,
};

const supports_windows_exceptions = builtin.os.tag == .windows;
const posix = if (supports_posix_signals) std.posix else undefined;

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
    data: [1232]u8,
};

const EXCEPTION_POINTERS = extern struct {
    ExceptionRecord: *EXCEPTION_RECORD,
    ContextRecord: *CONTEXT,
};

const LPTOP_LEVEL_EXCEPTION_FILTER = ?*const fn (*EXCEPTION_POINTERS) callconv(.winapi) LONG;

extern "kernel32" fn SetUnhandledExceptionFilter(lpTopLevelExceptionFilter: LPTOP_LEVEL_EXCEPTION_FILTER) callconv(.winapi) LPTOP_LEVEL_EXCEPTION_FILTER;
extern "kernel32" fn GetStdHandle(nStdHandle: DWORD) callconv(.winapi) HANDLE;
extern "kernel32" fn WriteFile(hFile: HANDLE, lpBuffer: [*]const u8, nNumberOfBytesToWrite: DWORD, lpNumberOfBytesWritten: ?*DWORD, lpOverlapped: ?*anyopaque) callconv(.winapi) BOOL;
extern "kernel32" fn ExitProcess(uExitCode: c_uint) callconv(.winapi) noreturn;
extern "kernel32" fn TerminateProcess(hProcess: HANDLE, uExitCode: c_uint) callconv(.winapi) BOOL;
extern "kernel32" fn GetCurrentProcess() callconv(.winapi) HANDLE;

const ALT_STACK_SIZE = 64 * 1024;

threadlocal var thread_stack_bounds: ?StackBounds = null;
threadlocal var thread_alt_stack_storage: [ALT_STACK_SIZE]u8 align(16) = undefined;
threadlocal var current_thread_installed = false;

var install_mutex: if (supports_posix_signals or supports_windows_exceptions) std.Thread.Mutex else void =
    if (supports_posix_signals or supports_windows_exceptions) .{} else {};
var process_handlers_installed = false;

var stack_overflow_callback: ?StackOverflowCallback = null;
var access_violation_callback: ?AccessViolationCallback = null;
var arithmetic_error_callback: ?ArithmeticErrorCallback = null;

/// Called when the handler identifies a stack overflow.
pub const StackOverflowCallback = *const fn () noreturn;
/// Called when the handler identifies a non-stack memory access violation.
pub const AccessViolationCallback = *const fn (fault_addr: usize) noreturn;
/// Called when the handler identifies an arithmetic exception.
pub const ArithmeticErrorCallback = *const fn () noreturn;

/// Crash callbacks used by process-wide handlers.
pub const Callbacks = struct {
    stack_overflow: StackOverflowCallback,
    access_violation: AccessViolationCallback,
    arithmetic_error: ArithmeticErrorCallback,
};

/// Stack address range for one thread, including guard-page bounds when known.
pub const StackBounds = struct {
    low: usize,
    high: usize,
    guard_low: ?usize = null,
    guard_high: ?usize = null,

    pub fn init(low: usize, high: usize, guard_low: ?usize, guard_high: ?usize) ?StackBounds {
        if (low == 0 or high <= low) return null;
        if (guard_low == null or guard_high == null) {
            return .{ .low = low, .high = high };
        }
        if (guard_high.? <= guard_low.?) return null;
        return .{
            .low = low,
            .high = high,
            .guard_low = guard_low,
            .guard_high = guard_high,
        };
    }

    pub fn containsStackPointer(self: StackBounds, sp: usize) bool {
        return sp >= self.low and sp < self.high;
    }

    pub fn containsGuardAddress(self: StackBounds, addr: usize) bool {
        const guard_low = self.guard_low orelse return false;
        const guard_high = self.guard_high orelse return false;
        return addr >= guard_low and addr < guard_high;
    }
};

/// Classification for a memory fault observed by a crash handler.
pub const FaultKind = enum {
    stack_overflow,
    access_violation,
};

/// Install crash handling for the current thread.
///
/// POSIX signal actions are process-wide, but the alternate signal stack and
/// stack bounds are per-thread. Call this from each thread before it runs work
/// that should produce Roc's crash messages.
pub fn installForCurrentThread(callbacks: Callbacks) bool {
    if (comptime supports_windows_exceptions) {
        return installWindows(callbacks);
    }

    if (comptime !supports_posix_signals) {
        return false;
    }

    if (!current_thread_installed) {
        const bounds = queryCurrentThreadStackBounds() orelse return false;
        thread_stack_bounds = bounds;

        var alt_stack = posix.stack_t{
            .sp = &thread_alt_stack_storage,
            .flags = 0,
            .size = ALT_STACK_SIZE,
        };
        posix.sigaltstack(&alt_stack, null) catch return false;

        current_thread_installed = true;
    }

    return installPosixProcessHandlers(callbacks);
}

/// Return the installed current-thread stack bounds for unit tests.
pub fn currentThreadStackBoundsForTest() ?StackBounds {
    return thread_stack_bounds;
}

/// Classify a fault from its address, interrupted stack pointer, and stack range.
pub fn classifyFault(fault_addr: usize, interrupted_sp: ?usize, bounds: ?StackBounds) FaultKind {
    const stack_bounds = bounds orelse return .access_violation;
    const sp = interrupted_sp orelse return .access_violation;

    if (sp < stack_bounds.low) return .stack_overflow;
    if (stack_bounds.containsGuardAddress(fault_addr)) return .stack_overflow;
    return .access_violation;
}

fn installPosixProcessHandlers(callbacks: Callbacks) bool {
    install_mutex.lock();
    defer install_mutex.unlock();

    stack_overflow_callback = callbacks.stack_overflow;
    access_violation_callback = callbacks.access_violation;
    arithmetic_error_callback = callbacks.arithmetic_error;

    if (process_handlers_installed) return true;

    const segv_action = posix.Sigaction{
        .handler = .{ .sigaction = handleSegvSignal },
        .mask = posix.sigemptyset(),
        .flags = posix.SA.SIGINFO | posix.SA.ONSTACK,
    };

    posix.sigaction(posix.SIG.SEGV, &segv_action, null);
    posix.sigaction(posix.SIG.BUS, &segv_action, null);

    const fpe_action = posix.Sigaction{
        .handler = .{ .sigaction = handleFpeSignal },
        .mask = posix.sigemptyset(),
        .flags = posix.SA.SIGINFO | posix.SA.ONSTACK,
    };

    posix.sigaction(posix.SIG.FPE, &fpe_action, null);

    process_handlers_installed = true;
    return true;
}

fn installWindows(callbacks: Callbacks) bool {
    install_mutex.lock();
    defer install_mutex.unlock();

    stack_overflow_callback = callbacks.stack_overflow;
    access_violation_callback = callbacks.access_violation;
    arithmetic_error_callback = callbacks.arithmetic_error;

    if (!process_handlers_installed) {
        _ = SetUnhandledExceptionFilter(handleExceptionWindows);
        process_handlers_installed = true;
    }
    current_thread_installed = true;
    return true;
}

fn handleExceptionWindows(exception_info: *EXCEPTION_POINTERS) callconv(.winapi) LONG {
    const exception_code = exception_info.ExceptionRecord.ExceptionCode;

    const is_stack_overflow = exception_code == EXCEPTION_STACK_OVERFLOW;
    const is_access_violation = exception_code == EXCEPTION_ACCESS_VIOLATION;
    const is_arithmetic_error =
        exception_code == EXCEPTION_INT_DIVIDE_BY_ZERO or exception_code == EXCEPTION_INT_OVERFLOW;

    if (!is_stack_overflow and !is_access_violation and !is_arithmetic_error) {
        return EXCEPTION_CONTINUE_SEARCH;
    }

    if (is_stack_overflow) {
        if (stack_overflow_callback) |callback| callback();
        _ = TerminateProcess(GetCurrentProcess(), 134);
        @trap();
    }

    if (is_arithmetic_error) {
        if (arithmetic_error_callback) |callback| callback();
        ExitProcess(136);
    }

    if (access_violation_callback) |callback| {
        const fault_addr = exception_info.ExceptionRecord.ExceptionInformation[1];
        callback(fault_addr);
    }
    ExitProcess(139);
}

fn handleSegvSignal(_: i32, info: *const posix.siginfo_t, context: ?*anyopaque) callconv(.c) void {
    const fault_addr = getFaultAddress(info);
    const interrupted_sp = getInterruptedStackPointer(context);

    switch (classifyFault(fault_addr, interrupted_sp, thread_stack_bounds)) {
        .stack_overflow => {
            if (stack_overflow_callback) |callback| callback();
            posix.exit(134);
        },
        .access_violation => {
            if (access_violation_callback) |callback| callback(fault_addr);
            posix.exit(139);
        },
    }
}

fn handleFpeSignal(_: i32, _: *const posix.siginfo_t, _: ?*anyopaque) callconv(.c) void {
    if (arithmetic_error_callback) |callback| callback();
    posix.exit(136);
}

fn getFaultAddress(info: *const posix.siginfo_t) usize {
    if (comptime builtin.os.tag == .linux) {
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
        return @intFromPtr(info.addr);
    } else {
        return 0;
    }
}

fn getInterruptedStackPointer(context: ?*anyopaque) ?usize {
    const context_ptr = context orelse return null;

    if (comptime posix.ucontext_t == void) {
        return null;
    }

    const unaligned: *align(1) posix.ucontext_t = @ptrCast(context_ptr);
    var ctx = unaligned.*;

    if (comptime builtin.os.tag == .macos or
        builtin.os.tag == .ios or
        builtin.os.tag == .tvos or
        builtin.os.tag == .watchos or
        builtin.os.tag == .visionos)
    {
        if (comptime builtin.cpu.arch == .aarch64) {
            ctx.__mcontext_data = @as(*align(1) extern struct {
                onstack: c_int,
                sigmask: std.c.sigset_t,
                stack: std.c.stack_t,
                link: ?*std.c.ucontext_t,
                mcsize: u64,
                mcontext: *std.c.mcontext_t,
                __mcontext_data: std.c.mcontext_t align(@sizeOf(usize)),
            }, @ptrCast(unaligned)).__mcontext_data;
        }
        ctx.mcontext = &ctx.__mcontext_data;

        return switch (builtin.cpu.arch) {
            .aarch64 => @intCast(ctx.mcontext.ss.sp),
            .x86_64 => @intCast(ctx.mcontext.ss.rsp),
            else => null,
        };
    }

    if (comptime builtin.os.tag == .linux) {
        return switch (builtin.cpu.arch) {
            .x86_64 => ctx.mcontext.gregs[std.os.linux.REG.RSP],
            .x86 => ctx.mcontext.gregs[std.os.linux.REG.ESP],
            .aarch64 => ctx.mcontext.sp,
            else => null,
        };
    }

    return null;
}

fn queryCurrentThreadStackBounds() ?StackBounds {
    if (comptime builtin.os.tag == .macos or
        builtin.os.tag == .ios or
        builtin.os.tag == .tvos or
        builtin.os.tag == .watchos or
        builtin.os.tag == .visionos)
    {
        const darwin = struct {
            extern "c" fn pthread_get_stackaddr_np(thread: std.c.pthread_t) ?*anyopaque;
            extern "c" fn pthread_get_stacksize_np(thread: std.c.pthread_t) usize;
        };

        const high_ptr = darwin.pthread_get_stackaddr_np(std.c.pthread_self()) orelse return null;
        const high = @intFromPtr(high_ptr);
        const size = darwin.pthread_get_stacksize_np(std.c.pthread_self());
        if (size == 0 or high <= size) return null;

        return StackBounds.init(high - size, high, null, null);
    }

    if (comptime builtin.os.tag == .linux) {
        const linux = struct {
            extern "c" fn pthread_getattr_np(thread: std.c.pthread_t, attr: *std.c.pthread_attr_t) c_int;
            extern "c" fn pthread_attr_getstack(attr: *const std.c.pthread_attr_t, stackaddr: *?*anyopaque, stacksize: *usize) c_int;
            extern "c" fn pthread_attr_getguardsize(attr: *const std.c.pthread_attr_t, guardsize: *usize) c_int;
        };

        var attr: std.c.pthread_attr_t = undefined;
        if (std.c.pthread_attr_init(&attr) != .SUCCESS) return null;
        defer _ = std.c.pthread_attr_destroy(&attr);

        if (linux.pthread_getattr_np(std.c.pthread_self(), &attr) != 0) return null;

        var stack_addr: ?*anyopaque = null;
        var stack_size: usize = 0;
        if (linux.pthread_attr_getstack(&attr, &stack_addr, &stack_size) != 0) return null;

        const low_ptr = stack_addr orelse return null;
        const low = @intFromPtr(low_ptr);
        if (stack_size == 0) return null;

        var guard_size: usize = 0;
        const has_guard = linux.pthread_attr_getguardsize(&attr, &guard_size) == 0 and guard_size > 0 and low >= guard_size;
        const guard_low: ?usize = if (has_guard) low - guard_size else null;
        const guard_high: ?usize = if (has_guard) low else null;

        return StackBounds.init(low, low + stack_size, guard_low, guard_high);
    }

    return null;
}

/// Format a pointer-sized integer as lowercase hexadecimal into caller storage.
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

test "classifyFault uses only exact stack data" {
    const bounds = StackBounds.init(0x7000, 0x9000, 0x6000, 0x7000).?;
    const unrelated_addr: usize = 0x5000_0000;

    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(unrelated_addr, 0x8000, bounds));
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(unrelated_addr, 0x9000, bounds));
    try std.testing.expectEqual(FaultKind.stack_overflow, classifyFault(0x6800, 0x8000, bounds));
    try std.testing.expectEqual(FaultKind.stack_overflow, classifyFault(0x5000, 0x5ff0, bounds));
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(0x6800, null, bounds));
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(unrelated_addr, 0x8000, null));
}

test "installForCurrentThread records current stack bounds" {
    if (comptime !supports_posix_signals and !supports_windows_exceptions) {
        return error.SkipZigTest;
    }

    const callbacks = Callbacks{
        .stack_overflow = struct {
            fn callback() noreturn {
                std.process.exit(134);
            }
        }.callback,
        .access_violation = struct {
            fn callback(_: usize) noreturn {
                std.process.exit(139);
            }
        }.callback,
        .arithmetic_error = struct {
            fn callback() noreturn {
                std.process.exit(136);
            }
        }.callback,
    };

    try std.testing.expect(installForCurrentThread(callbacks));

    if (comptime supports_posix_signals) {
        const bounds = currentThreadStackBoundsForTest() orelse return error.TestUnexpectedResult;
        try std.testing.expect(bounds.low < bounds.high);
    }
}
