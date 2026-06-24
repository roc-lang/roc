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
const PVECTORED_EXCEPTION_HANDLER = ?*const fn (*EXCEPTION_POINTERS) callconv(.winapi) LONG;

extern "kernel32" fn SetUnhandledExceptionFilter(lpTopLevelExceptionFilter: LPTOP_LEVEL_EXCEPTION_FILTER) callconv(.winapi) LPTOP_LEVEL_EXCEPTION_FILTER;
extern "kernel32" fn AddVectoredExceptionHandler(First: c_uint, Handler: PVECTORED_EXCEPTION_HANDLER) callconv(.winapi) PVOID;
extern "kernel32" fn SetThreadStackGuarantee(StackSizeInBytes: *ULONG) callconv(.winapi) BOOL;
extern "kernel32" fn GetStdHandle(nStdHandle: DWORD) callconv(.winapi) HANDLE;
extern "kernel32" fn WriteFile(hFile: HANDLE, lpBuffer: [*]const u8, nNumberOfBytesToWrite: DWORD, lpNumberOfBytesWritten: ?*DWORD, lpOverlapped: ?*anyopaque) callconv(.winapi) BOOL;
extern "kernel32" fn ExitProcess(uExitCode: c_uint) callconv(.winapi) noreturn;
extern "kernel32" fn TerminateProcess(hProcess: HANDLE, uExitCode: c_uint) callconv(.winapi) BOOL;
extern "kernel32" fn GetCurrentProcess() callconv(.winapi) HANDLE;

const ULONG = c_ulong;

const ALT_STACK_SIZE = 64 * 1024;

threadlocal var thread_stack_bounds: ?StackBounds = null;
threadlocal var thread_alt_stack_storage: [ALT_STACK_SIZE]u8 align(16) = undefined;
threadlocal var current_thread_installed = false;

// zig 0.16 moved the blocking Thread.Mutex behind std.Io; for this one-time,
// process-wide install guard we use the io-free std.atomic.Mutex (tryLock spin).
var install_mutex: if (supports_posix_signals or supports_windows_exceptions) std.atomic.Mutex else void =
    if (supports_posix_signals or supports_windows_exceptions) .unlocked else {};
var process_handlers_installed = false;

var stack_overflow_callback: ?StackOverflowCallback = null;
var access_violation_callback: ?AccessViolationCallback = null;
var arithmetic_error_callback: ?ArithmeticErrorCallback = null;

/// Called when the handler identifies a stack overflow.
pub const StackOverflowCallback = *const fn () noreturn;
/// Called when the handler identifies a non-stack memory access violation.
/// Targets with debug unwind support receive the interrupted CPU context.
pub const AccessViolationContext = if (std.debug.cpu_context.Native == noreturn)
    void
else
    ?std.debug.CpuContextPtr;
/// Called with the fault address and optional interrupted CPU context.
pub const AccessViolationCallback = *const fn (fault_addr: usize, context: AccessViolationContext) noreturn;
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
    page_size: usize,
    guard_low: ?usize = null,
    guard_high: ?usize = null,

    pub fn init(low: usize, high: usize, page_size: usize, guard_low: ?usize, guard_high: ?usize) ?StackBounds {
        if (low == 0 or high <= low or !std.math.isPowerOfTwo(page_size)) return null;
        if (guard_low == null or guard_high == null) {
            return .{ .low = low, .high = high, .page_size = page_size };
        }
        if (guard_high.? <= guard_low.?) return null;
        return .{
            .low = low,
            .high = high,
            .page_size = page_size,
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

    pub fn containsLowerBoundaryAddress(self: StackBounds, fault_addr: usize) bool {
        const boundary_low = self.low -| self.page_size;
        const boundary_high = self.low;

        return fault_addr >= boundary_low and fault_addr < boundary_high;
    }
};

/// Classification for a memory fault observed by a crash handler.
pub const FaultKind = enum {
    stack_overflow,
    access_violation,
};

fn stackPointerFromSignalContext(context: ?*anyopaque) ?usize {
    // zig 0.16 no longer exposes posix.ucontext_t / posix.REG, so for Linux we
    // read the faulting stack pointer from a locally-declared ucontext layout
    // (the kernel signal-frame ABI). For other arches/OSes we return null and
    // classifyFault falls back to the faulting address (guard-page check).
    if (comptime builtin.os.tag != .linux) return null;
    const raw_context = context orelse return null;
    switch (comptime builtin.cpu.arch) {
        .x86_64 => {
            // Linux x86_64 ucontext_t / mcontext_t; RSP is gregs[15] (REG_RSP).
            const stack_t = extern struct { ss_sp: ?*anyopaque, ss_flags: i32, ss_size: usize };
            const mcontext_t = extern struct { gregs: [23]u64, fpregs: ?*anyopaque, reserved1: [8]u64 };
            const ucontext_t = extern struct {
                uc_flags: u64,
                uc_link: ?*anyopaque,
                uc_stack: stack_t,
                uc_mcontext: mcontext_t,
            };
            const uc: *const ucontext_t = @ptrCast(@alignCast(raw_context));
            return @intCast(uc.uc_mcontext.gregs[15]);
        },
        .aarch64 => {
            // Linux aarch64 ucontext_t; uc_sigmask precedes uc_mcontext, which
            // holds fault_address, regs[31], then sp.
            //
            // uc_mcontext (the kernel's struct sigcontext) is 16-byte aligned:
            // its trailing FP/SIMD __reserved area is __aligned__(16). That
            // alignment inserts 8 bytes of padding after the 1024-bit uc_sigmask,
            // so uc_mcontext must be modeled as a 16-aligned struct rather than
            // flattened — otherwise every field shifts 8 bytes early and `sp`
            // lands on regs[30] (the link register). The trailing 16-aligned
            // reserved field reproduces that alignment (mirrors std's mcontext_t).
            const stack_t = extern struct { ss_sp: ?*anyopaque, ss_flags: i32, ss_size: usize };
            const mcontext_t = extern struct {
                fault_address: u64,
                regs: [31]u64,
                sp: u64,
                pc: u64,
                pstate: u64,
                reserved: [256 * 16]u8 align(16),
            };
            const ucontext_t = extern struct {
                uc_flags: u64,
                uc_link: ?*anyopaque,
                uc_stack: stack_t,
                uc_sigmask: [16]u64,
                uc_mcontext: mcontext_t,
            };
            const uc: *const ucontext_t = @ptrCast(@alignCast(raw_context));
            return @intCast(uc.uc_mcontext.sp);
        },
        else => return null,
    }
}

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

/// Distance from the stack pointer within which a fault counts as a stack
/// overflow. A genuine overflow faults inside the frame being set up — the
/// stack-probe/push that ran past the guard — so the fault address is close to
/// the stack pointer. A null or wild write faults far from it. This proximity
/// test is the primary, bounds-independent signal, because the reported stack
/// bounds are not always trustworthy: `pthread_getattr_np` on a static-musl
/// main thread reports a region that does not contain the real stack pointer,
/// which previously made any low-address (e.g. null) write look like an
/// overflow. The window is far larger than any real frame yet far smaller than
/// the gap between a high stack and a null/low pointer.
const stack_overflow_proximity: usize = 16 * 1024 * 1024;

/// Classify a fault from its address and stack range.
pub fn classifyFault(fault_addr: usize, stack_pointer: ?usize, bounds: ?StackBounds) FaultKind {
    // Primary signal (bounds-independent): a fault adjacent to the stack
    // pointer is the overflowing access itself.
    if (stack_pointer) |sp| {
        const distance = if (fault_addr >= sp) fault_addr - sp else sp - fault_addr;
        if (distance <= stack_overflow_proximity) return .stack_overflow;
    }

    // Secondary signals, used only when we trust the reported bounds: the stack
    // pointer or the fault sits in the guard page / just below the stack.
    const stack_bounds = bounds orelse return .access_violation;

    if (stack_pointer) |sp| {
        if (stack_bounds.containsGuardAddress(sp)) return .stack_overflow;
        if (stack_bounds.containsLowerBoundaryAddress(sp)) return .stack_overflow;
    }

    if (stack_bounds.containsGuardAddress(fault_addr)) return .stack_overflow;
    if (stack_bounds.containsLowerBoundaryAddress(fault_addr)) return .stack_overflow;
    return .access_violation;
}

fn installPosixProcessHandlers(callbacks: Callbacks) bool {
    while (!install_mutex.tryLock()) {}
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

// Per-thread guarantee reserved so EXCEPTION_STACK_OVERFLOW can dispatch.
// Without this, an overflow on a worker thread takes the process down before
// any handler runs — the OS only reserves a guarantee for the main thread.
// 32 KB is well below the default 1 MB stack and large enough to host our
// minimal handler frame.
const WINDOWS_STACK_GUARANTEE: ULONG = 32 * 1024;

fn installWindows(callbacks: Callbacks) bool {
    while (!install_mutex.tryLock()) {}
    defer install_mutex.unlock();

    stack_overflow_callback = callbacks.stack_overflow;
    access_violation_callback = callbacks.access_violation;
    arithmetic_error_callback = callbacks.arithmetic_error;

    if (!process_handlers_installed) {
        // Vectored handlers get first-chance dispatch and run even when
        // SetUnhandledExceptionFilter is preempted (e.g. by a debugger or
        // CRT-installed filter). Keep the legacy filter as a backstop.
        _ = AddVectoredExceptionHandler(1, handleExceptionWindows);
        _ = SetUnhandledExceptionFilter(handleExceptionWindows);
        process_handlers_installed = true;
    }

    if (!current_thread_installed) {
        var guarantee: ULONG = WINDOWS_STACK_GUARANTEE;
        _ = SetThreadStackGuarantee(&guarantee);
        current_thread_installed = true;
    }
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
        if (comptime AccessViolationContext == void) {
            callback(fault_addr, {});
        } else {
            callback(fault_addr, null);
        }
    }
    ExitProcess(139);
}

fn handleSegvSignal(_: posix.SIG, info: *const posix.siginfo_t, context: ?*anyopaque) callconv(.c) void {
    const fault_addr = getFaultAddress(info);
    const stack_pointer = stackPointerFromSignalContext(context);

    switch (classifyFault(fault_addr, stack_pointer, thread_stack_bounds)) {
        .stack_overflow => {
            if (stack_overflow_callback) |callback| callback();
            std.process.exit(134);
        },
        .access_violation => {
            if (access_violation_callback) |callback| {
                if (comptime AccessViolationContext == void) {
                    callback(fault_addr, {});
                } else {
                    var cpu_context = std.debug.cpu_context.fromPosixSignalContext(context);
                    callback(fault_addr, if (cpu_context) |*ctx| ctx else null);
                }
            }
            std.process.exit(139);
        },
    }
}

fn handleFpeSignal(_: posix.SIG, _: *const posix.siginfo_t, _: ?*anyopaque) callconv(.c) void {
    if (arithmetic_error_callback) |callback| callback();
    std.process.exit(136);
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

        return StackBounds.init(high - size, high, std.heap.pageSize(), null, null);
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

        return StackBounds.init(low, low + stack_size, std.heap.pageSize(), guard_low, guard_high);
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
    const bounds = StackBounds.init(0x7000, 0x9000, 0x1000, 0x6000, 0x7000).?;
    const unrelated_addr: usize = 0x5000_0000;

    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(unrelated_addr, null, bounds));
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(unrelated_addr, 0x8000, bounds));
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(unrelated_addr, 0x9000, bounds));
    try std.testing.expectEqual(FaultKind.stack_overflow, classifyFault(0x6800, null, bounds));
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(0x5000, null, bounds));
    try std.testing.expectEqual(FaultKind.stack_overflow, classifyFault(0x6800, 0x8000, bounds));
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(unrelated_addr, null, null));
}

test "classifyFault treats a lower stack boundary fault as overflow" {
    const bounds = StackBounds.init(0x7000, 0x9000, 0x1000, null, null).?;

    try std.testing.expectEqual(FaultKind.stack_overflow, classifyFault(0x6ff0, null, bounds));
    try std.testing.expectEqual(FaultKind.stack_overflow, classifyFault(0x6000, null, bounds));
    try std.testing.expectEqual(FaultKind.stack_overflow, classifyFault(0x6ff0, 0x8000, bounds));
    try std.testing.expectEqual(FaultKind.stack_overflow, classifyFault(0x1000_1000, 0x6ff0, bounds));
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(0x5ff0, null, bounds));
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(0x1000_1000, 0x8000, bounds));
}

test "classifyFault reports a null/wild write as access violation, not overflow" {
    // Regression: a JIT'd eval object that stores through an unrelocated GOT
    // slot writes to address 0 while the stack pointer is healthy and deep
    // inside the stack. That must be reported as an access violation, not
    // misclassified as a stack overflow (which previously sent a one-line
    // relocation bug down a multi-day stack-overflow investigation).
    const bounds = StackBounds.init(0x7000_0000, 0x7080_0000, 0x1000, 0x6fff_f000, 0x7000_0000).?;
    const healthy_sp: usize = 0x7040_0000;

    // Null write.
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(0, healthy_sp, bounds));
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(0, null, bounds));
    // Arbitrary wild write far from the stack.
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(0xdead_0000, healthy_sp, bounds));
    // A genuine overflow (fault just below the stack) still classifies as overflow.
    try std.testing.expectEqual(FaultKind.stack_overflow, classifyFault(0x6fff_f800, healthy_sp, bounds));
}

test "classifyFault: a stack pointer below reported bounds is not overflow on its own" {
    // When the reported bounds are unreliable (a real hazard: pthread_getattr_np
    // on a static-musl main thread reports a stack the real sp isn't in), a
    // "stack pointer below low" must not be treated as overflow by itself — the
    // fault has to corroborate by being near the stack pointer.
    const bounds = StackBounds.init(0x7000, 0x9000, 0x1000, null, null).?;

    // Fault far from the (below-bounds) sp: a null/wild write, not an overflow.
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(0x5000_0000, 0x5000, bounds));
    if (comptime @bitSizeOf(usize) >= 64) {
        try std.testing.expectEqual(FaultKind.access_violation, classifyFault(0xffff_fd1f_fea0, 0x5000, bounds));
    }
    try std.testing.expectEqual(FaultKind.access_violation, classifyFault(0x5000_0000, 0x9000, bounds));
    // Fault adjacent to the sp: a genuine overflow, even with untrustworthy bounds.
    try std.testing.expectEqual(FaultKind.stack_overflow, classifyFault(0x4040, 0x5000, bounds));
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
            fn callback(_: usize, _: AccessViolationContext) noreturn {
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
