//! The stack the compiler runs compiled Roc code on in its own process.
//!
//! Compiled code that the compiler executes in-process (compile-time roots,
//! test roots, REPL and glue evaluation) runs on a stack of its own instead of
//! the stack of whichever thread picked the work up. Each thread maps one such
//! stack the first time it runs compiled code and reuses it for every later
//! run until the thread exits. Every run starts at the same page-aligned top,
//! the same distance above the stack's guard, with no compiler frames beneath
//! the compiled code's entry, so how deep a program recurses before it
//! overflows, and the instruction that overflows, depend only on the program
//! and the compiler build.
//!
//! POSIX targets reserve the stack with one lazily committed anonymous mapping
//! whose lowest `guard_size` bytes are inaccessible, and switch the stack
//! pointer to its top for the duration of a run. While a run is on it, the
//! fault handler classifies the thread's faults against its bounds
//! (`signal_handler.swapCurrentThreadStackBounds`).
//!
//! Windows runs on a fiber instead. The kernel's stack growth, its guard-page
//! and `EXCEPTION_STACK_OVERFLOW` handling, `_resetstkoflw`, and the system
//! unwinder all read the stack's extent from the thread's TEB
//! (`StackBase`, `StackLimit`, `DeallocationStack`, `GuaranteedStackBytes`),
//! and `SwitchToFiber` is the documented operation that switches those fields
//! together with the stack pointer and the fiber-local storage that the C
//! runtime keeps its per-thread state in. A fiber's stack is reserved and
//! committed on demand like a thread's.

const std = @import("std");
const builtin = @import("builtin");
const memory_fault = @import("memory_fault.zig");
const signal_handler = @import("signal_handler.zig");
const stack_budget = @import("stack_budget.zig");
const invariant = @import("invariant.zig").invariant;

/// The usable size of an evaluation stack: the stack budget every thread that
/// runs Roc code gets.
pub const size: usize = stack_budget.roc_stack_size;

/// The inaccessible region below a POSIX evaluation stack. It is wide enough
/// that a frame larger than a page still lands in it rather than in whatever
/// mapping lies below.
pub const guard_size: usize = 1024 * 1024;

/// A failure to set up the calling thread's evaluation stack.
pub const Error = error{OutOfMemory};

/// The entry a run calls on the evaluation stack.
pub const Entry = *const fn (context: *anyopaque) callconv(.c) void;

const switches_stack_pointer = builtin.cpu.arch == .x86_64 or builtin.cpu.arch == .aarch64;

const os_kind: enum { posix, windows, unsupported } = if ((builtin.os.tag == .linux or builtin.os.tag == .macos) and switches_stack_pointer)
    .posix
else if (builtin.os.tag == .windows)
    .windows
else
    .unsupported;

/// Whether this target runs compiled code on an evaluation stack.
pub const supported = os_kind != .unsupported;

threadlocal var running = false;

/// Run `body(context)` on the calling thread's evaluation stack and return
/// once it returns. `body` must return normally: a crash boundary that escapes
/// from code `body` calls must be entered inside `body`.
pub fn run(comptime Context: type, context: *Context, comptime body: fn (*Context) void) Error!void {
    const Adapter = struct {
        fn call(raw: *anyopaque) callconv(.c) void {
            body(@ptrCast(@alignCast(raw)));
        }
    };
    try runEntry(&Adapter.call, context);
}

/// Run `entry(context)` on the calling thread's evaluation stack.
pub fn runEntry(entry: Entry, context: *anyopaque) Error!void {
    if (running) invariant("compiled code entered the evaluation stack while already running on it", .{});
    running = true;
    defer running = false;
    switch (comptime os_kind) {
        .posix => try Posix.run(entry, context),
        .windows => try Windows.run(entry, context),
        .unsupported => entry(context),
    }
}

/// The calling thread's evaluation stack bounds, for tests.
pub fn currentThreadBoundsForTest() ?memory_fault.StackBounds {
    if (comptime os_kind != .posix) return null;
    const stack = Posix.thread_stack orelse return null;
    return stack.bounds();
}

const Posix = struct {
    const page_size_min = std.heap.page_size_min;

    const Stack = struct {
        mapping: [*]align(page_size_min) u8,

        fn top(self: Stack) usize {
            return @intFromPtr(self.mapping) + guard_size + size;
        }

        fn bounds(self: Stack) memory_fault.StackBounds {
            const low = @intFromPtr(self.mapping) + guard_size;
            return .{
                .low = low,
                .high = low + size,
                .page_size = std.heap.pageSize(),
                .guard_low = @intFromPtr(self.mapping),
                .guard_high = low,
            };
        }
    };

    threadlocal var thread_stack: ?Stack = null;

    /// The process-wide key whose destructor unmaps a thread's stack when the
    /// thread exits.
    var release_key: std.c.pthread_key_t = undefined;
    var release_key_state: std.atomic.Value(u8) = .init(0);
    var release_key_mutex: std.atomic.Mutex = .unlocked;

    fn run(entry: Entry, context: *anyopaque) Error!void {
        const stack = try currentThreadStack();
        const previous = signal_handler.swapCurrentThreadStackBounds(stack.bounds());
        defer _ = signal_handler.swapCurrentThreadStackBounds(previous);
        switchStack(context, entry, stack.top());
    }

    fn currentThreadStack() Error!Stack {
        if (thread_stack) |stack| return stack;
        const key = try releaseKey();
        const total = guard_size + size;
        const mapping = std.posix.mmap(
            null,
            total,
            .{ .READ = true, .WRITE = true },
            if (comptime builtin.os.tag == .linux)
                .{ .TYPE = .PRIVATE, .ANONYMOUS = true, .NORESERVE = true, .STACK = true }
            else
                .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
            -1,
            0,
        ) catch return error.OutOfMemory;
        errdefer std.posix.munmap(mapping);
        if (std.c.mprotect(@ptrCast(mapping.ptr), guard_size, .{}) != 0) return error.OutOfMemory;
        if (std.c.pthread_setspecific(key, @ptrCast(mapping.ptr)) != 0) return error.OutOfMemory;
        const stack: Stack = .{ .mapping = mapping.ptr };
        thread_stack = stack;
        return stack;
    }

    fn releaseKey() Error!std.c.pthread_key_t {
        if (release_key_state.load(.acquire) == 1) return release_key;
        while (!release_key_mutex.tryLock()) {}
        defer release_key_mutex.unlock();
        if (release_key_state.load(.acquire) == 0) {
            if (std.c.pthread_key_create(&release_key, &releaseThreadStack) != .SUCCESS) return error.OutOfMemory;
            release_key_state.store(1, .release);
        }
        return release_key;
    }

    fn releaseThreadStack(value: *anyopaque) callconv(.c) void {
        const mapping: [*]align(page_size_min) u8 = @ptrCast(@alignCast(value));
        std.posix.munmap(mapping[0 .. guard_size + size]);
        thread_stack = null;
    }

    /// Call `entry(context)` with the stack pointer at `top`, then restore the
    /// caller's stack pointer. The frame this sets up on the caller's stack
    /// links the frame-pointer chain back to the caller, so a debugger's
    /// backtrace continues across the switch; walks that must stay on the
    /// evaluation stack bound themselves by its frames.
    const switchStack: *const fn (context: *anyopaque, entry: Entry, top: usize) callconv(.c) void = @ptrCast(&switchStackImpl);

    fn switchStackImpl() callconv(.naked) void {
        if (comptime builtin.cpu.arch == .x86_64) {
            asm volatile (
                \\pushq %%rbp
                \\movq %%rsp, %%rbp
                \\pushq %%rbx
                \\movq %%rsp, %%rbx
                \\movq %%rdx, %%rsp
                \\callq *%%rsi
                \\movq %%rbx, %%rsp
                \\popq %%rbx
                \\popq %%rbp
                \\retq
            );
        } else if (comptime builtin.cpu.arch == .aarch64) {
            asm volatile (
                \\stp x29, x30, [sp, #-32]!
                \\mov x29, sp
                \\str x19, [sp, #16]
                \\mov x19, sp
                \\mov sp, x2
                \\blr x1
                \\mov sp, x19
                \\ldr x19, [sp, #16]
                \\ldp x29, x30, [sp], #32
                \\ret
            );
        } else {
            @compileError("the evaluation stack switch supports only x86_64 and aarch64");
        }
    }
};

const Windows = struct {
    const windows = std.os.windows;
    const DWORD = u32;
    const BOOL = i32;
    const FiberStart = *const fn (parameter: ?*anyopaque) callconv(.winapi) void;
    const FlsCallback = *const fn (value: ?*anyopaque) callconv(.winapi) void;

    const FIBER_FLAG_FLOAT_SWITCH: DWORD = 0x1;
    const FLS_OUT_OF_INDEXES: DWORD = 0xffff_ffff;

    extern "kernel32" fn ConvertThreadToFiberEx(parameter: ?*anyopaque, flags: DWORD) callconv(.winapi) ?*anyopaque;
    extern "kernel32" fn CreateFiberEx(commit_size: usize, reserve_size: usize, flags: DWORD, start: FiberStart, parameter: ?*anyopaque) callconv(.winapi) ?*anyopaque;
    extern "kernel32" fn SwitchToFiber(fiber: *anyopaque) callconv(.winapi) void;
    extern "kernel32" fn DeleteFiber(fiber: *anyopaque) callconv(.winapi) void;
    extern "kernel32" fn IsThreadAFiber() callconv(.winapi) BOOL;
    extern "kernel32" fn FlsAlloc(callback: ?FlsCallback) callconv(.winapi) DWORD;
    extern "kernel32" fn FlsSetValue(index: DWORD, value: ?*anyopaque) callconv(.winapi) BOOL;
    extern "kernel32" fn SetThreadStackGuarantee(size_in_bytes: *c_ulong) callconv(.winapi) BOOL;

    /// The commit a fiber starts with; the rest of its reserve is committed
    /// as it grows.
    const initial_commit: usize = 64 * 1024;

    /// The run the evaluation fiber performs next, and the fiber it switches
    /// back to when that run returns.
    const Request = struct {
        entry: Entry,
        context: *anyopaque,
        caller: *anyopaque,
    };

    threadlocal var thread_fiber: ?*anyopaque = null;
    threadlocal var request: ?Request = null;

    /// The process-wide fiber-local storage index whose callback deletes a
    /// thread's evaluation fiber when the thread exits.
    var release_index: DWORD = FLS_OUT_OF_INDEXES;
    var release_index_mutex: std.atomic.Mutex = .unlocked;

    fn run(entry: Entry, context: *anyopaque) Error!void {
        const fiber = try currentThreadFiber();
        request = .{ .entry = entry, .context = context, .caller = try callerFiber() };
        SwitchToFiber(fiber);
    }

    /// The fiber the calling thread is running, converting the thread into
    /// one the first time.
    fn callerFiber() Error!*anyopaque {
        if (IsThreadAFiber() != 0) return windows.teb().NtTib.DUMMYUNIONNAME.FiberData;
        return ConvertThreadToFiberEx(null, FIBER_FLAG_FLOAT_SWITCH) orelse error.OutOfMemory;
    }

    fn currentThreadFiber() Error!*anyopaque {
        if (thread_fiber) |fiber| return fiber;
        const index = try releaseIndex();
        const fiber = CreateFiberEx(initial_commit, size, FIBER_FLAG_FLOAT_SWITCH, &fiberMain, null) orelse
            return error.OutOfMemory;
        errdefer DeleteFiber(fiber);
        if (FlsSetValue(index, fiber) == 0) return error.OutOfMemory;
        thread_fiber = fiber;
        return fiber;
    }

    fn releaseIndex() Error!DWORD {
        while (!release_index_mutex.tryLock()) {}
        defer release_index_mutex.unlock();
        if (release_index == FLS_OUT_OF_INDEXES) {
            const index = FlsAlloc(&releaseThreadFiber);
            if (index == FLS_OUT_OF_INDEXES) return error.OutOfMemory;
            release_index = index;
        }
        return release_index;
    }

    fn releaseThreadFiber(value: ?*anyopaque) callconv(.winapi) void {
        DeleteFiber(value orelse return);
        thread_fiber = null;
    }

    /// The evaluation fiber's body: every run starts at the same depth of this
    /// loop, so every run starts at the same point of the fiber's stack.
    fn fiberMain(_: ?*anyopaque) callconv(.winapi) void {
        // The guarantee is the stack the overflow exception's dispatch runs
        // on; a fiber has its own, set from the fiber itself.
        var guarantee: c_ulong = signal_handler.windows_stack_guarantee;
        _ = SetThreadStackGuarantee(&guarantee);
        while (true) {
            const current = request orelse invariant("the evaluation fiber was switched to without a request", .{});
            request = null;
            current.entry(current.context);
            SwitchToFiber(current.caller);
        }
    }
};

test "runs on a stack whose top is the same for every run" {
    if (comptime !supported) return error.SkipZigTest;
    const Probe = struct {
        frame: usize = 0,
        fn body(self: *@This()) void {
            self.frame = @frameAddress();
        }
    };
    var first: Probe = .{};
    try run(Probe, &first, Probe.body);
    var second: Probe = .{};
    try run(Probe, &second, Probe.body);
    try std.testing.expect(first.frame != 0);
    try std.testing.expectEqual(first.frame, second.frame);
    try std.testing.expect(first.frame != @frameAddress());
    if (currentThreadBoundsForTest()) |bounds| {
        try std.testing.expect(first.frame > bounds.low and first.frame < bounds.high);
        try std.testing.expect(bounds.high - first.frame < std.heap.pageSize());
        try std.testing.expectEqual(@as(usize, 0), bounds.high % std.heap.pageSize());
    }
}

test "classifies a fault in the evaluation stack's guard as a stack overflow while it runs" {
    if (comptime os_kind != .posix) return error.SkipZigTest;
    const Probe = struct {
        bounds: ?memory_fault.StackBounds = null,
        fn body(self: *@This()) void {
            self.bounds = signal_handler.currentThreadStackBoundsForTest();
        }
    };
    var probe: Probe = .{};
    const before = signal_handler.currentThreadStackBoundsForTest();
    try run(Probe, &probe, Probe.body);
    const bounds = probe.bounds orelse return error.TestUnexpectedResult;
    const guard_low = bounds.guard_low orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(memory_fault.FaultKind.stack_overflow, memory_fault.classifyFault(guard_low + 8, null, bounds));
    try std.testing.expectEqual(currentThreadBoundsForTest().?.low, bounds.low);
    try std.testing.expectEqual(before, signal_handler.currentThreadStackBoundsForTest());
}
