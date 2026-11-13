//! C-runtime default app support for non-Linux native targets.
//!
//! The dev backend emits symbol-ABI calls for compiled executables. This object
//! provides those symbols for the synthetic default platform on targets whose
//! process entrypoint is the C runtime's `main`.

const std = @import("std");
const builtin = @import("builtin");
const default_platform_options = @import("default_platform_options");

const memory_fault = @import("memory_fault");
const RocStr = @import("roc_str_view").RocStr;
const RocList = @import("roc_str_view").RocList;
const roc_args = @import("roc_args");
const shim_symbols = @import("shim_symbols");
const runtime_shared = @import("runtime_shared.zig");
const SourceFrame = runtime_shared.SourceFrame;
const normalizedAlignment = runtime_shared.normalizedAlignment;
const alignForward = runtime_shared.alignForward;

const c = switch (builtin.os.tag) {
    .windows => struct {
        extern fn malloc(size: usize) ?*anyopaque;
        extern fn free(ptr: ?*anyopaque) void;
        extern fn exit(code: i32) noreturn;
        extern fn _write(fd: i32, buf: [*]const u8, len: u32) i32;
        extern fn _setmode(fd: i32, mode: i32) i32;

        /// `_setmode` flag that writes bytes as given, with no `\n` to
        /// `\r\n` translation.
        const O_BINARY: i32 = 0x8000;

        fn write(fd: i32, buf: [*]const u8, len: usize) isize {
            const chunk_len: u32 = @intCast(@min(len, 0x7fff_ffff));
            return _write(fd, buf, chunk_len);
        }
    },
    .freestanding,
    .other,
    .contiki,
    .fuchsia,
    .hermit,
    .managarm,
    .haiku,
    .hurd,
    .illumos,
    .plan9,
    .rtems,
    .serenity,
    .dragonfly,
    .freebsd,
    .openbsd,
    .netbsd,
    .linux,
    .macos,
    .driverkit,
    .ios,
    .maccatalyst,
    .tvos,
    .visionos,
    .watchos,
    .uefi,
    .@"3ds",
    .wiiu,
    .@"switch",
    .gba,
    .psx,
    .tios,
    .ashetos,
    .ps3,
    .ps4,
    .ps5,
    .psp,
    .vita,
    .emscripten,
    .wasi,
    .amdhsa,
    .amdpal,
    .cuda,
    .mesa3d,
    .nvcl,
    .opencl,
    .opengl,
    .vulkan,
    => struct {
        extern fn malloc(size: usize) ?*anyopaque;
        extern fn free(ptr: ?*anyopaque) void;
        extern fn exit(code: i32) noreturn;
        extern fn write(fd: i32, buf: [*]const u8, len: usize) isize;
    },
};

const AllocationHeader = extern struct {
    raw: [*]u8,
    len: usize,
};

const windows = if (builtin.os.tag == .windows) struct {
    extern "kernel32" fn GetCommandLineW() callconv(.winapi) [*:0]u16;
    extern "shell32" fn CommandLineToArgvW(
        command_line: [*:0]const u16,
        argc: *c_int,
    ) callconv(.winapi) ?[*][*:0]u16;
    extern "kernel32" fn LocalFree(memory: ?*anyopaque) callconv(.winapi) ?*anyopaque;
} else struct {};

comptime {
    @export(&runtimeInit, .{ .name = shim_symbols.roc_default_runtime_init });
    @export(&defaultExit, .{ .name = shim_symbols.roc_default_exit });
    @export(&defaultEchoLine, .{ .name = shim_symbols.roc_default_echo_line });
    @export(&rocDbg, .{ .name = shim_symbols.roc_dbg });
    @export(&rocExpectFailed, .{ .name = shim_symbols.roc_expect_failed });
    @export(&rocCrashed, .{ .name = shim_symbols.roc_crashed });
    @export(&rocDefaultCrashedWithFrames, .{ .name = shim_symbols.roc_default_crashed_with_frames });
    @export(&rocAlloc, .{ .name = shim_symbols.roc_alloc });
    @export(&rocRealloc, .{ .name = shim_symbols.roc_realloc });
    @export(&rocDealloc, .{ .name = shim_symbols.roc_dealloc });
    if (default_platform_options.include_process_entrypoint) {
        @export(&cMain, .{ .name = "main" });
    }
}

/// Set when an inline `expect` fails. A failed inline expect reports and lets
/// the program continue; the process exit turns an otherwise-successful status
/// into 1, matching the interpreter's default-app behavior.
var inline_expect_failed: bool = false;

/// The Roc entrypoint the synthetic default platform exports.
const roc_default_start_main: *const fn (RocList) callconv(.c) i32 =
    @extern(*const fn (RocList) callconv(.c) i32, .{ .name = shim_symbols.roc_default_start_main });

/// The C runtime owns the process entrypoint: it initializes the Roc runtime,
/// runs the Roc entrypoint, and folds failed inline expects into the status.
fn cMain(argc: c_int, argv: [*][*:0]u8) callconv(.c) c_int {
    runtimeInit();
    const args = if (comptime builtin.os.tag == .windows)
        windowsArgs() orelse {
            writeAll(2, "Unable to read command-line arguments\n");
            return 1;
        }
    else
        roc_args.fromPosixArgv(@intCast(@max(argc, 0)), argv, &rocAlloc) orelse {
            writeAll(2, "Unable to allocate command-line arguments\n");
            return 1;
        };
    const status = roc_default_start_main(args);
    if (status == 0 and inline_expect_failed) return 1;
    return status;
}

fn windowsArgs() ?RocList {
    var argc: c_int = 0;
    const argv = windows.CommandLineToArgvW(windows.GetCommandLineW(), &argc) orelse return null;
    defer _ = windows.LocalFree(@ptrCast(argv));
    return roc_args.fromWindowsArgv(@intCast(@max(argc, 0)), argv, &rocAlloc);
}

/// The CRT opens stdout and stderr in text mode, which turns every `\n` a
/// program writes into `\r\n`. A Roc program's output is exactly the bytes it
/// writes, so both are switched to binary mode before any Roc code runs.
fn runtimeInit() callconv(.c) void {
    if (comptime builtin.os.tag == .windows) {
        _ = c._setmode(1, c.O_BINARY);
        _ = c._setmode(2, c.O_BINARY);
    }
    stack_overflow_reporting.install();
}

/// Reports a stack overflow on stderr and exits with status 1. Every other
/// fatal fault keeps the operating system's own termination.
const stack_overflow_reporting = if (builtin.os.tag.isDarwin())
    darwin_stack_overflow
else if (builtin.os.tag == .windows)
    windows_stack_overflow
else
    struct {
        fn install() void {}
    };

/// Darwin delivers a stack overflow as `SIGBUS` or `SIGSEGV` at an address in
/// the guard region below the stack. Both signals are classified with
/// `memory_fault.classifyFault`, the function the compiler's own crash handler
/// uses, from the fault address, the interrupted stack pointer and the main
/// thread's stack bounds, which is the thread Roc code runs on.
const darwin_stack_overflow = struct {
    const alt_stack_size: usize = 64 * 1024;

    /// The handler's own stack: the overflowed stack has no room left for it.
    var alt_stack: [alt_stack_size]u8 align(16) = undefined;
    var main_stack_bounds: ?memory_fault.StackBounds = null;

    const Thread = *opaque {};
    extern fn pthread_self() Thread;
    extern fn pthread_get_stackaddr_np(thread: Thread) usize;
    extern fn pthread_get_stacksize_np(thread: Thread) usize;
    extern fn getpagesize() c_int;
    extern fn sigaltstack(stack: ?*const std.c.stack_t, old_stack: ?*std.c.stack_t) c_int;
    extern fn sigaction(sig: std.c.SIG, action: ?*const std.c.Sigaction, old_action: ?*std.c.Sigaction) c_int;
    extern fn raise(sig: std.c.SIG) c_int;
    extern fn _exit(code: c_int) noreturn;

    /// The prefix of Darwin's `ucontext_t` up to `uc_mcontext` (`_ucontext.h`).
    const Ucontext = extern struct {
        onstack: i32,
        sigmask: std.c.sigset_t,
        stack: std.c.stack_t,
        link: ?*anyopaque,
        mcsize: u64,
        mcontext: *const Mcontext,
    };

    /// The prefix of Darwin's `mcontext` up to the stack pointer
    /// (`_mcontext.h`).
    const Mcontext = if (builtin.cpu.arch == .aarch64)
        extern struct {
            far: u64 align(16),
            esr: u64,
            x: [30]u64,
            lr: u64,
            sp: u64,
        }
    else if (builtin.cpu.arch == .x86_64)
        extern struct {
            trapno: u16,
            cpu: u16,
            err: u32,
            faultvaddr: u64,
            rax: u64,
            rbx: u64,
            rcx: u64,
            rdx: u64,
            rdi: u64,
            rsi: u64,
            rbp: u64,
            sp: u64,
        }
    else
        @compileError("the Darwin default platform runtime supports only aarch64 and x86_64");

    fn install() void {
        const thread = pthread_self();
        const stack_high = pthread_get_stackaddr_np(thread);
        const stack_size = pthread_get_stacksize_np(thread);
        main_stack_bounds = memory_fault.StackBounds.init(stack_high - stack_size, stack_high, @intCast(getpagesize()), null, null);

        const stack: std.c.stack_t = .{ .sp = &alt_stack, .size = @intCast(alt_stack_size), .flags = 0 };
        if (sigaltstack(&stack, null) != 0) {
            writeAll(2, "Roc runtime failed to install signal stack\n");
            _exit(1);
        }
        installHandler(.SEGV);
        installHandler(.BUS);
    }

    fn installHandler(sig: std.c.SIG) void {
        const action: std.c.Sigaction = .{
            .handler = .{ .sigaction = handleFault },
            .mask = 0,
            .flags = std.c.SA.SIGINFO | std.c.SA.ONSTACK,
        };
        if (sigaction(sig, &action, null) != 0) {
            writeAll(2, "Roc runtime failed to install signal handler\n");
            _exit(1);
        }
    }

    fn handleFault(sig: std.c.SIG, info: *const std.c.siginfo_t, context: ?*anyopaque) callconv(.c) void {
        const stack_pointer: ?usize = if (context) |raw| blk: {
            const ucontext: *const Ucontext = @ptrCast(@alignCast(raw));
            break :blk @intCast(ucontext.mcontext.sp);
        } else null;
        switch (memory_fault.classifyFault(@intFromPtr(info.addr), stack_pointer, main_stack_bounds)) {
            .stack_overflow => {
                writeAll(2, runtime_shared.stack_overflow_message);
                _exit(1);
            },
            .access_violation => {
                // The signal stays blocked until this handler returns, so the
                // raised one is delivered then, under the default action.
                const default_action: std.c.Sigaction = .{ .handler = .{ .handler = null }, .mask = 0, .flags = 0 };
                _ = sigaction(sig, &default_action, null);
                _ = raise(sig);
            },
        }
    }
};

/// Windows raises `EXCEPTION_STACK_OVERFLOW` for exactly this fault, so the
/// exception code alone identifies it. The handler runs on what remains of the
/// overflowed stack, within the guarantee `SetThreadStackGuarantee` reserves.
const windows_stack_overflow = struct {
    const DWORD = u32;
    const EXCEPTION_STACK_OVERFLOW: DWORD = 0xC00000FD;
    const EXCEPTION_CONTINUE_SEARCH: i32 = 0;
    const STD_ERROR_HANDLE: DWORD = @bitCast(@as(i32, -12));
    const stack_guarantee: u32 = 32 * 1024;

    const ExceptionRecord = extern struct {
        code: DWORD,
        flags: DWORD,
        record: ?*ExceptionRecord,
        address: ?*anyopaque,
        parameter_count: DWORD,
        information: [15]usize,
    };

    const ExceptionPointers = extern struct {
        record: *ExceptionRecord,
        context: *anyopaque,
    };

    const Handler = *const fn (*ExceptionPointers) callconv(.winapi) i32;

    extern "kernel32" fn AddVectoredExceptionHandler(first: u32, handler: Handler) callconv(.winapi) ?*anyopaque;
    extern "kernel32" fn SetThreadStackGuarantee(size: *u32) callconv(.winapi) i32;
    extern "kernel32" fn GetStdHandle(handle: DWORD) callconv(.winapi) ?*anyopaque;
    extern "kernel32" fn WriteFile(file: ?*anyopaque, buffer: [*]const u8, len: DWORD, written: ?*DWORD, overlapped: ?*anyopaque) callconv(.winapi) i32;
    extern "kernel32" fn GetCurrentProcess() callconv(.winapi) *anyopaque;
    extern "kernel32" fn TerminateProcess(process: *anyopaque, exit_code: u32) callconv(.winapi) i32;

    fn install() void {
        var guarantee = stack_guarantee;
        _ = SetThreadStackGuarantee(&guarantee);
        _ = AddVectoredExceptionHandler(1, handleException);
    }

    fn handleException(pointers: *ExceptionPointers) callconv(.winapi) i32 {
        if (pointers.record.code != EXCEPTION_STACK_OVERFLOW) return EXCEPTION_CONTINUE_SEARCH;
        const message = runtime_shared.stack_overflow_message;
        var written: DWORD = 0;
        _ = WriteFile(GetStdHandle(STD_ERROR_HANDLE), message.ptr, message.len, &written, null);
        _ = TerminateProcess(GetCurrentProcess(), 1);
        @trap();
    }
};

fn defaultExit(code: u8) callconv(.c) noreturn {
    if (code == 0 and inline_expect_failed) c.exit(1);
    c.exit(code);
}

fn defaultEchoLine(str: RocStr) callconv(.c) void {
    var owned = str;
    const message = owned.asSlice();
    writeAll(1, message);
    owned.decref(rocDealloc);
}

fn rocDbg(bytes: [*]const u8, len: usize) callconv(.c) void {
    writeAll(2, "[dbg] ");
    writeAll(2, bytes[0..len]);
    writeAll(2, "\n");
}

fn rocExpectFailed(bytes: [*]const u8, len: usize) callconv(.c) void {
    inline_expect_failed = true;
    writeAll(2, "Expect failed: ");
    writeAll(2, bytes[0..len]);
    writeAll(2, "\n");
}

fn rocCrashed(bytes: [*]const u8, len: usize) callconv(.c) noreturn {
    writeAll(2, "Roc application crashed with this message:\n\n\t");
    writeAll(2, bytes[0..len]);
    writeAll(2, "\n\n");
    c.exit(1);
}

fn rocDefaultCrashedWithFrames(
    bytes: [*]const u8,
    len: usize,
    _: [*]const SourceFrame,
    _: usize,
) callconv(.c) noreturn {
    rocCrashed(bytes, len);
}

/// `roc_alloc` and `roc_realloc` never return null, so allocation failure
/// crashes the program instead.
fn outOfMemory() noreturn {
    const message = "out of memory";
    rocCrashed(message.ptr, message.len);
}

fn rocAlloc(length: usize, alignment: usize) callconv(.c) *anyopaque {
    const byte_alignment = normalizedAlignment(alignment);
    const total = length + byte_alignment + @sizeOf(AllocationHeader);
    const raw_any = c.malloc(total) orelse outOfMemory();
    const raw: [*]u8 = @ptrCast(raw_any);
    const user_addr = alignForward(@intFromPtr(raw) + @sizeOf(AllocationHeader), byte_alignment);
    const user: [*]u8 = @ptrFromInt(user_addr);
    allocationHeader(user).* = .{ .raw = raw, .len = length };
    return @ptrCast(user);
}

fn rocRealloc(ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) *anyopaque {
    const old_user: [*]u8 = @ptrCast(ptr);
    const old_header = allocationHeader(old_user).*;
    const new_ptr = rocAlloc(new_length, alignment);
    const new_user: [*]u8 = @ptrCast(new_ptr);

    const copy_len = @min(old_header.len, new_length);
    var i: usize = 0;
    while (i < copy_len) : (i += 1) {
        new_user[i] = old_user[i];
    }
    rocDealloc(ptr, alignment);
    return new_ptr;
}

fn rocDealloc(ptr: *anyopaque, _: usize) callconv(.c) void {
    const user: [*]u8 = @ptrCast(ptr);
    c.free(@ptrCast(allocationHeader(user).raw));
}

fn allocationHeader(user: [*]u8) *AllocationHeader {
    return @ptrCast(@alignCast(user - @sizeOf(AllocationHeader)));
}

fn writeAll(fd: i32, bytes: []const u8) void {
    var remaining = bytes;
    while (remaining.len != 0) {
        const written = c.write(fd, remaining.ptr, remaining.len);
        if (written <= 0) return;
        remaining = remaining[@intCast(written)..];
    }
}
