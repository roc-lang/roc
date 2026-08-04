//! Freestanding FreeBSD and NetBSD runtime for the build-only default app platform.
//!
//! The runtime owns process startup and reaches the kernel directly, so default
//! apps for these targets can be cross-linked without a BSD libc or sysroot.

const std = @import("std");
const builtin = @import("builtin");
const default_platform_options = @import("default_platform_options");

const RocStr = @import("roc_str_view").RocStr;
const RocList = @import("roc_str_view").RocList;
const roc_args = @import("roc_args");
const shim_symbols = @import("shim_symbols");

pub const panic = std.debug.no_panic;

const stdout_fd: usize = 1;
const stderr_fd: usize = 2;
const page_size: usize = 4096;
const allocation_header_words = 3;
const allocation_header_size = allocation_header_words * @sizeOf(usize);

const syscall_number = switch (builtin.os.tag) {
    .freebsd => struct {
        const exit: usize = 1;
        const write: usize = 4;
        const munmap: usize = 73;
        const mmap: usize = 477;
    },
    .netbsd => struct {
        const exit: usize = 1;
        const write: usize = 4;
        const munmap: usize = 73;
        const mmap: usize = 197;
    },
    else => @compileError("default platform BSD runtime must be built for FreeBSD or NetBSD"),
};

const SourceFrame = extern struct {
    name_ptr: [*]const u8,
    name_len: usize,
    file_ptr: [*]const u8,
    file_len: usize,
    line: u32,
    column: u32,
};

const roc_default_start_main: *const fn (RocList) callconv(.c) i32 =
    @extern(*const fn (RocList) callconv(.c) i32, .{ .name = shim_symbols.roc_default_start_main });

comptime {
    @export(&defaultMemcpy, .{ .name = "memcpy", .linkage = .weak });
    @export(&defaultMemmove, .{ .name = "memmove", .linkage = .weak });
    @export(&defaultMemset, .{ .name = "memset", .linkage = .weak });
    @export(&defaultTrunc, .{ .name = "trunc", .linkage = .weak });

    @export(&runtimeInit, .{ .name = shim_symbols.roc_default_runtime_init });
    @export(&rocDbg, .{ .name = shim_symbols.roc_dbg });
    @export(&rocExpectFailed, .{ .name = shim_symbols.roc_expect_failed });
    @export(&rocCrashed, .{ .name = shim_symbols.roc_crashed });
    @export(&rocDefaultCrashedWithFrames, .{ .name = shim_symbols.roc_default_crashed_with_frames });
    @export(&defaultEchoLine, .{ .name = shim_symbols.roc_default_echo_line });
    @export(&defaultExit, .{ .name = shim_symbols.roc_default_exit });
    @export(&rocAlloc, .{ .name = shim_symbols.roc_alloc });
    @export(&rocRealloc, .{ .name = shim_symbols.roc_realloc });
    @export(&rocDealloc, .{ .name = shim_symbols.roc_dealloc });

    if (default_platform_options.include_process_entrypoint) {
        if (builtin.cpu.arch != .x86_64) {
            @compileError("unsupported default-platform BSD architecture");
        }
        @export(&bsdStartX86_64, .{ .name = "_start" });
        @export(&bsdStartMain, .{ .name = "roc_default_bsd_start_main", .visibility = .hidden });
    }

    switch (builtin.os.tag) {
        .freebsd => asm (
            \\.section .note.tag,"a",@note
            \\.p2align 2
            \\.long 8
            \\.long 4
            \\.long 1
            \\.asciz "FreeBSD"
            \\.p2align 2
            \\.long 1200000
            \\.section .note.GNU-stack,"",@progbits
        ),
        .netbsd => asm (
            \\.section .note.netbsd.ident,"a",@note
            \\.p2align 2
            \\.long 7
            \\.long 4
            \\.long 1
            \\.ascii "NetBSD"
            \\.byte 0, 0
            \\.long 800000000
            \\.text
            \\.globl roc_default_netbsd_mmap
            \\.hidden roc_default_netbsd_mmap
            \\.type roc_default_netbsd_mmap,@function
            \\roc_default_netbsd_mmap:
            \\movq %rcx, %r10
            \\movq $197, %rax
            \\syscall
            \\jnc 1f
            \\negq %rax
            \\1: retq
            \\.size roc_default_netbsd_mmap, .-roc_default_netbsd_mmap
            \\.section .note.GNU-stack,"",@progbits
        ),
        else => unreachable,
    }
}

/// Set when an inline `expect` fails. A failed inline expect reports and lets
/// the program continue; process exit turns an otherwise-successful status
/// into 1, matching the other default-platform runtimes.
var inline_expect_failed: bool = false;

fn bsdStartMain(argc: usize, argv: [*][*:0]u8) callconv(.c) noreturn {
    runtimeInit();
    const args = roc_args.fromPosixArgv(argc, argv, &rocAlloc) orelse {
        writeLiteral(stderr_fd, "Unable to allocate command-line arguments\n");
        exitFailure();
    };
    const status = roc_default_start_main(args);
    if (status == 0 and inline_expect_failed) rawExit(1);
    rawExit(@intCast(status));
}

fn bsdStartX86_64() callconv(.naked) noreturn {
    asm volatile (
        \\movq %%rsp, %%rbx
        \\andq $-16, %%rsp
        \\movq (%%rbx), %%rdi
        \\leaq 8(%%rbx), %%rsi
        \\call roc_default_bsd_start_main
        \\ud2
    );
}

fn runtimeInit() callconv(.c) void {}

fn rocDbg(bytes: [*]const u8, len: usize) callconv(.c) void {
    writeLiteral(stderr_fd, "[dbg] ");
    writeAll(stderr_fd, bytes[0..len]);
    writeLiteral(stderr_fd, "\n");
}

fn rocExpectFailed(bytes: [*]const u8, len: usize) callconv(.c) void {
    inline_expect_failed = true;
    writeLiteral(stderr_fd, "Expect failed: ");
    writeAll(stderr_fd, bytes[0..len]);
    writeLiteral(stderr_fd, "\n");
}

fn rocCrashed(bytes: [*]const u8, len: usize) callconv(.c) noreturn {
    writeCrashMessage(bytes[0..len]);
    exitFailure();
}

fn rocDefaultCrashedWithFrames(
    bytes: [*]const u8,
    len: usize,
    source_frames: [*]const SourceFrame,
    source_frame_count: usize,
) callconv(.c) noreturn {
    writeCrashMessage(bytes[0..len]);
    if (source_frame_count != 0) {
        writeLiteral(stderr_fd, "Backtrace:\n");
        for (source_frames[0..source_frame_count]) |frame| {
            writeLiteral(stderr_fd, "  ");
            writeAll(stderr_fd, frame.name_ptr[0..frame.name_len]);
            if (frame.file_len != 0) {
                writeLiteral(stderr_fd, " ");
                writeAll(stderr_fd, frame.file_ptr[0..frame.file_len]);
                if (frame.line != 0) {
                    writeLiteral(stderr_fd, ":");
                    writeUnsigned(stderr_fd, frame.line);
                    if (frame.column != 0) {
                        writeLiteral(stderr_fd, ":");
                        writeUnsigned(stderr_fd, frame.column);
                    }
                }
            }
            writeLiteral(stderr_fd, "\n");
        }
    }
    exitFailure();
}

fn writeCrashMessage(bytes: []const u8) void {
    writeLiteral(stderr_fd, "Roc application crashed with this message:\n\n\t");
    writeAll(stderr_fd, bytes);
    writeLiteral(stderr_fd, "\n\n");
}

fn defaultEchoLine(str: RocStr) callconv(.c) void {
    var owned = str;
    writeAll(stdout_fd, owned.asSlice());
    owned.decref(rocDealloc);
}

fn defaultExit(code: u8) callconv(.c) noreturn {
    if (code == 0 and inline_expect_failed) rawExit(1);
    rawExit(code);
}

fn rocAlloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const byte_alignment = normalizedAlignment(alignment);
    const prefix = alignForward(allocation_header_size, byte_alignment);
    const total = pageAlign(prefix + length);
    const raw_addr = rawMmap(total);
    if (isSyscallError(raw_addr)) return null;

    const raw: [*]u8 = @ptrFromInt(raw_addr);
    const user = raw + prefix;
    storeAllocationHeader(user, prefix, total, length);
    return @ptrCast(user);
}

fn rocRealloc(ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const old_user: [*]u8 = @ptrCast(ptr);
    const old_len = allocationHeaderValue(old_user, 2);
    const new_ptr = rocAlloc(new_length, alignment) orelse return null;
    const new_user: [*]u8 = @ptrCast(new_ptr);

    const copy_len = @min(old_len, new_length);
    var i: usize = 0;
    while (i < copy_len) : (i += 1) new_user[i] = old_user[i];
    rocDealloc(ptr, alignment);
    return new_ptr;
}

fn rocDealloc(ptr: *anyopaque, _: usize) callconv(.c) void {
    const user: [*]u8 = @ptrCast(ptr);
    const prefix = allocationHeaderValue(user, 0);
    const total = allocationHeaderValue(user, 1);
    _ = rawSyscall2(syscall_number.munmap, @intFromPtr(user - prefix), total);
}

fn rawMmap(length: usize) usize {
    const map_private_anonymous: usize = 0x0002 | 0x1000;
    const fd: usize = @bitCast(@as(isize, -1));
    return switch (builtin.os.tag) {
        .freebsd => rawSyscall6(syscall_number.mmap, 0, length, 0x01 | 0x02, map_private_anonymous, fd, 0),
        .netbsd => rawNetBsdMmap(length, map_private_anonymous, fd),
        else => unreachable,
    };
}

/// NetBSD's mmap ABI has an explicit padding slot before its seventh `off_t`
/// argument. The assembly symbol uses the C ABI so that seventh slot is at
/// `rsp + 8`, exactly where the x86_64 kernel reads syscall arguments beyond
/// the sixth.
fn rawNetBsdMmap(length: usize, flags: usize, fd: usize) usize {
    const mmap_fn = @extern(
        *const fn (usize, usize, usize, usize, usize, usize, usize) callconv(.c) usize,
        .{ .name = "roc_default_netbsd_mmap" },
    );
    return mmap_fn(0, length, 0x01 | 0x02, flags, fd, 0, 0);
}

fn rawSyscall1(number: usize, arg1: usize) usize {
    return asm volatile (
        \\syscall
        \\jnc 1f
        \\negq %%rax
        \\1:
        : [ret] "={rax}" (-> usize),
        : [number] "{rax}" (number),
          [arg1] "{rdi}" (arg1),
        : .{ .rcx = true, .r11 = true, .memory = true });
}

fn rawSyscall2(number: usize, arg1: usize, arg2: usize) usize {
    return asm volatile (
        \\syscall
        \\jnc 1f
        \\negq %%rax
        \\1:
        : [ret] "={rax}" (-> usize),
        : [number] "{rax}" (number),
          [arg1] "{rdi}" (arg1),
          [arg2] "{rsi}" (arg2),
        : .{ .rcx = true, .r11 = true, .memory = true });
}

fn rawSyscall3(number: usize, arg1: usize, arg2: usize, arg3: usize) usize {
    return asm volatile (
        \\syscall
        \\jnc 1f
        \\negq %%rax
        \\1:
        : [ret] "={rax}" (-> usize),
        : [number] "{rax}" (number),
          [arg1] "{rdi}" (arg1),
          [arg2] "{rsi}" (arg2),
          [arg3] "{rdx}" (arg3),
        : .{ .rcx = true, .r11 = true, .memory = true });
}

fn rawSyscall6(
    number: usize,
    arg1: usize,
    arg2: usize,
    arg3: usize,
    arg4: usize,
    arg5: usize,
    arg6: usize,
) usize {
    return asm volatile (
        \\syscall
        \\jnc 1f
        \\negq %%rax
        \\1:
        : [ret] "={rax}" (-> usize),
        : [number] "{rax}" (number),
          [arg1] "{rdi}" (arg1),
          [arg2] "{rsi}" (arg2),
          [arg3] "{rdx}" (arg3),
          [arg4] "{r10}" (arg4),
          [arg5] "{r8}" (arg5),
          [arg6] "{r9}" (arg6),
        : .{ .rcx = true, .r11 = true, .memory = true });
}

fn rawExit(code: usize) noreturn {
    _ = rawSyscall1(syscall_number.exit, code);
    unreachable;
}

fn isSyscallError(value: usize) bool {
    return @as(isize, @bitCast(value)) < 0;
}

fn writeLiteral(fd: usize, comptime text: []const u8) void {
    writeAll(fd, text);
}

fn writeAll(fd: usize, bytes: []const u8) void {
    var remaining = bytes;
    while (remaining.len != 0) {
        const result = rawSyscall3(syscall_number.write, fd, @intFromPtr(remaining.ptr), remaining.len);
        if (isSyscallError(result) or result == 0) return;
        remaining = remaining[result..];
    }
}

fn writeUnsigned(fd: usize, value: anytype) void {
    var buf: [20]u8 = undefined;
    var index = buf.len;
    var n: u64 = @intCast(value);
    if (n == 0) {
        writeLiteral(fd, "0");
        return;
    }
    while (n != 0) {
        index -= 1;
        buf[index] = '0' + @as(u8, @intCast(n % 10));
        n /= 10;
    }
    writeAll(fd, buf[index..]);
}

fn exitFailure() noreturn {
    rawExit(1);
}

fn storeAllocationHeader(user: [*]u8, prefix: usize, total: usize, length: usize) void {
    allocationHeaderPtr(user, 0).* = prefix;
    allocationHeaderPtr(user, 1).* = total;
    allocationHeaderPtr(user, 2).* = length;
}

fn allocationHeaderValue(user: [*]u8, index: usize) usize {
    return allocationHeaderPtr(user, index).*;
}

fn allocationHeaderPtr(user: [*]u8, index: usize) *usize {
    const byte_offset = (allocation_header_words - index) * @sizeOf(usize);
    return @ptrCast(@alignCast(user - byte_offset));
}

fn normalizedAlignment(alignment: usize) usize {
    return @max(alignment, @alignOf(usize));
}

fn alignForward(value: usize, alignment: usize) usize {
    return (value + alignment - 1) & ~(alignment - 1);
}

fn pageAlign(value: usize) usize {
    return alignForward(value, page_size);
}

fn defaultMemcpy(dest: [*]u8, src: [*]const u8, len: usize) callconv(.c) [*]u8 {
    var i: usize = 0;
    while (i < len) : (i += 1) dest[i] = src[i];
    return dest;
}

fn defaultMemmove(dest: [*]u8, src: [*]const u8, len: usize) callconv(.c) [*]u8 {
    if (@intFromPtr(dest) <= @intFromPtr(src)) {
        var i: usize = 0;
        while (i < len) : (i += 1) dest[i] = src[i];
    } else {
        var i = len;
        while (i != 0) {
            i -= 1;
            dest[i] = src[i];
        }
    }
    return dest;
}

fn defaultMemset(dest: [*]u8, value: c_int, len: usize) callconv(.c) [*]u8 {
    const byte: u8 = @bitCast(@as(i8, @truncate(value)));
    const volatile_dest: [*]volatile u8 = dest;
    var i: usize = 0;
    while (i < len) : (i += 1) volatile_dest[i] = byte;
    return dest;
}

fn defaultTrunc(value: f64) callconv(.c) f64 {
    const bits: u64 = @bitCast(value);
    const exponent_bits = (bits >> 52) & 0x7ff;
    const exponent: i32 = @as(i32, @intCast(exponent_bits)) - 1023;

    if (exponent >= 52) return value;
    if (exponent < 0) return @bitCast(bits & (@as(u64, 1) << 63));

    const fraction_bits: u6 = @intCast(52 - exponent);
    const fraction_mask = (@as(u64, 1) << fraction_bits) - 1;
    return @bitCast(bits & ~fraction_mask);
}
