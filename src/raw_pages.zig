//! Anonymous page mapping for the objects linked into standalone Roc programs.
//!
//! The default-platform runtimes and the boxy runtime both need read-write
//! pages straight from the kernel, and both are linked into executables that
//! carry no Zig standard library. On the systems whose default platform owns
//! the process entrypoint (Linux and the BSDs) those executables link with
//! `-nostdlib`, so `std.heap.page_allocator` is unavailable: its posix path
//! reaches `mmap` through libc and pulls in the errno accessor. Those systems
//! issue the syscall directly here. macOS and Windows reach their kernels only
//! through libSystem and kernel32—a direct syscall instruction raises `SIGSYS`
//! on macOS—and every executable on those systems already links the library
//! that provides them, so they call it.
//!
//! Both mappings are shared rather than restated per consumer because a NetBSD
//! program links the default-platform runtime and the boxy runtime together,
//! and the assembly thunk NetBSD's `mmap` ABI needs may only be defined once.

const std = @import("std");
const builtin = @import("builtin");

const Os = enum { linux, freebsd, netbsd, macos, windows, libc };

const os: Os = switch (builtin.os.tag) {
    .linux => .linux,
    .freebsd => .freebsd,
    .netbsd => .netbsd,
    .macos => .macos,
    .windows => .windows,
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
    .openbsd,
    .driverkit,
    .ios,
    .maccatalyst,
    .tvos,
    .visionos,
    .watchos,
    .uefi,
    .@"3ds",
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
    => .libc,
};

/// Map `len` bytes of zeroed, private, read-write memory, or null on failure.
/// The returned region is page-aligned. `len` need not be a multiple of the
/// page size; the kernel rounds it up.
pub fn map(len: usize) ?[*]u8 {
    if (len == 0) return null;
    return switch (os) {
        .linux => linuxMap(len),
        .freebsd, .netbsd => bsdMap(len),
        .macos, .libc => libcMap(len),
        .windows => windowsMap(len),
    };
}

/// Release a region previously returned by `map`, with the same `len`.
pub fn unmap(ptr: [*]u8, len: usize) void {
    switch (os) {
        .linux => _ = std.os.linux.munmap(ptr, len),
        .freebsd, .netbsd => _ = bsdSyscall2(bsd_munmap_syscall, @intFromPtr(ptr), len),
        .macos, .libc => _ = libc.munmap(ptr, len),
        .windows => _ = win32.VirtualFree(ptr, 0, win32.MEM_RELEASE),
    }
}

fn linuxMap(len: usize) ?[*]u8 {
    const linux = std.os.linux;
    const addr = linux.mmap(
        null,
        len,
        .{ .READ = true, .WRITE = true },
        .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
        -1,
        0,
    );
    if (linux.errno(addr) != .SUCCESS) return null;
    return @ptrFromInt(addr);
}

// The `mmap` protection and flag bits below are the same values on every BSD
// and on macOS: PROT_READ | PROT_WRITE, and MAP_PRIVATE | MAP_ANON.
const prot_read_write: usize = 0x01 | 0x02;
const map_private_anonymous: usize = 0x0002 | 0x1000;

fn libcMap(len: usize) ?[*]u8 {
    const addr = libc.mmap(
        null,
        len,
        @intCast(prot_read_write),
        @intCast(map_private_anonymous),
        -1,
        0,
    );
    // `mmap` reports failure as MAP_FAILED, which is (void *)-1 rather than null.
    if (@intFromPtr(addr) == @as(usize, @bitCast(@as(isize, -1)))) return null;
    return @ptrCast(addr);
}

const libc = struct {
    extern fn mmap(addr: ?*anyopaque, len: usize, prot: c_int, flags: c_int, fd: c_int, offset: i64) *anyopaque;
    extern fn munmap(addr: *anyopaque, len: usize) c_int;
};

fn windowsMap(len: usize) ?[*]u8 {
    const addr = win32.VirtualAlloc(
        null,
        len,
        win32.MEM_COMMIT | win32.MEM_RESERVE,
        win32.PAGE_READWRITE,
    ) orelse return null;
    return @ptrCast(addr);
}

// std.os.windows.VirtualAlloc / VirtualFree were removed in Zig 0.16.
const win32 = struct {
    const MEM_COMMIT: u32 = 0x1000;
    const MEM_RESERVE: u32 = 0x2000;
    const MEM_RELEASE: u32 = 0x8000;
    const PAGE_READWRITE: u32 = 0x04;

    extern "kernel32" fn VirtualAlloc(
        lpAddress: ?*anyopaque,
        dwSize: usize,
        flAllocationType: u32,
        flProtect: u32,
    ) callconv(.winapi) ?*anyopaque;

    extern "kernel32" fn VirtualFree(
        lpAddress: *anyopaque,
        dwSize: usize,
        dwFreeType: u32,
    ) callconv(.winapi) i32;
};

const bsd_mmap_syscall: usize = switch (os) {
    .freebsd => 477,
    .netbsd => 197,
    .linux, .macos, .windows, .libc => 0,
};

const bsd_munmap_syscall: usize = switch (os) {
    .freebsd, .netbsd => 73,
    .linux, .macos, .windows, .libc => 0,
};

fn bsdMap(len: usize) ?[*]u8 {
    const fd: usize = @bitCast(@as(isize, -1));
    const addr = switch (os) {
        .netbsd => netBsdMmap(len, fd),
        .freebsd => bsdSyscall6(bsd_mmap_syscall, 0, len, prot_read_write, map_private_anonymous, fd, 0),
        .linux, .macos, .windows, .libc => unreachable,
    };
    // A raw BSD syscall negates the errno on failure, so a value that reads as
    // negative marks the error.
    if (@as(isize, @bitCast(addr)) < 0) return null;
    return @ptrFromInt(addr);
}

/// NetBSD's `mmap` ABI has an explicit padding slot before its seventh `off_t`
/// argument. The assembly symbol uses the C ABI so that seventh slot is at
/// `rsp + 8`, exactly where the x86_64 kernel reads syscall arguments beyond
/// the sixth.
fn netBsdMmap(len: usize, fd: usize) usize {
    const mmap_fn = @extern(
        *const fn (usize, usize, usize, usize, usize, usize, usize) callconv(.c) usize,
        .{ .name = "roc_raw_pages_netbsd_mmap" },
    );
    return mmap_fn(0, len, prot_read_write, map_private_anonymous, fd, 0, 0);
}

comptime {
    // Weak so that a program linking both the default-platform runtime and the
    // boxy runtime—each carrying its own copy—resolves to one definition.
    if (os == .netbsd) asm (
        \\.text
        \\.weak roc_raw_pages_netbsd_mmap
        \\.hidden roc_raw_pages_netbsd_mmap
        \\.type roc_raw_pages_netbsd_mmap,@function
        \\roc_raw_pages_netbsd_mmap:
        \\movq %rcx, %r10
        \\movq $197, %rax
        \\syscall
        \\jnc 1f
        \\negq %rax
        \\1: retq
        \\.size roc_raw_pages_netbsd_mmap, .-roc_raw_pages_netbsd_mmap
        \\.section .note.GNU-stack,"",@progbits
    );
}

fn bsdSyscall2(number: usize, arg1: usize, arg2: usize) usize {
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

fn bsdSyscall6(
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
