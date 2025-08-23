//! C-compatible libc symbol stubs for the shim library
//! Provides minimal implementations of libc functions using direct syscalls

const std = @import("std");
const builtin = @import("builtin");
const syscalls = @import("syscalls.zig");

// Only compile on Linux
comptime {
    if (builtin.target.os.tag != .linux) {
        @compileError("libc_stubs.zig is only for Linux targets");
    }
}

// Export C-compatible functions that use our direct syscalls

export fn mmap(addr: ?*anyopaque, length: usize, prot: c_int, flags: c_int, fd: c_int, offset: i64) callconv(.C) ?*anyopaque {
    const aligned_addr: ?*align(4096) anyopaque = if (addr) |a| @ptrCast(@alignCast(a)) else null;
    return syscalls.mmap(aligned_addr, length, @intCast(prot), @intCast(flags), fd, offset);
}

export fn munmap(addr: *anyopaque, length: usize) callconv(.C) c_int {
    return syscalls.munmap(@ptrCast(@alignCast(addr)), length);
}

export fn close(fd: c_int) callconv(.C) c_int {
    return syscalls.close(fd);
}

export fn abort() callconv(.C) noreturn {
    syscalls.abort();
}

export fn __errno_location() callconv(.C) *c_int {
    return @ptrCast(syscalls.__errno_location());
}

export fn getenv(name: [*:0]const u8) callconv(.C) ?[*:0]const u8 {
    return syscalls.getenv(name);
}

export fn read(fd: c_int, buf: [*]u8, count: usize) callconv(.C) isize {
    return syscalls.read(fd, buf, count);
}

export fn write(fd: c_int, buf: [*]const u8, count: usize) callconv(.C) isize {
    return syscalls.write(fd, buf, count);
}

export fn openat(dirfd: c_int, pathname: [*:0]const u8, flags: c_int) callconv(.C) c_int {
    // Simple implementation - ignore dirfd for now
    _ = dirfd;
    return syscalls.open(pathname, @intCast(flags), 0o644);
}

export fn readlink(pathname: [*:0]const u8, buf: [*]u8, bufsiz: usize) callconv(.C) isize {
    // Minimal implementation - return error
    _ = pathname;
    _ = buf;
    _ = bufsiz;
    return -1;
}

export fn flock(fd: c_int, operation: c_int) callconv(.C) c_int {
    // Minimal implementation - always succeed
    _ = fd;
    _ = operation;
    return 0;
}

export fn isatty(fd: c_int) callconv(.C) c_int {
    // Simple implementation - assume not a tty
    _ = fd;
    return 0;
}

export fn pread(fd: c_int, buf: [*]u8, count: usize, offset: i64) callconv(.C) isize {
    // Simple implementation using read (ignoring offset)
    _ = offset;
    return syscalls.read(fd, buf, count);
}

export fn msync(addr: *anyopaque, length: usize, flags: c_int) callconv(.C) c_int {
    // Minimal implementation - always succeed
    _ = addr;
    _ = length;
    _ = flags;
    return 0;
}

export fn mremap(old_address: *anyopaque, old_size: usize, new_size: usize, flags: c_int) callconv(.C) ?*anyopaque {
    // Minimal implementation - return error
    _ = old_address;
    _ = old_size;
    _ = new_size;
    _ = flags;
    return null;
}

export fn realpath(path: [*:0]const u8, resolved_path: ?[*]u8) callconv(.C) ?[*:0]u8 {
    // Minimal implementation - return null (error)
    _ = path;
    _ = resolved_path;
    return null;
}

export fn sigaction(signum: c_int, act: ?*const anyopaque, oldact: ?*anyopaque) callconv(.C) c_int {
    // Minimal implementation - always succeed
    _ = signum;
    _ = act;
    _ = oldact;
    return 0;
}

export fn dl_iterate_phdr(callback: ?*const anyopaque, data: ?*anyopaque) callconv(.C) c_int {
    // Minimal implementation - don't iterate anything
    _ = callback;
    _ = data;
    return 0;
}

/// Included because it may be used in some code paths.
export fn getauxval(type_val: c_ulong) callconv(.C) c_ulong {
    // Minimal implementation - return 0 for all auxiliary vector values
    _ = type_val;
    return 0;
}

// Memory functions
export fn memcpy(dest: *anyopaque, src: *const anyopaque, n: usize) callconv(.C) *anyopaque {
    const dest_bytes: [*]u8 = @ptrCast(dest);
    const src_bytes: [*]const u8 = @ptrCast(src);
    for (0..n) |i| {
        dest_bytes[i] = src_bytes[i];
    }
    return dest;
}

export fn memset(s: *anyopaque, c: c_int, n: usize) callconv(.C) *anyopaque {
    const bytes: [*]u8 = @ptrCast(s);
    const byte_val: u8 = @intCast(c);
    for (0..n) |i| {
        bytes[i] = byte_val;
    }
    return s;
}

// Math functions - minimal implementations
export fn exp(x: f64) callconv(.C) f64 {
    return @exp(x);
}

export fn log(x: f64) callconv(.C) f64 {
    return @log(x);
}

// Stack protection - minimal implementation
export fn __stack_chk_fail() callconv(.C) noreturn {
    syscalls.abort();
}

// Compiler intrinsics for 128-bit integer operations
export fn __divti3(a: i128, b: i128) callconv(.C) i128 {
    return @divTrunc(a, b);
}

export fn __modti3(a: i128, b: i128) callconv(.C) i128 {
    return @rem(a, b);
}

export fn __udivti3(a: u128, b: u128) callconv(.C) u128 {
    return a / b;
}

export fn __umodti3(a: u128, b: u128) callconv(.C) u128 {
    return a % b;
}

export fn __floattidf(a: i128) callconv(.C) f64 {
    return @floatFromInt(a);
}

// Thread-local storage - minimal implementation
export fn __tls_get_addr(ti: *anyopaque) callconv(.C) *anyopaque {
    // Return the same pointer for simplicity
    return ti;
}

// Stack probing for large stack allocations
export fn __zig_probe_stack() callconv(.C) void {
    // No-op implementation
}

// Environment variable stub
export var environ: ?[*]?[*:0]const u8 = null;

// Process control functions
export fn exit(status: c_int) callconv(.C) noreturn {
    _ = status;
    syscalls.abort();
}

// Resource limit functions - minimal implementations
export fn getrlimit(resource: c_int, rlim: *anyopaque) callconv(.C) c_int {
    _ = resource;
    _ = rlim;
    // Return success but don't modify the structure
    return 0;
}

export fn setrlimit(resource: c_int, rlim: *const anyopaque) callconv(.C) c_int {
    _ = resource;
    _ = rlim;
    // Return success but don't actually set anything
    return 0;
}
