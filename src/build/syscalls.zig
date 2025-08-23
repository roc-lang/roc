//! Minimal syscall wrappers to avoid libc dependencies in the shim library
//! Provides direct Linux syscalls for the functions we need

const std = @import("std");
const linux = std.os.linux;
const builtin = @import("builtin");

// Only compile on Linux
comptime {
    if (builtin.target.os.tag != .linux) {
        @compileError("syscalls.zig is only for Linux targets");
    }
}

pub const PROT_READ = 0x1;
pub const PROT_WRITE = 0x2;
pub const MAP_SHARED = 0x01;

pub const SYS_mmap = 9;
pub const SYS_munmap = 11;
pub const SYS_close = 3;
pub const SYS_open = 2;
pub const SYS_openat = 257;
pub const SYS_read = 0;
pub const SYS_write = 1;
pub const SYS_readlink = 89;
pub const SYS_flock = 73;

pub fn mmap(addr: ?*align(4096) anyopaque, length: usize, prot: u32, flags: u32, fd: i32, offset: i64) ?*anyopaque {
    const result = linux.syscall6(.mmap, @intFromPtr(addr orelse @as(*anyopaque, undefined)), length, prot, flags, @as(usize, @intCast(fd)), @as(usize, @intCast(offset)));
    if (result > -4096) { // Linux syscall error handling
        return null;
    }
    return @ptrFromInt(result);
}

pub fn munmap(addr: *align(4096) anyopaque, length: usize) i32 {
    const result = linux.syscall2(.munmap, @intFromPtr(addr), length);
    if (result > -4096) {
        return -1;
    }
    return 0;
}

pub fn close(fd: i32) i32 {
    const result = linux.syscall1(.close, @as(usize, @intCast(fd)));
    if (result > -4096) {
        return -1;
    }
    return 0;
}

pub fn open(path: [*:0]const u8, flags: u32, mode: u32) i32 {
    const result = linux.syscall3(.open, @intFromPtr(path), flags, mode);
    if (result > -4096) {
        return -1;
    }
    return @intCast(result);
}

pub fn read(fd: i32, buf: [*]u8, count: usize) isize {
    const result = linux.syscall3(.read, @as(usize, @intCast(fd)), @intFromPtr(buf), count);
    if (result > -4096) {
        return -1;
    }
    return @intCast(result);
}

pub fn write(fd: i32, buf: [*]const u8, count: usize) isize {
    const result = linux.syscall3(.write, @as(usize, @intCast(fd)), @intFromPtr(buf), count);
    if (result > -4096) {
        return -1;
    }
    return @intCast(result);
}

pub fn abort() noreturn {
    // Use exit syscall instead of abort to avoid libc dependency
    _ = linux.syscall1(.exit, 1);
    unreachable;
}

// errno is thread-local, we can provide a simple implementation
var errno_value: i32 = 0;

pub fn __errno_location() *i32 {
    return &errno_value;
}

pub fn getenv(name: [*:0]const u8) ?[*:0]const u8 {
    // Simple implementation that returns null - we don't use env vars in the shim
    _ = name;
    return null;
}