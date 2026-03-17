//! macOS compatibility shims for LLVM-compiled shared libraries.
//!
//! Provides stub implementations of Darwin-specific linker symbols that
//! LLVM/LLD requires but are not needed for JIT evaluation.

const std = @import("std");

const dyld_build_version_t = extern struct {
    platform: u32,
    version: u32,
};

const copyfile_state_t = *opaque {};

const system___error = @extern(*const fn () callconv(.c) *c_int, .{ .name = "\x01___error" });
const system___availability_version_check = @extern(*const fn (u32, [*c]const dyld_build_version_t) callconv(.c) bool, .{ .name = "\x01__availability_version_check" });
const system_abort = @extern(*const fn () callconv(.c) noreturn, .{ .name = "\x01_abort" });
const system_bzero = @extern(*const fn (?*anyopaque, usize) callconv(.c) void, .{ .name = "\x01_bzero" });
const system_fcopyfile = @extern(*const fn (std.c.fd_t, std.c.fd_t, ?copyfile_state_t, std.c.COPYFILE) callconv(.c) c_int, .{ .name = "\x01_fcopyfile" });
const system_fstat = @extern(*const fn (std.c.fd_t, *std.c.Stat) callconv(.c) c_int, .{ .name = "\x01_fstat" });
const system_lseek = @extern(*const fn (std.c.fd_t, std.c.off_t, std.c.whence_t) callconv(.c) std.c.off_t, .{ .name = "\x01_lseek" });
const system_pthread_threadid_np = @extern(*const fn (?std.c.pthread_t, *u64) callconv(.c) c_int, .{ .name = "\x01_pthread_threadid_np" });
const system_pwrite = @extern(*const fn (std.c.fd_t, [*]const u8, usize, std.c.off_t) callconv(.c) isize, .{ .name = "\x01_pwrite" });
const system_readv = @extern(*const fn (c_int, [*]const std.c.iovec, c_uint) callconv(.c) isize, .{ .name = "\x01_readv" });
const system_sendfile = @extern(*const fn (std.c.fd_t, std.c.fd_t, std.c.off_t, *std.c.off_t, ?*std.c.sf_hdtr, u32) callconv(.c) c_int, .{ .name = "\x01_sendfile" });
const system_sys_icache_invalidate = @extern(*const fn (*const anyopaque, usize) callconv(.c) void, .{ .name = "\x01_sys_icache_invalidate" });
const system_write = @extern(*const fn (std.c.fd_t, [*]const u8, usize) callconv(.c) isize, .{ .name = "\x01_write" });
const system_writev = @extern(*const fn (c_int, [*]const std.c.iovec_const, c_uint) callconv(.c) isize, .{ .name = "\x01_writev" });

fn shim__error() callconv(.c) *c_int {
    return system___error();
}

fn shim_availability_version_check(count: u32, versions: [*c]const dyld_build_version_t) callconv(.c) bool {
    return system___availability_version_check(count, versions);
}

fn shim_abort() callconv(.c) noreturn {
    system_abort();
}

fn shim_bzero(buf: ?*anyopaque, len: usize) callconv(.c) void {
    system_bzero(buf, len);
}

fn shim_fcopyfile(from: std.c.fd_t, to: std.c.fd_t, state: ?copyfile_state_t, flags: std.c.COPYFILE) callconv(.c) c_int {
    return system_fcopyfile(from, to, state, flags);
}

fn shim_fstat(fd: std.c.fd_t, buf: *std.c.Stat) callconv(.c) c_int {
    return system_fstat(fd, buf);
}

fn shim_lseek(fd: std.c.fd_t, offset: std.c.off_t, whence: std.c.whence_t) callconv(.c) std.c.off_t {
    return system_lseek(fd, offset, whence);
}

fn shim_pthread_threadid_np(thread: ?std.c.pthread_t, thread_id: *u64) callconv(.c) c_int {
    return system_pthread_threadid_np(thread, thread_id);
}

fn shim_pwrite(fd: std.c.fd_t, buf: [*]const u8, nbyte: usize, offset: std.c.off_t) callconv(.c) isize {
    return system_pwrite(fd, buf, nbyte, offset);
}

fn shim_readv(fd: c_int, iov: [*]const std.c.iovec, iovcnt: c_uint) callconv(.c) isize {
    return system_readv(fd, iov, iovcnt);
}

fn shim_sendfile(in_fd: std.c.fd_t, out_fd: std.c.fd_t, offset: std.c.off_t, len: *std.c.off_t, sf_hdtr: ?*std.c.sf_hdtr, flags: u32) callconv(.c) c_int {
    return system_sendfile(in_fd, out_fd, offset, len, sf_hdtr, flags);
}

fn shim_sys_icache_invalidate(start: *const anyopaque, len: usize) callconv(.c) void {
    system_sys_icache_invalidate(start, len);
}

fn shim_write(fd: std.c.fd_t, buf: [*]const u8, nbyte: usize) callconv(.c) isize {
    return system_write(fd, buf, nbyte);
}

fn shim_writev(fd: c_int, iov: [*]const std.c.iovec_const, iovcnt: c_uint) callconv(.c) isize {
    return system_writev(fd, iov, iovcnt);
}

comptime {
    @export(&shim__error, .{ .name = "\x01__error" });
    @export(&shim_availability_version_check, .{ .name = "\x01_availability_version_check" });
    @export(&shim_abort, .{ .name = "\x01abort" });
    @export(&shim_bzero, .{ .name = "\x01bzero" });
    @export(&shim_fcopyfile, .{ .name = "\x01fcopyfile" });
    @export(&shim_fstat, .{ .name = "\x01fstat" });
    @export(&shim_lseek, .{ .name = "\x01lseek" });
    @export(&shim_pthread_threadid_np, .{ .name = "\x01pthread_threadid_np" });
    @export(&shim_pwrite, .{ .name = "\x01pwrite" });
    @export(&shim_readv, .{ .name = "\x01readv" });
    @export(&shim_sendfile, .{ .name = "\x01sendfile" });
    @export(&shim_sys_icache_invalidate, .{ .name = "\x01sys_icache_invalidate" });
    @export(&shim_write, .{ .name = "\x01write" });
    @export(&shim_writev, .{ .name = "\x01writev" });
}
