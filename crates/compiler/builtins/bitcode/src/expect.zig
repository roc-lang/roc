const std = @import("std");

extern fn shm_open(name: *const i8, oflag: c_int, mode: c_uint) c_int;
extern fn mmap(addr: ?*anyopaque, length: c_uint, prot: c_int, flags: c_int, fd: c_int, offset: c_uint) *anyopaque;
extern fn kill(pid: c_int, sig: c_int) c_int;
extern fn getppid() c_int;

const SIGUSR1: c_int = 10;

const O_RDWR: c_int = 2;
const O_CREAT: c_int = 64;

pub const PROT_WRITE: c_int = 2;
pub const MAP_SHARED: c_int = 0x0001;

// IMPORTANT: shared memory object names must begin with / and contain no other slashes!
var SHARED_BUFFER: []u8 = undefined;

pub fn setSharedBuffer(ptr: [*]u8, length: usize) callconv(.C) usize {
    SHARED_BUFFER = ptr[0..length];

    // the rust side expects that a pointer is returned
    return 0;
}

pub fn expectFailedStart() callconv(.C) [*]u8 {
    return SHARED_BUFFER.ptr;
}

pub fn expectFailedFinalize() callconv(.C) void {
    const parent_pid = getppid();

    _ = kill(parent_pid, SIGUSR1);
}
