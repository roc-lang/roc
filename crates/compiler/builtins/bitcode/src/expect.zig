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

pub fn expectFailedStart() callconv(.C) [*]u8 {
    const name = "/roc_expect_buffer"; // IMPORTANT: shared memory object names must begin with / and contain no other slashes!

    const shared_fd = shm_open(@ptrCast(*const i8, name), O_RDWR | O_CREAT, 0o666);

    const shared_ptr = mmap(
        null,
        4096,
        PROT_WRITE,
        MAP_SHARED,
        shared_fd,
        0,
    );

    const ptr = @ptrCast([*]u8, shared_ptr);

    return ptr;
}

pub fn expectFailedFinalize() callconv(.C) void {
    const parent_pid = getppid();

    _ = kill(parent_pid, SIGUSR1);
}
