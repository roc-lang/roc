//! Shared ABI layout for calls into compiler-owned native code.

const std = @import("std");

/// Byte offset of the `expect_err` observation flag.
pub const expect_err_set_offset: u32 = 0;
/// Byte offset of the `expect_err` source-region start.
pub const expect_err_start_offset: u32 = expect_err_set_offset + @sizeOf(u32);
/// Byte offset of the `expect_err` source-region end.
pub const expect_err_end_offset: u32 = expect_err_start_offset + @sizeOf(u32);

const observation_size: u32 = expect_err_end_offset + @sizeOf(u32);

/// State threaded through every generated call in one in-process invocation.
pub const Context = extern struct {
    expect_err_set: u32 = 0,
    expect_err_start: u32 = 0,
    expect_err_end: u32 = 0,
    boxy_fn_table: *const anyopaque,
};

/// Byte offset of the Boxy native-function table for a target pointer size.
pub fn boxyFnTableOffset(pointer_size: u32) u32 {
    return std.mem.alignForward(u32, observation_size, pointer_size);
}

/// Total context size for a target pointer size.
pub fn contextSize(pointer_size: u32) u32 {
    const unpadded_size = boxyFnTableOffset(pointer_size) + pointer_size;
    return std.mem.alignForward(u32, unpadded_size, @max(pointer_size, @alignOf(u32)));
}

comptime {
    const host_pointer_size: u32 = @sizeOf(usize);
    if (@offsetOf(Context, "expect_err_set") != expect_err_set_offset or
        @offsetOf(Context, "expect_err_start") != expect_err_start_offset or
        @offsetOf(Context, "expect_err_end") != expect_err_end_offset)
    {
        @compileError("in-process context observation offsets diverged from their target layout");
    }
    if (@offsetOf(Context, "boxy_fn_table") != boxyFnTableOffset(host_pointer_size)) {
        @compileError("in-process context Boxy table offset diverged from its target layout");
    }
    if (@sizeOf(Context) != contextSize(host_pointer_size)) {
        @compileError("in-process context size diverged from its target layout");
    }
}
