//! Shared ABI layout for calls into compiler-owned native code.

const std = @import("std");

/// Byte offset of the `expect_err` observation flag.
pub const expect_err_set_offset: u32 = 0;
/// Byte offset of the `expect_err` source-region start.
pub const expect_err_start_offset: u32 = expect_err_set_offset + @sizeOf(u32);
/// Byte offset of the `expect_err` source-region end.
pub const expect_err_end_offset: u32 = expect_err_start_offset + @sizeOf(u32);

const observation_size: u32 = expect_err_end_offset + @sizeOf(u32);

/// Byte offset of the passed-count array for a target pointer size.
pub fn expectPassedOffset(pointer_size: u32) u32 {
    return std.mem.alignForward(u32, observation_size, pointer_size);
}

/// Byte offset of the failed-count array for a target pointer size.
pub fn expectFailedOffset(pointer_size: u32) u32 {
    return expectPassedOffset(pointer_size) + pointer_size;
}

/// State threaded through every generated call in one in-process invocation.
pub fn Context(comptime BoxyFnTable: type) type {
    return extern struct {
        expect_err_set: u32 = 0,
        expect_err_start: u32 = 0,
        expect_err_end: u32 = 0,
        expect_passed: ?[*]u64 = null,
        expect_failed: ?[*]u64 = null,
        boxy_fn_table: *const BoxyFnTable,
    };
}

/// Byte offset of the Boxy native-function table for a target pointer size.
pub fn boxyFnTableOffset(pointer_size: u32) u32 {
    return expectFailedOffset(pointer_size) + pointer_size;
}

/// Total context size for a target pointer size.
pub fn contextSize(pointer_size: u32) u32 {
    const unpadded_size = boxyFnTableOffset(pointer_size) + pointer_size;
    return std.mem.alignForward(u32, unpadded_size, contextAlignment(pointer_size));
}

/// Required context alignment for a target pointer size.
pub fn contextAlignment(pointer_size: u32) u32 {
    return @max(pointer_size, @alignOf(u32));
}

comptime {
    const OpaqueContext = Context(anyopaque);
    const host_pointer_size: u32 = @sizeOf(usize);
    if (@offsetOf(OpaqueContext, "expect_err_set") != expect_err_set_offset or
        @offsetOf(OpaqueContext, "expect_err_start") != expect_err_start_offset or
        @offsetOf(OpaqueContext, "expect_err_end") != expect_err_end_offset)
    {
        @compileError("in-process context observation offsets diverged from their target layout");
    }
    if (@offsetOf(OpaqueContext, "boxy_fn_table") != boxyFnTableOffset(host_pointer_size)) {
        @compileError("in-process context Boxy table offset diverged from its target layout");
    }
    if (@offsetOf(OpaqueContext, "expect_passed") != expectPassedOffset(host_pointer_size) or
        @offsetOf(OpaqueContext, "expect_failed") != expectFailedOffset(host_pointer_size))
    {
        @compileError("in-process context expect counter offsets diverged from their target layout");
    }
    if (@sizeOf(OpaqueContext) != contextSize(host_pointer_size)) {
        @compileError("in-process context size diverged from its target layout");
    }
    if (@alignOf(OpaqueContext) != contextAlignment(host_pointer_size)) {
        @compileError("in-process context alignment diverged from its target layout");
    }
}

test "in-process ABI context layout follows pointer width" {
    try std.testing.expectEqual(@as(u32, 16), boxyFnTableOffset(2));
    try std.testing.expectEqual(@as(u32, 20), boxyFnTableOffset(4));
    try std.testing.expectEqual(@as(u32, 32), boxyFnTableOffset(8));

    try std.testing.expectEqual(@as(u32, 4), contextAlignment(2));
    try std.testing.expectEqual(@as(u32, 4), contextAlignment(4));
    try std.testing.expectEqual(@as(u32, 8), contextAlignment(8));

    try std.testing.expectEqual(@as(u32, 20), contextSize(2));
    try std.testing.expectEqual(@as(u32, 24), contextSize(4));
    try std.testing.expectEqual(@as(u32, 40), contextSize(8));
}
