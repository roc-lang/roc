const std = @import("std");
const str = @import("str");
const RocStr = str.RocStr;
const testing = std.testing;
const expectEqual = testing.expectEqual;
const expect = testing.expect;

comptime {
    // This is a workaround for https://github.com/ziglang/zig/issues/8218
    // which is only necessary on macOS.
    //
    // Once that issue is fixed, we can undo the changes in
    // 177cf12e0555147faa4d436e52fc15175c2c4ff0 and go back to passing
    // -fcompiler-rt in link.rs instead of doing this. Note that this
    // workaround is present in many host.zig files, so make sure to undo
    // it everywhere!
    if (std.builtin.os.tag == .macos) {
        _ = @import("compiler_rt");
    }
}

const Align = extern struct { a: usize, b: usize };
// extern fn malloc(size: usize) callconv(.C) ?*align(@alignOf(Align)) c_void;
// extern fn realloc(c_ptr: [*]align(@alignOf(Align)) u8, size: usize) callconv(.C) ?*c_void;
// extern fn free(c_ptr: [*]align(@alignOf(Align)) u8) callconv(.C) void;
// extern fn memcpy(dst: [*]u8, src: [*]u8, size: usize) callconv(.C) void;
// extern fn memset(dst: [*]u8, value: i32, size: usize) callconv(.C) void;

export fn roc_alloc(size: usize, alignment: u32) callconv(.C) ?*c_void {
    _ = alignment;

    // return malloc(size);
    const stderr = std.io.getStdErr().writer();
    stderr.print("Using memory is not allowed", .{}) catch unreachable;
    std.process.exit(0);
}

export fn roc_realloc(c_ptr: *c_void, old_size: usize, new_size: usize, alignment: u32) callconv(.C) ?*c_void {
    _ = old_size;
    _ = alignment;

    // return realloc(@alignCast(@alignOf(Align), @ptrCast([*]u8, c_ptr)), new_size);
    const stderr = std.io.getStdErr().writer();
    stderr.print("Using memory is not allowed", .{}) catch unreachable;
    std.process.exit(0);
}

export fn roc_dealloc(c_ptr: *c_void, alignment: u32) callconv(.C) void {
    _ = alignment;

    // free(@alignCast(@alignOf(Align), @ptrCast([*]u8, c_ptr)));
    const stderr = std.io.getStdErr().writer();
    stderr.print("Using memory is not allowed", .{}) catch unreachable;
    std.process.exit(0);
}

export fn roc_panic(c_ptr: *c_void, tag_id: u32) callconv(.C) void {
    _ = tag_id;
    const stderr = std.io.getStdErr().writer();
    const msg = @ptrCast([*:0]const u8, c_ptr);
    stderr.print("Application crashed with message\n\n    {s}\n\nShutting down\n", .{msg}) catch unreachable;
    std.process.exit(0);
}

export fn roc_memcpy(dst: [*]u8, src: [*]u8, size: usize) callconv(.C) void{
    const stderr = std.io.getStdErr().writer();
    stderr.print("Using memory is not allowed", .{}) catch unreachable;
    std.process.exit(0);
}

export fn roc_memset(dst: [*]u8, value: i32, size: usize) callconv(.C) void{
    const stderr = std.io.getStdErr().writer();
    stderr.print("Using memory is not allowed", .{}) catch unreachable;
    std.process.exit(0);
}

const mem = std.mem;
const Allocator = mem.Allocator;

extern fn roc__mainForHost_1_exposed() RocStr;

const Unit = extern struct {};

pub fn main() u8 {
    const stdout = std.io.getStdOut().writer();

    var content = roc__mainForHost_1_exposed();

    // stdout the result
    stdout.print("{s}\n", .{content.asSlice()}) catch unreachable;

    content.deinit();

    return 0;
}