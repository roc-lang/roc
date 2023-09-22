const std = @import("std");
const str = @import("glue").str;
const builtin = @import("builtin");
const RocStr = str.RocStr;

const Align = extern struct { a: usize, b: usize };
extern fn malloc(size: usize) callconv(.C) ?*align(@alignOf(Align)) anyopaque;
extern fn realloc(c_ptr: [*]align(@alignOf(Align)) u8, size: usize) callconv(.C) ?*anyopaque;
extern fn free(c_ptr: [*]align(@alignOf(Align)) u8) callconv(.C) void;
extern fn memcpy(dest: *anyopaque, src: *anyopaque, count: usize) *anyopaque;

export fn roc_alloc(size: usize, alignment: u32) callconv(.C) ?*anyopaque {
    _ = alignment;

    return malloc(size);
}

export fn roc_realloc(c_ptr: *anyopaque, new_size: usize, old_size: usize, alignment: u32) callconv(.C) ?*anyopaque {
    _ = old_size;
    _ = alignment;

    return realloc(@as([*]align(Align) u8, @alignCast(@ptrCast(c_ptr))), new_size);
}

export fn roc_dealloc(c_ptr: *anyopaque, alignment: u32) callconv(.C) void {
    _ = alignment;

    free(@as([*]align(Align) u8, @alignCast(@ptrCast(c_ptr))));
}

export fn roc_panic(c_ptr: *anyopaque, tag_id: u32) callconv(.C) void {
    _ = tag_id;
    const msg = @as([*:0]const u8, @ptrCast(c_ptr));
    const stderr = std.io.getStdErr().writer();
    stderr.print("Application crashed with message\n\n    {s}\n\nShutting down\n", .{msg}) catch unreachable;
    std.process.exit(0);
}

const ResultStrStr = extern struct {
    payload: RocStr,
    isOk: bool,
};

extern fn roc__main_1_exposed(RocStr, RocStr) callconv(.C) ResultStrStr;

const hostJavaScriptBytes = @embedFile("../client-side/host.js");

pub fn main() u8 {
    const json = RocStr.fromSlice("42");
    defer json.decref();

    const hostJavaScript = RocStr.fromSlice(hostJavaScriptBytes);
    defer hostJavaScript.decref();

    const result = roc__main_1_exposed(json, hostJavaScript);
    defer result.payload.decref();

    const writer = if (result.isOk)
        std.io.getStdOut().writer()
    else
        std.io.getStdErr().writer();

    const output = result.payload.asSlice();
    writer.print("{s}", .{output}) catch unreachable;

    return if (result.isOk) 0 else 1;
}
