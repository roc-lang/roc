const std = @import("std");
const builtin = @import("builtin");
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
    if (builtin.os.tag == .macos) {
        _ = @import("compiler_rt");
    }
}

const mem = std.mem;
const Allocator = mem.Allocator;

extern fn roc__mainForHost_1_exposed(input: RocList) callconv(.C) RocList;

const Align = 2 * @alignOf(usize);
extern fn malloc(size: usize) callconv(.C) ?*align(Align) anyopaque;
extern fn realloc(c_ptr: [*]align(Align) u8, size: usize) callconv(.C) ?*anyopaque;
extern fn free(c_ptr: [*]align(Align) u8) callconv(.C) void;
extern fn memcpy(dst: [*]u8, src: [*]u8, size: usize) callconv(.C) void;
extern fn memset(dst: [*]u8, value: i32, size: usize) callconv(.C) void;

const DEBUG: bool = false;

export fn roc_alloc(size: usize, alignment: u32) callconv(.C) ?*anyopaque {
    if (DEBUG) {
        var ptr = malloc(size);
        const stdout = std.io.getStdOut().writer();
        stdout.print("alloc:   {d} (alignment {d}, size {d})\n", .{ ptr, alignment, size }) catch unreachable;
        return ptr;
    } else {
        return malloc(size);
    }
}

export fn roc_realloc(c_ptr: *anyopaque, new_size: usize, old_size: usize, alignment: u32) callconv(.C) ?*anyopaque {
    if (DEBUG) {
        const stdout = std.io.getStdOut().writer();
        stdout.print("realloc: {d} (alignment {d}, old_size {d})\n", .{ c_ptr, alignment, old_size }) catch unreachable;
    }

    return realloc(@alignCast(Align, @ptrCast([*]u8, c_ptr)), new_size);
}

export fn roc_dealloc(c_ptr: *anyopaque, alignment: u32) callconv(.C) void {
    if (DEBUG) {
        const stdout = std.io.getStdOut().writer();
        stdout.print("dealloc: {d} (alignment {d})\n", .{ c_ptr, alignment }) catch unreachable;
    }

    free(@alignCast(Align, @ptrCast([*]u8, c_ptr)));
}

export fn roc_panic(c_ptr: *anyopaque, tag_id: u32) callconv(.C) void {
    _ = tag_id;

    const stderr = std.io.getStdErr().writer();
    const msg = @ptrCast([*:0]const u8, c_ptr);
    stderr.print("Application crashed with message\n\n    {s}\n\nShutting down\n", .{msg}) catch unreachable;
    std.process.exit(0);
}

export fn roc_memcpy(dst: [*]u8, src: [*]u8, size: usize) callconv(.C) void {
    return memcpy(dst, src, size);
}

export fn roc_memset(dst: [*]u8, value: i32, size: usize) callconv(.C) void {
    return memset(dst, value, size);
}

// warning! the array is currently stack-allocated so don't make this too big
const NUM_NUMS = 100;

const RocList = extern struct { elements: [*]i64, length: usize, capacity: usize };

const Unit = extern struct {};

pub export fn main() u8 {
    const stdout = std.io.getStdOut().writer();

    var raw_numbers: [NUM_NUMS + 1]i64 = undefined;

    // set refcount to one
    raw_numbers[0] = -9223372036854775808;

    var numbers = raw_numbers[1..];

    for (numbers) |_, i| {
        numbers[i] = @mod(@intCast(i64, i), 12);
    }

    var roc_list = RocList{ .elements = numbers, .length = NUM_NUMS, .capacity = NUM_NUMS };

    // start time
    var ts1: std.os.timespec = undefined;
    std.os.clock_gettime(std.os.CLOCK.REALTIME, &ts1) catch unreachable;

    // actually call roc to populate the callresult
    const callresult: RocList = roc__mainForHost_1_exposed(roc_list);

    // stdout the result
    const length = std.math.min(20, callresult.length);
    var result = callresult.elements[0..length];

    // end time
    var ts2: std.os.timespec = undefined;
    std.os.clock_gettime(std.os.CLOCK.REALTIME, &ts2) catch unreachable;

    for (result) |x, i| {
        if (i == 0) {
            stdout.print("[{}, ", .{x}) catch unreachable;
        } else if (i == length - 1) {
            stdout.print("{}]\n", .{x}) catch unreachable;
        } else {
            stdout.print("{}, ", .{x}) catch unreachable;
        }
    }

    // TODO apparently the typestamps are still (partially) undefined?
    // const delta = to_seconds(ts2) - to_seconds(ts1);
    // stderr.print("runtime: {d:.3}ms\n", .{delta * 1000}) catch unreachable;

    return 0;
}

fn to_seconds(tms: std.os.timespec) f64 {
    return @intToFloat(f64, tms.tv_sec) + (@intToFloat(f64, tms.tv_nsec) / 1_000_000_000.0);
}
