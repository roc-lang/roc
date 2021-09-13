const std = @import("std");
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

extern fn malloc(size: usize) callconv(.C) ?*c_void;
extern fn realloc(c_ptr: [*]align(@alignOf(u128)) u8, size: usize) callconv(.C) ?*c_void;
extern fn free(c_ptr: [*]align(@alignOf(u128)) u8) callconv(.C) void;

export fn roc_alloc(size: usize, alignment: u32) callconv(.C) ?*c_void {
    return malloc(size);
}

export fn roc_realloc(c_ptr: *c_void, old_size: usize, new_size: usize, alignment: u32) callconv(.C) ?*c_void {
    return realloc(@alignCast(16, @ptrCast([*]u8, c_ptr)), new_size);
}

export fn roc_dealloc(c_ptr: *c_void, alignment: u32) callconv(.C) void {
    free(@alignCast(16, @ptrCast([*]u8, c_ptr)));
}

export fn roc_panic(c_ptr: *c_void, tag_id: u32) callconv(.C) void {
    const stderr = std.io.getStdErr().writer();
    const msg = @ptrCast([*:0]const u8, c_ptr);
    stderr.print("Application crashed with message\n\n    {s}\n\nShutting down\n", .{msg}) catch unreachable;
    std.process.exit(0);
}

const mem = std.mem;
const Allocator = mem.Allocator;

extern fn roc__mainForHost_1_exposed(i64, *RocCallResult) void;

const RocCallResult = extern struct { flag: usize, content: u64 };

const Unit = extern struct {};

pub export fn main() u8 {
    const stdout = std.io.getStdOut().writer();
    const fib_number_to_find: u64 = 10; // find the nth Fibonacci number
    const iterations: usize = 50; // number of times to repeatedly find that Fibonacci number

    // make space for the result
    var callresult = RocCallResult{ .flag = 0, .content = 0 };
    var remaining_iterations = iterations;

    while (remaining_iterations > 0) {
        // actually call roc to populate the callresult
        roc__mainForHost_1_exposed(fib_number_to_find, &callresult);

        remaining_iterations -= 1;
    }

    // stdout the final result
    stdout.print("After calling the Roc app {d} times, the Fibonacci number at index {d} is {d}\n", .{iterations, fib_number_to_find, callresult.content}) catch unreachable;

    return 0;
}