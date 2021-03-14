const std = @import("std");
const testing = std.testing;
const expectEqual = testing.expectEqual;
const expect = testing.expect;

comptime {    _ = @import("compiler_rt"); }

const mem = std.mem;
const Allocator = mem.Allocator;

extern fn roc__mainForHost_1_exposed(RocList, *RocCallResult) void;

// warning! the array is currently stack-allocated so don't make this too big
const NUM_NUMS = 100;

const RocList = extern struct { elements: [*]i64, length: usize };

const RocCallResult = extern struct { flag: usize, content: RocList };

const Unit = extern struct {};

pub export fn main() i32 {
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    var raw_numbers: [NUM_NUMS + 1]i64 = undefined;

    // set refcount to one
    raw_numbers[0] = -9223372036854775808;

    var numbers = raw_numbers[1..];

    for (numbers) |x, i| {
        numbers[i] = @mod(@intCast(i64, i), 12);
    }

    const roc_list = RocList{ .elements = numbers, .length = NUM_NUMS };

    // make space for the result
    var callresult = RocCallResult{ .flag = 0, .content = undefined };

    // start time
    var ts1: std.os.timespec = undefined;
    std.os.clock_gettime(std.os.CLOCK_REALTIME, &ts1) catch unreachable;

    // actually call roc to populate the callresult
    roc__mainForHost_1_exposed(roc_list, &callresult);

    // stdout the result
    const length = std.math.min(20, callresult.content.length);
    var result = callresult.content.elements[0..length];

    for (result) |x, i| {
        if (i == 0) {
            stdout.print("[{}, ", .{x}) catch unreachable;
        } else if (i == length - 1) {
            stdout.print("{}]\n", .{x}) catch unreachable;
        } else {
            stdout.print("{}, ", .{x}) catch unreachable;
        }
    }

    // end time
    var ts2: std.os.timespec = undefined;
    std.os.clock_gettime(std.os.CLOCK_REALTIME, &ts2) catch unreachable;

    const delta = to_seconds(ts2) - to_seconds(ts1);

    stderr.print("runtime: {d:.3}ms\n", .{delta * 1000}) catch unreachable;

    return 0;
}

fn to_seconds(tms: std.os.timespec) f64 {
    return @intToFloat(f64, tms.tv_sec) + (@intToFloat(f64, tms.tv_nsec) / 1_000_000_000.0);
}
