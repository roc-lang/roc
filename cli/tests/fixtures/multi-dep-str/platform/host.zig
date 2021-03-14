const std = @import("std");
const str = @import("str");
const RocStr = str.RocStr;
const testing = std.testing;
const expectEqual = testing.expectEqual;
const expect = testing.expect;

comptime {    _ = @import("compiler_rt"); }


const mem = std.mem;
const Allocator = mem.Allocator;

extern fn roc__mainForHost_1_exposed(*RocCallResult) void;

const RocCallResult = extern struct { flag: usize, content: RocStr };

const Unit = extern struct {};

pub export fn main() i32 {
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    // make space for the result
    var callresult = RocCallResult{ .flag = 0, .content = RocStr.empty() };

    // start time
    var ts1: std.os.timespec = undefined;
    std.os.clock_gettime(std.os.CLOCK_REALTIME, &ts1) catch unreachable;

    // actually call roc to populate the callresult
    roc__mainForHost_1_exposed(&callresult);

    // stdout the result
    stdout.print("{}\n", .{callresult.content.asSlice()}) catch unreachable;

    callresult.content.deinit(std.heap.c_allocator);

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
