const std = @import("std");
const str = @import("str");
const RocStr = str.RocStr;
const testing = std.testing;
const expectEqual = testing.expectEqual;
const expect = testing.expect;
const maxInt = std.math.maxInt;

comptime {    _ = @import("compiler_rt"); }


const mem = std.mem;
const Allocator = mem.Allocator;

extern fn roc__mainForHost_1_exposed([*]u8) void;
extern fn roc__mainForHost_1_size() i64;
extern fn roc__mainForHost_1_Fx_caller(*const u8, *const u8, [*]u8, [*]u8) void;
extern fn roc__mainForHost_1_Fx_size() i64;
extern fn roc__mainForHost_1_Fx_result_size() i64;

const Unit = extern struct {};

pub export fn main() u8 {
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    const size = @intCast(usize, roc__mainForHost_1_size());
    const raw_output = std.heap.c_allocator.alloc(u8, size) catch unreachable;
    var output = @ptrCast([*]u8, raw_output);

    defer {
        std.heap.c_allocator.free(raw_output);
    }

    var ts1: std.os.timespec = undefined;
    std.os.clock_gettime(std.os.CLOCK_REALTIME, &ts1) catch unreachable;

    roc__mainForHost_1_exposed(output);

    const elements = @ptrCast([*]u64, @alignCast(8, output));

    var flag = elements[0];

    if (flag == 0) {
        // all is well
        const function_pointer = @intToPtr(*const u8, elements[1]);
        const closure_data_pointer = @ptrCast([*]u8, output[16..size]);

        call_the_closure(function_pointer, closure_data_pointer);
    } else {
        unreachable;
    }

    var ts2: std.os.timespec = undefined;
    std.os.clock_gettime(std.os.CLOCK_REALTIME, &ts2) catch unreachable;

    const delta = to_seconds(ts2) - to_seconds(ts1);

    stderr.print("runtime: {d:.3}ms\n", .{delta * 1000}) catch unreachable;

    return 0;
}

fn to_seconds(tms: std.os.timespec) f64 {
    return @intToFloat(f64, tms.tv_sec) + (@intToFloat(f64, tms.tv_nsec) / 1_000_000_000.0);
}

fn call_the_closure(function_pointer: *const u8, closure_data_pointer: [*]u8) void {
    const size = roc__mainForHost_1_Fx_result_size();
    const raw_output = std.heap.c_allocator.alloc(u8, @intCast(usize, size)) catch unreachable;
    var output = @ptrCast([*]u8, raw_output);

    defer {
        std.heap.c_allocator.free(raw_output);
    }

    const flags: u8 = 0;

    roc__mainForHost_1_Fx_caller(&flags, function_pointer, closure_data_pointer, output);

    const elements = @ptrCast([*]u64, @alignCast(8, output));

    var flag = elements[0];

    if (flag == 0) {
        return;
    } else {
        unreachable;
    }
}

pub export fn roc_fx_putLine(rocPath: str.RocStr) i64 {
    const stdout = std.io.getStdOut().writer();

    for (rocPath.asSlice()) |char| {
        stdout.print("{c}", .{char}) catch unreachable;
    }

    stdout.print("\n", .{}) catch unreachable;

    return 0;
}

const GetInt = extern struct {
    value: i64,
    error_code: u8,
    is_error: bool,
};

pub export fn roc_fx_getInt() GetInt {
    if (roc_fx_getInt_help()) |value| {
        const get_int = GetInt{ .is_error = false, .value = value, .error_code = 0 };
        return get_int;
    } else |err| switch (err) {
        error.InvalidCharacter => {
            return GetInt{ .is_error = true, .value = 0, .error_code = 0 };
        },
        else => {
            return GetInt{ .is_error = true, .value = 0, .error_code = 1 };
        },
    }

    return 0;
}

fn roc_fx_getInt_help() !i64 {
    const stdin = std.io.getStdIn().inStream();
    var buf: [40]u8 = undefined;

    const line: []u8 = (try stdin.readUntilDelimiterOrEof(&buf, '\n')) orelse "";

    return std.fmt.parseInt(i64, line, 10);
}

fn readLine() []u8 {
    const stdin = std.io.getStdIn().reader();
    return (stdin.readUntilDelimiterOrEof(&line_buf, '\n') catch unreachable) orelse "";
}
