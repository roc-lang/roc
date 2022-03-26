const std = @import("std");
const str = @import("str");
const RocStr = str.RocStr;
const testing = std.testing;
const expectEqual = testing.expectEqual;
const expect = testing.expect;
const maxInt = std.math.maxInt;

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

const mem = std.mem;
const Allocator = mem.Allocator;

const Program = extern struct { init: RocStr, update: Unit, view: Unit };

extern fn roc__mainForHost_1_exposed() Program;
extern fn roc__mainForHost_size() i64;

const ConstModel = [*]const u8;
const MutModel = [*]u8;

extern fn roc__mainForHost_1_Init_caller([*]u8, [*]u8, MutModel) void;
extern fn roc__mainForHost_1_Init_size() i64;
extern fn roc__mainForHost_1_Init_result_size() i64;

fn allocate_model(allocator: *Allocator) MutModel {
    const size = roc__mainForHost_1_Init_result_size();
    const raw_output = allocator.allocAdvanced(u8, @alignOf(u64), @intCast(usize, size), .at_least) catch unreachable;
    var output = @ptrCast([*]u8, raw_output);

    return output;
}

fn init(allocator: *Allocator) ConstModel {
    const closure: [*]u8 = undefined;
    const output = allocate_model(allocator);

    roc__mainForHost_1_Init_caller(closure, closure, output);

    return output;
}

extern fn roc__mainForHost_1_Update_caller(ConstModel, *const RocStr, [*]u8, MutModel) void;
extern fn roc__mainForHost_1_Update_size() i64;
extern fn roc__mainForHost_1_Update_result_size() i64;

fn update(allocator: *Allocator, model: ConstModel, msg: RocStr) ConstModel {
    const closure: [*]u8 = undefined;
    const output = allocate_model(allocator);

    roc__mainForHost_1_Update_caller(model, &msg, closure, output);

    return output;
}

extern fn roc__mainForHost_1_View_caller(ConstModel, [*]u8, *RocStr) void;
extern fn roc__mainForHost_1_View_size() i64;
extern fn roc__mainForHost_1_View_result_size() i64;

fn view(input: ConstModel) RocStr {
    const closure: [*]u8 = undefined;
    var output: RocStr = undefined;

    roc__mainForHost_1_View_caller(input, closure, &output);

    return output;
}

const Align = 2 * @alignOf(usize);
extern fn malloc(size: usize) callconv(.C) ?*align(Align) c_void;
extern fn realloc(c_ptr: [*]align(Align) u8, size: usize) callconv(.C) ?*c_void;
extern fn free(c_ptr: [*]align(Align) u8) callconv(.C) void;
extern fn memcpy(dst: [*]u8, src: [*]u8, size: usize) callconv(.C) void;
extern fn memset(dst: [*]u8, value: i32, size: usize) callconv(.C) void;

const DEBUG: bool = false;

export fn roc_alloc(size: usize, alignment: u32) callconv(.C) ?*c_void {
    if (DEBUG) {
        var ptr = malloc(size);
        const stdout = std.io.getStdOut().writer();
        stdout.print("alloc:   {d} (alignment {d}, size {d})\n", .{ ptr, alignment, size }) catch unreachable;
        return ptr;
    } else {
        return malloc(size);
    }
}

export fn roc_realloc(c_ptr: *c_void, new_size: usize, old_size: usize, alignment: u32) callconv(.C) ?*c_void {
    if (DEBUG) {
        const stdout = std.io.getStdOut().writer();
        stdout.print("realloc: {d} (alignment {d}, old_size {d})\n", .{ c_ptr, alignment, old_size }) catch unreachable;
    }

    return realloc(@alignCast(Align, @ptrCast([*]u8, c_ptr)), new_size);
}

export fn roc_dealloc(c_ptr: *c_void, alignment: u32) callconv(.C) void {
    if (DEBUG) {
        const stdout = std.io.getStdOut().writer();
        stdout.print("dealloc: {d} (alignment {d})\n", .{ c_ptr, alignment }) catch unreachable;
    }

    free(@alignCast(Align, @ptrCast([*]u8, c_ptr)));
}

export fn roc_panic(c_ptr: *c_void, tag_id: u32) callconv(.C) void {
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

const Unit = extern struct {};

pub export fn main() callconv(.C) u8 {
    const allocator = std.heap.page_allocator;

    var ts1: std.os.timespec = undefined;
    std.os.clock_gettime(std.os.CLOCK_REALTIME, &ts1) catch unreachable;

    const program = roc__mainForHost_1_exposed();

    call_the_closure(program);

    var ts2: std.os.timespec = undefined;
    std.os.clock_gettime(std.os.CLOCK_REALTIME, &ts2) catch unreachable;

    const delta = to_seconds(ts2) - to_seconds(ts1);

    const stderr = std.io.getStdErr().writer();
    stderr.print("runtime: {d:.3}ms\n", .{delta * 1000}) catch unreachable;

    return 0;
}

fn to_seconds(tms: std.os.timespec) f64 {
    return @intToFloat(f64, tms.tv_sec) + (@intToFloat(f64, tms.tv_nsec) / 1_000_000_000.0);
}

fn call_the_closure(program: Program) void {
    const allocator = std.heap.page_allocator;
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();

    var buf: [1000]u8 = undefined;

    var model = init(allocator);

    while (true) {
        const line = (stdin.readUntilDelimiterOrEof(buf[0..], '\n') catch unreachable) orelse return;

        if (line.len == 1 and line[0] == 'q') {
            return;
        }

        const to_append = RocStr.init(line.ptr, line.len);

        model = update(allocator, model, to_append);

        const viewed = view(model);
        for (viewed.asSlice()) |char| {
            stdout.print("{c}", .{char}) catch unreachable;
        }

        stdout.print("\n", .{}) catch unreachable;
    }

    // The closure returns result, nothing interesting to do with it
    return;
}

pub export fn roc_fx_putInt(int: i64) i64 {
    const stdout = std.io.getStdOut().writer();

    stdout.print("{d}", .{int}) catch unreachable;

    stdout.print("\n", .{}) catch unreachable;

    return 0;
}

export fn roc_fx_putLine(rocPath: str.RocStr) callconv(.C) void {
    const stdout = std.io.getStdOut().writer();

    for (rocPath.asSlice()) |char| {
        stdout.print("{c}", .{char}) catch unreachable;
    }

    stdout.print("\n", .{}) catch unreachable;
}

const GetInt = extern struct {
    value: i64,
    error_code: bool,
    is_error: bool,
};

comptime {
    if (@sizeOf(usize) == 8) {
        @export(roc_fx_getInt_64bit, .{ .name = "roc_fx_getInt" });
    } else {
        @export(roc_fx_getInt_32bit, .{ .name = "roc_fx_getInt" });
    }
}

fn roc_fx_getInt_64bit() callconv(.C) GetInt {
    if (roc_fx_getInt_help()) |value| {
        const get_int = GetInt{ .is_error = false, .value = value, .error_code = false };
        return get_int;
    } else |err| switch (err) {
        error.InvalidCharacter => {
            return GetInt{ .is_error = true, .value = 0, .error_code = false };
        },
        else => {
            return GetInt{ .is_error = true, .value = 0, .error_code = true };
        },
    }

    return 0;
}

fn roc_fx_getInt_32bit(output: *GetInt) callconv(.C) void {
    if (roc_fx_getInt_help()) |value| {
        const get_int = GetInt{ .is_error = false, .value = value, .error_code = false };
        output.* = get_int;
    } else |err| switch (err) {
        error.InvalidCharacter => {
            output.* = GetInt{ .is_error = true, .value = 0, .error_code = false };
        },
        else => {
            output.* = GetInt{ .is_error = true, .value = 0, .error_code = true };
        },
    }

    return;
}

fn roc_fx_getInt_help() !i64 {
    const stdin = std.io.getStdIn().reader();
    var buf: [40]u8 = undefined;

    const line: []u8 = (try stdin.readUntilDelimiterOrEof(&buf, '\n')) orelse "";

    return std.fmt.parseInt(i64, line, 10);
}

fn readLine() []u8 {
    const stdin = std.io.getStdIn().reader();
    return (stdin.readUntilDelimiterOrEof(&line_buf, '\n') catch unreachable) orelse "";
}
