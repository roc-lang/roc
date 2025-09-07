const std = @import("std");
const builtin = @import("builtin");
const str = @import("glue/str.zig");
const RocStr = str.RocStr;
const testing = std.testing;
const expectEqual = testing.expectEqual;
const expect = testing.expect;
const maxInt = std.math.maxInt;

const mem = std.mem;
const Allocator = mem.Allocator;

const Program = extern struct { init: RocStr, update: Unit, view: Unit };

extern fn roc__main_for_host_1_exposed() Program;
extern fn roc__main_for_host_size() i64;

const ConstModel = [*]const u8;
const MutModel = [*]u8;

extern fn roc__main_for_host_0_caller([*]u8, [*]u8, MutModel) void;
extern fn roc__main_for_host_0_size() i64;
extern fn roc__main_for_host_0_result_size() i64;

fn allocate_model(allocator: *Allocator) MutModel {
    const size = roc__main_for_host_0_result_size();
    const raw_output = allocator.alignedAlloc(u8, @alignOf(u64), @as(usize, @intCast(size))) catch unreachable;
    const output = @as([*]u8, @ptrCast(raw_output));

    return output;
}

fn init(allocator: *Allocator) ConstModel {
    const closure: [*]u8 = undefined;
    const output = allocate_model(allocator);

    roc__main_for_host_0_caller(closure, closure, output);

    return output;
}

extern fn roc__main_for_host_1_caller(ConstModel, *const RocStr, [*]u8, MutModel) void;
extern fn roc__main_for_host_1_size() i64;
extern fn roc__main_for_host_1_result_size() i64;

fn update(allocator: *Allocator, model: ConstModel, msg: RocStr) ConstModel {
    const closure: [*]u8 = undefined;
    const output = allocate_model(allocator);

    roc__main_for_host_1_caller(model, &msg, closure, output);

    return output;
}

extern fn roc__main_for_host_2_caller(ConstModel, [*]u8, *RocStr) void;
extern fn roc__main_for_host_2_size() i64;
extern fn roc__main_for_host_2_result_size() i64;

fn view(input: ConstModel) RocStr {
    const closure: [*]u8 = undefined;
    var output: RocStr = undefined;

    roc__main_for_host_2_caller(input, closure, &output);

    return output;
}

fn print_output(viewed: RocStr) void {
    const stdout = std.fs.File.stdout().deprecatedWriter();

    for (viewed.asSlice()) |char| {
        stdout.print("{c}", .{char}) catch unreachable;
    }

    stdout.print("\n", .{}) catch unreachable;
}

const Align = 2 * @alignOf(usize);
extern fn malloc(size: usize) callconv(.c) ?*align(Align) anyopaque;
extern fn realloc(c_ptr: [*]align(Align) u8, size: usize) callconv(.c) ?*anyopaque;
extern fn free(c_ptr: [*]align(Align) u8) callconv(.c) void;
extern fn memcpy(dst: [*]u8, src: [*]u8, size: usize) callconv(.c) void;
extern fn memset(dst: [*]u8, value: i32, size: usize) callconv(.c) void;

const DEBUG: bool = false;

export fn roc_alloc(size: usize, alignment: u32) callconv(.c) ?*anyopaque {
    if (DEBUG) {
        const ptr = malloc(size);
        const stdout = std.fs.File.stdout().deprecatedWriter();
        stdout.print("alloc:   {d} (alignment {d}, size {d})\n", .{ ptr, alignment, size }) catch unreachable;
        return ptr;
    } else {
        return malloc(size);
    }
}

export fn roc_realloc(c_ptr: *anyopaque, new_size: usize, old_size: usize, alignment: u32) callconv(.c) ?*anyopaque {
    if (DEBUG) {
        const stdout = std.fs.File.stdout().deprecatedWriter();
        stdout.print("realloc: {d} (alignment {d}, old_size {d})\n", .{ c_ptr, alignment, old_size }) catch unreachable;
    }

    return realloc(@as([*]align(Align) u8, @alignCast(@ptrCast(c_ptr))), new_size);
}

export fn roc_dealloc(c_ptr: *anyopaque, alignment: u32) callconv(.c) void {
    if (DEBUG) {
        const stdout = std.fs.File.stdout().deprecatedWriter();
        stdout.print("dealloc: {d} (alignment {d})\n", .{ c_ptr, alignment }) catch unreachable;
    }

    free(@as([*]align(Align) u8, @alignCast(@ptrCast(c_ptr))));
}

export fn roc_panic(msg: *RocStr, tag_id: u32) callconv(.c) void {
    const stderr = std.io.getStdErr().writer();
    switch (tag_id) {
        0 => {
            stderr.print("Roc standard library crashed with message\n\n    {s}\n\nShutting down\n", .{msg.asSlice()}) catch unreachable;
        },
        1 => {
            stderr.print("Application crashed with message\n\n    {s}\n\nShutting down\n", .{msg.asSlice()}) catch unreachable;
        },
        else => unreachable,
    }
    std.process.exit(1);
}

export fn roc_dbg(loc: *RocStr, msg: *RocStr, src: *RocStr) callconv(.c) void {
    const stderr = std.io.getStdErr().writer();
    stderr.print("[{s}] {s} = {s}\n", .{ loc.asSlice(), src.asSlice(), msg.asSlice() }) catch unreachable;
}

export fn roc_memset(dst: [*]u8, value: i32, size: usize) callconv(.c) void {
    return memset(dst, value, size);
}

extern fn kill(pid: c_int, sig: c_int) c_int;
extern fn shm_open(name: *const i8, oflag: c_int, mode: c_uint) c_int;
extern fn mmap(addr: ?*anyopaque, length: c_uint, prot: c_int, flags: c_int, fd: c_int, offset: c_uint) *anyopaque;
extern fn getppid() c_int;

fn roc_getppid() callconv(.c) c_int {
    return getppid();
}

fn roc_getppid_windows_stub() callconv(.c) c_int {
    return 0;
}

fn roc_shm_open(name: *const i8, oflag: c_int, mode: c_uint) callconv(.c) c_int {
    return shm_open(name, oflag, mode);
}
fn roc_mmap(addr: ?*anyopaque, length: c_uint, prot: c_int, flags: c_int, fd: c_int, offset: c_uint) callconv(.c) *anyopaque {
    return mmap(addr, length, prot, flags, fd, offset);
}

comptime {
    if (builtin.os.tag == .macos or builtin.os.tag == .linux) {
        @export(roc_getppid, .{ .name = "roc_getppid", .linkage = .strong });
        @export(roc_mmap, .{ .name = "roc_mmap", .linkage = .strong });
        @export(roc_shm_open, .{ .name = "roc_shm_open", .linkage = .strong });
    }

    if (builtin.os.tag == .windows) {
        @export(roc_getppid_windows_stub, .{ .name = "roc_getppid", .linkage = .strong });
    }
}

const Unit = extern struct {};

pub export fn main() callconv(.c) u8 {
    const program = roc__main_for_host_1_exposed();

    call_the_closure(program);

    return 0;
}

fn call_the_closure(program: Program) void {
    _ = program;

    var allocator = std.heap.page_allocator;
    const stdin = std.io.getStdIn().reader();

    var buf: [1000]u8 = undefined;

    var model = init(&allocator);

    while (true) {
        print_output(view(model));

        const line = (stdin.readUntilDelimiterOrEof(buf[0..], '\n') catch unreachable) orelse return;

        if (line.len == 1 and line[0] == 'q') {
            return;
        }

        const to_append = RocStr.init(line.ptr, line.len);

        model = update(&allocator, model, to_append);
    }

    // The closure returns result, nothing interesting to do with it
    return;
}

pub export fn roc_fx_put_int(int: i64) i64 {
    const stdout = std.fs.File.stdout().deprecatedWriter();

    stdout.print("{d}", .{int}) catch unreachable;

    stdout.print("\n", .{}) catch unreachable;

    return 0;
}

export fn roc_fx_put_line(rocPath: str.RocStr) callconv(.c) void {
    const stdout = std.fs.File.stdout().deprecatedWriter();

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
        @export(roc_fx_get_int_64bit, .{ .name = "roc_fx_get_int" });
    } else {
        @export(roc_fx_get_int_32bit, .{ .name = "roc_fx_get_int" });
    }
}

fn roc_fx_get_int_64bit() callconv(.c) GetInt {
    if (roc_fx_get_int_help()) |value| {
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

fn roc_fx_get_int_32bit(output: *GetInt) callconv(.c) void {
    if (roc_fx_get_int_help()) |value| {
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

fn roc_fx_get_int_help() !i64 {
    const stdin = std.io.getStdIn().reader();
    var buf: [40]u8 = undefined;

    // make sure to strip `\r` on windows
    const raw_line: []u8 = (try stdin.readUntilDelimiterOrEof(&buf, '\n')) orelse "";
    const line = std.mem.trimRight(u8, raw_line, &std.ascii.whitespace);

    return std.fmt.parseInt(i64, line, 10);
}
