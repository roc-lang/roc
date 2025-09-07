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

extern fn roc__main_for_host_1_exposed_generic([*]u8) void;
extern fn roc__main_for_host_1_exposed_size() i64;

const Align = 2 * @alignOf(usize);
extern fn malloc(size: usize) callconv(.c) ?*align(Align) anyopaque;
extern fn realloc(c_ptr: [*]align(Align) u8, size: usize) callconv(.c) ?*anyopaque;
extern fn free(c_ptr: [*]align(Align) u8) callconv(.c) void;
extern fn memcpy(dst: [*]u8, src: [*]u8, size: usize) callconv(.c) void;
extern fn memset(dst: [*]u8, value: i32, size: usize) void;
extern fn kill(pid: c_int, sig: c_int) c_int;
extern fn shm_open(name: *const i8, oflag: c_int, mode: c_uint) c_int;
extern fn mmap(addr: ?*anyopaque, length: c_uint, prot: c_int, flags: c_int, fd: c_int, offset: c_uint) *anyopaque;
extern fn getppid() c_int;

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
    const stderr = std.fs.File.stderr().deprecatedWriter();
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
    const stderr = std.fs.File.stderr().deprecatedWriter();
    stderr.print("[{s}] {s} = {s}\n", .{ loc.asSlice(), src.asSlice(), msg.asSlice() }) catch unreachable;
}

export fn roc_memset(dst: [*]u8, value: i32, size: usize) callconv(.c) void {
    return memset(dst, value, size);
}

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

pub export fn main() u8 {
    const allocator = std.heap.page_allocator;

    // NOTE the return size can be zero, which will segfault. Always allocate at least 8 bytes
    const size = @max(8, @as(usize, @intCast(roc__main_for_host_1_exposed_size())));
    const raw_output = allocator.alignedAlloc(u8, @alignOf(u64), @as(usize, @intCast(size))) catch unreachable;
    const output = @as([*]u8, @ptrCast(raw_output));

    defer {
        allocator.free(raw_output);
    }

    roc__main_for_host_1_exposed_generic(output);

    return 0;
}

pub export fn roc_fx_get_line() str.RocStr {
    return roc_fx_get_line_help() catch return str.RocStr.empty();
}

fn roc_fx_get_line_help() !RocStr {
    const stdin = std.io.getStdIn().reader();
    var buf: [400]u8 = undefined;

    const line: []u8 = (try stdin.readUntilDelimiterOrEof(&buf, '\n')) orelse "";

    return str.RocStr.init(@as([*]const u8, @ptrCast(line)), line.len);
}

pub export fn roc_fx_put_line(rocPath: *str.RocStr) i64 {
    const stdout = std.fs.File.stdout().deprecatedWriter();

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

pub export fn roc_fx_get_int() GetInt {
    if (roc_fx_get_int_help()) |value| {
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

fn roc_fx_get_int_help() !i64 {
    const stdin = std.io.getStdIn().reader();
    var buf: [40]u8 = undefined;

    // make sure to strip `\r` on windows
    const raw_line: []u8 = (try stdin.readUntilDelimiterOrEof(&buf, '\n')) orelse "";
    const line = std.mem.trimRight(u8, raw_line, &std.ascii.whitespace);

    return std.fmt.parseInt(i64, line, 10);
}

/// Just return the input for testing purposes
pub export fn roc_fx_id_effectful(input_num: u64) u64 {
    return input_num;
}
