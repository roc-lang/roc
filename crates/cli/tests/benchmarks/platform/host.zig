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

extern fn roc__mainForHost_1_exposed_generic([*]u8) void;
extern fn roc__mainForHost_1_exposed_size() i64;
extern fn roc__mainForHost_0_caller(*const u8, [*]u8, [*]u8) void;
extern fn roc__mainForHost_0_size() i64;
extern fn roc__mainForHost_0_result_size() i64;

const Align = 2 * @alignOf(usize);
extern fn malloc(size: usize) callconv(.C) ?*align(Align) anyopaque;
extern fn realloc(c_ptr: [*]align(Align) u8, size: usize) callconv(.C) ?*anyopaque;
extern fn free(c_ptr: [*]align(Align) u8) callconv(.C) void;
extern fn memcpy(dst: [*]u8, src: [*]u8, size: usize) callconv(.C) void;
extern fn memset(dst: [*]u8, value: i32, size: usize) callconv(.C) void;

const DEBUG: bool = false;

export fn roc_alloc(size: usize, alignment: u32) callconv(.C) ?*anyopaque {
    if (DEBUG) {
        const ptr = malloc(size);
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

    return realloc(@as([*]align(Align) u8, @alignCast(@ptrCast(c_ptr))), new_size);
}

export fn roc_dealloc(c_ptr: *anyopaque, alignment: u32) callconv(.C) void {
    if (DEBUG) {
        const stdout = std.io.getStdOut().writer();
        stdout.print("dealloc: {d} (alignment {d})\n", .{ c_ptr, alignment }) catch unreachable;
    }

    free(@as([*]align(Align) u8, @alignCast(@ptrCast(c_ptr))));
}

export fn roc_panic(msg: *RocStr, tag_id: u32) callconv(.C) void {
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

export fn roc_dbg(loc: *RocStr, msg: *RocStr, src: *RocStr) callconv(.C) void {
    const stderr = std.io.getStdErr().writer();
    stderr.print("[{s}] {s} = {s}\n", .{ loc.asSlice(), src.asSlice(), msg.asSlice() }) catch unreachable;
}

export fn roc_memset(dst: [*]u8, value: i32, size: usize) callconv(.C) void {
    return memset(dst, value, size);
}

extern fn kill(pid: c_int, sig: c_int) c_int;
extern fn shm_open(name: *const i8, oflag: c_int, mode: c_uint) c_int;
extern fn mmap(addr: ?*anyopaque, length: c_uint, prot: c_int, flags: c_int, fd: c_int, offset: c_uint) *anyopaque;
extern fn getppid() c_int;

fn roc_getppid() callconv(.C) c_int {
    return getppid();
}

fn roc_getppid_windows_stub() callconv(.C) c_int {
    return 0;
}

fn roc_shm_open(name: *const i8, oflag: c_int, mode: c_uint) callconv(.C) c_int {
    return shm_open(name, oflag, mode);
}
fn roc_mmap(addr: ?*anyopaque, length: c_uint, prot: c_int, flags: c_int, fd: c_int, offset: c_uint) callconv(.C) *anyopaque {
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
    // The size might be zero; if so, make it at least 8 so that we don't have a nullptr
    const size = @max(@as(usize, @intCast(roc__mainForHost_1_exposed_size())), 8);
    const raw_output = roc_alloc(@as(usize, @intCast(size)), @alignOf(u64)) orelse {
        std.log.err("Memory allocation failed", .{});
        return 1;
    };
    const output = @as([*]u8, @ptrCast(raw_output));

    defer {
        roc_dealloc(raw_output, @alignOf(u64));
    }

    roc__mainForHost_1_exposed_generic(output);

    const closure_data_pointer = @as([*]u8, @ptrCast(output));

    call_the_closure(closure_data_pointer);

    return 0;
}

fn call_the_closure(closure_data_pointer: [*]u8) void {
    const allocator = std.heap.page_allocator;

    // The size might be zero; if so, make it at least 8 so that we don't have a nullptr
    const size = @max(roc__mainForHost_0_result_size(), 8);
    const raw_output = allocator.alignedAlloc(u8, @alignOf(u64), @as(usize, @intCast(size))) catch unreachable;
    const output = @as([*]u8, @ptrCast(raw_output));

    defer {
        allocator.free(raw_output);
    }

    const flags: u8 = 0;

    roc__mainForHost_0_caller(&flags, closure_data_pointer, output);

    // The closure returns result, nothing interesting to do with it
    return;
}

pub export fn roc_fx_putInt(int: i64) i64 {
    const stdout = std.io.getStdOut().writer();

    stdout.print("{d}", .{int}) catch unreachable;

    stdout.print("\n", .{}) catch unreachable;

    return 0;
}

export fn roc_fx_putLine(rocPath: *str.RocStr) callconv(.C) void {
    const stdout = std.io.getStdOut().writer();

    for (rocPath.asSlice()) |char| {
        stdout.print("{c}", .{char}) catch unreachable;
    }

    stdout.print("\n", .{}) catch unreachable;
}

const GetInt = extern struct {
    value: i64,
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
        const get_int = GetInt{ .is_error = false, .value = value };
        return get_int;
    } else |err| switch (err) {
        error.InvalidCharacter => {
            return GetInt{ .is_error = true, .value = 0 };
        },
        else => {
            return GetInt{ .is_error = true, .value = 0 };
        },
    }

    return 0;
}

fn roc_fx_getInt_32bit(output: *GetInt) callconv(.C) void {
    if (roc_fx_getInt_help()) |value| {
        const get_int = GetInt{ .is_error = false, .value = value };
        output.* = get_int;
    } else |err| switch (err) {
        error.InvalidCharacter => {
            output.* = GetInt{ .is_error = true, .value = 0 };
        },
        else => {
            output.* = GetInt{ .is_error = true, .value = 0 };
        },
    }

    return;
}

fn roc_fx_getInt_help() !i64 {
    const stdout = std.io.getStdOut().writer();
    stdout.print("Please enter an integer\n", .{}) catch unreachable;

    const stdin = std.io.getStdIn().reader();
    var buf: [40]u8 = undefined;

    // make sure to strip `\r` on windows
    const raw_line: []u8 = (try stdin.readUntilDelimiterOrEof(&buf, '\n')) orelse "";
    const line = std.mem.trimRight(u8, raw_line, &std.ascii.whitespace);

    return std.fmt.parseInt(i64, line, 10);
}
