const builtin = @import("builtin");
const std = @import("std");
const RocStr = @import("str").RocStr;
const ArenaStack = @import("ArenaStack.zig");

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

extern fn roc__mainForHost_1_exposed(output: [*]u8) void;

extern fn roc__mainForHost_size() i64;

extern fn roc__mainForHost_1__Fx_caller(flags: *const u8, closure_data: [*]u8, output: [*]u8) void;

extern fn roc__mainForHost__Fx_size() i64;

extern fn roc__mainForHost_1__Fx_result_size() i64;

//extern fn memcpy(dst: [*]u8, src: [*]u8, size: usize) callconv(.C) void;
//extern fn memset(dst: [*]u8, value: i32, size: usize) callconv(.C) void;
//extern fn kill(pid: c_int, sig: c_int) c_int;
//extern fn shm_open(name: *const i8, oflag: c_int, mode: c_uint) c_int;
//extern fn mmap(addr: ?*anyopaque, length: c_uint, prot: c_int, flags: c_int, fd: c_int, offset: c_uint) *anyopaque;
extern fn getppid() std.os.pid_t;
extern fn shm_open(name: [*:0]const u8, flag: c_int, mode: std.c.mode_t) c_int;

var arena_stack: ArenaStack = undefined;

export fn roc_alloc(size: usize, alignment: u32) callconv(.C) ?*anyopaque {
    return arena_stack.alloc(size, alignment);
}

export fn roc_realloc(c_ptr: *anyopaque, new_size: usize, old_size: usize, alignment: u32) callconv(.C) ?*anyopaque {
    return arena_stack.realloc(c_ptr, new_size, old_size, alignment);
}

export fn roc_dealloc(c_ptr: *anyopaque, alignment: u32) callconv(.C) void {
    arena_stack.dealloc(c_ptr, alignment);
}

export fn roc_panic(c_ptr: *anyopaque, tag_id: u32) callconv(.C) void {
    _ = tag_id;

    const stderr = std.io.getStdErr().writer();
    const msg = @ptrCast([*:0]const u8, c_ptr);
    stderr.print("Application crashed with message\n\n    {s}\n", .{msg}) catch unreachable;
    std.process.exit(0);
}

export fn roc_memcpy(dst: ?*anyopaque, src: ?*const anyopaque, size: usize) callconv(.C) void {
    _ = std.zig.c_builtins.__builtin_memcpy(dst, src, size);
}

export fn roc_memset(dst: ?*anyopaque, value: i32, size: usize) callconv(.C) void {
    _ = std.zig.c_builtins.__builtin_memset(dst, value, size);
}

export fn roc_getppid() callconv(.C) std.os.pid_t {
    return getppid();
}

export fn roc_shm_open(name: [*:0]const u8, oflag: c_int, mode: c_uint) callconv(.C) c_int {
    return shm_open(name, oflag, mode);
}

export fn roc_mmap(addr: ?*align(4096) anyopaque, length: c_uint, prot: c_uint, flags: c_uint, fd: c_int, offset: c_uint) callconv(.C) *anyopaque {
    return std.c.mmap(addr, length, prot, flags, fd, offset);
}

//export fn roc_kill(pid: c_int, sig: c_int) callconv(.C) c_int {
  //  return kill(pid, sig);
//}

pub export fn main() callconv(.C) u8 {
    var allocator = std.heap.page_allocator;

    // NOTE the return size can be zero, which will segfault. Always allocate at least 8 bytes
    const size = std.math.max(8, @intCast(usize, roc__mainForHost_size()));
    const raw_output = allocator.allocAdvanced(u8, @alignOf(u64), @intCast(usize, size), .at_least) catch unreachable;
    var output = @ptrCast([*]u8, raw_output);

    arena_stack = ArenaStack.init();
    defer arena_stack.deinit();

    roc__mainForHost_1_exposed(output);

    call_the_closure(output);

    return 0;
}

fn call_the_closure(closure_data_pointer: [*]u8) void {
    const allocator = std.heap.page_allocator;

    const size = roc__mainForHost_1__Fx_result_size();

    if (size == 0) {
        // the function call returns an empty record
        // allocating 0 bytes causes issues because the allocator will return a NULL pointer
        // So it's special-cased
        const flags: u8 = 0;
        var result: [1]u8 = .{0};
        roc__mainForHost_1__Fx_caller(&flags, closure_data_pointer, &result);

        return;
    }

    const raw_output = allocator.allocAdvanced(u8, @alignOf(u64), @intCast(usize, size), .at_least) catch unreachable;
    var output = @ptrCast([*]u8, raw_output);

    defer {
        allocator.free(raw_output);
    }

    const flags: u8 = 0;
    roc__mainForHost_1__Fx_caller(&flags, closure_data_pointer, output);

    return;
}

export fn roc_fx_stdoutWrite(rocPath: RocStr) callconv(.C) void {
    const stdout = std.io.getStdOut().writer();

    for (rocPath.asSlice()) |char| {
        stdout.print("{c}", .{char}) catch unreachable;
    }
}

pub export fn roc_fx_stdinRead() RocStr {
    return stdin_read_help() catch RocStr.empty();
}

fn stdin_read_help() !RocStr {
    const stdin = std.io.getStdIn().reader();
    var buf: [128]u8 = undefined;

    const line: []u8 = (try stdin.readUntilDelimiterOrEof(&buf, '\n')) orelse "";

    return RocStr.init(@ptrCast([*]const u8, line), line.len);
}

export fn roc_fx_arenaStart() callconv(.C) u8 {
    arena_stack.push();
    return 0;
}

export fn roc_fx_arenaEnd() callconv(.C) void {
    arena_stack.pop();
}
