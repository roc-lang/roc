const std = @import("std");
const str = @import("str");
const RocStr = str.RocStr;

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

usingnamespace struct {
    extern fn roc__mainForHost_1_exposed(output: [*]u8) void;
    pub const roc_main = roc__mainForHost_1_exposed;

    extern fn roc__mainForHost_size() i64;
    pub const roc_main_size = roc__mainForHost_size;

    extern fn roc__mainForHost_1_Fx_caller(flags: *const u8, closure_data: [*]u8, output: [*]u8) void;
    pub const call_fx = roc__mainForHost_1_Fx_caller;

    extern fn roc__mainForHost_1_Fx_size() i64;
    pub const fx_size = roc__mainForHost_1_Fx_size;

    extern fn roc__mainForHost_1_Fx_result_size() i64;
    pub const fx_result_size = roc__mainForHost_1_Fx_result_size;
};

const Align = 2 * @alignOf(usize);
extern fn memcpy(dst: [*]u8, src: [*]u8, size: usize) callconv(.C) void;
extern fn memset(dst: [*]u8, value: i32, size: usize) callconv(.C) void;

const DEBUG: bool = true;

export fn roc_alloc(size: usize, alignment: u32) callconv(.C) ?*c_void {
    var ptr = @alignCast(Align, std.c.malloc(size));

    if (DEBUG) {
        const stdout = std.io.getStdOut().writer();
        stdout.print("alloc:   {d} (alignment {d}, size {d})\n", .{ ptr, alignment, size }) catch unreachable;
    }

    return ptr;
}

export fn roc_realloc(c_ptr: *c_void, new_size: usize, old_size: usize, alignment: u32) callconv(.C) ?*c_void {
    if (DEBUG) {
        const stdout = std.io.getStdOut().writer();
        stdout.print("realloc: {d} (alignment {d}, old_size {d})\n", .{ c_ptr, alignment, old_size }) catch unreachable;
    }

    return std.c.realloc(@alignCast(Align, @ptrCast([*]u8, c_ptr)), new_size);
}

export fn roc_dealloc(c_ptr: *c_void, alignment: u32) callconv(.C) void {
    if (DEBUG) {
        const stdout = std.io.getStdOut().writer();
        stdout.print("dealloc: {d} (alignment {d})\n", .{ c_ptr, alignment }) catch unreachable;
    }

    std.c.free(@alignCast(Align, @ptrCast([*]u8, c_ptr)));
}

export fn roc_panic(c_ptr: *c_void, tag_id: u32) callconv(.C) void {
    _ = tag_id;

    const stderr = std.io.getStdErr().writer();
    const msg = @ptrCast([*:0]const u8, c_ptr);
    stderr.print("Application crashed with message\n\n    {s}\n", .{msg}) catch unreachable;
    std.process.exit(0);
}

export fn roc_memcpy(dst: [*]u8, src: [*]u8, size: usize) callconv(.C) void{
    return memcpy(dst, src, size);
}

export fn roc_memset(dst: [*]u8, value: i32, size: usize) callconv(.C) void{
    return memset(dst, value, size);
}

const Unit = extern struct {};

pub export fn main() callconv(.C) u8 {
    const allocator = std.heap.page_allocator;

    const size = @intCast(usize, roc_main_size());
    const raw_output = allocator.allocAdvanced(u8, @alignOf(u64), @intCast(usize, size), .at_least) catch unreachable;
    var output = @ptrCast([*]u8, raw_output);

    defer {
        allocator.free(raw_output);
    }

    roc_main(output);

    const closure_data_pointer = @ptrCast([*]u8, output);

    call_the_closure(closure_data_pointer);

    return 0;
}

fn call_the_closure(closure_data_pointer: [*]u8) void {
    const allocator = std.heap.page_allocator;

    const size = @intCast(usize, fx_result_size());
    const raw_output = allocator.allocAdvanced(u8, @alignOf(u64), size, .at_least) catch unreachable;
    var output = @ptrCast([*]u8, raw_output);

    defer allocator.free(raw_output);

    const flags: u8 = 0;

    call_fx(&flags, closure_data_pointer, output);

    // The closure returns result, nothing interesting to do with it
    return;
}

export fn roc_fx_stdoutWrite(rocPath: str.RocStr) callconv(.C) void {
    const stdout = std.io.getStdOut().writer();

    for (rocPath.asSlice()) |char| {
        stdout.print("{c}", .{char}) catch unreachable;
    }
}

pub export fn roc_fx_stdinRead() str.RocStr {
    return stdin_read_help() catch RocStr.empty();
}

fn stdin_read_help() !RocStr {
    const stdin = std.io.getStdIn().reader();
    var buf: [128]u8 = undefined;

    const line: []u8 = (try stdin.readUntilDelimiterOrEof(&buf, '\n')) orelse "";

    return RocStr.init(@ptrCast([*]const u8, line), line.len);
}

export fn roc_fx_arenaStart() callconv(.C) void {
    // todo
}

export fn roc_fx_arenaEnd() callconv(.C) void {
    // todo
}
