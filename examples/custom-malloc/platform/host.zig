const std = @import("std");
const str = @import("str");
const RocStr = str.RocStr;
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

const mem = std.mem;
const Allocator = mem.Allocator;

extern fn roc__mainForHost_1_exposed([*]u8) void;
extern fn roc__mainForHost_1_size() i64;
extern fn roc__mainForHost_1_Fx_caller(*const u8, *const u8, [*]u8, [*]u8) void;
extern fn roc__mainForHost_1_Fx_size() i64;
extern fn roc__mainForHost_1_Fx_result_size() i64;

extern fn malloc(size: usize) callconv(.C) ?*c_void;
extern fn realloc(c_ptr: [*]align(@alignOf(u128)) u8, size: usize) callconv(.C) ?*c_void;
extern fn free(c_ptr: [*]align(@alignOf(u128)) u8) callconv(.C) void;

const Unit = extern struct {};

pub export fn main() u8 {
    const stdout = std.io.getStdOut().writer();

    const size = @intCast(usize, roc__mainForHost_1_size());
    const raw_output = std.heap.c_allocator.alloc(u8, size) catch unreachable;
    var output = @ptrCast([*]u8, raw_output);

    defer {
        std.heap.c_allocator.free(raw_output);
    }

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

    return 0;
}

export fn roc_alloc(alignment: usize, size: usize) callconv(.C) ?*c_void {
    const stdout = std.io.getStdOut().writer();
    const allocator = testing.allocator;

    // Perform the actual malloc
    const startNs = std.time.nanoTimestamp();
    const ptr = malloc(size) orelse return null;
    const endNs = std.time.nanoTimestamp();

    const totalMs = @divTrunc(endNs - startNs, 1000);

    stdout.print("\x1B[36m{} | \x1B[39m Custom malloc allocated {} bytes in {} ms!\n", .{startNs, size, totalMs}) catch unreachable;

    return ptr;
}

export fn roc_realloc(alignment: usize, c_ptr: *c_void, old_size: usize, new_size: usize) callconv(.C) ?*c_void {
    return realloc(@alignCast(16, @ptrCast([*]u8, c_ptr)), new_size);
}

export fn roc_dealloc(alignment: usize, c_ptr: *c_void) callconv(.C) void {
    const stdout = std.io.getStdOut().writer();
    const allocator = testing.allocator;

    // Perform the actual free
    const startNs = std.time.nanoTimestamp();
    free(@alignCast(16, @ptrCast([*]u8, c_ptr)));
    const endNs = std.time.nanoTimestamp();

    const totalMs = @divTrunc(endNs - startNs, 1000);

    stdout.print("\x1B[36m{} | \x1B[39m Custom dealloc ran in {} ms!\n", .{startNs, totalMs}) catch unreachable;
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

    const u8_ptr = rocPath.asU8ptr();

    var i: usize = 0;
    while (i < rocPath.len()) {
        stdout.print("{c}", .{u8_ptr[i]}) catch unreachable;

        i += 1;
    }

    stdout.print("\n", .{}) catch unreachable;

    return 0;
}

pub const ReadResult = extern struct {
    bytes: RocStr, // TODO RocList<u8> once Roc supports U8
    errno: i64, // TODO i32 when Roc supports I32
};

pub const WriteResult = extern struct {
    errno: i64,
};

pub export fn roc_fx_readAllUtf8(rocPath: RocStr) callconv(.C) ReadResult {
    var dir = std.fs.cwd();

    var content = dir.readFileAlloc(testing.allocator, rocPath.asSlice(), 1024) catch |e| switch (e) {
        error.FileNotFound => return .{ .bytes = RocStr.empty(), .errno = 2 },
        error.IsDir => return .{ .bytes = RocStr.empty(), .errno = 19 },
        else => return .{ .bytes = RocStr.empty(), .errno = 9999 },
    };

    var str_ptr = @ptrCast([*]u8, content);
    var roc_str3 = RocStr.init(str_ptr, content.len);

    return .{ .bytes = roc_str3, .errno = 0 };
}

pub export fn roc_fx_writeAllUtf8(filePath: RocStr, content: RocStr) callconv(.C) WriteResult {
    var dir = std.fs.cwd();

    dir.writeFile(filePath.asSlice(), content.asSlice()) catch |e| switch (e) {
        else => return .{ .errno = 1 },
    };

    return .{ .errno = 0 };
}
