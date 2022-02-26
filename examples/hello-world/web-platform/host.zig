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

const Align = extern struct { a: usize, b: usize };
extern fn malloc(size: usize) callconv(.C) ?*align(@alignOf(Align)) c_void;
extern fn realloc(c_ptr: [*]align(@alignOf(Align)) u8, size: usize) callconv(.C) ?*c_void;
extern fn free(c_ptr: [*]align(@alignOf(Align)) u8) callconv(.C) void;

export fn roc_alloc(size: usize, alignment: u32) callconv(.C) ?*c_void {
    _ = alignment;

    return malloc(size);
}

export fn roc_realloc(c_ptr: *c_void, new_size: usize, old_size: usize, alignment: u32) callconv(.C) ?*c_void {
    _ = old_size;
    _ = alignment;

    return realloc(@alignCast(@alignOf(Align), @ptrCast([*]u8, c_ptr)), new_size);
}

export fn roc_dealloc(c_ptr: *c_void, alignment: u32) callconv(.C) void {
    _ = alignment;

    free(@alignCast(@alignOf(Align), @ptrCast([*]u8, c_ptr)));
}

// NOTE roc_panic is provided in the JS file, so it can throw an exception

const mem = std.mem;
const Allocator = mem.Allocator;

extern fn roc__mainForHost_1_exposed(*RocStr) void;

const Unit = extern struct {};

extern fn js_display_roc_string(str_bytes: ?[*]u8, str_len: usize) void;

pub fn main() u8 {
    // actually call roc to populate the callresult
    var callresult = RocStr.empty();
    roc__mainForHost_1_exposed(&callresult);

    // display the result using JavaScript
    js_display_roc_string(callresult.asU8ptr(), callresult.len());

    callresult.deinit();

    return 0;
}
