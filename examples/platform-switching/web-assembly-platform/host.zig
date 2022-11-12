const str = @import("str");
const builtin = @import("builtin");
const RocStr = str.RocStr;

comptime {
    if (builtin.target.cpu.arch != .wasm32) {
        @compileError("This platform is for WebAssembly only. You need to pass `--target wasm32` to the Roc compiler.");
    }
}

const Align = extern struct { a: usize, b: usize };
extern fn malloc(size: usize) callconv(.C) ?*align(@alignOf(Align)) anyopaque;
extern fn realloc(c_ptr: [*]align(@alignOf(Align)) u8, size: usize) callconv(.C) ?*anyopaque;
extern fn free(c_ptr: [*]align(@alignOf(Align)) u8) callconv(.C) void;
extern fn memcpy(dest: *anyopaque, src: *anyopaque, count: usize) *anyopaque;

export fn roc_alloc(size: usize, alignment: u32) callconv(.C) ?*anyopaque {
    _ = alignment;

    return malloc(size);
}

export fn roc_realloc(c_ptr: *anyopaque, new_size: usize, old_size: usize, alignment: u32) callconv(.C) ?*anyopaque {
    _ = old_size;
    _ = alignment;

    return realloc(@alignCast(@alignOf(Align), @ptrCast([*]u8, c_ptr)), new_size);
}

export fn roc_dealloc(c_ptr: *anyopaque, alignment: u32) callconv(.C) void {
    _ = alignment;

    free(@alignCast(@alignOf(Align), @ptrCast([*]u8, c_ptr)));
}

export fn roc_memcpy(dest: *anyopaque, src: *anyopaque, count: usize) callconv(.C) void {
    _ = memcpy(dest, src, count);
}

// NOTE roc_panic is provided in the JS file, so it can throw an exception

extern fn roc__mainForHost_1_exposed(*RocStr) void;

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
