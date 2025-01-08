const str = @import("glue").str;
const builtin = @import("builtin");
const RocStr = str.RocStr;

comptime {
    if (builtin.target.cpu.arch != .wasm32) {
        @compileError("This platform is for WebAssembly only. You need to pass `--target wasm32` to the Roc compiler.");
    }
}

const Align = 2 * @alignOf(usize);
extern fn malloc(size: usize) callconv(.C) ?*align(Align) anyopaque;
extern fn realloc(c_ptr: [*]align(Align) u8, size: usize) callconv(.C) ?*anyopaque;
extern fn free(c_ptr: [*]align(Align) u8) callconv(.C) void;
extern fn memcpy(dest: *anyopaque, src: *anyopaque, count: usize) *anyopaque;

export fn roc_alloc(size: usize, alignment: u32) callconv(.C) ?*anyopaque {
    _ = alignment;

    return malloc(size);
}

export fn roc_realloc(c_ptr: *anyopaque, new_size: usize, old_size: usize, alignment: u32) callconv(.C) ?*anyopaque {
    _ = old_size;
    _ = alignment;

    return realloc(@as([*]align(Align) u8, @alignCast(@ptrCast(c_ptr))), new_size);
}

export fn roc_dealloc(c_ptr: *anyopaque, alignment: u32) callconv(.C) void {
    _ = alignment;

    free(@as([*]align(Align) u8, @alignCast(@ptrCast(c_ptr))));
}

// NOTE roc_panic and roc_dbg is provided in the JS file, so it can throw an exception

extern fn roc__main_for_host_1_exposed(*RocStr) void;

extern fn js_display_roc_string(str_bytes: ?[*]u8, str_len: usize) void;

pub export fn main() u8 {
    // actually call roc to populate the callresult
    var callresult = RocStr.empty();
    roc__main_for_host_1_exposed(&callresult);

    // display the result using JavaScript
    js_display_roc_string(callresult.asU8ptrMut(), callresult.len());

    callresult.decref();

    return 0;
}
