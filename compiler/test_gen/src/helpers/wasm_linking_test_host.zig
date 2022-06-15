extern fn js_called_directly_from_roc(x: i32) i32;
extern fn js_called_indirectly_from_roc(x: i32) i32;
extern fn js_called_directly_from_main(x: i32) i32;
extern fn js_called_indirectly_from_main(x: i32) i32;
extern fn js_unused(x: i32) i32;

extern fn app_proc(x: i32) i32;

// Something we can read from the test code
extern var host_result: i32;

export fn host_called_indirectly_from_roc(x: i32) i32 {
    host_result += x;
    return host_result;
}

export fn host_called_directly_from_roc(x: i32) i32 {
    return host_called_indirectly_from_roc(x) + js_called_indirectly_from_roc(2);
}

export fn host_called_indirectly_from_main(x: i32) i32 {
    host_result += x;
    return host_result * 4;
}

export fn host_called_directly_from_main(x: i32) i32 {
    return host_called_indirectly_from_main(x) + js_called_indirectly_from_main(16);
}

export fn host_unused(x: i32) i32 {
    // Call some functions from here to get them included in the output file,
    // without having to do a more complicated build
    host_result += x;
    return host_result + js_unused(123) + js_called_directly_from_roc(456);
}

pub fn main() !void {
    host_result += 1;
    const host = host_called_directly_from_main(11);
    const js = js_called_directly_from_main(22);
    const app = app_proc(33);
    host_result = host + js + app;

    if (@import("builtin").target.cpu.arch != .wasm32) {
        @import("std").debug.print("host_result = {}\n", .{host_result});
    }
}
