extern fn js_called_directly_from_roc() i32;
extern fn js_called_indirectly_from_roc() i32;
extern fn js_called_directly_from_main() i32;
extern fn js_called_indirectly_from_main() i32;
extern fn js_unused() i32;

extern fn roc__app_proc_1_exposed() i32;

export fn host_called_indirectly_from_roc() i32 {
    return 0x40;
}

export fn host_called_directly_from_roc() i32 {
    return 0x80 | host_called_indirectly_from_roc() | js_called_indirectly_from_roc();
}

export fn host_called_indirectly_from_main() i32 {
    return 0x100;
}

export fn host_called_directly_from_main() i32 {
    return 0x200 | host_called_indirectly_from_main() | js_called_indirectly_from_main();
}

export fn host_unused() i32 {
    // Call some functions from here to get them included in the output file
    return 0x400 | js_unused() | js_called_directly_from_roc();
}


// Result is an extern global so the test can read it from the Wasm module
extern var host_result: i32;

pub fn main() !void {
    const host = host_called_directly_from_main();
    const js = js_called_directly_from_main();
    const app = roc__app_proc_1_exposed();
    host_result = host | js | app;

    if (@import("builtin").target.cpu.arch != .wasm32) {
        const stdout = @import("std").io.getStdOut().writer();
        try stdout.print("{}\n", .{host_result});
    }
}
