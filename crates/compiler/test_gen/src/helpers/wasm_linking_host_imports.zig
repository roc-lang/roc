// Definitions to allow us to run an all-Zig version of the linking test
// Allows us to calculate the expected answer!

extern fn host_called_directly_from_roc() i32;

export var host_result: i32 = 0;

export fn js_called_directly_from_roc() i32 {
    return 0x01;
}
export fn js_called_indirectly_from_roc() i32 {
    return 0x02;
}
export fn js_called_directly_from_main() i32 {
    return 0x04;
}
export fn js_called_indirectly_from_main() i32 {
    return 0x08;
}
export fn js_unused() i32 {
    return 0x10;
}

export fn roc__app_proc_1_exposed() i32 {
    return 0x20 | js_called_directly_from_roc() | host_called_directly_from_roc();
}
