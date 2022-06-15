// Definitions to allow us to run an all-Zig version of the linking test
// Allows us to calculate the expected answer!

extern fn host_called_directly_from_roc(x: i32) i32;

export var host_result: i32 = 0;

export fn js_called_directly_from_roc(x: i32) i32 {
    return x + 100;
}
export fn js_called_indirectly_from_roc(x: i32) i32 {
    return x + 100;
}
export fn js_called_directly_from_main(x: i32) i32 {
    return x + 100;
}
export fn js_called_indirectly_from_main(x: i32) i32 {
    return x + 100;
}
export fn js_unused(x: i32) i32 {
    return x + 100;
}

export fn app_proc(x: i32) i32 {
    return js_called_directly_from_roc(x) + host_called_directly_from_roc(x);
}
