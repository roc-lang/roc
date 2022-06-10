extern fn roc_js_called_directly_from_roc(x: i32) i32;
extern fn roc_js_called_indirectly_from_roc(x: i32) i32;
extern fn roc_js_called_directly_from_main(x: i32) i32;
extern fn roc_js_called_indirectly_from_main(x: i32) i32;
extern fn roc_js_unused(x: i32) i32;

var roc_host_internal_state: i32 = 0;

export fn roc_host_called_indirectly_from_roc(x: i32) i32 {
    roc_host_internal_state += x;
    return roc_host_internal_state;
}

export fn roc_host_called_directly_from_roc(x: i32) i32 {
    return roc_host_called_indirectly_from_roc(x) + roc_js_called_indirectly_from_roc(2);
}

export fn roc_host_called_indirectly_from_main(x: i32) i32 {
    roc_host_internal_state += x;
    return roc_host_internal_state * 4;
}

export fn roc_host_called_directly_from_main(x: i32) i32 {
    return roc_host_called_indirectly_from_main(x) + roc_js_called_indirectly_from_main(16);
}

export fn roc_host_unused(x: i32) i32 {
    // Call some functions from here to get them included in the output file,
    // without having to do a more complicated build
    roc_host_internal_state += x;
    return roc_host_internal_state + roc_js_unused(123) + roc_js_called_directly_from_roc(456);
}

pub fn main() !void {
    roc_host_internal_state = roc_host_called_directly_from_main(roc_host_internal_state) + roc_js_called_directly_from_main(roc_host_internal_state);
}
