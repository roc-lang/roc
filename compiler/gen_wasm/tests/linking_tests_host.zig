extern fn js_called_directly_from_roc(x: i32) i32;
extern fn js_called_indirectly_from_roc(x: i32) i32;
extern fn js_called_directly_from_main(x: i32) i32;
extern fn js_called_indirectly_from_main(x: i32) i32;
extern fn js_unused(x: i32) i32;

var host_internal_state: i32 = 0;

export fn host_called_indirectly_from_roc(x: i32) i32
{
    host_internal_state += x;
    return host_internal_state;
}

export fn host_called_directly_from_roc(x: i32) i32
{
    return host_called_indirectly_from_roc(x) + js_called_indirectly_from_roc(2);
}

export fn host_called_indirectly_from_main(x: i32) i32
{
    host_internal_state += x;
    return host_internal_state * 4;
}

export fn host_called_directly_from_main(x: i32) i32
{
    return host_called_indirectly_from_main(x) + js_called_indirectly_from_main(16);
}

export fn host_unused(x: i32) i32
{
    // Call some functions from here to get them included in the output file,
    // without having to do a more complicated build
    host_internal_state += x;
    return host_internal_state + js_unused(123) + js_called_directly_from_roc(456);

}

pub fn main() !void {
    host_internal_state = host_called_directly_from_main(host_internal_state) + js_called_directly_from_main(host_internal_state);
}
