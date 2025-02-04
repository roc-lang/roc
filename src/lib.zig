const std = @import("std");
const mem = std.mem;
const Allocator = std.mem.Allocator;

export fn roc_check(path: *const c_char) i32 {

    // TODO implement `roc check` for libroc
    _ = path;

    return 0;
}
