pub fn exit_on_oom() void {
    // TOOD:  This should be a real message that is helpful.
    //        I don't know how anything is compiling right
    //        now without this file existing.
    @import("std").debug.panic("Compiler Error: OOM.", .{});
}
