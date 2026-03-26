const std = @import("std");
const stack_overflow = @import("stack_overflow.zig");

pub fn main() noreturn {
    if (!stack_overflow.install()) {
        std.debug.print("Failed to install stack overflow handler in helper process\n", .{});
        std.process.exit(98);
    }

    stack_overflow.triggerStackOverflowForTest();
}
