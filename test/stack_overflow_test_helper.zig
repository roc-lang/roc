//! Helper executable for validating the compiler stack overflow handler output.

const std = @import("std");
const stack_overflow = @import("base").stack_overflow;

/// Install the compiler stack overflow handler and intentionally overflow the
/// stack so tests can validate the emitted crash message in a child process.
pub fn main() noreturn {
    if (!stack_overflow.install()) {
        std.debug.print("Failed to install stack overflow handler in helper process\n", .{});
        std.process.exit(98);
    }

    stack_overflow.triggerStackOverflowForTest();
}
