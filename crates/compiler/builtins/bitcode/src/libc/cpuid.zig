const std = @import("std");
const builtin = @import("builtin");
const arch = builtin.cpu.arch;
const function_prefix = @import("assembly_util.zig").function_prefix;

// I couldn't manage to define this in a PIE friendly way with inline assembly.
// Instead, I am defining it as global assembly functions.
comptime {
    switch (arch) {
        .x86_64 => {
            asm (std.fmt.comptimePrint(@embedFile("cpuid.S"), .{ .function_prefix = function_prefix }));
        },
        else => unreachable,
    }
}

pub extern fn supports_avx2() bool;
pub extern fn supports_prefetchw() bool;
