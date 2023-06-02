const std = @import("std");
const builtin = @import("builtin");
const arch = builtin.cpu.arch;

comptime {
    switch (arch) {
        .x86_64 => {
            inline for ([_][]const u8{ "prefetchw", "prefetcht0" }) |prefetch| {
                asm (std.fmt.comptimePrint(@embedFile("memcpy-x86_64.S"), .{ .prefetch = prefetch }));
            }
        },
        else => unreachable,
    }
}

pub extern fn __folly_memcpy_prefetchw(noalias dest: [*]u8, noalias src: [*]const u8, len: usize) callconv(.C) [*]u8;
pub extern fn __folly_memcpy_prefetcht0(noalias dest: [*]u8, noalias src: [*]const u8, len: usize) callconv(.C) [*]u8;
