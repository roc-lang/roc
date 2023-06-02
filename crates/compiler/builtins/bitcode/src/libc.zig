const std = @import("std");
const builtin = @import("builtin");
const arch = builtin.cpu.arch;
const musl = @import("libc/musl.zig");
const folly = @import("libc/folly.zig");
const cpuid = @import("libc/cpuid.zig");

comptime {
    @export(memcpy, .{ .name = "roc_memcpy", .linkage = .Weak });
    @export(memcpy, .{ .name = "memcpy", .linkage = .Weak });
}

const Memcpy = fn (noalias [*]u8, noalias [*]const u8, len: usize) callconv(.C) [*]u8;

pub var memcpy_target: Memcpy = switch (arch) {
    // TODO: Switch to dispatch_memcpy once the surgical linker can support it.
    // .x86_64 => dispatch_memcpy,
    .x86_64 => musl.memcpy,
    else => unreachable,
};

pub fn memcpy(noalias dest: [*]u8, noalias src: [*]const u8, len: usize) callconv(.C) [*]u8 {
    switch (arch) {
        // x86_64 has a special optimized memcpy that can use avx2.
        .x86_64 => {
            return memcpy_target(dest, src, len);
        },
        else => {
            return musl.memcpy(dest, src, len);
        },
    }
}

fn dispatch_memcpy(noalias dest: [*]u8, noalias src: [*]const u8, len: usize) callconv(.C) [*]u8 {
    switch (arch) {
        .x86_64 => {
            if (cpuid.supports_avx2()) {
                if (cpuid.supports_prefetchw()) {
                    memcpy_target = folly.memcpy_prefetchw;
                } else {
                    memcpy_target = folly.memcpy_prefetcht0;
                }
            } else {
                memcpy_target = musl.memcpy;
            }
        },
        else => unreachable,
    }
    return memcpy_target(dest, src, len);
}
