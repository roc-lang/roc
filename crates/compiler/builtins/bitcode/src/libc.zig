const std = @import("std");
const builtin = @import("builtin");
const arch = builtin.cpu.arch;

comptime {
    @export(memcpy, .{ .name = "roc_memcpy", .linkage = .Weak });
    @export(memcpy, .{ .name = "memcpy", .linkage = .Weak });
}

const Memcpy = fn (noalias [*]u8, noalias [*]const u8, len: usize) callconv(.C) [*]u8;

pub var memcpy_target: Memcpy = switch (arch) {
    .x86_64 => memcpy,
    else => unreachable,
};

pub fn memcpy(noalias dest: [*]u8, noalias src: [*]const u8, len: usize) callconv(.C) [*]u8 {
    switch (arch) {
        .x86_64 => {
            @memcpy(dest, src, len);
            return memcpy_target(dest, src, len);
        },
        .i386 => {
            @memcpy(dest, src, len);
            return dest;
        },
        .aarch64 => {
            @memcpy(dest, src, len);
            return dest;
        },
        .arm => {
            @memcpy(dest, src, len);
            return dest;
        },
        .wasm32 => {
            @memcpy(dest, src, len);
            return dest;
        },
        else => @compileError("Unsupported architecture for memcpy"),
    }
}
