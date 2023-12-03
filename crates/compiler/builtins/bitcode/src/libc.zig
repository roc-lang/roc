const std = @import("std");
const builtin = @import("builtin");
const arch = builtin.cpu.arch;
const musl = @import("libc/musl.zig");
const folly = @import("libc/folly.zig");
const cpuid = @import("libc/cpuid.zig");

comptime {
    // TODO: remove this workaround.
    // Our wasm llvm pipeline always links in memcpy.
    // As such, our impl will conflict.
    if (builtin.is_test) {
        // We don't need memcpy for tests because the tests are built with -lc
    } else if (arch != .wasm32) {
        @export(memcpy, .{ .name = "memcpy", .linkage = .Strong });
    }
}

const Memcpy = *const fn (noalias [*]u8, noalias [*]const u8, len: usize) callconv(.C) [*]u8;

pub var memcpy_target: Memcpy = switch (arch) {
    .x86_64 => dispatch_memcpy,
    else => unreachable,
};

pub fn memcpy(noalias dest: [*]u8, noalias src: [*]const u8, len: usize) callconv(.C) [*]u8 {
    switch (builtin.os.tag) {
        .windows => {
            return musl.memcpy(dest, src, len);
        },
        else => switch (arch) {
            // x86_64 has a special optimized memcpy that can use avx2.
            .x86_64 => {
                return memcpy_target(dest, src, len);
            },
            else => {
                return musl.memcpy(dest, src, len);
            },
        },
    }
}

const MemcpyDecision = enum {
    uninitialized,
    folly_prefetchw,
    folly_prefetcht0,
    musl,
};

var memcpy_decision: MemcpyDecision = .uninitialized;

fn dispatch_memcpy(noalias dest: [*]u8, noalias src: [*]const u8, len: usize) callconv(.C) [*]u8 {
    switch (arch) {
        .x86_64 => {
            // TODO: Switch this to overwrite the memcpy_target pointer once the surgical linker can support it.
            // Then dispatch will just happen on the first call instead of every call.
            // if (cpuid.supports_avx2()) {
            //     if (cpuid.supports_prefetchw()) {
            //         memcpy_target = folly.memcpy_prefetchw;
            //     } else {
            //         memcpy_target = folly.memcpy_prefetcht0;
            //     }
            // } else {
            //     memcpy_target = musl.memcpy;
            // }
            // return memcpy_target(dest, src, len);
            switch (memcpy_decision) {
                .uninitialized => {
                    if (cpuid.supports_avx2()) {
                        if (cpuid.supports_prefetchw()) {
                            memcpy_decision = .folly_prefetchw;
                        } else {
                            memcpy_decision = .folly_prefetcht0;
                        }
                    } else {
                        memcpy_decision = .musl;
                    }
                    return dispatch_memcpy(dest, src, len);
                },
                .folly_prefetchw => return folly.memcpy_prefetchw(dest, src, len),
                .folly_prefetcht0 => return folly.memcpy_prefetcht0(dest, src, len),
                .musl => return musl.memcpy(dest, src, len),
            }
        },
        else => unreachable,
    }
}
