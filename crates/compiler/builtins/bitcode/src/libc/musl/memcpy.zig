const builtin = @import("builtin");
const arch = builtin.cpu.arch;

comptime {
    switch (arch) {
        .x86_64 => {
            asm (@embedFile("memcpy-x86_64.S"));
        },
        else => unreachable,
    }
}

pub extern fn musl_memcpy(noalias dest: [*]u8, noalias src: [*]const u8, len: usize) callconv(.C) [*]u8;
