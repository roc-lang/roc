const builtin = @import("builtin");
const arch = builtin.cpu.arch;

comptime {
    switch (arch) {
        .x86_64 => {
            asm (
                \\ .global musl_memcpy;
                \\ musl_memcpy:
                \\     mov %rdi,%rax
                \\     cmp $8,%rdx
                \\     jc 1f
                \\     test $7,%edi
                \\     jz 1f
                \\ 2:  movsb
                \\     dec %rdx
                \\     test $7,%edi
                \\     jnz 2b
                \\ 1:  mov %rdx,%rcx
                \\     shr $3,%rcx
                \\     rep
                \\     movsq
                \\     and $7,%edx
                \\     jz 1f
                \\ 2:  movsb
                \\     dec %edx
                \\     jnz 2b
                \\ 1:  ret
            );
        },
        else => unreachable,
    }
}

pub extern fn musl_memcpy(noalias dest: [*]u8, noalias src: [*]const u8, len: usize) callconv(.C) [*]u8;
