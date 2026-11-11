//! Self-relocation for libc-free Linux startup, before stored pointers or
//! instrumented runtime code can depend on the executable's chosen load base.

const std = @import("std");

/// `argc` and `argv` must come directly from the kernel's initial process stack.
pub inline fn relocate(argc: usize, argv: [*][*:0]u8) void {
    @setRuntimeSafety(false);
    @disableInstrumentation();
    const envp: [*:null]?[*:0]u8 = @ptrCast(argv + argc + 1);
    var env_count: usize = 0;
    while (envp[env_count] != null) : (env_count += 1) {}
    const auxv: [*]const std.elf.Auxv = @ptrCast(@alignCast(envp + env_count + 1));
    var phdr_address: usize = 0;
    var phdr_count: usize = 0;
    var aux_index: usize = 0;
    while (auxv[aux_index].a_type != std.elf.AT.NULL) : (aux_index += 1) {
        // Auxiliary vector keys are an open set of integers; only these two matter here.
        const entry = auxv[aux_index];
        if (entry.a_type == std.elf.AT.PHDR) {
            phdr_address = entry.a_un.a_val;
        } else if (entry.a_type == std.elf.AT.PHNUM) {
            phdr_count = entry.a_un.a_val;
        }
    }
    @call(.always_inline, std.pie.relocate, .{@as([*]const std.elf.ElfN.Phdr, @ptrFromInt(phdr_address))[0..phdr_count]});
}
