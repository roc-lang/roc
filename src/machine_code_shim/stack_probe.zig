//! Compiler-owned stack probing for the machine-code shim.
//!
//! ReleaseSafe Zig builds emit calls to `__zig_probe_stack` for large x86 and
//! x86_64 ELF and Mach-O stack frames. The shim is linked into programs whose
//! platform host need not carry Zig's compiler-rt, so the shim must define that
//! helper itself. Local binding resolves the generated calls without exposing
//! a compiler-private symbol to the platform link.

const builtin = @import("builtin");

const needs_zig_probe = builtin.mode == .ReleaseSafe and
    (builtin.object_format == .elf or builtin.object_format == .macho) and
    (builtin.cpu.arch == .x86 or builtin.cpu.arch == .x86_64);

/// Keep the local helper in the shim object. Zig's compiler-generated call is
/// added after source reachability, so it cannot root the helper itself.
pub inline fn retain() void {
    if (!needs_zig_probe) return;

    asm volatile (
        \\        # %[probe:P]
        :
        : [probe] "X" (&zigProbeStack),
    );
}

/// Touch every page in a large stack allocation before the caller moves its
/// stack pointer across the complete frame. The calling convention matches the
/// helper emitted by Zig: the requested byte count arrives in eax/rax.
/// The body mirrors Zig 0.16's `compiler_rt/stack_probe.zig` implementation.
fn zigProbeStack() callconv(.naked) void {
    @setRuntimeSafety(false);
    if (!needs_zig_probe) unreachable;

    const symbol = if (builtin.object_format == .macho)
        "___zig_probe_stack:\n"
    else
        "__zig_probe_stack:\n";

    if (builtin.cpu.arch == .x86_64) {
        asm volatile (symbol ++
                \\        push   %%rcx
                \\        mov    %%rax, %%rcx
                \\        cmp    $0x1000,%%rcx
                \\        jb     2f
                \\ 1:
                \\        sub    $0x1000,%%rsp
                \\        orl    $0,16(%%rsp)
                \\        sub    $0x1000,%%rcx
                \\        cmp    $0x1000,%%rcx
                \\        ja     1b
                \\ 2:
                \\        sub    %%rcx, %%rsp
                \\        orl    $0,16(%%rsp)
                \\        add    %%rax,%%rsp
                \\        pop    %%rcx
                \\        ret
        );
    } else if (builtin.cpu.arch == .x86) {
        asm volatile (symbol ++
                \\        push   %%ecx
                \\        mov    %%eax, %%ecx
                \\        cmp    $0x1000,%%ecx
                \\        jb     2f
                \\ 1:
                \\        sub    $0x1000,%%esp
                \\        orl    $0,8(%%esp)
                \\        sub    $0x1000,%%ecx
                \\        cmp    $0x1000,%%ecx
                \\        ja     1b
                \\ 2:
                \\        sub    %%ecx, %%esp
                \\        orl    $0,8(%%esp)
                \\        add    %%eax,%%esp
                \\        pop    %%ecx
                \\        ret
        );
    }

    unreachable;
}
