//! Compiler-owned stack probing for the machine-code shim.
//!
//! Safe Zig builds emit calls to `__zig_probe_stack` for large x86 and
//! x86_64 ELF and Mach-O stack frames. The shim is linked into programs whose
//! platform host need not carry Zig's compiler-rt, so the shim must define that
//! helper itself. Local binding resolves the generated calls without exposing
//! a compiler-private symbol to the platform link.

const builtin = @import("builtin");

// Zig's self-hosted x86 Debug backend does not emit this libcall. LLVM does
// in both safe modes; only retain the helper for that explicit codegen ABI.
const needs_zig_probe = builtin.zig_backend == .stage2_llvm and
    (builtin.mode == .Debug or builtin.mode == .ReleaseSafe) and
    (builtin.object_format == .elf or builtin.object_format == .macho) and
    (builtin.cpu.arch == .x86 or builtin.cpu.arch == .x86_64);

/// Keep the local helper in the shim object. Zig's compiler-generated call is
/// added after source reachability, so it cannot root the helper itself.
pub inline fn retain() void {
    if (builtin.os.tag == .windows) {
        asm volatile (if (builtin.cpu.arch == .aarch64) "// %[probe]" else "# %[probe:P]"
            :
            : [probe] "X" (&windowsProbeStack),
        );
    }
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

/// Windows stack-growth ABI, private to the shim just like the ELF helper.
/// LLVM passes bytes in RAX on x64, and sixteen-byte units in X15 on ARM64.
/// Mirrors Zig's compiler_rt/stack_probe.zig win_probe_stack_only.
fn windowsProbeStack() callconv(.naked) void {
    @setRuntimeSafety(false);
    if (builtin.cpu.arch == .x86_64) {
        const symbol = if (builtin.target.isMinGW()) "___chkstk_ms:\n" else "__chkstk:\n";
        asm volatile (symbol ++
                \\ pushq %%rcx
                \\ pushq %%rax
                \\ cmpq $0x1000, %%rax
                \\ leaq 24(%%rsp), %%rcx
                \\ jb 1f
                \\2:
                \\ subq $0x1000, %%rcx
                \\ testq %%rcx, (%%rcx)
                \\ subq $0x1000, %%rax
                \\ cmpq $0x1000, %%rax
                \\ ja 2b
                \\1:
                \\ subq %%rax, %%rcx
                \\ testq %%rcx, (%%rcx)
                \\ popq %%rax
                \\ popq %%rcx
                \\ retq
        );
    } else if (builtin.cpu.arch == .aarch64) {
        asm volatile (
            \\__chkstk:
            \\ lsl x16, x15, #4
            \\ mov x17, sp
            \\1:
            \\ sub x17, x17, #4096
            \\ subs x16, x16, #4096
            \\ ldr xzr, [x17]
            \\ b.gt 1b
            \\ ret
        );
    } else @compileError("unsupported Windows stack-probe architecture");
    unreachable;
}
