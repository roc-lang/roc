//! Publish newly written instructions without depending on the host's compiler
//! runtime. All stores and relocations must precede this call; executable code
//! must not be published to callers until it completes.
const std = @import("std");
const builtin = @import("builtin");

/// OS-reported failure to publish newly written instructions.
pub const Error = error{FlushInstructionCacheFailed};

/// Synchronize the written byte range before changing permissions and publishing it.
pub fn flush(memory: []const u8) Error!void {
    if (memory.len == 0) return;
    switch (builtin.cpu.arch) {
        .x86, .x86_64 => {}, // Hardware maintains instruction/data cache coherence.
        .aarch64, .aarch64_be => switch (builtin.os.tag) {
            .linux => flushLinuxAarch64(memory),
            .macos => darwin.sys_icache_invalidate(memory.ptr, memory.len),
            .windows => {
                if (windows.FlushInstructionCache(std.os.windows.GetCurrentProcess(), memory.ptr, memory.len) == .FALSE)
                    return error.FlushInstructionCacheFailed;
            },
            .driverkit,
            .ios,
            .maccatalyst,
            .tvos,
            .visionos,
            .watchos,
            .freestanding,
            .other,
            .contiki,
            .fuchsia,
            .hermit,
            .managarm,
            .haiku,
            .hurd,
            .illumos,
            .plan9,
            .rtems,
            .serenity,
            .dragonfly,
            .freebsd,
            .openbsd,
            .netbsd,
            .uefi,
            .@"3ds",
            .ps3,
            .ps4,
            .ps5,
            .psp,
            .vita,
            .emscripten,
            .wasi,
            .amdhsa,
            .amdpal,
            .cuda,
            .mesa3d,
            .nvcl,
            .opencl,
            .opengl,
            .vulkan,
            => @compileError("instruction-cache synchronization is not implemented for this OS"),
        },
        .alpha,
        .amdgcn,
        .arc,
        .arceb,
        .arm,
        .armeb,
        .avr,
        .bpfeb,
        .bpfel,
        .csky,
        .hexagon,
        .hppa,
        .hppa64,
        .kalimba,
        .kvx,
        .lanai,
        .loongarch32,
        .loongarch64,
        .m68k,
        .microblaze,
        .microblazeel,
        .mips,
        .mipsel,
        .mips64,
        .mips64el,
        .msp430,
        .nvptx,
        .nvptx64,
        .or1k,
        .powerpc,
        .powerpcle,
        .powerpc64,
        .powerpc64le,
        .propeller,
        .riscv32,
        .riscv32be,
        .riscv64,
        .riscv64be,
        .s390x,
        .sh,
        .sheb,
        .sparc,
        .sparc64,
        .spirv32,
        .spirv64,
        .thumb,
        .thumbeb,
        .ve,
        .wasm32,
        .wasm64,
        .x86_16,
        .xcore,
        .xtensa,
        .xtensaeb,
        => @compileError("instruction-cache synchronization is not implemented for this architecture"),
    }
}

const darwin = struct {
    extern "c" fn sys_icache_invalidate(start: [*]const u8, len: usize) void;
};
const windows = struct {
    extern "kernel32" fn FlushInstructionCache(process: std.os.windows.HANDLE, start: [*]const u8, len: usize) callconv(.winapi) std.os.windows.BOOL;
};

fn flushLinuxAarch64(memory: []const u8) void {
    // Linux exposes a migration-safe CTR_EL0 value (emulating the read when
    // CPUs differ). Read it per publication rather than cache a CPU-local value.
    const ctr = asm volatile ("mrs %[ctr], ctr_el0"
        : [ctr] "=r" (-> u64),
    );
    const start = @intFromPtr(memory.ptr);
    const end = start + memory.len;
    if (ctr & (@as(u64, 1) << 28) == 0) { // IDC: clean to point of unification
        const line_size = @as(usize, 4) << @as(u6, @intCast((ctr >> 16) & 15));
        var address = std.mem.alignBackward(usize, start, line_size);
        while (address < end) : (address += line_size) {
            asm volatile ("dc cvau, %[address]"
                :
                : [address] "r" (address),
                : .{ .memory = true });
        }
        asm volatile ("dsb ish" ::: .{ .memory = true });
    } else {
        // IDC removes the clean, not the ordering of prior instruction stores.
        asm volatile ("dsb ishst" ::: .{ .memory = true });
    }
    if (ctr & (@as(u64, 1) << 29) == 0) { // DIC: invalidate to point of unification
        const line_size = @as(usize, 4) << @as(u6, @intCast(ctr & 15));
        var address = std.mem.alignBackward(usize, start, line_size);
        while (address < end) : (address += line_size) {
            asm volatile ("ic ivau, %[address]"
                :
                : [address] "r" (address),
                : .{ .memory = true });
        }
        // Complete invalidation before any subsequent instruction fetch.
        asm volatile ("dsb ish" ::: .{ .memory = true });
    }
    asm volatile ("isb" ::: .{ .memory = true });
}

test "new instructions execute after repeated writes to reused addresses" {
    if (builtin.os.tag != .linux or builtin.cpu.arch != .aarch64) return error.SkipZigTest;
    const linux = std.os.linux;
    const page_size = std.heap.pageSize();
    const length = 2 * page_size;
    const address = linux.mmap(null, length, .{ .READ = true, .WRITE = true, .EXEC = true }, .{ .TYPE = .PRIVATE, .ANONYMOUS = true }, -1, 0);
    if (linux.errno(address) != .SUCCESS) return error.MmapFailed;
    const memory: [*]align(std.heap.page_size_min) u8 = @ptrFromInt(address);
    defer _ = linux.munmap(memory, length);
    // Only this test uses RWX: changing permissions between generations would
    // let kernel cache maintenance hide a broken flush implementation.
    // The instructions straddle a page boundary, covering distinct cache lines
    // even on machines with larger line sizes. Each generation reuses the VA.
    const code = memory[page_size - 4 ..][0..8];
    const call: *const fn () callconv(.c) u32 = @ptrCast(@alignCast(code.ptr));
    for (0..128) |generation| {
        const value: u32 = @intCast(generation + 1);
        std.mem.writeInt(u32, code[0..4], 0x52800000 | (value << 5), .little); // mov w0, value
        std.mem.writeInt(u32, code[4..8], 0xd65f03c0, .little); // ret
        try flush(code);
        try std.testing.expectEqual(value, call());
    }
}
