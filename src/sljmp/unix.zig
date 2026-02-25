//! Unix implementation of setjmp/longjmp using C library functions.

const builtin = @import("builtin");

/// Platform-specific jmp_buf size for Unix systems.
pub const JmpBuf = switch (builtin.os.tag) {
    .macos => [48]c_int,
    .linux => switch (builtin.cpu.arch) {
        .aarch64 => [64]c_long,
        .x86_64 => [25]c_long,
        .arm => [32]c_longlong, // musl: unsigned long long __jb[32]
        .x86 => [6]c_ulong, // musl: unsigned long __jb[6]
        .amdgcn,
        .arc,
        .armeb,
        .thumb,
        .thumbeb,
        .aarch64_be,
        .avr,
        .bpfel,
        .bpfeb,
        .csky,
        .hexagon,
        .kalimba,
        .lanai,
        .loongarch32,
        .loongarch64,
        .m68k,
        .mips,
        .mipsel,
        .mips64,
        .mips64el,
        .msp430,
        .or1k,
        .nvptx,
        .nvptx64,
        .powerpc,
        .powerpcle,
        .powerpc64,
        .powerpc64le,
        .propeller,
        .riscv32,
        .riscv64,
        .s390x,
        .sparc,
        .sparc64,
        .spirv32,
        .spirv64,
        .ve,
        .wasm32,
        .wasm64,
        .xcore,
        .xtensa,
        => @compileError("Unsupported architecture for jmp_buf"),
    },
    .freestanding,
    .other,
    .contiki,
    .fuchsia,
    .hermit,
    .aix,
    .haiku,
    .hurd,
    .plan9,
    .rtems,
    .serenity,
    .zos,
    .dragonfly,
    .freebsd,
    .netbsd,
    .openbsd,
    .driverkit,
    .ios,
    .tvos,
    .visionos,
    .watchos,
    .illumos,
    .solaris,
    .windows,
    .uefi,
    .ps3,
    .ps4,
    .ps5,
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
    => @compileError("Unsupported OS for jmp_buf"),
};

/// Whether to use underscore-prefixed names.
/// - glibc/macOS: _setjmp/_longjmp don't save signal mask (faster)
/// - musl: only provides setjmp/longjmp (which also don't save signal mask)
const use_underscore = builtin.abi != .musl and builtin.abi != .musleabi and builtin.abi != .musleabihf;

/// Link to C library's setjmp variant that doesn't save signal mask.
pub const setjmp = @extern(*const fn (*JmpBuf) callconv(.c) c_int, .{
    .name = if (use_underscore) "_setjmp" else "setjmp",
});

/// Link to C library's longjmp variant.
pub const longjmp = @extern(*const fn (*JmpBuf, c_int) callconv(.c) noreturn, .{
    .name = if (use_underscore) "_longjmp" else "longjmp",
});
