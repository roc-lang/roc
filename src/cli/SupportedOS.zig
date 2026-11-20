//! The operating system this build targets, coarsened to the platforms the REPL
//! supports. The line editor talks to a terminal, so all it needs to know is
//! whether that terminal is a POSIX tty or a Windows console; anything finer is
//! handled by `Unix.zig` and `Windows.zig`.

const builtin = @import("builtin");

/// The platforms the REPL distinguishes: a POSIX tty, or a Windows console.
pub const SupportedOS = enum { windows, posix };

/// The operating system this build is targeting.
pub const current: SupportedOS = switch (builtin.os.tag) {
    .windows => .windows,
    .linux, .macos, .freebsd, .openbsd, .netbsd, .dragonfly => .posix,
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
    .driverkit,
    .ios,
    .maccatalyst,
    .tvos,
    .visionos,
    .watchos,
    .uefi,
    .@"3ds",
    .wiiu,
    .@"switch",
    .gba,
    .psx,
    .tios,
    .ashetos,
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
    => |tag| @compileError(@tagName(tag) ++ " is not a supported OS for ReplLine!"),
};
