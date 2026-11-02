//! The instruction sets the dev backend emits, and the rule for choosing
//! between them.

const std = @import("std");
const builtin = @import("builtin");
const roc_target = @import("roc_target");
const RocTarget = roc_target.RocTarget;

/// The instruction set a dev-backend component is instantiated for. Every
/// instruction-set choice in the shared driver (`LirCodeGen`, `FrameBuilder`,
/// `CallingConvention`) is an exhaustive `switch` on this, never `isa == .x`:
/// adding an instruction set then makes each choice a compile error until it
/// has an answer for the new one.
pub const Isa = enum {
    x86_64,
    aarch64,
};

/// The instruction set that `target` compiles to in the dev backend, or null
/// when the dev backend has no code generator for it. Both emitters write
/// little-endian code, so big-endian aarch64 is null with the other
/// architectures.
pub fn isaOfOrNull(target: RocTarget) ?Isa {
    return isaOfCpuArch(target.toCpuArch());
}

/// The instruction set of the machine this compiler runs on, or null when the
/// dev backend has no code generator for it. Read from the CPU architecture
/// itself, not from `RocTarget.detectNative`, which maps an unknown
/// architecture to an x64 target.
pub const host: ?Isa = isaOfCpuArch(builtin.cpu.arch);

fn isaOfCpuArch(arch: std.Target.Cpu.Arch) ?Isa {
    return switch (roc_target.classifyCpuArch(arch)) {
        .x86_64 => .x86_64,
        .aarch64 => .aarch64,
        .aarch64_be, .arm, .wasm32, .other => null,
    };
}

/// The instruction set that `target` compiles to in the dev backend. A target
/// without one is a compile error.
pub fn isaOf(comptime target: RocTarget) Isa {
    return comptime isaOfOrNull(target) orelse
        @compileError("the dev backend has no code generator for " ++ @tagName(target));
}

test "isaOfOrNull gives x64 targets x86_64, arm64 targets aarch64, others none" {
    // The target names are an independent record of each target's
    // architecture, so this does not restate isaOfOrNull's own switch.
    inline for (@typeInfo(RocTarget).@"enum".field_names) |name| {
        const expected: ?Isa = if (comptime std.mem.startsWith(u8, name, "x64"))
            .x86_64
        else if (comptime std.mem.startsWith(u8, name, "arm64"))
            .aarch64
        else
            null;
        try std.testing.expectEqual(expected, isaOfOrNull(@field(RocTarget, name)));
    }
}

test "isaOf maps the native RocTargets" {
    try std.testing.expectEqual(Isa.x86_64, comptime isaOf(.x64musl));
    try std.testing.expectEqual(Isa.x86_64, comptime isaOf(.x64v1win));
    try std.testing.expectEqual(Isa.aarch64, comptime isaOf(.arm64mac));
}
