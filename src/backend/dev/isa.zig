//! The instruction sets the dev backend emits, and the rule for choosing
//! between them.

const std = @import("std");
const builtin = @import("builtin");
const roc_target = @import("roc_target");
const layout = @import("layout");
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

/// What a dev-backend code generator is instantiated for: its instruction set
/// and the calling convention of the code it emits. The generator reads
/// nothing else about its target at compile time, so every `RocTarget` with
/// the same `Abi` shares one instantiation and gets the same machine code;
/// what differs between those targets (object format, symbol names, CPU
/// level) is runtime data the generator's callers pass in.
pub const Abi = enum {
    x86_64_sysv,
    x86_64_windows,
    /// AAPCS64, on every operating system that is neither Apple's nor Windows.
    aarch64_aapcs,
    aarch64_apple,
    aarch64_windows,

    pub fn isa(self: Abi) Isa {
        return switch (self) {
            .x86_64_sysv, .x86_64_windows => .x86_64,
            .aarch64_aapcs, .aarch64_apple, .aarch64_windows => .aarch64,
        };
    }

    pub fn isWindows(self: Abi) bool {
        return switch (self) {
            .x86_64_windows, .aarch64_windows => true,
            .x86_64_sysv, .aarch64_aapcs, .aarch64_apple => false,
        };
    }

    /// The C ABI the generated code calls and is called through.
    pub fn cAbi(self: Abi) layout.abi.Target {
        return switch (self) {
            .x86_64_sysv => .x86_64_sysv,
            .x86_64_windows => .x86_64_windows,
            .aarch64_aapcs => .aarch64,
            .aarch64_apple => .aarch64_macho,
            .aarch64_windows => .aarch64_windows,
        };
    }
};

/// The code generator instantiation that compiles `target`, or null when the
/// dev backend has none for it.
pub fn abiOfOrNull(target: RocTarget) ?Abi {
    return switch (isaOfOrNull(target) orelse return null) {
        .x86_64 => if (target.isWindows()) .x86_64_windows else .x86_64_sysv,
        .aarch64 => switch (layout.abi.aarch64Target(target.toOsTag())) {
            .aarch64 => .aarch64_aapcs,
            .aarch64_macho => .aarch64_apple,
            .aarch64_windows => .aarch64_windows,
            .x86_64_sysv, .x86_64_windows, .wasm32, .wasm32v1, .wasm64 => unreachable, // aarch64Target returns only AArch64 ABIs
        },
    };
}

/// The code generator instantiation that compiles `target`. A target without
/// one is a compile error.
pub fn abiOf(comptime target: RocTarget) Abi {
    return comptime abiOfOrNull(target) orelse
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

test "abiOfOrNull agrees with each target's instruction set, Windows-ness and C ABI" {
    inline for (@typeInfo(RocTarget).@"enum".field_names) |name| {
        const target = @field(RocTarget, name);
        if (abiOfOrNull(target)) |abi| {
            try std.testing.expectEqual(isaOfOrNull(target).?, abi.isa());
            try std.testing.expectEqual(target.isWindows(), abi.isWindows());
            const c_abi: layout.abi.Target = switch (abi.isa()) {
                .x86_64 => if (target.isWindows()) .x86_64_windows else .x86_64_sysv,
                .aarch64 => layout.abi.aarch64Target(target.toOsTag()),
            };
            try std.testing.expectEqual(c_abi, abi.cAbi());
        } else {
            try std.testing.expectEqual(@as(?Isa, null), isaOfOrNull(target));
        }
    }
}

test "abiOf maps the native RocTargets" {
    try std.testing.expectEqual(Abi.x86_64_sysv, comptime abiOf(.x64musl));
    try std.testing.expectEqual(Abi.x86_64_sysv, comptime abiOf(.x64mac));
    try std.testing.expectEqual(Abi.x86_64_windows, comptime abiOf(.x64v1win));
    try std.testing.expectEqual(Abi.x86_64_windows, comptime abiOf(.x64mingw));
    try std.testing.expectEqual(Abi.aarch64_apple, comptime abiOf(.arm64mac));
    try std.testing.expectEqual(Abi.aarch64_aapcs, comptime abiOf(.arm64linux));
    try std.testing.expectEqual(Abi.aarch64_windows, comptime abiOf(.arm64win));
}
