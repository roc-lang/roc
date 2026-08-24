//! Roc target definitions - shared between build.zig and CLI
//!
//! This module is importable by build.zig (build-time) and CLI code (runtime).
//! It contains no dependencies on compiler modules like `parse`.

const std = @import("std");
const builtin = @import("builtin");

/// CPU architecture categories used by Roc's target-selection logic.
pub const CpuArchClass = enum { x86_64, aarch64, aarch64_be, arm, wasm32, other };

/// Classifies a Zig CPU architecture into the categories Roc distinguishes.
pub fn classifyCpuArch(arch: std.Target.Cpu.Arch) CpuArchClass {
    return switch (arch) {
        .x86_64 => .x86_64,
        .aarch64 => .aarch64,
        .aarch64_be => .aarch64_be,
        .arm => .arm,
        .wasm32 => .wasm32,
        .alpha,
        .amdgcn,
        .arc,
        .arceb,
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
        .wasm64,
        .x86_16,
        .x86,
        .xcore,
        .xtensa,
        .xtensaeb,
        => .other,
    };
}

/// Operating-system categories used by Roc's target-selection logic.
pub const OsClass = enum { macos, windows, linux, freebsd, openbsd, netbsd, other };

/// Classifies a Zig OS tag into the categories Roc distinguishes.
pub fn classifyOs(os: std.Target.Os.Tag) OsClass {
    return switch (os) {
        .macos => .macos,
        .windows => .windows,
        .linux => .linux,
        .freebsd => .freebsd,
        .openbsd => .openbsd,
        .netbsd => .netbsd,
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
        .driverkit,
        .ios,
        .maccatalyst,
        .tvos,
        .visionos,
        .watchos,
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
        => .other,
    };
}

const AbiClass = enum { musl, gnu, gnu_x32, other };

fn classifyAbi(abi: std.Target.Abi) AbiClass {
    return switch (abi) {
        .musl, .musleabi, .musleabihf => .musl,
        .gnu, .gnueabi, .gnueabihf => .gnu,
        .gnux32 => .gnu_x32,
        .none,
        .gnuabin32,
        .gnuabi64,
        .gnuf32,
        .gnusf,
        .eabi,
        .eabihf,
        .ilp32,
        .android,
        .androideabi,
        .muslabin32,
        .muslabi64,
        .muslf32,
        .muslsf,
        .muslx32,
        .msvc,
        .itanium,
        .simulator,
        .ohos,
        .ohoseabi,
        => .other,
    };
}

/// Roc's minimum supported macOS deployment target.
///
/// Keep this in one place because LLVM triples, Mach-O linker metadata, Mach-O
/// object metadata, and Zig-built platform host archives must all agree. If
/// these drift, `ld64.lld` warns when a host archive member advertises a newer
/// minimum OS than the final executable.
pub const macos_deployment = struct {
    pub const semantic_version: std.SemanticVersion = .{ .major = 11, .minor = 0, .patch = 0 };
    pub const linker_version = "11.0";
    pub const llvm_version = "11.0.0";
    pub const macho_encoded_version: u32 = 0x000b0000;
    pub const query_os_version: std.Target.Query.OsVersion = .{ .semver = semantic_version };

    pub fn query(arch: std.Target.Cpu.Arch) std.Target.Query {
        return .{
            .cpu_arch = arch,
            .os_tag = .macos,
            .os_version_min = query_os_version,
            .abi = .none,
        };
    }
};

/// Errors returned when a target cannot be expressed for a Mach-O link.
pub const MachoArchError = error{
    /// The CPU architecture is not one Mach-O links support in this compiler.
    UnsupportedMachoArch,
};

/// The Mach-O `-arch` name (`ld64.lld`'s spelling) for a CPU architecture.
///
/// Returns `error.UnsupportedMachoArch` for any architecture Roc does not link
/// as Mach-O, so an unexpected arch fails loudly instead of being silently
/// linked as the wrong architecture.
pub fn machoArchName(arch: std.Target.Cpu.Arch) MachoArchError![]const u8 {
    return switch (classifyCpuArch(arch)) {
        .aarch64 => "arm64",
        .x86_64 => "x86_64",
        .aarch64_be, .arm, .wasm32, .other => error.UnsupportedMachoArch,
    };
}

/// Dynamic-linker (`ld.so`) soname filenames, one authority for the bare
/// filenames that `glibcProgramInterpreter` and `RocTarget.getDynamicLinkerPath`
/// prefix with the ABI-defined absolute directory.
pub const ld_so = struct {
    /// glibc ld.so soname for x86_64.
    pub const glibc_x86_64 = "ld-linux-x86-64.so.2";
    /// glibc ld.so soname for aarch64.
    pub const glibc_aarch64 = "ld-linux-aarch64.so.1";
    /// glibc ld.so soname for 32-bit hard-float ARM.
    pub const glibc_arm = "ld-linux-armhf.so.3";
};

/// The absolute path a Linux kernel uses as the program interpreter of a
/// glibc-target executable.
///
/// The glibc ABI defines one canonical path per architecture. Every glibc
/// distro provides it: most store the real file there, and Debian-family
/// distros provide it as a `libc6` compatibility symlink to their multiarch
/// copy. The path is a constant of the target ABI, not a property of the
/// build machine, so writing any other path (such as the multiarch one)
/// produces an executable that fails with `ENOENT` on other distros. Returns
/// null for an architecture without a known canonical path.
pub fn glibcProgramInterpreter(arch: std.Target.Cpu.Arch) ?[]const u8 {
    return switch (classifyCpuArch(arch)) {
        .x86_64 => "/lib64/" ++ ld_so.glibc_x86_64,
        .aarch64 => "/lib/" ++ ld_so.glibc_aarch64,
        .arm => "/lib/" ++ ld_so.glibc_arm,
        .aarch64_be, .wasm32, .other => null,
    };
}

/// The absolute path a BSD kernel uses as an executable's program interpreter.
///
/// Each BSD ships exactly one loader at a fixed absolute path, so unlike Linux
/// (where the path depends on which libc the target links against) this is
/// determined by the OS alone. Keyed on `std.Target.Os.Tag` because the linker
/// carries a resolved OS tag rather than a `RocTarget`; returns null for any OS
/// whose loader is not addressed this way.
pub fn bsdProgramInterpreter(os_tag: std.Target.Os.Tag) ?[]const u8 {
    return switch (classifyOs(os_tag)) {
        .freebsd => "/libexec/ld-elf.so.1",
        .openbsd => "/usr/libexec/ld.so",
        .netbsd => "/usr/libexec/ld.elf_so",
        .macos, .windows, .linux, .other => null,
    };
}

/// How old a CPU the code generated for a target must run on.
///
/// Every Roc builtin and every operation Roc can express works at both levels.
/// The level only changes which instructions implement them, so lowering an
/// operation at `.v1` is always a matter of emitting a different sequence,
/// never of rejecting the program.
pub const CpuLevel = enum {
    /// The oldest revision of the architecture, named `v1` in target names
    /// (`x64v1musl`, `arm64v1glibc`, `wasm32v1`).
    ///
    /// - x86-64: the `x86-64` psABI level, i.e. SSE2 and nothing above it.
    ///   Runs on every AMD64 CPU, including K8, Core 2, and every Atom.
    /// - aarch64: Armv8.0-A. NEON is mandatory at this level, so Roc's 128-bit
    ///   integer SIMD is native here; only carryless multiply needs software
    ///   emulation. Runs on Cortex-A53/A57/A72, so on Raspberry Pi 3 and 4.
    /// - wasm32: the WebAssembly 1.0 core instruction set, with no SIMD.
    v1,

    /// The level Roc targets when a target name carries no `v1`.
    ///
    /// This is deliberately no higher than "AVX2 and its equivalents", because
    /// its whole purpose is to give the SIMD builtins native instructions and
    /// LLVM room to autovectorize:
    ///
    /// - x86-64: `x86-64-v3` (AVX2, BMI2, POPCNT, LZCNT) plus AES and
    ///   PCLMULQDQ, which the psABI level omits and the SIMD builtins use.
    /// - aarch64: Armv8.0-A plus AES and DotProd, the two extensions the SIMD
    ///   builtins lower to. Armv8.0 already provides the rest via NEON.
    /// - wasm32: the `simd128` proposal.
    default,
};

/// The single CPU contract shared by code generation and native platform
/// selection. `instruction_features` contains only requirements above the
/// architecture baseline; their transitive dependencies are derived here.
const CpuContract = struct {
    codegen_model: std.Target.Query.CpuModel = .determined_by_arch_os,
    architecture_baseline_features: ?std.Target.Cpu.Feature.Set = null,
    instruction_features: std.Target.Cpu.Feature.Set = .empty,

    fn requiredRuntimeFeatures(self: CpuContract, arch: std.Target.Cpu.Arch) std.Target.Cpu.Feature.Set {
        var required = self.instruction_features;
        required.populateDependencies(arch.allFeaturesList());
        return required;
    }

    /// The complete feature set code generation may use when this contract
    /// names both its scheduling model and architecture baseline.
    fn constrainedCodegenFeatures(self: CpuContract, arch: std.Target.Cpu.Arch) ?std.Target.Cpu.Feature.Set {
        var allowed = self.architecture_baseline_features orelse return null;
        allowed.addFeatureSet(self.instruction_features);
        allowed.populateDependencies(arch.allFeaturesList());
        return allowed;
    }

    fn applyToQuery(self: CpuContract, arch: std.Target.Cpu.Arch, query: *std.Target.Query) void {
        query.cpu_model = self.codegen_model;
        query.cpu_features_add.addFeatureSet(self.instruction_features);

        const allowed = self.constrainedCodegenFeatures(arch) orelse return;
        const codegen_model = switch (self.codegen_model) {
            .explicit => |model| model,
            .native, .baseline, .determined_by_arch_os => unreachable,
        };

        const model_features = codegen_model.toCpu(arch).features;

        // Preserve baseline features the scheduling model does not name.
        var missing_allowed_features = allowed;
        missing_allowed_features.removeFeatureSet(model_features);
        query.cpu_features_add.addFeatureSet(missing_allowed_features);

        // A named model also carries tuning and instruction features that are
        // not necessarily part of Roc's floor. Keep the name for scheduling,
        // but explicitly disable every model feature outside the contract.
        var undeclared_model_features = model_features;
        undeclared_model_features.removeFeatureSet(allowed);
        query.cpu_features_sub.addFeatureSet(undeclared_model_features);
    }
};

const TargetFamily = enum { macos, windows_msvc, windows_mingw, bsd, linux_dynamic, linux_static, elf, wasm };

/// The C runtime ABI selected by a Windows Roc target.
pub const WindowsAbi = enum { msvc, mingw };

/// Translate Zig's ABI spelling into one of Roc's supported Windows ABIs.
pub fn windowsAbiFromStd(abi: std.Target.Abi) ?WindowsAbi {
    return switch (abi) {
        .msvc => .msvc,
        .gnu => .mingw,
        .none,
        .gnuabin32,
        .gnuabi64,
        .gnueabi,
        .gnueabihf,
        .gnuf32,
        .gnusf,
        .gnux32,
        .eabi,
        .eabihf,
        .ilp32,
        .android,
        .androideabi,
        .musl,
        .muslabin32,
        .muslabi64,
        .musleabi,
        .musleabihf,
        .muslf32,
        .muslsf,
        .muslx32,
        .itanium,
        .simulator,
        .ohos,
        .ohoseabi,
        => null,
    };
}

/// Roc's simplified target representation.
/// Maps to specific OS/arch/ABI combinations for cross-compilation.
///
/// Targets whose generated code sits above their architecture's oldest
/// revision have a `v1` twin that sits exactly on it; see `CpuLevel`. Both
/// spellings of a target are separate names in a platform's `targets:` section
/// and separate directories under its inputs directory, because a host built
/// for one level cannot be linked into a binary promising the other.
pub const RocTarget = enum {
    // x64 (x86_64) targets
    x64mac,
    x64win,
    x64mingw,
    x64freebsd,
    x64openbsd,
    x64netbsd,
    x64musl,
    x64glibc,
    x64linux,
    x64elf,

    // x64 (x86_64) targets, baseline CPU
    x64v1mac,
    x64v1win,
    x64v1mingw,
    x64v1freebsd,
    x64v1openbsd,
    x64v1netbsd,
    x64v1musl,
    x64v1glibc,
    x64v1linux,
    x64v1elf,

    // arm64 (aarch64) targets
    arm64mac,
    arm64win,
    arm64mingw,
    arm64linux,
    arm64musl,
    arm64glibc,

    // arm64 (aarch64) targets, baseline CPU.
    //
    // There is no arm64v1mac: every Apple Silicon Mac is Armv8.4-A or newer,
    // so arm64mac already generates code its whole hardware range runs.
    arm64v1win,
    arm64v1mingw,
    arm64v1linux,
    arm64v1musl,
    arm64v1glibc,

    // arm32 targets
    arm32linux,
    arm32musl,

    // WebAssembly
    wasm32,
    wasm32v1,

    /// Parse target from string (e.g., "arm64mac", "x64musl")
    pub fn fromString(str: []const u8) ?RocTarget {
        const enum_info = @typeInfo(RocTarget);
        inline for (enum_info.@"enum".fields) |field| {
            if (std.mem.eql(u8, str, field.name)) {
                return @enumFromInt(field.value);
            }
        }
        return null;
    }

    /// Convert a std.Target to a RocTarget.
    /// This is the runtime equivalent of detectNative() which uses builtin.target.
    pub fn fromStdTarget(target: std.Target) RocTarget {
        const os = target.os.tag;
        const arch = target.cpu.arch;
        const abi = target.abi;

        switch (classifyCpuArch(arch)) {
            .x86_64 => {
                switch (classifyOs(os)) {
                    .macos => return .x64mac,
                    .windows => return switch (windowsAbiFromStd(abi).?) {
                        .msvc => .x64win,
                        .mingw => .x64mingw,
                    },
                    .freebsd => return .x64freebsd,
                    .openbsd => return .x64openbsd,
                    .netbsd => return .x64netbsd,
                    .linux => {
                        return switch (classifyAbi(abi)) {
                            .musl => .x64musl,
                            .gnu, .gnu_x32 => .x64glibc,
                            .other => .x64musl, // Default to musl for static linking
                        };
                    },
                    .other => return .x64elf, // Generic fallback
                }
            },
            .aarch64, .aarch64_be => {
                switch (classifyOs(os)) {
                    .macos => return .arm64mac,
                    .windows => return switch (windowsAbiFromStd(abi).?) {
                        .msvc => .arm64win,
                        .mingw => .arm64mingw,
                    },
                    .linux => {
                        return switch (classifyAbi(abi)) {
                            .musl => .arm64musl,
                            .gnu => .arm64glibc,
                            .gnu_x32, .other => .arm64musl, // Default to musl for static linking
                        };
                    },
                    .freebsd, .openbsd, .netbsd, .other => return .arm64linux, // Generic ARM64 Linux
                }
            },
            .arm => {
                switch (classifyOs(os)) {
                    .linux => return .arm32musl, // Default to musl for static linking
                    .macos, .windows, .freebsd, .openbsd, .netbsd, .other => return .arm32linux, // Generic ARM32 Linux
                }
            },
            .wasm32 => return .wasm32,
            .other => {
                // Default fallback based on OS
                switch (classifyOs(os)) {
                    .macos => return .x64mac,
                    .windows => return switch (windowsAbiFromStd(abi).?) {
                        .msvc => .x64win,
                        .mingw => .x64mingw,
                    },
                    .linux => return .x64musl, // Default to musl
                    .freebsd, .openbsd, .netbsd, .other => return .x64elf,
                }
            },
        }
    }

    /// Detect the current system's Roc target (compile-time)
    pub fn detectNative() RocTarget {
        return fromStdTarget(builtin.target);
    }

    /// Get the string name of this target (e.g., "arm64mac", "x64musl")
    pub fn toName(self: RocTarget) []const u8 {
        return @tagName(self);
    }

    /// The target this one draws its OS, architecture, and ABI from.
    ///
    /// A `v1` target maps to the target it is the baseline twin of; every
    /// other target maps to itself.
    ///
    /// Accessors that end in a catch-all arm route through this, because there
    /// a missed `v1` target would silently take the catch-all and answer wrong:
    /// `isStatic` would call `x64v1musl` dynamically linked. Accessors whose
    /// switch is exhaustive list their `v1` targets directly instead, since
    /// there the compiler already refuses to let one go unanswered.
    ///
    /// Every target is listed rather than using `else`, so adding a target
    /// fails to compile until its CPU level is declared here.
    pub fn defaultCpuTarget(self: RocTarget) RocTarget {
        return switch (self) {
            .x64v1mac => .x64mac,
            .x64v1win => .x64win,
            .x64v1mingw => .x64mingw,
            .x64v1freebsd => .x64freebsd,
            .x64v1openbsd => .x64openbsd,
            .x64v1netbsd => .x64netbsd,
            .x64v1musl => .x64musl,
            .x64v1glibc => .x64glibc,
            .x64v1linux => .x64linux,
            .x64v1elf => .x64elf,

            .arm64v1win => .arm64win,
            .arm64v1mingw => .arm64mingw,
            .arm64v1linux => .arm64linux,
            .arm64v1musl => .arm64musl,
            .arm64v1glibc => .arm64glibc,

            .wasm32v1 => .wasm32,

            .x64mac,
            .x64win,
            .x64mingw,
            .x64freebsd,
            .x64openbsd,
            .x64netbsd,
            .x64musl,
            .x64glibc,
            .x64linux,
            .x64elf,
            .arm64mac,
            .arm64win,
            .arm64mingw,
            .arm64linux,
            .arm64musl,
            .arm64glibc,
            .arm32linux,
            .arm32musl,
            .wasm32,
            => self,
        };
    }

    /// The `v1` twin of this target, or null when it has none.
    ///
    /// A target has no twin when Roc names no CPU floor for it: `arm32*` and
    /// `arm64mac` already generate code for every CPU that can run them, so
    /// there is nothing below them to drop to. A `v1` target is its own twin.
    ///
    /// Every target is listed rather than using `else`, so adding a target
    /// fails to compile until its baseline spelling is declared here.
    pub fn baselineCpuTarget(self: RocTarget) ?RocTarget {
        return switch (self) {
            .x64mac => .x64v1mac,
            .x64win => .x64v1win,
            .x64mingw => .x64v1mingw,
            .x64freebsd => .x64v1freebsd,
            .x64openbsd => .x64v1openbsd,
            .x64netbsd => .x64v1netbsd,
            .x64musl => .x64v1musl,
            .x64glibc => .x64v1glibc,
            .x64linux => .x64v1linux,
            .x64elf => .x64v1elf,

            .arm64win => .arm64v1win,
            .arm64mingw => .arm64v1mingw,
            .arm64linux => .arm64v1linux,
            .arm64musl => .arm64v1musl,
            .arm64glibc => .arm64v1glibc,

            .wasm32 => .wasm32v1,

            .x64v1mac,
            .x64v1win,
            .x64v1mingw,
            .x64v1freebsd,
            .x64v1openbsd,
            .x64v1netbsd,
            .x64v1musl,
            .x64v1glibc,
            .x64v1linux,
            .x64v1elf,
            .arm64v1win,
            .arm64v1mingw,
            .arm64v1linux,
            .arm64v1musl,
            .arm64v1glibc,
            .wasm32v1,
            => self,

            .arm64mac, .arm32linux, .arm32musl => null,
        };
    }

    /// How old a CPU this target's generated code must run on.
    pub fn cpuLevel(self: RocTarget) CpuLevel {
        return if (self.defaultCpuTarget() == self) .default else .v1;
    }

    fn family(self: RocTarget) TargetFamily {
        return switch (self) {
            .x64mac, .x64v1mac, .arm64mac => .macos,
            .x64win, .x64v1win, .arm64win, .arm64v1win => .windows_msvc,
            .x64mingw, .x64v1mingw, .arm64mingw, .arm64v1mingw => .windows_mingw,
            .x64freebsd,
            .x64openbsd,
            .x64netbsd,
            .x64v1freebsd,
            .x64v1openbsd,
            .x64v1netbsd,
            => .bsd,
            .x64glibc,
            .x64linux,
            .x64v1glibc,
            .x64v1linux,
            .arm64glibc,
            .arm64linux,
            .arm64v1glibc,
            .arm64v1linux,
            .arm32linux,
            => .linux_dynamic,
            .x64musl,
            .x64v1musl,
            .arm64musl,
            .arm64v1musl,
            .arm32musl,
            => .linux_static,
            .x64elf, .x64v1elf => .elf,
            .wasm32, .wasm32v1 => .wasm,
        };
    }

    /// Get the OS tag for this RocTarget
    pub fn toOsTag(self: RocTarget) std.Target.Os.Tag {
        return switch (self) {
            .x64mac, .x64v1mac, .arm64mac => .macos,
            .x64win, .x64v1win, .x64mingw, .x64v1mingw, .arm64win, .arm64v1win, .arm64mingw, .arm64v1mingw => .windows,
            .x64freebsd, .x64v1freebsd => .freebsd,
            .x64openbsd, .x64v1openbsd => .openbsd,
            .x64netbsd, .x64v1netbsd => .netbsd,
            .x64musl, .x64glibc, .x64linux, .x64elf, .arm64musl, .arm64glibc, .arm64linux, .arm32musl, .arm32linux => .linux,
            .x64v1musl, .x64v1glibc, .x64v1linux, .x64v1elf, .arm64v1musl, .arm64v1glibc, .arm64v1linux => .linux,
            .wasm32, .wasm32v1 => .freestanding,
        };
    }

    /// Get the CPU architecture for this RocTarget
    pub fn toCpuArch(self: RocTarget) std.Target.Cpu.Arch {
        return switch (self) {
            // x64 targets
            .x64mac, .x64win, .x64mingw, .x64freebsd, .x64openbsd, .x64netbsd, .x64musl, .x64glibc, .x64linux, .x64elf => .x86_64,
            .x64v1mac, .x64v1win, .x64v1mingw, .x64v1freebsd, .x64v1openbsd, .x64v1netbsd, .x64v1musl, .x64v1glibc, .x64v1linux, .x64v1elf => .x86_64,

            // arm64 targets
            .arm64mac, .arm64win, .arm64mingw, .arm64linux, .arm64musl, .arm64glibc => .aarch64,
            .arm64v1win, .arm64v1mingw, .arm64v1linux, .arm64v1musl, .arm64v1glibc => .aarch64,

            // arm32 targets
            .arm32linux, .arm32musl => .arm,

            // WebAssembly
            .wasm32, .wasm32v1 => .wasm32,
        };
    }

    /// The CPU model and instruction floor used by both code generation and
    /// runtime host compatibility. Adding an instruction here changes both.
    fn cpuContract(self: RocTarget) CpuContract {
        const arch = self.toCpuArch();
        const level = self.cpuLevel();

        var contract: CpuContract = switch (classifyCpuArch(arch)) {
            .x86_64 => switch (level) {
                .default => .{
                    .codegen_model = .{ .explicit = &std.Target.x86.cpu.x86_64_v3 },
                    .architecture_baseline_features = .empty,
                },
                .v1 => .{
                    .codegen_model = .{ .explicit = &std.Target.x86.cpu.x86_64 },
                    .architecture_baseline_features = .empty,
                },
            },
            .aarch64, .aarch64_be => if (self.toOsTag() == .macos)
                .{}
            else switch (level) {
                .default, .v1 => .{
                    .codegen_model = .{ .explicit = &std.Target.aarch64.cpu.generic },
                    .architecture_baseline_features = .empty,
                },
            },
            .wasm32 => if (level == .v1)
                .{
                    .codegen_model = .{ .explicit = &std.Target.wasm.cpu.mvp },
                    .architecture_baseline_features = .empty,
                }
            else
                .{},
            .arm, .other => .{},
        };

        if (contract.architecture_baseline_features) |initial| {
            var baseline = initial;
            switch (classifyCpuArch(arch)) {
                .x86_64 => {
                    // The x86-64 psABI v1 instruction set. Do not import the
                    // `x86_64` model's unrelated tuning flags.
                    inline for ([_]std.Target.x86.Feature{
                        .@"64bit",
                        .cmov,
                        .cx8,
                        .fxsr,
                        .mmx,
                        .nopl,
                        .sse2,
                        .x87,
                    }) |feature| {
                        baseline.addFeature(@intFromEnum(feature));
                    }
                },
                // Advanced SIMD and floating point are mandatory in the
                // application profile Roc targets at Armv8.0-A.
                .aarch64, .aarch64_be => baseline.addFeature(@intFromEnum(std.Target.aarch64.Feature.neon)),
                .wasm32 => {},
                .arm, .other => unreachable,
            }
            baseline.populateDependencies(arch.allFeaturesList());
            contract.architecture_baseline_features = baseline;
        }

        if (level == .v1) return contract;

        switch (classifyCpuArch(arch)) {
            .x86_64 => {
                // The complete x86-64-v3 ISA contract plus the two extensions
                // Roc adds to the named psABI level.
                inline for ([_]std.Target.x86.Feature{
                    .cx16,
                    .sahf,
                    .sse4_2,
                    .popcnt,
                    .avx2,
                    .bmi,
                    .bmi2,
                    .f16c,
                    .fma,
                    .lzcnt,
                    .movbe,
                    .xsave,
                    .aes,
                    .pclmul,
                }) |feature| {
                    contract.instruction_features.addFeature(@intFromEnum(feature));
                }
            },
            .aarch64, .aarch64_be => {
                // Every Apple Silicon CPU implements the macOS target floor.
                if (self.toOsTag() != .macos) {
                    contract.instruction_features.addFeature(@intFromEnum(std.Target.aarch64.Feature.aes));
                    contract.instruction_features.addFeature(@intFromEnum(std.Target.aarch64.Feature.dotprod));
                }
            },
            .wasm32 => contract.instruction_features.addFeature(@intFromEnum(std.Target.wasm.Feature.simd128)),
            .arm, .other => {},
        }

        return contract;
    }

    /// Instruction features above the architecture baseline that codegen may
    /// emit and runtime host detection must confirm.
    pub fn requiredRuntimeCpuFeatures(self: RocTarget) std.Target.Cpu.Feature.Set {
        return self.cpuContract().requiredRuntimeFeatures(self.toCpuArch());
    }

    /// Build the single target query used for every LLVM compilation of Roc
    /// program code, including linked applications, host shims, eval, and
    /// optimized `roc test` roots.
    pub fn llvmTargetQuery(self: RocTarget) std.Target.Query {
        var query = std.Target.Query{
            .cpu_arch = self.toCpuArch(),
            .os_tag = self.toOsTag(),
            .abi = switch (self.family()) {
                .linux_static => .musl,
                .linux_dynamic => .gnu,
                .windows_msvc => .msvc,
                .windows_mingw => .gnu,
                .macos, .bsd, .elf, .wasm => .none,
            },
        };
        if (self.toOsTag() == .macos) {
            query.os_version_min = macos_deployment.query_os_version;
        }

        self.cpuContract().applyToQuery(self.toCpuArch(), &query);

        return query;
    }

    /// Convert Roc target to LLVM target triple
    /// A `v1` target shares its default twin's triple: the CPU level is carried
    /// by the target machine's CPU model, not by the triple.
    pub fn toTriple(self: RocTarget) []const u8 {
        return switch (self) {
            // x64 targets
            .x64mac, .x64v1mac => "x86_64-apple-darwin",
            .x64win, .x64v1win => "x86_64-pc-windows-msvc",
            .x64mingw, .x64v1mingw => "x86_64-w64-windows-gnu",
            .x64freebsd, .x64v1freebsd => "x86_64-unknown-freebsd",
            .x64openbsd, .x64v1openbsd => "x86_64-unknown-openbsd",
            .x64netbsd, .x64v1netbsd => "x86_64-unknown-netbsd",
            .x64musl, .x64v1musl => "x86_64-unknown-linux-musl",
            .x64glibc, .x64v1glibc => "x86_64-unknown-linux-gnu",
            .x64linux, .x64v1linux => "x86_64-unknown-linux-gnu",
            .x64elf, .x64v1elf => "x86_64-unknown-none-elf",

            // arm64 targets
            .arm64mac => "aarch64-apple-darwin",
            .arm64win, .arm64v1win => "aarch64-pc-windows-msvc",
            .arm64mingw, .arm64v1mingw => "aarch64-w64-windows-gnu",
            .arm64linux, .arm64v1linux => "aarch64-unknown-linux-gnu",
            .arm64musl, .arm64v1musl => "aarch64-unknown-linux-musl",
            .arm64glibc, .arm64v1glibc => "aarch64-unknown-linux-gnu",

            // arm32 targets
            .arm32linux => "arm-unknown-linux-gnueabihf",
            .arm32musl => "arm-unknown-linux-musleabihf",

            // WebAssembly
            .wasm32, .wasm32v1 => "wasm32-unknown-unknown",
        };
    }

    /// Check if target uses dynamic linking (glibc targets)
    pub fn isDynamic(self: RocTarget) bool {
        return self.family() == .linux_dynamic;
    }

    /// Check if target uses static linking (musl targets)
    pub fn isStatic(self: RocTarget) bool {
        return self.family() == .linux_static;
    }

    /// Check if target is macOS
    pub fn isMacOS(self: RocTarget) bool {
        return self.family() == .macos;
    }

    /// Check if target is Windows
    pub fn isWindows(self: RocTarget) bool {
        return switch (self.family()) {
            .windows_msvc, .windows_mingw => true,
            .macos, .bsd, .linux_dynamic, .linux_static, .elf, .wasm => false,
        };
    }

    /// Return the selected Windows C runtime ABI, or null for non-Windows targets.
    pub fn windowsAbi(self: RocTarget) ?WindowsAbi {
        return switch (self.family()) {
            .windows_msvc => .msvc,
            .windows_mingw => .mingw,
            .macos, .bsd, .linux_dynamic, .linux_static, .elf, .wasm => null,
        };
    }

    /// Check if target is Linux-based
    pub fn isLinux(self: RocTarget) bool {
        const target_family = self.family();
        return target_family == .linux_dynamic or target_family == .linux_static;
    }

    /// Get the pointer bit width for this target
    pub fn ptrBitWidth(self: RocTarget) u16 {
        return switch (classifyCpuArch(self.toCpuArch())) {
            .x86_64, .aarch64, .aarch64_be => 64,
            .arm, .wasm32 => 32,
            .other => 64, // Default to 64-bit
        };
    }

    /// Check if this target has the same OS and CPU architecture as the current host.
    pub fn matchesHostOsAndArch(self: RocTarget) bool {
        return self.toOsTag() == builtin.target.os.tag and
            self.toCpuArch() == builtin.target.cpu.arch;
    }

    /// The complete LLVM target feature set, including scheduling and tuning
    /// flags that do not represent instruction-set requirements.
    fn llvmTargetFeatures(self: RocTarget) std.Target.Cpu.Feature.Set {
        const query = self.llvmTargetQuery();
        const arch = self.toCpuArch();
        const os = self.toOsTag().defaultVersionRange(arch, query.abi.?);
        var cpu = switch (query.cpu_model) {
            .explicit => |model| model.toCpu(arch),
            .baseline, .determined_by_arch_os => std.Target.Cpu.baseline(arch, os),
            // Roc target queries are static contracts and never inherit the
            // compiler process's CPU model.
            .native => unreachable,
        };

        cpu.features.removeFeatureSet(query.cpu_features_sub);
        cpu.features.addFeatureSet(query.cpu_features_add);
        cpu.features.populateDependencies(arch.allFeaturesList());
        cpu.features.removeFeatureSet(query.cpu_features_sub);
        return cpu.features;
    }

    /// Check if this target can be built on the current host.
    /// wasm32 is always compatible because wasm code generation is host-independent.
    /// Native targets are compatible if both OS and architecture match the host.
    pub fn isCompatibleWithHost(self: RocTarget) bool {
        // wasm32 can be built from any host
        if (self.toCpuArch() == .wasm32) return true;

        // Otherwise, check if both OS and architecture match
        return self.matchesHostOsAndArch();
    }

    /// Check if this target produces a process executable that can run on this host.
    /// This is intentionally stricter than build compatibility: wasm32 can be
    /// built on any host, but the default `roc` command does not execute wasm artifacts directly.
    pub fn isExecutableOnHost(self: RocTarget) bool {
        if (self.toCpuArch() == .wasm32) return false;

        return self.matchesHostOsAndArch();
    }

    /// Get the dynamic linker path for this target
    pub fn getDynamicLinkerPath(self: RocTarget) error{ StaticLinkingTarget, WindowsTarget, NoKnownLinkerPath, WebAssemblyTarget }![]const u8 {
        return switch (self) {
            // glibc targets
            .x64glibc,
            .x64linux,
            .x64v1glibc,
            .x64v1linux,
            .arm64glibc,
            .arm64linux,
            .arm64v1glibc,
            .arm64v1linux,
            .arm32linux,
            => glibcProgramInterpreter(self.toCpuArch()) orelse return error.NoKnownLinkerPath,

            // Static linking targets don't need dynamic linker
            .x64musl, .arm64musl, .arm32musl, .x64v1musl, .arm64v1musl => return error.StaticLinkingTarget,

            // macOS uses dyld
            .x64mac, .arm64mac, .x64v1mac => "/usr/lib/dyld",

            // Windows doesn't use ELF-style dynamic linker
            .x64win, .arm64win, .x64v1win, .arm64v1win, .x64mingw, .arm64mingw, .x64v1mingw, .arm64v1mingw => return error.WindowsTarget,

            // BSD variants
            .x64freebsd,
            .x64openbsd,
            .x64netbsd,
            .x64v1freebsd,
            .x64v1openbsd,
            .x64v1netbsd,
            => bsdProgramInterpreter(self.toOsTag()) orelse return error.NoKnownLinkerPath,

            // Generic ELF doesn't have a specific linker
            .x64elf, .x64v1elf => return error.NoKnownLinkerPath,

            // WebAssembly doesn't use dynamic linker
            .wasm32, .wasm32v1 => return error.WebAssemblyTarget,
        };
    }
};

/// What the CPU running this compiler can execute, which `builtin.cpu` cannot
/// answer because the compiler itself is built for the architecture baseline.
pub const host_cpu = @import("host_cpu.zig");

/// LLVM spelling of a resolved Zig CPU model.
pub fn llvmCpuName(target: std.Target) []const u8 {
    return target.cpu.model.llvm_name orelse "";
}

/// LLVM feature delta relative to the resolved CPU model.
pub fn llvmFeatureString(allocator: std.mem.Allocator, target: std.Target) std.mem.Allocator.Error![:0]u8 {
    const all_features = target.cpu.arch.allFeaturesList();
    var model_features = target.cpu.model.features;
    model_features.populateDependencies(all_features);

    var features = std.ArrayList(u8).empty;
    errdefer features.deinit(allocator);

    for (all_features) |feature| {
        const llvm_name = feature.llvm_name orelse continue;
        const enabled = target.cpu.features.isEnabled(feature.index);
        const model_enabled = model_features.isEnabled(feature.index);
        if (enabled == model_enabled) continue;

        if (features.items.len > 0) try features.append(allocator, ',');
        try features.append(allocator, if (enabled) '+' else '-');
        try features.appendSlice(allocator, llvm_name);
    }

    return features.toOwnedSliceSentinel(allocator, 0);
}

test {
    // Nothing in this file references host CPU detection, so name it here to
    // put its tests and its comptime check of the CPU floor in this run.
    std.testing.refAllDecls(host_cpu);
}

test "native target matches host OS and architecture" {
    try std.testing.expect(RocTarget.detectNative().matchesHostOsAndArch());
}

test "every v1 target shares its default target's platform" {
    for (std.enums.values(RocTarget)) |target| {
        if (target.cpuLevel() != .v1) continue;

        const default = target.defaultCpuTarget();
        try std.testing.expect(default != target);
        try std.testing.expectEqual(default.toOsTag(), target.toOsTag());
        try std.testing.expectEqual(default.toCpuArch(), target.toCpuArch());
        try std.testing.expectEqualStrings(default.toTriple(), target.toTriple());
        try std.testing.expectEqual(default.isStatic(), target.isStatic());
        try std.testing.expectEqual(default.isDynamic(), target.isDynamic());
        try std.testing.expectEqual(default.isLinux(), target.isLinux());
        try std.testing.expectEqual(default.isMacOS(), target.isMacOS());
        try std.testing.expectEqual(default.isWindows(), target.isWindows());
        try std.testing.expectEqual(default.windowsAbi(), target.windowsAbi());
        try std.testing.expectEqual(default.ptrBitWidth(), target.ptrBitWidth());
        try std.testing.expectEqual(default.isCompatibleWithHost(), target.isCompatibleWithHost());
        try std.testing.expectEqual(default.isExecutableOnHost(), target.isExecutableOnHost());

        // The switches that list `v1` targets by hand are the ones that could
        // put a target in the wrong arm without the compiler noticing.
        const default_linker = default.getDynamicLinkerPath();
        const target_linker = target.getDynamicLinkerPath();
        if (default_linker) |expected| {
            try std.testing.expectEqualStrings(expected, try target_linker);
        } else |expected_err| {
            try std.testing.expectError(expected_err, target_linker);
        }

        // The ABI is chosen from the default twin, so it must agree too.
        try std.testing.expectEqual(
            default.llvmTargetQuery().abi,
            target.llvmTargetQuery().abi,
        );
    }
}

test "Windows targets preserve their C runtime ABI in their query and triple" {
    const cases = [_]struct {
        target: RocTarget,
        abi: WindowsAbi,
        zig_abi: std.Target.Abi,
        triple: []const u8,
    }{
        .{ .target = .x64win, .abi = .msvc, .zig_abi = .msvc, .triple = "x86_64-pc-windows-msvc" },
        .{ .target = .x64v1win, .abi = .msvc, .zig_abi = .msvc, .triple = "x86_64-pc-windows-msvc" },
        .{ .target = .arm64win, .abi = .msvc, .zig_abi = .msvc, .triple = "aarch64-pc-windows-msvc" },
        .{ .target = .arm64v1win, .abi = .msvc, .zig_abi = .msvc, .triple = "aarch64-pc-windows-msvc" },
        .{ .target = .x64mingw, .abi = .mingw, .zig_abi = .gnu, .triple = "x86_64-w64-windows-gnu" },
        .{ .target = .x64v1mingw, .abi = .mingw, .zig_abi = .gnu, .triple = "x86_64-w64-windows-gnu" },
        .{ .target = .arm64mingw, .abi = .mingw, .zig_abi = .gnu, .triple = "aarch64-w64-windows-gnu" },
        .{ .target = .arm64v1mingw, .abi = .mingw, .zig_abi = .gnu, .triple = "aarch64-w64-windows-gnu" },
    };

    for (cases) |case| {
        try std.testing.expect(case.target.isWindows());
        try std.testing.expectEqual(case.abi, case.target.windowsAbi().?);
        try std.testing.expectEqual(case.zig_abi, case.target.llvmTargetQuery().abi.?);
        try std.testing.expectEqualStrings(case.triple, case.target.toTriple());
    }
}

test "a v1 target's name is its default target's name with v1 after the arch" {
    for (std.enums.values(RocTarget)) |target| {
        if (target.cpuLevel() != .v1) continue;

        const name = target.toName();
        const default_name = target.defaultCpuTarget().toName();
        const split = std.mem.find(u8, name, "v1").?;

        try std.testing.expectEqualStrings(default_name[0..split], name[0..split]);
        try std.testing.expectEqualStrings(default_name[split..], name[split + "v1".len ..]);
    }
}

test "default targets round-trip through defaultCpuTarget" {
    for (std.enums.values(RocTarget)) |target| {
        if (target.cpuLevel() != .default) continue;
        try std.testing.expectEqual(target, target.defaultCpuTarget());
    }
}

test "every target Roc raises the CPU floor for has a v1 twin" {
    // The rule the target list follows: wherever Roc names a CPU model or adds
    // CPU features of its own, it has chosen a floor above what the target
    // would otherwise get, and owes users a way to ask for the floor back.
    //
    // Targets that name neither are excluded on purpose. `arm64mac` is the
    // case that makes the distinction matter: it inherits Zig's model for
    // aarch64-macOS, which is above Armv8.0 but is also exactly the hardware
    // macOS runs on, so there is nothing below it to be locked out of.
    for (std.enums.values(RocTarget)) |target| {
        if (target.cpuLevel() != .default) continue;

        const query = target.llvmTargetQuery();
        const names_model = switch (query.cpu_model) {
            .explicit => true,
            .baseline, .determined_by_arch_os, .native => false,
        };
        if (!names_model and query.cpu_features_add.isEmpty()) continue;

        var has_twin = false;
        for (std.enums.values(RocTarget)) |candidate| {
            if (candidate.cpuLevel() == .v1 and candidate.defaultCpuTarget() == target) {
                has_twin = true;
                break;
            }
        }
        std.testing.expect(has_twin) catch |err| {
            std.debug.print("{s} raises the CPU floor but has no v1 twin\n", .{target.toName()});
            return err;
        };
    }
}

test "v1 targets ask LLVM for the architecture baseline" {
    try std.testing.expectEqual(
        &std.Target.x86.cpu.x86_64,
        RocTarget.x64v1musl.llvmTargetQuery().cpu_model.explicit,
    );
    try std.testing.expectEqual(
        &std.Target.aarch64.cpu.generic,
        RocTarget.arm64v1musl.llvmTargetQuery().cpu_model.explicit,
    );
    try std.testing.expectEqual(
        &std.Target.wasm.cpu.mvp,
        RocTarget.wasm32v1.llvmTargetQuery().cpu_model.explicit,
    );

    // A baseline query must not then add features back on top of the model.
    for (std.enums.values(RocTarget)) |target| {
        if (target.cpuLevel() != .v1) continue;
        try std.testing.expect(target.llvmTargetQuery().cpu_features_add.isEmpty());
    }
}

test "CPU contracts exactly constrain LLVM target features" {
    for (std.enums.values(RocTarget)) |target| {
        const expected = target.cpuContract().constrainedCodegenFeatures(target.toCpuArch()) orelse continue;
        std.testing.expect(target.llvmTargetFeatures().eql(expected)) catch |err| {
            std.debug.print("{s} LLVM features differ from its CPU contract\n", .{target.toName()});
            return err;
        };
    }
}

test "x86 scheduling model cannot silently raise the instruction floor" {
    const query = RocTarget.x64musl.llvmTargetQuery();
    try std.testing.expectEqual(&std.Target.x86.cpu.x86_64_v3, query.cpu_model.explicit);

    const tuning_feature = @intFromEnum(std.Target.x86.Feature.false_deps_lzcnt_tzcnt);
    try std.testing.expect(query.cpu_features_sub.isEnabled(tuning_feature));
    try std.testing.expect(!RocTarget.x64musl.llvmTargetFeatures().isEnabled(tuning_feature));
    try std.testing.expect(RocTarget.x64musl.llvmTargetFeatures().isEnabled(@intFromEnum(std.Target.x86.Feature.avx2)));
}

test "arm64 keeps its floor at Armv8.0 plus the SIMD builtins' extensions" {
    // Raspberry Pi 3 and 4 are Cortex-A53/A72, i.e. Armv8.0-A. A CPU model
    // above that floor makes arm64musl binaries fault on them.
    const query = RocTarget.arm64musl.llvmTargetQuery();
    try std.testing.expectEqual(&std.Target.aarch64.cpu.generic, query.cpu_model.explicit);

    const aes = @intFromEnum(std.Target.aarch64.Feature.aes);
    const dotprod = @intFromEnum(std.Target.aarch64.Feature.dotprod);
    try std.testing.expect(query.cpu_features_add.isEnabled(aes));
    try std.testing.expect(query.cpu_features_add.isEnabled(dotprod));

    // Nothing else: every other extension a CPU model would drag in costs
    // hardware without giving the SIMD builtins an instruction.
    var expected = std.Target.Cpu.Feature.Set.empty;
    expected.addFeature(aes);
    expected.addFeature(dotprod);
    try std.testing.expect(query.cpu_features_add.eql(expected));

    // Zig's generic model currently names ETE, but it is not part of Armv8.0-A.
    const ete = @intFromEnum(std.Target.aarch64.Feature.ete);
    try std.testing.expect(query.cpu_features_sub.isEnabled(ete));
    try std.testing.expect(!RocTarget.arm64musl.llvmTargetFeatures().isEnabled(ete));
}

test "arm32 and macOS arm64 have no v1 twin because Roc names no floor for them" {
    for ([_]RocTarget{ .arm32linux, .arm32musl, .arm64mac }) |target| {
        const query = target.llvmTargetQuery();
        try std.testing.expectEqual(
            std.Target.Query.CpuModel.determined_by_arch_os,
            query.cpu_model,
        );
        try std.testing.expect(query.cpu_features_add.isEmpty());
    }
}

test "wasm32 is not host executable" {
    try std.testing.expect(!RocTarget.wasm32.isExecutableOnHost());
}

test "wasm32 host matching is distinct from build compatibility" {
    if (RocTarget.detectNative() != .wasm32) {
        try std.testing.expect(!RocTarget.wasm32.matchesHostOsAndArch());
    }
    try std.testing.expect(RocTarget.wasm32.isCompatibleWithHost());
}
