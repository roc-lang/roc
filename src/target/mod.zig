//! Roc target definitions - shared between build.zig and CLI
//!
//! This module is importable by build.zig (build-time) and CLI code (runtime).
//! It contains no dependencies on compiler modules like `parse`.

const std = @import("std");
const builtin = @import("builtin");

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
    return switch (arch) {
        .aarch64 => "arm64",
        .x86_64 => "x86_64",
        else => error.UnsupportedMachoArch,
    };
}

/// Dynamic-linker (`ld.so`) soname filenames, one authority for the bare
/// filenames used both by `RocTarget.getDynamicLinkerPath` (which prefixes them
/// with a known absolute directory) and by `cli/libc_finder.zig` (which probes
/// the real filesystem for them). Only the filenames live here; absolute search
/// directories stay with the code that owns them.
pub const ld_so = struct {
    /// glibc ld.so soname for x86_64.
    pub const glibc_x86_64 = "ld-linux-x86-64.so.2";
    /// glibc ld.so soname for aarch64.
    pub const glibc_aarch64 = "ld-linux-aarch64.so.1";
    /// glibc ld.so soname for 32-bit hard-float ARM.
    pub const glibc_arm = "ld-linux-armhf.so.3";
    /// glibc ld.so soname for 32-bit x86.
    pub const glibc_x86 = "ld-linux.so.2";
    /// musl ld.so soname for x86_64.
    pub const musl_x86_64 = "ld-musl-x86_64.so.1";
    /// musl ld.so soname for aarch64.
    pub const musl_aarch64 = "ld-musl-aarch64.so.1";
    /// musl ld.so soname for 32-bit ARM.
    pub const musl_arm = "ld-musl-arm.so.1";
    /// musl ld.so soname for 32-bit x86.
    pub const musl_x86 = "ld-musl-i386.so.1";
};

/// The absolute path a BSD kernel uses as an executable's program interpreter.
///
/// Each BSD ships exactly one loader at a fixed absolute path, so unlike Linux
/// (where the path depends on which libc the target links against) this is
/// determined by the OS alone. Keyed on `std.Target.Os.Tag` because the linker
/// carries a resolved OS tag rather than a `RocTarget`; returns null for any OS
/// whose loader is not addressed this way.
pub fn bsdProgramInterpreter(os_tag: std.Target.Os.Tag) ?[]const u8 {
    return switch (os_tag) {
        .freebsd => "/libexec/ld-elf.so.1",
        .openbsd => "/usr/libexec/ld.so",
        .netbsd => "/usr/libexec/ld.elf_so",
        else => null,
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
    arm64linux,
    arm64musl,
    arm64glibc,

    // arm64 (aarch64) targets, baseline CPU.
    //
    // There is no arm64v1mac: every Apple Silicon Mac is Armv8.4-A or newer,
    // so arm64mac already generates code its whole hardware range runs.
    arm64v1win,
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

        switch (arch) {
            .x86_64 => {
                switch (os) {
                    .macos => return .x64mac,
                    .windows => return .x64win,
                    .freebsd => return .x64freebsd,
                    .openbsd => return .x64openbsd,
                    .netbsd => return .x64netbsd,
                    .linux => {
                        return switch (abi) {
                            .musl, .musleabi, .musleabihf => .x64musl,
                            .gnu, .gnueabi, .gnueabihf, .gnux32 => .x64glibc,
                            else => .x64musl, // Default to musl for static linking
                        };
                    },
                    else => return .x64elf, // Generic fallback
                }
            },
            .aarch64, .aarch64_be => {
                switch (os) {
                    .macos => return .arm64mac,
                    .windows => return .arm64win,
                    .linux => {
                        return switch (abi) {
                            .musl, .musleabi, .musleabihf => .arm64musl,
                            .gnu, .gnueabi, .gnueabihf => .arm64glibc,
                            else => .arm64musl, // Default to musl for static linking
                        };
                    },
                    else => return .arm64linux, // Generic ARM64 Linux
                }
            },
            .arm => {
                switch (os) {
                    .linux => return .arm32musl, // Default to musl for static linking
                    else => return .arm32linux, // Generic ARM32 Linux
                }
            },
            .wasm32 => return .wasm32,
            else => {
                // Default fallback based on OS
                switch (os) {
                    .macos => return .x64mac,
                    .windows => return .x64win,
                    .linux => return .x64musl, // Default to musl
                    else => return .x64elf,
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
            .x64v1freebsd => .x64freebsd,
            .x64v1openbsd => .x64openbsd,
            .x64v1netbsd => .x64netbsd,
            .x64v1musl => .x64musl,
            .x64v1glibc => .x64glibc,
            .x64v1linux => .x64linux,
            .x64v1elf => .x64elf,

            .arm64v1win => .arm64win,
            .arm64v1linux => .arm64linux,
            .arm64v1musl => .arm64musl,
            .arm64v1glibc => .arm64glibc,

            .wasm32v1 => .wasm32,

            .x64mac,
            .x64win,
            .x64freebsd,
            .x64openbsd,
            .x64netbsd,
            .x64musl,
            .x64glibc,
            .x64linux,
            .x64elf,
            .arm64mac,
            .arm64win,
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
            .x64freebsd => .x64v1freebsd,
            .x64openbsd => .x64v1openbsd,
            .x64netbsd => .x64v1netbsd,
            .x64musl => .x64v1musl,
            .x64glibc => .x64v1glibc,
            .x64linux => .x64v1linux,
            .x64elf => .x64v1elf,

            .arm64win => .arm64v1win,
            .arm64linux => .arm64v1linux,
            .arm64musl => .arm64v1musl,
            .arm64glibc => .arm64v1glibc,

            .wasm32 => .wasm32v1,

            .x64v1mac,
            .x64v1win,
            .x64v1freebsd,
            .x64v1openbsd,
            .x64v1netbsd,
            .x64v1musl,
            .x64v1glibc,
            .x64v1linux,
            .x64v1elf,
            .arm64v1win,
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

    /// Get the OS tag for this RocTarget
    pub fn toOsTag(self: RocTarget) std.Target.Os.Tag {
        return switch (self) {
            .x64mac, .x64v1mac, .arm64mac => .macos,
            .x64win, .x64v1win, .arm64win, .arm64v1win => .windows,
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
            .x64mac, .x64win, .x64freebsd, .x64openbsd, .x64netbsd, .x64musl, .x64glibc, .x64linux, .x64elf => .x86_64,
            .x64v1mac, .x64v1win, .x64v1freebsd, .x64v1openbsd, .x64v1netbsd, .x64v1musl, .x64v1glibc, .x64v1linux, .x64v1elf => .x86_64,

            // arm64 targets
            .arm64mac, .arm64win, .arm64linux, .arm64musl, .arm64glibc => .aarch64,
            .arm64v1win, .arm64v1linux, .arm64v1musl, .arm64v1glibc => .aarch64,

            // arm32 targets
            .arm32linux, .arm32musl => .arm,

            // WebAssembly
            .wasm32, .wasm32v1 => .wasm32,
        };
    }

    /// Build the single target query used for every LLVM compilation of Roc
    /// program code, including linked applications, host shims, eval, and
    /// optimized `roc test` roots.
    pub fn llvmTargetQuery(self: RocTarget) std.Target.Query {
        var query = std.Target.Query{
            .cpu_arch = self.toCpuArch(),
            .os_tag = self.toOsTag(),
            .abi = switch (self.defaultCpuTarget()) {
                .x64musl, .arm64musl, .arm32musl => .musl,
                .x64glibc, .x64linux, .arm64glibc, .arm64linux, .arm32linux => .gnu,
                .x64win, .arm64win => .msvc,
                else => .none,
            },
        };
        if (self.toOsTag() == .macos) {
            query.os_version_min = macos_deployment.query_os_version;
        }

        const level = self.cpuLevel();
        switch (self.toCpuArch()) {
            .x86_64 => switch (level) {
                .default => {
                    // x86-64-v3 covers AVX2/SSSE3/BMI2/POPCNT/FMA. The named
                    // level omits AES and PCLMULQDQ, so enable both explicitly.
                    query.cpu_model = .{ .explicit = &std.Target.x86.cpu.x86_64_v3 };
                    query.cpu_features_add.addFeature(@intFromEnum(std.Target.x86.Feature.aes));
                    query.cpu_features_add.addFeature(@intFromEnum(std.Target.x86.Feature.pclmul));
                },
                // The `x86_64` model is the psABI's x86-64-v1: SSE2 and no more.
                .v1 => query.cpu_model = .{ .explicit = &std.Target.x86.cpu.x86_64 },
            },
            .aarch64, .aarch64_be => switch (level) {
                .default => {
                    // NEON is mandatory in Armv8.0-A, so Roc's 128-bit integer
                    // SIMD needs no extension to be native on aarch64. Name the
                    // two extensions the SIMD builtins do lower to instead of
                    // pinning a CPU model: a model drags in unrelated revisions
                    // of the architecture, and each one it raises the floor by
                    // is hardware that can no longer run the result.
                    if (self.toOsTag() != .macos) {
                        query.cpu_model = .{ .explicit = &std.Target.aarch64.cpu.generic };
                        // AES supplies PMULL64, which carryless multiply lowers
                        // to directly.
                        query.cpu_features_add.addFeature(@intFromEnum(std.Target.aarch64.Feature.aes));
                        // DotProd supplies SDOT/UDOT, which LLVM selects for
                        // the widening multiply-accumulate `dot_pairs` emits.
                        query.cpu_features_add.addFeature(@intFromEnum(std.Target.aarch64.Feature.dotprod));
                    }
                },
                // Armv8.0-A exactly. macOS is excluded from `v1` targets
                // because every Apple Silicon Mac is well above this floor.
                .v1 => query.cpu_model = .{ .explicit = &std.Target.aarch64.cpu.generic },
            },
            .wasm32 => switch (level) {
                .default => query.cpu_features_add.addFeature(@intFromEnum(std.Target.wasm.Feature.simd128)),
                // The WebAssembly 1.0 core instruction set.
                .v1 => query.cpu_model = .{ .explicit = &std.Target.wasm.cpu.mvp },
            },
            else => {},
        }

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
        return switch (self.defaultCpuTarget()) {
            .x64glibc, .arm64glibc, .x64linux, .arm64linux, .arm32linux => true,
            else => false,
        };
    }

    /// Check if target uses static linking (musl targets)
    pub fn isStatic(self: RocTarget) bool {
        return switch (self.defaultCpuTarget()) {
            .x64musl, .arm64musl, .arm32musl => true,
            else => false,
        };
    }

    /// Check if target is macOS
    pub fn isMacOS(self: RocTarget) bool {
        return switch (self.defaultCpuTarget()) {
            .x64mac, .arm64mac => true,
            else => false,
        };
    }

    /// Check if target is Windows
    pub fn isWindows(self: RocTarget) bool {
        return switch (self.defaultCpuTarget()) {
            .x64win, .arm64win => true,
            else => false,
        };
    }

    /// Check if target is Linux-based
    pub fn isLinux(self: RocTarget) bool {
        return switch (self.defaultCpuTarget()) {
            .x64musl, .x64glibc, .x64linux, .arm64musl, .arm64glibc, .arm64linux, .arm32musl, .arm32linux => true,
            else => false,
        };
    }

    /// Get the pointer bit width for this target
    pub fn ptrBitWidth(self: RocTarget) u16 {
        return switch (self.toCpuArch()) {
            .x86_64, .aarch64, .aarch64_be => 64,
            .arm, .wasm32 => 32,
            else => 64, // Default to 64-bit
        };
    }

    /// Check if this target has the same OS and CPU architecture as the current host.
    pub fn matchesHostOsAndArch(self: RocTarget) bool {
        return self.toOsTag() == builtin.target.os.tag and
            self.toCpuArch() == builtin.target.cpu.arch;
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
            // x64 glibc targets
            .x64glibc, .x64linux, .x64v1glibc, .x64v1linux => "/lib64/" ++ ld_so.glibc_x86_64,

            // arm64 glibc targets
            .arm64glibc, .arm64linux, .arm64v1glibc, .arm64v1linux => "/lib/" ++ ld_so.glibc_aarch64,

            // arm32 glibc targets
            .arm32linux => "/lib/" ++ ld_so.glibc_arm,

            // Static linking targets don't need dynamic linker
            .x64musl, .arm64musl, .arm32musl, .x64v1musl, .arm64v1musl => return error.StaticLinkingTarget,

            // macOS uses dyld
            .x64mac, .arm64mac, .x64v1mac => "/usr/lib/dyld",

            // Windows doesn't use ELF-style dynamic linker
            .x64win, .arm64win, .x64v1win, .arm64v1win => return error.WindowsTarget,

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
