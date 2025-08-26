//! Roc target definitions and system library path resolution

const std = @import("std");
const builtin = @import("builtin");

const Allocator = std.mem.Allocator;

/// Roc's simplified targets
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

    // arm64 (aarch64) targets
    arm64mac,
    arm64win,
    arm64linux,
    arm64musl,
    arm64glibc,

    // arm32 targets
    arm32linux,
    arm32musl,

    // WebAssembly
    wasm32,

    /// Parse target from string
    pub fn fromString(str: []const u8) ?RocTarget {
        const enum_info = @typeInfo(RocTarget);
        inline for (enum_info.@"enum".fields) |field| {
            if (std.mem.eql(u8, str, field.name)) {
                return @enumFromInt(field.value);
            }
        }
        return null;
    }

    /// Convert Roc target to LLVM target triple
    pub fn toTriple(self: RocTarget) []const u8 {
        return switch (self) {
            // x64 targets
            .x64mac => "x86_64-apple-darwin",
            .x64win => "x86_64-pc-windows-msvc",
            .x64freebsd => "x86_64-unknown-freebsd",
            .x64openbsd => "x86_64-unknown-openbsd",
            .x64netbsd => "x86_64-unknown-netbsd",
            .x64musl => "x86_64-unknown-linux-musl",
            .x64glibc => "x86_64-unknown-linux-gnu",
            .x64linux => "x86_64-unknown-linux-gnu",
            .x64elf => "x86_64-unknown-none-elf",

            // arm64 targets
            .arm64mac => "aarch64-apple-darwin",
            .arm64win => "aarch64-pc-windows-msvc",
            .arm64linux => "aarch64-unknown-linux-gnu",
            .arm64musl => "aarch64-unknown-linux-musl",
            .arm64glibc => "aarch64-unknown-linux-gnu",

            // arm32 targets
            .arm32linux => "arm-unknown-linux-gnueabihf",
            .arm32musl => "arm-unknown-linux-musleabihf",

            // WebAssembly
            .wasm32 => "wasm32-unknown-unknown",
        };
    }

    /// Detect the current system's Roc target (defaults to musl for static linking)
    pub fn detectNative() RocTarget {
        const os = builtin.target.os.tag;
        const arch = builtin.target.cpu.arch;

        // Handle architecture first
        switch (arch) {
            .x86_64 => {
                switch (os) {
                    .macos => return .x64mac,
                    .windows => return .x64win,
                    .freebsd => return .x64freebsd,
                    .openbsd => return .x64openbsd,
                    .netbsd => return .x64netbsd,
                    .linux => {
                        // Default to musl for static linking
                        return .x64musl;
                    },
                    else => return .x64elf, // Generic fallback
                }
            },
            .aarch64, .aarch64_be => {
                switch (os) {
                    .macos => return .arm64mac,
                    .windows => return .arm64win,
                    .linux => {
                        // Default to musl for static linking
                        return .arm64musl;
                    },
                    else => return .arm64linux, // Generic ARM64 Linux
                }
            },
            .arm => {
                switch (os) {
                    .linux => {
                        // Default to musl for static linking
                        return .arm32musl;
                    },
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

    /// Check if target uses dynamic linking (glibc targets)
    pub fn isDynamic(self: RocTarget) bool {
        return switch (self) {
            .x64glibc, .arm64glibc, .x64linux, .arm64linux, .arm32linux => true,
            else => false,
        };
    }

    /// Check if target uses static linking (musl targets)
    pub fn isStatic(self: RocTarget) bool {
        return switch (self) {
            .x64musl, .arm64musl, .arm32musl => true,
            else => false,
        };
    }

    /// Check if target is macOS
    pub fn isMacOS(self: RocTarget) bool {
        return switch (self) {
            .x64mac, .arm64mac => true,
            else => false,
        };
    }

    /// Check if target is Windows
    pub fn isWindows(self: RocTarget) bool {
        return switch (self) {
            .x64win, .arm64win => true,
            else => false,
        };
    }

    /// Check if target is Linux-based
    pub fn isLinux(self: RocTarget) bool {
        return switch (self) {
            .x64musl, .x64glibc, .x64linux,
            .arm64musl, .arm64glibc, .arm64linux,
            .arm32musl, .arm32linux => true,
            else => false,
        };
    }

    /// Get the dynamic linker path for this target
    pub fn getDynamicLinkerPath(self: RocTarget) ![]const u8 {
        return switch (self) {
            // x64 glibc targets
            .x64glibc, .x64linux => "/lib64/ld-linux-x86-64.so.2",
            
            // arm64 glibc targets
            .arm64glibc, .arm64linux => "/lib/ld-linux-aarch64.so.1",
            
            // arm32 glibc targets
            .arm32linux => "/lib/ld-linux-armhf.so.3",
            
            // Static linking targets don't need dynamic linker
            .x64musl, .arm64musl, .arm32musl => return error.StaticLinkingTarget,
            
            // macOS uses dyld
            .x64mac, .arm64mac => "/usr/lib/dyld",
            
            // Windows doesn't use ELF-style dynamic linker
            .x64win, .arm64win => return error.WindowsTarget,
            
            // BSD variants
            .x64freebsd => "/libexec/ld-elf.so.1",
            .x64openbsd => "/usr/libexec/ld.so",
            .x64netbsd => "/usr/libexec/ld.elf_so",
            
            // Generic ELF doesn't have a specific linker
            .x64elf => return error.NoKnownLinkerPath,
            
            // WebAssembly doesn't use dynamic linker
            .wasm32 => return error.WebAssemblyTarget,
        };
    }
};

/// CRT (C runtime) file paths for linking
pub const CRTFiles = struct {
    crt1_o: ?[]const u8 = null,      // crt1.o or Scrt1.o (for PIE)
    crti_o: ?[]const u8 = null,      // crti.o
    crtn_o: ?[]const u8 = null,      // crtn.o
    libc_a: ?[]const u8 = null,      // libc.a (for static linking)
};

/// Get vendored CRT object files for a platform target
/// All CRT files must be provided by the platform in its targets/ directory
pub fn getVendoredCRTFiles(allocator: Allocator, target: RocTarget, platform_dir: []const u8) !CRTFiles {
    // Build path to the vendored CRT files
    const target_subdir = switch (target) {
        .x64musl => "x64musl",
        .x64glibc => "x64glibc",
        .arm64musl => "arm64musl",
        .arm64glibc => "arm64glibc",
        .arm32musl => "arm32musl",
        .arm32linux => "arm32glibc",
        else => return error.UnsupportedTargetForPlatform,
    };

    const targets_dir = try std.fs.path.join(allocator, &[_][]const u8{ platform_dir, "targets", target_subdir });
    
    var result = CRTFiles{};

    if (target.isStatic()) {
        // For musl static linking
        result.crt1_o = try std.fs.path.join(allocator, &[_][]const u8{ targets_dir, "crt1.o" });
        result.libc_a = try std.fs.path.join(allocator, &[_][]const u8{ targets_dir, "libc.a" });
    } else {
        // For glibc dynamic linking
        result.crt1_o = try std.fs.path.join(allocator, &[_][]const u8{ targets_dir, "Scrt1.o" });
        result.crti_o = try std.fs.path.join(allocator, &[_][]const u8{ targets_dir, "crti.o" });
        result.crtn_o = try std.fs.path.join(allocator, &[_][]const u8{ targets_dir, "crtn.o" });
    }

    return result;
}