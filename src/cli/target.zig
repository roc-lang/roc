//! Roc target definitions and system library path resolution

const std = @import("std");
const builtin = @import("builtin");
const parse = @import("parse");

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

    /// Get the OS tag for this RocTarget
    pub fn toOsTag(self: RocTarget) std.Target.Os.Tag {
        return switch (self) {
            // x64 targets
            .x64mac, .arm64mac => .macos,
            .x64win, .arm64win => .windows,
            .x64freebsd => .freebsd,
            .x64openbsd => .openbsd,
            .x64netbsd => .netbsd,
            .x64musl, .x64glibc, .x64linux, .x64elf, .arm64musl, .arm64glibc, .arm64linux, .arm32musl, .arm32linux => .linux,
            .wasm32 => .wasi,
        };
    }

    /// Get the CPU architecture for this RocTarget
    pub fn toCpuArch(self: RocTarget) std.Target.Cpu.Arch {
        return switch (self) {
            // x64 targets
            .x64mac, .x64win, .x64freebsd, .x64openbsd, .x64netbsd, .x64musl, .x64glibc, .x64linux, .x64elf => .x86_64,

            // arm64 targets
            .arm64mac, .arm64win, .arm64linux, .arm64musl, .arm64glibc => .aarch64,

            // arm32 targets
            .arm32linux, .arm32musl => .arm,

            // WebAssembly
            .wasm32 => .wasm32,
        };
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

    /// Detect the current system's Roc target
    pub fn detectNative() RocTarget {
        const os = builtin.target.os.tag;
        const arch = builtin.target.cpu.arch;
        const abi = builtin.target.abi;

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
                        // Check ABI to determine musl vs glibc
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
                        // Check ABI to determine musl vs glibc
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
    crt1_o: ?[]const u8 = null, // crt1.o or Scrt1.o (for PIE)
    crti_o: ?[]const u8 = null, // crti.o
    crtn_o: ?[]const u8 = null, // crtn.o
    libc_a: ?[]const u8 = null, // libc.a (for static linking)
};

/// Get vendored CRT object files for a platform target
/// All CRT files must be provided by the platform in its targets/ directory
pub fn getVendoredCRTFiles(allocator: Allocator, target: RocTarget, platform_dir: []const u8) !CRTFiles {
    // macOS and Windows targets don't need vendored CRT files - they use system libraries
    if (target.isMacOS() or target.isWindows()) {
        return CRTFiles{}; // Return empty CRTFiles struct
    }

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

/// Individual link item from a targets section
/// Can be a file path (relative to files/ directory) or a special identifier
pub const LinkItem = union(enum) {
    /// A file path (string literal in the source)
    /// Path is relative to the targets/<target>/ directory
    file_path: []const u8,

    /// The compiled Roc application
    app,

    /// Windows GUI subsystem flag (/subsystem:windows)
    win_gui,
};

/// Link specification for a single target
/// Contains the ordered list of items to link for this target
pub const TargetLinkSpec = struct {
    target: RocTarget,
    items: []const LinkItem,
};

/// Type of output binary
pub const LinkType = enum {
    /// Executable binary
    exe,
    /// Static library (.a, .lib)
    static_lib,
    /// Shared/dynamic library (.so, .dylib, .dll)
    shared_lib,
};

/// Complete targets configuration from a platform header
pub const TargetsConfig = struct {
    /// Base directory for target-specific files (e.g., "targets/")
    files_dir: ?[]const u8,

    /// Executable target specifications (in priority order)
    exe: []const TargetLinkSpec,

    /// Static library target specifications (in priority order)
    static_lib: []const TargetLinkSpec,

    /// Shared library target specifications (in priority order)
    shared_lib: []const TargetLinkSpec,

    /// Get the link spec for a specific target and link type
    pub fn getLinkSpec(self: TargetsConfig, target: RocTarget, link_type: LinkType) ?TargetLinkSpec {
        const specs = switch (link_type) {
            .exe => self.exe,
            .static_lib => self.static_lib,
            .shared_lib => self.shared_lib,
        };
        for (specs) |spec| {
            if (spec.target == target) {
                return spec;
            }
        }
        return null;
    }

    /// Get the default target for a given link type based on the current system
    /// Returns the first target in the list that's compatible with the current OS
    pub fn getDefaultTarget(self: TargetsConfig, link_type: LinkType) ?RocTarget {
        const specs = switch (link_type) {
            .exe => self.exe,
            .static_lib => self.static_lib,
            .shared_lib => self.shared_lib,
        };

        const native = RocTarget.detectNative();
        const native_os = native.toOsTag();

        // First pass: look for exact OS match
        for (specs) |spec| {
            if (spec.target.toOsTag() == native_os) {
                return spec.target;
            }
        }

        // wasm32 is considered compatible with all OSes as a fallback
        for (specs) |spec| {
            if (spec.target == .wasm32) {
                return spec.target;
            }
        }

        return null;
    }

    /// Check if a specific target is supported
    pub fn supportsTarget(self: TargetsConfig, target: RocTarget, link_type: LinkType) bool {
        return self.getLinkSpec(target, link_type) != null;
    }

    /// Get all supported targets for a link type
    pub fn getSupportedTargets(self: TargetsConfig, link_type: LinkType) []const TargetLinkSpec {
        return switch (link_type) {
            .exe => self.exe,
            .static_lib => self.static_lib,
            .shared_lib => self.shared_lib,
        };
    }

    /// Create a TargetsConfig from a parsed AST
    /// Returns null if the platform header has no targets section
    pub fn fromAST(allocator: Allocator, ast: anytype) !?TargetsConfig {
        const NodeStore = parse.NodeStore;

        const store: *const NodeStore = &ast.store;

        // Get the file node first, then get the header from it
        const file = store.getFile();
        const header = store.getHeader(file.header);

        // Only platform headers have targets
        const platform = switch (header) {
            .platform => |p| p,
            else => return null,
        };

        // If no targets section, return null
        const targets_section_idx = platform.targets orelse return null;
        const targets_section = store.getTargetsSection(targets_section_idx);

        // Extract files_dir from string literal token (StringPart token)
        const files_dir: ?[]const u8 = if (targets_section.files_path) |tok_idx|
            ast.resolve(tok_idx)
        else
            null;

        // Convert exe link type
        var exe_specs = std.array_list.Managed(TargetLinkSpec).init(allocator);
        errdefer exe_specs.deinit();

        if (targets_section.exe) |exe_idx| {
            const link_type = store.getTargetLinkType(exe_idx);
            const entry_indices = store.targetEntrySlice(link_type.entries);

            for (entry_indices) |entry_idx| {
                const entry = store.getTargetEntry(entry_idx);

                // Parse target name from token
                const target_name = ast.resolve(entry.target);
                const target = RocTarget.fromString(target_name) orelse continue; // Skip unknown targets

                // Convert files
                var link_items = std.array_list.Managed(LinkItem).init(allocator);
                errdefer link_items.deinit();

                const file_indices = store.targetFileSlice(entry.files);
                for (file_indices) |file_idx| {
                    const target_file = store.getTargetFile(file_idx);

                    switch (target_file) {
                        .string_literal => |tok| {
                            // The tok points to StringPart token containing the path
                            const path = ast.resolve(tok);
                            try link_items.append(.{ .file_path = path });
                        },
                        .special_ident => |tok| {
                            const ident = ast.resolve(tok);
                            if (std.mem.eql(u8, ident, "app")) {
                                try link_items.append(.app);
                            } else if (std.mem.eql(u8, ident, "win_gui")) {
                                try link_items.append(.win_gui);
                            }
                            // Skip unknown special identifiers
                        },
                        .malformed => continue, // Skip malformed entries
                    }
                }

                try exe_specs.append(.{
                    .target = target,
                    .items = try link_items.toOwnedSlice(),
                });
            }
        }

        // static_lib and shared_lib to be added later
        const empty_specs: []const TargetLinkSpec = &.{};

        return TargetsConfig{
            .files_dir = files_dir,
            .exe = try exe_specs.toOwnedSlice(),
            .static_lib = empty_specs,
            .shared_lib = empty_specs,
        };
    }
};
