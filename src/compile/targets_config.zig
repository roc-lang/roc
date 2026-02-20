//! Target link configuration types for platform headers.
//!
//! These types represent the `targets:` section of a platform header,
//! which specifies what files to link for each supported target.
//!
//! Moved from `src/cli/target.zig` to the compile layer so that
//! `parseHeaderDeps()` can extract TargetsConfig during header parsing,
//! eliminating redundant re-parsing in CLI commands.

const std = @import("std");
const parse = @import("parse");
const roc_target = @import("roc_target");

const Allocator = std.mem.Allocator;
const RocTarget = roc_target.RocTarget;

/// Individual link item from a targets section.
/// Can be a file path (relative to files/ directory) or a special identifier.
pub const LinkItem = union(enum) {
    /// A file path (string literal in the source).
    /// Path is relative to the targets/<target>/ directory.
    file_path: []const u8,

    /// The compiled Roc application.
    app,

    /// Windows GUI subsystem flag (/subsystem:windows).
    win_gui,
};

/// Link specification for a single target.
/// Contains the ordered list of items to link for this target.
pub const TargetLinkSpec = struct {
    target: RocTarget,
    items: []const LinkItem,
};

/// Type of output binary.
pub const LinkType = enum {
    /// Executable binary.
    exe,
    /// Static library (.a, .lib).
    static_lib,
    /// Shared/dynamic library (.so, .dylib, .dll).
    shared_lib,
};

/// Complete targets configuration from a platform header.
pub const TargetsConfig = struct {
    /// Base directory for target-specific files (e.g., "targets/").
    files_dir: ?[]const u8,

    /// Executable target specifications (in priority order).
    exe: []const TargetLinkSpec,

    /// Static library target specifications (in priority order).
    static_lib: []const TargetLinkSpec,

    /// Shared library target specifications (in priority order).
    shared_lib: []const TargetLinkSpec,

    /// Free all owned memory. Call this when the TargetsConfig is no longer needed,
    /// but only if it was created via `fromAST()` (which dupes all strings).
    /// Do NOT call this on TargetsConfig values created with comptime/static data.
    pub fn deinit(self: TargetsConfig, allocator: Allocator) void {
        if (self.files_dir) |fd| allocator.free(fd);
        for (self.exe) |spec| {
            for (spec.items) |item| switch (item) {
                .file_path => |fp| allocator.free(fp),
                else => {},
            };
            allocator.free(spec.items);
        }
        allocator.free(self.exe);
        for (self.static_lib) |spec| {
            for (spec.items) |item| switch (item) {
                .file_path => |fp| allocator.free(fp),
                else => {},
            };
            allocator.free(spec.items);
        }
        allocator.free(self.static_lib);
        for (self.shared_lib) |spec| {
            for (spec.items) |item| switch (item) {
                .file_path => |fp| allocator.free(fp),
                else => {},
            };
            allocator.free(spec.items);
        }
        allocator.free(self.shared_lib);
    }

    /// Get the link spec for a specific target and link type.
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

    /// Get the default target for a given link type based on the current system.
    /// Returns the first target in the list that's compatible with the current host (OS and arch).
    pub fn getDefaultTarget(self: TargetsConfig, link_type: LinkType) ?RocTarget {
        const specs = switch (link_type) {
            .exe => self.exe,
            .static_lib => self.static_lib,
            .shared_lib => self.shared_lib,
        };

        // First pass: look for exact OS and architecture match
        for (specs) |spec| {
            if (spec.target.isCompatibleWithHost()) {
                return spec.target;
            }
        }

        return null;
    }

    /// Result of finding a compatible target.
    pub const CompatibleTarget = struct {
        target: RocTarget,
        link_type: LinkType,
    };

    /// Get the first compatible target across all link types.
    /// Iterates through exe, static_lib, shared_lib in order,
    /// returning the first target compatible with the current host.
    pub fn getFirstCompatibleTarget(self: TargetsConfig) ?CompatibleTarget {
        const link_types = [_]LinkType{ .exe, .static_lib, .shared_lib };

        for (link_types) |lt| {
            const specs = self.getSupportedTargets(lt);
            for (specs) |spec| {
                if (spec.target.isCompatibleWithHost()) {
                    return CompatibleTarget{ .target = spec.target, .link_type = lt };
                }
            }
        }

        return null;
    }

    /// Check if a specific target is supported.
    pub fn supportsTarget(self: TargetsConfig, target: RocTarget, link_type: LinkType) bool {
        return self.getLinkSpec(target, link_type) != null;
    }

    /// Get all supported targets for a link type.
    pub fn getSupportedTargets(self: TargetsConfig, link_type: LinkType) []const TargetLinkSpec {
        return switch (link_type) {
            .exe => self.exe,
            .static_lib => self.static_lib,
            .shared_lib => self.shared_lib,
        };
    }

    /// Create a TargetsConfig from a parsed AST.
    /// Returns null if the platform header has no targets section.
    /// All string values are duped with the provided allocator, so the
    /// returned TargetsConfig owns its memory and is independent of the AST.
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
        // Dupe the string so we own the memory
        const files_dir: ?[]const u8 = if (targets_section.files_path) |tok_idx|
            try allocator.dupe(u8, ast.resolve(tok_idx))
        else
            null;
        errdefer if (files_dir) |fd| allocator.free(fd);

        // Convert exe link type
        const exe_specs = try parseLinkTypeSpecs(allocator, store, ast, targets_section.exe);
        errdefer freeLinkTypeSpecs(allocator, exe_specs);

        // Convert static_lib link type
        const static_lib_specs = try parseLinkTypeSpecs(allocator, store, ast, targets_section.static_lib);
        errdefer freeLinkTypeSpecs(allocator, static_lib_specs);

        // shared_lib to be added later
        const empty_specs: []const TargetLinkSpec = &.{};

        return TargetsConfig{
            .files_dir = files_dir,
            .exe = exe_specs,
            .static_lib = static_lib_specs,
            .shared_lib = empty_specs,
        };
    }

    /// Parse link specs for a single link type (exe, static_lib, shared_lib).
    fn parseLinkTypeSpecs(
        allocator: Allocator,
        store: *const parse.NodeStore,
        ast: anytype,
        link_type_idx: anytype,
    ) ![]const TargetLinkSpec {
        const idx = link_type_idx orelse return &.{};

        const link_type = store.getTargetLinkType(idx);
        const entry_indices = store.targetEntrySlice(link_type.entries);

        var specs = std.array_list.Managed(TargetLinkSpec).init(allocator);
        errdefer {
            for (specs.items) |spec| {
                for (spec.items) |item| switch (item) {
                    .file_path => |fp| allocator.free(fp),
                    else => {},
                };
                allocator.free(spec.items);
            }
            specs.deinit();
        }

        for (entry_indices) |entry_idx| {
            const entry = store.getTargetEntry(entry_idx);

            // Parse target name from token
            const target_name = ast.resolve(entry.target);
            const target = RocTarget.fromString(target_name) orelse continue; // Skip unknown targets

            // Convert files
            var link_items = std.array_list.Managed(LinkItem).init(allocator);
            errdefer {
                for (link_items.items) |item| switch (item) {
                    .file_path => |fp| allocator.free(fp),
                    else => {},
                };
                link_items.deinit();
            }

            const file_indices = store.targetFileSlice(entry.files);
            for (file_indices) |file_idx| {
                const target_file = store.getTargetFile(file_idx);

                switch (target_file) {
                    .string_literal => |tok| {
                        const path = ast.resolve(tok);
                        try link_items.append(.{ .file_path = try allocator.dupe(u8, path) });
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

            try specs.append(.{
                .target = target,
                .items = try link_items.toOwnedSlice(),
            });
        }

        return try specs.toOwnedSlice();
    }

    /// Free link type specs allocated by parseLinkTypeSpecs.
    fn freeLinkTypeSpecs(allocator: Allocator, specs: []const TargetLinkSpec) void {
        for (specs) |spec| {
            for (spec.items) |item| switch (item) {
                .file_path => |fp| allocator.free(fp),
                else => {},
            };
            allocator.free(spec.items);
        }
        allocator.free(specs);
    }
};

// Tests
const testing = std.testing;
const builtin = @import("builtin");

test "getDefaultTarget returns first compatible target" {
    // Create a config with only x64glibc (not x64musl)
    // On a Linux x64 system, both are compatible, but we only include glibc
    const config = TargetsConfig{
        .files_dir = "targets",
        .exe = &.{
            .{ .target = .x64glibc, .items = &.{.app} },
        },
        .static_lib = &.{},
        .shared_lib = &.{},
    };

    // getDefaultTarget should return x64glibc if we're on Linux x64
    // (since both x64musl and x64glibc are compatible with Linux x64)
    if (builtin.target.os.tag == .linux and builtin.target.cpu.arch == .x86_64) {
        const result = config.getDefaultTarget(.exe);
        try testing.expect(result != null);
        try testing.expectEqual(RocTarget.x64glibc, result.?);
    }
}

test "getFirstCompatibleTarget finds exe target first" {
    const config = TargetsConfig{
        .files_dir = "targets",
        .exe = &.{
            .{ .target = .x64mac, .items = &.{.app} },
            .{ .target = .arm64mac, .items = &.{.app} },
        },
        .static_lib = &.{
            .{ .target = .wasm32, .items = &.{.app} },
        },
        .shared_lib = &.{},
    };

    // On macOS x64, should find x64mac from exe targets
    if (builtin.target.os.tag == .macos and builtin.target.cpu.arch == .x86_64) {
        const result = config.getFirstCompatibleTarget();
        try testing.expect(result != null);
        try testing.expectEqual(RocTarget.x64mac, result.?.target);
        try testing.expectEqual(LinkType.exe, result.?.link_type);
    }
    // On macOS arm64, should find arm64mac from exe targets
    if (builtin.target.os.tag == .macos and builtin.target.cpu.arch == .aarch64) {
        const result = config.getFirstCompatibleTarget();
        try testing.expect(result != null);
        try testing.expectEqual(RocTarget.arm64mac, result.?.target);
        try testing.expectEqual(LinkType.exe, result.?.link_type);
    }
}

test "getFirstCompatibleTarget returns null when no compatible target" {
    // Create a config with only Windows targets
    const config = TargetsConfig{
        .files_dir = "targets",
        .exe = &.{
            .{ .target = .x64win, .items = &.{.app} },
            .{ .target = .arm64win, .items = &.{.app} },
        },
        .static_lib = &.{},
        .shared_lib = &.{},
    };

    // On non-Windows systems, no compatible target should be found
    if (builtin.target.os.tag != .windows) {
        const result = config.getFirstCompatibleTarget();
        try testing.expect(result == null);
    }
}

test "getLinkSpec returns correct spec for supported target" {
    const config = TargetsConfig{
        .files_dir = "targets",
        .exe = &.{
            .{ .target = .x64mac, .items = &.{ .{ .file_path = "libhost.a" }, .app } },
            .{ .target = .arm64mac, .items = &.{.app} },
        },
        .static_lib = &.{},
        .shared_lib = &.{},
    };

    const spec = config.getLinkSpec(.x64mac, .exe);
    try testing.expect(spec != null);
    try testing.expectEqual(RocTarget.x64mac, spec.?.target);
    try testing.expectEqual(@as(usize, 2), spec.?.items.len);
}

test "getLinkSpec returns null for unsupported target" {
    const config = TargetsConfig{
        .files_dir = "targets",
        .exe = &.{
            .{ .target = .x64mac, .items = &.{.app} },
        },
        .static_lib = &.{},
        .shared_lib = &.{},
    };

    // x64musl is not in the config
    const spec = config.getLinkSpec(.x64musl, .exe);
    try testing.expect(spec == null);
}
