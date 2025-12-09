//! Roc target definitions and link configuration
//!
//! Re-exports RocTarget and adds link configuration types that depend on the parse module.

const std = @import("std");
const parse = @import("parse");

const Allocator = std.mem.Allocator;

// Re-export RocTarget from the shared build module
pub const RocTarget = @import("roc_target").RocTarget;

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
