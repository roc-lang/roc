//! Target selection policy for CLI build and run commands.

const std = @import("std");
const builtin = @import("builtin");

const target_mod = @import("target.zig");

pub const RocTarget = target_mod.RocTarget;
pub const TargetsConfig = target_mod.TargetsConfig;
pub const TargetLinkSpec = target_mod.TargetLinkSpec;
pub const LinkType = target_mod.LinkType;

/// Whether the selected target came from a command default or `--target`.
pub const SelectionSource = enum {
    default,
    explicit,
};

/// Platform target and link spec chosen for a CLI command.
pub const SelectedTarget = struct {
    target: RocTarget,
    link_type: LinkType,
    link_spec: TargetLinkSpec,
    source: SelectionSource,
};

/// Result of resolving a requested or default platform target.
pub const SelectionResult = union(enum) {
    selected: SelectedTarget,
    invalid_target: []const u8,
    unsupported_target: RocTarget,
    no_default,
    not_runnable_on_host: RocTarget,
};

const build_link_type_order = [_]LinkType{ .exe, .static_lib, .shared_lib };

fn isBuildDefaultTarget(target: RocTarget) bool {
    return target == .wasm32 or target.matchesHostOsAndArch();
}

fn selectExplicitBuildTarget(config: TargetsConfig, target: RocTarget) SelectionResult {
    for (build_link_type_order) |link_type| {
        if (config.getLinkSpec(target, link_type)) |link_spec| {
            return .{ .selected = .{
                .target = target,
                .link_type = link_type,
                .link_spec = link_spec,
                .source = .explicit,
            } };
        }
    }

    return .{ .unsupported_target = target };
}

fn selectDefaultBuildTarget(config: TargetsConfig) SelectionResult {
    for (build_link_type_order) |link_type| {
        for (config.getSupportedTargets(link_type)) |link_spec| {
            if (isBuildDefaultTarget(link_spec.target)) {
                return .{ .selected = .{
                    .target = link_spec.target,
                    .link_type = link_type,
                    .link_spec = link_spec,
                    .source = .default,
                } };
            }
        }
    }

    return .no_default;
}

/// Select a platform target for `roc build` without considering backend opt level.
pub fn selectBuildTarget(config: TargetsConfig, target_arg: ?[]const u8) SelectionResult {
    if (target_arg) |target_str| {
        const target = RocTarget.fromString(target_str) orelse {
            return .{ .invalid_target = target_str };
        };
        return selectExplicitBuildTarget(config, target);
    }

    return selectDefaultBuildTarget(config);
}

fn selectRunTargetForParsed(config: TargetsConfig, target: RocTarget, source: SelectionSource) SelectionResult {
    const link_spec = config.getLinkSpec(target, .exe) orelse {
        return .{ .unsupported_target = target };
    };

    if (!target.isExecutableOnHost()) {
        return .{ .not_runnable_on_host = target };
    }

    return .{ .selected = .{
        .target = target,
        .link_type = .exe,
        .link_spec = link_spec,
        .source = source,
    } };
}

/// Select a host-runnable `exe` target for `roc run`.
pub fn selectRunTarget(config: TargetsConfig, target_arg: ?[]const u8) SelectionResult {
    if (target_arg) |target_str| {
        const target = RocTarget.fromString(target_str) orelse {
            return .{ .invalid_target = target_str };
        };
        return selectRunTargetForParsed(config, target, .explicit);
    }

    for (config.getSupportedTargets(.exe)) |link_spec| {
        if (link_spec.target.isExecutableOnHost()) {
            return .{ .selected = .{
                .target = link_spec.target,
                .link_type = .exe,
                .link_spec = link_spec,
                .source = .default,
            } };
        }
    }

    return .no_default;
}

/// Default file extension for the selected build link type and target.
pub fn defaultBuildOutputExtension(link_type: LinkType, target: RocTarget) []const u8 {
    return switch (link_type) {
        .exe => switch (target.toOsTag()) {
            .windows => ".exe",
            .freestanding => ".wasm",
            else => "",
        },
        .static_lib => switch (target.toOsTag()) {
            .windows => ".lib",
            .freestanding => ".wasm",
            else => ".a",
        },
        .shared_lib => switch (target.toOsTag()) {
            .windows => ".dll",
            .macos => ".dylib",
            else => ".so",
        },
    };
}

fn expectSelected(result: SelectionResult) !SelectedTarget {
    return switch (result) {
        .selected => |selected| selected,
        else => error.ExpectedSelectedTarget,
    };
}

test "explicit build target uses link type priority" {
    const config = TargetsConfig{
        .files_dir = null,
        .exe = &.{
            .{ .target = .wasm32, .items = &.{.app} },
        },
        .static_lib = &.{
            .{ .target = .wasm32, .items = &.{.app} },
        },
        .shared_lib = &.{},
    };

    const selected = try expectSelected(selectBuildTarget(config, "wasm32"));
    try std.testing.expectEqual(RocTarget.wasm32, selected.target);
    try std.testing.expectEqual(LinkType.exe, selected.link_type);
    try std.testing.expectEqual(SelectionSource.explicit, selected.source);
}

test "default build target selects wasm static library" {
    const config = TargetsConfig{
        .files_dir = "targets",
        .exe = &.{},
        .static_lib = &.{
            .{ .target = .wasm32, .items = &.{ .{ .file_path = "libhost.a" }, .app } },
        },
        .shared_lib = &.{},
    };

    const selected = try expectSelected(selectBuildTarget(config, null));
    try std.testing.expectEqual(RocTarget.wasm32, selected.target);
    try std.testing.expectEqual(LinkType.static_lib, selected.link_type);
    try std.testing.expectEqual(SelectionSource.default, selected.source);
}

test "default build target uses platform order within link type" {
    const config = TargetsConfig{
        .files_dir = null,
        .exe = &.{
            .{ .target = .wasm32, .items = &.{.app} },
            .{ .target = RocTarget.detectNative(), .items = &.{.app} },
        },
        .static_lib = &.{},
        .shared_lib = &.{},
    };

    const selected = try expectSelected(selectBuildTarget(config, null));
    try std.testing.expectEqual(RocTarget.wasm32, selected.target);
    try std.testing.expectEqual(LinkType.exe, selected.link_type);
}

test "run target requires host exe target" {
    const config = TargetsConfig{
        .files_dir = null,
        .exe = &.{},
        .static_lib = &.{
            .{ .target = .wasm32, .items = &.{.app} },
        },
        .shared_lib = &.{},
    };

    try std.testing.expectEqual(SelectionResult.no_default, selectRunTarget(config, null));
    try std.testing.expectEqual(SelectionResult{ .unsupported_target = .wasm32 }, selectRunTarget(config, "wasm32"));
}

test "run target excludes wasm exe targets" {
    const config = TargetsConfig{
        .files_dir = null,
        .exe = &.{
            .{ .target = .wasm32, .items = &.{.app} },
        },
        .static_lib = &.{},
        .shared_lib = &.{},
    };

    try std.testing.expectEqual(SelectionResult.no_default, selectRunTarget(config, null));
    try std.testing.expectEqual(SelectionResult{ .not_runnable_on_host = .wasm32 }, selectRunTarget(config, "wasm32"));
}

test "run target selects native exe target" {
    const config = TargetsConfig{
        .files_dir = null,
        .exe = &.{
            .{ .target = .wasm32, .items = &.{.app} },
            .{ .target = RocTarget.detectNative(), .items = &.{.app} },
        },
        .static_lib = &.{},
        .shared_lib = &.{},
    };

    const selected = try expectSelected(selectRunTarget(config, null));
    try std.testing.expectEqual(RocTarget.detectNative(), selected.target);
    try std.testing.expectEqual(LinkType.exe, selected.link_type);
}

test "wasm static library output extension is wasm" {
    try std.testing.expectEqualStrings(".wasm", defaultBuildOutputExtension(.static_lib, .wasm32));
}

test "native static library output extension follows host object format" {
    const expected: []const u8 = if (builtin.target.os.tag == .windows) ".lib" else ".a";
    try std.testing.expectEqualStrings(expected, defaultBuildOutputExtension(.static_lib, RocTarget.detectNative()));
}
