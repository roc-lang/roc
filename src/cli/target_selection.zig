//! Target selection policy for CLI build and run commands.

const std = @import("std");
const builtin = @import("builtin");

const target_mod = @import("target.zig");

pub const RocTarget = target_mod.RocTarget;
pub const TargetsConfig = target_mod.TargetsConfig;
pub const TargetLinkSpec = target_mod.TargetLinkSpec;
pub const OutputKind = target_mod.OutputKind;

/// Whether the selected target came from a command default or `--target`.
pub const SelectionSource = enum {
    default,
    explicit,
};

/// Platform target and link spec chosen for a CLI command.
pub const SelectedTarget = struct {
    target: RocTarget,
    output: OutputKind,
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

fn isBuildDefaultTarget(target: RocTarget) bool {
    return target == .wasm32 or target.matchesHostOsAndArch();
}

fn selectExplicitBuildTarget(config: TargetsConfig, target: RocTarget) SelectionResult {
    if (config.getLinkSpec(target)) |link_spec| {
        return .{ .selected = .{
            .target = target,
            .output = link_spec.output,
            .link_spec = link_spec,
            .source = .explicit,
        } };
    }

    return .{ .unsupported_target = target };
}

fn selectDefaultBuildTarget(config: TargetsConfig) SelectionResult {
    for (config.getSupportedTargets()) |link_spec| {
        if (isBuildDefaultTarget(link_spec.target)) {
            return .{ .selected = .{
                .target = link_spec.target,
                .output = link_spec.output,
                .link_spec = link_spec,
                .source = .default,
            } };
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
    const link_spec = config.getLinkSpec(target) orelse {
        return .{ .unsupported_target = target };
    };

    if (link_spec.output != .exe) {
        return .{ .unsupported_target = target };
    }

    if (!target.isExecutableOnHost()) {
        return .{ .not_runnable_on_host = target };
    }

    return .{ .selected = .{
        .target = target,
        .output = .exe,
        .link_spec = link_spec,
        .source = source,
    } };
}

/// Select a host-runnable `output: Exe` target for the default `roc` command.
pub fn selectRunTarget(config: TargetsConfig, target_arg: ?[]const u8) SelectionResult {
    if (target_arg) |target_str| {
        const target = RocTarget.fromString(target_str) orelse {
            return .{ .invalid_target = target_str };
        };
        return selectRunTargetForParsed(config, target, .explicit);
    }

    for (config.getSupportedTargets()) |link_spec| {
        if (link_spec.output == .exe and link_spec.target.isExecutableOnHost()) {
            return .{ .selected = .{
                .target = link_spec.target,
                .output = .exe,
                .link_spec = link_spec,
                .source = .default,
            } };
        }
    }

    return .no_default;
}

/// Default file extension for the selected output kind and target.
pub fn defaultBuildOutputExtension(output: OutputKind, target: RocTarget) []const u8 {
    return switch (output) {
        .exe => switch (target.toOsTag()) {
            .windows => ".exe",
            .freestanding => ".wasm",
            else => "",
        },
        .archive => switch (target.toOsTag()) {
            .windows => ".lib",
            else => ".a",
        },
        .shared => switch (target.toOsTag()) {
            .windows => ".dll",
            .macos => ".dylib",
            .freestanding => ".wasm",
            else => ".so",
        },
    };
}

fn expectSelected(result: SelectionResult) error{ExpectedSelectedTarget}!SelectedTarget {
    return switch (result) {
        .selected => |selected| selected,
        else => error.ExpectedSelectedTarget,
    };
}

test "explicit build target uses the target's declared output kind" {
    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{
            .{ .target = .wasm32, .output = .shared, .items = &.{.app} },
        },
    };

    const selected = try expectSelected(selectBuildTarget(config, "wasm32"));
    try std.testing.expectEqual(RocTarget.wasm32, selected.target);
    try std.testing.expectEqual(OutputKind.shared, selected.output);
    try std.testing.expectEqual(SelectionSource.explicit, selected.source);
}

test "default build target selects wasm shared module" {
    const config = TargetsConfig{
        .inputs_dir = "targets",
        .targets = &.{
            .{ .target = .wasm32, .output = .shared, .items = &.{ .{ .file_path = "libhost.a" }, .app } },
        },
    };

    const selected = try expectSelected(selectBuildTarget(config, null));
    try std.testing.expectEqual(RocTarget.wasm32, selected.target);
    try std.testing.expectEqual(OutputKind.shared, selected.output);
    try std.testing.expectEqual(SelectionSource.default, selected.source);
}

test "default build target uses platform order" {
    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{
            .{ .target = .wasm32, .output = .exe, .items = &.{.app} },
            .{ .target = RocTarget.detectNative(), .output = .exe, .items = &.{.app} },
        },
    };

    const selected = try expectSelected(selectBuildTarget(config, null));
    try std.testing.expectEqual(RocTarget.wasm32, selected.target);
    try std.testing.expectEqual(OutputKind.exe, selected.output);
}

test "run target requires host exe target" {
    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{
            .{ .target = .wasm32, .output = .shared, .items = &.{.app} },
        },
    };

    try std.testing.expectEqual(SelectionResult.no_default, selectRunTarget(config, null));
    try std.testing.expectEqual(SelectionResult{ .unsupported_target = .wasm32 }, selectRunTarget(config, "wasm32"));
}

test "run target excludes wasm exe targets" {
    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{
            .{ .target = .wasm32, .output = .exe, .items = &.{.app} },
        },
    };

    try std.testing.expectEqual(SelectionResult.no_default, selectRunTarget(config, null));
    try std.testing.expectEqual(SelectionResult{ .not_runnable_on_host = .wasm32 }, selectRunTarget(config, "wasm32"));
}

test "run target excludes non-exe outputs" {
    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{
            .{ .target = RocTarget.detectNative(), .output = .shared, .items = &.{.app} },
        },
    };

    try std.testing.expectEqual(SelectionResult.no_default, selectRunTarget(config, null));
}

test "run target selects native exe target" {
    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{
            .{ .target = .wasm32, .output = .exe, .items = &.{.app} },
            .{ .target = RocTarget.detectNative(), .output = .exe, .items = &.{.app} },
        },
    };

    const selected = try expectSelected(selectRunTarget(config, null));
    try std.testing.expectEqual(RocTarget.detectNative(), selected.target);
    try std.testing.expectEqual(OutputKind.exe, selected.output);
}

test "wasm shared module output extension is wasm" {
    try std.testing.expectEqualStrings(".wasm", defaultBuildOutputExtension(.shared, .wasm32));
}

test "archive output extension follows target convention" {
    const expected: []const u8 = if (builtin.target.os.tag == .windows) ".lib" else ".a";
    try std.testing.expectEqualStrings(expected, defaultBuildOutputExtension(.archive, RocTarget.detectNative()));
}
