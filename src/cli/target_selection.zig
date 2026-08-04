//! Target selection policy for CLI build and run commands.

const std = @import("std");
const builtin = @import("builtin");

const target_mod = @import("target.zig");

pub const RocTarget = target_mod.RocTarget;
pub const RuntimeHost = target_mod.RuntimeHost;
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
    requires_executable: SelectedTarget,
    invalid_target: []const u8,
    unsupported_target: RocTarget,
    incompatible_cpu: RocTarget,
    no_default,
    not_runnable_on_host: RocTarget,
};

fn isBuildDefaultTarget(target: RocTarget, host: RuntimeHost) bool {
    // Compare the architecture rather than the target, so both the default and
    // the baseline spelling of wasm are covered.
    return target.toCpuArch() == .wasm32 or target.isExecutableOnRuntimeHost(host);
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

fn selectDefaultBuildTarget(config: TargetsConfig, host: RuntimeHost) SelectionResult {
    var incompatible_cpu: ?RocTarget = null;
    for (config.getSupportedTargets()) |link_spec| {
        if (isBuildDefaultTarget(link_spec.target, host)) {
            return .{ .selected = .{
                .target = link_spec.target,
                .output = link_spec.output,
                .link_spec = link_spec,
                .source = .default,
            } };
        }
        if (incompatible_cpu == null and link_spec.target.matchesRuntimeHostOsAndArch(host)) {
            incompatible_cpu = link_spec.target;
        }
    }

    if (incompatible_cpu) |target| return .{ .incompatible_cpu = target };
    return .no_default;
}

/// Select a platform target for `roc build` without considering backend opt level.
pub fn selectBuildTarget(config: TargetsConfig, target_arg: ?[]const u8, host: RuntimeHost) SelectionResult {
    if (target_arg) |target_str| {
        const target = RocTarget.fromString(target_str) orelse {
            return .{ .invalid_target = target_str };
        };
        return selectExplicitBuildTarget(config, target);
    }

    return selectDefaultBuildTarget(config, host);
}

fn selectRunTargetForParsed(config: TargetsConfig, target: RocTarget, source: SelectionSource, host: RuntimeHost) SelectionResult {
    const link_spec = config.getLinkSpec(target) orelse {
        return .{ .unsupported_target = target };
    };

    if (link_spec.output != .exe) {
        return .{ .requires_executable = .{
            .target = target,
            .output = link_spec.output,
            .link_spec = link_spec,
            .source = source,
        } };
    }

    if (!target.matchesRuntimeHostOsAndArch(host) or target.toCpuArch() == .wasm32) {
        return .{ .not_runnable_on_host = target };
    }
    if (!target.isCpuCompatibleWith(host.cpu)) return .{ .incompatible_cpu = target };

    return .{ .selected = .{
        .target = target,
        .output = .exe,
        .link_spec = link_spec,
        .source = source,
    } };
}

/// Select a host-runnable `output: Exe` target for the default `roc` command.
pub fn selectRunTarget(config: TargetsConfig, target_arg: ?[]const u8, host: RuntimeHost) SelectionResult {
    if (target_arg) |target_str| {
        const target = RocTarget.fromString(target_str) orelse {
            return .{ .invalid_target = target_str };
        };
        return selectRunTargetForParsed(config, target, .explicit, host);
    }

    var default_non_executable: ?SelectedTarget = null;
    var incompatible_cpu: ?RocTarget = null;
    for (config.getSupportedTargets()) |link_spec| {
        if (link_spec.output == .exe and link_spec.target.isExecutableOnRuntimeHost(host)) {
            return .{ .selected = .{
                .target = link_spec.target,
                .output = .exe,
                .link_spec = link_spec,
                .source = .default,
            } };
        }
        if (incompatible_cpu == null and
            link_spec.output == .exe and
            link_spec.target.matchesRuntimeHostOsAndArch(host) and
            !link_spec.target.isCpuCompatibleWith(host.cpu))
        {
            incompatible_cpu = link_spec.target;
        }
        if (default_non_executable == null and link_spec.output != .exe and isBuildDefaultTarget(link_spec.target, host)) {
            default_non_executable = .{
                .target = link_spec.target,
                .output = link_spec.output,
                .link_spec = link_spec,
                .source = .default,
            };
        }
    }

    if (incompatible_cpu) |target| return .{ .incompatible_cpu = target };
    if (default_non_executable) |selected| return .{ .requires_executable = selected };
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

fn expectRequiresExecutable(result: SelectionResult) error{ExpectedRequiresExecutableTarget}!SelectedTarget {
    return switch (result) {
        .requires_executable => |selected| selected,
        else => error.ExpectedRequiresExecutableTarget,
    };
}

fn x64RuntimeHost(model: *const std.Target.Cpu.Model, default_extensions: bool) RuntimeHost {
    var cpu = model.toCpu(.x86_64);
    if (default_extensions) {
        cpu.features.addFeature(@intFromEnum(std.Target.x86.Feature.aes));
        cpu.features.addFeature(@intFromEnum(std.Target.x86.Feature.pclmul));
        cpu.features.populateDependencies(std.Target.Cpu.Arch.x86_64.allFeaturesList());
    }
    return .{ .os_tag = .linux, .cpu = cpu };
}

fn baselineX64Host() RuntimeHost {
    return x64RuntimeHost(&std.Target.x86.cpu.x86_64, false);
}

fn defaultX64Host() RuntimeHost {
    return x64RuntimeHost(&std.Target.x86.cpu.x86_64_v3, true);
}

test "explicit build target uses the target's declared output kind" {
    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{
            .{ .target = .wasm32, .output = .shared, .items = &.{.app} },
        },
    };

    const selected = try expectSelected(selectBuildTarget(config, "wasm32", baselineX64Host()));
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

    const selected = try expectSelected(selectBuildTarget(config, null, baselineX64Host()));
    try std.testing.expectEqual(RocTarget.wasm32, selected.target);
    try std.testing.expectEqual(OutputKind.shared, selected.output);
    try std.testing.expectEqual(SelectionSource.default, selected.source);
}

test "default build target uses platform order" {
    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{
            .{ .target = .wasm32, .output = .exe, .items = &.{.app} },
            .{ .target = .x64v1musl, .output = .exe, .items = &.{.app} },
        },
    };

    const selected = try expectSelected(selectBuildTarget(config, null, baselineX64Host()));
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

    const default_selected = try expectRequiresExecutable(selectRunTarget(config, null, baselineX64Host()));
    try std.testing.expectEqual(RocTarget.wasm32, default_selected.target);
    try std.testing.expectEqual(OutputKind.shared, default_selected.output);
    try std.testing.expectEqual(SelectionSource.default, default_selected.source);

    const explicit_selected = try expectRequiresExecutable(selectRunTarget(config, "wasm32", baselineX64Host()));
    try std.testing.expectEqual(RocTarget.wasm32, explicit_selected.target);
    try std.testing.expectEqual(OutputKind.shared, explicit_selected.output);
    try std.testing.expectEqual(SelectionSource.explicit, explicit_selected.source);
}

test "run target excludes wasm exe targets" {
    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{
            .{ .target = .wasm32, .output = .exe, .items = &.{.app} },
        },
    };

    try std.testing.expectEqual(SelectionResult.no_default, selectRunTarget(config, null, baselineX64Host()));
    try std.testing.expectEqual(SelectionResult{ .not_runnable_on_host = .wasm32 }, selectRunTarget(config, "wasm32", baselineX64Host()));
}

test "run target excludes non-exe outputs" {
    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{
            .{ .target = .x64v1musl, .output = .shared, .items = &.{.app} },
        },
    };

    const selected = try expectRequiresExecutable(selectRunTarget(config, null, baselineX64Host()));
    try std.testing.expectEqual(RocTarget.x64v1musl, selected.target);
    try std.testing.expectEqual(OutputKind.shared, selected.output);
}

test "run target selects native exe target" {
    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{
            .{ .target = .wasm32, .output = .shared, .items = &.{.app} },
            .{ .target = .x64v1musl, .output = .exe, .items = &.{.app} },
        },
    };

    const selected = try expectSelected(selectRunTarget(config, null, baselineX64Host()));
    try std.testing.expectEqual(RocTarget.x64v1musl, selected.target);
    try std.testing.expectEqual(OutputKind.exe, selected.output);
}

test "wasm shared module output extension is wasm" {
    try std.testing.expectEqualStrings(".wasm", defaultBuildOutputExtension(.shared, .wasm32));
}

test "archive output extension follows target convention" {
    const expected: []const u8 = if (builtin.target.os.tag == .windows) ".lib" else ".a";
    try std.testing.expectEqualStrings(expected, defaultBuildOutputExtension(.archive, RocTarget.detectNative()));
}

test "baseline wasm is a build default target like its default twin" {
    // Both spellings of wasm build from any host, so both are eligible as a
    // build default. Comparing against `.wasm32` alone silently excluded
    // `wasm32v1` and sent it down the native path instead.
    try std.testing.expect(isBuildDefaultTarget(.wasm32, baselineX64Host()));
    try std.testing.expect(isBuildDefaultTarget(.wasm32v1, baselineX64Host()));
}

test "default selection skips an incompatible CPU floor and selects v1" {
    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{
            .{ .target = .x64musl, .output = .exe, .items = &.{.app} },
            .{ .target = .x64v1musl, .output = .exe, .items = &.{.app} },
        },
    };

    const build = try expectSelected(selectBuildTarget(config, null, baselineX64Host()));
    try std.testing.expectEqual(RocTarget.x64v1musl, build.target);

    const run = try expectSelected(selectRunTarget(config, null, baselineX64Host()));
    try std.testing.expectEqual(RocTarget.x64v1musl, run.target);
}

test "default selection retains the platform's faster target on a compatible CPU" {
    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{
            .{ .target = .x64musl, .output = .exe, .items = &.{.app} },
            .{ .target = .x64v1musl, .output = .exe, .items = &.{.app} },
        },
    };

    const build = try expectSelected(selectBuildTarget(config, null, defaultX64Host()));
    try std.testing.expectEqual(RocTarget.x64musl, build.target);

    const run = try expectSelected(selectRunTarget(config, null, defaultX64Host()));
    try std.testing.expectEqual(RocTarget.x64musl, run.target);
}

test "an incompatible sole platform target is rejected before execution" {
    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{
            .{ .target = .x64musl, .output = .exe, .items = &.{.app} },
        },
    };
    const host = baselineX64Host();

    try std.testing.expectEqual(
        SelectionResult{ .incompatible_cpu = .x64musl },
        selectBuildTarget(config, null, host),
    );
    try std.testing.expectEqual(
        SelectionResult{ .incompatible_cpu = .x64musl },
        selectRunTarget(config, null, host),
    );
    try std.testing.expectEqual(
        SelectionResult{ .incompatible_cpu = .x64musl },
        selectRunTarget(config, "x64musl", host),
    );

    // An explicit build is a cross-compilation request and does not execute
    // the artifact, so it remains available.
    const explicit_build = try expectSelected(selectBuildTarget(config, "x64musl", host));
    try std.testing.expectEqual(RocTarget.x64musl, explicit_build.target);
}
