//! Target selection policy for CLI build and run commands.

const std = @import("std");
const builtin = @import("builtin");

const target_mod = @import("target.zig");

pub const RocTarget = target_mod.RocTarget;
pub const CpuLevel = target_mod.CpuLevel;
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

/// Whether this machine's CPU executes the code generated for this target.
///
/// The OS and architecture matching is not enough: a target whose CPU level is
/// above what the machine running the compiler supports links a host and emits
/// instructions that fault here, so it is not a target this machine can be
/// given by default.
fn runsOnHostCpu(target: RocTarget, host_cpu_level: CpuLevel) bool {
    return switch (host_cpu_level) {
        .default => true,
        .v1 => target.cpuLevel() == .v1,
    };
}

fn isBuildDefaultTarget(target: RocTarget, host_cpu_level: CpuLevel) bool {
    // Compare the architecture rather than the target, so both the default and
    // the baseline spelling of wasm are covered. Neither spelling runs on the
    // host CPU, so the host's CPU level does not narrow them.
    if (target.toCpuArch() == .wasm32) return true;

    return target.matchesHostOsAndArch() and runsOnHostCpu(target, host_cpu_level);
}

/// Whether the default `roc` command can execute an artifact built for this
/// target on this machine.
fn isRunnableOnHost(target: RocTarget, host_cpu_level: CpuLevel) bool {
    return target.isExecutableOnHost() and runsOnHostCpu(target, host_cpu_level);
}

fn hasIncompatibleHostCpu(target: RocTarget, host_cpu_level: CpuLevel) bool {
    return target.matchesHostOsAndArch() and !runsOnHostCpu(target, host_cpu_level);
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

fn selectDefaultBuildTarget(config: TargetsConfig, host_cpu_level: CpuLevel) SelectionResult {
    var incompatible_cpu: ?RocTarget = null;
    for (config.getSupportedTargets()) |link_spec| {
        if (isBuildDefaultTarget(link_spec.target, host_cpu_level)) {
            return .{ .selected = .{
                .target = link_spec.target,
                .output = link_spec.output,
                .link_spec = link_spec,
                .source = .default,
            } };
        }
        if (incompatible_cpu == null and hasIncompatibleHostCpu(link_spec.target, host_cpu_level)) {
            incompatible_cpu = link_spec.target;
        }
    }

    if (incompatible_cpu) |target| return .{ .incompatible_cpu = target };
    return .no_default;
}

/// Select a platform target for `roc build` without considering backend opt level.
///
/// `host_cpu_level` only narrows the default: an explicit `--target` names a
/// machine other than this one just as legitimately as it names another OS.
pub fn selectBuildTarget(config: TargetsConfig, target_arg: ?[]const u8, host_cpu_level: CpuLevel) SelectionResult {
    if (target_arg) |target_str| {
        const target = RocTarget.fromString(target_str) orelse {
            return .{ .invalid_target = target_str };
        };
        return selectExplicitBuildTarget(config, target);
    }

    return selectDefaultBuildTarget(config, host_cpu_level);
}

fn selectRunTargetForParsed(
    config: TargetsConfig,
    target: RocTarget,
    source: SelectionSource,
    host_cpu_level: CpuLevel,
) SelectionResult {
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

    if (hasIncompatibleHostCpu(target, host_cpu_level)) {
        return .{ .incompatible_cpu = target };
    }
    if (!isRunnableOnHost(target, host_cpu_level)) {
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
pub fn selectRunTarget(config: TargetsConfig, target_arg: ?[]const u8, host_cpu_level: CpuLevel) SelectionResult {
    if (target_arg) |target_str| {
        const target = RocTarget.fromString(target_str) orelse {
            return .{ .invalid_target = target_str };
        };
        return selectRunTargetForParsed(config, target, .explicit, host_cpu_level);
    }

    var default_non_executable: ?SelectedTarget = null;
    var incompatible_cpu: ?RocTarget = null;
    for (config.getSupportedTargets()) |link_spec| {
        if (link_spec.output == .exe and isRunnableOnHost(link_spec.target, host_cpu_level)) {
            return .{ .selected = .{
                .target = link_spec.target,
                .output = .exe,
                .link_spec = link_spec,
                .source = .default,
            } };
        }
        if (incompatible_cpu == null and
            link_spec.output == .exe and
            hasIncompatibleHostCpu(link_spec.target, host_cpu_level))
        {
            incompatible_cpu = link_spec.target;
        }
        if (default_non_executable == null and link_spec.output != .exe and isBuildDefaultTarget(link_spec.target, host_cpu_level)) {
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

test "explicit build target uses the target's declared output kind" {
    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{
            .{ .target = .wasm32, .output = .shared, .items = &.{.app} },
        },
    };

    const selected = try expectSelected(selectBuildTarget(config, "wasm32", .default));
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

    const selected = try expectSelected(selectBuildTarget(config, null, .default));
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

    const selected = try expectSelected(selectBuildTarget(config, null, .default));
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

    const default_selected = try expectRequiresExecutable(selectRunTarget(config, null, .default));
    try std.testing.expectEqual(RocTarget.wasm32, default_selected.target);
    try std.testing.expectEqual(OutputKind.shared, default_selected.output);
    try std.testing.expectEqual(SelectionSource.default, default_selected.source);

    const explicit_selected = try expectRequiresExecutable(selectRunTarget(config, "wasm32", .default));
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

    try std.testing.expectEqual(SelectionResult.no_default, selectRunTarget(config, null, .default));
    try std.testing.expectEqual(SelectionResult{ .not_runnable_on_host = .wasm32 }, selectRunTarget(config, "wasm32", .default));
}

test "run target excludes non-exe outputs" {
    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{
            .{ .target = RocTarget.detectNative(), .output = .shared, .items = &.{.app} },
        },
    };

    const selected = try expectRequiresExecutable(selectRunTarget(config, null, .default));
    try std.testing.expectEqual(RocTarget.detectNative(), selected.target);
    try std.testing.expectEqual(OutputKind.shared, selected.output);
}

test "run target selects native exe target" {
    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{
            .{ .target = .wasm32, .output = .shared, .items = &.{.app} },
            .{ .target = RocTarget.detectNative(), .output = .exe, .items = &.{.app} },
        },
    };

    const selected = try expectSelected(selectRunTarget(config, null, .default));
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

test "baseline wasm is a build default target like its default twin" {
    // Both spellings of wasm build from any host, so both are eligible as a
    // build default. Comparing against `.wasm32` alone silently excluded
    // `wasm32v1` and sent it down the native path instead.
    for ([_]CpuLevel{ .default, .v1 }) |host_cpu_level| {
        try std.testing.expect(isBuildDefaultTarget(.wasm32, host_cpu_level));
        try std.testing.expect(isBuildDefaultTarget(.wasm32v1, host_cpu_level));
    }
}

test "a v1 target is a build default exactly when its default twin is on a host that runs both" {
    for (std.enums.values(RocTarget)) |target| {
        if (target.cpuLevel() != .v1) continue;
        try std.testing.expectEqual(
            isBuildDefaultTarget(target.defaultCpuTarget(), .default),
            isBuildDefaultTarget(target, .default),
        );
    }
}

test "a host below the default CPU level only takes native v1 targets" {
    // The whole point of detecting the host CPU: a machine that cannot execute
    // the default level's instructions must not be handed them by default.
    for (std.enums.values(RocTarget)) |target| {
        if (target.toCpuArch() == .wasm32) continue;
        if (target.cpuLevel() != .default) continue;

        try std.testing.expect(!isBuildDefaultTarget(target, .v1));
        try std.testing.expect(!isRunnableOnHost(target, .v1));
    }
}

test "default build target drops to the v1 twin on a host below the default CPU level" {
    const native = RocTarget.detectNative();
    const baseline = native.baselineCpuTarget() orelse return error.SkipZigTest;

    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{
            .{ .target = native, .output = .exe, .items = &.{.app} },
            .{ .target = baseline, .output = .exe, .items = &.{.app} },
        },
    };

    const default_host = try expectSelected(selectBuildTarget(config, null, .default));
    try std.testing.expectEqual(native, default_host.target);

    const baseline_host = try expectSelected(selectBuildTarget(config, null, .v1));
    try std.testing.expectEqual(baseline, baseline_host.target);

    const run_on_baseline_host = try expectSelected(selectRunTarget(config, null, .v1));
    try std.testing.expectEqual(baseline, run_on_baseline_host.target);
}

test "a platform without a v1 target reports its incompatible CPU floor" {
    const native = RocTarget.detectNative();
    if (native.baselineCpuTarget() == null) return error.SkipZigTest;

    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{
            .{ .target = native, .output = .exe, .items = &.{.app} },
        },
    };

    try std.testing.expectEqual(
        SelectionResult{ .incompatible_cpu = native },
        selectBuildTarget(config, null, .v1),
    );
    try std.testing.expectEqual(
        SelectionResult{ .incompatible_cpu = native },
        selectRunTarget(config, null, .v1),
    );

    // Naming the target explicitly still builds it — cross-compiling for a
    // newer CPU than this one is as legitimate as compiling for another OS —
    // but running it here is not.
    const explicit = try expectSelected(selectBuildTarget(config, native.toName(), .v1));
    try std.testing.expectEqual(native, explicit.target);
    try std.testing.expectEqual(
        SelectionResult{ .incompatible_cpu = native },
        selectRunTarget(config, native.toName(), .v1),
    );
}
