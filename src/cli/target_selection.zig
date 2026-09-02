//! Target selection policy for CLI build and run commands.

const std = @import("std");
const builtin = @import("builtin");

const target_mod = @import("target.zig");

pub const RocTarget = target_mod.RocTarget;
pub const CpuLevel = target_mod.CpuLevel;
pub const TargetsConfig = target_mod.TargetsConfig;
pub const TargetLinkSpec = target_mod.TargetLinkSpec;
pub const OutputKind = target_mod.OutputKind;

const OutputOs = enum { windows, macos, wasm, other };

fn outputOs(target: RocTarget) OutputOs {
    return switch (target) {
        .x64win, .x64v1win, .x64mingw, .x64v1mingw, .arm64win, .arm64v1win, .arm64mingw, .arm64v1mingw => .windows,
        .x64mac, .x64v1mac, .arm64mac => .macos,
        .wasm32, .wasm32v1 => .wasm,
        .x64freebsd,
        .x64openbsd,
        .x64netbsd,
        .x64musl,
        .x64glibc,
        .x64linux,
        .x64elf,
        .x64v1freebsd,
        .x64v1openbsd,
        .x64v1netbsd,
        .x64v1musl,
        .x64v1glibc,
        .x64v1linux,
        .x64v1elf,
        .arm64linux,
        .arm64musl,
        .arm64glibc,
        .arm64v1linux,
        .arm64v1musl,
        .arm64v1glibc,
        .arm32linux,
        .arm32musl,
        => .other,
    };
}

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

/// Whether this target is the host's own OS, architecture, and ABI, in either
/// CPU-level spelling.
///
/// `matchesHostOsAndArch` deliberately ignores the ABI, so on a machine whose
/// native ABI is mingw both `x64win` and `x64mingw` match it. This one does
/// not: `defaultCpuTarget` folds a `v1` twin onto the target it is the
/// baseline of, leaving a comparison of OS, architecture, and ABI alone.
fn matchesNativeAbi(target: RocTarget) bool {
    const native = comptime RocTarget.detectNative().defaultCpuTarget();
    return target.defaultCpuTarget() == native;
}

/// Whether the platform lists a target for this exact machine, ABI included.
///
/// A platform can list several targets that share this machine's OS and
/// architecture but differ in ABI (`x64win` and `x64mingw`, say). Those are not
/// interchangeable: the interpreter backend only builds for the native target,
/// so defaulting to whichever the platform happened to list first would reject
/// the build outright. When this answers true, the host targets in the other
/// ABI stop being default candidates.
///
/// `runsOnHostCpu` still narrows the two CPU-level spellings, so a machine
/// below the default level looks for the `v1` twin of its native ABI, not the
/// exact native target it cannot execute.
fn hasNativeAbiTarget(config: TargetsConfig, host_cpu_level: CpuLevel, require_exe: bool) bool {
    for (config.getSupportedTargets()) |link_spec| {
        if (require_exe and link_spec.output != .exe) continue;
        if (matchesNativeAbi(link_spec.target) and runsOnHostCpu(link_spec.target, host_cpu_level)) return true;
    }
    return false;
}

/// Whether a native-ABI target elsewhere in the platform rules this one out.
///
/// Only host targets are displaced: wasm and other cross targets are picked for
/// reasons that have nothing to do with this machine's ABI, so the platform's
/// listing order still decides among them.
fn displacedByNativeAbi(target: RocTarget, has_native_abi: bool) bool {
    return has_native_abi and target.matchesHostOsAndArch() and !matchesNativeAbi(target);
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

    const has_native_abi = hasNativeAbiTarget(config, host_cpu_level, false);

    for (config.getSupportedTargets()) |link_spec| {
        if (displacedByNativeAbi(link_spec.target, has_native_abi)) continue;
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

    const has_native_abi = hasNativeAbiTarget(config, host_cpu_level, true);

    for (config.getSupportedTargets()) |link_spec| {
        if (link_spec.output == .exe and
            !displacedByNativeAbi(link_spec.target, has_native_abi) and
            isRunnableOnHost(link_spec.target, host_cpu_level))
        {
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
        .exe => switch (outputOs(target)) {
            .windows => ".exe",
            .wasm => ".wasm",
            .macos, .other => "",
        },
        .archive => switch (outputOs(target)) {
            .windows => ".lib",
            .macos, .wasm, .other => ".a",
        },
        .shared => switch (outputOs(target)) {
            .windows => ".dll",
            .macos => ".dylib",
            .wasm => ".wasm",
            .other => ".so",
        },
    };
}

fn expectSelected(result: SelectionResult) error{ExpectedSelectedTarget}!SelectedTarget {
    if (std.meta.activeTag(result) != .selected) return error.ExpectedSelectedTarget;
    return result.selected;
}

fn expectRequiresExecutable(result: SelectionResult) error{ExpectedRequiresExecutableTarget}!SelectedTarget {
    if (std.meta.activeTag(result) != .requires_executable) return error.ExpectedRequiresExecutableTarget;
    return result.requires_executable;
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

    // Naming the target explicitly still builds it—cross-compiling for a
    // newer CPU than this one is as legitimate as compiling for another OS—
    // but running it here is not.
    const explicit = try expectSelected(selectBuildTarget(config, native.toName(), .v1));
    try std.testing.expectEqual(native, explicit.target);
    try std.testing.expectEqual(
        SelectionResult{ .incompatible_cpu = native },
        selectRunTarget(config, native.toName(), .v1),
    );
}

/// The native target's sibling under the other Windows C runtime ABI.
///
/// Used by the tests below to build a platform that lists an ABI this machine
/// cannot use before the one it can. Non-Windows hosts have no such sibling.
fn otherWindowsAbiSibling(target: RocTarget) ?RocTarget {
    const abi = target.windowsAbi() orelse return null;
    for (std.enums.values(RocTarget)) |candidate| {
        const candidate_abi = candidate.windowsAbi() orelse continue;
        if (candidate_abi != abi and
            candidate.toCpuArch() == target.toCpuArch() and
            candidate.cpuLevel() == target.cpuLevel()) return candidate;
    }
    return null;
}

test "default build target prefers the native ABI over an OS-and-arch match" {
    // Both spellings pass `matchesHostOsAndArch`, which ignores the ABI, so
    // without the ABI tie-break the platform's listing order would decide—and
    // the interpreter backend refuses anything but the native target.
    const native = RocTarget.detectNative();
    const sibling = otherWindowsAbiSibling(native) orelse return error.SkipZigTest;

    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{
            .{ .target = sibling, .output = .exe, .items = &.{.app} },
            .{ .target = native, .output = .exe, .items = &.{.app} },
        },
    };

    const built = try expectSelected(selectBuildTarget(config, null, .default));
    try std.testing.expectEqual(native, built.target);

    const run = try expectSelected(selectRunTarget(config, null, .default));
    try std.testing.expectEqual(native, run.target);
    try std.testing.expectEqual(OutputKind.exe, run.output);
}

test "a host below the default CPU level takes its own ABI's v1 twin" {
    // The v1 case the strict-equality tie-break missed: the exact native target
    // fails `runsOnHostCpu` here, so only the ABI comparison keeps the
    // selection off the other ABI's v1 target listed ahead of it.
    const native = RocTarget.detectNative();
    const sibling = otherWindowsAbiSibling(native) orelse return error.SkipZigTest;
    const native_v1 = native.baselineCpuTarget() orelse return error.SkipZigTest;
    const sibling_v1 = sibling.baselineCpuTarget() orelse return error.SkipZigTest;

    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{
            .{ .target = sibling_v1, .output = .exe, .items = &.{.app} },
            .{ .target = native_v1, .output = .exe, .items = &.{.app} },
        },
    };

    const built = try expectSelected(selectBuildTarget(config, null, .v1));
    try std.testing.expectEqual(native_v1, built.target);

    const run = try expectSelected(selectRunTarget(config, null, .v1));
    try std.testing.expectEqual(native_v1, run.target);
}

test "the ABI tie-break does not steal a run target from a non-exe listing" {
    // A shared-library listing for the native target must still report
    // `requires_executable` rather than being selected by the tie-break.
    const native = RocTarget.detectNative();
    if (!native.isExecutableOnHost()) return error.SkipZigTest;

    const config = TargetsConfig{
        .inputs_dir = null,
        .targets = &.{
            .{ .target = native, .output = .shared, .items = &.{.app} },
        },
    };

    const selected = try expectRequiresExecutable(selectRunTarget(config, null, .default));
    try std.testing.expectEqual(native, selected.target);
    try std.testing.expectEqual(OutputKind.shared, selected.output);
}
