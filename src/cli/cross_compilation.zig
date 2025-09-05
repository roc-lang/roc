//! Cross-compilation support and validation for Roc CLI
//! Handles host detection, target validation, and capability matrix

const std = @import("std");
const builtin = @import("builtin");
const target_mod = @import("target.zig");

const RocTarget = target_mod.RocTarget;

/// Result of cross-compilation validation
pub const CrossCompilationResult = union(enum) {
    supported: void,
    unsupported_host_target: struct {
        host: RocTarget,
        reason: []const u8,
    },
    unsupported_cross_compilation: struct {
        host: RocTarget,
        target: RocTarget,
        reason: []const u8,
    },
    missing_toolchain: struct {
        host: RocTarget,
        target: RocTarget,
        required_tools: []const []const u8,
    },
};

/// Cross-compilation capability matrix
pub const CrossCompilationMatrix = struct {
    /// Targets that support static linking (musl) - these should work from any host
    pub const musl_targets = [_]RocTarget{
        .x64musl,
        .arm64musl,
    };

    /// Targets that require dynamic linking (glibc) - more complex cross-compilation
    pub const glibc_targets = [_]RocTarget{
        .x64glibc,
        .arm64glibc,
    };

    /// Windows targets - require MinGW or similar toolchain
    pub const windows_targets = [_]RocTarget{
        // Future: .x64windows, .arm64windows
    };

    /// macOS targets - require OSXCross or similar toolchain
    pub const macos_targets = [_]RocTarget{
        // Future: .x64macos, .arm64macos
    };
};

/// Detect the host target platform
pub fn detectHostTarget() RocTarget {
    return switch (builtin.target.cpu.arch) {
        .x86_64 => switch (builtin.target.os.tag) {
            .linux => .x64glibc, // Default to glibc on Linux hosts
            .windows => .x64win,
            .macos => .x64mac,
            else => .x64glibc,
        },
        .aarch64 => switch (builtin.target.os.tag) {
            .linux => .arm64glibc,
            .windows => .arm64win,
            .macos => .arm64mac,
            else => .arm64glibc,
        },
        else => .x64glibc, // Fallback
    };
}

/// Check if a target is supported for static linking (musl)
pub fn isMuslTarget(target: RocTarget) bool {
    return switch (target) {
        .x64musl, .arm64musl => true,
        else => false,
    };
}

/// Check if a target requires dynamic linking (glibc)
pub fn isGlibcTarget(target: RocTarget) bool {
    return switch (target) {
        .x64glibc, .arm64glibc => true,
        else => false,
    };
}

/// Validate cross-compilation from host to target
pub fn validateCrossCompilation(host: RocTarget, target: RocTarget) CrossCompilationResult {
    // Native compilation (host == target) is always supported
    if (host == target) {
        return CrossCompilationResult{ .supported = {} };
    }

    // Support both musl and glibc targets for cross-compilation
    if (isMuslTarget(target) or isGlibcTarget(target)) {
        return CrossCompilationResult{ .supported = {} };
    }

    // Windows and macOS cross-compilation not yet supported
    return CrossCompilationResult{
        .unsupported_cross_compilation = .{
            .host = host,
            .target = target,
            .reason = "Windows and macOS cross-compilation not yet implemented. Please use Linux targets (x64musl, arm64musl, x64glibc, arm64glibc) or log an issue at https://github.com/roc-lang/roc/issues",
        },
    };
}

/// Get host capabilities (what this host can cross-compile to)
pub fn getHostCapabilities(host: RocTarget) []const RocTarget {
    _ = host; // For now, all hosts have the same capabilities

    // Support both musl and glibc targets from any host
    const all_targets = CrossCompilationMatrix.musl_targets ++ CrossCompilationMatrix.glibc_targets;
    return &all_targets;
}

/// Print supported targets for the current host
pub fn printSupportedTargets(writer: anytype, host: RocTarget) !void {
    const capabilities = getHostCapabilities(host);

    try writer.print("Supported cross-compilation targets from {s}:\n", .{@tagName(host)});
    for (capabilities) |target| {
        try writer.print("  {s} ({s})\n", .{ @tagName(target), target.toTriple() });
    }

    try writer.print("\nUnsupported targets (not yet implemented):\n", .{});
    const unsupported = [_][]const u8{
        "x64windows, arm64windows (Windows cross-compilation)",
        "x64macos, arm64macos (macOS cross-compilation)",
    };

    for (unsupported) |target_desc| {
        try writer.print("  {s}\n", .{target_desc});
    }

    try writer.print("\nTo request support for additional targets, please log an issue at:\n", .{});
    try writer.print("https://github.com/roc-lang/roc/issues\n", .{});
}

/// Print cross-compilation error with helpful context
pub fn printCrossCompilationError(writer: anytype, result: CrossCompilationResult) !void {
    switch (result) {
        .supported => {}, // No error
        .unsupported_host_target => |info| {
            try writer.print("Error: Unsupported host platform '{s}'\n", .{@tagName(info.host)});
            try writer.print("Reason: {s}\n", .{info.reason});
        },
        .unsupported_cross_compilation => |info| {
            try writer.print("Error: Cross-compilation from {s} to {s} is not supported\n", .{ @tagName(info.host), @tagName(info.target) });
            try writer.print("Reason: {s}\n", .{info.reason});
            try writer.print("\n", .{});
            try printSupportedTargets(writer, info.host);
        },
        .missing_toolchain => |info| {
            try writer.print("Error: Missing required toolchain for cross-compilation from {s} to {s}\n", .{ @tagName(info.host), @tagName(info.target) });
            try writer.print("Required tools:\n", .{});
            for (info.required_tools) |tool| {
                try writer.print("  {s}\n", .{tool});
            }
        },
    }
}
