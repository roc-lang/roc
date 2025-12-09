//! Platform configurations for test platforms.
//!
//! This module defines configurations for all test platforms, including:
//! - Available targets
//! - Test app discovery
//! - Platform capabilities (native exec, IO specs, valgrind)

const std = @import("std");
const fx_test_specs = @import("fx_test_specs.zig");

/// Target information
pub const TargetInfo = struct {
    name: []const u8,
    requires_linux: bool,
};

/// How test apps are discovered for a platform
pub const TestApps = union(enum) {
    /// Single app file (like int/str)
    single: []const u8,
    /// List of test specs with IO expectations (like fx)
    spec_list: []const fx_test_specs.TestSpec,
};

/// Platform configuration
pub const PlatformConfig = struct {
    name: []const u8,
    base_dir: []const u8,
    targets: []const TargetInfo,
    test_apps: TestApps,
    supports_native_exec: bool,
    supports_io_specs: bool,
    valgrind_safe: bool,
};

/// All available cross-compilation targets (superset)
pub const all_cross_targets = [_][]const u8{
    "x64musl",
    "arm64musl",
    "x64glibc",
    "arm64glibc",
};

/// Standard targets for platforms with glibc support
const targets_with_glibc = [_]TargetInfo{
    .{ .name = "x64musl", .requires_linux = false },
    .{ .name = "arm64musl", .requires_linux = false },
    .{ .name = "x64glibc", .requires_linux = true },
    .{ .name = "arm64glibc", .requires_linux = true },
};

/// Standard targets for platforms without glibc support
const targets_musl_only = [_]TargetInfo{
    .{ .name = "x64musl", .requires_linux = false },
    .{ .name = "arm64musl", .requires_linux = false },
};

/// All platform configurations
pub const platforms = [_]PlatformConfig{
    // INT PLATFORM - Integer operations
    .{
        .name = "int",
        .base_dir = "test/int",
        .targets = &targets_with_glibc,
        .test_apps = .{ .single = "app.roc" },
        .supports_native_exec = true,
        .supports_io_specs = false,
        .valgrind_safe = true,
    },

    // STR PLATFORM - String processing
    .{
        .name = "str",
        .base_dir = "test/str",
        .targets = &targets_with_glibc,
        .test_apps = .{ .single = "app.roc" },
        .supports_native_exec = true,
        .supports_io_specs = false,
        .valgrind_safe = true,
    },

    // FX PLATFORM - Effectful (stdout, stderr, stdin)
    .{
        .name = "fx",
        .base_dir = "test/fx",
        .targets = &targets_musl_only,
        .test_apps = .{ .spec_list = &fx_test_specs.io_spec_tests },
        .supports_native_exec = true,
        .supports_io_specs = true,
        .valgrind_safe = false, // Has stdin tests
    },

    // FX-OPEN PLATFORM - Effectful with open union errors
    .{
        .name = "fx-open",
        .base_dir = "test/fx-open",
        .targets = &targets_musl_only,
        .test_apps = .{ .single = "app.roc" },
        .supports_native_exec = true,
        .supports_io_specs = false,
        .valgrind_safe = true,
    },
};

/// Find a platform configuration by name
pub fn findPlatform(name: []const u8) ?PlatformConfig {
    for (platforms) |platform| {
        if (std.mem.eql(u8, platform.name, name)) {
            return platform;
        }
    }
    return null;
}

/// Find a target in a platform's target list
pub fn findTarget(platform: PlatformConfig, target_name: []const u8) ?TargetInfo {
    for (platform.targets) |target| {
        if (std.mem.eql(u8, target.name, target_name)) {
            return target;
        }
    }
    return null;
}

/// Get list of all platform names
pub fn getPlatformNames() []const []const u8 {
    comptime {
        var names: [platforms.len][]const u8 = undefined;
        for (platforms, 0..) |platform, i| {
            names[i] = platform.name;
        }
        return &names;
    }
}

/// Get test app paths for a platform
pub fn getTestApps(platform: PlatformConfig) []const []const u8 {
    switch (platform.test_apps) {
        .single => |app| {
            const result = [_][]const u8{app};
            return &result;
        },
        .spec_list => |specs| {
            // Return just the roc_file paths
            var paths: [specs.len][]const u8 = undefined;
            for (specs, 0..) |spec, i| {
                paths[i] = spec.roc_file;
            }
            return &paths;
        },
    }
}

test "findPlatform works" {
    const int_platform = findPlatform("int");
    try std.testing.expect(int_platform != null);
    try std.testing.expectEqualStrings("test/int", int_platform.?.base_dir);

    const fx_platform = findPlatform("fx");
    try std.testing.expect(fx_platform != null);
    try std.testing.expect(fx_platform.?.supports_io_specs);

    const unknown = findPlatform("nonexistent");
    try std.testing.expect(unknown == null);
}

test "findTarget works" {
    const int_platform = findPlatform("int").?;

    const musl = findTarget(int_platform, "x64musl");
    try std.testing.expect(musl != null);
    try std.testing.expect(!musl.?.requires_linux);

    const glibc = findTarget(int_platform, "x64glibc");
    try std.testing.expect(glibc != null);
    try std.testing.expect(glibc.?.requires_linux);

    const nonexistent = findTarget(int_platform, "x64windows");
    try std.testing.expect(nonexistent == null);
}

test "fx platform has io specs" {
    const fx_platform = findPlatform("fx").?;
    try std.testing.expect(fx_platform.supports_io_specs);

    switch (fx_platform.test_apps) {
        .spec_list => |specs| {
            try std.testing.expect(specs.len > 0);
        },
        .single => {
            try std.testing.expect(false); // fx should have spec_list
        },
    }
}
