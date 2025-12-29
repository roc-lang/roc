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

/// Simple test file specification (no IO expectations)
pub const SimpleTestSpec = struct {
    /// Path to the roc file (relative to project root)
    roc_file: []const u8,
    /// Description of what the test verifies
    description: []const u8 = "",
};

/// How test apps are discovered for a platform
pub const TestApps = union(enum) {
    /// Single app file (like int)
    single: []const u8,
    /// List of test specs with IO expectations (like fx)
    spec_list: []const fx_test_specs.TestSpec,
    /// List of simple test files without IO specs (like str)
    simple_list: []const SimpleTestSpec,
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

/// Targets for fx platforms (musl + Windows)
const targets_fx = [_]TargetInfo{
    .{ .name = "x64musl", .requires_linux = false },
    .{ .name = "arm64musl", .requires_linux = false },
    .{ .name = "x64win", .requires_linux = false },
    .{ .name = "arm64win", .requires_linux = false },
};

/// Str platform test apps - test cross-module function calls
const str_tests = [_]SimpleTestSpec{
    // Basic test - no module imports from app
    .{
        .roc_file = "test/str/app.roc",
        .description = "Basic app with no platform module imports",
    },

    // Direct calls from app to each exposed module
    .{
        .roc_file = "test/str/app_direct_utils.roc",
        .description = "Direct call to Utils (base module, no imports)",
    },
    .{
        .roc_file = "test/str/app_direct_core.roc",
        .description = "Direct call to Core.wrap (Core imports Utils)",
    },
    .{
        .roc_file = "test/str/app_direct_helper.roc",
        .description = "Direct call to Helper.simple (no internal module calls)",
    },
    .{
        .roc_file = "test/str/app_direct_helper2.roc",
        .description = "Direct import of Helper (first in exposes list)",
    },

    // Transitive calls through modules
    .{
        .roc_file = "test/str/app_transitive.roc",
        .description = "Transitive call: app->Helper.wrap_fancy->Core.wrap",
    },
    .{
        .roc_file = "test/str/app_core_tagged.roc",
        .description = "Transitive call: app->Core.wrap_tagged->Utils.tag",
    },

    // Diamond dependency pattern
    .{
        .roc_file = "test/str/app_diamond.roc",
        .description = "Diamond: app->Helper->{Core->Utils, Utils}",
    },
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

    // STR PLATFORM - String processing with multi-module tests
    .{
        .name = "str",
        .base_dir = "test/str",
        .targets = &targets_with_glibc,
        .test_apps = .{ .simple_list = &str_tests },
        .supports_native_exec = true,
        .supports_io_specs = false,
        .valgrind_safe = true,
    },

    // FX PLATFORM - Effectful (stdout, stderr, stdin)
    .{
        .name = "fx",
        .base_dir = "test/fx",
        .targets = &targets_fx,
        .test_apps = .{ .spec_list = &fx_test_specs.io_spec_tests },
        .supports_native_exec = true,
        .supports_io_specs = true,
        .valgrind_safe = false, // Has stdin tests
    },

    // FX-OPEN PLATFORM - Effectful with open union errors
    .{
        .name = "fx-open",
        .base_dir = "test/fx-open",
        .targets = &targets_fx,
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
        .simple_list => |specs| {
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
