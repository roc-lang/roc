//! Compilation-related types and functionality, such as cache management and package building.

const std = @import("std");

pub const package = @import("compile_package.zig");
pub const build = @import("compile_build.zig");
pub const targets_config = @import("targets_config.zig");
pub const TargetsConfig = targets_config.TargetsConfig;
pub const single_module = @import("compile_module.zig");
pub const module_discovery = @import("module_discovery.zig");
pub const dependency_sort = @import("dependency_sort.zig");
pub const serialize_modules = @import("serialize_modules.zig");
pub const runner = @import("runner.zig");

// Actor model components
pub const messages = @import("messages.zig");
pub const channel = @import("channel.zig");
pub const coordinator = @import("coordinator.zig");

pub const module = @import("cache_module.zig");
pub const key = @import("cache_key.zig");
pub const config = @import("cache_config.zig");
pub const reporting = @import("cache_reporting.zig");
pub const manager = @import("cache_manager.zig");
pub const cleanup = @import("cache_cleanup.zig");

pub const Header = module.Header;
pub const CacheModule = module.CacheModule;
pub const Diagnostics = module.Diagnostics;
pub const CacheManager = manager.CacheManager;
pub const CacheResult = manager.CacheResult;
pub const CacheConfig = config.CacheConfig;
pub const CacheStats = config.CacheStats;
/// Cache cleanup utilities for managing temporary and persistent cache files.
pub const CacheCleanup = cleanup;
pub const CleanupStats = cleanup.CleanupStats;
pub const PackageEnv = package.PackageEnv;
pub const BuildEnv = build.BuildEnv;

// /// Global cache statistics (optional, for debugging)
// var global_stats: Stats = .{};

// /// Get reference to global stats
// pub fn getGlobalStats() *Stats {
//     return &global_stats;
// }

// /// Reset global stats
// pub fn resetGlobalStats() void {
//     global_stats.reset();
// }

// /// Print global stats to stderr
// pub fn printGlobalStats() !void {
//     var stderr_buffer: [1024]u8 = undefined;
//     var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
//     const stderr = &stderr_writer.interface;
//     try global_stats.print(stderr.any());
// }

test "compile tests" {
    std.testing.refAllDecls(@import("cache_cleanup.zig"));
    std.testing.refAllDecls(@import("cache_config.zig"));
    std.testing.refAllDecls(@import("cache_key.zig"));
    std.testing.refAllDecls(@import("cache_manager.zig"));
    std.testing.refAllDecls(@import("cache_module.zig"));
    std.testing.refAllDecls(@import("cache_reporting.zig"));
    std.testing.refAllDecls(@import("compile_build.zig"));
    std.testing.refAllDecls(@import("targets_config.zig"));
    std.testing.refAllDecls(@import("compile_module.zig"));
    std.testing.refAllDecls(@import("compile_package.zig"));
    std.testing.refAllDecls(@import("module_discovery.zig"));
    std.testing.refAllDecls(@import("dependency_sort.zig"));
    std.testing.refAllDecls(@import("serialize_modules.zig"));
    std.testing.refAllDecls(@import("runner.zig"));

    // Actor model components
    std.testing.refAllDecls(@import("messages.zig"));
    std.testing.refAllDecls(@import("channel.zig"));
    std.testing.refAllDecls(@import("coordinator.zig"));

    std.testing.refAllDecls(@import("test/cache_test.zig"));
    std.testing.refAllDecls(@import("test/module_env_test.zig"));
    std.testing.refAllDecls(@import("test/test_build_env.zig"));
    std.testing.refAllDecls(@import("test/test_package_env.zig"));
    std.testing.refAllDecls(@import("test/type_printing_bug_test.zig"));
}
