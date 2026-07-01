//! Compilation-related types and functionality, such as cache management and package building.

const std = @import("std");
const Allocator = std.mem.Allocator;
const threading_mod = @import("threading.zig");

pub const package = @import("compile_package.zig");
pub const build = @import("compile_build.zig");
pub const targets_config = @import("targets_config.zig");
pub const TargetsConfig = targets_config.TargetsConfig;
pub const single_module = @import("compile_module.zig");
pub const module_discovery = @import("module_discovery.zig");
pub const dependency_sort = @import("dependency_sort.zig");
pub const threading = @import("threading.zig");
pub const static_data_exports = @import("static_data_exports.zig");
pub const package_source = @import("package_source.zig");
pub const package_resolution = @import("package_resolution.zig");
pub const watch_inputs = @import("watch_inputs.zig");

// Actor model components
pub const messages = @import("messages.zig");
pub const channel = @import("channel.zig");
pub const coordinator = @import("coordinator.zig");
pub const app_header = @import("app_header.zig");

pub const key = @import("cache_key.zig");
pub const config = @import("cache_config.zig");
pub const reporting = @import("cache_reporting.zig");
pub const manager = @import("cache_manager.zig");
pub const cleanup = if (!threading_mod.is_freestanding) @import("cache_cleanup.zig") else struct {
    pub const CleanupStats = struct {
        temp_dirs_deleted: u32 = 0,
        temp_files_deleted: u32 = 0,
        cache_files_deleted: u32 = 0,
        empty_dirs_deleted: u32 = 0,
        errors: u32 = 0,
    };

    pub const CleanupThread = struct {};

    pub fn startBackgroundCleanup(_: []const u8, _: []const u8, _: std.Io) Allocator.Error!?CleanupThread {
        return null;
    }

    pub fn deleteTempDir(_: std.Io, _: []const u8) void {}
};

pub const CacheManager = manager.CacheManager;
pub const CacheConfig = config.CacheConfig;
pub const CacheStats = config.CacheStats;
/// Cache cleanup utilities for managing temporary and persistent cache files.
pub const CacheCleanup = cleanup;
pub const CleanupStats = cleanup.CleanupStats;
pub const PackageEnv = package.PackageEnv;
pub const BuildEnv = build.BuildEnv;
pub const CoreCtx = @import("ctx").CoreCtx;

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
//     // TODO: Use CoreCtx abstraction for stderr output
//     // try global_stats.print(stderr);
// }

test "compile tests" {
    if (!threading_mod.is_freestanding) {
        std.testing.refAllDecls(@import("cache_cleanup.zig"));
    }
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
    std.testing.refAllDecls(@import("static_data_exports.zig"));
    std.testing.refAllDecls(@import("package_source.zig"));
    std.testing.refAllDecls(@import("package_resolution.zig"));
    std.testing.refAllDecls(@import("watch_inputs.zig"));

    // Actor model components
    std.testing.refAllDecls(@import("messages.zig"));
    std.testing.refAllDecls(@import("channel.zig"));
    std.testing.refAllDecls(@import("coordinator.zig"));
    std.testing.refAllDecls(@import("app_header.zig"));

    std.testing.refAllDecls(@import("test/cache_test.zig"));
    std.testing.refAllDecls(@import("test/test_build_env.zig"));
    std.testing.refAllDecls(@import("test/test_package_env.zig"));
    std.testing.refAllDecls(@import("test/module_env_test.zig"));
    std.testing.refAllDecls(@import("test/type_printing_bug_test.zig"));
    std.testing.refAllDecls(@import("test/embedding_smoke.zig"));
    std.testing.refAllDecls(@import("test/hoisted_constants_test.zig"));
    std.testing.refAllDecls(@import("test/issue_9614_test.zig"));
    std.testing.refAllDecls(@import("test/issue_9634_test.zig"));
    std.testing.refAllDecls(@import("test/issue_806_stack_aggregate_test.zig"));
    std.testing.refAllDecls(@import("test/issue_9703_test.zig"));
    std.testing.refAllDecls(@import("test/issue_9704_test.zig"));
    std.testing.refAllDecls(@import("test/issue_9825_test.zig"));
    std.testing.refAllDecls(@import("test/issue_9884_test.zig"));
    std.testing.refAllDecls(@import("test/tce_capture_test.zig"));
    std.testing.refAllDecls(@import("test/list_map_target_independent_lir_test.zig"));
    std.testing.refAllDecls(@import("test/url_package_test.zig"));
}
