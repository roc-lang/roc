//! Cache module for the Roc compiler
//!
//! This module provides memory-mapped caching for compiled Roc modules,
//! allowing fast serialization and deserialization of ModuleEnv and CIR data.

const std = @import("std");

// Re-export the unified cache
pub const CacheModule = @import("CacheModule.zig").CacheModule;
pub const Header = @import("CacheModule.zig").Header;
pub const Diagnostics = @import("CacheModule.zig").Diagnostics;

// Re-export new cache management components
pub const CacheManager = @import("CacheManager.zig").CacheManager;
pub const CacheResult = @import("CacheManager.zig").CacheResult;
pub const CacheConfig = @import("CacheConfig.zig").CacheConfig;
pub const CacheStats = @import("CacheConfig.zig").CacheStats;

/// Cache configuration constants
pub const Config = struct {
    /// Default cache directory name
    pub const DEFAULT_CACHE_DIR = ".roc_cache";

    /// Default file extension for cache files
    pub const CACHE_FILE_EXT = ".rcache";

    /// Maximum cache file size (256MB)
    pub const MAX_CACHE_SIZE = 256 * 1024 * 1024;

    /// Cache format version
    pub const CACHE_VERSION = 1;
};

/// Statistics for cache operations (optional, for debugging)
pub const Stats = struct {
    hits: u32 = 0,
    misses: u32 = 0,
    writes: u32 = 0,
    errors: u32 = 0,
    total_bytes_written: u64 = 0,
    total_bytes_read: u64 = 0,

    pub fn reset(self: *Stats) void {
        self.* = .{};
    }

    pub fn hitRate(self: *const Stats) f64 {
        const total = self.hits + self.misses;
        if (total == 0) return 0.0;
        return @as(f64, @floatFromInt(self.hits)) / @as(f64, @floatFromInt(total));
    }

    pub fn print(self: *const Stats, writer: std.io.AnyWriter) !void {
        try writer.print("Cache Stats:\n", .{});
        try writer.print("  Hits: {d}\n", .{self.hits});
        try writer.print("  Misses: {d}\n", .{self.misses});
        try writer.print("  Writes: {d}\n", .{self.writes});
        try writer.print("  Errors: {d}\n", .{self.errors});
        try writer.print("  Hit Rate: {d:.2}%\n", .{self.hitRate() * 100.0});
        try writer.print("  Bytes Written: {d}\n", .{self.total_bytes_written});
        try writer.print("  Bytes Read: {d}\n", .{self.total_bytes_read});
    }
};

/// Global cache statistics (optional, for debugging)
var global_stats: Stats = .{};

/// Get reference to global stats
pub fn getGlobalStats() *Stats {
    return &global_stats;
}

/// Reset global stats
pub fn resetGlobalStats() void {
    global_stats.reset();
}

/// Print global stats to stderr
pub fn printGlobalStats() !void {
    const stderr = std.io.getStdErr().writer();
    try global_stats.print(stderr.any());
}

/// Hashes the given data using the BLAKE3 algorithm.
pub fn blake3Hash(data: []const u8) [32]u8 {
    var digest: [32]u8 = undefined;
    std.crypto.hash.Blake3.hash(data, &digest, .{});
    return digest;
}

test "cache module" {
    // Basic test to ensure module compiles and types are accessible
    const allocator = std.testing.allocator;

    // Test that we can access the main types
    _ = CacheModule;
    _ = Header;
    _ = Diagnostics;
    _ = CacheManager;
    _ = CacheResult;
    _ = CacheConfig;
    _ = CacheStats;

    // Test stats functionality
    var stats = Stats{};
    stats.hits = 10;
    stats.misses = 5;
    try std.testing.expectEqual(@as(f64, 2.0 / 3.0), stats.hitRate());

    // Test config constants
    try std.testing.expect(std.mem.eql(u8, Config.DEFAULT_CACHE_DIR, ".roc_cache"));
    try std.testing.expect(std.mem.eql(u8, Config.CACHE_FILE_EXT, ".rcache"));

    _ = allocator; // Suppress unused variable warning
}

test "cache tests" {
    std.testing.refAllDecls(@import("CacheConfig.zig"));
    std.testing.refAllDecls(@import("CacheKey.zig"));
    std.testing.refAllDecls(@import("CacheManager.zig"));
    std.testing.refAllDecls(@import("CacheModule.zig"));
    std.testing.refAllDecls(@import("CacheReporting.zig"));
}
