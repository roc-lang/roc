//! Cache configuration and statistics tracking for the Roc compiler cache system.

const std = @import("std");

const Allocator = std.mem.Allocator;

/// Configuration for the Roc cache system.
///
/// This struct controls cache behavior including storage location,
/// size limits, and cleanup policies.
pub const CacheConfig = struct {
    enabled: bool = true,
    cache_dir: ?[]const u8 = null, // null = use default
    max_size_mb: u32 = 1024, // 1GB default
    max_age_days: u32 = 30, // 30 days default
    verbose: bool = false, // Print cache statistics

    const Self = @This();

    /// Get the default cache directory for the current platform.
    ///
    /// Uses platform-specific cache directories:
    /// - Linux: ~/.cache/roc
    /// - macOS: ~/Library/Caches/roc
    /// - Windows: %LOCALAPPDATA%\roc\cache
    pub fn getDefaultCacheDir(allocator: Allocator) ![]u8 {
        const home_dir = std.posix.getenv("HOME") orelse {
            return error.NoHomeDirectory;
        };

        const cache_subdir = switch (@import("builtin").target.os.tag) {
            .linux => ".cache/roc",
            .macos => "Library/Caches/roc",
            .windows => "AppData/Local/roc/cache",
            else => ".cache/roc", // fallback to Linux style
        };

        return std.fs.path.join(allocator, &[_][]const u8{ home_dir, cache_subdir });
    }

    /// Get the effective cache directory, using default if none specified.
    pub fn getEffectiveCacheDir(self: Self, allocator: Allocator) ![]u8 {
        if (self.cache_dir) |dir| {
            return allocator.dupe(u8, dir);
        } else {
            return getDefaultCacheDir(allocator);
        }
    }

    /// Get the version-specific cache directory.
    ///
    /// This isolates cache entries by compiler version to prevent
    /// conflicts when switching between compiler versions.
    pub fn getVersionCacheDir(self: Self, allocator: Allocator) ![]u8 {
        const base_dir = try self.getEffectiveCacheDir(allocator);
        defer allocator.free(base_dir);

        // Create a simple version hash for directory isolation
        const version_hash = comptime blk: {
            const zig_version = @import("builtin").zig_version;
            const version_info = std.fmt.comptimePrint("roc-{d}.{d}.{d}-{s}", .{
                zig_version.major,
                zig_version.minor,
                zig_version.patch,
                @tagName(@import("builtin").mode),
            });

            var hasher = std.crypto.hash.sha2.Sha256.init(.{});
            hasher.update(version_info);
            const hash = hasher.finalResult();

            // Use first 16 hex chars for directory name
            var hex_buf: [32]u8 = undefined;
            _ = std.fmt.bufPrint(&hex_buf, "{}", .{std.fmt.fmtSliceHexLower(hash[0..16])}) catch unreachable;
            break :blk hex_buf;
        };

        return std.fs.path.join(allocator, &[_][]const u8{ base_dir, &version_hash });
    }

    /// Get the cache entries directory.
    pub fn getCacheEntriesDir(self: Self, allocator: Allocator) ![]u8 {
        const version_dir = try self.getVersionCacheDir(allocator);
        defer allocator.free(version_dir);

        return std.fs.path.join(allocator, &[_][]const u8{ version_dir, "entries" });
    }

    /// Get the temporary directory for cache operations.
    pub fn getTempDir(self: Self, allocator: Allocator) ![]u8 {
        const version_dir = try self.getVersionCacheDir(allocator);
        defer allocator.free(version_dir);

        return std.fs.path.join(allocator, &[_][]const u8{ version_dir, "temp" });
    }

    /// Get maximum cache size in bytes.
    pub fn getMaxSizeBytes(self: Self) u64 {
        return @as(u64, self.max_size_mb) * 1024 * 1024;
    }

    /// Get maximum age in nanoseconds.
    pub fn getMaxAgeNanos(self: Self) i64 {
        return @as(i64, self.max_age_days) * 24 * 60 * 60 * 1_000_000_000;
    }
};

/// Statistics tracking for cache operations.
///
/// This struct tracks cache performance metrics that can be
/// displayed with the --verbose flag.
pub const CacheStats = struct {
    hits: u64 = 0,
    misses: u64 = 0,
    invalidations: u64 = 0,
    stores: u64 = 0,
    store_failures: u64 = 0,
    bytes_read: u64 = 0,
    bytes_written: u64 = 0,
    time_saved_ns: u64 = 0, // Time saved by cache hits

    const Self = @This();

    /// Record a cache hit.
    pub fn recordHit(self: *Self, bytes_read: u64, time_saved_ns: u64) void {
        self.hits += 1;
        self.bytes_read += bytes_read;
        self.time_saved_ns += time_saved_ns;
    }

    /// Record a cache miss.
    pub fn recordMiss(self: *Self) void {
        self.misses += 1;
    }

    /// Record a cache invalidation.
    pub fn recordInvalidation(self: *Self) void {
        self.invalidations += 1;
    }

    /// Record a successful cache store.
    pub fn recordStore(self: *Self, bytes_written: u64) void {
        self.stores += 1;
        self.bytes_written += bytes_written;
    }

    /// Record a failed cache store.
    pub fn recordStoreFailure(self: *Self) void {
        self.store_failures += 1;
    }

    /// Get total cache operations.
    pub fn getTotalOps(self: Self) u64 {
        return self.hits + self.misses;
    }

    /// Get cache hit rate as a percentage.
    pub fn getHitRate(self: Self) f64 {
        const total = self.getTotalOps();
        if (total == 0) return 0.0;
        return (@as(f64, @floatFromInt(self.hits)) / @as(f64, @floatFromInt(total))) * 100.0;
    }

    /// Get time saved in milliseconds.
    pub fn getTimeSavedMs(self: Self) f64 {
        return @as(f64, @floatFromInt(self.time_saved_ns)) / 1_000_000.0;
    }

    /// Print cache statistics to stderr.
    pub fn print(self: Self, writer: anytype) !void {
        const total_ops = self.getTotalOps();
        if (total_ops == 0) {
            try writer.print("Cache: No operations performed\n", .{});
            return;
        }

        try writer.print("Cache Statistics:\n", .{});
        try writer.print("  Operations: {} total ({} hits, {} misses)\n", .{ total_ops, self.hits, self.misses });
        try writer.print("  Hit rate: {d:.1}%\n", .{self.getHitRate()});
        try writer.print("  Data: {d:.1} MB read, {d:.1} MB written\n", .{
            @as(f64, @floatFromInt(self.bytes_read)) / (1024.0 * 1024.0),
            @as(f64, @floatFromInt(self.bytes_written)) / (1024.0 * 1024.0),
        });
        try writer.print("  Time saved: {d:.1} ms\n", .{self.getTimeSavedMs()});
        try writer.print("  Stores: {} successful, {} failed\n", .{ self.stores, self.store_failures });
        if (self.invalidations > 0) {
            try writer.print("  Invalidations: {}\n", .{self.invalidations});
        }
    }
};

// Tests
const testing = std.testing;

test "CacheConfig default values" {
    const config = CacheConfig{};

    try testing.expect(config.enabled == true);
    try testing.expect(config.cache_dir == null);
    try testing.expect(config.max_size_mb == 1024);
    try testing.expect(config.max_age_days == 30);
    try testing.expect(config.verbose == false);
}

test "CacheConfig getMaxSizeBytes" {
    const config = CacheConfig{ .max_size_mb = 100 };

    try testing.expectEqual(@as(u64, 100 * 1024 * 1024), config.getMaxSizeBytes());
}

test "CacheConfig getMaxAgeNanos" {
    const config = CacheConfig{ .max_age_days = 7 };

    const expected = @as(i64, 7) * 24 * 60 * 60 * 1_000_000_000;
    try testing.expectEqual(expected, config.getMaxAgeNanos());
}

test "CacheConfig getEffectiveCacheDir with explicit dir" {
    const allocator = testing.allocator;
    const config = CacheConfig{ .cache_dir = "/custom/cache" };

    const dir = try config.getEffectiveCacheDir(allocator);
    defer allocator.free(dir);

    try testing.expectEqualStrings("/custom/cache", dir);
}

test "CacheStats basic operations" {
    var stats = CacheStats{};

    // Record some operations
    stats.recordHit(1024, 1000000); // 1KB read, 1ms saved
    stats.recordMiss();
    stats.recordStore(2048); // 2KB written

    try testing.expectEqual(@as(u64, 1), stats.hits);
    try testing.expectEqual(@as(u64, 1), stats.misses);
    try testing.expectEqual(@as(u64, 1), stats.stores);
    try testing.expectEqual(@as(u64, 2), stats.getTotalOps());
    try testing.expectEqual(@as(f64, 50.0), stats.getHitRate());
    try testing.expectEqual(@as(f64, 1.0), stats.getTimeSavedMs());
}

test "CacheStats hit rate calculation" {
    var stats = CacheStats{};

    // No operations - should be 0%
    try testing.expectEqual(@as(f64, 0.0), stats.getHitRate());

    // 3 hits, 1 miss = 75%
    stats.recordHit(100, 1000);
    stats.recordHit(200, 2000);
    stats.recordHit(300, 3000);
    stats.recordMiss();

    try testing.expectEqual(@as(f64, 75.0), stats.getHitRate());
}
