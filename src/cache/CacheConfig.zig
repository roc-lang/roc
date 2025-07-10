//! Cache configuration and directory management for the Roc compiler cache system.
//!
//! This module provides platform-specific cache directory logic that matches
//! the Rust implementation, ensuring consistency across the codebase.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const cache_mod = @import("mod.zig");

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
    /// This implementation matches the Rust roc_cache_dir() function:
    /// - Respects XDG_CACHE_HOME if set
    /// - Falls back to ~/.cache/roc on Unix and %APPDATA%\Roc on Windows
    /// - Uses "roc" on Unix and "Roc" on Windows as the cache dir name
    pub fn getDefaultCacheDir(allocator: Allocator) ![]u8 {
        // Respect XDG_CACHE_HOME if set
        if (std.process.getEnvVarOwned(allocator, "XDG_CACHE_HOME")) |xdg_cache| {
            defer allocator.free(xdg_cache);
            return std.fs.path.join(allocator, &[_][]const u8{ xdg_cache, getCacheDirName() });
        } else |_| {
            // Fall back to platform defaults
            const home_env = switch (builtin.target.os.tag) {
                .windows => "APPDATA",
                else => "HOME",
            };

            const home_dir = std.process.getEnvVarOwned(allocator, home_env) catch {
                return error.NoHomeDirectory;
            };
            defer allocator.free(home_dir);

            const cache_path = switch (builtin.target.os.tag) {
                .linux => try std.fs.path.join(allocator, &[_][]const u8{ home_dir, ".cache", getCacheDirName() }),
                .macos => try std.fs.path.join(allocator, &[_][]const u8{ home_dir, "Library", "Caches", getCacheDirName() }),
                .windows => try std.fs.path.join(allocator, &[_][]const u8{ home_dir, getCacheDirName() }),
                else => try std.fs.path.join(allocator, &[_][]const u8{ home_dir, ".cache", getCacheDirName() }),
            };

            return cache_path;
        }
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

        const version_dir = try getCompilerVersionDir(allocator);
        defer allocator.free(version_dir);

        return std.fs.path.join(allocator, &[_][]const u8{ base_dir, version_dir });
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

    const Self = @This();

    /// Record a cache hit.
    pub fn recordHit(self: *Self, bytes_read: u64) void {
        self.hits += 1;
        self.bytes_read += bytes_read;
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
};

/// Get the platform-specific cache directory name.
/// Returns "roc" on Unix and "Roc" on Windows (matches Rust implementation).
fn getCacheDirName() []const u8 {
    return switch (builtin.target.os.tag) {
        .windows => "Roc",
        else => "roc",
    };
}

/// Get a compiler version-specific directory name.
///
/// This creates a hash-based directory name that includes compiler version
/// and build information to isolate cache entries between different builds.
fn getCompilerVersionDir(allocator: Allocator) ![]u8 {
    // Use build-time compiler version that includes git commit SHA
    const version_info = build_options.compiler_version;

    // Use BLAKE3 hash instead of SHA-256 for consistency
    const hash = cache_mod.blake3Hash(version_info);

    // Use first 16 bytes (32 hex chars) for directory name
    const hex_chars = try allocator.alloc(u8, 32);
    _ = std.fmt.bufPrint(hex_chars, "{}", .{std.fmt.fmtSliceHexLower(hash[0..16])}) catch unreachable;

    return hex_chars;
}

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

test "getCacheDirName platform specific" {
    const name = getCacheDirName();

    // Should be "roc" or "Roc" depending on platform
    try testing.expect(std.mem.eql(u8, name, "roc") or std.mem.eql(u8, name, "Roc"));
}

test "getCompilerVersionDir" {
    const allocator = testing.allocator;

    const version_dir = try getCompilerVersionDir(allocator);
    defer allocator.free(version_dir);

    // Should be 32 hex characters
    try testing.expectEqual(@as(usize, 32), version_dir.len);

    // Should only contain hex characters
    for (version_dir) |char| {
        try testing.expect(std.ascii.isHex(char));
    }
}

test "CacheStats basic operations" {
    var stats = CacheStats{};

    // Record some operations
    stats.recordHit(1024); // 1KB read
    stats.recordMiss();
    stats.recordStore(2048); // 2KB written

    try testing.expectEqual(@as(u64, 1), stats.hits);
    try testing.expectEqual(@as(u64, 1), stats.misses);
    try testing.expectEqual(@as(u64, 1), stats.stores);
    try testing.expectEqual(@as(u64, 2), stats.getTotalOps());
    try testing.expectEqual(@as(f64, 50.0), stats.getHitRate());
}

test "CacheStats hit rate calculation" {
    var stats = CacheStats{};

    // No operations - should be 0%
    try testing.expectEqual(@as(f64, 0.0), stats.getHitRate());

    // 3 hits, 1 miss = 75%
    stats.recordHit(100);
    stats.recordHit(200);
    stats.recordHit(300);
    stats.recordMiss();

    try testing.expectEqual(@as(f64, 75.0), stats.getHitRate());
}
