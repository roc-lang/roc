//! Cache configuration and directory management for the Roc compiler cache system.
//!
//! This module provides platform-specific cache directory logic that matches
//! the Rust implementation, ensuring consistency across the codebase.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");

const Allocator = std.mem.Allocator;

/// Cache configuration constants
pub const Constants = struct {
    /// Default cache directory name
    pub const DEFAULT_CACHE_DIR = ".roc_cache";

    /// Default file extension for cache files
    pub const CACHE_FILE_EXT = ".rcache";

    /// Maximum cache file size (256MB)
    pub const MAX_CACHE_SIZE = 256 * 1024 * 1024;

    /// Cache format version
    pub const CACHE_VERSION = 1;
};

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

    /// Get the module cache directory (for cached ModuleEnvs).
    pub fn getModuleCacheDir(self: Self, allocator: Allocator) ![]u8 {
        const version_dir = try self.getVersionCacheDir(allocator);
        defer allocator.free(version_dir);

        return std.fs.path.join(allocator, &[_][]const u8{ version_dir, "mod" });
    }

    /// Get the executable cache directory (for cached linked executables).
    pub fn getExeCacheDir(self: Self, allocator: Allocator) ![]u8 {
        const version_dir = try self.getVersionCacheDir(allocator);
        defer allocator.free(version_dir);

        return std.fs.path.join(allocator, &[_][]const u8{ version_dir, "exe" });
    }

    /// Get the test cache directory (for cached test results).
    pub fn getTestCacheDir(self: Self, allocator: Allocator) ![]u8 {
        const version_dir = try self.getVersionCacheDir(allocator);
        defer allocator.free(version_dir);

        return std.fs.path.join(allocator, &[_][]const u8{ version_dir, "test" });
    }

    /// Alias for getModuleCacheDir for backwards compatibility.
    pub fn getCacheEntriesDir(self: Self, allocator: Allocator) ![]u8 {
        return self.getModuleCacheDir(allocator);
    }

    /// Get the temporary directory for runtime executables.
    /// This is in the system temp dir, not the persistent cache.
    pub fn getTempDir(allocator: Allocator) ![]u8 {
        const temp_base = switch (builtin.target.os.tag) {
            .windows => std.process.getEnvVarOwned(allocator, "TEMP") catch
                std.process.getEnvVarOwned(allocator, "TMP") catch
                try allocator.dupe(u8, "C:\\Windows\\Temp"),
            else => std.process.getEnvVarOwned(allocator, "TMPDIR") catch
                try allocator.dupe(u8, "/tmp"),
        };
        defer allocator.free(temp_base);

        return std.fs.path.join(allocator, &[_][]const u8{ temp_base, "roc" });
    }

    /// Get the version-specific temporary directory for runtime executables.
    pub fn getVersionTempDir(allocator: Allocator) ![]u8 {
        const temp_base = try getTempDir(allocator);
        defer allocator.free(temp_base);

        const version_dir = try getCompilerVersionDir(allocator);
        defer allocator.free(version_dir);

        return std.fs.path.join(allocator, &[_][]const u8{ temp_base, version_dir });
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
pub fn getCacheDirName() []const u8 {
    return switch (builtin.target.os.tag) {
        .windows => "Roc",
        else => "roc",
    };
}

/// Get a compiler version-specific directory name.
///
/// Returns the human-readable compiler version string (e.g., "debug-abcd1234")
/// to isolate cache entries between different compiler builds.
pub fn getCompilerVersionDir(allocator: Allocator) ![]u8 {
    // Use build-time compiler version that includes git commit SHA
    const version_info = build_options.compiler_version;
    return allocator.dupe(u8, version_info);
}
