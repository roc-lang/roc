//! Central cache manager that handles cache operations, directory management, and statistics tracking.

const std = @import("std");
const base = @import("../base.zig");
const canonicalize = @import("../check/canonicalize.zig");
const reporting = @import("../reporting.zig");
const Filesystem = @import("../fs/Filesystem.zig");
const cache_mod = @import("mod.zig");
const Cache = cache_mod.CacheModule;
const CacheKey = cache_mod.CacheKey;
const CacheConfig = cache_mod.CacheConfig;
const CacheStats = cache_mod.CacheStats;
const SERIALIZATION_ALIGNMENT = @import("../serialization/mod.zig").SERIALIZATION_ALIGNMENT;
const coordinate_simple = @import("../coordinate_simple.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = base.ModuleEnv;
const CIR = canonicalize.CIR;

/// Result of a cache lookup operation.
pub const CacheResult = union(enum) {
    hit: coordinate_simple.ProcessResult,
    miss: void,
    invalid: void,
};

/// Central cache manager for handling all cache operations.
///
/// This manager handles cache directory setup, file operations,
/// and maintains cache statistics. It provides a clean interface
/// for cache operations while handling errors gracefully.
pub const CacheManager = struct {
    config: CacheConfig,
    filesystem: Filesystem,
    allocator: Allocator,
    stats: CacheStats,

    const Self = @This();

    /// Initialize a new cache manager.
    pub fn init(allocator: Allocator, config: CacheConfig, filesystem: Filesystem) Self {
        return Self{
            .config = config,
            .filesystem = filesystem,
            .allocator = allocator,
            .stats = CacheStats{},
        };
    }

    /// Look up a cache entry by key.
    ///
    /// Returns CacheResult indicating hit, miss, or invalid entry.
    /// On cache hit, the returned ProcessResult owns all its data.
    pub fn lookup(self: *Self, key: CacheKey) !CacheResult {
        if (!self.config.enabled) {
            return CacheResult.miss;
        }

        const start_time = std.time.nanoTimestamp();

        const cache_filename = key.toCacheFileName(self.allocator) catch {
            return CacheResult.miss;
        };
        defer self.allocator.free(cache_filename);

        const entries_dir = self.config.getCacheEntriesDir(self.allocator) catch {
            return CacheResult.miss;
        };
        defer self.allocator.free(entries_dir);

        const cache_path = std.fs.path.join(self.allocator, &[_][]const u8{ entries_dir, cache_filename }) catch {
            return CacheResult.miss;
        };
        defer self.allocator.free(cache_path);

        // Store the original file path from the key for later use
        const source_path = try key.getSourcePath(self.allocator);
        defer self.allocator.free(source_path);

        // Check if cache file exists
        const exists = self.filesystem.fileExists(cache_path) catch false;
        if (!exists) {
            self.stats.recordMiss();
            return CacheResult.miss;
        }

        // Read cache data using memory mapping for better performance
        const mapped_cache = cache_mod.CacheModule.readFromFileMapped(self.allocator, cache_path, self.filesystem) catch |err| {
            if (self.config.verbose) {
                std.log.debug("Failed to read cache file {s}: {}", .{ cache_path, err });
            }
            self.stats.recordMiss();
            return CacheResult.miss;
        };
        defer mapped_cache.deinit(self.allocator);

        // Validate and restore from cache
        const result = self.restoreFromCache(mapped_cache.data(), key, source_path) catch |err| {
            if (self.config.verbose) {
                std.log.debug("Failed to restore from cache {s}: {}", .{ cache_path, err });
            }
            self.stats.recordInvalidation();
            return CacheResult.invalid;
        };

        const end_time = std.time.nanoTimestamp();
        const time_saved = end_time - start_time;

        self.stats.recordHit(mapped_cache.data().len, @as(u64, @intCast(time_saved)));

        return CacheResult{ .hit = result };
    }

    /// Store a cache entry.
    ///
    /// Serializes the ProcessResult and stores it in the cache.
    /// Failures are logged but don't propagate to avoid breaking compilation.
    pub fn store(self: *Self, key: CacheKey, result: *const coordinate_simple.ProcessResult) !void {
        if (!self.config.enabled) {
            return;
        }

        const start_time = std.time.nanoTimestamp();

        // Ensure cache directories exist
        self.ensureCacheDir() catch |err| {
            if (self.config.verbose) {
                std.log.debug("Failed to create cache directory: {}", .{err});
            }
            self.stats.recordStoreFailure();
            return;
        };

        // Serialize the result
        const cache_data = self.serializeResult(result) catch |err| {
            if (self.config.verbose) {
                std.log.debug("Failed to serialize cache data: {}", .{err});
            }
            self.stats.recordStoreFailure();
            return;
        };
        defer self.allocator.free(cache_data);

        // Get cache file path
        const cache_filename = key.toCacheFileName(self.allocator) catch {
            self.stats.recordStoreFailure();
            return;
        };
        defer self.allocator.free(cache_filename);

        const entries_dir = self.config.getCacheEntriesDir(self.allocator) catch {
            self.stats.recordStoreFailure();
            return;
        };
        defer self.allocator.free(entries_dir);

        const cache_path = std.fs.path.join(self.allocator, &[_][]const u8{ entries_dir, cache_filename }) catch {
            self.stats.recordStoreFailure();
            return;
        };
        defer self.allocator.free(cache_path);

        // Write to temporary file first, then rename for atomicity
        const temp_dir = self.config.getTempDir(self.allocator) catch {
            self.stats.recordStoreFailure();
            return;
        };
        defer self.allocator.free(temp_dir);

        const temp_filename = std.fmt.allocPrint(self.allocator, "{s}.tmp", .{cache_filename}) catch {
            self.stats.recordStoreFailure();
            return;
        };
        defer self.allocator.free(temp_filename);

        const temp_path = std.fs.path.join(self.allocator, &[_][]const u8{ temp_dir, temp_filename }) catch {
            self.stats.recordStoreFailure();
            return;
        };
        defer self.allocator.free(temp_path);

        // Write to temp file
        self.filesystem.writeFile(temp_path, cache_data) catch |err| {
            if (self.config.verbose) {
                std.log.debug("Failed to write cache temp file {s}: {}", .{ temp_path, err });
            }
            self.stats.recordStoreFailure();
            return;
        };

        // Move temp file to final location (atomic operation)
        self.filesystem.rename(temp_path, cache_path) catch |err| {
            if (self.config.verbose) {
                std.log.debug("Failed to rename cache file {s} -> {s}: {}", .{ temp_path, cache_path, err });
            }
            self.stats.recordStoreFailure();
            return;
        };

        const end_time = std.time.nanoTimestamp();
        self.stats.recordStore(cache_data.len);

        if (self.config.verbose) {
            const time_ms = @as(f64, @floatFromInt(end_time - start_time)) / 1_000_000.0;
            std.log.debug("Stored cache entry {s} ({d:.1} MB in {d:.1} ms)", .{
                cache_path,
                @as(f64, @floatFromInt(cache_data.len)) / (1024.0 * 1024.0),
                time_ms,
            });
        }
    }

    /// Ensure cache directories exist.
    pub fn ensureCacheDir(self: *Self) !void {
        const entries_dir = try self.config.getCacheEntriesDir(self.allocator);
        defer self.allocator.free(entries_dir);

        const temp_dir = try self.config.getTempDir(self.allocator);
        defer self.allocator.free(temp_dir);

        // Create directories
        self.filesystem.makePath(entries_dir) catch |err| switch (err) {
            error.PathAlreadyExists => {}, // OK
            else => return err,
        };

        self.filesystem.makePath(temp_dir) catch |err| switch (err) {
            error.PathAlreadyExists => {}, // OK
            else => return err,
        };
    }

    /// Get cache statistics.
    pub fn getStats(self: *const Self) CacheStats {
        return self.stats;
    }

    /// Print cache statistics if verbose mode is enabled.
    pub fn printStats(self: *const Self) void {
        if (!self.config.verbose) return;

        const stderr = std.io.getStdErr().writer();
        self.stats.print(stderr) catch {
            // If we can't print stats, just continue
        };
    }

    /// Serialize a ProcessResult to cache data.
    fn serializeResult(self: *Self, result: *const coordinate_simple.ProcessResult) ![]u8 {
        // Note: We don't cache reports - they can be recomputed if needed
        // Create cache data using the ModuleEnv from the CIR
        const cache_data = try Cache.create(self.allocator, result.cir.env, result.cir);

        return cache_data;
    }

    /// Restore a ProcessResult from cache data.
    fn restoreFromCache(self: *Self, cache_data: []align(SERIALIZATION_ALIGNMENT) const u8, key: CacheKey, source_path: []const u8) !coordinate_simple.ProcessResult {
        // Load cache using existing Cache functionality
        var cache = cache_mod.CacheModule.fromMappedMemory(cache_data) catch return error.InvalidCache;

        // Validate cache
        cache.validate() catch return error.InvalidCache;

        // Restore the data
        const restored = cache.restore(self.allocator) catch return error.RestoreError;

        // Reports are not cached - they need to be recomputed if needed
        // Users can use --no-cache to see diagnostic reports
        std.log.info("Loaded from cache - diagnostic reports not shown. Use --no-cache to see Errors and Warnings for this module.", .{});
        const reports = try self.allocator.alloc(reporting.Report, 0);

        // Allocate and copy ModuleEnv to heap for ownership
        const module_env = try self.allocator.create(ModuleEnv);
        module_env.* = restored.module_env;

        // Allocate CIR to heap for ownership
        const cir = try self.allocator.create(CIR);

        // Copy CIR but don't copy the invalid env pointer
        cir.* = restored.cir;
        // Immediately fix env pointer to point to our heap-allocated module_env
        cir.env = module_env;

        // Re-read the source file - we need it for any potential error reporting
        const source = self.filesystem.readFile(source_path, self.allocator) catch |err| blk: {
            // If we can't read the source, provide a fallback
            if (self.config.verbose) {
                std.log.debug("Failed to read source file {s}: {}", .{ source_path, err });
            }
            break :blk try self.allocator.dupe(u8, "# Source file not available");
        };

        // Create ProcessResult with proper ownership
        return coordinate_simple.ProcessResult{
            .cir = cir,
            .reports = reports,
            .source = source,
            .cache_key = key,
            .was_cached = true,
        };
    }
};

// Tests
const testing = std.testing;

test "CacheManager initialization" {
    const allocator = testing.allocator;
    const config = CacheConfig{};
    const filesystem = Filesystem.testing();

    var manager = CacheManager.init(allocator, config, filesystem);

    try testing.expect(manager.config.enabled == true);
    try testing.expect(manager.stats.getTotalOps() == 0);
}

test "CacheManager lookup miss" {
    const allocator = testing.allocator;
    const config = CacheConfig{};
    var filesystem = Filesystem.testing();

    // Mock fileExists to return false
    const TestFS = struct {
        fn fileExists(path: []const u8) Filesystem.OpenError!bool {
            _ = path;
            return false;
        }
    };
    filesystem.fileExists = TestFS.fileExists;

    var manager = CacheManager.init(allocator, config, filesystem);

    const key = CacheKey{
        .content_hash = [_]u8{0} ** 32,
        .file_mtime = 0,
        .compiler_version = [_]u8{0} ** 32,
        .source_path = "test.roc",
    };

    const result = try manager.lookup(key);
    try testing.expect(result == .miss);
    try testing.expect(manager.stats.misses == 1);
}

test "CacheManager disabled" {
    const allocator = testing.allocator;
    const config = CacheConfig{ .enabled = false };
    const filesystem = Filesystem.testing();

    var manager = CacheManager.init(allocator, config, filesystem);

    const key = CacheKey{
        .content_hash = [_]u8{0} ** 32,
        .file_mtime = 0,
        .compiler_version = [_]u8{0} ** 32,
        .source_path = "test.roc",
    };

    const result = try manager.lookup(key);
    try testing.expect(result == .miss);
    try testing.expect(manager.stats.getTotalOps() == 0); // No stats recorded when disabled
}
