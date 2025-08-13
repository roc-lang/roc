//! Modern cache manager that uses BLAKE3-based keys and subdirectory splitting.

const std = @import("std");
const base = @import("base");
const fs_mod = @import("fs");
const can = @import("can");
const reporting = @import("reporting");
const collections = @import("collections");

const CacheReporting = @import("cache_reporting.zig").CacheReporting;
const CacheModule = @import("cache_module.zig").CacheModule;
const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const Filesystem = fs_mod.Filesystem;
const CacheStats = @import("cache_config.zig").CacheStats;
const CacheConfig = @import("cache_config.zig").CacheConfig;
const SERIALIZATION_ALIGNMENT = collections.SERIALIZATION_ALIGNMENT;

/// Result of a cache lookup operation
pub const CacheResult = union(enum) {
    hit: struct {
        module_env: *ModuleEnv,
        error_count: u32,
        warning_count: u32,
    },
    miss: struct {
        key: [32]u8,
    },
    not_enabled,
};

/// Cache manager using BLAKE3-based keys.
///
/// This manager combines content and compiler version into a single BLAKE3 hash,
/// then uses subdirectory splitting to organize cache files efficiently.
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

    /// Deinitialize the cache manager.
    pub fn deinit(self: *Self) void {
        _ = self;
        // Nothing to deinit currently
    }

    /// Load a cached module based on its content and compiler version.
    /// Look up a cache entry by content and compiler version.
    ///
    /// Returns CacheResult indicating hit, miss, or invalid entry.
    /// Both source and compiler_version are borrowed (not owned).
    pub fn loadFromCache(
        self: *Self,
        compiler_version: []const u8,
        source: []const u8,
        module_name: []const u8,
    ) CacheResult {
        if (!self.config.enabled) {
            return .not_enabled;
        }

        const cache_key = generateCacheKey(source, compiler_version);

        const cache_path = self.getCacheFilePath(cache_key) catch {
            return CacheResult{ .miss = .{
                .key = cache_key,
            } };
        };
        defer self.allocator.free(cache_path);

        // Check if cache file exists
        const exists = self.filesystem.fileExists(cache_path) catch false;
        if (!exists) {
            self.stats.recordMiss();
            return CacheResult{ .miss = .{
                .key = cache_key,
            } };
        }

        // Read cache data using memory mapping for better performance
        const mapped_cache = CacheModule.readFromFileMapped(self.allocator, cache_path, self.filesystem) catch |err| {
            if (self.config.verbose) {
                std.log.debug("Failed to read cache file {s}: {}", .{ cache_path, err });
            }
            self.stats.recordMiss();
            return CacheResult{ .miss = .{
                .key = cache_key,
            } };
        };
        defer mapped_cache.deinit(self.allocator);

        // Validate and restore from cache
        // restoreFromCache takes ownership of content
        const result = self.restoreFromCache(
            mapped_cache,
            source,
            module_name,
        ) catch |err| {
            if (self.config.verbose) {
                std.log.debug("Failed to restore from cache {s}: {}", .{ cache_path, err });
            }
            self.stats.recordInvalidation();
            return CacheResult{ .miss = .{
                .key = cache_key,
            } };
        };

        self.stats.recordHit(mapped_cache.data().len);
        return result;
    }

    /// Store a cache entry.
    ///
    /// Serializes the ModuleEnv and stores it in the cache using BLAKE3-based
    /// filenames with subdirectory splitting.
    pub fn store(self: *Self, cache_key: [32]u8, module_env: *const ModuleEnv, error_count: u32, warning_count: u32) !void {
        if (!self.config.enabled) {
            return;
        }

        // Ensure cache subdirectory exists
        self.ensureCacheSubdir(cache_key) catch |err| {
            if (self.config.verbose) {
                std.log.debug("Failed to create cache subdirectory: {}", .{err});
            }
            self.stats.recordStoreFailure();
            return;
        };

        // Create arena for serialization
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();

        const cache_data = CacheModule.create(self.allocator, arena.allocator(), module_env, module_env, error_count, warning_count) catch |err| {
            if (self.config.verbose) {
                std.log.debug("Failed to serialize cache data: {}", .{err});
            }
            self.stats.recordStoreFailure();
            return;
        };
        defer self.allocator.free(cache_data);

        // Get cache file path
        const cache_path = self.getCacheFilePath(cache_key) catch {
            self.stats.recordStoreFailure();
            return;
        };
        defer self.allocator.free(cache_path);

        // Write to temporary file first, then rename for atomicity
        const temp_path = std.fmt.allocPrint(self.allocator, "{s}.tmp", .{cache_path}) catch {
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

        self.stats.recordStore(cache_data.len);
    }

    /// Generate a BLAKE3-based cache key from source and compiler version.
    pub fn generateCacheKey(source: []const u8, compiler_version: []const u8) [32]u8 {
        var hasher = std.crypto.hash.Blake3.init(.{});
        hasher.update(std.mem.asBytes(&compiler_version.len));
        hasher.update(compiler_version);
        hasher.update(std.mem.asBytes(&source.len));
        hasher.update(source);
        var hash: [32]u8 = undefined;
        hasher.final(&hash);
        return hash;
    }

    /// Get the full cache file path for a given cache key.
    /// Uses subdirectory splitting: first 2 chars for subdir, rest for filename.
    pub fn getCacheFilePath(self: *Self, cache_key: [32]u8) ![]u8 {
        const entries_dir = try self.config.getCacheEntriesDir(self.allocator);
        defer self.allocator.free(entries_dir);

        // Split key: first 2 chars for subdirectory, rest for filename
        var subdir_buf: [2]u8 = undefined;
        _ = std.fmt.bufPrint(&subdir_buf, "{}", .{std.fmt.fmtSliceHexLower(cache_key[0..1])}) catch unreachable;
        const subdir = subdir_buf[0..];

        var filename_buf: [62]u8 = undefined;
        _ = std.fmt.bufPrint(&filename_buf, "{}", .{std.fmt.fmtSliceHexLower(cache_key[1..32])}) catch unreachable;
        const filename = filename_buf[0..];

        const cache_subdir = try std.fs.path.join(self.allocator, &[_][]const u8{ entries_dir, subdir });
        defer self.allocator.free(cache_subdir);

        return std.fs.path.join(self.allocator, &[_][]const u8{ cache_subdir, filename });
    }

    /// Ensure the cache subdirectory exists for the given cache key.
    fn ensureCacheSubdir(self: *Self, cache_key: [32]u8) !void {
        const entries_dir = try self.config.getCacheEntriesDir(self.allocator);
        defer self.allocator.free(entries_dir);

        // Print the hex of the first byte into a fixed-size buffer for the subdir
        var subdir_buf: [2]u8 = undefined;
        _ = std.fmt.bufPrint(&subdir_buf, "{}", .{std.fmt.fmtSliceHexLower(cache_key[0..1])}) catch unreachable;
        const subdir = subdir_buf[0..];
        const full_subdir = try std.fs.path.join(self.allocator, &[_][]const u8{ entries_dir, subdir });
        defer self.allocator.free(full_subdir);

        // Create the subdirectory
        self.filesystem.makePath(full_subdir) catch |err| switch (err) {
            error.PathAlreadyExists => {}, // OK
            else => return err,
        };
    }

    /// Get cache statistics.
    pub fn getStats(self: *const Self) CacheStats {
        return self.stats;
    }

    /// Print cache statistics if verbose mode is enabled.
    pub fn printStats(self: *const Self, allocator: Allocator) void {
        if (!self.config.verbose) return;

        const stderr = std.io.getStdErr().writer();
        CacheReporting.renderCacheStatsToTerminal(allocator, self.stats, stderr) catch {
            // If we can't print stats, just continue
        };
    }

    /// Restore a ProcessResult from cache data with diagnostic counts.
    /// IMPORTANT: This function takes ownership of `source`.
    /// The caller must not free it after calling this function.
    fn restoreFromCache(
        self: *Self,
        mapped_cache: CacheModule.CacheData,
        source: []const u8,
        module_name: []const u8,
    ) !CacheResult {
        // Validate cache format
        const cache = try CacheModule.fromMappedMemory(mapped_cache.data());

        // Restore the ModuleEnv from cache
        const module_env = try cache.restore(self.allocator, module_name, source);

        return CacheResult{ .hit = .{
            .module_env = module_env,
            .error_count = cache.header.error_count,
            .warning_count = cache.header.warning_count,
        } };
    }
};
