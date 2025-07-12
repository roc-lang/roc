//! Modern cache manager that uses BLAKE3-based keys and subdirectory splitting.

const std = @import("std");
const base = @import("../base.zig");
const canonicalize = @import("../check/canonicalize.zig");
const reporting = @import("../reporting.zig");
const Filesystem = @import("../fs/Filesystem.zig");
const cache_mod = @import("mod.zig");
const Cache = cache_mod.CacheModule;
const CacheConfig = cache_mod.CacheConfig;
const CacheStats = cache_mod.CacheStats;
const CacheReporting = @import("CacheReporting.zig");
const SERIALIZATION_ALIGNMENT = @import("../serialization/mod.zig").SERIALIZATION_ALIGNMENT;
const coordinate_simple = @import("../coordinate_simple.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = base.ModuleEnv;
const CIR = canonicalize.CIR;

/// Cache hit result containing the process result and diagnostic counts
/// Result of a cache lookup operation
pub const CacheResult = union(enum) {
    hit: coordinate_simple.ProcessResult,
    miss: struct {
        source: []const u8,
        module_path: []const u8,
        compiler_version: []const u8,
    },
    invalid: struct {
        source: []const u8,
        module_path: []const u8,
        compiler_version: []const u8,
    },
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

    /// Look up a cache entry by content and compiler version.
    ///
    /// Returns CacheResult indicating hit, miss, or invalid entry.
    ///
    /// IMPORTANT: This function takes ownership of content, module_path, and compiler_version.
    /// On cache miss/invalid, these are returned in the result for the caller to reuse.
    pub fn lookup(
        self: *Self,
        content: []const u8,
        module_path: []const u8,
        compiler_version: []const u8,
    ) !CacheResult {
        if (!self.config.enabled) {
            return CacheResult{ .miss = .{
                .source = content,
                .module_path = module_path,
                .compiler_version = compiler_version,
            } };
        }

        const cache_key = self.generateCacheKey(content, compiler_version) catch {
            return CacheResult{ .miss = .{
                .source = content,
                .module_path = module_path,
                .compiler_version = compiler_version,
            } };
        };
        defer self.allocator.free(cache_key);

        const cache_path = self.getCacheFilePath(cache_key) catch {
            return CacheResult{ .miss = .{
                .source = content,
                .module_path = module_path,
                .compiler_version = compiler_version,
            } };
        };
        defer self.allocator.free(cache_path);

        // Check if cache file exists
        const exists = self.filesystem.fileExists(cache_path) catch false;
        if (!exists) {
            self.stats.recordMiss();
            return CacheResult{ .miss = .{
                .source = content,
                .module_path = module_path,
                .compiler_version = compiler_version,
            } };
        }

        // Read cache data using memory mapping for better performance
        const mapped_cache = cache_mod.CacheModule.readFromFileMapped(self.allocator, cache_path, self.filesystem) catch |err| {
            if (self.config.verbose) {
                std.log.debug("Failed to read cache file {s}: {}", .{ cache_path, err });
            }
            self.stats.recordMiss();
            return CacheResult{ .miss = .{
                .source = content,
                .module_path = module_path,
                .compiler_version = compiler_version,
            } };
        };
        defer mapped_cache.deinit(self.allocator);

        // Validate and restore from cache
        // restoreFromCache takes ownership of content and module_path
        const result = self.restoreFromCache(
            mapped_cache.data(),
            content,
            module_path,
        ) catch |err| {
            if (self.config.verbose) {
                std.log.debug("Failed to restore from cache {s}: {}", .{ cache_path, err });
            }
            self.stats.recordInvalidation();
            return CacheResult{ .invalid = .{
                .source = content,
                .module_path = module_path,
                .compiler_version = compiler_version,
            } };
        };

        self.stats.recordHit(mapped_cache.data().len);

        // Free compiler_version since we don't need it anymore
        self.allocator.free(compiler_version);

        return CacheResult{ .hit = result.result };
    }

    /// Store a cache entry.
    ///
    /// Serializes the ProcessResult and stores it in the cache using BLAKE3-based
    /// filenames with subdirectory splitting.
    pub fn store(self: *Self, content: []const u8, compiler_version: []const u8, process_result: *const coordinate_simple.ProcessResult) !void {
        if (!self.config.enabled) {
            return;
        }

        const cache_key = self.generateCacheKey(content, compiler_version) catch {
            self.stats.recordStoreFailure();
            return;
        };
        defer self.allocator.free(cache_key);

        // Ensure cache subdirectory exists
        self.ensureCacheSubdir(cache_key) catch |err| {
            if (self.config.verbose) {
                std.log.debug("Failed to create cache subdirectory: {}", .{err});
            }
            self.stats.recordStoreFailure();
            return;
        };

        // Serialize the result
        const cache_data = self.serializeResult(process_result) catch |err| {
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

    /// Generate a BLAKE3-based cache key from content and compiler version.
    fn generateCacheKey(self: *Self, content: []const u8, compiler_version: []const u8) ![]u8 {
        // Combine content and compiler version
        const combined = try std.fmt.allocPrint(self.allocator, "{s}|{s}", .{ content, compiler_version });
        defer self.allocator.free(combined);

        // Hash with BLAKE3
        const hash = cache_mod.blake3Hash(combined);

        // Convert to hex string
        const hex_key = try self.allocator.alloc(u8, hash.len * 2);
        _ = std.fmt.bufPrint(hex_key, "{}", .{std.fmt.fmtSliceHexLower(&hash)}) catch unreachable;

        return hex_key;
    }

    /// Get the full cache file path for a given cache key.
    /// Uses subdirectory splitting: first 2 chars for subdir, rest for filename.
    fn getCacheFilePath(self: *Self, cache_key: []const u8) ![]u8 {
        const entries_dir = try self.config.getCacheEntriesDir(self.allocator);
        defer self.allocator.free(entries_dir);

        if (cache_key.len < 2) return error.InvalidCacheKey;

        // Split key: first 2 chars for subdirectory, rest for filename
        const subdir = cache_key[0..2];
        const filename = cache_key[2..];

        const cache_subdir = try std.fs.path.join(self.allocator, &[_][]const u8{ entries_dir, subdir });
        defer self.allocator.free(cache_subdir);

        return std.fs.path.join(self.allocator, &[_][]const u8{ cache_subdir, filename });
    }

    /// Ensure the cache subdirectory exists for the given cache key.
    fn ensureCacheSubdir(self: *Self, cache_key: []const u8) !void {
        const entries_dir = try self.config.getCacheEntriesDir(self.allocator);
        defer self.allocator.free(entries_dir);

        if (cache_key.len < 2) return error.InvalidCacheKey;

        const subdir = cache_key[0..2];
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

    /// Serialize a ProcessResult to cache data.
    fn serializeResult(self: *Self, result: *const coordinate_simple.ProcessResult) ![]u8 {
        // Store error and warning reports in cache
        const error_count = result.error_count;
        const warning_count = result.warning_count;

        // Create cache data using the ModuleEnv from the CIR with diagnostic counts
        const cache_data = try Cache.create(self.allocator, result.cir.env, result.cir, error_count, warning_count);

        return cache_data;
    }

    /// Restore a ProcessResult from cache data with diagnostic counts.
    ///
    /// IMPORTANT: This function takes ownership of both `source` and `module_path`.
    /// The caller must not free these after calling this function.
    fn restoreFromCache(
        self: *Self,
        cache_data: []align(SERIALIZATION_ALIGNMENT) const u8,
        source: []const u8,
        module_path: []const u8,
    ) !struct {
        result: coordinate_simple.ProcessResult,
        error_count: u32,
        warning_count: u32,
    } {
        // Load cache using existing Cache functionality
        var cache = cache_mod.CacheModule.fromMappedMemory(cache_data) catch return error.InvalidCache;

        // Validate cache
        cache.validate() catch return error.InvalidCache;

        // Restore the data
        // Use a default module name when restoring from cache
        // since we don't have access to the original source path
        const module_name = "cached_module";
        // Transfer ownership of source and module_path to the restored ModuleEnv
        const restored = cache.restore(self.allocator, module_name, source, module_path) catch return error.RestoreError;

        // Reports are not cached - they need to be recomputed if needed
        // Users can use --no-cache to see diagnostic reports
        const reports = try self.allocator.alloc(reporting.Report, 0);

        // Allocate and copy ModuleEnv to heap for ownership
        const module_env = try self.allocator.create(ModuleEnv);
        module_env.* = restored.module_env;

        // Note: source and module_path ownership was already transferred to module_env by cache.restore()

        // Allocate CIR to heap for ownership
        const cir = try self.allocator.create(CIR);

        // Copy CIR but don't copy the invalid env pointer
        cir.* = restored.cir;
        // Immediately fix env pointer to point to our heap-allocated module_env
        cir.env = module_env;

        // Create ProcessResult with proper ownership
        // Use the same source as ModuleEnv to avoid inconsistency
        const process_result = coordinate_simple.ProcessResult{
            .cir = cir,
            .reports = reports,
            .source = module_env.source,
            .was_cached = true,
        };

        // Return both the process result and diagnostic counts from cache header
        return .{
            .result = process_result,
            .error_count = cache.header.error_count,
            .warning_count = cache.header.warning_count,
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

test "CacheManager generateCacheKey" {
    const allocator = testing.allocator;
    const config = CacheConfig{};
    const filesystem = Filesystem.testing();

    var manager = CacheManager.init(allocator, config, filesystem);

    const content = "module [test]\n\ntest = 42";
    const compiler_version = "roc-zig-0.11.0-debug";

    const key1 = try manager.generateCacheKey(content, compiler_version);
    defer allocator.free(key1);
    const key2 = try manager.generateCacheKey(content, compiler_version);
    defer allocator.free(key2);

    // Same input should produce same key
    try testing.expectEqualStrings(key1, key2);

    // Should be 64 hex characters (32 bytes * 2)
    try testing.expectEqual(@as(usize, 64), key1.len);

    // Should only contain hex characters
    for (key1) |char| {
        try testing.expect(std.ascii.isHex(char));
    }
}

test "CacheManager getCacheFilePath with subdirectory splitting" {
    const allocator = testing.allocator;
    const config = CacheConfig{};
    const filesystem = Filesystem.testing();

    var manager = CacheManager.init(allocator, config, filesystem);

    const cache_key = "abcdef123456789";
    const cache_path = try manager.getCacheFilePath(cache_key);
    defer allocator.free(cache_path);

    // Should contain subdirectory split
    try testing.expect(std.mem.containsAtLeast(u8, cache_path, 1, "ab"));
    try testing.expect(std.mem.containsAtLeast(u8, cache_path, 1, "cdef123456789"));
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

    const content = try allocator.dupe(u8, "module [test]\n\ntest = 42");
    const module_path = try allocator.dupe(u8, "test.roc");
    const compiler_version = try allocator.dupe(u8, "roc-zig-0.11.0-debug");

    const result = try manager.lookup(content, module_path, compiler_version);
    switch (result) {
        .miss => |returned| {
            try testing.expect(manager.stats.misses == 1);
            // Free the returned values
            allocator.free(returned.source);
            allocator.free(returned.module_path);
            allocator.free(returned.compiler_version);
        },
        else => return error.TestUnexpectedResult,
    }
}

test "CacheManager disabled" {
    const allocator = testing.allocator;
    const config = CacheConfig{ .enabled = false };
    const filesystem = Filesystem.testing();

    var manager = CacheManager.init(allocator, config, filesystem);

    const content = try allocator.dupe(u8, "module [test]\n\ntest = 42");
    const module_path = try allocator.dupe(u8, "test.roc");
    const compiler_version = try allocator.dupe(u8, "roc-zig-0.11.0-debug");

    const result = try manager.lookup(content, module_path, compiler_version);
    switch (result) {
        .miss => |returned| {
            // Free the returned values
            allocator.free(returned.source);
            allocator.free(returned.module_path);
            allocator.free(returned.compiler_version);
        },
        else => return error.TestUnexpectedResult,
    }
    try testing.expect(manager.stats.getTotalOps() == 0); // No stats recorded when disabled
}

test "CacheManager short cache key error" {
    const allocator = testing.allocator;
    const config = CacheConfig{};
    const filesystem = Filesystem.testing();

    var manager = CacheManager.init(allocator, config, filesystem);

    const short_key = "x"; // Too short for subdirectory splitting
    const result = manager.getCacheFilePath(short_key);
    try testing.expectError(error.InvalidCacheKey, result);
}
