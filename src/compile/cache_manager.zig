//! Modern cache manager that uses BLAKE3-based keys and subdirectory splitting.

const std = @import("std");
const fs_mod = @import("fs");
const can = @import("can");

const CacheReporting = @import("cache_reporting.zig").CacheReporting;
const CacheModule = @import("cache_module.zig").CacheModule;
const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const Filesystem = fs_mod.Filesystem;
const CacheStats = @import("cache_config.zig").CacheStats;
const CacheConfig = @import("cache_config.zig").CacheConfig;
const builtin = @import("builtin");

const is_windows = builtin.target.os.tag == .windows;

var stderr_file_writer: std.fs.File.Writer = .{
    .interface = std.fs.File.Writer.initInterface(&.{}),
    .file = if (is_windows) undefined else std.fs.File.stderr(),
    .mode = .streaming,
};

fn stderrWriter() *std.Io.Writer {
    if (is_windows) stderr_file_writer.file = std.fs.File.stderr();
    return &stderr_file_writer.interface;
}

/// Result of a cache lookup operation
pub const CacheResult = union(enum) {
    hit: struct {
        module_env: *ModuleEnv,
        error_count: u32,
        warning_count: u32,
        /// The backing buffer that contains the deserialized ModuleEnv data.
        /// IMPORTANT: This must be kept alive for the lifetime of module_env,
        /// as the ModuleEnv's internal pointers reference this memory.
        cache_data: CacheModule.CacheData,
    },
    miss: struct {
        key: [32]u8,
    },
    not_enabled,

    /// Free the cache data backing buffer (only for hit results)
    pub fn deinit(self: *CacheResult, allocator: Allocator) void {
        switch (self.*) {
            .hit => |*h| h.cache_data.deinit(allocator),
            .miss, .not_enabled => {},
        }
    }
};

/// Information about an import for metadata cache
pub const ImportInfo = struct {
    /// Package qualifier (e.g., "pf" for "pf.Stdout", empty for local imports)
    package: []const u8,
    /// Module name (e.g., "Stdout" or "Helper")
    module: []const u8,
    /// Source hash of the dependency at the time of caching
    /// Used to verify the dependency hasn't changed
    source_hash: [32]u8,

    pub fn deinit(self: *ImportInfo, allocator: Allocator) void {
        if (self.package.len > 0) allocator.free(self.package);
        if (self.module.len > 0) allocator.free(self.module);
    }
};

/// Metadata for fast path cache lookup: source_hash → {imports, full_cache_key}
pub const CacheMetadata = struct {
    /// List of imports this module depends on
    imports: []ImportInfo,
    /// The full cache key to load the ModuleEnv
    full_cache_key: [32]u8,
    /// Error count from last compilation
    error_count: u32,
    /// Warning count from last compilation
    warning_count: u32,

    pub fn deinit(self: *CacheMetadata, allocator: Allocator) void {
        for (self.imports) |*imp| {
            imp.deinit(allocator);
        }
        allocator.free(self.imports);
    }
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
        _ = std.fmt.bufPrint(&subdir_buf, "{x}", .{cache_key[0..1]}) catch unreachable;
        const subdir = subdir_buf[0..];

        var filename_buf: [62]u8 = undefined;
        _ = std.fmt.bufPrint(&filename_buf, "{x}", .{cache_key[1..32]}) catch unreachable;
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
        _ = std.fmt.bufPrint(&subdir_buf, "{x}", .{cache_key[0..1]}) catch unreachable;
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

        const stderr = stderrWriter();
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

        return CacheResult{
            .hit = .{
                .module_env = module_env,
                .error_count = cache.header.error_count,
                .warning_count = cache.header.warning_count,
                .cache_data = mapped_cache, // Transfer ownership - keeps buffer alive
            },
        };
    }

    /// Compute source-only hash (without compiler version) for metadata lookups.
    /// This allows checking metadata before we know if we need to compile.
    pub fn computeSourceHash(source: []const u8) [32]u8 {
        var hasher = std.crypto.hash.Blake3.init(.{});
        hasher.update(std.mem.asBytes(&source.len));
        hasher.update(source);
        var hash: [32]u8 = undefined;
        hasher.final(&hash);
        return hash;
    }

    /// Get the metadata file path for a given source hash.
    fn getMetadataFilePath(self: *Self, source_hash: [32]u8) ![]u8 {
        const entries_dir = try self.config.getCacheEntriesDir(self.allocator);
        defer self.allocator.free(entries_dir);

        // Use same subdirectory structure as cache entries
        var subdir_buf: [2]u8 = undefined;
        _ = std.fmt.bufPrint(&subdir_buf, "{x}", .{source_hash[0..1]}) catch unreachable;
        const subdir = subdir_buf[0..];

        var filename_buf: [67]u8 = undefined; // 62 chars for hash + 5 for ".meta"
        _ = std.fmt.bufPrint(&filename_buf, "{x}.meta", .{source_hash[1..32]}) catch unreachable;
        const filename = filename_buf[0..67];

        const cache_subdir = try std.fs.path.join(self.allocator, &[_][]const u8{ entries_dir, subdir });
        defer self.allocator.free(cache_subdir);

        return std.fs.path.join(self.allocator, &[_][]const u8{ cache_subdir, filename });
    }

    /// Look up metadata by source hash (for fast path check).
    /// Returns null if metadata doesn't exist or cache is disabled.
    pub fn getMetadata(self: *Self, source_hash: [32]u8) ?CacheMetadata {
        if (!self.config.enabled) {
            return null;
        }

        const meta_path = self.getMetadataFilePath(source_hash) catch return null;
        defer self.allocator.free(meta_path);

        // Check if metadata file exists
        const exists = self.filesystem.fileExists(meta_path) catch false;
        if (!exists) {
            return null;
        }

        // Read metadata file
        const data = self.filesystem.readFile(meta_path, self.allocator) catch return null;
        defer self.allocator.free(data);

        // Parse metadata
        return self.parseMetadata(data) catch null;
    }

    /// Parse metadata from binary format.
    /// Format: [4 bytes: import_count][4 bytes: error_count][4 bytes: warning_count][32 bytes: full_cache_key]
    ///         [for each import: [4 bytes: pkg_len][pkg_bytes][4 bytes: mod_len][mod_bytes]]
    fn parseMetadata(self: *Self, data: []const u8) !CacheMetadata {
        if (data.len < 44) return error.InvalidMetadata; // Minimum: 4+4+4+32 bytes header

        var offset: usize = 0;

        // Read import count
        const import_count = std.mem.readInt(u32, data[offset..][0..4], .little);
        offset += 4;

        // Read error and warning counts
        const error_count = std.mem.readInt(u32, data[offset..][0..4], .little);
        offset += 4;
        const warning_count = std.mem.readInt(u32, data[offset..][0..4], .little);
        offset += 4;

        // Read full cache key
        var full_cache_key: [32]u8 = undefined;
        @memcpy(&full_cache_key, data[offset..][0..32]);
        offset += 32;

        // Allocate imports array
        const imports = try self.allocator.alloc(ImportInfo, import_count);
        errdefer self.allocator.free(imports);

        var i: u32 = 0;
        while (i < import_count) : (i += 1) {
            if (offset + 4 > data.len) {
                // Free already allocated imports
                for (imports[0..i]) |*imp| {
                    imp.deinit(self.allocator);
                }
                self.allocator.free(imports);
                return error.InvalidMetadata;
            }

            // Read package length
            const pkg_len = std.mem.readInt(u32, data[offset..][0..4], .little);
            offset += 4;

            if (offset + pkg_len > data.len) {
                for (imports[0..i]) |*imp| imp.deinit(self.allocator);
                self.allocator.free(imports);
                return error.InvalidMetadata;
            }

            // Read package name
            const pkg = if (pkg_len > 0)
                try self.allocator.dupe(u8, data[offset..][0..pkg_len])
            else
                "";
            offset += pkg_len;

            if (offset + 4 > data.len) {
                if (pkg.len > 0) self.allocator.free(pkg);
                for (imports[0..i]) |*imp| imp.deinit(self.allocator);
                self.allocator.free(imports);
                return error.InvalidMetadata;
            }

            // Read module length
            const mod_len = std.mem.readInt(u32, data[offset..][0..4], .little);
            offset += 4;

            if (offset + mod_len > data.len) {
                if (pkg.len > 0) self.allocator.free(pkg);
                for (imports[0..i]) |*imp| imp.deinit(self.allocator);
                self.allocator.free(imports);
                return error.InvalidMetadata;
            }

            // Read module name
            const mod = if (mod_len > 0)
                try self.allocator.dupe(u8, data[offset..][0..mod_len])
            else
                "";
            offset += mod_len;

            // Read source hash
            if (offset + 32 > data.len) {
                if (mod.len > 0) self.allocator.free(mod);
                if (pkg.len > 0) self.allocator.free(pkg);
                for (imports[0..i]) |*imp| imp.deinit(self.allocator);
                self.allocator.free(imports);
                return error.InvalidMetadata;
            }
            var source_hash: [32]u8 = undefined;
            @memcpy(&source_hash, data[offset..][0..32]);
            offset += 32;

            imports[i] = .{
                .package = pkg,
                .module = mod,
                .source_hash = source_hash,
            };
        }

        return CacheMetadata{
            .imports = imports,
            .full_cache_key = full_cache_key,
            .error_count = error_count,
            .warning_count = warning_count,
        };
    }

    /// Store metadata after successful compilation.
    pub fn storeMetadata(
        self: *Self,
        source_hash: [32]u8,
        full_cache_key: [32]u8,
        imports: []const ImportInfo,
        error_count: u32,
        warning_count: u32,
    ) !void {
        if (!self.config.enabled) {
            return;
        }

        // Ensure cache subdirectory exists
        self.ensureCacheSubdir(source_hash) catch |err| {
            if (self.config.verbose) {
                std.log.debug("Failed to create metadata cache subdirectory: {}", .{err});
            }
            return;
        };

        // Calculate total size needed
        // Header: 4 (import_count) + 4 (error_count) + 4 (warning_count) + 32 (full_cache_key) = 44
        // Per import: 4 (pkg_len) + pkg_len + 4 (mod_len) + mod_len + 32 (source_hash)
        var total_size: usize = 44;
        for (imports) |imp| {
            total_size += 8 + imp.package.len + imp.module.len + 32;
        }

        // Allocate buffer
        const buffer = self.allocator.alloc(u8, total_size) catch return;
        defer self.allocator.free(buffer);

        var offset: usize = 0;

        // Write import count
        std.mem.writeInt(u32, buffer[offset..][0..4], @intCast(imports.len), .little);
        offset += 4;

        // Write error and warning counts
        std.mem.writeInt(u32, buffer[offset..][0..4], error_count, .little);
        offset += 4;
        std.mem.writeInt(u32, buffer[offset..][0..4], warning_count, .little);
        offset += 4;

        // Write full cache key
        @memcpy(buffer[offset..][0..32], &full_cache_key);
        offset += 32;

        // Write imports
        for (imports) |imp| {
            // Write package length and data
            std.mem.writeInt(u32, buffer[offset..][0..4], @intCast(imp.package.len), .little);
            offset += 4;
            if (imp.package.len > 0) {
                @memcpy(buffer[offset..][0..imp.package.len], imp.package);
                offset += imp.package.len;
            }

            // Write module length and data
            std.mem.writeInt(u32, buffer[offset..][0..4], @intCast(imp.module.len), .little);
            offset += 4;
            if (imp.module.len > 0) {
                @memcpy(buffer[offset..][0..imp.module.len], imp.module);
                offset += imp.module.len;
            }

            // Write source hash of the dependency
            @memcpy(buffer[offset..][0..32], &imp.source_hash);
            offset += 32;
        }

        // Get metadata file path
        const meta_path = self.getMetadataFilePath(source_hash) catch return;
        defer self.allocator.free(meta_path);

        // Write to file atomically
        const temp_path = std.fmt.allocPrint(self.allocator, "{s}.tmp", .{meta_path}) catch return;
        defer self.allocator.free(temp_path);

        self.filesystem.writeFile(temp_path, buffer) catch return;
        self.filesystem.rename(temp_path, meta_path) catch return;
    }

    /// Load from cache using a pre-computed cache key (for fast path).
    /// This bypasses source hash computation since we already know the key from metadata.
    pub fn loadFromCacheByKey(
        self: *Self,
        cache_key: [32]u8,
        source: []const u8,
        module_name: []const u8,
    ) CacheResult {
        if (!self.config.enabled) {
            return .not_enabled;
        }

        const cache_path = self.getCacheFilePath(cache_key) catch {
            return CacheResult{ .miss = .{ .key = cache_key } };
        };
        defer self.allocator.free(cache_path);

        // Check if cache file exists
        const exists = self.filesystem.fileExists(cache_path) catch false;
        if (!exists) {
            self.stats.recordMiss();
            return CacheResult{ .miss = .{ .key = cache_key } };
        }

        // Read cache data
        var mapped_cache = CacheModule.readFromFileMapped(self.allocator, cache_path, self.filesystem) catch {
            self.stats.recordMiss();
            return CacheResult{ .miss = .{ .key = cache_key } };
        };
        errdefer mapped_cache.deinit(self.allocator);

        // Restore from cache
        const result = self.restoreFromCache(mapped_cache, source, module_name) catch {
            self.stats.recordInvalidation();
            mapped_cache.deinit(self.allocator);
            return CacheResult{ .miss = .{ .key = cache_key } };
        };

        self.stats.recordHit(mapped_cache.data().len);
        // Transfer ownership of cache_data to result - do NOT deinit here
        return result;
    }
};
