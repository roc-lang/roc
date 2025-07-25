
const std = @import("std");
const base = @import("base");
const reporting = @import("../reporting.zig");
const Filesystem = @import("../fs/Filesystem.zig");
const cache_mod = @import("mod.zig");
const Cache = cache_mod.CacheModule;
const CacheConfig = cache_mod.CacheConfig;
const CacheStats = cache_mod.CacheStats;
const CacheReporting = @import("CacheReporting.zig");
const SERIALIZATION_ALIGNMENT = 16;
const coordinate_simple = @import("../coordinate_simple.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = base.ModuleEnv;

/// Result of a cache lookup operation
pub const CacheResult = union(enum) {
    hit: coordinate_simple.ProcessResult,
    miss: struct {
        key: [32]u8,
    },
    not_enabled,
};

/// Manages cache operations for compiled Roc modules
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
        const cache_data = Cache.readFromFileMapped(self.allocator, cache_path, self.filesystem) catch |err| {
            if (self.config.verbose) {
                std.log.debug("Failed to read cache file {s}: {}", .{ cache_path, err });
            }
            self.stats.recordMiss();
            return CacheResult{ .miss = .{
                .key = cache_key,
            } };
        };
        defer cache_data.deinit(self.allocator);

        // Validate and restore from cache
        // restoreFromCache takes ownership of content
        const restore_result = self.restoreFromCache(
            cache_data.data(),
            source,
        ) catch |err| {
            if (self.config.verbose) {
                std.log.debug("Failed to restore from cache {s}: {}", .{ cache_path, err });
            }
            self.stats.recordInvalidation();
            return CacheResult{ .miss = .{
                .key = cache_key,
            } };
        };

        self.stats.recordHit(cache_data.data().len);
        return CacheResult{ .hit = restore_result.result };
    }

    /// Store a cache entry.
    ///
    /// Serializes the ProcessResult and stores it in the cache using BLAKE3-based
    /// filenames with subdirectory splitting.
    pub fn store(self: *Self, cache_key: [32]u8, process_result: *const coordinate_simple.ProcessResult) !void {
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

        // Create cache from the compile.ModuleEnv
        // Since compile.ModuleEnv contains all the base.ModuleEnv fields, we cast it
        // Note: This works because compile.ModuleEnv extends base.ModuleEnv with the same initial fields
        const base_env_ptr = @as(*const base.ModuleEnv, @ptrCast(process_result.cir));
        const cache_data = Cache.create(
            self.allocator, 
            base_env_ptr,
            process_result.cir,
            process_result.error_count,
            process_result.warning_count
        ) catch |err| {
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
        var tmp_path = try self.allocator.alloc(u8, cache_path.len + 4);
        defer self.allocator.free(tmp_path);
        @memcpy(tmp_path[0..cache_path.len], cache_path);
        @memcpy(tmp_path[cache_path.len..], ".tmp");

        // Write the cache data
        const file = std.fs.createFileAbsolute(tmp_path, .{}) catch |err| {
            if (self.config.verbose) {
                std.log.debug("Failed to create cache file: {}", .{err});
            }
            self.stats.recordStoreFailure();
            return;
        };
        defer file.close();

        file.writeAll(cache_data) catch |err| {
            if (self.config.verbose) {
                std.log.debug("Failed to write cache data: {}", .{err});
            }
            self.stats.recordStoreFailure();
            // Clean up temp file on error
            std.fs.deleteFileAbsolute(tmp_path) catch {};
            return;
        };

        // Atomically rename temp file to final location
        std.fs.renameAbsolute(tmp_path, cache_path) catch |err| {
            if (self.config.verbose) {
                std.log.debug("Failed to rename cache file: {}", .{err});
            }
            self.stats.recordStoreFailure();
            // Clean up temp file on error
            std.fs.deleteFileAbsolute(tmp_path) catch {};
            return;
        };

        self.stats.recordStore(cache_data.len);
    }

    /// Generate a BLAKE3-based cache key from source and compiler version.
    fn generateCacheKey(source: []const u8, compiler_version: []const u8) [32]u8 {
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
    fn getCacheFilePath(self: *Self, cache_key: [32]u8) ![]u8 {
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
        cache_data: []align(SERIALIZATION_ALIGNMENT) const u8,
        source: []const u8,
    ) !struct {
        result: coordinate_simple.ProcessResult,
        error_count: u32,
        warning_count: u32,
    } {
        // Load cache using existing Cache functionality
        var cache = Cache.fromMappedMemory(cache_data) catch return error.InvalidCache;

        // Restore the data
        // Use a default module name when restoring from cache
        // since we don't have access to the original source path
        const module_name = "cached_module";
        // Transfer ownership of source to the restored ModuleEnv
        const restored = cache.restore(self.allocator, module_name, source) catch return error.RestoreError;

        // Reports are not cached - they need to be recomputed if needed
        // Users can use --no-cache to see diagnostic reports
        const reports = try self.allocator.alloc(reporting.Report, 0);

        // Allocate and create compile.ModuleEnv from restored base.ModuleEnv
        const compile_mod = @import("../compile/ModuleEnv.zig");
        const cir = try self.allocator.create(compile_mod);
        
        // Copy base fields from restored.module_env to cir
        // This works because compile.ModuleEnv has the same initial fields as base.ModuleEnv
        cir.gpa = restored.module_env.gpa;
        cir.idents = restored.module_env.idents;
        cir.ident_ids_for_slicing = restored.module_env.ident_ids_for_slicing;
        cir.strings = restored.module_env.strings;
        cir.types = restored.module_env.types;
        cir.exposed_items = restored.module_env.exposed_items;
        cir.line_starts = restored.module_env.line_starts;
        cir.source = restored.module_env.source;
        
        // Initialize CIR-specific fields with empty values
        cir.all_defs = .{ .span = .{ .start = 0, .len = 0 } };
        cir.all_statements = .{ .span = .{ .start = 0, .len = 0 } };
        cir.external_decls = try compile_mod.ExternalDecl.SafeList.initCapacity(self.allocator, 0);
        cir.imports = compile_mod.Import.Store.init();
        cir.module_name = "";
        cir.diagnostics = compile_mod.Diagnostic.Span{ .span = base.DataSpan{ .start = 0, .len = 0 } };
        cir.store = try compile_mod.NodeStore.initCapacity(self.allocator, 0);

        // Store is now inside module_env.store
        // (removed unused variable)

        // Create ProcessResult with proper ownership
        const process_result = coordinate_simple.ProcessResult{
            .cir = cir,
            .source = source,
            .own_source = true,
            .reports = reports,
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
    const content = "module [test]\n\ntest = 42";
    const compiler_version = "roc-zig-0.11.0-debug";

    const key1 = CacheManager.generateCacheKey(compiler_version, content);
    const key2 = CacheManager.generateCacheKey(compiler_version, content);

    // Same input should produce same key
    try testing.expectEqualSlices(u8, &key1, &key2);

    // Should be 32 bytes
    try testing.expectEqual(@as(usize, 32), key1.len);
}

test "CacheManager getCacheFilePath with subdirectory splitting" {
    const allocator = testing.allocator;
    const config = CacheConfig{};
    const filesystem = Filesystem.testing();

    var manager = CacheManager.init(allocator, config, filesystem);

    const cache_key = [_]u8{
        0xab, 0xcd, 0xef, 0x12, 0x34, 0x56, 0x78, 0x90,
        0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88,
        0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00,
        0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
    };
    const cache_path = try manager.getCacheFilePath(cache_key);
    defer allocator.free(cache_path);

    // Should contain subdirectory split
    try testing.expect(std.mem.containsAtLeast(u8, cache_path, 1, "ab"));
    try testing.expect(std.mem.containsAtLeast(u8, cache_path, 1, "cdef123456789"));
}

test "CacheManager loadFromCache miss" {
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

    const source = "module [test]\n\ntest = 42";

    const result = manager.loadFromCache(source, "roc-zig-0.11.0-debug");
    switch (result) {
        .miss => |_| {
            try testing.expect(manager.stats.misses == 1);
        },
        else => return error.TestUnexpectedResult,
    }
}

test "CacheManager disabled" {
    const allocator = testing.allocator;
    const config = CacheConfig{ .enabled = false };
    const filesystem = Filesystem.testing();

    var manager = CacheManager.init(allocator, config, filesystem);

    const source = "module [test]\n\ntest = 42";

    const result = manager.loadFromCache(source, "roc-zig-0.11.0-debug");
    switch (result) {
        .not_enabled => |_| {},
        else => return error.TestUnexpectedResult,
    }
    try testing.expect(manager.stats.getTotalOps() == 0); // No stats recorded when disabled
}
