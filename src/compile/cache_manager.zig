//! Cache manager for opaque compiler cache entries.
//!
//! Checked artifacts use the explicit target-independent key model in
//! `cache_key.zig`. This manager deliberately does not know how to serialize or
//! restore `ModuleEnv`; callers may only store and load raw bytes for artifacts
//! that already have a published cache key.

const std = @import("std");
const ctx_mod = @import("ctx");

const CacheReporting = @import("cache_reporting.zig").CacheReporting;
pub const CacheModule = @import("cache_module.zig").CacheModule;
const Allocator = std.mem.Allocator;
const CoreCtx = ctx_mod.CoreCtx;
const CacheStats = @import("cache_config.zig").CacheStats;
const CacheConfig = @import("cache_config.zig").CacheConfig;

/// Public `CacheManager` declaration.
pub const CacheManager = struct {
    config: CacheConfig,
    roc_ctx: CoreCtx = undefined,
    allocator: Allocator,
    stats: CacheStats,

    const Self = @This();

    fn verboseLog(self: *Self, comptime fmt: []const u8, args: anytype) void {
        if (!self.config.verbose) return;
        var buf: [1024]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, fmt, args) catch return;
        self.roc_ctx.writeStderr(msg) catch {};
    }

    pub fn init(allocator: Allocator, config: CacheConfig, roc_ctx: CoreCtx) Self {
        var cfg = config;
        cfg.roc_ctx = roc_ctx;
        return .{
            .config = cfg,
            .roc_ctx = roc_ctx,
            .allocator = allocator,
            .stats = CacheStats{},
        };
    }

    pub fn getCacheFilePath(self: *Self, cache_key: [32]u8) (Allocator.Error || error{NoHomeDirectory})![]u8 {
        const entries_dir = try self.config.getCheckedArtifactCacheDir(self.allocator);
        defer self.allocator.free(entries_dir);
        return self.computeCacheFilePath(cache_key, entries_dir);
    }

    pub fn computeCacheFilePath(self: *Self, cache_key: [32]u8, entries_dir: []const u8) Allocator.Error![]u8 {
        var subdir_buf: [2]u8 = undefined;
        _ = std.fmt.bufPrint(&subdir_buf, "{x}", .{cache_key[0..1]}) catch unreachable;
        const subdir = subdir_buf[0..];

        var filename_buf: [62]u8 = undefined;
        _ = std.fmt.bufPrint(&filename_buf, "{x}", .{cache_key[1..32]}) catch unreachable;
        const filename = filename_buf[0..];

        const cache_subdir = try std.fs.path.join(self.allocator, &.{ entries_dir, subdir });
        defer self.allocator.free(cache_subdir);

        return std.fs.path.join(self.allocator, &.{ cache_subdir, filename });
    }

    pub fn ensureCacheSubdirIn(self: *Self, cache_key: [32]u8, entries_dir: []const u8) (Allocator.Error || error{ AccessDenied, IoError })!void {
        var subdir_buf: [2]u8 = undefined;
        _ = std.fmt.bufPrint(&subdir_buf, "{x}", .{cache_key[0..1]}) catch unreachable;
        const subdir = subdir_buf[0..];
        const full_subdir = try std.fs.path.join(self.allocator, &.{ entries_dir, subdir });
        defer self.allocator.free(full_subdir);

        try self.roc_ctx.makePath(full_subdir);
    }

    pub fn storeRawBytes(self: *Self, cache_key: [32]u8, data: []const u8, entries_dir: []const u8) void {
        if (!self.config.enabled) return;

        self.ensureCacheSubdirIn(cache_key, entries_dir) catch |err| {
            self.verboseLog("Failed to create cache subdirectory: {}\n", .{err});
            self.stats.recordStoreFailure();
            return;
        };

        const cache_path = self.computeCacheFilePath(cache_key, entries_dir) catch {
            self.stats.recordStoreFailure();
            return;
        };
        defer self.allocator.free(cache_path);

        const temp_path = std.fmt.allocPrint(self.allocator, "{s}.tmp", .{cache_path}) catch {
            self.stats.recordStoreFailure();
            return;
        };
        defer self.allocator.free(temp_path);

        self.roc_ctx.writeFile(temp_path, data) catch |err| {
            self.verboseLog("Failed to write cache temp file {s}: {}\n", .{ temp_path, err });
            self.stats.recordStoreFailure();
            return;
        };

        self.roc_ctx.rename(temp_path, cache_path) catch |err| {
            self.verboseLog("Failed to rename cache file {s} -> {s}: {}\n", .{ temp_path, cache_path, err });
            self.stats.recordStoreFailure();
            return;
        };

        self.stats.recordStore(data.len);
    }

    pub fn loadRawBytes(self: *Self, cache_key: [32]u8, entries_dir: []const u8) ?[]const u8 {
        if (!self.config.enabled) return null;

        const cache_path = self.computeCacheFilePath(cache_key, entries_dir) catch {
            self.stats.recordMiss();
            return null;
        };
        defer self.allocator.free(cache_path);

        if (!self.roc_ctx.fileExists(cache_path)) {
            self.stats.recordMiss();
            return null;
        }

        const data = self.roc_ctx.readFile(cache_path, self.allocator) catch |err| {
            self.verboseLog("Failed to read cache file {s}: {}\n", .{ cache_path, err });
            self.stats.recordMiss();
            return null;
        };

        self.stats.recordHit(data.len);
        return data;
    }

    pub fn getStats(self: *const Self) CacheStats {
        return self.stats;
    }

    pub fn printStats(self: *const Self, allocator: Allocator) void {
        if (!self.config.verbose) return;

        var buf: [8192]u8 = undefined;
        var fbs = std.io.fixedBufferStream(&buf);
        CacheReporting.renderCacheStatsToTerminal(allocator, self.stats, fbs.writer()) catch return;
        self.roc_ctx.writeStderr(fbs.getWritten()) catch {};
    }
};
