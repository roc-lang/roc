//! Cache manager for opaque compiler cache entries.
//!
//! Checked artifacts use the explicit target-independent key model in
//! `cache_key.zig`. This manager deliberately does not know how to serialize or
//! restore `ModuleEnv`; callers may only store and load raw bytes for artifacts
//! that already have a published cache key.

const std = @import("std");
const ctx_mod = @import("ctx");
const threading = @import("threading.zig");

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
    store_failure_warning_emitted: bool = false,
    /// Guards the manager's own mutable state (`stats` and the one-shot store
    /// failure warning). The canonicalized-module cache is read and written from
    /// compiler worker threads, so every recording path goes through here while
    /// the file work itself stays outside the lock.
    stats_mutex: threading.Mutex = .init,

    const Self = @This();

    /// Which cache an operation belongs to; each has its own counters.
    pub const Kind = CacheStats.Kind;

    fn verboseLog(self: *Self, comptime fmt: []const u8, args: anytype) void {
        if (!self.config.verbose) return;
        var buf: [1024]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, fmt, args) catch return;
        self.stats_mutex.lockUncancelable(self.roc_ctx.std_io);
        defer self.stats_mutex.unlock(self.roc_ctx.std_io);
        self.roc_ctx.writeStderr(msg) catch {};
    }

    fn warnStoreFailureOnce(self: *Self) void {
        self.stats_mutex.lockUncancelable(self.roc_ctx.std_io);
        defer self.stats_mutex.unlock(self.roc_ctx.std_io);
        if (self.store_failure_warning_emitted) return;
        self.store_failure_warning_emitted = true;
        self.roc_ctx.writeStderr(
            "warning: Roc cache writes are failing; compilation will continue without updating the cache. Run with --verbose for details.\n",
        ) catch {};
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

    pub fn recordStoreFailure(self: *Self) void {
        self.recordStoreFailureFor(.checked);
    }

    /// Record one cache's failed store.
    pub fn recordStoreFailureFor(self: *Self, kind: Kind) void {
        {
            self.stats_mutex.lockUncancelable(self.roc_ctx.std_io);
            defer self.stats_mutex.unlock(self.roc_ctx.std_io);
            self.stats.recordStoreFailureFor(kind);
        }
        self.warnStoreFailureOnce();
    }

    /// Record one cache's hit.
    pub fn recordHitFor(self: *Self, kind: Kind, bytes_read: u64) void {
        self.stats_mutex.lockUncancelable(self.roc_ctx.std_io);
        defer self.stats_mutex.unlock(self.roc_ctx.std_io);
        self.stats.recordHitFor(kind, bytes_read);
    }

    /// Record one cache's miss.
    pub fn recordMissFor(self: *Self, kind: Kind) void {
        self.stats_mutex.lockUncancelable(self.roc_ctx.std_io);
        defer self.stats_mutex.unlock(self.roc_ctx.std_io);
        self.stats.recordMissFor(kind);
    }

    /// Record one cache's invalidation.
    pub fn recordInvalidationFor(self: *Self, kind: Kind) void {
        self.stats_mutex.lockUncancelable(self.roc_ctx.std_io);
        defer self.stats_mutex.unlock(self.roc_ctx.std_io);
        self.stats.recordInvalidationFor(kind);
    }

    /// Record one cache's successful store.
    pub fn recordStoreFor(self: *Self, kind: Kind, bytes_written: u64) void {
        self.stats_mutex.lockUncancelable(self.roc_ctx.std_io);
        defer self.stats_mutex.unlock(self.roc_ctx.std_io);
        self.stats.recordStoreFor(kind, bytes_written);
    }

    pub fn getCacheFilePath(self: *Self, cache_key: [32]u8) (Allocator.Error || error{NoHomeDirectory})![]u8 {
        const entries_dir = try self.config.getCheckedArtifactCacheDir(self.allocator);
        defer self.allocator.free(entries_dir);
        return self.computeCacheFilePath(cache_key, entries_dir);
    }

    pub fn computeCacheFilePath(self: *Self, cache_key: [32]u8, entries_dir: []const u8) Allocator.Error![]u8 {
        return computeCacheFilePathIn(self.allocator, cache_key, entries_dir);
    }

    /// Build a cache entry's path using a caller-supplied allocator, so a worker
    /// thread never allocates from the manager's own allocator.
    pub fn computeCacheFilePathIn(allocator: Allocator, cache_key: [32]u8, entries_dir: []const u8) Allocator.Error![]u8 {
        var subdir_buf: [2]u8 = undefined;
        _ = std.fmt.bufPrint(&subdir_buf, "{x}", .{cache_key[0..1]}) catch unreachable;
        const subdir = subdir_buf[0..];

        var filename_buf: [62]u8 = undefined;
        _ = std.fmt.bufPrint(&filename_buf, "{x}", .{cache_key[1..32]}) catch unreachable;
        const filename = filename_buf[0..];

        const cache_subdir = try std.fs.path.join(allocator, &.{ entries_dir, subdir });
        defer allocator.free(cache_subdir);

        return std.fs.path.join(allocator, &.{ cache_subdir, filename });
    }

    pub fn ensureCacheSubdirIn(self: *Self, cache_key: [32]u8, entries_dir: []const u8) (Allocator.Error || error{ AccessDenied, IoError })!void {
        return self.ensureCacheSubdirWith(self.allocator, cache_key, entries_dir);
    }

    /// Create a cache entry's subdirectory using a caller-supplied allocator.
    pub fn ensureCacheSubdirWith(
        self: *Self,
        allocator: Allocator,
        cache_key: [32]u8,
        entries_dir: []const u8,
    ) (Allocator.Error || error{ AccessDenied, IoError })!void {
        var subdir_buf: [2]u8 = undefined;
        _ = std.fmt.bufPrint(&subdir_buf, "{x}", .{cache_key[0..1]}) catch unreachable;
        const subdir = subdir_buf[0..];
        const full_subdir = try std.fs.path.join(allocator, &.{ entries_dir, subdir });
        defer allocator.free(full_subdir);

        try self.roc_ctx.makePath(full_subdir);
    }

    pub fn storeRawBytes(self: *Self, cache_key: [32]u8, data: []const u8, entries_dir: []const u8) void {
        self.storeRawBytesIn(self.allocator, .checked, cache_key, data, entries_dir);
    }

    /// Store one cache entry, allocating path scratch from `allocator` and
    /// counting the operation against `kind`'s counters.
    pub fn storeRawBytesIn(
        self: *Self,
        allocator: Allocator,
        kind: Kind,
        cache_key: [32]u8,
        data: []const u8,
        entries_dir: []const u8,
    ) void {
        if (!self.config.enabled) return;

        self.ensureCacheSubdirWith(allocator, cache_key, entries_dir) catch |err| {
            self.verboseLog("Failed to create cache subdirectory: {}\n", .{err});
            self.recordStoreFailureFor(kind);
            return;
        };

        const cache_path = computeCacheFilePathIn(allocator, cache_key, entries_dir) catch {
            self.recordStoreFailureFor(kind);
            return;
        };
        defer allocator.free(cache_path);

        // Concurrent compilers can publish the same key. Each writer needs its
        // own staging file so one rename cannot remove another writer's input.
        var suffix: [16]u8 = undefined;
        self.roc_ctx.std_io.random(&suffix);
        const temp_path = std.fmt.allocPrint(allocator, "{s}.{s}.tmp", .{ cache_path, std.fmt.bytesToHex(suffix, .lower) }) catch {
            self.recordStoreFailureFor(kind);
            return;
        };
        defer allocator.free(temp_path);

        self.roc_ctx.writeFile(temp_path, data) catch |err| {
            self.roc_ctx.deleteFile(temp_path) catch {};
            self.verboseLog("Failed to write cache temp file {s}: {}\n", .{ temp_path, err });
            self.recordStoreFailureFor(kind);
            return;
        };

        self.roc_ctx.rename(temp_path, cache_path) catch |err| {
            self.roc_ctx.deleteFile(temp_path) catch {};
            self.verboseLog("Failed to rename cache file {s} -> {s}: {}\n", .{ temp_path, cache_path, err });
            self.recordStoreFailureFor(kind);
            return;
        };

        self.recordStoreFor(kind, data.len);
    }

    pub fn loadRawBytes(self: *Self, cache_key: [32]u8, entries_dir: []const u8) ?[]const u8 {
        if (!self.config.enabled) return null;

        const cache_path = self.computeCacheFilePath(cache_key, entries_dir) catch {
            self.recordMissFor(.checked);
            return null;
        };
        defer self.allocator.free(cache_path);

        if (!self.roc_ctx.fileExists(cache_path)) {
            self.recordMissFor(.checked);
            return null;
        }

        const data = self.roc_ctx.readFile(cache_path, self.allocator) catch |err| {
            self.verboseLog("Failed to read cache file {s}: {}\n", .{ cache_path, err });
            self.recordMissFor(.checked);
            return null;
        };

        self.recordHitFor(.checked, data.len);
        return data;
    }

    /// Load a cache entry as a `CacheModule.CacheData`, memory-mapping the file on
    /// POSIX targets and reading it onto the heap elsewhere. The returned value must
    /// be released with `CacheData.deinit(self.allocator)`, which unmaps a mapped
    /// entry and frees a heap-read entry. The caller must keep the entry alive for
    /// as long as it reads from the returned bytes.
    pub fn loadRawBytesMapped(self: *Self, cache_key: [32]u8, entries_dir: []const u8) ?CacheModule.CacheData {
        return self.loadRawBytesMappedIn(self.allocator, .checked, cache_key, entries_dir);
    }

    /// Load one cache entry, allocating from `allocator` (which must also
    /// release the returned `CacheData`) and counting the operation against
    /// `kind`'s counters.
    pub fn loadRawBytesMappedIn(
        self: *Self,
        allocator: Allocator,
        kind: Kind,
        cache_key: [32]u8,
        entries_dir: []const u8,
    ) ?CacheModule.CacheData {
        if (!self.config.enabled) return null;

        const cache_path = computeCacheFilePathIn(allocator, cache_key, entries_dir) catch {
            self.recordMissFor(kind);
            return null;
        };
        defer allocator.free(cache_path);

        if (!self.roc_ctx.fileExists(cache_path)) {
            self.recordMissFor(kind);
            return null;
        }

        // Prefer a copy-on-write mapping of the file. A `roc_ctx` that cannot
        // map files (a virtual filesystem, a target without `mmap`) yields
        // `null` and reads onto the heap instead.
        if (CacheModule.tryMapCacheFile(self.roc_ctx, cache_path)) |mapped| {
            self.recordHitFor(kind, mapped.data().len);
            return mapped;
        }

        const data = self.roc_ctx.readFile(cache_path, allocator) catch |err| {
            self.verboseLog("Failed to read cache file {s}: {}\n", .{ cache_path, err });
            self.recordMissFor(kind);
            return null;
        };
        defer allocator.free(data);

        const buffer = allocator.alignedAlloc(u8, CacheModule.SERIALIZATION_ALIGNMENT, data.len) catch {
            self.recordMissFor(kind);
            return null;
        };
        @memcpy(buffer, data);

        self.recordHitFor(kind, buffer.len);
        return CacheModule.CacheData{ .allocated = buffer };
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
