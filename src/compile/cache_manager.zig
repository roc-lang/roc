//! Cache manager for opaque compiler cache entries.
//!
//! Checked artifacts use the explicit target-independent key model in
//! `cache_key.zig`. This manager deliberately does not know how to serialize or
//! restore `ModuleEnv`; callers may only store and load raw bytes for artifacts
//! that already have a published cache key.

const std = @import("std");
const ctx_mod = @import("ctx");
const threading = @import("threading.zig");

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

    fn verboseLog(self: *Self, allocator: Allocator, comptime fmt: []const u8, args: anytype) void {
        if (!self.config.verbose) return;
        const msg = std.fmt.allocPrint(allocator, fmt, args) catch return;
        defer allocator.free(msg);
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

    pub fn computeCacheFilePath(self: *Self, cache_key: [32]u8, entries_dir: []const u8) Allocator.Error![]u8 {
        return computeCacheFilePathIn(self.allocator, cache_key, entries_dir);
    }

    /// Build a cache entry's path using a caller-supplied allocator, so a worker
    /// thread never allocates from the manager's own allocator.
    pub fn computeCacheFilePathIn(allocator: Allocator, cache_key: [32]u8, entries_dir: []const u8) Allocator.Error![]u8 {
        const subdir_buf = std.fmt.bytesToHex(cache_key[0..1], .lower);

        const filename_buf: [cache_file_name_len]u8 = std.fmt.bytesToHex(cache_key[1..32], .lower);

        return std.fs.path.join(allocator, &.{ entries_dir, &subdir_buf, &filename_buf });
    }

    /// Hex spelling of the key bytes after the subdirectory byte.
    const cache_file_name_len = 62;

    /// The subdirectory holding the entry at `cache_path`, a path built by
    /// `computeCacheFilePathIn`. The file name has no separators, so the join
    /// put exactly one separator before it.
    fn cacheSubdirOf(cache_path: []const u8) []const u8 {
        return cache_path[0 .. cache_path.len - cache_file_name_len - 1];
    }

    pub fn storeRawBytes(self: *Self, cache_key: [32]u8, data: []const u8, entries_dir: []const u8, source_name: []const u8) void {
        self.storeRawBytesIn(self.allocator, .checked, cache_key, data, entries_dir, source_name);
    }

    /// Store one cache entry, allocating path scratch from `allocator` and
    /// counting the operation against `kind`'s counters. The source name is
    /// diagnostic context only; it does not contribute to the cache key.
    pub fn storeRawBytesIn(
        self: *Self,
        allocator: Allocator,
        kind: Kind,
        cache_key: [32]u8,
        data: []const u8,
        entries_dir: []const u8,
        source_name: []const u8,
    ) void {
        self.storeEntry(allocator, kind, cache_key, .{ .bytes = data }, data.len, entries_dir, source_name);
    }

    /// Atomically persist a known-size entry emitted incrementally by its producer.
    /// The source name is diagnostic context only, as in `storeRawBytesIn`.
    pub fn storeStream(
        self: *Self,
        cache_key: [32]u8,
        stream: CoreCtx.FileStream,
        size: usize,
        entries_dir: []const u8,
        source_name: []const u8,
    ) void {
        self.storeStreamIn(self.allocator, .checked, cache_key, stream, size, entries_dir, source_name);
    }

    /// `storeStream`, allocating path scratch from `allocator` and counting the
    /// operation against `kind`'s counters.
    pub fn storeStreamIn(
        self: *Self,
        allocator: Allocator,
        kind: Kind,
        cache_key: [32]u8,
        stream: CoreCtx.FileStream,
        size: usize,
        entries_dir: []const u8,
        source_name: []const u8,
    ) void {
        self.storeEntry(allocator, kind, cache_key, .{ .stream = stream }, size, entries_dir, source_name);
    }

    const EntrySource = union(enum) { bytes: []const u8, stream: CoreCtx.FileStream };

    fn storeEntry(
        self: *Self,
        allocator: Allocator,
        kind: Kind,
        cache_key: [32]u8,
        source: EntrySource,
        size: usize,
        entries_dir: []const u8,
        source_name: []const u8,
    ) void {
        if (!self.config.enabled) return;

        const cache_path = computeCacheFilePathIn(allocator, cache_key, entries_dir) catch |err| {
            self.verboseLog(allocator, "Failed to create {s} cache subdirectory in {s} for {s} (key {x}): {}\n", .{ @tagName(kind), entries_dir, source_name, cache_key, err });
            self.recordStoreFailureFor(kind);
            return;
        };
        defer allocator.free(cache_path);

        self.roc_ctx.makePath(cacheSubdirOf(cache_path)) catch |err| {
            self.verboseLog(allocator, "Failed to create {s} cache subdirectory in {s} for {s} (key {x}): {}\n", .{ @tagName(kind), entries_dir, source_name, cache_key, err });
            self.recordStoreFailureFor(kind);
            return;
        };

        // Concurrent compilers can publish the same key. Each writer needs its
        // own staging file so one rename cannot remove another writer's input.
        var suffix: [16]u8 = undefined;
        self.roc_ctx.std_io.random(&suffix);
        const temp_path = std.fmt.allocPrint(allocator, "{s}.{s}.tmp", .{ cache_path, std.fmt.bytesToHex(suffix, .lower) }) catch |err| {
            self.verboseLog(allocator, "Failed to allocate cache staging path for {s}: {}\n", .{ source_name, err });
            self.recordStoreFailureFor(kind);
            return;
        };
        defer allocator.free(temp_path);

        const write_result = switch (source) {
            .bytes => |data| self.roc_ctx.writeFile(temp_path, data),
            .stream => |stream| self.roc_ctx.writeFileStream(temp_path, stream),
        };
        write_result catch |err| {
            self.verboseLog(allocator, "Failed to write {s} cache temp file {s} for {s} ({d} bytes): {}\n", .{ @tagName(kind), temp_path, source_name, size, err });
            self.removeFailedTempFile(allocator, temp_path);
            self.recordStoreFailureFor(kind);
            return;
        };

        self.roc_ctx.rename(temp_path, cache_path) catch |err| {
            self.verboseLog(allocator, "Failed to rename {s} cache file {s} -> {s} for {s} ({d} bytes): {}\n", .{ @tagName(kind), temp_path, cache_path, source_name, size, err });
            self.removeFailedTempFile(allocator, temp_path);
            self.recordStoreFailureFor(kind);
            return;
        };

        self.recordStoreFor(kind, size);
    }

    /// One part of a checked cache entry, written to a staging file before the
    /// rest of the entry exists. Nothing reads it until the entry is published.
    pub const StagedPart = struct {
        /// Owned by the manager's allocator.
        path: []u8,
        len: usize,
    };

    /// Write `stream` to a staging file beside the checked entry for
    /// `cache_key`, publishing nothing. Null when the cache is off or the write
    /// failed; a failure counts as a store failure, since the entry it belonged
    /// to can no longer be written.
    pub fn stageCheckedPart(
        self: *Self,
        cache_key: [32]u8,
        stream: CoreCtx.FileStream,
        size: usize,
        entries_dir: []const u8,
        source_name: []const u8,
    ) ?StagedPart {
        if (!self.config.enabled) return null;
        const allocator = self.allocator;
        const cache_path = computeCacheFilePathIn(allocator, cache_key, entries_dir) catch {
            self.recordStoreFailureFor(.checked);
            return null;
        };
        defer allocator.free(cache_path);
        self.roc_ctx.makePath(cacheSubdirOf(cache_path)) catch |err| {
            self.verboseLog(allocator, "Failed to create checked cache subdirectory in {s} for {s} (key {x}): {}\n", .{ entries_dir, source_name, cache_key, err });
            self.recordStoreFailureFor(.checked);
            return null;
        };
        // Staged parts are named like the writer's own staging files, so cache
        // cleanup ages out one left behind by a compilation that never finished.
        var suffix: [16]u8 = undefined;
        self.roc_ctx.std_io.random(&suffix);
        const path = std.fmt.allocPrint(allocator, "{s}.{s}.part.tmp", .{ cache_path, std.fmt.bytesToHex(suffix, .lower) }) catch {
            self.recordStoreFailureFor(.checked);
            return null;
        };
        self.roc_ctx.writeFileStream(path, stream) catch |err| {
            self.verboseLog(allocator, "Failed to write checked cache part {s} for {s} ({d} bytes): {}\n", .{ path, source_name, size, err });
            self.removeFailedTempFile(allocator, path);
            allocator.free(path);
            self.recordStoreFailureFor(.checked);
            return null;
        };
        return .{ .path = path, .len = size };
    }

    /// The bytes of a staged part, owned by `allocator`. Null, counted as a
    /// store failure, when the staging file can no longer be read whole.
    pub fn readStagedPart(self: *Self, part: StagedPart, allocator: Allocator) ?[]u8 {
        const bytes = self.roc_ctx.readFile(part.path, allocator) catch |err| {
            self.verboseLog(allocator, "Failed to read checked cache part {s}: {}\n", .{ part.path, err });
            self.recordStoreFailureFor(.checked);
            return null;
        };
        if (bytes.len != part.len) {
            allocator.free(bytes);
            self.recordStoreFailureFor(.checked);
            return null;
        }
        return bytes;
    }

    /// Delete a staged part's file and release it.
    pub fn discardStagedPart(self: *Self, part: StagedPart) void {
        self.removeFailedTempFile(self.allocator, part.path);
        self.allocator.free(part.path);
    }

    fn removeFailedTempFile(self: *Self, allocator: Allocator, path: []const u8) void {
        self.roc_ctx.deleteFile(path) catch |err| switch (err) {
            error.FileNotFound => {},
            error.AccessDenied, error.IoError => self.verboseLog(allocator, "Failed to remove cache temp file {s}: {}\n", .{ path, err }),
        };
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
            self.verboseLog(self.allocator, "Failed to read cache file {s}: {}\n", .{ cache_path, err });
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
            self.verboseLog(allocator, "Failed to read cache file {s}: {}\n", .{ cache_path, err });
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

    const placement_pointer_magic = "roc-mod-latest-v1";
    const placement_pointer_len = placement_pointer_magic.len + 32;

    /// Record `key` as the most recently stored checked entry of the module
    /// filed under `placement`, replacing the previous pointer. A failure
    /// leaves no pointer and only costs a later edit its reuse.
    pub fn storePlacementPointer(self: *Self, placement: [32]u8, key: [32]u8) void {
        if (!self.config.enabled) return;
        const allocator = self.allocator;
        const dir = self.config.getCheckedPlacementDir(allocator) catch return;
        defer allocator.free(dir);
        const path = computeCacheFilePathIn(allocator, placement, dir) catch return;
        defer allocator.free(path);
        self.roc_ctx.makePath(cacheSubdirOf(path)) catch return;
        var contents: [placement_pointer_len]u8 = undefined;
        @memcpy(contents[0..placement_pointer_magic.len], placement_pointer_magic);
        @memcpy(contents[placement_pointer_magic.len..], &key);
        var suffix: [16]u8 = undefined;
        self.roc_ctx.std_io.random(&suffix);
        const temp_path = std.fmt.allocPrint(allocator, "{s}.{s}.tmp", .{ path, std.fmt.bytesToHex(suffix, .lower) }) catch return;
        defer allocator.free(temp_path);
        self.roc_ctx.writeFile(temp_path, &contents) catch {
            self.removeFailedTempFile(allocator, temp_path);
            return;
        };
        self.roc_ctx.rename(temp_path, path) catch self.removeFailedTempFile(allocator, temp_path);
    }

    /// The key `storePlacementPointer` last recorded for `placement`, or
    /// null when there is none or it is unreadable.
    pub fn loadPlacementPointer(self: *Self, placement: [32]u8) ?[32]u8 {
        if (!self.config.enabled) return null;
        const allocator = self.allocator;
        const dir = self.config.getCheckedPlacementDir(allocator) catch return null;
        defer allocator.free(dir);
        const path = computeCacheFilePathIn(allocator, placement, dir) catch return null;
        defer allocator.free(path);
        var contents: [placement_pointer_len]u8 = undefined;
        const len = self.roc_ctx.readFileInto(path, &contents) catch return null;
        if (len != placement_pointer_len or !std.mem.eql(u8, contents[0..placement_pointer_magic.len], placement_pointer_magic)) return null;
        return contents[placement_pointer_magic.len..].*;
    }

    /// Replace the literal values file of the program root module filed
    /// under `placement` with `bytes`. A failure leaves the previous file, or
    /// none, and only costs a later compilation its reuse.
    pub fn storeLiteralValues(self: *Self, placement: [32]u8, bytes: []const u8) void {
        if (!self.config.enabled) return;
        const allocator = self.allocator;
        const dir = self.config.getLiteralValuesDir(allocator) catch return;
        defer allocator.free(dir);
        const path = computeCacheFilePathIn(allocator, placement, dir) catch return;
        defer allocator.free(path);
        self.roc_ctx.makePath(cacheSubdirOf(path)) catch return;
        var suffix: [16]u8 = undefined;
        self.roc_ctx.std_io.random(&suffix);
        const temp_path = std.fmt.allocPrint(allocator, "{s}.{s}.tmp", .{ path, std.fmt.bytesToHex(suffix, .lower) }) catch return;
        defer allocator.free(temp_path);
        self.roc_ctx.writeFile(temp_path, bytes) catch {
            self.removeFailedTempFile(allocator, temp_path);
            return;
        };
        self.roc_ctx.rename(temp_path, path) catch self.removeFailedTempFile(allocator, temp_path);
    }

    /// The literal values file `storeLiteralValues` last wrote for
    /// `placement`, or null when there is none or it is unreadable. Owned.
    pub fn loadLiteralValues(self: *Self, placement: [32]u8) ?[]u8 {
        if (!self.config.enabled) return null;
        const allocator = self.allocator;
        const dir = self.config.getLiteralValuesDir(allocator) catch return null;
        defer allocator.free(dir);
        const path = computeCacheFilePathIn(allocator, placement, dir) catch return null;
        defer allocator.free(path);
        if (!self.roc_ctx.fileExists(path)) return null;
        return self.roc_ctx.readFile(path, allocator) catch null;
    }

    /// Load a checked entry without counting it as a cache lookup: reading a
    /// module's previous entry for value reuse is not a probe of its key.
    pub fn loadCheckedEntryUncounted(self: *Self, cache_key: [32]u8) ?CacheModule.CacheData {
        if (!self.config.enabled) return null;
        const allocator = self.allocator;
        const entries_dir = self.config.getCheckedArtifactCacheDir(allocator) catch return null;
        defer allocator.free(entries_dir);
        const cache_path = computeCacheFilePathIn(allocator, cache_key, entries_dir) catch return null;
        defer allocator.free(cache_path);
        if (!self.roc_ctx.fileExists(cache_path)) return null;
        if (CacheModule.tryMapCacheFile(self.roc_ctx, cache_path)) |mapped| return mapped;
        const data = self.roc_ctx.readFile(cache_path, allocator) catch return null;
        defer allocator.free(data);
        const buffer = allocator.alignedAlloc(u8, CacheModule.SERIALIZATION_ALIGNMENT, data.len) catch return null;
        @memcpy(buffer, data);
        return CacheModule.CacheData{ .allocated = buffer };
    }

    pub fn getStats(self: *const Self) CacheStats {
        return self.stats;
    }
};
