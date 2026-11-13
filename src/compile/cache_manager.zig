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
/// An acquired cache entry: its open file and lease.
pub const AcquiredArtifact = @import("artifact_store.zig").Artifact;
const Allocator = std.mem.Allocator;
const CoreCtx = ctx_mod.CoreCtx;
const CacheStats = @import("cache_config.zig").CacheStats;
const CacheConfig = @import("cache_config.zig").CacheConfig;
const artifacts = @import("artifact_store.zig");

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
    /// The store every read and write of this manager goes through,
    /// opened on first use and closed by `deinit`.
    store: ?*artifacts.Store = null,
    store_mutex: threading.Mutex = .init,

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

    /// Release the cache directory owned by this manager.
    pub fn deinit(self: *Self) void {
        if (comptime threading.is_freestanding) return;
        if (self.store) |store| {
            store.deinit();
            self.allocator.destroy(store);
            self.store = null;
        }
    }

    /// The manager's store, opened on first use. Null when the cache
    /// directory cannot be resolved, which every caller counts as a miss or a
    /// store failure.
    fn storeHandle(self: *Self) ?*artifacts.Store {
        if (comptime threading.is_freestanding) return null;
        self.store_mutex.lockUncancelable(self.roc_ctx.std_io);
        defer self.store_mutex.unlock(self.roc_ctx.std_io);
        if (self.store) |store| return store;
        const store = self.allocator.create(artifacts.Store) catch return null;
        store.* = self.openStore(self.allocator) catch {
            self.allocator.destroy(store);
            return null;
        };
        self.store = store;
        return store;
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

    /// Keep each producer's scope in the direct file identity.
    pub fn identity(family: artifacts.Family, key: [32]u8, scope: []const u8) artifacts.Identity {
        return .{
            .namespace = artifacts.hashBytes(@import("build_options").compiler_compatibility_id),
            .family = family,
            .group = artifacts.hashBytes(scope),
            .key = key,
        };
    }

    pub fn openStore(self: *const Self, allocator: Allocator) (Allocator.Error || error{NoHomeDirectory})!artifacts.Store {
        const root = try self.config.getEffectiveCacheDir(allocator);
        defer allocator.free(root);
        return .{ .allocator = allocator, .io = self.roc_ctx.std_io, .root = try std.fs.path.join(allocator, &.{ root, artifacts.format_directory }) };
    }

    fn familyFor(kind: Kind) artifacts.Family {
        return switch (kind) {
            .checked => .checked,
            .canonicalized => .canonicalized,
        };
    }

    pub fn storeRawBytes(self: *Self, key: [32]u8, bytes: []const u8, scope: []const u8, name: []const u8) void {
        self.storeFamilyBytes(.checked, key, bytes, scope, name);
    }

    pub fn storeFamilyBytes(self: *Self, family: artifacts.Family, key: [32]u8, bytes: []const u8, scope: []const u8, name: []const u8) void {
        self.storeEntry(self.allocator, .checked, family, key, .{ .bytes = bytes }, bytes.len, scope, name);
    }

    pub fn storeRawBytesIn(self: *Self, allocator: Allocator, kind: Kind, key: [32]u8, bytes: []const u8, scope: []const u8, name: []const u8) void {
        self.storeEntry(allocator, kind, familyFor(kind), key, .{ .bytes = bytes }, bytes.len, scope, name);
    }

    pub fn storeStream(self: *Self, key: [32]u8, stream: CoreCtx.FileStream, size: usize, scope: []const u8, name: []const u8) void {
        self.storeStreamIn(self.allocator, .checked, key, stream, size, scope, name);
    }

    pub fn storeStreamIn(self: *Self, allocator: Allocator, kind: Kind, key: [32]u8, stream: CoreCtx.FileStream, size: usize, scope: []const u8, name: []const u8) void {
        self.storeEntry(allocator, kind, familyFor(kind), key, .{ .stream = stream }, size, scope, name);
    }

    const EntrySource = union(enum) { bytes: []const u8, stream: CoreCtx.FileStream };

    /// A framed file's contents: its length prefix, the entry, its length
    /// trailer.
    const FramedEntry = struct {
        prefix: [artifacts.length_prefix_len]u8,
        source: EntrySource,
        trailer: [artifacts.length_trailer_len]u8,

        fn write(context: *const anyopaque, writer: *std.Io.Writer) std.Io.Writer.Error!void {
            const self: *const FramedEntry = @ptrCast(@alignCast(context));
            try writer.writeAll(&self.prefix);
            switch (self.source) {
                .bytes => |bytes| try writer.writeAll(bytes),
                .stream => |stream| try stream.write(stream.context, writer),
            }
            try writer.writeAll(&self.trailer);
        }
    };

    fn storeEntry(self: *Self, allocator: Allocator, kind: Kind, family: artifacts.Family, key: [32]u8, source: EntrySource, size: usize, scope: []const u8, name: []const u8) void {
        if (comptime threading.is_freestanding) return;
        if (!self.config.enabled) return;
        const store = self.storeHandle() orelse {
            self.recordStoreFailureFor(kind);
            return;
        };
        var stage = store.reserve(identity(family, key, scope)) catch |err| {
            self.verboseLog(allocator, "Failed to reserve {s} cache entry for {s}: {}\n", .{ @tagName(kind), name, err });
            self.recordStoreFailureFor(kind);
            return;
        };
        defer stage.deinit();
        const framed: FramedEntry = .{ .prefix = artifacts.lengthPrefix(size), .source = source, .trailer = artifacts.lengthTrailer(size) };
        const result = if (family.framed())
            self.roc_ctx.writeFileStream(stage.path, .{ .context = &framed, .write = FramedEntry.write })
        else switch (source) {
            .bytes => |bytes| self.roc_ctx.writeFile(stage.path, bytes),
            .stream => |stream| self.roc_ctx.writeFileStream(stage.path, stream),
        };
        result catch |err| {
            self.verboseLog(allocator, "Failed to write {s} cache entry for {s}: {}\n", .{ @tagName(kind), name, err });
            self.recordStoreFailureFor(kind);
            return;
        };
        if (family.framed()) {
            // The framing records the length the producer declared, so a
            // producer that wrote another length must not publish its file.
            const written = self.roc_ctx.stat(stage.path) catch |err| {
                self.verboseLog(allocator, "Failed to stat {s} cache entry for {s}: {}\n", .{ @tagName(kind), name, err });
                self.recordStoreFailureFor(kind);
                return;
            };
            if (written.size != size + artifacts.framing_len) {
                self.verboseLog(allocator, "Wrote {d} bytes of {s} cache entry for {s}, expected {d}\n", .{ written.size, @tagName(kind), name, size + artifacts.framing_len });
                self.recordStoreFailureFor(kind);
                return;
            }
        }
        stage.publish() catch |err| {
            self.verboseLog(allocator, "Failed to publish {s} cache entry for {s}: {}\n", .{ @tagName(kind), name, err });
            self.recordStoreFailureFor(kind);
            return;
        };
        self.recordStoreFor(kind, size);
    }

    /// An entry's payload read onto the heap, owned by the caller (free
    /// `bytes` with the manager's allocator), and which file it came from.
    pub const LoadedBytes = struct {
        bytes: []const u8,
        stamp: artifacts.Stamp,
    };

    pub fn loadRawBytes(self: *Self, key: [32]u8, scope: []const u8) ?LoadedBytes {
        return self.loadFamilyBytes(.checked, key, scope);
    }

    /// Read `family`'s entry for `key` after checking the length it records
    /// at both ends. A file whose recorded lengths are not its length is
    /// discarded and missed.
    pub fn loadFamilyBytes(self: *Self, family: artifacts.Family, key: [32]u8, scope: []const u8) ?LoadedBytes {
        std.debug.assert(family.framed());
        if (comptime threading.is_freestanding) return null;
        if (!self.config.enabled) return null;
        const store = self.storeHandle() orelse {
            self.recordMissFor(.checked);
            return null;
        };
        const id = identity(family, key, scope);
        var artifact = (store.acquire(id) catch null) orelse {
            self.recordMissFor(.checked);
            return null;
        };
        defer artifact.deinit();
        var prefix: [artifacts.length_prefix_len]u8 = @splat(0);
        var trailer: [artifacts.length_trailer_len]u8 = @splat(0);
        if (artifact.size >= artifacts.framing_len) {
            const prefix_read = artifact.file.readPositionalAll(store.io, &prefix, 0) catch 0;
            const trailer_read = artifact.file.readPositionalAll(store.io, &trailer, artifact.size - trailer.len) catch 0;
            if (prefix_read != prefix.len or trailer_read != trailer.len) {
                self.recordMissFor(.checked);
                return null;
            }
        }
        if (!artifacts.lengthsMatch(artifact.size, &prefix, &trailer)) {
            self.rejectEntry(.checked, family, key, scope, artifact.stamp(), "the lengths it records are not its length of {d} bytes", .{artifact.size});
            return null;
        }
        const payload_len = std.math.cast(usize, artifact.size - artifacts.framing_len) orelse {
            self.recordMissFor(.checked);
            return null;
        };
        const bytes = self.allocator.alloc(u8, payload_len) catch {
            self.recordMissFor(.checked);
            return null;
        };
        const len = artifact.file.readPositionalAll(store.io, bytes, artifacts.length_prefix_len) catch 0;
        if (len != payload_len) {
            self.allocator.free(bytes);
            self.recordMissFor(.checked);
            return null;
        }
        self.recordHitFor(.checked, artifact.size);
        return .{ .bytes = bytes, .stamp = artifact.stamp() };
    }

    /// Discard a loaded entry that is corrupt: count it as an invalidation of
    /// `kind`, say why under `--verbose`, and delete its file unless another
    /// writer has replaced it since it was loaded (`artifacts.Store.discard`),
    /// so no later build reads it again.
    pub fn rejectEntry(
        self: *Self,
        kind: Kind,
        family: artifacts.Family,
        key: [32]u8,
        scope: []const u8,
        stamp: artifacts.Stamp,
        comptime reason: []const u8,
        args: anytype,
    ) void {
        self.recordInvalidationFor(kind);
        self.verboseLog(self.allocator, "Discarding corrupt {s} cache entry {s}: " ++ reason ++ "\n", .{ @tagName(family), std.fmt.bytesToHex(key, .lower) } ++ args);
        if (comptime threading.is_freestanding) return;
        const store = self.storeHandle() orelse return;
        store.discard(identity(family, key, scope), stamp);
    }

    pub fn loadRawBytesMapped(self: *Self, key: [32]u8, scope: []const u8) ?CacheModule.CacheData {
        return self.loadFamilyMapped(self.allocator, .checked, .checked, key, scope);
    }

    pub fn loadRawBytesMappedIn(self: *Self, allocator: Allocator, kind: Kind, key: [32]u8, scope: []const u8) ?CacheModule.CacheData {
        return self.loadFamilyMapped(allocator, kind, familyFor(kind), key, scope);
    }

    /// File a copy of the file at `source_path` as `family`'s entry for
    /// `key`, staged beside the entry and renamed into place.
    pub fn storeFamilyFile(self: *Self, family: artifacts.Family, key: [32]u8, scope: []const u8, source_path: []const u8, name: []const u8) void {
        std.debug.assert(!family.framed());
        if (comptime threading.is_freestanding) return;
        if (!self.config.enabled) return;
        const store = self.storeHandle() orelse {
            self.recordStoreFailureFor(.checked);
            return;
        };
        var stage = store.reserve(identity(family, key, scope)) catch |err| {
            self.verboseLog(self.allocator, "Failed to reserve {s} cache entry for {s}: {}\n", .{ @tagName(family), name, err });
            self.recordStoreFailureFor(.checked);
            return;
        };
        defer stage.deinit();
        self.roc_ctx.copyFile(source_path, stage.path) catch |err| {
            self.verboseLog(self.allocator, "Failed to copy {s} into its cache entry: {}\n", .{ name, err });
            self.recordStoreFailureFor(.checked);
            return;
        };
        stage.publish() catch |err| {
            self.verboseLog(self.allocator, "Failed to publish {s} cache entry: {}\n", .{ name, err });
            self.recordStoreFailureFor(.checked);
            return;
        };
        self.recordStoreFor(.checked, 0);
    }

    /// Acquire `key`'s entry of `family` as an open file, owned by the
    /// caller until its `deinit`, for a consumer that copies its bytes by
    /// file range. Null for a miss.
    pub fn acquireFamily(self: *Self, family: artifacts.Family, key: [32]u8, scope: []const u8) ?artifacts.Artifact {
        std.debug.assert(!family.framed());
        if (comptime threading.is_freestanding) return null;
        if (!self.config.enabled) return null;
        const store = self.storeHandle() orelse {
            self.recordMissFor(.checked);
            return null;
        };
        const artifact = (store.acquire(identity(family, key, scope)) catch null) orelse {
            self.recordMissFor(.checked);
            return null;
        };
        self.recordHitFor(.checked, artifact.size);
        return artifact;
    }

    /// Map (or read) `family`'s entry for `key` after checking the length it
    /// records at both ends. A file whose recorded lengths are not its length
    /// is discarded and missed.
    pub fn loadFamilyMapped(self: *Self, allocator: Allocator, kind: Kind, family: artifacts.Family, key: [32]u8, scope: []const u8) ?CacheModule.CacheData {
        std.debug.assert(family.framed());
        const data = self.mapFamily(allocator, kind, family, key, scope) orelse return null;
        if (!artifacts.fileLengthsMatch(data.file())) {
            self.rejectEntry(kind, family, key, scope, data.stamp, "the lengths it records are not its length of {d} bytes", .{data.file().len});
            data.deinit(allocator);
            return null;
        }
        self.recordHitFor(kind, data.file().len);
        return data;
    }

    fn mapFamily(self: *Self, allocator: Allocator, kind: Kind, family: artifacts.Family, key: [32]u8, scope: []const u8) ?CacheModule.CacheData {
        if (comptime threading.is_freestanding) return null;
        if (!self.config.enabled) return null;
        const store = self.storeHandle() orelse {
            self.recordMissFor(kind);
            return null;
        };
        var artifact = (store.acquire(identity(family, key, scope)) catch null) orelse {
            self.recordMissFor(kind);
            return null;
        };
        defer artifact.deinit();
        const size = std.math.cast(usize, artifact.size) orelse return null;
        if (comptime CoreCtx.can_map_files) {
            if (size != 0) {
                const region = std.posix.mmap(null, size, .{ .READ = true, .WRITE = true }, .{ .TYPE = .PRIVATE }, artifact.file.handle, 0) catch null;
                if (region) |mapped| {
                    return .{ .storage = .{ .mapped = .{ .region = mapped, .file_len = size } }, .stamp = artifact.stamp() };
                }
            }
        }
        const bytes = allocator.alignedAlloc(u8, CacheModule.SERIALIZATION_ALIGNMENT, size) catch return null;
        const len = artifact.file.readPositionalAll(store.io, bytes, 0) catch {
            allocator.free(bytes);
            return null;
        };
        if (len != size) {
            allocator.free(bytes);
            return null;
        }
        return .{ .storage = .{ .allocated = bytes }, .stamp = artifact.stamp() };
    }

    pub fn getStats(self: *const Self) CacheStats {
        return self.stats;
    }
};
