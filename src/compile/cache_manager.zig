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

    /// The caller's cache scope is explicit identity data. It is no longer a
    /// directory that maintenance must discover by walking the filesystem.
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
            .checked, .inference => .checked,
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

    fn storeEntry(self: *Self, allocator: Allocator, kind: Kind, family: artifacts.Family, key: [32]u8, source: EntrySource, size: usize, scope: []const u8, name: []const u8) void {
        if (comptime threading.is_freestanding) return;
        if (!self.config.enabled) return;
        var store = self.openStore(allocator) catch {
            self.recordStoreFailureFor(kind);
            return;
        };
        defer store.deinit();
        var stage = store.reserve(identity(family, key, scope), size, store.now()) catch |err| {
            switch (err) {
                error.Busy => return,
                else => {},
            }
            self.verboseLog(allocator, "Failed to reserve {s} cache entry for {s}: {}\n", .{ @tagName(kind), name, err });
            self.recordStoreFailureFor(kind);
            return;
        };
        defer stage.deinit();
        const result = switch (source) {
            .bytes => |bytes| self.roc_ctx.writeFile(stage.path, bytes),
            .stream => |stream| self.roc_ctx.writeFileStream(stage.path, stream),
        };
        result catch |err| {
            self.verboseLog(allocator, "Failed to write {s} cache entry for {s}: {}\n", .{ @tagName(kind), name, err });
            self.recordStoreFailureFor(kind);
            return;
        };
        stage.publish() catch |err| {
            switch (err) {
                error.Busy => return,
                else => {},
            }
            self.verboseLog(allocator, "Failed to publish {s} cache entry for {s}: {}\n", .{ @tagName(kind), name, err });
            self.recordStoreFailureFor(kind);
            return;
        };
        self.recordStoreFor(kind, size);
    }

    pub const StagedPart = struct {
        store: *artifacts.Store,
        stage: artifacts.Staging,
        len: usize,
    };

    pub fn stageCheckedPart(self: *Self, key: [32]u8, stream: CoreCtx.FileStream, size: usize, scope: []const u8, name: []const u8) ?StagedPart {
        if (comptime threading.is_freestanding) return null;
        if (!self.config.enabled) return null;
        const store = self.allocator.create(artifacts.Store) catch {
            self.recordStoreFailure();
            return null;
        };
        store.* = self.openStore(self.allocator) catch {
            self.allocator.destroy(store);
            self.recordStoreFailure();
            return null;
        };
        var stage = store.reserve(identity(.checked, key, scope), size, store.now()) catch |err| {
            store.deinit();
            self.allocator.destroy(store);
            switch (err) {
                error.Busy => {},
                else => self.recordStoreFailure(),
            }
            return null;
        };
        self.roc_ctx.writeFileStream(stage.path, stream) catch |err| {
            self.verboseLog(self.allocator, "Failed to stage checked cache part for {s}: {}\n", .{ name, err });
            stage.deinit();
            store.deinit();
            self.allocator.destroy(store);
            self.recordStoreFailure();
            return null;
        };
        return .{ .store = store, .stage = stage, .len = size };
    }

    pub fn readStagedPart(self: *Self, part: StagedPart, allocator: Allocator) ?[]u8 {
        const bytes = self.roc_ctx.readFile(part.stage.path, allocator) catch {
            self.recordStoreFailure();
            return null;
        };
        if (bytes.len != part.len) {
            allocator.free(bytes);
            self.recordStoreFailure();
            return null;
        }
        return bytes;
    }

    pub fn discardStagedPart(self: *Self, part: StagedPart) void {
        self.roc_ctx.deleteFile(part.stage.path) catch {};
        var stage = part.stage;
        stage.deinit();
        // The catalog owns reclamation if this process exits or cannot acquire
        // metadata ownership here. It never depends on this destructor running.
        part.store.deinit();
        self.allocator.destroy(part.store);
    }

    pub fn loadRawBytes(self: *Self, key: [32]u8, scope: []const u8) ?[]const u8 {
        return self.loadFamilyBytes(.checked, key, scope);
    }

    pub fn loadFamilyBytes(self: *Self, family: artifacts.Family, key: [32]u8, scope: []const u8) ?[]const u8 {
        if (comptime threading.is_freestanding) return null;
        if (!self.config.enabled) return null;
        var store = self.openStore(self.allocator) catch {
            self.recordMissFor(.checked);
            return null;
        };
        defer store.deinit();
        const bytes = (store.read(self.allocator, identity(family, key, scope), store.now()) catch null) orelse {
            self.recordMissFor(.checked);
            return null;
        };
        self.recordHitFor(.checked, bytes.len);
        return bytes;
    }

    pub fn loadRawBytesMapped(self: *Self, key: [32]u8, scope: []const u8) ?CacheModule.CacheData {
        return self.loadFamilyMapped(self.allocator, .checked, .checked, key, scope);
    }

    pub fn loadRawBytesMappedIn(self: *Self, allocator: Allocator, kind: Kind, key: [32]u8, scope: []const u8) ?CacheModule.CacheData {
        return self.loadFamilyMapped(allocator, kind, familyFor(kind), key, scope);
    }

    pub fn loadFamilyMapped(self: *Self, allocator: Allocator, kind: Kind, family: artifacts.Family, key: [32]u8, scope: []const u8) ?CacheModule.CacheData {
        return self.loadMapped(allocator, kind, family, key, scope, true);
    }

    fn loadMapped(self: *Self, allocator: Allocator, kind: Kind, family: artifacts.Family, key: [32]u8, scope: []const u8, counted: bool) ?CacheModule.CacheData {
        if (comptime threading.is_freestanding) return null;
        if (!self.config.enabled) return null;
        var store = self.openStore(allocator) catch {
            if (counted) self.recordMissFor(kind);
            return null;
        };
        defer store.deinit();
        var artifact = (store.acquire(identity(family, key, scope), store.now()) catch null) orelse {
            if (counted) self.recordMissFor(kind);
            return null;
        };
        defer artifact.deinit();
        const size = std.math.cast(usize, artifact.size) orelse return null;
        if (comptime CoreCtx.can_map_files) {
            if (size != 0) {
                const region = std.posix.mmap(null, size, .{ .READ = true, .WRITE = true }, .{ .TYPE = .PRIVATE }, artifact.file.handle, 0) catch null;
                if (region) |mapped| {
                    if (counted) self.recordHitFor(kind, size);
                    return .{ .mapped = .{ .region = mapped, .file_len = size } };
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
        if (counted) self.recordHitFor(kind, size);
        return .{ .allocated = bytes };
    }

    const placement_pointer_magic = "roc-mod-latest-v1";
    const placement_pointer_len = placement_pointer_magic.len + 32;

    pub fn storePlacementPointer(self: *Self, placement: [32]u8, key: [32]u8) void {
        var contents: [placement_pointer_len]u8 = undefined;
        @memcpy(contents[0..placement_pointer_magic.len], placement_pointer_magic);
        @memcpy(contents[placement_pointer_magic.len..], &key);
        self.storeFamilyBytes(.placement, placement, &contents, "placement", "placement");
    }

    pub fn loadPlacementPointer(self: *Self, placement: [32]u8) ?[32]u8 {
        const bytes = self.loadFamilyBytes(.placement, placement, "placement") orelse return null;
        defer self.allocator.free(bytes);
        if (bytes.len != placement_pointer_len or !std.mem.eql(u8, bytes[0..placement_pointer_magic.len], placement_pointer_magic)) return null;
        return bytes[placement_pointer_magic.len..][0..32].*;
    }

    pub fn storeLiteralValues(self: *Self, placement: [32]u8, bytes: []const u8) void {
        self.storeFamilyBytes(.literals, placement, bytes, "literals", "literals");
    }

    pub fn loadLiteralValues(self: *Self, placement: [32]u8) ?[]u8 {
        return if (self.loadFamilyBytes(.literals, placement, "literals")) |bytes| @constCast(bytes) else null;
    }

    pub fn loadCheckedEntryUncounted(self: *Self, key: [32]u8) ?CacheModule.CacheData {
        const directory = self.config.getCheckedArtifactCacheDir(self.allocator) catch return null;
        defer self.allocator.free(directory);
        // This is a real cache acquisition for retention, even though it is a
        // reuse probe rather than the ordinary current-key lookup.
        return self.loadMapped(self.allocator, .checked, .checked, key, directory, false);
    }

    pub fn getStats(self: *const Self) CacheStats {
        return self.stats;
    }
};
