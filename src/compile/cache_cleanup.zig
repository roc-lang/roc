//! Cache cleanup utilities for managing temporary and persistent cache files.
//!
//! This module provides background cleanup functionality that:
//! - Removes per-invocation scratch directories older than 5 minutes
//! - Removes persistent cache files (mod/, exe/, test/, and wasm-host/) older than 30 days
//! - Removes object-cache packs by last use, and every pack namespace of a
//!   format other than the current one
//! - Evicts the least recently used persistent cache files while the cache
//!   exceeds its configured size
//!
//! The cleanup runs on a single background thread that is fire-and-forget:
//! it exits automatically when done, and if the main process exits first,
//! the OS will terminate the cleanup thread along with the process.
//!
//! This is strictly a native `roc` CLI operation—it is never reached on
//! wasm (`compile/mod.zig` substitutes a stub there). It therefore talks to
//! the real OS directly through `std.Io.Dir` rather than going through the
//! `CoreCtx` filesystem abstraction, whose only purpose is injecting a
//! non-OS implementation (the playground's virtual FS or test mocks)—none
//! of which can apply here. The cache root path is resolved once by the
//! caller (see `startBackgroundCleanup`) and copied in by value, so this code
//! depends on neither `CoreCtx` nor a caller-provided allocator. The walk uses
//! open directory handles and operates on entry basenames, so it allocates
//! nothing and is unaffected by long total path lengths.

const std = @import("std");
const threading = @import("threading.zig");
const cache_config = @import("cache_config.zig");
const scratch_dir_name = cache_config.scratch_dir_name;

const Dir = std.Io.Dir;
const Io = std.Io;

const is_freestanding = threading.is_freestanding;

/// Cleanup configuration constants
pub const Config = struct {
    /// Maximum age for scratch directories (5 minutes in nanoseconds)
    pub const TEMP_MAX_AGE_NS: i96 = 5 * 60 * std.time.ns_per_s;

    /// Maximum age for persistent cache files (30 days in nanoseconds)
    pub const PERSISTENT_MAX_AGE_NS: i96 = 30 * 24 * 60 * 60 * std.time.ns_per_s;

    /// Maximum age for object-cache packs of modules reached by path
    /// (`objects-v<format>/<mode>/local/`), which every edit rewrites: one day.
    pub const LOCAL_PACK_MAX_AGE_NS: i96 = 24 * 60 * 60 * std.time.ns_per_s;
};

/// What the sweep needs to know about the compiler that started it.
pub const Limits = struct {
    /// Format version of the object-cache packs this compiler reads and
    /// writes. Every other pack namespace holds packs no compiler sharing this
    /// cache version can use.
    object_pack_format_version: u32,
    /// Upper bound on the total size of the persistent cache files, in bytes
    /// (`CacheConfig.max_size_mb`).
    max_size_bytes: u64,
};

/// Statistics from a cleanup operation
pub const CleanupStats = struct {
    temp_dirs_deleted: u32 = 0,
    temp_files_deleted: u32 = 0,
    cache_files_deleted: u32 = 0,
    stale_namespaces_deleted: u32 = 0,
    evicted_files_deleted: u32 = 0,
    empty_dirs_deleted: u32 = 0,
    errors: u32 = 0,
};

/// Background cleanup thread handle.
///
/// This is a fire-and-forget thread - you don't need to join it.
/// If the main process exits before cleanup completes, the OS will
/// automatically terminate this thread along with the process.
pub const CleanupThread = if (!is_freestanding) struct {
    thread: std.Thread,

    /// Wait for the cleanup thread to complete.
    /// Note: This is optional - if you don't call join(), the thread will
    /// be automatically terminated when the main process exits.
    pub fn join(self: *CleanupThread) void {
        self.thread.join();
    }
} else struct {};

/// The cache root path, owned by value so the detached thread depends on
/// neither the caller's allocator nor the caller's `CoreCtx`. Filled by
/// copying the resolved path slice in `startBackgroundCleanup`.
const CacheBase = struct {
    buf: [std.fs.max_path_bytes]u8 = undefined,
    len: usize = 0,

    fn path(self: *const CacheBase) []const u8 {
        return self.buf[0..self.len];
    }
};

/// Start background cleanup on a separate thread.
///
/// `cache_base` is the cache root (e.g. `~/.cache/roc`); resolve it with
/// `CacheConfig.getEffectiveCacheDir` so this matches where the cache writer
/// stores artifacts and where scratch directories are created. The slice is
/// copied by value into the spawned thread, so it only needs to be valid until
/// this function returns.
///
/// The thread is fire-and-forget: if the main process exits before cleanup
/// completes, the OS terminates it. You do not need to join the returned handle.
pub fn startBackgroundCleanup(cache_base: []const u8, std_io: Io, limits: Limits) std.Thread.SpawnError!?CleanupThread {
    if (comptime is_freestanding) return null;

    var base = CacheBase{};
    // A path longer than the platform path limit can't be opened anyway.
    if (cache_base.len > base.buf.len) return null;
    @memcpy(base.buf[0..cache_base.len], cache_base);
    base.len = cache_base.len;

    const thread = try std.Thread.spawn(.{}, runCleanup, .{ base, std_io, limits });
    return CleanupThread{ .thread = thread };
}

/// Run the full cleanup process (called on background thread).
fn runCleanup(base: CacheBase, std_io: Io, limits: Limits) void {
    sweep(std_io, base.path(), limits, nowNs(std_io), null);
}

/// Every cleanup pass over the cache rooted at `cache_base`, as of `now_ns`.
pub fn sweep(std_io: Io, cache_base: []const u8, limits: Limits, now_ns: i128, maybe_stats: ?*CleanupStats) void {
    // Remove old flat files without treating current compiler namespaces as legacy.
    cleanupLegacyPersistentCache(std_io, cache_base, maybe_stats);

    // Clean up scratch directories (5 minute threshold)
    cleanupScratchDirs(std_io, cache_base, now_ns, maybe_stats);

    // Clean up persistent cache (30 day threshold)
    cleanupPersistentCache(std_io, cache_base, now_ns, maybe_stats);

    // Clean up object-cache packs (one-day and 30-day thresholds by origin)
    cleanupObjectPacks(std_io, cache_base, limits.object_pack_format_version, now_ns, maybe_stats);

    // Evict least recently used files past the size bound
    enforceSizeLimit(std_io, cache_base, limits, maybe_stats);
}

/// Current wall-clock time in nanoseconds since the Unix epoch.
pub fn nowNs(std_io: Io) i128 {
    return std.Io.Timestamp.now(std_io, .real).nanoseconds;
}

/// Modification time of `name` (relative to `dir`) in nanoseconds, or null.
fn mtimeNs(dir: Dir, std_io: Io, name: []const u8) ?i128 {
    const info = dir.statFile(std_io, name, .{}) catch return null;
    const ns: i128 = @intCast(info.mtime.nanoseconds);
    return ns;
}

/// Clean up per-invocation scratch directories older than 5 minutes.
///
/// Layout: `<cache_base>/<version>/tmp/<random-dir>` plus sibling
/// `<random>.txt` coordination files. We act on the entries of each version's
/// scratch directory; we never recurse into the random dirs themselves.
fn cleanupScratchDirs(std_io: Io, cache_base: []const u8, now_ns: i128, maybe_stats: ?*CleanupStats) void {
    var base_dir = Dir.cwd().openDir(std_io, cache_base, .{ .iterate = true }) catch return;
    defer base_dir.close(std_io);

    var version_it = base_dir.iterate();
    while (true) {
        const version_entry = (version_it.next(std_io) catch break) orelse break;
        if (version_entry.kind != .directory) continue;

        var version_dir = base_dir.openDir(std_io, version_entry.name, .{}) catch continue;
        defer version_dir.close(std_io);
        var scratch_dir = version_dir.openDir(std_io, scratch_dir_name, .{ .iterate = true }) catch continue;
        defer scratch_dir.close(std_io);

        var entry_it = scratch_dir.iterate();
        while (true) {
            const entry = (entry_it.next(std_io) catch break) orelse break;

            if (entry.kind == .directory) {
                const mtime = mtimeNs(scratch_dir, std_io, entry.name) orelse continue;
                if (now_ns - mtime <= Config.TEMP_MAX_AGE_NS) continue;

                scratch_dir.deleteTree(std_io, entry.name) catch {
                    if (maybe_stats) |stats| stats.errors += 1;
                    continue;
                };
                if (maybe_stats) |stats| stats.temp_dirs_deleted += 1;

                // Also try to delete the coordination file (`<name>.txt`).
                var txt_buf: [std.fs.max_path_bytes]u8 = undefined;
                const txt_path = std.fmt.bufPrint(&txt_buf, "{s}.txt", .{entry.name}) catch continue;
                scratch_dir.deleteFile(std_io, txt_path) catch {};
                if (maybe_stats) |stats| stats.temp_files_deleted += 1;
            } else if (entry.kind == .file) {
                // Stale `.txt` coordination file with no surviving directory.
                if (!std.mem.endsWith(u8, entry.name, ".txt")) continue;
                const mtime = mtimeNs(scratch_dir, std_io, entry.name) orelse continue;
                if (now_ns - mtime <= Config.TEMP_MAX_AGE_NS) continue;

                scratch_dir.deleteFile(std_io, entry.name) catch {
                    if (maybe_stats) |stats| stats.errors += 1;
                    continue;
                };
                if (maybe_stats) |stats| stats.temp_files_deleted += 1;
            }
        }
    }

    // NOTE: We intentionally do NOT delete empty scratch directories. It's
    // harmless and avoids race conditions with concurrent processes.
}

/// Clean up persistent cache files older than 30 days.
///
/// Layout: `<cache_base>/<version>/{mod,exe,test,wasm-host}/...`.
fn cleanupPersistentCache(std_io: Io, cache_base: []const u8, now_ns: i128, maybe_stats: ?*CleanupStats) void {
    var base_dir = Dir.cwd().openDir(std_io, cache_base, .{ .iterate = true }) catch return;
    defer base_dir.close(std_io);

    var version_it = base_dir.iterate();
    while (true) {
        const version_entry = (version_it.next(std_io) catch break) orelse break;
        if (version_entry.kind != .directory) continue;

        var version_dir = base_dir.openDir(std_io, version_entry.name, .{ .iterate = true }) catch continue;
        defer version_dir.close(std_io);

        for (persistent_families) |subdir_info| {
            var subdir = version_dir.openDir(std_io, subdir_info.name, .{ .iterate = true }) catch continue;
            defer subdir.close(std_io);
            cleanupCacheSubdir(std_io, subdir, now_ns, maybe_stats, subdir_info.nested_directory_depth);
        }
    }

    // NOTE: We intentionally do NOT delete empty version directories. Empty
    // directories are harmless and deleting them can cause race conditions.
}

/// The persistent cache families under each version directory, with the
/// number of directory levels between a family's root and its files.
const persistent_families = [_]struct { name: []const u8, nested_directory_depth: u8 }{
    .{ .name = "mod", .nested_directory_depth = 1 },
    .{ .name = "mod-latest", .nested_directory_depth = 1 },
    .{ .name = "pair", .nested_directory_depth = 1 },
    .{ .name = "exe", .nested_directory_depth = 1 },
    .{ .name = "test", .nested_directory_depth = 1 },
    .{ .name = "wasm-host", .nested_directory_depth = 1 },
    .{ .name = "glue-plugin", .nested_directory_depth = 2 },
};

/// Directory levels between an object-pack namespace and its packs:
/// `<target-opt>/<local|pkg>/<module identity>/`.
const object_pack_nested_directory_depth = 3;

/// Writes the namespace of pack format `format_version` into `buf`.
fn objectPackNamespaceName(buf: []u8, format_version: u32) []const u8 {
    return std.fmt.bufPrint(buf, cache_config.object_pack_namespace_prefix ++ "{d}", .{format_version}) catch
        unreachable;
}

/// Clean up object-cache packs.
///
/// Layout: `<cache_base>/<version>/objects-v<format>/<target-opt>/<local|pkg>/
/// <module identity>/<artifact key>.rpk`. A namespace of any format other
/// than `format_version` is deleted outright: its packs can never be read
/// again. In the current namespace, packs under `pkg/` come from URL
/// packages, are written once per package version, and keep the 30-day
/// window; packs under `local/` are rewritten by every edit of the module and
/// keep a one-day window, so superseded packs do not pile up. A pack ages from
/// its last use, not its write: an in-use pack whose bytes never change must
/// not be deleted and rebuilt once per window.
fn cleanupObjectPacks(std_io: Io, cache_base: []const u8, format_version: u32, now_ns: i128, maybe_stats: ?*CleanupStats) void {
    var base_dir = Dir.cwd().openDir(std_io, cache_base, .{ .iterate = true }) catch return;
    defer base_dir.close(std_io);
    var namespace_buf: [cache_config.object_pack_namespace_prefix.len + 10]u8 = undefined;
    const current_namespace = objectPackNamespaceName(&namespace_buf, format_version);

    var version_it = base_dir.iterate();
    while (true) {
        const version_entry = (version_it.next(std_io) catch break) orelse break;
        if (version_entry.kind != .directory) continue;
        var version_dir = base_dir.openDir(std_io, version_entry.name, .{ .iterate = true }) catch continue;
        defer version_dir.close(std_io);

        var namespace_it = version_dir.iterate();
        while (true) {
            const namespace_entry = (namespace_it.next(std_io) catch break) orelse break;
            if (namespace_entry.kind != .directory) continue;
            if (!std.mem.startsWith(u8, namespace_entry.name, cache_config.object_pack_namespace_prefix)) continue;
            if (!std.mem.eql(u8, namespace_entry.name, current_namespace)) {
                version_dir.deleteTree(std_io, namespace_entry.name) catch {
                    if (maybe_stats) |stats| stats.errors += 1;
                    continue;
                };
                if (maybe_stats) |stats| stats.stale_namespaces_deleted += 1;
                continue;
            }
            var namespace_dir = version_dir.openDir(std_io, namespace_entry.name, .{ .iterate = true }) catch continue;
            defer namespace_dir.close(std_io);
            cleanupPackNamespace(std_io, namespace_dir, now_ns, maybe_stats);
        }
    }
}

fn cleanupPackNamespace(std_io: Io, namespace_dir: Dir, now_ns: i128, maybe_stats: ?*CleanupStats) void {
    var mode_it = namespace_dir.iterate();
    while (true) {
        const mode_entry = (mode_it.next(std_io) catch break) orelse break;
        if (mode_entry.kind != .directory) continue;
        var mode_dir = namespace_dir.openDir(std_io, mode_entry.name, .{ .iterate = true }) catch continue;
        defer mode_dir.close(std_io);
        const origins = [_]struct { name: []const u8, max_age_ns: i96 }{
            .{ .name = "local", .max_age_ns = Config.LOCAL_PACK_MAX_AGE_NS },
            .{ .name = "pkg", .max_age_ns = Config.PERSISTENT_MAX_AGE_NS },
        };
        for (origins) |origin| {
            var origin_dir = mode_dir.openDir(std_io, origin.name, .{ .iterate = true }) catch continue;
            defer origin_dir.close(std_io);
            var identity_it = origin_dir.iterate();
            while (true) {
                const identity_entry = (identity_it.next(std_io) catch break) orelse break;
                if (identity_entry.kind != .directory) continue;
                var identity_dir = origin_dir.openDir(std_io, identity_entry.name, .{ .iterate = true }) catch continue;
                defer identity_dir.close(std_io);
                var pack_it = identity_dir.iterate();
                while (true) {
                    const pack_entry = (pack_it.next(std_io) catch break) orelse break;
                    if (pack_entry.kind != .file) continue;
                    const last_use = lastUseNs(identity_dir, std_io, pack_entry.name) orelse continue;
                    if (now_ns - last_use <= origin.max_age_ns) continue;
                    identity_dir.deleteFile(std_io, pack_entry.name) catch {
                        if (maybe_stats) |stats| stats.errors += 1;
                        continue;
                    };
                    if (maybe_stats) |stats| stats.cache_files_deleted += 1;
                }
            }
        }
    }
}

/// One persistent cache file the size bound may evict.
const SizedFile = struct {
    /// Path relative to the cache root.
    path: []const u8,
    size: u64,
    last_use: i128,

    fn lessThan(_: void, lhs: SizedFile, rhs: SizedFile) bool {
        if (lhs.last_use != rhs.last_use) return lhs.last_use < rhs.last_use;
        return std.mem.order(u8, lhs.path, rhs.path) == .lt;
    }
};

/// Delete the least recently used persistent cache files, by `max(atime,
/// mtime)`, until the files left total at most `limits.max_size_bytes`. The
/// bound covers every family the age passes cover (checked modules, pairings,
/// executables, test results, wasm hosts, glue plugins, and object packs) in
/// every version directory, so one oversized family evicts the oldest files
/// of all of them. Lock files are never evicted: they name a stable identity
/// another process may hold.
fn enforceSizeLimit(std_io: Io, cache_base: []const u8, limits: Limits, maybe_stats: ?*CleanupStats) void {
    var base_dir = Dir.cwd().openDir(std_io, cache_base, .{ .iterate = true }) catch return;
    defer base_dir.close(std_io);

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var files: std.ArrayList(SizedFile) = .empty;
    var total: u64 = 0;
    collectSizedFiles(std_io, base_dir, arena.allocator(), limits.object_pack_format_version, &files, &total) catch {
        if (maybe_stats) |stats| stats.errors += 1;
        return;
    };
    if (total <= limits.max_size_bytes) return;

    std.mem.sort(SizedFile, files.items, {}, SizedFile.lessThan);
    for (files.items) |file| {
        if (total <= limits.max_size_bytes) break;
        base_dir.deleteFile(std_io, file.path) catch {
            if (maybe_stats) |stats| stats.errors += 1;
            continue;
        };
        total -= file.size;
        if (maybe_stats) |stats| stats.evicted_files_deleted += 1;
    }
}

fn collectSizedFiles(std_io: Io, base_dir: Dir, arena: std.mem.Allocator, format_version: u32, files: *std.ArrayList(SizedFile), total: *u64) std.mem.Allocator.Error!void {
    var namespace_buf: [cache_config.object_pack_namespace_prefix.len + 10]u8 = undefined;
    const current_namespace = objectPackNamespaceName(&namespace_buf, format_version);

    var version_it = base_dir.iterate();
    while (true) {
        const version_entry = (version_it.next(std_io) catch break) orelse break;
        if (version_entry.kind != .directory) continue;
        var version_dir = base_dir.openDir(std_io, version_entry.name, .{}) catch continue;
        defer version_dir.close(std_io);
        for (persistent_families) |family| {
            var family_dir = version_dir.openDir(std_io, family.name, .{ .iterate = true }) catch continue;
            defer family_dir.close(std_io);
            const prefix = try std.fs.path.join(arena, &.{ version_entry.name, family.name });
            try collectSizedFilesIn(std_io, family_dir, prefix, family.nested_directory_depth, arena, files, total);
        }
        var namespace_dir = version_dir.openDir(std_io, current_namespace, .{ .iterate = true }) catch continue;
        defer namespace_dir.close(std_io);
        const prefix = try std.fs.path.join(arena, &.{ version_entry.name, current_namespace });
        try collectSizedFilesIn(std_io, namespace_dir, prefix, object_pack_nested_directory_depth, arena, files, total);
    }
}

fn collectSizedFilesIn(std_io: Io, dir: Dir, prefix: []const u8, nested_directory_depth: u8, arena: std.mem.Allocator, files: *std.ArrayList(SizedFile), total: *u64) std.mem.Allocator.Error!void {
    var it = dir.iterate();
    while (true) {
        const entry = (it.next(std_io) catch break) orelse break;
        if (entry.kind == .file) {
            if (std.mem.endsWith(u8, entry.name, ".lock")) continue;
            const info = dir.statFile(std_io, entry.name, .{}) catch continue;
            const mtime: i128 = @intCast(info.mtime.nanoseconds);
            const atime: i128 = if (info.atime) |atime| @intCast(atime.nanoseconds) else mtime;
            try files.append(arena, .{
                .path = try std.fs.path.join(arena, &.{ prefix, entry.name }),
                .size = info.size,
                .last_use = @max(mtime, atime),
            });
            total.* += info.size;
        } else if (entry.kind == .directory and nested_directory_depth > 0) {
            var child = dir.openDir(std_io, entry.name, .{ .iterate = true }) catch continue;
            defer child.close(std_io);
            const child_prefix = try std.fs.path.join(arena, &.{ prefix, entry.name });
            try collectSizedFilesIn(std_io, child, child_prefix, nested_directory_depth - 1, arena, files, total);
        }
    }
}

/// The later of a file's access and modification times, so a read keeps a
/// file alive where the filesystem maintains access times, and a file ages
/// from its write where it does not.
fn lastUseNs(dir: Dir, std_io: Io, name: []const u8) ?i128 {
    const info = dir.statFile(std_io, name, .{}) catch return null;
    const mtime: i128 = @intCast(info.mtime.nanoseconds);
    const atime: i128 = if (info.atime) |atime| @intCast(atime.nanoseconds) else mtime;
    return @max(mtime, atime);
}

/// Clean up files in a cache subdirectory older than 30 days. The caller passes
/// the exact nested directory depth for the cache family: one bucket level for
/// mod/exe/test/wasm-host and target/opt levels for glue dylibs.
fn cleanupCacheSubdir(std_io: Io, subdir: Dir, now_ns: i128, maybe_stats: ?*CleanupStats, nested_directory_depth: u8) void {
    var it = subdir.iterate();
    while (true) {
        const entry = (it.next(std_io) catch break) orelse break;

        if (entry.kind == .file) {
            // A lock file names a stable cache identity. Removing it while a
            // process holds the lock would let another process create a new
            // inode and enter the same critical section concurrently.
            if (std.mem.endsWith(u8, entry.name, ".lock")) continue;
            deleteCacheFileIfOld(std_io, subdir, entry.name, now_ns, maybe_stats);
        } else if (entry.kind == .directory and nested_directory_depth > 0) {
            var child = subdir.openDir(std_io, entry.name, .{ .iterate = true }) catch continue;
            defer child.close(std_io);
            cleanupCacheSubdir(std_io, child, now_ns, maybe_stats, nested_directory_depth - 1);
        }
    }

    // NOTE: We intentionally do NOT delete empty bucket directories or subdirs.
    // Empty directories are harmless and deleting them can cause race conditions.
}

/// Delete `name` (relative to `dir`) if it is older than the persistent cache
/// threshold.
fn deleteCacheFileIfOld(std_io: Io, dir: Dir, name: []const u8, now_ns: i128, maybe_stats: ?*CleanupStats) void {
    const mtime = mtimeNs(dir, std_io, name) orelse return;
    if (now_ns - mtime <= Config.PERSISTENT_MAX_AGE_NS) return;

    dir.deleteFile(std_io, name) catch {
        if (maybe_stats) |stats| stats.errors += 1;
        return;
    };
    if (maybe_stats) |stats| stats.cache_files_deleted += 1;
}

/// Delete a specific scratch directory and its coordination file.
/// Used for immediate cleanup after spawning a child process.
pub fn deleteTempDir(std_io: Io, temp_dir_path: []const u8) void {
    // `temp_dir_path` is absolute; cwd()-relative operations resolve it directly.
    Dir.cwd().deleteTree(std_io, temp_dir_path) catch {};

    var txt_buf: [std.fs.max_path_bytes + 4]u8 = undefined;
    const txt_path = std.fmt.bufPrint(&txt_buf, "{s}.txt", .{temp_dir_path}) catch return;
    Dir.cwd().deleteFile(std_io, txt_path) catch {};
}

/// Remove legacy `.rcache` files directly inside the cache root.
///
/// Directory names cannot identify legacy caches: current source-derived
/// compiler compatibility IDs are hexadecimal, just like old flat hash
/// directories. Never delete a directory here. The scoped expiry passes
/// handle scratch directories and artifacts within compiler namespaces.
fn cleanupLegacyPersistentCache(std_io: Io, cache_base: []const u8, maybe_stats: ?*CleanupStats) void {
    var base = Dir.cwd().openDir(std_io, cache_base, .{ .iterate = true }) catch return;
    defer base.close(std_io);

    var it = base.iterate();
    while (true) {
        const entry = (it.next(std_io) catch break) orelse break;
        if (entry.kind != .file or !std.mem.endsWith(u8, entry.name, ".rcache")) continue;
        base.deleteFile(std_io, entry.name) catch {
            if (maybe_stats) |stats| stats.errors += 1;
            continue;
        };
        if (maybe_stats) |stats| stats.temp_files_deleted += 1;
    }
}

test "background cleanup preserves compatibility namespaces and removes only flat legacy files" {
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const cache_base = try std.fs.path.join(allocator, &.{ ".zig-cache", "tmp", &tmp_dir.sub_path, "roc" });
    defer allocator.free(cache_base);
    const compatibility_id = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef";
    const namespaces = [_][]const u8{ compatibility_id, "compat-" ++ compatibility_id, "debug-abcd1234", "embedder-compiler-v1" };
    for (namespaces) |namespace| {
        const scratch = try std.fs.path.join(allocator, &.{ cache_base, namespace, scratch_dir_name, "active" });
        defer allocator.free(scratch);
        const artifact_dir = try std.fs.path.join(allocator, &.{ cache_base, namespace, "mod", "aa" });
        defer allocator.free(artifact_dir);
        try Dir.cwd().createDirPath(std.testing.io, scratch);
        try Dir.cwd().createDirPath(std.testing.io, artifact_dir);
        const source = try std.fs.path.join(allocator, &.{ scratch, "main.roc" });
        defer allocator.free(source);
        const artifact = try std.fs.path.join(allocator, &.{ artifact_dir, "checked.rcache" });
        defer allocator.free(artifact);
        try Dir.cwd().writeFile(std.testing.io, .{ .sub_path = source, .data = "live staged source" });
        try Dir.cwd().writeFile(std.testing.io, .{ .sub_path = artifact, .data = "live checked artifact" });
    }
    const legacy_file = try std.fs.path.join(allocator, &.{ cache_base, "old.rcache" });
    defer allocator.free(legacy_file);
    try Dir.cwd().writeFile(std.testing.io, .{ .sub_path = legacy_file, .data = "flat legacy cache" });

    var cleanup = (try startBackgroundCleanup(cache_base, std.testing.io, test_limits)).?;
    cleanup.join();

    for (namespaces) |namespace| {
        const source = try std.fs.path.join(allocator, &.{ cache_base, namespace, scratch_dir_name, "active", "main.roc" });
        defer allocator.free(source);
        const artifact = try std.fs.path.join(allocator, &.{ cache_base, namespace, "mod", "aa", "checked.rcache" });
        defer allocator.free(artifact);
        const source_bytes = try Dir.cwd().readFileAlloc(std.testing.io, source, allocator, .unlimited);
        defer allocator.free(source_bytes);
        const artifact_bytes = try Dir.cwd().readFileAlloc(std.testing.io, artifact, allocator, .unlimited);
        defer allocator.free(artifact_bytes);
        try std.testing.expectEqualStrings("live staged source", source_bytes);
        try std.testing.expectEqualStrings("live checked artifact", artifact_bytes);
    }
    try std.testing.expectError(error.FileNotFound, Dir.cwd().access(std.testing.io, legacy_file, .{}));
}

const test_limits: Limits = .{ .object_pack_format_version = 1, .max_size_bytes = std.math.maxInt(u64) };

test "CleanupStats initializes to zero" {
    const stats = CleanupStats{};
    try std.testing.expectEqual(@as(u32, 0), stats.temp_dirs_deleted);
    try std.testing.expectEqual(@as(u32, 0), stats.temp_files_deleted);
    try std.testing.expectEqual(@as(u32, 0), stats.cache_files_deleted);
    try std.testing.expectEqual(@as(u32, 0), stats.empty_dirs_deleted);
    try std.testing.expectEqual(@as(u32, 0), stats.errors);
}

test "background cleanup preserves directories next to the cache root" {
    const allocator = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const root_path = try std.fs.path.join(allocator, &.{ ".zig-cache", "tmp", &tmp_dir.sub_path });
    defer allocator.free(root_path);
    const cache_base = try std.fs.path.join(allocator, &.{ root_path, "roc" });
    defer allocator.free(cache_base);
    const unrelated_dir = try std.fs.path.join(allocator, &.{ root_path, "roc-active-cache" });
    defer allocator.free(unrelated_dir);
    const sentinel_path = try std.fs.path.join(allocator, &.{ unrelated_dir, "sentinel" });
    defer allocator.free(sentinel_path);

    try Dir.cwd().createDirPath(std.testing.io, cache_base);
    try Dir.cwd().createDirPath(std.testing.io, unrelated_dir);
    (try Dir.cwd().createFile(std.testing.io, sentinel_path, .{})).close(std.testing.io);

    var base = CacheBase{};
    @memcpy(base.buf[0..cache_base.len], cache_base);
    base.len = cache_base.len;

    runCleanup(base, std.testing.io, test_limits);

    try Dir.cwd().access(std.testing.io, sentinel_path, .{});
}

test "cleanupScratchDirs deletes only stale entries of each version's scratch directory" {
    const allocator = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const cache_base = try std.fs.path.join(allocator, &.{ ".zig-cache", "tmp", &tmp_dir.sub_path, "roc" });
    defer allocator.free(cache_base);
    const run_dir = try std.fs.path.join(allocator, &.{ cache_base, "0.0.0-test", scratch_dir_name, "run" });
    defer allocator.free(run_dir);
    const run_exe = try std.fs.path.join(allocator, &.{ run_dir, "app" });
    defer allocator.free(run_exe);
    const run_txt = try std.fs.path.join(allocator, &.{ cache_base, "0.0.0-test", scratch_dir_name, "run.txt" });
    defer allocator.free(run_txt);
    const exe_dir = try std.fs.path.join(allocator, &.{ cache_base, "0.0.0-test", "exe", "old" });
    defer allocator.free(exe_dir);
    const exe_file = try std.fs.path.join(allocator, &.{ exe_dir, "app" });
    defer allocator.free(exe_file);

    try Dir.cwd().createDirPath(std.testing.io, run_dir);
    try Dir.cwd().createDirPath(std.testing.io, exe_dir);
    (try Dir.cwd().createFile(std.testing.io, run_exe, .{})).close(std.testing.io);
    (try Dir.cwd().createFile(std.testing.io, run_txt, .{})).close(std.testing.io);
    (try Dir.cwd().createFile(std.testing.io, exe_file, .{})).close(std.testing.io);

    const now_ns = nowNs(std.testing.io);

    var fresh_stats = CleanupStats{};
    cleanupScratchDirs(std.testing.io, cache_base, now_ns, &fresh_stats);
    try std.testing.expectEqual(@as(u32, 0), fresh_stats.temp_dirs_deleted);
    try Dir.cwd().access(std.testing.io, run_exe, .{});
    try Dir.cwd().access(std.testing.io, run_txt, .{});

    const stale_ns: i128 = now_ns + Config.TEMP_MAX_AGE_NS + std.time.ns_per_s;
    var stale_stats = CleanupStats{};
    cleanupScratchDirs(std.testing.io, cache_base, stale_ns, &stale_stats);
    try std.testing.expectEqual(@as(u32, 1), stale_stats.temp_dirs_deleted);
    try std.testing.expectError(error.FileNotFound, Dir.cwd().access(std.testing.io, run_dir, .{}));
    try std.testing.expectError(error.FileNotFound, Dir.cwd().access(std.testing.io, run_txt, .{}));
    try Dir.cwd().access(std.testing.io, exe_file, .{});
}

test "deleteTempDir deletes directory and coordination file" {
    const allocator = std.testing.allocator;

    // Create a temporary test directory
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create a subdirectory simulating a temp runtime dir
    tmp_dir.dir.createDir(std.testing.io, "test_temp_dir", .default_dir) catch unreachable;

    // Create a file inside the directory
    const inner_file = tmp_dir.dir.createFile(std.testing.io, "test_temp_dir/executable", .{}) catch unreachable;
    inner_file.close(std.testing.io);

    // Create the coordination file (.txt)
    const coord_file = tmp_dir.dir.createFile(std.testing.io, "test_temp_dir.txt", .{}) catch unreachable;
    coord_file.close(std.testing.io);

    // Get the full path to the temp dir
    const temp_dir_path = std.fs.path.join(allocator, &.{ ".zig-cache", "tmp", &tmp_dir.sub_path, "test_temp_dir" }) catch unreachable;
    defer allocator.free(temp_dir_path);

    // Verify both exist
    tmp_dir.dir.access(std.testing.io, "test_temp_dir", .{}) catch {
        return error.TestSetupFailed;
    };
    tmp_dir.dir.access(std.testing.io, "test_temp_dir.txt", .{}) catch {
        return error.TestSetupFailed;
    };

    // Delete the temp dir
    deleteTempDir(std.testing.io, temp_dir_path);

    // Verify directory is deleted
    tmp_dir.dir.access(std.testing.io, "test_temp_dir", .{}) catch |err| {
        try std.testing.expectEqual(error.FileNotFound, err);
        return; // Success - directory was deleted
    };
    // If we get here, the directory still exists
    return error.DirectoryNotDeleted;
}

test "cleanupCacheSubdir deletes old files and keeps new files" {
    const allocator = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create a cache subdir structure
    tmp_dir.dir.createDir(std.testing.io, "cache_subdir", .default_dir) catch unreachable;
    tmp_dir.dir.createDir(std.testing.io, "cache_subdir/bucket1", .default_dir) catch unreachable;
    tmp_dir.dir.createDir(std.testing.io, "cache_subdir/bucket2", .default_dir) catch unreachable;

    // Create files in buckets
    const file1 = tmp_dir.dir.createFile(std.testing.io, "cache_subdir/bucket1/old_file.rcache", .{}) catch unreachable;
    file1.close(std.testing.io);

    const file2 = tmp_dir.dir.createFile(std.testing.io, "cache_subdir/bucket2/new_file.rcache", .{}) catch unreachable;
    file2.close(std.testing.io);

    // Get the full path
    const subdir_path = std.fs.path.join(allocator, &.{ ".zig-cache", "tmp", &tmp_dir.sub_path, "cache_subdir" }) catch unreachable;
    defer allocator.free(subdir_path);

    // Get current time - files will be very recent (age ~0)
    const now_ns = nowNs(std.testing.io);

    // Run cleanup with current time - nothing should be deleted (files are too new)
    var stats = CleanupStats{};
    {
        var subdir = Dir.cwd().openDir(std.testing.io, subdir_path, .{ .iterate = true }) catch unreachable;
        defer subdir.close(std.testing.io);
        cleanupCacheSubdir(std.testing.io, subdir, now_ns, &stats, 1);
    }

    // Both files should still exist since they're brand new
    try std.testing.expectEqual(@as(u32, 0), stats.cache_files_deleted);

    tmp_dir.dir.access(std.testing.io, "cache_subdir/bucket1/old_file.rcache", .{}) catch {
        return error.FileShouldExist;
    };
    tmp_dir.dir.access(std.testing.io, "cache_subdir/bucket2/new_file.rcache", .{}) catch {
        return error.FileShouldExist;
    };

    // Now test with a fake "future" time that makes all files appear old
    const far_future_ns: i128 = now_ns + Config.PERSISTENT_MAX_AGE_NS + std.time.ns_per_s;
    var stats2 = CleanupStats{};
    {
        var subdir = Dir.cwd().openDir(std.testing.io, subdir_path, .{ .iterate = true }) catch unreachable;
        defer subdir.close(std.testing.io);
        cleanupCacheSubdir(std.testing.io, subdir, far_future_ns, &stats2, 1);
    }

    // Both files should be deleted now
    try std.testing.expectEqual(@as(u32, 2), stats2.cache_files_deleted);

    // Verify files are gone
    tmp_dir.dir.access(std.testing.io, "cache_subdir/bucket1/old_file.rcache", .{}) catch |err| {
        try std.testing.expectEqual(error.FileNotFound, err);
    };
}

test "cleanupPersistentCache deletes old cache files at each family depth" {
    const allocator = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const cache_base = std.fs.path.join(allocator, &.{ ".zig-cache", "tmp", &tmp_dir.sub_path, "roc-cache" }) catch unreachable;
    defer allocator.free(cache_base);

    const glue_dir = std.fs.path.join(allocator, &.{ cache_base, "0.0.0-test", "glue-plugin", "x64mac", "dev" }) catch unreachable;
    defer allocator.free(glue_dir);

    const mod_dir = std.fs.path.join(allocator, &.{ cache_base, "0.0.0-test", "mod", "aa" }) catch unreachable;
    defer allocator.free(mod_dir);
    const pair_dir = std.fs.path.join(allocator, &.{ cache_base, "0.0.0-test", "pair", "cc" }) catch unreachable;
    defer allocator.free(pair_dir);
    const wasm_host_dir = std.fs.path.join(allocator, &.{ cache_base, "0.0.0-test", "wasm-host", "bb" }) catch unreachable;
    defer allocator.free(wasm_host_dir);

    Dir.cwd().createDirPath(std.testing.io, glue_dir) catch unreachable;
    Dir.cwd().createDirPath(std.testing.io, mod_dir) catch unreachable;
    Dir.cwd().createDirPath(std.testing.io, pair_dir) catch unreachable;
    Dir.cwd().createDirPath(std.testing.io, wasm_host_dir) catch unreachable;

    const glue_file = std.fs.path.join(allocator, &.{ glue_dir, "old.o" }) catch unreachable;
    defer allocator.free(glue_file);
    const glue_tmp = std.fs.path.join(allocator, &.{ glue_dir, "old.o.1.0.tmp" }) catch unreachable;
    defer allocator.free(glue_tmp);
    const mod_file = std.fs.path.join(allocator, &.{ mod_dir, "old.rcache" }) catch unreachable;
    defer allocator.free(mod_file);
    const pair_file = std.fs.path.join(allocator, &.{ pair_dir, "old-pairing" }) catch unreachable;
    defer allocator.free(pair_file);
    const wasm_host_file = std.fs.path.join(allocator, &.{ wasm_host_dir, "old-host" }) catch unreachable;
    defer allocator.free(wasm_host_file);
    const wasm_host_lock = std.fs.path.join(allocator, &.{ wasm_host_dir, "old-host.lock" }) catch unreachable;
    defer allocator.free(wasm_host_lock);

    (Dir.cwd().createFile(std.testing.io, glue_file, .{}) catch unreachable).close(std.testing.io);
    (Dir.cwd().createFile(std.testing.io, glue_tmp, .{}) catch unreachable).close(std.testing.io);
    (Dir.cwd().createFile(std.testing.io, mod_file, .{}) catch unreachable).close(std.testing.io);
    (Dir.cwd().createFile(std.testing.io, pair_file, .{}) catch unreachable).close(std.testing.io);
    (Dir.cwd().createFile(std.testing.io, wasm_host_file, .{}) catch unreachable).close(std.testing.io);
    (Dir.cwd().createFile(std.testing.io, wasm_host_lock, .{}) catch unreachable).close(std.testing.io);

    const now_ns = nowNs(std.testing.io);
    const far_future_ns: i128 = now_ns + Config.PERSISTENT_MAX_AGE_NS + std.time.ns_per_s;
    var stats = CleanupStats{};
    cleanupPersistentCache(std.testing.io, cache_base, far_future_ns, &stats);

    try std.testing.expectEqual(@as(u32, 5), stats.cache_files_deleted);

    Dir.cwd().access(std.testing.io, glue_file, .{}) catch |err| {
        try std.testing.expectEqual(error.FileNotFound, err);
    };
    Dir.cwd().access(std.testing.io, glue_tmp, .{}) catch |err| {
        try std.testing.expectEqual(error.FileNotFound, err);
    };
    Dir.cwd().access(std.testing.io, mod_file, .{}) catch |err| {
        try std.testing.expectEqual(error.FileNotFound, err);
    };
    Dir.cwd().access(std.testing.io, wasm_host_file, .{}) catch |err| {
        try std.testing.expectEqual(error.FileNotFound, err);
    };
    try std.testing.expectError(error.FileNotFound, Dir.cwd().access(std.testing.io, pair_file, .{}));
    try Dir.cwd().access(std.testing.io, wasm_host_lock, .{});
}

/// Write a cache file whose last use is `last_use_ns`. The timestamps are
/// set through the open file, which every host supports.
fn writeTestFile(dir_path: []const u8, name: []const u8, data: []const u8, last_use_ns: i128) (Dir.CreateDirPathError || Dir.OpenError || Dir.WriteFileError || std.Io.File.OpenError || std.Io.File.SetTimestampsError)!void {
    try Dir.cwd().createDirPath(std.testing.io, dir_path);
    var dir = try Dir.cwd().openDir(std.testing.io, dir_path, .{});
    defer dir.close(std.testing.io);
    try dir.writeFile(std.testing.io, .{ .sub_path = name, .data = data });
    const file = try dir.openFile(std.testing.io, name, .{ .mode = .read_write });
    defer file.close(std.testing.io);
    const stamp: std.Io.File.SetTimestamp = .{ .new = .{ .nanoseconds = @intCast(last_use_ns) } };
    try file.setTimestamps(std.testing.io, .{ .access_timestamp = stamp, .modify_timestamp = stamp });
}

test "object pack sweep deletes stale format namespaces and ages current packs by origin" {
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const cache_base = try std.fs.path.join(allocator, &.{ ".zig-cache", "tmp", &tmp_dir.sub_path, "roc" });
    defer allocator.free(cache_base);

    const now_ns = nowNs(std.testing.io);
    const local_dir = try std.fs.path.join(allocator, &.{ cache_base, "0.0.0-test", "objects-v7", "x64glibc-dev", "local", "aa" });
    defer allocator.free(local_dir);
    const pkg_dir = try std.fs.path.join(allocator, &.{ cache_base, "0.0.0-test", "objects-v7", "x64glibc-dev", "pkg", "bb" });
    defer allocator.free(pkg_dir);
    const stale_dir = try std.fs.path.join(allocator, &.{ cache_base, "0.0.0-test", "objects-v6", "x64glibc-dev", "pkg", "bb" });
    defer allocator.free(stale_dir);
    const stale_namespace = try std.fs.path.join(allocator, &.{ cache_base, "0.0.0-test", "objects-v6" });
    defer allocator.free(stale_namespace);
    try writeTestFile(local_dir, "fresh.rpk", "local", now_ns);
    try writeTestFile(pkg_dir, "fresh.rpk", "pkg", now_ns);
    try writeTestFile(stale_dir, "fresh.rpk", "stale", now_ns);

    const limits: Limits = .{ .object_pack_format_version = 7, .max_size_bytes = std.math.maxInt(u64) };
    var stats = CleanupStats{};
    sweep(std.testing.io, cache_base, limits, now_ns, &stats);
    try std.testing.expectEqual(@as(u32, 1), stats.stale_namespaces_deleted);
    try std.testing.expectEqual(@as(u32, 0), stats.cache_files_deleted);
    try std.testing.expectError(error.FileNotFound, Dir.cwd().access(std.testing.io, stale_namespace, .{}));

    var local_dir_handle = try Dir.cwd().openDir(std.testing.io, local_dir, .{});
    defer local_dir_handle.close(std.testing.io);
    var pkg_dir_handle = try Dir.cwd().openDir(std.testing.io, pkg_dir, .{});
    defer pkg_dir_handle.close(std.testing.io);
    try local_dir_handle.access(std.testing.io, "fresh.rpk", .{});
    try pkg_dir_handle.access(std.testing.io, "fresh.rpk", .{});

    const two_days_later = now_ns + 2 * Config.LOCAL_PACK_MAX_AGE_NS;
    var aged_stats = CleanupStats{};
    sweep(std.testing.io, cache_base, limits, two_days_later, &aged_stats);
    try std.testing.expectEqual(@as(u32, 1), aged_stats.cache_files_deleted);
    try std.testing.expectError(error.FileNotFound, local_dir_handle.access(std.testing.io, "fresh.rpk", .{}));
    try pkg_dir_handle.access(std.testing.io, "fresh.rpk", .{});

    const past_window = now_ns + Config.PERSISTENT_MAX_AGE_NS + std.time.ns_per_s;
    var pkg_stats = CleanupStats{};
    sweep(std.testing.io, cache_base, limits, past_window, &pkg_stats);
    try std.testing.expectEqual(@as(u32, 1), pkg_stats.cache_files_deleted);
    try std.testing.expectError(error.FileNotFound, pkg_dir_handle.access(std.testing.io, "fresh.rpk", .{}));
}

test "size bound evicts least recently used files across families until under the limit" {
    const allocator = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const cache_base = try std.fs.path.join(allocator, &.{ ".zig-cache", "tmp", &tmp_dir.sub_path, "roc" });
    defer allocator.free(cache_base);

    const now_ns = nowNs(std.testing.io);
    const hour: i128 = 60 * 60 * std.time.ns_per_s;
    const mod_dir = try std.fs.path.join(allocator, &.{ cache_base, "0.0.0-test", "mod", "aa" });
    defer allocator.free(mod_dir);
    const exe_dir = try std.fs.path.join(allocator, &.{ cache_base, "0.0.0-test", "exe", "bb" });
    defer allocator.free(exe_dir);
    const pack_dir = try std.fs.path.join(allocator, &.{ cache_base, "0.0.0-test", "objects-v7", "x64glibc-dev", "pkg", "cc" });
    defer allocator.free(pack_dir);
    const block: [100]u8 = @splat('x');
    const lock_block: [300]u8 = @splat('x');
    try writeTestFile(mod_dir, "oldest.rcache", &block, now_ns - 3 * hour);
    try writeTestFile(pack_dir, "middle.rpk", &block, now_ns - 2 * hour);
    try writeTestFile(exe_dir, "newest", &block, now_ns - hour);
    try writeTestFile(exe_dir, "newest.lock", &lock_block, now_ns - 4 * hour);

    var stats = CleanupStats{};
    sweep(std.testing.io, cache_base, .{ .object_pack_format_version = 7, .max_size_bytes = 450 }, now_ns, &stats);
    try std.testing.expectEqual(@as(u32, 0), stats.evicted_files_deleted);

    sweep(std.testing.io, cache_base, .{ .object_pack_format_version = 7, .max_size_bytes = 150 }, now_ns, &stats);
    try std.testing.expectEqual(@as(u32, 2), stats.evicted_files_deleted);
    var mod = try Dir.cwd().openDir(std.testing.io, mod_dir, .{});
    defer mod.close(std.testing.io);
    var exe = try Dir.cwd().openDir(std.testing.io, exe_dir, .{});
    defer exe.close(std.testing.io);
    var pack = try Dir.cwd().openDir(std.testing.io, pack_dir, .{});
    defer pack.close(std.testing.io);
    try std.testing.expectError(error.FileNotFound, mod.access(std.testing.io, "oldest.rcache", .{}));
    try std.testing.expectError(error.FileNotFound, pack.access(std.testing.io, "middle.rpk", .{}));
    try exe.access(std.testing.io, "newest", .{});
    try exe.access(std.testing.io, "newest.lock", .{});
}
