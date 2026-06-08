//! Cache cleanup utilities for managing temporary and persistent cache files.
//!
//! This module provides background cleanup functionality that:
//! - Removes temporary runtime directories older than 5 minutes
//! - Removes persistent cache files (mod/, exe/, and test/) older than 30 days
//!
//! The cleanup runs on a single background thread that is fire-and-forget:
//! it exits automatically when done, and if the main process exits first,
//! the OS will terminate the cleanup thread along with the process.
//!
//! This is strictly a native `roc` CLI operation — it is never reached on
//! wasm (`compile/mod.zig` substitutes a stub there). It therefore talks to
//! the real OS directly through `std.Io.Dir` rather than going through the
//! `CoreCtx` filesystem abstraction, whose only purpose is injecting a
//! non-OS implementation (the playground's virtual FS or test mocks) — none
//! of which can apply here. The directory base paths are resolved once by the
//! caller (see `startBackgroundCleanup`) and copied in by value, so this code
//! depends on neither `CoreCtx` nor a caller-provided allocator. The walk uses
//! open directory handles and operates on entry basenames, so it allocates
//! nothing and is unaffected by long total path lengths.

const std = @import("std");
const threading = @import("threading.zig");

const Dir = std.Io.Dir;
const Io = std.Io;

const is_freestanding = threading.is_freestanding;

/// Cleanup configuration constants
pub const Config = struct {
    /// Maximum age for temp directories (5 minutes in nanoseconds)
    pub const TEMP_MAX_AGE_NS: i96 = 5 * 60 * std.time.ns_per_s;

    /// Maximum age for persistent cache files (30 days in nanoseconds)
    pub const PERSISTENT_MAX_AGE_NS: i96 = 30 * 24 * 60 * 60 * std.time.ns_per_s;
};

/// Statistics from a cleanup operation
pub const CleanupStats = struct {
    temp_dirs_deleted: u32 = 0,
    temp_files_deleted: u32 = 0,
    cache_files_deleted: u32 = 0,
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

/// Cleanup base directories, owned by value so the detached thread depends on
/// neither the caller's allocator nor the caller's `CoreCtx`. Filled by
/// copying the resolved base path slices in `startBackgroundCleanup`.
const Bases = struct {
    temp_buf: [std.fs.max_path_bytes]u8 = undefined,
    temp_len: usize = 0,
    cache_buf: [std.fs.max_path_bytes]u8 = undefined,
    cache_len: usize = 0,

    /// The `<tmp>/roc` directory (empty if it could not be resolved).
    fn tempBase(self: *const Bases) []const u8 {
        return self.temp_buf[0..self.temp_len];
    }

    /// The persistent cache root, e.g. `~/.cache/roc` (empty if unresolved).
    fn cacheBase(self: *const Bases) []const u8 {
        return self.cache_buf[0..self.cache_len];
    }
};

/// Start background cleanup on a separate thread.
///
/// `temp_base` is the `<tmp>/roc` directory and `cache_base` is the persistent
/// cache root; resolve them with `cache_config.getTempDir` /
/// `CacheConfig.getEffectiveCacheDir` so this matches where the cache writer
/// stores artifacts. Pass an empty slice for either to skip that half of the
/// cleanup. The slices are copied by value into the spawned thread, so they
/// only need to be valid until this function returns.
///
/// The thread is fire-and-forget: if the main process exits before cleanup
/// completes, the OS terminates it. You do not need to join the returned handle.
pub fn startBackgroundCleanup(temp_base: []const u8, cache_base: []const u8, std_io: Io) std.Thread.SpawnError!?CleanupThread {
    if (comptime is_freestanding) return null;

    var bases = Bases{};
    // A base longer than the platform path limit can't be opened anyway; skip it.
    if (temp_base.len <= bases.temp_buf.len) {
        @memcpy(bases.temp_buf[0..temp_base.len], temp_base);
        bases.temp_len = temp_base.len;
    }
    if (cache_base.len <= bases.cache_buf.len) {
        @memcpy(bases.cache_buf[0..cache_base.len], cache_base);
        bases.cache_len = cache_base.len;
    }

    const thread = try std.Thread.spawn(.{}, runCleanup, .{ bases, std_io });
    return CleanupThread{ .thread = thread };
}

/// Run the full cleanup process (called on background thread).
fn runCleanup(bases: Bases, std_io: Io) void {
    const now_ns = nowNs(std_io);

    // TODO: REMOVE THIS FOR THE 0.1.0 RELEASE - NOT NEEDED ANYMORE
    // This is just to clean up people who have old stale Roc caches from before
    // we restructured the cache directories to use roc/{version}/ structure.
    // The legacy temp layout lived directly in the system temp dir (the parent
    // of `<tmp>/roc`), so walk that parent.
    if (std.fs.path.dirname(bases.tempBase())) |legacy_temp_root| {
        cleanupLegacyTempDirs(std_io, legacy_temp_root, null);
    }
    cleanupLegacyPersistentCache(std_io, bases.cacheBase(), null);
    // END OF LEGACY CLEANUP - REMOVE ABOVE FOR 0.1.0

    // Clean up temp directories (5 minute threshold)
    cleanupTempDirs(std_io, bases.tempBase(), now_ns, null);

    // Clean up persistent cache (30 day threshold)
    cleanupPersistentCache(std_io, bases.cacheBase(), now_ns, null);
}

/// Current wall-clock time in nanoseconds since the Unix epoch.
fn nowNs(std_io: Io) i128 {
    return std.Io.Timestamp.now(std_io, .real).nanoseconds;
}

/// Modification time of `name` (relative to `dir`) in nanoseconds, or null.
fn mtimeNs(dir: Dir, std_io: Io, name: []const u8) ?i128 {
    const info = dir.statFile(std_io, name, .{}) catch return null;
    const ns: i128 = @intCast(info.mtime.nanoseconds);
    return ns;
}

/// Clean up temporary runtime directories older than 5 minutes.
///
/// Layout: `<temp_base>/<version>/<random-dir>` plus sibling `<random>.txt`
/// coordination files. We descend one level (into version directories) and
/// act on their entries; we never recurse into the random dirs themselves.
fn cleanupTempDirs(std_io: Io, temp_base: []const u8, now_ns: i128, maybe_stats: ?*CleanupStats) void {
    var base_dir = Dir.cwd().openDir(std_io, temp_base, .{ .iterate = true }) catch return;
    defer base_dir.close(std_io);

    var version_it = base_dir.iterate();
    while (true) {
        const version_entry = (version_it.next(std_io) catch break) orelse break;
        if (version_entry.kind != .directory) continue;

        var version_dir = base_dir.openDir(std_io, version_entry.name, .{ .iterate = true }) catch continue;
        defer version_dir.close(std_io);

        var entry_it = version_dir.iterate();
        while (true) {
            const entry = (entry_it.next(std_io) catch break) orelse break;

            if (entry.kind == .directory) {
                const mtime = mtimeNs(version_dir, std_io, entry.name) orelse continue;
                if (now_ns - mtime <= Config.TEMP_MAX_AGE_NS) continue;

                version_dir.deleteTree(std_io, entry.name) catch {
                    if (maybe_stats) |stats| stats.errors += 1;
                    continue;
                };
                if (maybe_stats) |stats| stats.temp_dirs_deleted += 1;

                // Also try to delete the coordination file (`<name>.txt`).
                var txt_buf: [std.fs.max_path_bytes]u8 = undefined;
                const txt_path = std.fmt.bufPrint(&txt_buf, "{s}.txt", .{entry.name}) catch continue;
                version_dir.deleteFile(std_io, txt_path) catch {};
                if (maybe_stats) |stats| stats.temp_files_deleted += 1;
            } else if (entry.kind == .file) {
                // Stale `.txt` coordination file with no surviving directory.
                if (!std.mem.endsWith(u8, entry.name, ".txt")) continue;
                const mtime = mtimeNs(version_dir, std_io, entry.name) orelse continue;
                if (now_ns - mtime <= Config.TEMP_MAX_AGE_NS) continue;

                version_dir.deleteFile(std_io, entry.name) catch {
                    if (maybe_stats) |stats| stats.errors += 1;
                    continue;
                };
                if (maybe_stats) |stats| stats.temp_files_deleted += 1;
            }
        }
    }

    // NOTE: We intentionally do NOT delete empty version directories or the roc
    // temp directory. It's harmless and avoids race conditions with concurrent
    // processes.
}

/// Clean up persistent cache files older than 30 days.
///
/// Layout: `<cache_base>/<version>/{mod,exe,test}/...`.
fn cleanupPersistentCache(std_io: Io, cache_base: []const u8, now_ns: i128, maybe_stats: ?*CleanupStats) void {
    var base_dir = Dir.cwd().openDir(std_io, cache_base, .{ .iterate = true }) catch return;
    defer base_dir.close(std_io);

    var version_it = base_dir.iterate();
    while (true) {
        const version_entry = (version_it.next(std_io) catch break) orelse break;
        if (version_entry.kind != .directory) continue;

        var version_dir = base_dir.openDir(std_io, version_entry.name, .{ .iterate = true }) catch continue;
        defer version_dir.close(std_io);

        for ([_][]const u8{ "mod", "exe", "test" }) |subdir_name| {
            var subdir = version_dir.openDir(std_io, subdir_name, .{ .iterate = true }) catch continue;
            defer subdir.close(std_io);
            cleanupCacheSubdir(std_io, subdir, now_ns, maybe_stats);
        }
    }

    // NOTE: We intentionally do NOT delete empty version directories. Empty
    // directories are harmless and deleting them can cause race conditions.
}

/// Clean up files in a cache subdirectory (mod/, exe/, or test/) older than 30
/// days. Accepts files directly in `subdir` as well as files one level deep in
/// bucket directories (e.g. "a0", "b1"); deeper nesting is ignored.
fn cleanupCacheSubdir(std_io: Io, subdir: Dir, now_ns: i128, maybe_stats: ?*CleanupStats) void {
    var it = subdir.iterate();
    while (true) {
        const entry = (it.next(std_io) catch break) orelse break;

        if (entry.kind == .file) {
            deleteCacheFileIfOld(std_io, subdir, entry.name, now_ns, maybe_stats);
        } else if (entry.kind == .directory) {
            var bucket = subdir.openDir(std_io, entry.name, .{ .iterate = true }) catch continue;
            defer bucket.close(std_io);

            var bucket_it = bucket.iterate();
            while (true) {
                const file = (bucket_it.next(std_io) catch break) orelse break;
                if (file.kind != .file) continue;
                deleteCacheFileIfOld(std_io, bucket, file.name, now_ns, maybe_stats);
            }
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

/// Delete a specific temp directory and its coordination file.
/// Used for immediate cleanup after spawning a child process.
pub fn deleteTempDir(std_io: Io, temp_dir_path: []const u8) void {
    // `temp_dir_path` is absolute; cwd()-relative operations resolve it directly.
    Dir.cwd().deleteTree(std_io, temp_dir_path) catch {};

    var txt_buf: [std.fs.max_path_bytes + 4]u8 = undefined;
    const txt_path = std.fmt.bufPrint(&txt_buf, "{s}.txt", .{temp_dir_path}) catch return;
    Dir.cwd().deleteFile(std_io, txt_path) catch {};
}

// TODO: REMOVE THESE FOR THE 0.1.0 RELEASE - NOT NEEDED ANYMORE
// These clean up old cache directories from before we restructured to use
// the roc/{version}/ directory structure.

/// Clean up legacy temp directories that used the old "roc-*" prefix pattern.
/// Old structure: <legacy_temp_root>/roc-{random}/ (directly in temp, with roc- prefix)
/// New structure: <legacy_temp_root>/roc/{version}/{random}/
fn cleanupLegacyTempDirs(std_io: Io, legacy_temp_root: []const u8, maybe_stats: ?*CleanupStats) void {
    var root = Dir.cwd().openDir(std_io, legacy_temp_root, .{ .iterate = true }) catch return;
    defer root.close(std_io);

    // Look for directories matching the old "roc-*" naming convention and
    // remove them unconditionally (this format is no longer produced).
    var it = root.iterate();
    while (true) {
        const entry = (it.next(std_io) catch break) orelse break;
        if (entry.kind != .directory) continue;
        if (!std.mem.startsWith(u8, entry.name, "roc-")) continue;

        root.deleteTree(std_io, entry.name) catch {
            if (maybe_stats) |stats| stats.errors += 1;
            continue;
        };
        if (maybe_stats) |stats| stats.temp_dirs_deleted += 1;
    }
}

/// Clean up legacy persistent cache that used the old flat structure.
/// Old structure: ~/.cache/roc/{hash}/ or ~/.cache/roc/*.rcache (flat)
/// New structure: ~/.cache/roc/{version}/mod/ and ~/.cache/roc/{version}/exe/
fn cleanupLegacyPersistentCache(std_io: Io, cache_base: []const u8, maybe_stats: ?*CleanupStats) void {
    var base = Dir.cwd().openDir(std_io, cache_base, .{ .iterate = true }) catch return;
    defer base.close(std_io);

    var it = base.iterate();
    while (true) {
        const entry = (it.next(std_io) catch break) orelse break;

        if (entry.kind == .file) {
            // Old-style: direct .rcache files in the cache root.
            if (!std.mem.endsWith(u8, entry.name, ".rcache")) continue;
            base.deleteFile(std_io, entry.name) catch {
                if (maybe_stats) |stats| stats.errors += 1;
                continue;
            };
            if (maybe_stats) |stats| stats.cache_files_deleted += 1;
        } else if (entry.kind == .directory) {
            // Old-style hash directory (vs. a new version dir, which has a hyphen).
            if (!isLegacyHashDir(entry.name)) continue;
            base.deleteTree(std_io, entry.name) catch {
                if (maybe_stats) |stats| stats.errors += 1;
                continue;
            };
            if (maybe_stats) |stats| stats.temp_dirs_deleted += 1;
        }
    }
}

/// Check if a directory name looks like an old-style hash directory.
/// Old hash dirs: all hex characters, typically 16+ chars (blake3 hash prefix)
/// New version dirs: contain hyphens like "debug-abcd1234"
fn isLegacyHashDir(name: []const u8) bool {
    // New version directories always contain a hyphen
    if (std.mem.findScalar(u8, name, '-') != null) {
        return false;
    }

    // Old hash directories are all hex characters and fairly long
    if (name.len < 8) return false;

    for (name) |c| {
        const is_hex = (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
        if (!is_hex) return false;
    }

    return true;
}

test "Config constants are reasonable" {
    // 5 minutes in nanoseconds
    try std.testing.expectEqual(@as(i96, 300_000_000_000), Config.TEMP_MAX_AGE_NS);

    // 30 days in nanoseconds
    try std.testing.expectEqual(@as(i96, 30 * 24 * 60 * 60 * 1_000_000_000), Config.PERSISTENT_MAX_AGE_NS);
}

test "CleanupStats initializes to zero" {
    const stats = CleanupStats{};
    try std.testing.expectEqual(@as(u32, 0), stats.temp_dirs_deleted);
    try std.testing.expectEqual(@as(u32, 0), stats.temp_files_deleted);
    try std.testing.expectEqual(@as(u32, 0), stats.cache_files_deleted);
    try std.testing.expectEqual(@as(u32, 0), stats.empty_dirs_deleted);
    try std.testing.expectEqual(@as(u32, 0), stats.errors);
}

test "deleteTempDir handles non-existent directory" {
    // Should not crash when directory doesn't exist
    deleteTempDir(std.testing.io, "/nonexistent/path/that/does/not/exist");
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
        cleanupCacheSubdir(std.testing.io, subdir, now_ns, &stats);
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
        cleanupCacheSubdir(std.testing.io, subdir, far_future_ns, &stats2);
    }

    // Both files should be deleted now
    try std.testing.expectEqual(@as(u32, 2), stats2.cache_files_deleted);

    // Verify files are gone
    tmp_dir.dir.access(std.testing.io, "cache_subdir/bucket1/old_file.rcache", .{}) catch |err| {
        try std.testing.expectEqual(error.FileNotFound, err);
    };
}

test "CleanupStats tracks operations correctly" {
    var stats = CleanupStats{};

    // Simulate operations
    stats.temp_dirs_deleted += 1;
    stats.temp_dirs_deleted += 1;
    stats.temp_files_deleted += 1;
    stats.cache_files_deleted += 5;
    stats.empty_dirs_deleted += 3;
    stats.errors += 1;

    try std.testing.expectEqual(@as(u32, 2), stats.temp_dirs_deleted);
    try std.testing.expectEqual(@as(u32, 1), stats.temp_files_deleted);
    try std.testing.expectEqual(@as(u32, 5), stats.cache_files_deleted);
    try std.testing.expectEqual(@as(u32, 3), stats.empty_dirs_deleted);
    try std.testing.expectEqual(@as(u32, 1), stats.errors);
}
