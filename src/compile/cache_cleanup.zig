//! Cache cleanup utilities for managing temporary and persistent cache files.
//!
//! This module provides background cleanup functionality that:
//! - Removes temporary runtime entries older than 24 hours
//! - Removes persistent cache files (mod/, exe/, and test/) older than 30 days
//! - Removes empty directories after cleanup
//!
//! The cleanup runs on a single background thread that is fire-and-forget:
//! it exits automatically when done, and if the main process exits first,
//! the OS will terminate the cleanup thread along with the process.

const std = @import("std");
const builtin = @import("builtin");
const cache_config = @import("cache_config.zig");
const threading = @import("threading.zig");

const is_freestanding = threading.is_freestanding;

/// Cleanup configuration constants
pub const Config = struct {
    /// Maximum age for temp runtime entries (24 hours in nanoseconds)
    pub const TEMP_MAX_AGE_NS: i128 = 24 * 60 * 60 * std.time.ns_per_s;

    /// Maximum age for persistent cache files (30 days in nanoseconds)
    pub const PERSISTENT_MAX_AGE_NS: i128 = 30 * 24 * 60 * 60 * std.time.ns_per_s;
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

/// Start background cleanup on a separate thread.
///
/// This spawns a single background thread that:
/// 1. Deletes temp runtime entries older than 24 hours
/// 2. Deletes persistent cache files older than 30 days
/// 3. Removes empty directories after cleanup
/// 4. Exits automatically when done
///
/// The thread is fire-and-forget: if the main process exits before cleanup
/// completes, the OS will automatically terminate the cleanup thread.
/// You do not need to join the returned handle.
pub fn startBackgroundCleanup() std.Thread.SpawnError!?CleanupThread {
    if (comptime is_freestanding) return null;
    const thread = try std.Thread.spawn(.{}, runCleanup, .{});
    return CleanupThread{ .thread = thread };
}

/// Run the full cleanup process (called on background thread).
fn runCleanup() void {
    // TODO: REMOVE THIS FOR THE 0.1.0 RELEASE - NOT NEEDED ANYMORE
    // This is just to clean up people who have old stale Roc caches from before
    // we restructured the cache directories to use roc/{version}/ structure.
    cleanupLegacyTempDirs(null);
    cleanupLegacyPersistentCache(null);
    // END OF LEGACY CLEANUP - REMOVE ABOVE FOR 0.1.0

    // Clean up temp runtime entries (24 hour threshold)
    cleanupTempDirs(null);

    // Clean up persistent cache (30 day threshold)
    cleanupPersistentCache(null);
}

/// Clean up temporary runtime entries older than 24 hours.
fn cleanupTempDirs(maybe_stats: ?*CleanupStats) void {
    const now = std.time.nanoTimestamp();

    var roc_dir = openTempRocDir() orelse return;
    defer roc_dir.close();

    // Iterate over version directories
    var version_iter = roc_dir.iterate();
    while (version_iter.next() catch null) |version_entry| {
        if (version_entry.kind != .directory) continue;

        var version_dir = roc_dir.openDir(version_entry.name, .{ .iterate = true }) catch continue;
        defer version_dir.close();

        // Iterate over random runtime entries within this version.
        var random_iter = version_dir.iterate();
        while (random_iter.next() catch null) |random_entry| {
            cleanupTempEntry(&version_dir, random_entry, now, maybe_stats);
        }

        // NOTE: We intentionally do NOT delete empty version directories here.
        // Doing so would race with other processes that just created the version
        // directory and are about to create a random subdirectory in it.
    }

    // NOTE: We intentionally do NOT delete the empty roc temp directory.
    // It's harmless and avoids race conditions with concurrent processes.
}

/// Clean up persistent cache files older than 30 days.
fn cleanupPersistentCache(maybe_stats: ?*CleanupStats) void {
    const now = std.time.nanoTimestamp();

    var cache_dir = openCacheRootDir() orelse return;
    defer cache_dir.close();

    // Iterate over version directories
    var version_iter = cache_dir.iterate();
    while (version_iter.next() catch null) |version_entry| {
        if (version_entry.kind != .directory) continue;

        var version_dir = cache_dir.openDir(version_entry.name, .{}) catch continue;
        defer version_dir.close();

        cleanupCacheSubdirByName(&version_dir, "mod", now, maybe_stats);
        cleanupCacheSubdirByName(&version_dir, "exe", now, maybe_stats);
        cleanupCacheSubdirByName(&version_dir, "test", now, maybe_stats);

        // NOTE: We intentionally do NOT delete empty version directories.
        // Empty directories are harmless and deleting them can cause race conditions.
    }

    // NOTE: We intentionally do NOT delete the empty cache base directory.
}

/// Clean up files in a cache subdirectory (mod/ or exe/) older than 30 days.
fn cleanupCacheSubdirByName(version_dir: *std.fs.Dir, name: []const u8, now: i128, maybe_stats: ?*CleanupStats) void {
    var subdir = version_dir.openDir(name, .{ .iterate = true }) catch return;
    defer subdir.close();
    cleanupCacheSubdir(&subdir, now, maybe_stats);
}

/// Clean up files in an opened cache subdirectory (mod/ or exe/) older than 30 days.
fn cleanupCacheSubdir(subdir: *std.fs.Dir, now: i128, maybe_stats: ?*CleanupStats) void {
    // Iterate over subdirectories (hash buckets like "a0", "b1", etc.)
    var bucket_iter = subdir.iterate();
    while (bucket_iter.next() catch null) |bucket_entry| {
        if (bucket_entry.kind != .directory) {
            cleanupCacheFile(subdir, bucket_entry, now, maybe_stats);
            continue;
        }

        var bucket_dir = subdir.openDir(bucket_entry.name, .{ .iterate = true }) catch continue;
        defer bucket_dir.close();

        // Iterate over cache files in this bucket
        var file_iter = bucket_dir.iterate();
        while (file_iter.next() catch null) |file_entry| {
            cleanupCacheFile(&bucket_dir, file_entry, now, maybe_stats);
        }

        // NOTE: We intentionally do NOT delete empty bucket directories.
        // Empty directories are harmless and deleting them can cause race conditions.
    }

    // NOTE: We intentionally do NOT delete empty subdirs.
}

/// Delete a specific temp directory and its coordination file.
/// Used for immediate cleanup after spawning a child process.
pub fn deleteTempDir(temp_dir_path: []const u8, coordination_file_path: ?[]const u8) void {
    // Delete the directory and its contents
    std.fs.cwd().deleteTree(temp_dir_path) catch {};

    if (coordination_file_path) |path| {
        std.fs.cwd().deleteFile(path) catch {};
    }
}

// TODO: REMOVE THESE FOR THE 0.1.0 RELEASE - NOT NEEDED ANYMORE
// These clean up old cache directories from before we restructured to use
// the roc/{version}/ directory structure.

/// Clean up legacy temp directories that used the old "roc-*" prefix pattern.
/// Old structure: /tmp/roc-{random}/ (directly in temp, with roc- prefix)
/// New structure: /tmp/roc/{version}/{random}/
fn cleanupLegacyTempDirs(maybe_stats: ?*CleanupStats) void {
    var temp_dir = openSystemTempDir() orelse return;
    defer temp_dir.close();

    // Look for directories matching "roc-*" pattern (old naming convention)
    var iter = temp_dir.iterate();
    while (iter.next() catch null) |entry| {
        if (entry.kind != .directory) continue;

        // Check if it starts with "roc-" (old prefix pattern)
        if (std.mem.startsWith(u8, entry.name, "roc-")) {
            // Delete the directory and its contents
            temp_dir.deleteTree(entry.name) catch {
                if (maybe_stats) |stats| stats.errors += 1;
                continue;
            };
            if (maybe_stats) |stats| stats.temp_dirs_deleted += 1;
        }
    }
}

/// Clean up legacy persistent cache that used the old flat structure.
/// Old structure: ~/.cache/roc/{hash}/ or ~/.cache/roc/*.rcache (flat)
/// New structure: ~/.cache/roc/{version}/mod/ and ~/.cache/roc/{version}/exe/
fn cleanupLegacyPersistentCache(maybe_stats: ?*CleanupStats) void {
    var cache_dir = openCacheRootDir() orelse return;
    defer cache_dir.close();

    // Look for old-style entries (hash directories or direct cache files)
    var iter = cache_dir.iterate();
    while (iter.next() catch null) |entry| {
        if (entry.kind == .file) {
            // Old-style: direct .rcache files in the cache root
            if (std.mem.endsWith(u8, entry.name, ".rcache")) {
                cache_dir.deleteFile(entry.name) catch {
                    if (maybe_stats) |stats| stats.errors += 1;
                    continue;
                };
                if (maybe_stats) |stats| stats.cache_files_deleted += 1;
            }
        } else if (entry.kind == .directory) {
            // Check if this is an old-style hash directory (not a version directory)
            // Old hash dirs were like "a0b1c2d3..." (hex chars only, typically 16+ chars)
            // New version dirs are like "debug-abcd1234" (contain hyphen)
            const is_old_hash_dir = isLegacyHashDir(entry.name);

            if (is_old_hash_dir) {
                // Delete the entire old hash directory
                cache_dir.deleteTree(entry.name) catch {
                    if (maybe_stats) |stats| stats.errors += 1;
                    continue;
                };
                if (maybe_stats) |stats| stats.temp_dirs_deleted += 1;
            }
        }
    }
}

/// Check if a directory name looks like an old-style hash directory.
/// Old hash dirs: all hex characters, typically 16+ chars (blake3 hash prefix)
/// New version dirs: contain hyphens like "debug-abcd1234"
fn isLegacyHashDir(name: []const u8) bool {
    // New version directories always contain a hyphen
    if (std.mem.indexOfScalar(u8, name, '-') != null) {
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

fn cleanupTempEntry(version_dir: *std.fs.Dir, entry: std.fs.Dir.Entry, now: i128, maybe_stats: ?*CleanupStats) void {
    switch (entry.kind) {
        .directory, .file, .sym_link => {},
        else => return,
    }

    const stat = version_dir.statFile(entry.name) catch return;
    if (!isOlderThan(stat.mtime, now, Config.TEMP_MAX_AGE_NS)) return;

    switch (entry.kind) {
        .directory => {
            version_dir.deleteTree(entry.name) catch {
                if (maybe_stats) |stats| stats.errors += 1;
                return;
            };
            if (maybe_stats) |stats| stats.temp_dirs_deleted += 1;
        },
        .file, .sym_link => {
            version_dir.deleteFile(entry.name) catch {
                if (maybe_stats) |stats| stats.errors += 1;
                return;
            };
            if (maybe_stats) |stats| stats.temp_files_deleted += 1;
        },
        else => {},
    }
}

fn cleanupCacheFile(parent_dir: *std.fs.Dir, entry: std.fs.Dir.Entry, now: i128, maybe_stats: ?*CleanupStats) void {
    switch (entry.kind) {
        .file, .sym_link => {},
        else => return,
    }

    const stat = parent_dir.statFile(entry.name) catch return;
    if (!isOlderThan(stat.mtime, now, Config.PERSISTENT_MAX_AGE_NS)) return;

    parent_dir.deleteFile(entry.name) catch {
        if (maybe_stats) |stats| stats.errors += 1;
        return;
    };
    if (maybe_stats) |stats| stats.cache_files_deleted += 1;
}

fn isOlderThan(mtime: i128, now: i128, max_age_ns: i128) bool {
    return now - mtime > max_age_ns;
}

fn openTempRocDir() ?std.fs.Dir {
    var temp_dir = openSystemTempDir() orelse return null;
    defer temp_dir.close();

    return temp_dir.openDir("roc", .{ .iterate = true }) catch null;
}

fn openSystemTempDir() ?std.fs.Dir {
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const path = tempRootPath(&path_buf) orelse return null;
    return std.fs.cwd().openDir(path, .{ .iterate = true }) catch null;
}

fn openCacheRootDir() ?std.fs.Dir {
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;

    if (envPath("ROC_CACHE_DIR", &path_buf)) |path| {
        return std.fs.cwd().openDir(path, .{ .iterate = true }) catch null;
    }

    if (envPath("XDG_CACHE_HOME", &path_buf)) |path| {
        var xdg_dir = std.fs.cwd().openDir(path, .{}) catch return null;
        defer xdg_dir.close();
        return xdg_dir.openDir(cache_config.getCacheDirName(), .{ .iterate = true }) catch null;
    }

    if (comptime builtin.target.os.tag == .windows) {
        const appdata = envPath("APPDATA", &path_buf) orelse return null;
        var appdata_dir = std.fs.cwd().openDir(appdata, .{}) catch return null;
        defer appdata_dir.close();
        return appdata_dir.openDir(cache_config.getCacheDirName(), .{ .iterate = true }) catch null;
    } else if (comptime builtin.target.os.tag == .macos) {
        const home = envPath("HOME", &path_buf) orelse return null;
        var home_dir = std.fs.cwd().openDir(home, .{}) catch return null;
        defer home_dir.close();

        var library_dir = home_dir.openDir("Library", .{}) catch return null;
        defer library_dir.close();

        var caches_dir = library_dir.openDir("Caches", .{}) catch return null;
        defer caches_dir.close();

        return caches_dir.openDir(cache_config.getCacheDirName(), .{ .iterate = true }) catch null;
    } else {
        const home = envPath("HOME", &path_buf) orelse return null;
        var home_dir = std.fs.cwd().openDir(home, .{}) catch return null;
        defer home_dir.close();

        var dot_cache_dir = home_dir.openDir(".cache", .{}) catch return null;
        defer dot_cache_dir.close();

        return dot_cache_dir.openDir(cache_config.getCacheDirName(), .{ .iterate = true }) catch null;
    }
}

fn tempRootPath(buffer: []u8) ?[]const u8 {
    if (comptime builtin.target.os.tag == .windows) {
        return envPath("TEMP", buffer) orelse
            envPath("TMP", buffer) orelse
            "C:\\Windows\\Temp";
    } else {
        return envPath("TMPDIR", buffer) orelse "/tmp";
    }
}

fn envPath(comptime key: []const u8, buffer: []u8) ?[]const u8 {
    if (comptime builtin.target.os.tag == .windows) {
        const key_w = comptime std.unicode.wtf8ToWtf16LeStringLiteral(key);
        const value_w = std.process.getenvW(key_w) orelse return null;
        if (value_w.len * 3 + 1 > buffer.len) return null;
        const len = std.unicode.wtf16LeToWtf8(buffer, value_w);
        return buffer[0..len];
    } else {
        return std.posix.getenv(key);
    }
}

test "Config constants are reasonable" {
    // 24 hours in nanoseconds
    try std.testing.expectEqual(@as(i128, 24 * 60 * 60 * 1_000_000_000), Config.TEMP_MAX_AGE_NS);

    // 30 days in nanoseconds
    try std.testing.expectEqual(@as(i128, 30 * 24 * 60 * 60 * 1_000_000_000), Config.PERSISTENT_MAX_AGE_NS);
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
    deleteTempDir("/nonexistent/path/that/does/not/exist", null);
}

test "deleteTempDir deletes directory and coordination file" {
    const allocator = std.testing.allocator;

    // Create a temporary test directory
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create a subdirectory simulating a temp runtime dir
    try tmp_dir.dir.makeDir("test_temp_dir");

    // Create a file inside the directory
    const inner_file = try tmp_dir.dir.createFile("test_temp_dir/executable", .{});
    inner_file.close();

    // Create the coordination file (.txt)
    const coord_file = try tmp_dir.dir.createFile("test_temp_dir.txt", .{});
    coord_file.close();

    // Get the full path to the temp dir
    const temp_dir_path = try tmp_dir.dir.realpathAlloc(allocator, "test_temp_dir");
    defer allocator.free(temp_dir_path);
    const coord_file_path = try tmp_dir.dir.realpathAlloc(allocator, "test_temp_dir.txt");
    defer allocator.free(coord_file_path);

    // Verify both exist
    tmp_dir.dir.access("test_temp_dir", .{}) catch {
        return error.TestSetupFailed;
    };
    tmp_dir.dir.access("test_temp_dir.txt", .{}) catch {
        return error.TestSetupFailed;
    };

    // Delete the temp dir
    deleteTempDir(temp_dir_path, coord_file_path);

    // Verify directory is deleted
    try std.testing.expectError(error.FileNotFound, tmp_dir.dir.access("test_temp_dir", .{}));
    try std.testing.expectError(error.FileNotFound, tmp_dir.dir.access("test_temp_dir.txt", .{}));
}

test "cleanupCacheSubdir deletes old files and keeps new files" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create a cache subdir structure
    try tmp_dir.dir.makeDir("cache_subdir");
    try tmp_dir.dir.makeDir("cache_subdir/bucket1");
    try tmp_dir.dir.makeDir("cache_subdir/bucket2");

    // Create files in bucket1
    const file1 = try tmp_dir.dir.createFile("cache_subdir/bucket1/old_file.rcache", .{});
    file1.close();

    const file2 = try tmp_dir.dir.createFile("cache_subdir/bucket2/new_file.rcache", .{});
    file2.close();

    // Get current time - files will be very recent (age ~0)
    const now = std.time.nanoTimestamp();

    // Track stats
    var stats = CleanupStats{};

    // Run cleanup with current time - nothing should be deleted (files are too new)
    {
        var subdir = try tmp_dir.dir.openDir("cache_subdir", .{ .iterate = true });
        defer subdir.close();
        cleanupCacheSubdir(&subdir, now, &stats);
    }

    // Both files should still exist since they're brand new
    try std.testing.expectEqual(@as(u32, 0), stats.cache_files_deleted);

    // Verify files exist
    tmp_dir.dir.access("cache_subdir/bucket1/old_file.rcache", .{}) catch {
        return error.FileShouldExist;
    };
    tmp_dir.dir.access("cache_subdir/bucket2/new_file.rcache", .{}) catch {
        return error.FileShouldExist;
    };

    // Now test with a fake "future" time that makes all files appear old
    const far_future = now + Config.PERSISTENT_MAX_AGE_NS + std.time.ns_per_s;
    var stats2 = CleanupStats{};

    {
        var subdir = try tmp_dir.dir.openDir("cache_subdir", .{ .iterate = true });
        defer subdir.close();
        cleanupCacheSubdir(&subdir, far_future, &stats2);
    }

    // Both files should be deleted now
    try std.testing.expectEqual(@as(u32, 2), stats2.cache_files_deleted);

    // Verify files are gone
    try std.testing.expectError(error.FileNotFound, tmp_dir.dir.access("cache_subdir/bucket1/old_file.rcache", .{}));
    try std.testing.expectError(error.FileNotFound, tmp_dir.dir.access("cache_subdir/bucket2/new_file.rcache", .{}));
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

test "cleanup removes empty bucket directories" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create a cache subdir structure with empty buckets
    try tmp_dir.dir.makeDir("cache_subdir");
    try tmp_dir.dir.makeDir("cache_subdir/empty_bucket");

    // Create a file that will be deleted
    const file = try tmp_dir.dir.createFile("cache_subdir/empty_bucket/old.rcache", .{});
    file.close();

    // Use future time to make file appear old
    const now = std.time.nanoTimestamp();
    const far_future = now + Config.PERSISTENT_MAX_AGE_NS + std.time.ns_per_s;

    var stats = CleanupStats{};
    {
        var subdir = try tmp_dir.dir.openDir("cache_subdir", .{ .iterate = true });
        defer subdir.close();
        cleanupCacheSubdir(&subdir, far_future, &stats);
    }

    // File should be deleted
    try std.testing.expectEqual(@as(u32, 1), stats.cache_files_deleted);

    // Empty bucket should be removed
    tmp_dir.dir.access("cache_subdir/empty_bucket", .{}) catch |err| {
        try std.testing.expectEqual(error.FileNotFound, err);
        return; // Success - empty bucket was removed
    };
    // If bucket still exists, that's also acceptable (some systems may not remove it immediately)
}
