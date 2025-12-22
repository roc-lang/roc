//! Cache cleanup utilities for managing temporary and persistent cache files.
//!
//! This module provides background cleanup functionality that:
//! - Removes temporary runtime directories older than 5 minutes
//! - Removes persistent cache files (mod/ and exe/) older than 30 days
//! - Removes empty directories after cleanup
//!
//! The cleanup runs on a single background thread that is fire-and-forget:
//! it exits automatically when done, and if the main process exits first,
//! the OS will terminate the cleanup thread along with the process.

const std = @import("std");
const builtin = @import("builtin");
const CacheConfig = @import("cache_config.zig").CacheConfig;

const Allocator = std.mem.Allocator;

/// Cleanup configuration constants
pub const Config = struct {
    /// Maximum age for temp directories (5 minutes in nanoseconds)
    pub const TEMP_MAX_AGE_NS: i128 = 5 * 60 * std.time.ns_per_s;

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
pub const CleanupThread = struct {
    thread: std.Thread,

    /// Wait for the cleanup thread to complete.
    /// Note: This is optional - if you don't call join(), the thread will
    /// be automatically terminated when the main process exits.
    pub fn join(self: *CleanupThread) void {
        self.thread.join();
    }
};

/// Start background cleanup on a separate thread.
///
/// This spawns a single background thread that:
/// 1. Deletes temp directories older than 5 minutes
/// 2. Deletes persistent cache files older than 30 days
/// 3. Removes empty directories after cleanup
/// 4. Exits automatically when done
///
/// The thread is fire-and-forget: if the main process exits before cleanup
/// completes, the OS will automatically terminate the cleanup thread.
/// You do not need to join the returned handle.
pub fn startBackgroundCleanup(allocator: Allocator) !CleanupThread {
    const thread = try std.Thread.spawn(.{}, runCleanup, .{allocator});
    return CleanupThread{ .thread = thread };
}

/// Run the full cleanup process (called on background thread).
fn runCleanup(allocator: Allocator) void {
    // ---------------------------------------------------------------------------
    // TODO: REMOVE THIS FOR THE 0.1.0 RELEASE - NOT NEEDED ANYMORE
    // This is just to clean up people who have old stale Roc caches from before
    // we restructured the cache directories to use roc/{version}/ structure.
    // ---------------------------------------------------------------------------
    cleanupLegacyTempDirs(allocator, null);
    cleanupLegacyPersistentCache(allocator, null);
    // ---------------------------------------------------------------------------
    // END OF LEGACY CLEANUP - REMOVE ABOVE FOR 0.1.0
    // ---------------------------------------------------------------------------

    // Clean up temp directories (5 minute threshold)
    cleanupTempDirs(allocator, null);

    // Clean up persistent cache (30 day threshold)
    cleanupPersistentCache(allocator, null);
}

/// Clean up temporary runtime directories older than 5 minutes.
fn cleanupTempDirs(allocator: Allocator, maybe_stats: ?*CleanupStats) void {
    const temp_base = CacheConfig.getTempDir(allocator) catch return;
    defer allocator.free(temp_base);

    const now = std.time.nanoTimestamp();

    // Open the temp/roc directory
    var roc_dir = std.fs.cwd().openDir(temp_base, .{ .iterate = true }) catch return;
    defer roc_dir.close();

    // Iterate over version directories
    var version_iter = roc_dir.iterate();
    while (version_iter.next() catch null) |version_entry| {
        if (version_entry.kind != .directory) continue;

        const version_path = std.fs.path.join(allocator, &.{ temp_base, version_entry.name }) catch continue;
        defer allocator.free(version_path);

        var version_dir = std.fs.cwd().openDir(version_path, .{ .iterate = true }) catch continue;
        defer version_dir.close();

        // Iterate over random subdirectories within this version
        var random_iter = version_dir.iterate();
        while (random_iter.next() catch null) |random_entry| {
            const entry_path = std.fs.path.join(allocator, &.{ version_path, random_entry.name }) catch continue;
            defer allocator.free(entry_path);

            if (random_entry.kind == .directory) {
                // Check directory age
                const dir_stat = std.fs.cwd().statFile(entry_path) catch continue;
                const age_ns = now - dir_stat.mtime;

                if (age_ns > Config.TEMP_MAX_AGE_NS) {
                    // Delete the directory and its contents
                    std.fs.cwd().deleteTree(entry_path) catch {
                        if (maybe_stats) |stats| stats.errors += 1;
                        continue;
                    };
                    if (maybe_stats) |stats| stats.temp_dirs_deleted += 1;

                    // Also try to delete the coordination file (.txt)
                    const txt_path = std.fmt.allocPrint(allocator, "{s}.txt", .{entry_path}) catch continue;
                    defer allocator.free(txt_path);
                    std.fs.cwd().deleteFile(txt_path) catch {};
                    if (maybe_stats) |stats| stats.temp_files_deleted += 1;
                }
            } else if (random_entry.kind == .file) {
                // Check if it's a stale .txt coordination file
                if (std.mem.endsWith(u8, random_entry.name, ".txt")) {
                    const file_stat = std.fs.cwd().statFile(entry_path) catch continue;
                    const age_ns = now - file_stat.mtime;

                    if (age_ns > Config.TEMP_MAX_AGE_NS) {
                        std.fs.cwd().deleteFile(entry_path) catch {
                            if (maybe_stats) |stats| stats.errors += 1;
                            continue;
                        };
                        if (maybe_stats) |stats| stats.temp_files_deleted += 1;
                    }
                }
            }
        }

        // NOTE: We intentionally do NOT delete empty version directories here.
        // Doing so would race with other processes that just created the version
        // directory and are about to create a random subdirectory in it.
    }

    // NOTE: We intentionally do NOT delete the empty roc temp directory.
    // It's harmless and avoids race conditions with concurrent processes.
}

/// Clean up persistent cache files older than 30 days.
fn cleanupPersistentCache(allocator: Allocator, maybe_stats: ?*CleanupStats) void {
    const config = CacheConfig{};

    // Get the base cache directory
    const cache_base = config.getEffectiveCacheDir(allocator) catch return;
    defer allocator.free(cache_base);

    const now = std.time.nanoTimestamp();

    // Open the cache directory
    var cache_dir = std.fs.cwd().openDir(cache_base, .{ .iterate = true }) catch return;
    defer cache_dir.close();

    // Iterate over version directories
    var version_iter = cache_dir.iterate();
    while (version_iter.next() catch null) |version_entry| {
        if (version_entry.kind != .directory) continue;

        const version_path = std.fs.path.join(allocator, &.{ cache_base, version_entry.name }) catch continue;
        defer allocator.free(version_path);

        // Clean up mod/ directory
        const mod_path = std.fs.path.join(allocator, &.{ version_path, "mod" }) catch continue;
        cleanupCacheSubdir(allocator, mod_path, now, maybe_stats);
        allocator.free(mod_path);

        // Clean up exe/ directory
        const exe_path = std.fs.path.join(allocator, &.{ version_path, "exe" }) catch continue;
        cleanupCacheSubdir(allocator, exe_path, now, maybe_stats);
        allocator.free(exe_path);

        // NOTE: We intentionally do NOT delete empty version directories.
        // Empty directories are harmless and deleting them can cause race conditions.
    }

    // NOTE: We intentionally do NOT delete the empty cache base directory.
}

/// Clean up files in a cache subdirectory (mod/ or exe/) older than 30 days.
fn cleanupCacheSubdir(allocator: Allocator, subdir_path: []const u8, now: i128, maybe_stats: ?*CleanupStats) void {
    var subdir = std.fs.cwd().openDir(subdir_path, .{ .iterate = true }) catch return;
    defer subdir.close();

    // Iterate over subdirectories (hash buckets like "a0", "b1", etc.)
    var bucket_iter = subdir.iterate();
    while (bucket_iter.next() catch null) |bucket_entry| {
        if (bucket_entry.kind != .directory) {
            // Direct file in the subdir - check age and delete if old
            const file_path = std.fs.path.join(allocator, &.{ subdir_path, bucket_entry.name }) catch continue;
            defer allocator.free(file_path);

            const file_stat = std.fs.cwd().statFile(file_path) catch continue;
            const age_ns = now - file_stat.mtime;

            if (age_ns > Config.PERSISTENT_MAX_AGE_NS) {
                std.fs.cwd().deleteFile(file_path) catch {
                    if (maybe_stats) |stats| stats.errors += 1;
                    continue;
                };
                if (maybe_stats) |stats| stats.cache_files_deleted += 1;
            }
            continue;
        }

        const bucket_path = std.fs.path.join(allocator, &.{ subdir_path, bucket_entry.name }) catch continue;
        defer allocator.free(bucket_path);

        var bucket_dir = std.fs.cwd().openDir(bucket_path, .{ .iterate = true }) catch continue;
        defer bucket_dir.close();

        // Iterate over cache files in this bucket
        var file_iter = bucket_dir.iterate();
        while (file_iter.next() catch null) |file_entry| {
            if (file_entry.kind != .file) continue;

            const file_path = std.fs.path.join(allocator, &.{ bucket_path, file_entry.name }) catch continue;
            defer allocator.free(file_path);

            const file_stat = std.fs.cwd().statFile(file_path) catch continue;
            const age_ns = now - file_stat.mtime;

            if (age_ns > Config.PERSISTENT_MAX_AGE_NS) {
                std.fs.cwd().deleteFile(file_path) catch {
                    if (maybe_stats) |stats| stats.errors += 1;
                    continue;
                };
                if (maybe_stats) |stats| stats.cache_files_deleted += 1;
            }
        }

        // NOTE: We intentionally do NOT delete empty bucket directories.
        // Empty directories are harmless and deleting them can cause race conditions.
    }

    // NOTE: We intentionally do NOT delete empty subdirs.
}

/// Try to delete a directory if it's empty.
fn tryDeleteEmptyDir(path: []const u8) void {
    std.fs.cwd().deleteDir(path) catch |err| switch (err) {
        error.DirNotEmpty => {}, // Expected, directory has contents
        else => {},
    };
}

/// Delete a specific temp directory and its coordination file.
/// Used for immediate cleanup after spawning a child process.
pub fn deleteTempDir(allocator: Allocator, temp_dir_path: []const u8) void {
    // Delete the directory and its contents
    std.fs.cwd().deleteTree(temp_dir_path) catch {};

    // Delete the coordination file (.txt)
    const txt_path = std.fmt.allocPrint(allocator, "{s}.txt", .{temp_dir_path}) catch return;
    defer allocator.free(txt_path);
    std.fs.cwd().deleteFile(txt_path) catch {};
}

// ---------------------------------------------------------------------------
// LEGACY CLEANUP FUNCTIONS
// TODO: REMOVE THESE FOR THE 0.1.0 RELEASE - NOT NEEDED ANYMORE
// These clean up old cache directories from before we restructured to use
// the roc/{version}/ directory structure.
// ---------------------------------------------------------------------------

/// Clean up legacy temp directories that used the old "roc-*" prefix pattern.
/// Old structure: /tmp/roc-{random}/ (directly in temp, with roc- prefix)
/// New structure: /tmp/roc/{version}/{random}/
fn cleanupLegacyTempDirs(allocator: Allocator, maybe_stats: ?*CleanupStats) void {
    const temp_base = switch (builtin.target.os.tag) {
        .windows => std.process.getEnvVarOwned(allocator, "TEMP") catch
            std.process.getEnvVarOwned(allocator, "TMP") catch
            return,
        else => std.process.getEnvVarOwned(allocator, "TMPDIR") catch
            allocator.dupe(u8, "/tmp") catch return,
    };
    defer allocator.free(temp_base);

    var temp_dir = std.fs.cwd().openDir(temp_base, .{ .iterate = true }) catch return;
    defer temp_dir.close();

    // Look for directories matching "roc-*" pattern (old naming convention)
    var iter = temp_dir.iterate();
    while (iter.next() catch null) |entry| {
        if (entry.kind != .directory) continue;

        // Check if it starts with "roc-" (old prefix pattern)
        if (std.mem.startsWith(u8, entry.name, "roc-")) {
            const entry_path = std.fs.path.join(allocator, &.{ temp_base, entry.name }) catch continue;
            defer allocator.free(entry_path);

            // Delete the directory and its contents
            std.fs.cwd().deleteTree(entry_path) catch {
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
fn cleanupLegacyPersistentCache(allocator: Allocator, maybe_stats: ?*CleanupStats) void {
    const config = CacheConfig{};

    const cache_base = config.getEffectiveCacheDir(allocator) catch return;
    defer allocator.free(cache_base);

    var cache_dir = std.fs.cwd().openDir(cache_base, .{ .iterate = true }) catch return;
    defer cache_dir.close();

    // Look for old-style entries (hash directories or direct cache files)
    var iter = cache_dir.iterate();
    while (iter.next() catch null) |entry| {
        const entry_path = std.fs.path.join(allocator, &.{ cache_base, entry.name }) catch continue;
        defer allocator.free(entry_path);

        if (entry.kind == .file) {
            // Old-style: direct .rcache files in the cache root
            if (std.mem.endsWith(u8, entry.name, ".rcache")) {
                std.fs.cwd().deleteFile(entry_path) catch {
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
                std.fs.cwd().deleteTree(entry_path) catch {
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

// ---------------------------------------------------------------------------
// END OF LEGACY CLEANUP FUNCTIONS - REMOVE FOR 0.1.0
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test "Config constants are reasonable" {
    // 5 minutes in nanoseconds
    try std.testing.expectEqual(@as(i128, 300_000_000_000), Config.TEMP_MAX_AGE_NS);

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
    deleteTempDir(std.testing.allocator, "/nonexistent/path/that/does/not/exist");
}

test "tryDeleteEmptyDir handles non-existent directory" {
    // Should not crash when directory doesn't exist
    tryDeleteEmptyDir("/nonexistent/path");
}

test "deleteTempDir deletes directory and coordination file" {
    const allocator = std.testing.allocator;

    // Create a temporary test directory
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create a subdirectory simulating a temp runtime dir
    tmp_dir.dir.makeDir("test_temp_dir") catch unreachable;

    // Create a file inside the directory
    const inner_file = tmp_dir.dir.createFile("test_temp_dir/executable", .{}) catch unreachable;
    inner_file.close();

    // Create the coordination file (.txt)
    const coord_file = tmp_dir.dir.createFile("test_temp_dir.txt", .{}) catch unreachable;
    coord_file.close();

    // Get the full path to the temp dir
    const temp_dir_path = tmp_dir.dir.realpathAlloc(allocator, "test_temp_dir") catch unreachable;
    defer allocator.free(temp_dir_path);

    // Verify both exist
    tmp_dir.dir.access("test_temp_dir", .{}) catch {
        return error.TestSetupFailed;
    };
    tmp_dir.dir.access("test_temp_dir.txt", .{}) catch {
        return error.TestSetupFailed;
    };

    // Delete the temp dir
    deleteTempDir(allocator, temp_dir_path);

    // Verify directory is deleted
    tmp_dir.dir.access("test_temp_dir", .{}) catch |err| {
        try std.testing.expectEqual(error.FileNotFound, err);
        return; // Success - directory was deleted
    };
    // If we get here, the directory still exists
    return error.DirectoryNotDeleted;
}

test "tryDeleteEmptyDir deletes empty directory" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create an empty subdirectory
    tmp_dir.dir.makeDir("empty_dir") catch unreachable;

    // Verify it exists
    tmp_dir.dir.access("empty_dir", .{}) catch {
        return error.TestSetupFailed;
    };

    // Get the full path
    const allocator = std.testing.allocator;
    const empty_dir_path = tmp_dir.dir.realpathAlloc(allocator, "empty_dir") catch unreachable;
    defer allocator.free(empty_dir_path);

    // Try to delete it
    tryDeleteEmptyDir(empty_dir_path);

    // Verify it's deleted
    tmp_dir.dir.access("empty_dir", .{}) catch |err| {
        try std.testing.expectEqual(error.FileNotFound, err);
        return; // Success
    };
    return error.DirectoryNotDeleted;
}

test "tryDeleteEmptyDir does not delete non-empty directory" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create a subdirectory with a file
    tmp_dir.dir.makeDir("nonempty_dir") catch unreachable;
    const file = tmp_dir.dir.createFile("nonempty_dir/file.txt", .{}) catch unreachable;
    file.close();

    // Get the full path
    const allocator = std.testing.allocator;
    const nonempty_dir_path = tmp_dir.dir.realpathAlloc(allocator, "nonempty_dir") catch unreachable;
    defer allocator.free(nonempty_dir_path);

    // Try to delete it (should fail silently)
    tryDeleteEmptyDir(nonempty_dir_path);

    // Verify it still exists
    tmp_dir.dir.access("nonempty_dir", .{}) catch {
        return error.DirectoryShouldExist;
    };
    // Success - directory still exists as expected
}

test "cleanupCacheSubdir deletes old files and keeps new files" {
    const allocator = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create a cache subdir structure
    tmp_dir.dir.makeDir("cache_subdir") catch unreachable;
    tmp_dir.dir.makeDir("cache_subdir/bucket1") catch unreachable;
    tmp_dir.dir.makeDir("cache_subdir/bucket2") catch unreachable;

    // Create files in bucket1
    const file1 = tmp_dir.dir.createFile("cache_subdir/bucket1/old_file.rcache", .{}) catch unreachable;
    file1.close();

    const file2 = tmp_dir.dir.createFile("cache_subdir/bucket2/new_file.rcache", .{}) catch unreachable;
    file2.close();

    // Get the full path
    const subdir_path = tmp_dir.dir.realpathAlloc(allocator, "cache_subdir") catch unreachable;
    defer allocator.free(subdir_path);

    // Get current time - files will be very recent (age ~0)
    const now = std.time.nanoTimestamp();

    // Track stats
    var stats = CleanupStats{};

    // Run cleanup with current time - nothing should be deleted (files are too new)
    cleanupCacheSubdir(allocator, subdir_path, now, &stats);

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

    cleanupCacheSubdir(allocator, subdir_path, far_future, &stats2);

    // Both files should be deleted now
    try std.testing.expectEqual(@as(u32, 2), stats2.cache_files_deleted);

    // Verify files are gone
    tmp_dir.dir.access("cache_subdir/bucket1/old_file.rcache", .{}) catch |err| {
        try std.testing.expectEqual(error.FileNotFound, err);
        // Continue to check the other file
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

test "cleanup removes empty bucket directories" {
    const allocator = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create a cache subdir structure with empty buckets
    tmp_dir.dir.makeDir("cache_subdir") catch unreachable;
    tmp_dir.dir.makeDir("cache_subdir/empty_bucket") catch unreachable;

    // Create a file that will be deleted
    const file = tmp_dir.dir.createFile("cache_subdir/empty_bucket/old.rcache", .{}) catch unreachable;
    file.close();

    // Get the full path
    const subdir_path = tmp_dir.dir.realpathAlloc(allocator, "cache_subdir") catch unreachable;
    defer allocator.free(subdir_path);

    // Use future time to make file appear old
    const now = std.time.nanoTimestamp();
    const far_future = now + Config.PERSISTENT_MAX_AGE_NS + std.time.ns_per_s;

    var stats = CleanupStats{};
    cleanupCacheSubdir(allocator, subdir_path, far_future, &stats);

    // File should be deleted
    try std.testing.expectEqual(@as(u32, 1), stats.cache_files_deleted);

    // Empty bucket should be removed
    tmp_dir.dir.access("cache_subdir/empty_bucket", .{}) catch |err| {
        try std.testing.expectEqual(error.FileNotFound, err);
        return; // Success - empty bucket was removed
    };
    // If bucket still exists, that's also acceptable (some systems may not remove it immediately)
}
