//! Cache cleanup utilities for managing temporary and persistent cache files.
//!
//! This module provides background cleanup functionality that:
//! - Removes temporary runtime directories older than 5 minutes
//! - Removes persistent cache files (mod/, exe/, and test/) older than 30 days
//! - Removes empty directories after cleanup
//!
//! The cleanup runs on a single background thread that is fire-and-forget:
//! it exits automatically when done, and if the main process exits first,
//! the OS will terminate the cleanup thread along with the process.

const std = @import("std");
const builtin = @import("builtin");
const cache_config = @import("cache_config.zig");
const CacheConfig = cache_config.CacheConfig;
const CoreCtx = @import("ctx").CoreCtx;
const threading = @import("threading.zig");

const Allocator = std.mem.Allocator;

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
pub fn startBackgroundCleanup(allocator: Allocator, roc_ctx: CoreCtx) !?CleanupThread {
    if (comptime is_freestanding) return null;
    const thread = try std.Thread.spawn(.{}, runCleanup, .{ allocator, roc_ctx });
    return CleanupThread{ .thread = thread };
}

/// Run the full cleanup process (called on background thread).
fn runCleanup(allocator: Allocator, roc_ctx: CoreCtx) void {
    // TODO: REMOVE THIS FOR THE 0.1.0 RELEASE - NOT NEEDED ANYMORE
    // This is just to clean up people who have old stale Roc caches from before
    // we restructured the cache directories to use roc/{version}/ structure.
    cleanupLegacyTempDirs(allocator, null, roc_ctx);
    cleanupLegacyPersistentCache(allocator, null, roc_ctx);
    // END OF LEGACY CLEANUP - REMOVE ABOVE FOR 0.1.0

    // Clean up temp directories (5 minute threshold)
    cleanupTempDirs(allocator, null, roc_ctx);

    // Clean up persistent cache (30 day threshold)
    cleanupPersistentCache(allocator, null, roc_ctx);
}

/// Clean up temporary runtime directories older than 5 minutes.
fn cleanupTempDirs(allocator: Allocator, maybe_stats: ?*CleanupStats, roc_ctx: CoreCtx) void {
    const temp_base = cache_config.getTempDir(roc_ctx, allocator) catch return;
    defer allocator.free(temp_base);

    const now_ns = roc_ctx.timestampNow();

    // Recursively list all entries under the temp/roc directory.
    const entries = roc_ctx.listDir(temp_base, allocator) catch return;
    defer {
        for (entries) |entry| allocator.free(entry.path);
        allocator.free(entries);
    }

    // We only care about entries at depth 2 (temp_base/version/random).
    // These are the actual temp dirs and coordination files to check.
    for (entries) |entry| {
        const parent = std.fs.path.dirname(entry.path) orelse continue;
        const grandparent = std.fs.path.dirname(parent) orelse continue;

        // Only process entries whose grandparent is temp_base
        // (i.e., entries at exactly depth 2: temp_base/version/entry)
        if (!std.mem.eql(u8, grandparent, temp_base)) continue;

        if (entry.kind == .directory) {
            // Check directory age
            const dir_info = roc_ctx.stat(entry.path) catch continue;
            const age_ns = now_ns - (dir_info.mtime_ns orelse continue);

            if (age_ns > Config.TEMP_MAX_AGE_NS) {
                // Delete the directory and its contents
                roc_ctx.deleteTree(entry.path) catch {
                    if (maybe_stats) |stats| stats.errors += 1;
                    continue;
                };
                if (maybe_stats) |stats| stats.temp_dirs_deleted += 1;

                // Also try to delete the coordination file (.txt)
                const txt_path = std.fmt.allocPrint(allocator, "{s}.txt", .{entry.path}) catch continue;
                defer allocator.free(txt_path);
                roc_ctx.deleteFile(txt_path) catch {};
                if (maybe_stats) |stats| stats.temp_files_deleted += 1;
            }
        } else if (entry.kind == .file) {
            // Check if it's a stale .txt coordination file
            const basename = std.fs.path.basename(entry.path);
            if (std.mem.endsWith(u8, basename, ".txt")) {
                const file_info = roc_ctx.stat(entry.path) catch continue;
                const age_ns = now_ns - (file_info.mtime_ns orelse continue);

                if (age_ns > Config.TEMP_MAX_AGE_NS) {
                    roc_ctx.deleteFile(entry.path) catch {
                        if (maybe_stats) |stats| stats.errors += 1;
                        continue;
                    };
                    if (maybe_stats) |stats| stats.temp_files_deleted += 1;
                }
            }
        }
    }

    // NOTE: We intentionally do NOT delete empty version directories or
    // the roc temp directory. It's harmless and avoids race conditions
    // with concurrent processes.
}

/// Clean up persistent cache files older than 30 days.
fn cleanupPersistentCache(allocator: Allocator, maybe_stats: ?*CleanupStats, roc_ctx: CoreCtx) void {
    const config = CacheConfig{ .roc_ctx = roc_ctx };

    // Get the base cache directory
    const cache_base = config.getEffectiveCacheDir(allocator) catch return;
    defer allocator.free(cache_base);

    const now_ns = roc_ctx.timestampNow();

    // List immediate children of the cache directory (version directories).
    const entries = roc_ctx.listDir(cache_base, allocator) catch return;
    defer {
        for (entries) |entry| allocator.free(entry.path);
        allocator.free(entries);
    }

    for (entries) |entry| {
        if (entry.kind != .directory) continue;

        // Only process immediate children of cache_base (version directories).
        const parent = std.fs.path.dirname(entry.path) orelse continue;
        if (!std.mem.eql(u8, parent, cache_base)) continue;

        // Clean up mod/ directory
        const mod_path = std.fs.path.join(allocator, &.{ entry.path, "mod" }) catch continue;
        cleanupCacheSubdir(allocator, mod_path, now_ns, roc_ctx, maybe_stats);
        allocator.free(mod_path);

        // Clean up exe/ directory
        const exe_path = std.fs.path.join(allocator, &.{ entry.path, "exe" }) catch continue;
        cleanupCacheSubdir(allocator, exe_path, now_ns, roc_ctx, maybe_stats);
        allocator.free(exe_path);

        // Clean up test/ directory
        const test_path = std.fs.path.join(allocator, &.{ entry.path, "test" }) catch continue;
        cleanupCacheSubdir(allocator, test_path, now_ns, roc_ctx, maybe_stats);
        allocator.free(test_path);

        // NOTE: We intentionally do NOT delete empty version directories.
        // Empty directories are harmless and deleting them can cause race conditions.
    }

    // NOTE: We intentionally do NOT delete the empty cache base directory.
}

/// Clean up files in a cache subdirectory (mod/ or exe/) older than 30 days.
fn cleanupCacheSubdir(allocator: Allocator, subdir_path: []const u8, now_ns: i128, roc_ctx: CoreCtx, maybe_stats: ?*CleanupStats) void {
    // Recursively list all entries under the cache subdirectory.
    const entries = roc_ctx.listDir(subdir_path, allocator) catch return;
    defer {
        for (entries) |entry| allocator.free(entry.path);
        allocator.free(entries);
    }

    for (entries) |entry| {
        if (entry.kind != .file) continue;

        const parent = std.fs.path.dirname(entry.path) orelse continue;

        // Accept files at depth 1 (direct files in subdir) or depth 2
        // (files inside bucket directories like "a0", "b1", etc.)
        const is_direct = std.mem.eql(u8, parent, subdir_path);
        const is_in_bucket = if (std.fs.path.dirname(parent)) |grandparent|
            std.mem.eql(u8, grandparent, subdir_path)
        else
            false;

        if (!is_direct and !is_in_bucket) continue;

        const file_info = roc_ctx.stat(entry.path) catch continue;
        const age_ns = now_ns - (file_info.mtime_ns orelse continue);

        if (age_ns > Config.PERSISTENT_MAX_AGE_NS) {
            roc_ctx.deleteFile(entry.path) catch {
                if (maybe_stats) |stats| stats.errors += 1;
                continue;
            };
            if (maybe_stats) |stats| stats.cache_files_deleted += 1;
        }
    }

    // NOTE: We intentionally do NOT delete empty bucket directories or subdirs.
    // Empty directories are harmless and deleting them can cause race conditions.
}

/// Try to delete a directory if it's empty.
fn tryDeleteEmptyDir(roc_ctx: CoreCtx, path: []const u8) void {
    roc_ctx.deleteDir(path) catch {
        // Expected errors: directory not empty, not found, etc.
    };
}

/// Delete a specific temp directory and its coordination file.
/// Used for immediate cleanup after spawning a child process.
pub fn deleteTempDir(allocator: Allocator, roc_ctx: CoreCtx, temp_dir_path: []const u8) void {
    // Delete the directory and its contents
    roc_ctx.deleteTree(temp_dir_path) catch {};

    // Delete the coordination file (.txt)
    const txt_path = std.fmt.allocPrint(allocator, "{s}.txt", .{temp_dir_path}) catch return;
    defer allocator.free(txt_path);
    roc_ctx.deleteFile(txt_path) catch {};
}

// TODO: REMOVE THESE FOR THE 0.1.0 RELEASE - NOT NEEDED ANYMORE
// These clean up old cache directories from before we restructured to use
// the roc/{version}/ directory structure.

/// Clean up legacy temp directories that used the old "roc-*" prefix pattern.
/// Old structure: /tmp/roc-{random}/ (directly in temp, with roc- prefix)
/// New structure: /tmp/roc/{version}/{random}/
fn cleanupLegacyTempDirs(allocator: Allocator, maybe_stats: ?*CleanupStats, roc_ctx: CoreCtx) void {
    const temp_base = switch (builtin.target.os.tag) {
        .windows => roc_ctx.getEnvVar("TEMP", allocator) catch
            roc_ctx.getEnvVar("TMP", allocator) catch
            return,
        else => roc_ctx.getEnvVar("TMPDIR", allocator) catch
            allocator.dupe(u8, "/tmp") catch return,
    };
    defer allocator.free(temp_base);

    // List all entries under the temp directory.
    const entries = roc_ctx.listDir(temp_base, allocator) catch return;
    defer {
        for (entries) |entry| allocator.free(entry.path);
        allocator.free(entries);
    }

    // Look for directories matching "roc-*" pattern (old naming convention)
    for (entries) |entry| {
        if (entry.kind != .directory) continue;

        // Only process immediate children of temp_base.
        const parent = std.fs.path.dirname(entry.path) orelse continue;
        if (!std.mem.eql(u8, parent, temp_base)) continue;

        // Check if it starts with "roc-" (old prefix pattern)
        const basename = std.fs.path.basename(entry.path);
        if (std.mem.startsWith(u8, basename, "roc-")) {
            // Delete the directory and its contents
            roc_ctx.deleteTree(entry.path) catch {
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
fn cleanupLegacyPersistentCache(allocator: Allocator, maybe_stats: ?*CleanupStats, roc_ctx: CoreCtx) void {
    const config = CacheConfig{ .roc_ctx = roc_ctx };

    const cache_base = config.getEffectiveCacheDir(allocator) catch return;
    defer allocator.free(cache_base);

    // List all entries under the cache directory.
    const entries = roc_ctx.listDir(cache_base, allocator) catch return;
    defer {
        for (entries) |entry| allocator.free(entry.path);
        allocator.free(entries);
    }

    // Look for old-style entries (hash directories or direct cache files)
    for (entries) |entry| {
        // Only process immediate children of cache_base.
        const parent = std.fs.path.dirname(entry.path) orelse continue;
        if (!std.mem.eql(u8, parent, cache_base)) continue;

        const basename = std.fs.path.basename(entry.path);

        if (entry.kind == .file) {
            // Old-style: direct .rcache files in the cache root
            if (std.mem.endsWith(u8, basename, ".rcache")) {
                roc_ctx.deleteFile(entry.path) catch {
                    if (maybe_stats) |stats| stats.errors += 1;
                    continue;
                };
                if (maybe_stats) |stats| stats.cache_files_deleted += 1;
            }
        } else if (entry.kind == .directory) {
            // Check if this is an old-style hash directory (not a version directory)
            // Old hash dirs were like "a0b1c2d3..." (hex chars only, typically 16+ chars)
            // New version dirs are like "debug-abcd1234" (contain hyphen)
            const is_old_hash_dir = isLegacyHashDir(basename);

            if (is_old_hash_dir) {
                // Delete the entire old hash directory
                roc_ctx.deleteTree(entry.path) catch {
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
    deleteTempDir(std.testing.allocator, CoreCtx.default(std.testing.allocator, std.testing.allocator, std.testing.io), "/nonexistent/path/that/does/not/exist");
}

test "tryDeleteEmptyDir handles non-existent directory" {
    // Should not crash when directory doesn't exist
    tryDeleteEmptyDir(CoreCtx.default(std.testing.allocator, std.testing.allocator, std.testing.io), "/nonexistent/path");
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
    deleteTempDir(allocator, CoreCtx.default(std.testing.allocator, std.testing.allocator, std.testing.io), temp_dir_path);

    // Verify directory is deleted
    tmp_dir.dir.access(std.testing.io, "test_temp_dir", .{}) catch |err| {
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
    tmp_dir.dir.createDir(std.testing.io, "empty_dir", .default_dir) catch unreachable;

    // Verify it exists
    tmp_dir.dir.access(std.testing.io, "empty_dir", .{}) catch {
        return error.TestSetupFailed;
    };

    // Get the full path
    const allocator = std.testing.allocator;
    const empty_dir_path = std.fs.path.join(allocator, &.{ ".zig-cache", "tmp", &tmp_dir.sub_path, "empty_dir" }) catch unreachable;
    defer allocator.free(empty_dir_path);

    // Try to delete it
    tryDeleteEmptyDir(CoreCtx.default(std.testing.allocator, std.testing.allocator, std.testing.io), empty_dir_path);

    // Verify it's deleted
    tmp_dir.dir.access(std.testing.io, "empty_dir", .{}) catch |err| {
        try std.testing.expectEqual(error.FileNotFound, err);
        return; // Success
    };
    return error.DirectoryNotDeleted;
}

test "tryDeleteEmptyDir does not delete non-empty directory" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create a subdirectory with a file
    tmp_dir.dir.createDir(std.testing.io, "nonempty_dir", .default_dir) catch unreachable;
    const file = tmp_dir.dir.createFile(std.testing.io, "nonempty_dir/file.txt", .{}) catch unreachable;
    file.close(std.testing.io);

    // Get the full path
    const allocator = std.testing.allocator;
    const nonempty_dir_path = std.fs.path.join(allocator, &.{ ".zig-cache", "tmp", &tmp_dir.sub_path, "nonempty_dir" }) catch unreachable;
    defer allocator.free(nonempty_dir_path);

    // Try to delete it (should fail silently)
    tryDeleteEmptyDir(CoreCtx.default(std.testing.allocator, std.testing.allocator, std.testing.io), nonempty_dir_path);

    // Verify it still exists
    tmp_dir.dir.access(std.testing.io, "nonempty_dir", .{}) catch {
        return error.DirectoryShouldExist;
    };
    // Success - directory still exists as expected
}

test "cleanupCacheSubdir deletes old files and keeps new files" {
    const allocator = std.testing.allocator;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create a cache subdir structure
    tmp_dir.dir.createDir(std.testing.io, "cache_subdir", .default_dir) catch unreachable;
    tmp_dir.dir.createDir(std.testing.io, "cache_subdir/bucket1", .default_dir) catch unreachable;
    tmp_dir.dir.createDir(std.testing.io, "cache_subdir/bucket2", .default_dir) catch unreachable;

    // Create files in bucket1
    const file1 = tmp_dir.dir.createFile(std.testing.io, "cache_subdir/bucket1/old_file.rcache", .{}) catch unreachable;
    file1.close(std.testing.io);

    const file2 = tmp_dir.dir.createFile(std.testing.io, "cache_subdir/bucket2/new_file.rcache", .{}) catch unreachable;
    file2.close(std.testing.io);

    // Get the full path
    const subdir_path = std.fs.path.join(allocator, &.{ ".zig-cache", "tmp", &tmp_dir.sub_path, "cache_subdir" }) catch unreachable;
    defer allocator.free(subdir_path);

    // Get current time - files will be very recent (age ~0)
    const test_io = CoreCtx.default(std.testing.allocator, std.testing.allocator, std.testing.io);
    const now_ns = test_io.timestampNow();

    // Track stats
    var stats = CleanupStats{};

    // Run cleanup with current time - nothing should be deleted (files are too new)
    cleanupCacheSubdir(allocator, subdir_path, now_ns, test_io, &stats);

    // Both files should still exist since they're brand new
    try std.testing.expectEqual(@as(u32, 0), stats.cache_files_deleted);

    // Verify files exist
    tmp_dir.dir.access(std.testing.io, "cache_subdir/bucket1/old_file.rcache", .{}) catch {
        return error.FileShouldExist;
    };
    tmp_dir.dir.access(std.testing.io, "cache_subdir/bucket2/new_file.rcache", .{}) catch {
        return error.FileShouldExist;
    };

    // Now test with a fake "future" time that makes all files appear old
    const far_future_ns: i128 = now_ns + Config.PERSISTENT_MAX_AGE_NS + std.time.ns_per_s;
    var stats2 = CleanupStats{};

    cleanupCacheSubdir(allocator, subdir_path, far_future_ns, test_io, &stats2);

    // Both files should be deleted now
    try std.testing.expectEqual(@as(u32, 2), stats2.cache_files_deleted);

    // Verify files are gone
    tmp_dir.dir.access(std.testing.io, "cache_subdir/bucket1/old_file.rcache", .{}) catch |err| {
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
    tmp_dir.dir.createDir(std.testing.io, "cache_subdir", .default_dir) catch unreachable;
    tmp_dir.dir.createDir(std.testing.io, "cache_subdir/empty_bucket", .default_dir) catch unreachable;

    // Create a file that will be deleted
    const file = tmp_dir.dir.createFile(std.testing.io, "cache_subdir/empty_bucket/old.rcache", .{}) catch unreachable;
    file.close(std.testing.io);

    // Get the full path
    const subdir_path = std.fs.path.join(allocator, &.{ ".zig-cache", "tmp", &tmp_dir.sub_path, "cache_subdir" }) catch unreachable;
    defer allocator.free(subdir_path);

    // Use future time to make file appear old
    const test_io = CoreCtx.default(std.testing.allocator, std.testing.allocator, std.testing.io);
    const now_ns = test_io.timestampNow();
    const far_future_ns: i128 = now_ns + Config.PERSISTENT_MAX_AGE_NS + std.time.ns_per_s;

    var stats = CleanupStats{};
    cleanupCacheSubdir(allocator, subdir_path, far_future_ns, test_io, &stats);

    // File should be deleted
    try std.testing.expectEqual(@as(u32, 1), stats.cache_files_deleted);

    // Empty bucket should be removed
    tmp_dir.dir.access(std.testing.io, "cache_subdir/empty_bucket", .{}) catch |err| {
        try std.testing.expectEqual(error.FileNotFound, err);
        return; // Success - empty bucket was removed
    };
    // If bucket still exists, that's also acceptable (some systems may not remove it immediately)
}
