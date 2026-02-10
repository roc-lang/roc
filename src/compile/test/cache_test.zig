const std = @import("std");
const fs_mod = @import("fs");

const CacheManager = @import("../cache_manager.zig").CacheManager;
const CacheConfig = @import("../cache_config.zig").CacheConfig;
const CacheStats = @import("../cache_config.zig").CacheStats;
const Filesystem = fs_mod.Filesystem;
const testing = std.testing;

test "getTestCacheDir returns test subdirectory" {
    const allocator = testing.allocator;
    const config = CacheConfig{};

    const version_dir = try config.getVersionCacheDir(allocator);
    defer allocator.free(version_dir);

    const test_dir = try config.getTestCacheDir(allocator);
    defer allocator.free(test_dir);

    // Should end with "/test" or "\\test"
    try testing.expect(std.mem.endsWith(u8, test_dir, "/test") or std.mem.endsWith(u8, test_dir, "\\test"));

    // Should start with the version cache dir
    try testing.expect(std.mem.startsWith(u8, test_dir, version_dir));
}

test "computeCacheFilePath uses subdirectory splitting" {
    const allocator = testing.allocator;
    const config = CacheConfig{};
    const filesystem = Filesystem.testing();

    var manager = CacheManager.init(allocator, config, filesystem);

    const cache_key = [_]u8{
        0xab, 0xcd, 0xef, 0x12, 0x34, 0x56, 0x78, 0x90,
        0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88,
        0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00,
        0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef,
    };

    // Test with a custom entries dir
    const path = try manager.computeCacheFilePath(cache_key, "/tmp/test_cache");
    defer allocator.free(path);

    // Should contain the subdirectory split: first byte "ab" as subdir
    try testing.expect(std.mem.containsAtLeast(u8, path, 1, "ab"));
    // Path should start with our test dir
    try testing.expect(std.mem.startsWith(u8, path, "/tmp/test_cache"));
}

test "storeRawBytes and loadRawBytes round-trip" {
    const allocator = testing.allocator;

    // Create a real temporary directory for testing
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const tmp_path = try tmp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);

    const config = CacheConfig{};
    const filesystem = Filesystem.default();

    var manager = CacheManager.init(allocator, config, filesystem);

    // Generate a cache key
    const test_data = "Hello, test cache!";
    const cache_key = CacheManager.generateCacheKey("test_source", "test_version");

    // Store raw bytes
    manager.storeRawBytes(cache_key, test_data, tmp_path);

    // Load raw bytes back
    const loaded = manager.loadRawBytes(cache_key, tmp_path);
    try testing.expect(loaded != null);
    defer allocator.free(loaded.?);

    // Verify they match
    try testing.expectEqualStrings(test_data, loaded.?);
}

test "loadRawBytes returns null on miss" {
    const allocator = testing.allocator;

    // Create a real temporary directory for testing
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const tmp_path = try tmp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(tmp_path);

    const config = CacheConfig{};
    const filesystem = Filesystem.default();

    var manager = CacheManager.init(allocator, config, filesystem);

    // Try to load a key that was never stored
    const cache_key = CacheManager.generateCacheKey("nonexistent_source", "nonexistent_version");
    const loaded = manager.loadRawBytes(cache_key, tmp_path);

    // Should return null
    try testing.expect(loaded == null);

    // Stats should record a miss
    try testing.expectEqual(@as(u64, 1), manager.stats.misses);
}
