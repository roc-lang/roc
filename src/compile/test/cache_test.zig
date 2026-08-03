const std = @import("std");
const ctx_mod = @import("ctx");

const CacheManager = @import("../cache_manager.zig").CacheManager;
const CacheConfig = @import("../cache_config.zig").CacheConfig;
const CoreCtx = ctx_mod.CoreCtx;
const testing = std.testing;

const WarningCapture = struct {
    allocator: std.mem.Allocator,
    stderr: std.ArrayList(u8) = .empty,

    fn deinit(self: *WarningCapture) void {
        self.stderr.deinit(self.allocator);
    }
};

fn captureStderr(ctx: ?*anyopaque, _: std.Io, bytes: []const u8) CoreCtx.StdioError!void {
    const capture: *WarningCapture = @ptrCast(@alignCast(ctx.?));
    capture.stderr.appendSlice(capture.allocator, bytes) catch return error.IoError;
}

test "getTestCacheDir returns test subdirectory" {
    const allocator = testing.allocator;
    // Use an explicit cache_dir so the test does not depend on HOME/XDG env vars
    // (the default testing CoreCtx returns EnvironmentVariableMissing for all vars).
    const config = CacheConfig{
        .cache_dir = "/tmp/roc_test_cache",
        .roc_ctx = CoreCtx.testing(testing.allocator, testing.allocator),
    };

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
    const filesystem = CoreCtx.testing(std.testing.allocator, std.testing.allocator);
    const config = CacheConfig{ .roc_ctx = filesystem };

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

    const tmp_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(tmp_path);

    const filesystem = CoreCtx.os(std.testing.allocator, std.testing.allocator, std.testing.io);
    const config = CacheConfig{ .roc_ctx = filesystem };

    var manager = CacheManager.init(allocator, config, filesystem);

    const test_data = "Hello, test cache!";
    const cache_key = [_]u8{0x42} ** 32;

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

    const tmp_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(tmp_path);

    const filesystem = CoreCtx.os(std.testing.allocator, std.testing.allocator, std.testing.io);
    const config = CacheConfig{ .roc_ctx = filesystem };

    var manager = CacheManager.init(allocator, config, filesystem);

    const cache_key = [_]u8{0x24} ** 32;
    const loaded = manager.loadRawBytes(cache_key, tmp_path);

    // Should return null
    try testing.expect(loaded == null);

    // Stats should record a miss
    try testing.expectEqual(@as(u64, 1), manager.stats.misses);
}

test "recordStoreFailure prints non-verbose warning once" {
    const allocator = testing.allocator;

    var capture = WarningCapture{ .allocator = allocator };
    defer capture.deinit();

    var filesystem = CoreCtx.testing(allocator, allocator);
    filesystem.ctx = &capture;
    filesystem.vtable.writeStderr = &captureStderr;

    var manager = CacheManager.init(allocator, .{ .roc_ctx = filesystem }, filesystem);
    manager.recordStoreFailure();
    manager.recordStoreFailure();

    try testing.expectEqual(@as(u64, 2), manager.stats.store_failures);
    try testing.expectEqual(@as(usize, 1), std.mem.count(u8, capture.stderr.items, "Roc cache writes are failing"));
}
