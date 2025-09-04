//! Test file for cache functionality (currently commented out/disabled).

const std = @import("std");
const Can = @import("can");
const fs_mod = @import("fs");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");
const collections = @import("collections");
const reporting = @import("reporting");

const SERIALIZATION_ALIGNMENT = collections.SERIALIZATION_ALIGNMENT;

const DataSizeUnit = @import("../cache_reporting.zig").DataSizeUnit;
const CacheManager = @import("../cache_manager.zig").CacheManager;
const CacheConfig = @import("../cache_config.zig").CacheConfig;
const CacheModule = @import("../cache_module.zig").CacheModule;
const CacheStats = @import("../cache_config.zig").CacheStats;
const Filesystem = fs_mod.Filesystem;
const ModuleEnv = can.ModuleEnv;
const Severity = reporting.Severity;
const CacheKey = @import("../cache_key.zig").CacheKey;
const SExprTree = base.SExprTree;
const Header = @import("../cache_module.zig").Header;
const testing = std.testing;
const formatDataSize = @import("../cache_reporting.zig").formatDataSize;

test "cache module" {
    // Basic test to ensure module compiles and types are accessible
    const allocator = std.testing.allocator;

    // Test stats functionality
    var stats = CacheStats{};
    stats.hits = 10;
    stats.misses = 5;
    try std.testing.expectEqual(@as(f64, 2.0 / 3.0), stats.hitRate());

    // Test config constants
    const Constants = @import("../cache_config.zig").Constants;
    try std.testing.expect(std.mem.eql(u8, Constants.DEFAULT_CACHE_DIR, ".roc_cache"));
    try std.testing.expect(std.mem.eql(u8, Constants.CACHE_FILE_EXT, ".rcache"));

    _ = allocator; // Suppress unused variable warning
}

test "CacheConfig default values" {
    const config = CacheConfig{};

    try testing.expect(config.enabled == true);
    try testing.expect(config.cache_dir == null);
    try testing.expect(config.max_size_mb == 1024);
    try testing.expect(config.max_age_days == 30);
    try testing.expect(config.verbose == false);
}

test "CacheConfig getMaxSizeBytes" {
    const config = CacheConfig{ .max_size_mb = 100 };

    try testing.expectEqual(@as(u64, 100 * 1024 * 1024), config.getMaxSizeBytes());
}

test "CacheConfig getMaxAgeNanos" {
    const config = CacheConfig{ .max_age_days = 7 };

    const expected = @as(i64, 7) * 24 * 60 * 60 * 1_000_000_000;
    try testing.expectEqual(expected, config.getMaxAgeNanos());
}

test "CacheConfig getEffectiveCacheDir with explicit dir" {
    const allocator = testing.allocator;
    const config = CacheConfig{ .cache_dir = "/custom/cache" };

    const dir = try config.getEffectiveCacheDir(allocator);
    defer allocator.free(dir);

    try testing.expectEqualStrings("/custom/cache", dir);
}

test "getCacheDirName platform specific" {
    const name = @import("../cache_config.zig").getCacheDirName();

    // Should be "roc" or "Roc" depending on platform
    try testing.expect(std.mem.eql(u8, name, "roc") or std.mem.eql(u8, name, "Roc"));
}

test "getCompilerVersionDir" {
    const allocator = testing.allocator;

    const version_dir = try @import("../cache_config.zig").getCompilerVersionDir(allocator);
    defer allocator.free(version_dir);

    // Should be 32 hex characters
    try testing.expectEqual(@as(usize, 32), version_dir.len);

    // Should only contain hex characters
    for (version_dir) |char| {
        try testing.expect(std.ascii.isHex(char));
    }
}

test "CacheStats basic operations" {
    var stats = CacheStats{};

    // Record some operations
    stats.recordHit(1024); // 1KB read
    stats.recordMiss();
    stats.recordStore(2048); // 2KB written

    try testing.expectEqual(@as(u64, 1), stats.hits);
    try testing.expectEqual(@as(u64, 1), stats.misses);
    try testing.expectEqual(@as(u64, 1), stats.stores);
    try testing.expectEqual(@as(u64, 2), stats.getTotalOps());
    try testing.expectEqual(@as(f64, 50.0), stats.getHitRate());
}

test "CacheStats hit rate calculation" {
    var stats = CacheStats{};

    // No operations - should be 0%
    try testing.expectEqual(@as(f64, 0.0), stats.getHitRate());

    // 3 hits, 1 miss = 75%
    stats.recordHit(100);
    stats.recordHit(200);
    stats.recordHit(300);
    stats.recordMiss();

    try testing.expectEqual(@as(f64, 75.0), stats.getHitRate());
}

test "CacheKey generation" {
    const allocator = testing.allocator;

    // Mock filesystem for testing
    const fs = Filesystem.testing();

    const source1 = "module [foo]\n\nfoo = 42";
    const source2 = "module [bar]\n\nbar = 24";

    var key1 = try CacheKey.generate(source1, "test1.roc", fs, allocator);
    defer key1.deinit(allocator);
    var key2 = try CacheKey.generate(source2, "test2.roc", fs, allocator);
    defer key2.deinit(allocator);
    var key1_again = try CacheKey.generate(source1, "test1.roc", fs, allocator);
    defer key1_again.deinit(allocator);

    // Different sources should produce different keys
    try testing.expect(!key1.eql(key2));

    // Same source should produce same key
    try testing.expect(key1.eql(key1_again));
}

test "CacheKey to filename conversion" {
    const allocator = testing.allocator;

    const fs = Filesystem.testing();

    const source = "module [test]\n\ntest = 123";
    var key = try CacheKey.generate(source, "test.roc", fs, allocator);
    defer key.deinit(allocator);

    const filename = try key.toCacheFileName(allocator);
    defer allocator.free(filename);

    // Should be a hex string
    try testing.expect(filename.len == 64); // SHA-256 hex = 64 chars

    // Should only contain hex characters
    for (filename) |char| {
        try testing.expect(std.ascii.isHex(char));
    }
}

test "CacheKey equality" {
    const allocator = testing.allocator;

    const fs = Filesystem.testing();

    const source = "module [test]\n\ntest = 456";
    var key1 = try CacheKey.generate(source, "test.roc", fs, allocator);
    defer key1.deinit(allocator);
    var key2 = try CacheKey.generate(source, "test.roc", fs, allocator);
    defer key2.deinit(allocator);

    try testing.expect(key1.eql(key2));

    // Different content should produce different keys
    const different_source = "module [test]\n\ntest = 789";
    var key3 = try CacheKey.generate(different_source, "test.roc", fs, allocator);
    defer key3.deinit(allocator);

    try testing.expect(!key1.eql(key3));
}

test "CacheKey format" {
    const allocator = testing.allocator;

    const fs = Filesystem.testing();

    const source = "module [format_test]\n\nformat_test = 1";
    var key = try CacheKey.generate(source, "format_test.roc", fs, allocator);
    defer key.deinit(allocator);

    var buffer: [256]u8 = undefined;
    const formatted = try std.fmt.bufPrint(&buffer, "{}", .{key});

    // Should contain expected format elements
    try testing.expect(std.mem.containsAtLeast(u8, formatted, 1, "CacheKey"));
    try testing.expect(std.mem.containsAtLeast(u8, formatted, 1, "content"));
    try testing.expect(std.mem.containsAtLeast(u8, formatted, 1, "mtime"));
    try testing.expect(std.mem.containsAtLeast(u8, formatted, 1, "compiler"));
}

test "CacheManager initialization" {
    const allocator = testing.allocator;
    const config = CacheConfig{};
    const filesystem = Filesystem.testing();

    var manager = CacheManager.init(allocator, config, filesystem);

    try testing.expect(manager.config.enabled == true);
    try testing.expect(manager.stats.getTotalOps() == 0);
}

test "CacheManager generateCacheKey" {
    const content = "module [test]\n\ntest = 42";
    const compiler_version = "roc-zig-0.11.0-debug";

    const key1 = CacheManager.generateCacheKey(content, compiler_version);
    const key2 = CacheManager.generateCacheKey(content, compiler_version);

    // Same content should produce same key
    try testing.expectEqualSlices(u8, &key1, &key2);

    // Should be 32 bytes
    try testing.expectEqual(@as(usize, 32), key1.len);
}

test "CacheManager getCacheFilePath with subdirectory splitting" {
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
    const cache_path = try manager.getCacheFilePath(cache_key);
    defer allocator.free(cache_path);

    // Should contain subdirectory split
    try testing.expect(std.mem.containsAtLeast(u8, cache_path, 1, "ab"));
    try testing.expect(std.mem.containsAtLeast(u8, cache_path, 1, "cdef123456789"));
}

test "CacheManager loadFromCache miss" {
    const allocator = testing.allocator;
    const config = CacheConfig{};
    var filesystem = Filesystem.testing();

    // Mock fileExists to return false
    const TestFS = struct {
        fn fileExists(path: []const u8) Filesystem.OpenError!bool {
            _ = path;
            return false;
        }
    };
    filesystem.fileExists = TestFS.fileExists;

    var manager = CacheManager.init(allocator, config, filesystem);

    const source = "module [test]\n\ntest = 42";

    const result = manager.loadFromCache("roc-zig-0.11.0-debug", source, "test");
    switch (result) {
        .miss => |_| {
            try testing.expect(manager.stats.misses == 1);
        },
        else => return error.TestUnexpectedResult,
    }
}

test "CacheManager disabled" {
    const allocator = testing.allocator;
    const config = CacheConfig{ .enabled = false };
    const filesystem = Filesystem.testing();

    var manager = CacheManager.init(allocator, config, filesystem);

    const source = "module [test]\n\ntest = 42";

    const result = manager.loadFromCache("roc-zig-0.11.0-debug", source, "test");
    switch (result) {
        .not_enabled => |_| {},
        else => return error.TestUnexpectedResult,
    }
    try testing.expect(manager.stats.getTotalOps() == 0); // No stats recorded when disabled
}

test "Header alignment" {
    // Verify the header is properly aligned
    try testing.expect(@sizeOf(Header) % SERIALIZATION_ALIGNMENT == 0);
}

test "create and restore cache - SKIPPED" {
    // Skipped: needs CIR reimplementation - Can.canonicalizeFile() no longer exists
    // The test code has been removed to avoid unreachable code errors
    // Original test created a cache, serialized it, and restored it
    return error.SkipZigTest;
}

test "formatDataSize bytes" {
    const result = @import("../cache_reporting.zig").formatDataSize(512);
    try testing.expectEqual(@as(f64, 512.0), result.value);
    try testing.expectEqual(DataSizeUnit.bytes, result.unit);
}

test "formatDataSize KB" {
    const result = @import("../cache_reporting.zig").formatDataSize(1536); // 1.5 KB
    try testing.expectApproxEqRel(@as(f64, 1.5), result.value, 0.01);
    try testing.expectEqual(DataSizeUnit.kb, result.unit);
}

test "formatDataSize MB" {
    const result = @import("../cache_reporting.zig").formatDataSize(2 * 1024 * 1024); // 2 MB
    try testing.expectApproxEqRel(@as(f64, 2.0), result.value, 0.01);
    try testing.expectEqual(DataSizeUnit.mb, result.unit);
}

test "formatDataSize GB" {
    const result = @import("../cache_reporting.zig").formatDataSize(3 * 1024 * 1024 * 1024); // 3 GB
    try testing.expectApproxEqRel(@as(f64, 3.0), result.value, 0.01);
    try testing.expectEqual(DataSizeUnit.gb, result.unit);
}

test "getUnitString" {
    try testing.expectEqualStrings("B", @import("../cache_reporting.zig").getUnitString(.bytes));
    try testing.expectEqualStrings("KB", @import("../cache_reporting.zig").getUnitString(.kb));
    try testing.expectEqualStrings("MB", @import("../cache_reporting.zig").getUnitString(.mb));
    try testing.expectEqualStrings("GB", @import("../cache_reporting.zig").getUnitString(.gb));
}

test "createCacheStatsReport empty stats" {
    const allocator = testing.allocator;
    const stats = CacheStats{};

    var report = try @import("../cache_reporting.zig").createCacheStatsReport(allocator, stats);
    defer report.deinit();

    try testing.expectEqualStrings("CACHE STATISTICS", report.title);
    try testing.expectEqual(Severity.info, report.severity);
}

test "createCacheStatsReport with operations" {
    const allocator = testing.allocator;
    var stats = CacheStats{};
    stats.recordHit(1024); // 1KB read
    stats.recordMiss();
    stats.recordStore(2048); // 2KB written

    var report = try @import("../cache_reporting.zig").createCacheStatsReport(allocator, stats);
    defer report.deinit();

    try testing.expectEqualStrings("CACHE STATISTICS", report.title);
    try testing.expectEqual(Severity.info, report.severity);

    // The report should contain information about operations
    // More detailed testing would require inspecting the document structure
    try testing.expectEqual(@as(u64, 1), stats.hits);
    try testing.expectEqual(@as(u64, 1), stats.misses);
    try testing.expectEqual(@as(u64, 1), stats.stores);
}
