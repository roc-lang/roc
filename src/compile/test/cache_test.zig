//! Cache persistence, concurrent publication, and failure reporting tests.

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

const CacheWriteBarrier = struct {
    base: CoreCtx,
    mutex: std.Io.Mutex = .init,
    condition: std.Io.Condition = .init,
    writes: usize = 0,

    fn writeFile(ctx: ?*anyopaque, io: std.Io, path: []const u8, data: []const u8) CoreCtx.WriteError!void {
        const self: *CacheWriteBarrier = @ptrCast(@alignCast(ctx.?));
        try self.base.writeFile(path, data);

        // Both publishers finish staging before either can rename. A shared
        // temp filename makes the second rename fail with FileNotFound.
        self.mutex.lockUncancelable(io);
        defer self.mutex.unlock(io);
        self.writes += 1;
        if (self.writes == 2) {
            self.condition.broadcast(io);
        } else {
            while (self.writes < 2) self.condition.waitUncancelable(io, &self.mutex);
        }
    }
};

const CacheStoreTask = struct {
    manager: *CacheManager,
    directory: []const u8,
    key: [32]u8,
    data: []const u8,

    fn run(self: *CacheStoreTask) void {
        self.manager.storeRawBytes(self.key, self.data, self.directory, "Test");
    }
};

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
    const namespace = std.fs.path.basename(version_dir);
    try testing.expect(std.mem.startsWith(u8, namespace, "compat-"));
    try testing.expectEqualStrings(@import("build_options").compiler_compatibility_id, namespace["compat-".len..]);

    const test_dir = try config.getTestCacheDir(allocator);
    defer allocator.free(test_dir);

    // Should end with "/test" or "\\test"
    try testing.expect(std.mem.endsWith(u8, test_dir, "/test") or std.mem.endsWith(u8, test_dir, "\\test"));

    // Should start with the version cache dir
    try testing.expect(std.mem.startsWith(u8, test_dir, version_dir));
}

test "getScratchDir is the version cache dir's scratch subdirectory" {
    const allocator = testing.allocator;
    const config = CacheConfig{
        .cache_dir = "/home/user/.cache/roc",
        .roc_ctx = CoreCtx.testing(testing.allocator, testing.allocator),
    };

    const version_dir = try config.getVersionCacheDir(allocator);
    defer allocator.free(version_dir);

    const scratch_dir = try config.getScratchDir(allocator);
    defer allocator.free(scratch_dir);

    const expected = try std.fs.path.join(allocator, &.{ version_dir, "tmp" });
    defer allocator.free(expected);
    try testing.expectEqualStrings(expected, scratch_dir);
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
    const cache_key = @as([32]u8, @splat(0x42));

    // Store raw bytes
    manager.storeRawBytes(cache_key, test_data, tmp_path, "Test");

    // Load raw bytes back
    const loaded = manager.loadRawBytes(cache_key, tmp_path);
    try testing.expect(loaded != null);
    defer allocator.free(loaded.?);

    // Verify they match
    try testing.expectEqualStrings(test_data, loaded.?);
}

test "concurrent cache stores of one key use separate staging files" {
    const allocator = testing.allocator;
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(tmp_path);

    var barrier = CacheWriteBarrier{ .base = CoreCtx.os(std.heap.page_allocator, std.heap.page_allocator, std.testing.io) };
    var filesystem = barrier.base;
    filesystem.ctx = &barrier;
    filesystem.vtable.writeFile = &CacheWriteBarrier.writeFile;
    const config = CacheConfig{ .roc_ctx = filesystem };
    var first = CacheManager.init(std.heap.page_allocator, config, filesystem);
    var second = CacheManager.init(std.heap.page_allocator, config, filesystem);

    const key = @as([32]u8, @splat(0x51));
    const data = "same checked artifact";
    var first_task = CacheStoreTask{ .manager = &first, .directory = tmp_path, .key = key, .data = data };
    var second_task = CacheStoreTask{ .manager = &second, .directory = tmp_path, .key = key, .data = data };
    const first_thread = try std.Thread.spawn(.{}, CacheStoreTask.run, .{&first_task});
    const second_thread = try std.Thread.spawn(.{}, CacheStoreTask.run, .{&second_task});
    first_thread.join();
    second_thread.join();

    try testing.expectEqual(@as(u64, 1), first.stats.stores);
    try testing.expectEqual(@as(u64, 1), second.stats.stores);
    try testing.expectEqual(@as(u64, 0), first.stats.store_failures);
    try testing.expectEqual(@as(u64, 0), second.stats.store_failures);
    const loaded = first.loadRawBytes(key, tmp_path).?;
    defer std.heap.page_allocator.free(loaded);
    try testing.expectEqualStrings(data, loaded);
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

    const cache_key = @as([32]u8, @splat(0x24));
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

test "issue 11961: streamed cache publication preserves the old entry on write failure" {
    const Stream = struct {
        fail: bool,
        fn write(raw: *const anyopaque, writer: *std.Io.Writer) std.Io.Writer.Error!void {
            const self: *const @This() = @ptrCast(@alignCast(raw));
            try writer.writeAll("new ");
            if (self.fail) {
                try writer.flush(); // fail after staging a real partial write
                return error.WriteFailed;
            }
            try writer.writeAll("entry");
        }
        fn rejectRename(_: ?*anyopaque, _: std.Io, _: []const u8, _: []const u8) CoreCtx.RenameError!void {
            return error.AccessDenied;
        }
    };
    const gpa = testing.allocator;
    const io = testing.io;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(path);
    var capture = WarningCapture{ .allocator = gpa };
    defer capture.deinit();
    var filesystem = CoreCtx.os(gpa, gpa, io);
    filesystem.ctx = &capture;
    filesystem.vtable.writeStderr = captureStderr;
    var manager = CacheManager.init(gpa, .{ .roc_ctx = filesystem }, filesystem);
    const key = @as([32]u8, @splat(0x42));
    manager.storeRawBytes(key, "old entry", path, "Test");
    const bad = Stream{ .fail = true };
    manager.storeStream(key, .{ .context = &bad, .write = Stream.write }, 9, path, "Test");
    const old = manager.loadRawBytes(key, path).?;
    defer gpa.free(old);
    try testing.expectEqualStrings("old entry", old);
    try testing.expectEqual(@as(u64, 1), manager.stats.store_failures);

    var directory = try tmp.dir.openDir(io, "42", .{ .iterate = true });
    defer directory.close(io);
    var iterator = directory.iterate();
    var files: usize = 0;
    while (try iterator.next(io)) |_| files += 1;
    try testing.expectEqual(@as(usize, 1), files);

    const good = Stream{ .fail = false };
    const rename = manager.roc_ctx.vtable.rename;
    manager.roc_ctx.vtable.rename = Stream.rejectRename;
    manager.storeStream(key, .{ .context = &good, .write = Stream.write }, 9, path, "Test");
    manager.roc_ctx.vtable.rename = rename;
    try testing.expectEqual(@as(u64, 2), manager.stats.store_failures);
    const preserved = manager.loadRawBytes(key, path).?;
    defer gpa.free(preserved);
    try testing.expectEqualStrings("old entry", preserved);
    iterator = directory.iterate();
    files = 0;
    while (try iterator.next(io)) |_| files += 1;
    try testing.expectEqual(@as(usize, 1), files);

    manager.storeStream(key, .{ .context = &good, .write = Stream.write }, 9, path, "Test");
    const current = manager.loadRawBytes(key, path).?;
    defer gpa.free(current);
    try testing.expectEqualStrings("new entry", current);
}

const FailingCacheFilesystem = struct {
    capture: WarningCapture,
    stage: enum { directory, write, rename },

    fn makePath(ctx: ?*anyopaque, _: std.Io, _: []const u8) CoreCtx.MakePathError!void {
        const self: *FailingCacheFilesystem = @ptrCast(@alignCast(ctx.?));
        if (self.stage == .directory) return error.ReadOnlyFileSystem;
    }

    fn writeFile(ctx: ?*anyopaque, _: std.Io, _: []const u8, _: []const u8) CoreCtx.WriteError!void {
        const self: *FailingCacheFilesystem = @ptrCast(@alignCast(ctx.?));
        if (self.stage == .write) return error.FileTooBig;
    }

    fn rename(ctx: ?*anyopaque, _: std.Io, _: []const u8, _: []const u8) CoreCtx.RenameError!void {
        const self: *FailingCacheFilesystem = @ptrCast(@alignCast(ctx.?));
        if (self.stage == .rename) return error.DiskQuota;
    }

    fn deleteFile(_: ?*anyopaque, _: std.Io, _: []const u8) CoreCtx.DeleteError!void {
        return error.AccessDenied;
    }

    fn writeStderr(ctx: ?*anyopaque, _: std.Io, bytes: []const u8) CoreCtx.StdioError!void {
        const self: *FailingCacheFilesystem = @ptrCast(@alignCast(ctx.?));
        self.capture.stderr.appendSlice(self.capture.allocator, bytes) catch return error.IoError;
    }
};

test "cache write failures report every operation only in verbose mode" {
    const allocator = testing.allocator;
    const warning = "warning: Roc cache writes are failing; compilation will continue without updating the cache. Run with --verbose for details.\n";
    // Long diagnostic context must not silently discard a failure report.
    const source_name = comptime blk: {
        const piece = "LongModuleName";
        var buffer: [8 + piece.len * 100]u8 = undefined;
        @memcpy(buffer[0..8], "package.");
        for (0..100) |index| @memcpy(buffer[8 + index * piece.len ..][0..piece.len], piece);
        const final = buffer;
        break :blk &final;
    };
    const key = @as([32]u8, @splat(0x42));
    const expected_path = try CacheManager.computeCacheFilePathIn(allocator, key, "cache");
    defer allocator.free(expected_path);
    for ([_]bool{ false, true }) |verbose| {
        for (std.enums.values(@FieldType(FailingCacheFilesystem, "stage"))) |stage| {
            var capture = FailingCacheFilesystem{
                .capture = .{ .allocator = allocator },
                .stage = stage,
            };
            defer capture.capture.deinit();
            var filesystem = CoreCtx.testing(allocator, allocator);
            filesystem.std_io = testing.io;
            filesystem.ctx = &capture;
            filesystem.vtable.makePath = &FailingCacheFilesystem.makePath;
            filesystem.vtable.writeFile = &FailingCacheFilesystem.writeFile;
            filesystem.vtable.rename = &FailingCacheFilesystem.rename;
            filesystem.vtable.deleteFile = &FailingCacheFilesystem.deleteFile;
            filesystem.vtable.writeStderr = &FailingCacheFilesystem.writeStderr;
            var manager = CacheManager.init(allocator, .{ .verbose = verbose }, filesystem);
            for ([_]CacheManager.Kind{ .checked, .canonicalized }) |kind| {
                manager.storeRawBytesIn(allocator, kind, key, "cache data", "cache", source_name);
            }
            const output = capture.capture.stderr.items;
            try testing.expectEqual(@as(u64, 1), manager.stats.store_failures);
            try testing.expectEqual(@as(u64, 1), manager.stats.canonicalized_store_failures);
            if (!verbose) {
                try testing.expectEqualStrings(warning, output);
                continue;
            }
            const cause = switch (stage) {
                .directory => "error.ReadOnlyFileSystem",
                .write => "error.FileTooBig",
                .rename => "error.DiskQuota",
            };
            try testing.expectEqual(@as(usize, 2), std.mem.count(u8, output, cause));
            try testing.expectEqual(@as(usize, 2), std.mem.count(u8, output, source_name));
            try testing.expectEqual(@as(usize, 1), std.mem.count(u8, output, warning));
            try testing.expect(std.mem.containsAtLeast(u8, output, 1, "checked cache"));
            try testing.expect(std.mem.containsAtLeast(u8, output, 1, "canonicalized cache"));
            if (stage == .directory) {
                try testing.expectEqual(@as(usize, 2), std.mem.count(u8, output, "in cache for "));
            } else {
                try testing.expect(std.mem.containsAtLeast(u8, output, 2, expected_path));
                try testing.expectEqual(@as(usize, 2), std.mem.count(u8, output, "(10 bytes)"));
                try testing.expectEqual(@as(usize, 2), std.mem.count(u8, output, "Failed to remove cache temp file"));
            }
        }
    }
}
