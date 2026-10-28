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

test "storeRawBytes and loadRawBytes round-trip" {
    const allocator = testing.allocator;

    // Create a real temporary directory for testing
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const tmp_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(tmp_path);

    const filesystem = CoreCtx.os(std.testing.allocator, std.testing.allocator, std.testing.io);
    const config = CacheConfig{ .cache_dir = tmp_path, .roc_ctx = filesystem };

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

    const filesystem = CoreCtx.os(std.heap.page_allocator, std.heap.page_allocator, std.testing.io);
    // Verbose output names the failing step and error if a store fails.
    const config = CacheConfig{ .cache_dir = tmp_path, .roc_ctx = filesystem, .verbose = true };
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

    try testing.expect(first.stats.stores + second.stats.stores >= 1);
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
    const config = CacheConfig{ .cache_dir = tmp_path, .roc_ctx = filesystem };

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
    var manager = CacheManager.init(gpa, .{ .cache_dir = path, .roc_ctx = filesystem }, filesystem);
    const key = @as([32]u8, @splat(0x42));
    manager.storeRawBytes(key, "old entry", path, "Test");
    const bad = Stream{ .fail = true };
    manager.storeStream(key, .{ .context = &bad, .write = Stream.write }, 9, path, "Test");
    const old = manager.loadRawBytes(key, path).?;
    defer gpa.free(old);
    try testing.expectEqualStrings("old entry", old);
    try testing.expectEqual(@as(u64, 1), manager.stats.store_failures);

    const good = Stream{ .fail = false };
    manager.storeStream(key, .{ .context = &good, .write = Stream.write }, 9, path, "Test");
    const current = manager.loadRawBytes(key, path).?;
    defer gpa.free(current);
    try testing.expectEqualStrings("new entry", current);
}

const FailingCacheFilesystem = struct {
    capture: WarningCapture,
    base: CoreCtx,
    stage: enum { directory, write, publish },

    fn writeFile(ctx: ?*anyopaque, _: std.Io, path: []const u8, bytes: []const u8) CoreCtx.WriteError!void {
        const self: *@This() = @ptrCast(@alignCast(ctx.?));
        if (self.stage == .write) return error.FileTooBig;
        try self.base.writeFile(path, bytes);
        if (self.stage == .publish) {
            const blocked = std.fmt.allocPrint(self.capture.allocator, "{s}.data", .{path[0 .. path.len - ".pending".len]}) catch return error.IoError;
            defer self.capture.allocator.free(blocked);
            self.base.makePath(blocked) catch return error.IoError;
        }
    }

    fn writeStderr(ctx: ?*anyopaque, _: std.Io, bytes: []const u8) CoreCtx.StdioError!void {
        const self: *@This() = @ptrCast(@alignCast(ctx.?));
        self.capture.stderr.appendSlice(self.capture.allocator, bytes) catch return error.IoError;
    }
};

test "cache write failures report every operation only in verbose mode" {
    const allocator = testing.allocator;
    const warning = "warning: Roc cache writes are failing; compilation will continue without updating the cache. Run with --verbose for details.\n";
    const key: [32]u8 = @splat(0x42);
    for ([_]bool{ false, true }) |verbose| {
        for (std.enums.values(@FieldType(FailingCacheFilesystem, "stage"))) |stage| {
            var tmp = testing.tmpDir(.{});
            defer tmp.cleanup();
            const root = try tmp.dir.realPathFileAlloc(testing.io, ".", allocator);
            defer allocator.free(root);
            if (stage == .directory) try tmp.dir.writeFile(testing.io, .{ .sub_path = @import("../artifact_store.zig").format_directory, .data = "blocked directory" });
            var capture = FailingCacheFilesystem{ .capture = .{ .allocator = allocator }, .base = CoreCtx.os(allocator, allocator, testing.io), .stage = stage };
            defer capture.capture.deinit();
            var filesystem = capture.base;
            filesystem.ctx = &capture;
            filesystem.vtable.writeFile = &FailingCacheFilesystem.writeFile;
            filesystem.vtable.writeStderr = &FailingCacheFilesystem.writeStderr;
            var manager = CacheManager.init(allocator, .{ .cache_dir = root, .verbose = verbose }, filesystem);
            for ([_]CacheManager.Kind{ .checked, .canonicalized }) |kind| manager.storeRawBytesIn(allocator, kind, key, "cache data", "scope", "package.Module");
            const output = capture.capture.stderr.items;
            try testing.expectEqual(@as(u64, 1), manager.stats.store_failures);
            try testing.expectEqual(@as(u64, 1), manager.stats.canonicalized_store_failures);
            try testing.expectEqual(@as(usize, 1), std.mem.count(u8, output, warning));
            if (!verbose) {
                try testing.expectEqualStrings(warning, output);
            } else {
                try testing.expectEqual(@as(usize, 2), std.mem.count(u8, output, if (stage == .write) "error.FileTooBig" else "error.IoFailure"));
                try testing.expectEqual(@as(usize, 2), std.mem.count(u8, output, "package.Module"));
                try testing.expect(std.mem.containsAtLeast(u8, output, 1, "checked cache"));
                try testing.expect(std.mem.containsAtLeast(u8, output, 1, "canonicalized cache"));
            }
        }
    }
}
