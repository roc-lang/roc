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
    defer manager.deinit();

    const test_data = "Hello, test cache!";
    const cache_key = @as([32]u8, @splat(0x42));

    // Store raw bytes
    manager.storeRawBytes(cache_key, test_data, tmp_path, "Test");

    // Load raw bytes back
    const loaded = manager.loadRawBytes(cache_key, tmp_path);
    try testing.expect(loaded != null);
    defer allocator.free(loaded.?.bytes);

    // Verify they match
    try testing.expectEqualStrings(test_data, loaded.?.bytes);
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
    defer first.deinit();
    var second = CacheManager.init(std.heap.page_allocator, config, filesystem);
    defer second.deinit();

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
    const loaded = first.loadRawBytes(key, tmp_path).?.bytes;
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
    defer manager.deinit();

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
    defer manager.deinit();
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
    defer manager.deinit();
    const key = @as([32]u8, @splat(0x42));
    manager.storeRawBytes(key, "old entry", path, "Test");
    const bad = Stream{ .fail = true };
    manager.storeStream(key, .{ .context = &bad, .write = Stream.write }, 9, path, "Test");
    const old = manager.loadRawBytes(key, path).?.bytes;
    defer gpa.free(old);
    try testing.expectEqualStrings("old entry", old);
    try testing.expectEqual(@as(u64, 1), manager.stats.store_failures);

    const good = Stream{ .fail = false };
    manager.storeStream(key, .{ .context = &good, .write = Stream.write }, 9, path, "Test");
    const current = manager.loadRawBytes(key, path).?.bytes;
    defer gpa.free(current);
    try testing.expectEqualStrings("new entry", current);
}

const FailingCacheFilesystem = struct {
    capture: WarningCapture,
    base: CoreCtx,
    stage: enum { directory, write, publish },

    fn writeFileStream(ctx: ?*anyopaque, _: std.Io, path: []const u8, stream: CoreCtx.FileStream) CoreCtx.WriteError!void {
        const self: *@This() = @ptrCast(@alignCast(ctx.?));
        if (self.stage == .write) return error.FileTooBig;
        try self.base.writeFileStream(path, stream);
        if (self.stage == .publish) {
            const end = (std.mem.findLast(u8, path, ".data.") orelse return error.IoError) + ".data".len;
            const blocked = path[0..end];
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
            filesystem.vtable.writeFileStream = &FailingCacheFilesystem.writeFileStream;
            filesystem.vtable.writeStderr = &FailingCacheFilesystem.writeStderr;
            var manager = CacheManager.init(allocator, .{ .cache_dir = root, .verbose = verbose }, filesystem);
            defer manager.deinit();
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

test "a file whose recorded length is not its length is discarded, counted and logged" {
    const gpa = testing.allocator;
    const io = std.testing.io;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(path);
    var capture = WarningCapture{ .allocator = gpa };
    defer capture.deinit();
    var filesystem = CoreCtx.os(gpa, gpa, io);
    filesystem.ctx = &capture;
    filesystem.vtable.writeStderr = captureStderr;
    var manager = CacheManager.init(gpa, .{ .cache_dir = path, .roc_ctx = filesystem, .verbose = true }, filesystem);
    defer manager.deinit();
    var store = try manager.openStore(gpa);
    defer store.deinit();

    const Damage = union(enum) { truncated: usize, extra_byte, wrong_stored_length, zeroed_trailer, trailer_disagrees };
    const damages = [_]Damage{
        .{ .truncated = 1 }, .{ .truncated = 2 },  .{ .truncated = 3 }, .{ .truncated = 4 },
        .{ .truncated = 5 }, .{ .truncated = 6 },  .{ .truncated = 7 }, .{ .truncated = 8 },
        .extra_byte,         .wrong_stored_length, .zeroed_trailer,     .trailer_disagrees,
    };
    for (damages, 0..) |damage, i| {
        const key = @as([32]u8, @splat(@intCast(0x40 + i)));
        manager.storeRawBytes(key, "a whole entry", path, "Test");
        const entry_path = try store.entryPath(gpa, CacheManager.identity(.checked, key, path));
        defer gpa.free(entry_path);
        const file = try filesystem.readFile(entry_path, gpa);
        defer gpa.free(file);
        const damaged: []const u8 = switch (damage) {
            .truncated => |cut| file[0 .. file.len - cut],
            .extra_byte => try std.mem.concat(gpa, u8, &.{ file, "!" }),
            .wrong_stored_length => blk: {
                std.mem.writeInt(u64, file[0..8], file.len + 7, .little);
                break :blk file;
            },
            .zeroed_trailer => blk: {
                @memset(file[file.len - 8 ..], 0);
                break :blk file;
            },
            .trailer_disagrees => blk: {
                std.mem.writeInt(u64, file[file.len - 8 ..][0..8], file.len - 1, .little);
                break :blk file;
            },
        };
        defer if (damage == .extra_byte) gpa.free(damaged);

        // Read and mapped loads check the same framing.
        for ([_]bool{ false, true }) |mapped| {
            try filesystem.writeFile(entry_path, damaged);
            const invalidations = manager.stats.invalidations;
            if (mapped) {
                try testing.expect(manager.loadRawBytesMapped(key, path) == null);
            } else {
                try testing.expect(manager.loadRawBytes(key, path) == null);
            }
            try testing.expectEqual(invalidations + 1, manager.stats.invalidations);
            try testing.expect(!filesystem.fileExists(entry_path));
        }
    }
    try testing.expect(std.mem.find(u8, capture.stderr.items, "Discarding corrupt checked cache entry") != null);

    // An intact file loads.
    const key = @as([32]u8, @splat(0x70));
    manager.storeRawBytes(key, "a whole entry", path, "Test");
    const loaded = manager.loadRawBytes(key, path).?;
    defer gpa.free(loaded.bytes);
    try testing.expectEqualStrings("a whole entry", loaded.bytes);
    const mapped = manager.loadRawBytesMapped(key, path).?;
    defer mapped.deinit(gpa);
    try testing.expectEqualStrings("a whole entry", mapped.data());
}
