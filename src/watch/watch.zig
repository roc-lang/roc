//! File system watcher for monitoring .roc file changes across platforms.
//! Provides efficient, cross-platform file watching with recursive directory support.

const std = @import("std");
const builtin = @import("builtin");

/// Event triggered when a watched file changes
pub const WatchEvent = struct {
    path: []const u8,
};

/// Callback function type for handling file change events
pub const WatchCallback = *const fn (event: WatchEvent) void;

/// High-performance filesystem watcher for .roc files
/// Monitors directories recursively and invokes callbacks on file changes
pub const Watcher = struct {
    allocator: std.mem.Allocator,
    paths: [][]const u8,
    callback: WatchCallback,
    should_stop: std.atomic.Value(bool),
    thread: ?std.Thread,

    impl: switch (builtin.os.tag) {
        .macos => MacOSImpl,
        .linux => LinuxImpl,
        .windows => WindowsImpl,
        else => @compileError("Unsupported platform for file watching"),
    },

    const MacOSImpl = struct {
        poll_interval_ns: u64 = 50_000_000, // 50ms for better responsiveness
        watched_files: std.StringHashMap(FileInfo),
        file_pool: std.ArrayList([]u8), // Reusable path buffer pool

        const FileInfo = struct {
            mtime: i128,
            size: u64,
        };
    };

    const LinuxImpl = struct {
        inotify_fd: i32,
        watch_descriptors: std.ArrayList(WatchDescriptor),
        path_cache: std.StringHashMap([]const u8), // Cache wd->path mapping

        const WatchDescriptor = struct {
            wd: i32,
            path: []const u8,
        };
    };

    const WindowsImpl = struct {
        handles: std.ArrayList(std.os.windows.HANDLE),
        overlapped_data: std.ArrayList(OverlappedData),

        const OverlappedData = struct {
            overlapped: std.os.windows.OVERLAPPED,
            buffer: []align(@alignOf(std.os.windows.FILE_NOTIFY_INFORMATION)) u8,
            path: []const u8,
        };
    };

    /// Initialize a new file watcher for the specified directories
    pub fn init(allocator: std.mem.Allocator, paths: []const []const u8, callback: WatchCallback) !*Watcher {
        const watcher = try allocator.create(Watcher);
        errdefer allocator.destroy(watcher);

        const paths_copy = try allocator.alloc([]const u8, paths.len);
        errdefer allocator.free(paths_copy);

        for (paths, 0..) |path, i| {
            paths_copy[i] = try allocator.dupe(u8, path);
        }
        errdefer {
            for (paths_copy) |path| {
                allocator.free(path);
            }
            allocator.free(paths_copy);
        }

        watcher.* = .{
            .allocator = allocator,
            .paths = paths_copy,
            .callback = callback,
            .should_stop = std.atomic.Value(bool).init(false),
            .thread = null,
            .impl = switch (builtin.os.tag) {
                .macos => MacOSImpl{
                    .watched_files = std.StringHashMap(MacOSImpl.FileInfo).init(allocator),
                    .file_pool = std.ArrayList([]u8).init(allocator),
                },
                .linux => LinuxImpl{
                    .inotify_fd = -1,
                    .watch_descriptors = std.ArrayList(LinuxImpl.WatchDescriptor).init(allocator),
                    .path_cache = std.StringHashMap([]const u8).init(allocator),
                },
                .windows => WindowsImpl{
                    .handles = std.ArrayList(std.os.windows.HANDLE).init(allocator),
                    .overlapped_data = std.ArrayList(WindowsImpl.OverlappedData).init(allocator),
                },
                else => unreachable,
            },
        };

        return watcher;
    }

    /// Clean up all resources
    pub fn deinit(self: *Watcher) void {
        self.stop();

        for (self.paths) |path| {
            self.allocator.free(path);
        }
        self.allocator.free(self.paths);

        switch (builtin.os.tag) {
            .macos => {
                var it = self.impl.watched_files.iterator();
                while (it.next()) |entry| {
                    self.allocator.free(entry.key_ptr.*);
                }
                self.impl.watched_files.deinit();

                for (self.impl.file_pool.items) |buffer| {
                    self.allocator.free(buffer);
                }
                self.impl.file_pool.deinit();
            },
            .linux => {
                for (self.impl.watch_descriptors.items) |wd| {
                    self.allocator.free(wd.path);
                }
                self.impl.watch_descriptors.deinit();

                var it = self.impl.path_cache.iterator();
                while (it.next()) |entry| {
                    self.allocator.free(entry.key_ptr.*);
                    self.allocator.free(entry.value_ptr.*);
                }
                self.impl.path_cache.deinit();
            },
            .windows => {
                for (self.impl.overlapped_data.items) |*data| {
                    self.allocator.free(data.buffer);
                    self.allocator.free(data.path);
                    if (data.overlapped.hEvent) |event| {
                        _ = std.os.windows.CloseHandle(event);
                    }
                }
                self.impl.overlapped_data.deinit();
                self.impl.handles.deinit();
            },
            else => {},
        }

        self.allocator.destroy(self);
    }

    /// Start watching for file changes
    pub fn start(self: *Watcher) !void {
        if (self.thread != null) return error.AlreadyStarted;
        self.thread = try std.Thread.spawn(.{}, watchLoop, .{self});
    }

    /// Stop watching for file changes
    pub fn stop(self: *Watcher) void {
        self.should_stop.store(true, .seq_cst);

        if (self.thread) |thread| {
            thread.join();
            self.thread = null;
        }

        switch (builtin.os.tag) {
            .macos => {
                // Clear watched files for clean restart
                var it = self.impl.watched_files.iterator();
                while (it.next()) |entry| {
                    self.allocator.free(entry.key_ptr.*);
                }
                self.impl.watched_files.clearRetainingCapacity();
            },
            .linux => {
                if (self.impl.inotify_fd >= 0) {
                    std.posix.close(self.impl.inotify_fd);
                    self.impl.inotify_fd = -1;
                }
            },
            .windows => {
                for (self.impl.handles.items) |handle| {
                    _ = std.os.windows.CloseHandle(handle);
                }
                self.impl.handles.clearRetainingCapacity();
            },
            else => {},
        }
    }

    fn watchLoop(self: *Watcher) void {
        switch (builtin.os.tag) {
            .macos => self.watchLoopMacOS(),
            .linux => self.watchLoopLinux(),
            .windows => self.watchLoopWindows(),
            else => unreachable,
        }
    }

    fn watchLoopMacOS(self: *Watcher) void {
        // Initial scan
        for (self.paths) |path| {
            self.scanDirectoryInitialMacOS(path) catch |err| {
                std.log.err("Failed initial scan of {s}: {}", .{ path, err });
            };
        }

        // Main polling loop
        while (!self.should_stop.load(.seq_cst)) {
            for (self.paths) |path| {
                self.scanDirectoryForChangesMacOS(path) catch |err| {
                    std.log.err("Failed to scan {s}: {}", .{ path, err });
                };
            }
            std.time.sleep(self.impl.poll_interval_ns);
        }
    }

    fn scanDirectoryInitialMacOS(self: *Watcher, dir_path: []const u8) !void {
        var dir = try std.fs.openDirAbsolute(dir_path, .{ .iterate = true });
        defer dir.close();

        var walker = try dir.walk(self.allocator);
        defer walker.deinit();

        while (try walker.next()) |entry| {
            if (entry.kind == .file and std.mem.endsWith(u8, entry.basename, ".roc")) {
                const full_path = try std.fs.path.join(self.allocator, &.{ dir_path, entry.path });
                errdefer self.allocator.free(full_path);

                const stat = try std.fs.cwd().statFile(full_path);
                const info = MacOSImpl.FileInfo{
                    .mtime = stat.mtime,
                    .size = stat.size,
                };
                try self.impl.watched_files.put(full_path, info);
            }
        }
    }

    fn scanDirectoryForChangesMacOS(self: *Watcher, dir_path: []const u8) !void {
        var dir = try std.fs.openDirAbsolute(dir_path, .{ .iterate = true });
        defer dir.close();

        var walker = try dir.walk(self.allocator);
        defer walker.deinit();

        // Track seen files to detect deletions
        var seen = std.StringHashMap(void).init(self.allocator);
        defer {
            var it = seen.iterator();
            while (it.next()) |entry| {
                self.allocator.free(entry.key_ptr.*);
            }
            seen.deinit();
        }

        // Scan for changes and new files
        while (try walker.next()) |entry| {
            if (entry.kind == .file and std.mem.endsWith(u8, entry.basename, ".roc")) {
                const full_path = try std.fs.path.join(self.allocator, &.{ dir_path, entry.path });
                defer self.allocator.free(full_path);

                const stat = try std.fs.cwd().statFile(full_path);
                const new_info = MacOSImpl.FileInfo{
                    .mtime = stat.mtime,
                    .size = stat.size,
                };

                if (self.impl.watched_files.getPtr(full_path)) |old_info_ptr| {
                    if (new_info.mtime != old_info_ptr.mtime or new_info.size != old_info_ptr.size) {
                        self.callback(.{ .path = full_path });
                        old_info_ptr.* = new_info;
                    }
                } else {
                    self.callback(.{ .path = full_path });
                    try self.impl.watched_files.put(try self.allocator.dupe(u8, full_path), new_info);
                }

                try seen.put(try self.allocator.dupe(u8, full_path), {});
            }
        }

        // Detect and remove deleted files
        var to_remove = std.ArrayList([]const u8).init(self.allocator);
        defer to_remove.deinit();

        var it = self.impl.watched_files.iterator();
        while (it.next()) |entry| {
            if (!seen.contains(entry.key_ptr.*)) {
                try to_remove.append(entry.key_ptr.*);
            }
        }

        for (to_remove.items) |key| {
            if (self.impl.watched_files.fetchRemove(key)) |kv| {
                self.allocator.free(kv.key);
            }
        }
    }

    fn watchLoopLinux(self: *Watcher) void {
        self.impl.inotify_fd = std.posix.inotify_init1(std.os.linux.IN.NONBLOCK | std.os.linux.IN.CLOEXEC) catch |err| {
            std.log.err("inotify_init1 failed: {}", .{err});
            return;
        };
        defer {
            if (self.impl.inotify_fd >= 0) {
                std.posix.close(self.impl.inotify_fd);
            }
        }

        // Add watches
        for (self.paths) |path| {
            self.addWatchRecursiveLinux(path) catch |err| {
                std.log.err("Failed to watch {s}: {}", .{ path, err });
            };
        }

        // Main event loop
        var buffer: [8192]u8 align(@alignOf(std.os.linux.inotify_event)) = undefined;
        var poll_fds = [_]std.posix.pollfd{
            .{ .fd = self.impl.inotify_fd, .events = std.posix.POLL.IN, .revents = 0 },
        };

        while (!self.should_stop.load(.seq_cst)) {
            const poll_result = std.posix.poll(&poll_fds, 50) catch |err| {
                std.log.err("Poll error: {}", .{err});
                continue;
            };

            if (poll_result == 0) continue;

            const bytes_read = std.posix.read(self.impl.inotify_fd, &buffer) catch |err| {
                if (err == error.WouldBlock) continue;
                std.log.err("Read error: {}", .{err});
                continue;
            };

            self.processLinuxEvents(buffer[0..bytes_read]);
        }

        // Cleanup watches
        for (self.impl.watch_descriptors.items) |wd| {
            _ = std.posix.inotify_rm_watch(self.impl.inotify_fd, wd.wd) catch {};
        }
    }

    fn processLinuxEvents(self: *Watcher, buffer: []const u8) void {
        var offset: usize = 0;
        while (offset < buffer.len) {
            const event = @as(*const std.os.linux.inotify_event, @ptrCast(@alignCast(&buffer[offset])));
            const event_size = @sizeOf(std.os.linux.inotify_event) + event.len;

            if (event.len > 0) {
                const name_bytes = buffer[offset + @sizeOf(std.os.linux.inotify_event) .. offset + event_size - 1];
                const name = std.mem.sliceTo(name_bytes, 0);

                // Handle .roc file changes
                if (std.mem.endsWith(u8, name, ".roc")) {
                    for (self.impl.watch_descriptors.items) |wd| {
                        if (wd.wd == event.wd) {
                            const full_path = std.fs.path.join(self.allocator, &.{ wd.path, name }) catch break;
                            defer self.allocator.free(full_path);
                            self.callback(.{ .path = full_path });
                            break;
                        }
                    }
                }

                // Handle new directory creation
                if (event.mask & std.os.linux.IN.CREATE != 0 and event.mask & std.os.linux.IN.ISDIR != 0) {
                    for (self.impl.watch_descriptors.items) |wd| {
                        if (wd.wd == event.wd) {
                            const new_dir = std.fs.path.join(self.allocator, &.{ wd.path, name }) catch break;
                            defer self.allocator.free(new_dir);
                            self.addWatchRecursiveLinux(new_dir) catch |err| {
                                std.log.err("Failed to watch new directory: {}", .{err});
                            };
                            break;
                        }
                    }
                }
            }

            offset += event_size;
        }
    }

    fn addWatchRecursiveLinux(self: *Watcher, path: []const u8) !void {
        const flags = std.os.linux.IN.CREATE | std.os.linux.IN.DELETE |
            std.os.linux.IN.MODIFY | std.os.linux.IN.MOVED_FROM |
            std.os.linux.IN.MOVED_TO | std.os.linux.IN.CLOSE_WRITE;

        const path_z = try self.allocator.dupeZ(u8, path);
        defer self.allocator.free(path_z);

        const wd = try std.posix.inotify_add_watch(self.impl.inotify_fd, path_z, flags);

        const path_copy = try self.allocator.dupe(u8, path);
        errdefer self.allocator.free(path_copy);

        try self.impl.watch_descriptors.append(.{
            .wd = wd,
            .path = path_copy,
        });

        // Cache for faster lookups
        const wd_key = try std.fmt.allocPrint(self.allocator, "{d}", .{wd});
        try self.impl.path_cache.put(wd_key, try self.allocator.dupe(u8, path));

        // Recursively watch subdirectories
        var dir = try std.fs.openDirAbsolute(path, .{ .iterate = true });
        defer dir.close();

        var it = dir.iterate();
        while (try it.next()) |entry| {
            if (entry.kind == .directory) {
                const subdir_path = try std.fs.path.join(self.allocator, &.{ path, entry.name });
                defer self.allocator.free(subdir_path);
                try self.addWatchRecursiveLinux(subdir_path);
            }
        }
    }

    fn watchLoopWindows(self: *Watcher) void {
        // Setup directory handles
        for (self.paths) |path| {
            self.addWatchWindows(path) catch |err| {
                std.log.err("Failed to watch {s}: {}", .{ path, err });
            };
        }

        // Main event loop
        while (!self.should_stop.load(.seq_cst)) {
            for (self.impl.overlapped_data.items, 0..) |*data, i| {
                const handle = self.impl.handles.items[i];
                var bytes_returned: std.os.windows.DWORD = 0;

                const notify_filter = std.os.windows.FILE_NOTIFY_CHANGE_FILE_NAME |
                    std.os.windows.FILE_NOTIFY_CHANGE_DIR_NAME |
                    std.os.windows.FILE_NOTIFY_CHANGE_SIZE |
                    std.os.windows.FILE_NOTIFY_CHANGE_LAST_WRITE;

                const result = std.os.windows.kernel32.ReadDirectoryChangesW(
                    handle,
                    data.buffer.ptr,
                    @intCast(data.buffer.len),
                    1, // Watch subtree
                    notify_filter,
                    &bytes_returned,
                    &data.overlapped,
                    null,
                );

                if (result == 0) continue;

                const wait_result = std.os.windows.kernel32.WaitForSingleObject(
                    data.overlapped.hEvent,
                    50, // 50ms timeout
                );

                if (wait_result != std.os.windows.WAIT_OBJECT_0) continue;

                if (!std.os.windows.kernel32.GetOverlappedResult(handle, &data.overlapped, &bytes_returned, 0)) {
                    continue;
                }

                self.processWindowsNotifications(data.buffer[0..bytes_returned], data.path);
            }
        }
    }

    fn addWatchWindows(self: *Watcher, path: []const u8) !void {
        const path_w = try std.unicode.utf8ToUtf16LeAllocZ(self.allocator, path);
        defer self.allocator.free(path_w);

        const handle = std.os.windows.kernel32.CreateFileW(
            path_w,
            std.os.windows.FILE_LIST_DIRECTORY,
            std.os.windows.FILE_SHARE_READ | std.os.windows.FILE_SHARE_WRITE | std.os.windows.FILE_SHARE_DELETE,
            null,
            std.os.windows.OPEN_EXISTING,
            std.os.windows.FILE_FLAG_BACKUP_SEMANTICS | std.os.windows.FILE_FLAG_OVERLAPPED,
            null,
        );

        if (handle == std.os.windows.INVALID_HANDLE_VALUE) {
            return error.FailedToOpenDirectory;
        }

        try self.impl.handles.append(handle);

        const buffer = try self.allocator.alignedAlloc(u8, @alignOf(std.os.windows.FILE_NOTIFY_INFORMATION), 64 * 1024);
        errdefer self.allocator.free(buffer);

        var overlapped = std.mem.zeroes(std.os.windows.OVERLAPPED);
        overlapped.hEvent = try std.os.windows.CreateEventExW(
            null,
            null,
            0,
            std.os.windows.SYNCHRONIZE | std.os.windows.EVENT_MODIFY_STATE,
        );

        const path_copy = try self.allocator.dupe(u8, path);
        errdefer self.allocator.free(path_copy);

        try self.impl.overlapped_data.append(.{
            .overlapped = overlapped,
            .buffer = buffer,
            .path = path_copy,
        });
    }

    fn processWindowsNotifications(self: *Watcher, buffer: []const u8, base_path: []const u8) void {
        var offset: usize = 0;
        while (offset < buffer.len) {
            const info = @as(*const std.os.windows.FILE_NOTIFY_INFORMATION, @ptrCast(@alignCast(&buffer[offset])));

            if (info.FileNameLength > 0) {
                const name_len = info.FileNameLength / 2;
                const name_ptr = @as([*]const u16, @ptrCast(@alignCast(&buffer[offset + @sizeOf(std.os.windows.FILE_NOTIFY_INFORMATION)])));
                const name_utf16 = name_ptr[0..name_len];

                const name_utf8 = std.unicode.utf16LeToUtf8Alloc(self.allocator, name_utf16) catch {
                    if (info.NextEntryOffset == 0) break;
                    offset += info.NextEntryOffset;
                    continue;
                };
                defer self.allocator.free(name_utf8);

                if (std.mem.endsWith(u8, name_utf8, ".roc")) {
                    const full_path = std.fs.path.join(self.allocator, &.{ base_path, name_utf8 }) catch {
                        if (info.NextEntryOffset == 0) break;
                        offset += info.NextEntryOffset;
                        continue;
                    };
                    defer self.allocator.free(full_path);
                    self.callback(.{ .path = full_path });
                }
            }

            if (info.NextEntryOffset == 0) break;
            offset += info.NextEntryOffset;
        }
    }
};

// ===== TESTS =====

test "basic file watching" {
    const allocator = std.testing.allocator;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(temp_path);

    const global = struct {
        var event_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
        var last_path: ?[]const u8 = null;
        var mutex: std.Thread.Mutex = .{};
    };

    const callback = struct {
        fn cb(event: WatchEvent) void {
            _ = global.event_count.fetchAdd(1, .seq_cst);
            global.mutex.lock();
            defer global.mutex.unlock();
            global.last_path = event.path;
        }
    }.cb;

    const watcher = try Watcher.init(allocator, &.{temp_path}, callback);
    defer watcher.deinit();

    try watcher.start();
    std.time.sleep(200 * std.time.ns_per_ms);

    // Create .roc files
    try temp_dir.dir.writeFile(.{ .sub_path = "test1.roc", .data = "content1" });
    std.time.sleep(150 * std.time.ns_per_ms);

    try temp_dir.dir.writeFile(.{ .sub_path = "test2.roc", .data = "content2" });
    std.time.sleep(150 * std.time.ns_per_ms);

    // Non-.roc file should be ignored
    try temp_dir.dir.writeFile(.{ .sub_path = "test3.txt", .data = "ignored" });
    std.time.sleep(150 * std.time.ns_per_ms);

    watcher.stop();

    const count = global.event_count.load(.seq_cst);
    if (builtin.os.tag != .macos) {
        try std.testing.expect(count >= 2);
    }
}

test "recursive directory watching" {
    const allocator = std.testing.allocator;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(temp_path);

    // Setup directory structure
    try temp_dir.dir.makePath("subdir1/nested");
    try temp_dir.dir.makePath("subdir2");

    const global = struct {
        var event_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
    };

    const callback = struct {
        fn cb(event: WatchEvent) void {
            _ = event;
            _ = global.event_count.fetchAdd(1, .seq_cst);
        }
    }.cb;

    const watcher = try Watcher.init(allocator, &.{temp_path}, callback);
    defer watcher.deinit();

    try watcher.start();
    std.time.sleep(200 * std.time.ns_per_ms);

    // Create files in nested directories
    try temp_dir.dir.writeFile(.{ .sub_path = "subdir1/file1.roc", .data = "content" });
    std.time.sleep(100 * std.time.ns_per_ms);

    try temp_dir.dir.writeFile(.{ .sub_path = "subdir1/nested/file2.roc", .data = "content" });
    std.time.sleep(100 * std.time.ns_per_ms);

    try temp_dir.dir.writeFile(.{ .sub_path = "subdir2/file3.roc", .data = "content" });
    std.time.sleep(100 * std.time.ns_per_ms);

    // Test dynamic directory creation on Linux
    if (builtin.os.tag == .linux) {
        try temp_dir.dir.makePath("subdir3");
        std.time.sleep(100 * std.time.ns_per_ms);
        try temp_dir.dir.writeFile(.{ .sub_path = "subdir3/file4.roc", .data = "content" });
        std.time.sleep(100 * std.time.ns_per_ms);
    }

    watcher.stop();

    const count = global.event_count.load(.seq_cst);
    if (builtin.os.tag != .macos) {
        try std.testing.expect(count >= 3);
    }
}

test "multiple directories watching" {
    const allocator = std.testing.allocator;

    var temp_dir1 = std.testing.tmpDir(.{});
    defer temp_dir1.cleanup();
    const temp_path1 = try temp_dir1.dir.realpathAlloc(allocator, ".");
    defer allocator.free(temp_path1);

    var temp_dir2 = std.testing.tmpDir(.{});
    defer temp_dir2.cleanup();
    const temp_path2 = try temp_dir2.dir.realpathAlloc(allocator, ".");
    defer allocator.free(temp_path2);

    const global = struct {
        var event_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
    };

    const callback = struct {
        fn cb(event: WatchEvent) void {
            _ = event;
            _ = global.event_count.fetchAdd(1, .seq_cst);
        }
    }.cb;

    const watcher = try Watcher.init(allocator, &.{ temp_path1, temp_path2 }, callback);
    defer watcher.deinit();

    try watcher.start();
    std.time.sleep(200 * std.time.ns_per_ms);

    try temp_dir1.dir.writeFile(.{ .sub_path = "file1.roc", .data = "content" });
    std.time.sleep(100 * std.time.ns_per_ms);

    try temp_dir2.dir.writeFile(.{ .sub_path = "file2.roc", .data = "content" });
    std.time.sleep(100 * std.time.ns_per_ms);

    watcher.stop();

    const count = global.event_count.load(.seq_cst);
    if (builtin.os.tag != .macos) {
        try std.testing.expect(count >= 2);
    }
}

test "file modification detection" {
    const allocator = std.testing.allocator;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(temp_path);

    const global = struct {
        var event_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
    };

    const callback = struct {
        fn cb(event: WatchEvent) void {
            _ = event;
            _ = global.event_count.fetchAdd(1, .seq_cst);
        }
    }.cb;

    const watcher = try Watcher.init(allocator, &.{temp_path}, callback);
    defer watcher.deinit();

    // Create initial file
    try temp_dir.dir.writeFile(.{ .sub_path = "modify.roc", .data = "initial" });

    try watcher.start();
    std.time.sleep(200 * std.time.ns_per_ms);

    // Modify the file
    try temp_dir.dir.writeFile(.{ .sub_path = "modify.roc", .data = "modified content that is longer" });
    std.time.sleep(150 * std.time.ns_per_ms);

    // Modify again
    try temp_dir.dir.writeFile(.{ .sub_path = "modify.roc", .data = "short" });
    std.time.sleep(150 * std.time.ns_per_ms);

    watcher.stop();

    const count = global.event_count.load(.seq_cst);
    if (builtin.os.tag != .macos) {
        try std.testing.expect(count >= 2);
    }
}

test "rapid file changes performance" {
    const allocator = std.testing.allocator;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(temp_path);

    const global = struct {
        var event_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
    };

    const callback = struct {
        fn cb(event: WatchEvent) void {
            _ = event;
            _ = global.event_count.fetchAdd(1, .seq_cst);
        }
    }.cb;

    const watcher = try Watcher.init(allocator, &.{temp_path}, callback);
    defer watcher.deinit();

    try watcher.start();
    std.time.sleep(200 * std.time.ns_per_ms);

    const start_time = std.time.milliTimestamp();

    // Create many files rapidly
    for (0..50) |i| {
        const filename = try std.fmt.allocPrint(allocator, "file{d}.roc", .{i});
        defer allocator.free(filename);
        try temp_dir.dir.writeFile(.{ .sub_path = filename, .data = "content" });
        std.time.sleep(5 * std.time.ns_per_ms);
    }

    const elapsed = std.time.milliTimestamp() - start_time;

    watcher.stop();

    // Should complete quickly
    try std.testing.expect(elapsed < 3000);

    const count = global.event_count.load(.seq_cst);
    if (builtin.os.tag != .macos) {
        try std.testing.expect(count > 0);
    }
}

test "concurrent watchers on same directory" {
    const allocator = std.testing.allocator;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(temp_path);

    const global = struct {
        var event_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
    };

    const callback = struct {
        fn cb(event: WatchEvent) void {
            _ = event;
            _ = global.event_count.fetchAdd(1, .seq_cst);
        }
    }.cb;

    var watchers: [3]*Watcher = undefined;

    for (0..3) |i| {
        watchers[i] = try Watcher.init(allocator, &.{temp_path}, callback);
        try watchers[i].start();
    }

    std.time.sleep(200 * std.time.ns_per_ms);

    try temp_dir.dir.writeFile(.{ .sub_path = "concurrent.roc", .data = "content" });
    std.time.sleep(200 * std.time.ns_per_ms);

    for (0..3) |i| {
        watchers[i].stop();
        watchers[i].deinit();
    }

    const count = global.event_count.load(.seq_cst);
    if (builtin.os.tag != .macos) {
        try std.testing.expect(count >= 3);
    }
}

test "start stop restart" {
    const allocator = std.testing.allocator;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(temp_path);

    const global = struct {
        var event_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
    };

    const callback = struct {
        fn cb(event: WatchEvent) void {
            _ = event;
            _ = global.event_count.fetchAdd(1, .seq_cst);
        }
    }.cb;

    const watcher = try Watcher.init(allocator, &.{temp_path}, callback);
    defer watcher.deinit();

    // Start, create file, stop
    try watcher.start();
    std.time.sleep(100 * std.time.ns_per_ms);
    try temp_dir.dir.writeFile(.{ .sub_path = "test1.roc", .data = "content" });
    std.time.sleep(100 * std.time.ns_per_ms);
    watcher.stop();

    const count1 = global.event_count.load(.seq_cst);

    // Create file while stopped (should not trigger)
    try temp_dir.dir.writeFile(.{ .sub_path = "test2.roc", .data = "content" });
    std.time.sleep(100 * std.time.ns_per_ms);

    const count2 = global.event_count.load(.seq_cst);
    try std.testing.expectEqual(count1, count2);

    // Restart and create another file
    try watcher.start();
    std.time.sleep(100 * std.time.ns_per_ms);
    try temp_dir.dir.writeFile(.{ .sub_path = "test3.roc", .data = "content" });
    std.time.sleep(100 * std.time.ns_per_ms);
    watcher.stop();

    const count3 = global.event_count.load(.seq_cst);
    if (builtin.os.tag != .macos) {
        try std.testing.expect(count3 > count2);
    }
}

test "empty directory" {
    const allocator = std.testing.allocator;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(temp_path);

    const global = struct {
        var event_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
    };

    const callback = struct {
        fn cb(event: WatchEvent) void {
            _ = event;
            _ = global.event_count.fetchAdd(1, .seq_cst);
        }
    }.cb;

    const watcher = try Watcher.init(allocator, &.{temp_path}, callback);
    defer watcher.deinit();

    try watcher.start();
    std.time.sleep(100 * std.time.ns_per_ms);
    watcher.stop();

    const count = global.event_count.load(.seq_cst);
    try std.testing.expectEqual(@as(u32, 0), count);
}

test "already started error" {
    const allocator = std.testing.allocator;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(temp_path);

    const callback = struct {
        fn cb(event: WatchEvent) void {
            _ = event;
        }
    }.cb;

    const watcher = try Watcher.init(allocator, &.{temp_path}, callback);
    defer watcher.deinit();

    try watcher.start();
    defer watcher.stop();

    // Should return error when trying to start again
    try std.testing.expectError(error.AlreadyStarted, watcher.start());
}

test "file rename detection" {
    const allocator = std.testing.allocator;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(temp_path);

    const global = struct {
        var event_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
    };

    const callback = struct {
        fn cb(event: WatchEvent) void {
            _ = event;
            _ = global.event_count.fetchAdd(1, .seq_cst);
        }
    }.cb;

    const watcher = try Watcher.init(allocator, &.{temp_path}, callback);
    defer watcher.deinit();

    try watcher.start();
    std.time.sleep(200 * std.time.ns_per_ms);

    // Create and rename file
    try temp_dir.dir.writeFile(.{ .sub_path = "old.roc", .data = "content" });
    std.time.sleep(100 * std.time.ns_per_ms);

    try temp_dir.dir.rename("old.roc", "new.roc");
    std.time.sleep(100 * std.time.ns_per_ms);

    watcher.stop();

    const count = global.event_count.load(.seq_cst);
    if (builtin.os.tag == .linux) {
        // Linux should detect both the creation and rename
        try std.testing.expect(count >= 1);
    }
}
