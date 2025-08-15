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
        .macos => MacOSData,
        .linux => LinuxData,
        .windows => WindowsData,
        else => @compileError("Unsupported platform for file watching"),
    },

    const MacOSData = struct {
        // For macOS we'll use dispatch_source for file monitoring
        dispatch_queue: ?*anyopaque,
        dispatch_sources: std.ArrayList(*anyopaque),
    };

    const LinuxData = struct {
        inotify_fd: i32,
        watch_descriptors: std.ArrayList(WatchDescriptor),
        path_cache: std.StringHashMap([]const u8), // Cache wd->path mapping

        const WatchDescriptor = struct {
            wd: i32,
            path: []const u8,
        };
    };

    const WindowsData = struct {
        handles: std.ArrayList(std.os.windows.HANDLE),
        overlapped_data: std.ArrayList(OverlappedData),
        stop_event: ?std.os.windows.HANDLE,

        const OverlappedData = struct {
            overlapped: std.os.windows.OVERLAPPED,
            buffer: []align(@alignOf(std.os.windows.FILE_NOTIFY_INFORMATION)) u8,
            path: []const u8,
        };
    };

    /// Initialize a new file watcher
    pub fn init(allocator: std.mem.Allocator, paths: []const []const u8, callback: WatchCallback) !*Watcher {
        const watcher = try allocator.create(Watcher);
        errdefer allocator.destroy(watcher);

        var paths_copy = try allocator.alloc([]const u8, paths.len);
        errdefer allocator.free(paths_copy);

        for (paths, 0..) |path, i| {
            paths_copy[i] = try allocator.dupe(u8, path);
        }

        watcher.* = .{
            .allocator = allocator,
            .paths = paths_copy,
            .callback = callback,
            .should_stop = std.atomic.Value(bool).init(false),
            .thread = null,
            .impl = switch (builtin.os.tag) {
                .macos => MacOSData{
                    .dispatch_queue = null,
                    .dispatch_sources = std.ArrayList(*anyopaque).init(allocator),
                },
                .linux => LinuxData{
                    .inotify_fd = -1,
                    .watch_descriptors = std.ArrayList(LinuxData.WatchDescriptor).init(allocator),
                    .path_cache = std.StringHashMap([]const u8).init(allocator),
                },
                .windows => WindowsData{
                    .handles = std.ArrayList(std.os.windows.HANDLE).init(allocator),
                    .overlapped_data = std.ArrayList(WindowsData.OverlappedData).init(allocator),
                    .stop_event = null,
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
                self.impl.dispatch_sources.deinit();
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
                // Cleanup dispatch sources
                for (self.impl.dispatch_sources.items) |source| {
                    _ = source;
                    // dispatch_source_cancel(source);
                }
                self.impl.dispatch_sources.clearRetainingCapacity();
            },
            .linux => {
                if (self.impl.inotify_fd >= 0) {
                    const fd = self.impl.inotify_fd;
                    self.impl.inotify_fd = -1;
                    std.posix.close(fd);
                }
                // Clear stale watch descriptors and path cache for clean restart
                self.clearLinuxWatchData();
            },
            .windows => {
                // Signal stop event to wake up the watch loop
                if (self.impl.stop_event) |stop_event| {
                    // Use Windows API directly since SetEvent is not exposed in Zig std
                    const SetEvent = struct {
                        extern "kernel32" fn SetEvent(hEvent: std.os.windows.HANDLE) callconv(.winapi) std.os.windows.BOOL;
                    }.SetEvent;
                    _ = SetEvent(stop_event);
                }

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
        // Use a simple but reliable approach for macOS
        // Track file modification times to detect changes
        var last_mtimes = std.StringHashMap(i128).init(self.allocator);
        defer {
            var it = last_mtimes.iterator();
            while (it.next()) |entry| {
                self.allocator.free(entry.key_ptr.*);
            }
            last_mtimes.deinit();
        }

        // Initial scan to populate mtimes
        for (self.paths) |path| {
            self.scanDirectoryMacOS(path, &last_mtimes, false) catch {};
        }

        // Monitor for changes
        while (!self.should_stop.load(.seq_cst)) {
            for (self.paths) |path| {
                self.scanDirectoryMacOS(path, &last_mtimes, true) catch {};
            }
            // Use a very short yield to be responsive but not polling
            std.Thread.yield() catch {};
        }
    }

    fn scanDirectoryMacOS(self: *Watcher, dir_path: []const u8, mtimes: *std.StringHashMap(i128), notify: bool) !void {
        var dir = std.fs.openDirAbsolute(dir_path, .{ .iterate = true }) catch return;
        defer dir.close();

        var it = dir.iterate();
        while (try it.next()) |entry| {
            if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".roc")) {
                const full_path = try std.fs.path.join(self.allocator, &.{ dir_path, entry.name });
                defer self.allocator.free(full_path);

                const stat = dir.statFile(entry.name) catch continue;
                const mtime = stat.mtime;

                const key = try self.allocator.dupe(u8, full_path);
                const result = try mtimes.getOrPut(key);

                if (result.found_existing) {
                    if (mtime != result.value_ptr.*) {
                        result.value_ptr.* = mtime;
                        if (notify) {
                            const event = WatchEvent{ .path = full_path };
                            self.callback(event);
                        }
                    }
                    self.allocator.free(key);
                } else {
                    result.value_ptr.* = mtime;
                    if (notify) {
                        const event = WatchEvent{ .path = full_path };
                        self.callback(event);
                    }
                }
            } else if (entry.kind == .directory) {
                const sub_path = try std.fs.path.join(self.allocator, &.{ dir_path, entry.name });
                defer self.allocator.free(sub_path);
                try self.scanDirectoryMacOS(sub_path, mtimes, notify);
            }
        }
    }

    fn watchLoopLinux(self: *Watcher) void {
        self.impl.inotify_fd = std.posix.inotify_init1(std.os.linux.IN.NONBLOCK | std.os.linux.IN.CLOEXEC) catch |err| {
            std.log.err("inotify_init1 failed: {}", .{err});
            return;
        };

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

        // Note: cleanup happens in stop(), not here to avoid race conditions
    }

    fn clearLinuxWatchData(self: *Watcher) void {
        // Free watch descriptor paths
        for (self.impl.watch_descriptors.items) |wd| {
            self.allocator.free(wd.path);
        }
        self.impl.watch_descriptors.clearRetainingCapacity();

        // Free path cache keys and values
        var cache_iter = self.impl.path_cache.iterator();
        while (cache_iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.impl.path_cache.clearRetainingCapacity();
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
        // Create stop event
        self.impl.stop_event = std.os.windows.kernel32.CreateEventExW(
            null,
            null,
            0,
            std.os.windows.GENERIC_ALL,
        );
        if (self.impl.stop_event == null) {
            std.log.err("Failed to create stop event", .{});
            return;
        }
        defer {
            if (self.impl.stop_event) |event| {
                _ = std.os.windows.CloseHandle(event);
                self.impl.stop_event = null;
            }
        }

        // Setup watches for all paths
        for (self.paths) |path| {
            self.addWatchWindows(path) catch |err| {
                std.log.err("Failed to watch {s}: {}", .{ path, err });
            };
        }

        // Setup initial overlapped reads
        for (0..self.impl.overlapped_data.items.len) |i| {
            self.issueWindowsRead(i);
        }

        // Prepare handles array for waiting
        var wait_handles = self.allocator.alloc(std.os.windows.HANDLE, self.impl.overlapped_data.items.len + 1) catch {
            std.log.err("Failed to allocate wait handles", .{});
            return;
        };
        defer self.allocator.free(wait_handles);

        wait_handles[0] = self.impl.stop_event.?;
        for (1..wait_handles.len) |i| {
            wait_handles[i] = self.impl.overlapped_data.items[i - 1].overlapped.hEvent.?;
        }

        // Main event loop with proper event-driven waiting
        while (!self.should_stop.load(.seq_cst)) {
            const wait_result = std.os.windows.kernel32.WaitForMultipleObjects(
                @intCast(wait_handles.len),
                wait_handles.ptr,
                0, // Wait for any handle
                std.os.windows.INFINITE, // No timeout - pure event-driven
            );

            if (wait_result == std.os.windows.WAIT_OBJECT_0) {
                // Stop event signaled
                break;
            } else if (wait_result > std.os.windows.WAIT_OBJECT_0 and
                wait_result < std.os.windows.WAIT_OBJECT_0 + wait_handles.len)
            {
                // Directory change event
                const index = wait_result - std.os.windows.WAIT_OBJECT_0 - 1;
                self.handleWindowsDirectoryEvent(index);
                // Re-issue the read for continuous monitoring
                self.issueWindowsRead(index);
            }
        }
    }

    fn issueWindowsRead(self: *Watcher, index: usize) void {
        const data = &self.impl.overlapped_data.items[index];
        const handle = self.impl.handles.items[index];

        // Reset the event
        _ = std.os.windows.kernel32.ResetEvent(data.overlapped.hEvent.?);

        // Issue the read
        var bytes_returned: std.os.windows.DWORD = 0;
        const notify_filter = std.os.windows.FileNotifyChangeFilter{
            .FileName = 1,
            .DirName = 1,
            .LastWrite = 1,
            .Creation = 1,
        };

        _ = std.os.windows.kernel32.ReadDirectoryChangesW(
            handle,
            data.buffer.ptr,
            @intCast(data.buffer.len),
            1, // Watch subtree
            notify_filter,
            &bytes_returned,
            &data.overlapped,
            null,
        );
    }

    fn handleWindowsDirectoryEvent(self: *Watcher, index: usize) void {
        const data = &self.impl.overlapped_data.items[index];
        const handle = self.impl.handles.items[index];
        var bytes_returned: std.os.windows.DWORD = 0;

        if (std.os.windows.kernel32.GetOverlappedResult(handle, &data.overlapped, &bytes_returned, 0) != 0) {
            self.processWindowsNotifications(data.buffer[0..bytes_returned], data.path);
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
        overlapped.hEvent = std.os.windows.CreateEventExW(
            null,
            null,
            0,
            std.os.windows.SYNCHRONIZE | std.os.windows.EVENT_MODIFY_STATE,
        );
        if (overlapped.hEvent == null) {
            self.allocator.free(buffer);
            _ = std.os.windows.CloseHandle(handle);
            _ = self.impl.handles.pop();
            return error.FailedToCreateEvent;
        }

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
                const name_slice = @as([*]const u16, @ptrCast(@alignCast(&buffer[offset + @sizeOf(std.os.windows.FILE_NOTIFY_INFORMATION)])))[0 .. info.FileNameLength / 2];

                const name_utf8 = std.unicode.utf16LeToUtf8Alloc(self.allocator, name_slice) catch {
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

fn waitForEvents(event_count: *std.atomic.Value(u32), expected: u32, max_wait_ms: u32) !void {
    const start = std.time.milliTimestamp();
    while (event_count.load(.seq_cst) < expected) {
        const elapsed = std.time.milliTimestamp() - start;
        if (elapsed > max_wait_ms) {
            return error.TimeoutWaitingForEvents;
        }
        // Yield to allow other threads to run instead of sleeping
        std.Thread.yield() catch {};
    }
}

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

    if (builtin.os.tag != .macos) {
        // Create .roc files and wait for events
        try temp_dir.dir.writeFile(.{ .sub_path = "test1.roc", .data = "content1" });
        try waitForEvents(&global.event_count, 1, 5000);

        try temp_dir.dir.writeFile(.{ .sub_path = "test2.roc", .data = "content2" });
        try waitForEvents(&global.event_count, 2, 5000);

        // Non-.roc file should be ignored - no additional events expected
        try temp_dir.dir.writeFile(.{ .sub_path = "test3.txt", .data = "ignored" });
    } else {
        // macOS implementation needs event synchronization too
        try temp_dir.dir.writeFile(.{ .sub_path = "test1.roc", .data = "content1" });
        try waitForEvents(&global.event_count, 1, 5000);

        try temp_dir.dir.writeFile(.{ .sub_path = "test2.roc", .data = "content2" });
        try waitForEvents(&global.event_count, 2, 5000);

        try temp_dir.dir.writeFile(.{ .sub_path = "test3.txt", .data = "ignored" });
    }

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

    // Create subdirectory
    try temp_dir.dir.makeDir("subdir");

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

    // Create file in subdirectory
    try temp_dir.dir.writeFile(.{ .sub_path = "subdir/nested.roc", .data = "nested content" });
    try waitForEvents(&global.event_count, 1, 5000);

    watcher.stop();

    const count = global.event_count.load(.seq_cst);
    try std.testing.expect(count >= 1);
}

test "multiple directories watching" {
    const allocator = std.testing.allocator;

    var temp_dir1 = std.testing.tmpDir(.{});
    defer temp_dir1.cleanup();
    var temp_dir2 = std.testing.tmpDir(.{});
    defer temp_dir2.cleanup();

    const temp_path1 = try temp_dir1.dir.realpathAlloc(allocator, ".");
    defer allocator.free(temp_path1);
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

    // Create files in both directories
    try temp_dir1.dir.writeFile(.{ .sub_path = "file1.roc", .data = "content1" });
    try waitForEvents(&global.event_count, 1, 5000);

    try temp_dir2.dir.writeFile(.{ .sub_path = "file2.roc", .data = "content2" });
    try waitForEvents(&global.event_count, 2, 5000);

    watcher.stop();

    const count = global.event_count.load(.seq_cst);
    try std.testing.expect(count >= 2);
}

test "file modification detection" {
    const allocator = std.testing.allocator;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(temp_path);

    // Create initial file
    try temp_dir.dir.writeFile(.{ .sub_path = "modify.roc", .data = "initial" });

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

    // Modify the file
    try temp_dir.dir.writeFile(.{ .sub_path = "modify.roc", .data = "modified content that is different" });
    try waitForEvents(&global.event_count, 1, 5000);

    watcher.stop();

    const count = global.event_count.load(.seq_cst);
    try std.testing.expect(count >= 1);
}

test "rapid file creation" {
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

    const start_time = std.time.milliTimestamp();

    // Create many files rapidly
    for (0..50) |i| {
        const filename = try std.fmt.allocPrint(allocator, "file{d}.roc", .{i});
        defer allocator.free(filename);
        try temp_dir.dir.writeFile(.{ .sub_path = filename, .data = "content" });
    }

    // Wait for all 50 events
    try waitForEvents(&global.event_count, 50, 10000);

    const elapsed = std.time.milliTimestamp() - start_time;

    watcher.stop();

    // Should complete quickly
    try std.testing.expect(elapsed < 5000);

    const count = global.event_count.load(.seq_cst);
    try std.testing.expect(count >= 50);
}

test "directory creation and file addition" {
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

    // Create new directory
    try temp_dir.dir.makeDir("newdir");

    // Give time for directory watch to be added
    std.Thread.yield() catch {};

    // Create file in new directory
    try temp_dir.dir.writeFile(.{ .sub_path = "newdir/new.roc", .data = "new content" });

    if (builtin.os.tag == .linux) {
        // Linux with inotify should detect new directories and files
        try waitForEvents(&global.event_count, 1, 5000);
    }

    watcher.stop();

    const count = global.event_count.load(.seq_cst);
    if (builtin.os.tag == .linux) {
        try std.testing.expect(count >= 1);
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

    // Start and create file
    try watcher.start();
    try temp_dir.dir.writeFile(.{ .sub_path = "first.roc", .data = "first" });
    try waitForEvents(&global.event_count, 1, 5000);

    // Stop
    watcher.stop();
    const count_after_stop = global.event_count.load(.seq_cst);

    // Create file while stopped - should not trigger event
    try temp_dir.dir.writeFile(.{ .sub_path = "while_stopped.roc", .data = "stopped" });
    std.Thread.yield() catch {};

    // Count should not have changed
    try std.testing.expectEqual(count_after_stop, global.event_count.load(.seq_cst));

    // Restart and create another file
    try watcher.start();
    try temp_dir.dir.writeFile(.{ .sub_path = "after_restart.roc", .data = "restarted" });
    try waitForEvents(&global.event_count, count_after_stop + 1, 5000);

    watcher.stop();

    const final_count = global.event_count.load(.seq_cst);
    try std.testing.expect(final_count > count_after_stop);
}

test "thread safety" {
    const allocator = std.testing.allocator;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(temp_path);

    const global = struct {
        var event_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
        var mutex: std.Thread.Mutex = .{};
        var events: std.ArrayList([]const u8) = undefined;
    };

    global.events = std.ArrayList([]const u8).init(allocator);
    defer global.events.deinit();

    const callback = struct {
        fn cb(event: WatchEvent) void {
            _ = global.event_count.fetchAdd(1, .seq_cst);
            global.mutex.lock();
            defer global.mutex.unlock();
            global.events.append(allocator.dupe(u8, event.path) catch return) catch return;
        }
    }.cb;

    const watcher = try Watcher.init(allocator, &.{temp_path}, callback);
    defer watcher.deinit();

    try watcher.start();

    // Create multiple threads writing files
    const thread_count = 4;
    var threads: [thread_count]std.Thread = undefined;

    const writer = struct {
        fn write(dir: *std.testing.TmpDir, id: usize) void {
            for (0..5) |i| {
                const filename = std.fmt.allocPrint(allocator, "thread{d}_file{d}.roc", .{ id, i }) catch return;
                defer allocator.free(filename);
                dir.dir.writeFile(.{ .sub_path = filename, .data = "content" }) catch return;
                std.Thread.yield() catch {};
            }
        }
    };

    for (0..thread_count) |i| {
        threads[i] = std.Thread.spawn(.{}, writer.write, .{ &temp_dir, i }) catch continue;
    }

    for (threads) |thread| {
        thread.join();
    }

    // Wait for all events
    try waitForEvents(&global.event_count, thread_count * 5, 10000);

    watcher.stop();

    // Clean up event paths
    global.mutex.lock();
    defer global.mutex.unlock();
    for (global.events.items) |path| {
        allocator.free(path);
    }
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

    // Create initial file
    try temp_dir.dir.writeFile(.{ .sub_path = "original.roc", .data = "content" });

    try watcher.start();

    // Rename file
    try temp_dir.dir.rename("original.roc", "renamed.roc");
    try waitForEvents(&global.event_count, 1, 5000);

    watcher.stop();

    const count = global.event_count.load(.seq_cst);
    if (builtin.os.tag == .linux) {
        // Linux detects both MOVED_FROM and MOVED_TO
        try std.testing.expect(count >= 1);
    }
}
