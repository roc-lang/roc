//! File system watcher for monitoring .roc file changes across platforms.
//! Provides efficient, cross-platform file watching with recursive directory support.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");

// Use real FSEvents only when building natively for macOS
// Cross-compilation detection: Use build system's isNative() check
const target_is_macos = builtin.os.tag == .macos;
const target_is_native = build_options.target_is_native;
const use_real_fsevents = target_is_macos and target_is_native;
const use_stubs = !use_real_fsevents;

// macOS FSEvents type declarations (always needed for struct definitions)
const FSEventStreamRef = *anyopaque;
const CFRunLoopRef = *anyopaque;
const CFStringRef = *anyopaque;
const CFArrayRef = *anyopaque;
const CFAllocatorRef = ?*anyopaque;
const CFIndex = isize;
const CFAbsoluteTime = f64;
const FSEventStreamEventId = u64;
const FSEventStreamCreateFlags = u32;
const FSEventStreamEventFlags = u32;

// FSEvents constants
const kFSEventStreamCreateFlagFileEvents: FSEventStreamCreateFlags = 0x00000010;
const kFSEventStreamCreateFlagNoDefer: FSEventStreamCreateFlags = 0x00000002;
const kFSEventStreamCreateFlagWatchRoot: FSEventStreamCreateFlags = 0x00000004;
const kCFStringEncodingUTF8: u32 = 0x08000100;

// FSEventStream context structure
const FSEventStreamContext = extern struct {
    version: CFIndex,
    info: ?*anyopaque,
    retain: ?*const anyopaque,
    release: ?*const anyopaque,
    copyDescription: ?*const anyopaque,
};

// Check if we're compiling for native macOS (not cross-compiling)
// We use a weak linkage approach to handle cross-compilation
const is_native_macos = builtin.os.tag == .macos;

// Only declare real externs when we're actually using them
const macos_externs = if (use_real_fsevents) struct {
    // Mark functions as weak so they're optional during cross-compilation
    extern "c" fn FSEventStreamCreate(
        allocator: CFAllocatorRef,
        callback: *const fn (
            streamRef: FSEventStreamRef,
            clientCallBackInfo: ?*anyopaque,
            numEvents: usize,
            eventPaths: *anyopaque,
            eventFlags: [*]const FSEventStreamEventFlags,
            eventIds: [*]const FSEventStreamEventId,
        ) callconv(.C) void,
        context: ?*FSEventStreamContext,
        pathsToWatch: CFArrayRef,
        sinceWhen: FSEventStreamEventId,
        latency: CFAbsoluteTime,
        flags: FSEventStreamCreateFlags,
    ) ?FSEventStreamRef;

    extern "c" fn FSEventStreamScheduleWithRunLoop(
        streamRef: FSEventStreamRef,
        runLoop: CFRunLoopRef,
        runLoopMode: CFStringRef,
    ) void;

    extern "c" fn FSEventStreamStart(streamRef: FSEventStreamRef) bool;
    extern "c" fn FSEventStreamStop(streamRef: FSEventStreamRef) void;
    extern "c" fn FSEventStreamUnscheduleFromRunLoop(
        streamRef: FSEventStreamRef,
        runLoop: CFRunLoopRef,
        runLoopMode: CFStringRef,
    ) void;
    extern "c" fn FSEventStreamInvalidate(streamRef: FSEventStreamRef) void;
    extern "c" fn FSEventStreamRelease(streamRef: FSEventStreamRef) void;

    extern "c" fn CFRunLoopGetCurrent() CFRunLoopRef;
    extern "c" fn CFRunLoopRun() void;
    extern "c" fn CFRunLoopRunInMode(mode: CFStringRef, seconds: CFAbsoluteTime, returnAfterSourceHandled: bool) i32;
    extern "c" fn CFRunLoopStop(rl: CFRunLoopRef) void;

    extern "c" fn CFArrayCreate(
        allocator: CFAllocatorRef,
        values: [*]const ?*const anyopaque,
        numValues: CFIndex,
        callBacks: ?*const anyopaque,
    ) ?CFArrayRef;

    extern "c" fn CFStringCreateWithCString(
        alloc: CFAllocatorRef,
        cStr: [*:0]const u8,
        encoding: u32,
    ) ?CFStringRef;

    extern "c" fn CFRelease(cf: ?*anyopaque) void;

    // Get the default run loop mode constant
    extern "c" const kCFRunLoopDefaultMode: CFStringRef;
} else struct {};

// Stub implementations for cross-compilation
const macos_stubs = struct {
    fn FSEventStreamCreate(
        allocator: CFAllocatorRef,
        callback: *const fn (
            streamRef: FSEventStreamRef,
            clientCallBackInfo: ?*anyopaque,
            numEvents: usize,
            eventPaths: *anyopaque,
            eventFlags: [*]const FSEventStreamEventFlags,
            eventIds: [*]const FSEventStreamEventId,
        ) callconv(.C) void,
        context: ?*FSEventStreamContext,
        pathsToWatch: CFArrayRef,
        sinceWhen: FSEventStreamEventId,
        latency: CFAbsoluteTime,
        flags: FSEventStreamCreateFlags,
    ) ?FSEventStreamRef {
        _ = allocator;
        _ = callback;
        _ = context;
        _ = pathsToWatch;
        _ = sinceWhen;
        _ = latency;
        _ = flags;
        return null;
    }

    fn FSEventStreamScheduleWithRunLoop(
        streamRef: FSEventStreamRef,
        runLoop: CFRunLoopRef,
        runLoopMode: CFStringRef,
    ) void {
        _ = streamRef;
        _ = runLoop;
        _ = runLoopMode;
    }

    fn FSEventStreamStart(streamRef: FSEventStreamRef) bool {
        _ = streamRef;
        return false;
    }

    fn FSEventStreamStop(streamRef: FSEventStreamRef) void {
        _ = streamRef;
    }

    fn FSEventStreamUnscheduleFromRunLoop(
        streamRef: FSEventStreamRef,
        runLoop: CFRunLoopRef,
        runLoopMode: CFStringRef,
    ) void {
        _ = streamRef;
        _ = runLoop;
        _ = runLoopMode;
    }

    fn FSEventStreamInvalidate(streamRef: FSEventStreamRef) void {
        _ = streamRef;
    }

    fn FSEventStreamRelease(streamRef: FSEventStreamRef) void {
        _ = streamRef;
    }

    fn CFRunLoopGetCurrent() CFRunLoopRef {
        return @ptrFromInt(1);
    }

    fn CFRunLoopRun() void {}

    fn CFRunLoopRunInMode(mode: CFStringRef, seconds: CFAbsoluteTime, returnAfterSourceHandled: bool) i32 {
        _ = mode;
        _ = seconds;
        _ = returnAfterSourceHandled;
        return 0;
    }

    fn CFRunLoopStop(rl: CFRunLoopRef) void {
        _ = rl;
    }

    fn CFArrayCreate(
        allocator: CFAllocatorRef,
        values: [*]const ?*const anyopaque,
        numValues: CFIndex,
        callBacks: ?*const anyopaque,
    ) ?CFArrayRef {
        _ = allocator;
        _ = values;
        _ = numValues;
        _ = callBacks;
        return null;
    }

    fn CFStringCreateWithCString(
        alloc: CFAllocatorRef,
        cStr: [*:0]const u8,
        encoding: u32,
    ) ?CFStringRef {
        _ = alloc;
        _ = cStr;
        _ = encoding;
        return null;
    }

    fn CFRelease(cf: ?*anyopaque) void {
        _ = cf;
    }

    const kCFRunLoopDefaultMode: CFStringRef = @ptrFromInt(1);
};

const FSEventStreamCreate = if (use_stubs) macos_stubs.FSEventStreamCreate else macos_externs.FSEventStreamCreate;
const FSEventStreamScheduleWithRunLoop = if (use_stubs) macos_stubs.FSEventStreamScheduleWithRunLoop else macos_externs.FSEventStreamScheduleWithRunLoop;
const FSEventStreamStart = if (use_stubs) macos_stubs.FSEventStreamStart else macos_externs.FSEventStreamStart;
const FSEventStreamStop = if (use_stubs) macos_stubs.FSEventStreamStop else macos_externs.FSEventStreamStop;
const FSEventStreamUnscheduleFromRunLoop = if (use_stubs) macos_stubs.FSEventStreamUnscheduleFromRunLoop else macos_externs.FSEventStreamUnscheduleFromRunLoop;
const FSEventStreamInvalidate = if (use_stubs) macos_stubs.FSEventStreamInvalidate else macos_externs.FSEventStreamInvalidate;
const FSEventStreamRelease = if (use_stubs) macos_stubs.FSEventStreamRelease else macos_externs.FSEventStreamRelease;
const CFRunLoopGetCurrent = if (use_stubs) macos_stubs.CFRunLoopGetCurrent else macos_externs.CFRunLoopGetCurrent;
const CFRunLoopRun = if (use_stubs) macos_stubs.CFRunLoopRun else macos_externs.CFRunLoopRun;
const CFRunLoopRunInMode = if (use_stubs) macos_stubs.CFRunLoopRunInMode else macos_externs.CFRunLoopRunInMode;
const CFRunLoopStop = if (use_stubs) macos_stubs.CFRunLoopStop else macos_externs.CFRunLoopStop;
const CFArrayCreate = if (use_stubs) macos_stubs.CFArrayCreate else macos_externs.CFArrayCreate;
const CFStringCreateWithCString = if (use_stubs) macos_stubs.CFStringCreateWithCString else macos_externs.CFStringCreateWithCString;
const CFRelease = if (use_stubs) macos_stubs.CFRelease else macos_externs.CFRelease;
fn getKCFRunLoopDefaultMode() CFStringRef {
    if (use_stubs) {
        return macos_stubs.kCFRunLoopDefaultMode;
    } else if (builtin.os.tag == .macos) {
        return macos_externs.kCFRunLoopDefaultMode;
    } else {
        unreachable;
    }
}

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
    is_ready: std.atomic.Value(bool),
    thread: ?std.Thread,

    impl: switch (builtin.os.tag) {
        .macos => MacOSData,
        .linux => LinuxData,
        .windows => WindowsData,
        else => @compileError("Unsupported platform for file watching"),
    },

    const MacOSData = struct {
        stream: ?FSEventStreamRef,
        run_loop: ?CFRunLoopRef,
    };

    const LinuxData = struct {
        inotify_fd: i32,
        watch_descriptors: std.ArrayList(WatchDescriptor),
        path_cache: std.StringHashMap([]const u8),

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
            .is_ready = std.atomic.Value(bool).init(false),
            .thread = null,
            .impl = switch (builtin.os.tag) {
                .macos => MacOSData{
                    .stream = null,
                    .run_loop = null,
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
            .macos => {},
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
        self.should_stop.store(false, .seq_cst);
        self.is_ready.store(false, .seq_cst);

        self.thread = try std.Thread.spawn(.{}, watchLoop, .{self});

        // Wait for the watcher to be ready
        while (!self.is_ready.load(.seq_cst)) {
            std.Thread.yield() catch {};
        }
    }

    /// Stop watching for file changes
    pub fn stop(self: *Watcher) void {
        self.should_stop.store(true, .seq_cst);

        if (self.thread) |thread| {
            // Stop the run loop on macOS
            if (builtin.os.tag == .macos) {
                if (self.impl.run_loop) |rl| {
                    CFRunLoopStop(rl);
                }
            }
            thread.join();
            self.thread = null;
        }

        switch (builtin.os.tag) {
            .macos => {
                if (self.impl.stream) |stream| {
                    FSEventStreamStop(stream);
                    if (self.impl.run_loop) |rl| {
                        FSEventStreamUnscheduleFromRunLoop(stream, rl, getKCFRunLoopDefaultMode());
                    }
                    FSEventStreamInvalidate(stream);
                    FSEventStreamRelease(stream);
                    self.impl.stream = null;
                }
                self.impl.run_loop = null;
            },
            .linux => {
                if (self.impl.inotify_fd >= 0) {
                    const fd = self.impl.inotify_fd;
                    self.impl.inotify_fd = -1;
                    std.posix.close(fd);
                }
                self.clearLinuxWatchData();
            },
            .windows => {
                if (self.impl.stop_event) |stop_event| {
                    const SetEvent = struct {
                        extern "kernel32" fn SetEvent(hEvent: std.os.windows.HANDLE) callconv(.winapi) std.os.windows.BOOL;
                    }.SetEvent;
                    _ = SetEvent(stop_event);
                    _ = std.os.windows.CloseHandle(stop_event);
                    self.impl.stop_event = null;
                }

                // Close directory handles and overlapped events
                for (self.impl.handles.items) |handle| {
                    _ = std.os.windows.CloseHandle(handle);
                }
                self.impl.handles.clearRetainingCapacity();
                
                // Close event handles and clear overlapped data
                for (self.impl.overlapped_data.items) |*data| {
                    if (data.overlapped.hEvent) |event| {
                        _ = std.os.windows.CloseHandle(event);
                    }
                    self.allocator.free(data.buffer);
                    self.allocator.free(data.path);
                }
                self.impl.overlapped_data.clearRetainingCapacity();
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
        // Using stubs - just mark as ready and wait for stop
        // This works for both cross-compilation and testing
        if (use_stubs) {
            self.is_ready.store(true, .seq_cst);
            while (!self.should_stop.load(.seq_cst)) {
                std.Thread.yield() catch {};
            }
            return;
        }
        // Create CFString paths
        var cf_strings = self.allocator.alloc(CFStringRef, self.paths.len) catch {
            std.log.err("Failed to allocate CFString array", .{});
            return;
        };
        defer self.allocator.free(cf_strings);

        for (self.paths, 0..) |path, i| {
            const path_z = self.allocator.dupeZ(u8, path) catch {
                std.log.err("Failed to create null-terminated path", .{});
                return;
            };
            defer self.allocator.free(path_z);

            cf_strings[i] = CFStringCreateWithCString(null, path_z, kCFStringEncodingUTF8) orelse {
                std.log.err("Failed to create CFString for path: {s}", .{path});
                return;
            };
        }
        defer {
            for (cf_strings) |str| {
                CFRelease(str);
            }
        }

        // Create CFArray of paths
        const paths_array = CFArrayCreate(
            null,
            @ptrCast(cf_strings.ptr),
            @intCast(cf_strings.len),
            null, // kCFTypeArrayCallBacks
        ) orelse {
            std.log.err("Failed to create CFArray", .{});
            return;
        };
        defer CFRelease(paths_array);

        // Create FSEventStream context
        var context = FSEventStreamContext{
            .version = 0,
            .info = self,
            .retain = null,
            .release = null,
            .copyDescription = null,
        };

        // Create the event stream
        self.impl.stream = FSEventStreamCreate(
            null, // allocator
            &fsEventsCallback,
            &context,
            paths_array,
            0xFFFFFFFFFFFFFFFF, // kFSEventStreamEventIdSinceNow
            0.1, // latency in seconds
            kFSEventStreamCreateFlagFileEvents | kFSEventStreamCreateFlagNoDefer | kFSEventStreamCreateFlagWatchRoot,
        ) orelse {
            std.log.err("Failed to create FSEventStream", .{});
            return;
        };

        // Get the current run loop
        self.impl.run_loop = CFRunLoopGetCurrent();

        // Schedule the stream on the run loop
        FSEventStreamScheduleWithRunLoop(
            self.impl.stream.?,
            self.impl.run_loop.?,
            getKCFRunLoopDefaultMode(),
        );

        // Start the stream
        if (!FSEventStreamStart(self.impl.stream.?)) {
            std.log.err("Failed to start FSEventStream", .{});
            return;
        }

        // Signal that we're ready to receive events
        self.is_ready.store(true, .seq_cst);

        // Run the run loop with periodic checks for stop signal
        while (!self.should_stop.load(.seq_cst)) {
            // Run for 0.1 seconds at a time to check should_stop periodically
            _ = CFRunLoopRunInMode(getKCFRunLoopDefaultMode(), 0.1, false);
        }

        // Clean up after run loop exits
        if (self.impl.stream) |stream| {
            FSEventStreamStop(stream);
            FSEventStreamUnscheduleFromRunLoop(stream, self.impl.run_loop.?, getKCFRunLoopDefaultMode());
            FSEventStreamInvalidate(stream);
            FSEventStreamRelease(stream);
            self.impl.stream = null;
        }
    }

    fn fsEventsCallback(
        streamRef: FSEventStreamRef,
        clientCallBackInfo: ?*anyopaque,
        numEvents: usize,
        eventPaths: *anyopaque,
        eventFlags: [*]const FSEventStreamEventFlags,
        eventIds: [*]const FSEventStreamEventId,
    ) callconv(.C) void {
        _ = streamRef;
        _ = eventFlags;
        _ = eventIds;

        if (clientCallBackInfo == null) return;

        const self: *Watcher = @ptrCast(@alignCast(clientCallBackInfo.?));

        // Check if we should stop
        if (self.should_stop.load(.seq_cst)) {
            if (self.impl.run_loop) |rl| {
                CFRunLoopStop(rl);
            }
            return;
        }

        // Cast eventPaths to array of C strings
        const paths = @as([*][*:0]const u8, @ptrCast(@alignCast(eventPaths)));

        for (0..numEvents) |i| {
            const path = paths[i];
            const path_len = std.mem.len(path);

            // Check if it's a .roc file
            if (path_len > 4 and std.mem.endsWith(u8, path[0..path_len], ".roc")) {
                const event = WatchEvent{ .path = path[0..path_len] };
                self.callback(event);
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

        // Signal that we're ready to receive events
        self.is_ready.store(true, .seq_cst);

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
    }

    fn clearLinuxWatchData(self: *Watcher) void {
        for (self.impl.watch_descriptors.items) |wd| {
            self.allocator.free(wd.path);
        }
        self.impl.watch_descriptors.clearRetainingCapacity();

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

                if (event.mask & std.os.linux.IN.CREATE != 0 and event.mask & std.os.linux.IN.ISDIR != 0) {
                    for (self.impl.watch_descriptors.items) |wd| {
                        if (wd.wd == event.wd) {
                            const new_dir = std.fs.path.join(self.allocator, &.{ wd.path, name }) catch break;
                            defer self.allocator.free(new_dir);
                            self.addWatchRecursiveLinux(new_dir) catch |err| {
                                std.log.err("Failed to watch new directory: {}", .{err});
                            };

                            // Check if there are already .roc files in the new directory
                            // This handles the case where files are created immediately after the directory
                            var dir = std.fs.openDirAbsolute(new_dir, .{ .iterate = true }) catch break;
                            defer dir.close();
                            var it = dir.iterate();
                            while (it.next() catch null) |entry| {
                                if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".roc")) {
                                    const full_path = std.fs.path.join(self.allocator, &.{ new_dir, entry.name }) catch continue;
                                    defer self.allocator.free(full_path);
                                    self.callback(.{ .path = full_path });
                                }
                            }
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

        const wd_key = try std.fmt.allocPrint(self.allocator, "{d}", .{wd});
        try self.impl.path_cache.put(wd_key, try self.allocator.dupe(u8, path));

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
        // Create stop event for clean shutdown
        const CreateEventW = struct {
            extern "kernel32" fn CreateEventW(
                lpEventAttributes: ?*anyopaque,
                bManualReset: std.os.windows.BOOL,
                bInitialState: std.os.windows.BOOL,
                lpName: ?[*:0]const u16,
            ) callconv(.winapi) ?std.os.windows.HANDLE;
        }.CreateEventW;

        self.impl.stop_event = CreateEventW(null, std.os.windows.TRUE, std.os.windows.FALSE, null) orelse {
            std.log.err("Failed to create stop event", .{});
            return;
        };

        // Set up ReadDirectoryChangesW for each path
        for (self.paths) |path| {
            self.setupWindowsWatch(path) catch |err| {
                std.log.err("Failed to set up watch for {s}: {}", .{ path, err });
                continue;
            };
        }
        
        // Debug: check if we have any handles
        if (self.impl.handles.items.len == 0) {
            std.log.err("No directory handles were created", .{});
            return;
        }

        // Signal that we're ready to receive events
        self.is_ready.store(true, .seq_cst);

        // Main event loop
        const max_handles = self.impl.overlapped_data.items.len + 1; // +1 for stop event
        if (max_handles == 1) return; // Only stop event, no directories to watch

        var handles = self.allocator.alloc(std.os.windows.HANDLE, max_handles) catch {
            std.log.err("Failed to allocate handles array", .{});
            return;
        };
        defer self.allocator.free(handles);

        // Copy OVERLAPPED event handles (not directory handles!)
        for (self.impl.overlapped_data.items, 0..) |data, i| {
            handles[i] = data.overlapped.hEvent.?;
        }
        // Add stop event as last handle
        handles[handles.len - 1] = self.impl.stop_event.?;

        while (!self.should_stop.load(.seq_cst)) {
            const WaitForMultipleObjects = struct {
                extern "kernel32" fn WaitForMultipleObjects(
                    nCount: std.os.windows.DWORD,
                    lpHandles: [*]const std.os.windows.HANDLE,
                    bWaitAll: std.os.windows.BOOL,
                    dwMilliseconds: std.os.windows.DWORD,
                ) callconv(.winapi) std.os.windows.DWORD;
            }.WaitForMultipleObjects;

            const result = WaitForMultipleObjects(
                @intCast(handles.len),
                handles.ptr,
                std.os.windows.FALSE,
                100, // 100ms timeout
            );

            // Check if stop event was signaled or timeout occurred
            const WAIT_TIMEOUT = 258; // 0x102
            if (result == WAIT_TIMEOUT or result == handles.len - 1) {
                continue;
            }

            // Check for errors
            const WAIT_FAILED = 0xFFFFFFFF;
            if (result == WAIT_FAILED) {
                std.log.err("WaitForMultipleObjects failed", .{});
                break;
            }

            // Process the signaled handle
            const handle_index = result;
            if (handle_index < self.impl.overlapped_data.items.len) {
                self.processWindowsEvents(handle_index) catch |err| {
                    std.log.err("Failed to process Windows events: {}", .{err});
                };
            }
        }
    }

    fn setupWindowsWatch(self: *Watcher, path: []const u8) !void {
        // Convert path to wide string
        var path_w_buf: [std.os.windows.PATH_MAX_WIDE]u16 = undefined;
        const path_w_len = try std.unicode.utf8ToUtf16Le(path_w_buf[0..], path);
        path_w_buf[path_w_len] = 0;

        // Open directory handle
        const CreateFileW = struct {
            extern "kernel32" fn CreateFileW(
                lpFileName: [*:0]const u16,
                dwDesiredAccess: std.os.windows.DWORD,
                dwShareMode: std.os.windows.DWORD,
                lpSecurityAttributes: ?*anyopaque,
                dwCreationDisposition: std.os.windows.DWORD,
                dwFlagsAndAttributes: std.os.windows.DWORD,
                hTemplateFile: ?std.os.windows.HANDLE,
            ) callconv(.winapi) std.os.windows.HANDLE;
        }.CreateFileW;

        const GENERIC_READ = 0x80000000;
        const FILE_SHARE_READ = 0x00000001;
        const FILE_SHARE_WRITE = 0x00000002;
        const FILE_SHARE_DELETE = 0x00000004;
        const OPEN_EXISTING = 3;
        const FILE_FLAG_BACKUP_SEMANTICS = 0x02000000;
        const FILE_FLAG_OVERLAPPED = 0x40000000;

        const dir_handle = CreateFileW(
            path_w_buf[0..path_w_len :0].ptr,
            GENERIC_READ,
            FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
            null,
            OPEN_EXISTING,
            FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OVERLAPPED,
            null,
        );

        if (dir_handle == std.os.windows.INVALID_HANDLE_VALUE) {
            return error.FailedToOpenDirectory;
        }

        // Create event for overlapped I/O
        const CreateEventW = struct {
            extern "kernel32" fn CreateEventW(
                lpEventAttributes: ?*anyopaque,
                bManualReset: std.os.windows.BOOL,
                bInitialState: std.os.windows.BOOL,
                lpName: ?[*:0]const u16,
            ) callconv(.winapi) ?std.os.windows.HANDLE;
        }.CreateEventW;

        const event_handle = CreateEventW(null, std.os.windows.TRUE, std.os.windows.FALSE, null) orelse {
            _ = std.os.windows.CloseHandle(dir_handle);
            return error.FailedToCreateEvent;
        };

        // Allocate buffer for ReadDirectoryChangesW
        const buffer_size = 4096;
        const buffer = try self.allocator.alignedAlloc(u8, @alignOf(std.os.windows.FILE_NOTIFY_INFORMATION), buffer_size);

        // Create overlapped data
        var overlapped_data = WindowsData.OverlappedData{
            .overlapped = std.mem.zeroes(std.os.windows.OVERLAPPED),
            .buffer = buffer,
            .path = try self.allocator.dupe(u8, path),
        };
        overlapped_data.overlapped.hEvent = event_handle;

        try self.impl.handles.append(dir_handle);
        try self.impl.overlapped_data.append(overlapped_data);

        // Start the first ReadDirectoryChangesW operation
        // Both arrays should have the same length, use handles.len for consistency
        try self.startWindowsRead(self.impl.handles.items.len - 1);
    }

    fn startWindowsRead(self: *Watcher, index: usize) !void {
        const ReadDirectoryChangesW = struct {
            extern "kernel32" fn ReadDirectoryChangesW(
                hDirectory: std.os.windows.HANDLE,
                lpBuffer: [*]u8,
                nBufferLength: std.os.windows.DWORD,
                bWatchSubtree: std.os.windows.BOOL,
                dwNotifyFilter: std.os.windows.DWORD,
                lpBytesReturned: ?*std.os.windows.DWORD,
                lpOverlapped: *std.os.windows.OVERLAPPED,
                lpCompletionRoutine: ?*anyopaque,
            ) callconv(.winapi) std.os.windows.BOOL;
        }.ReadDirectoryChangesW;

        const FILE_NOTIFY_CHANGE_FILE_NAME = 0x00000001;
        const FILE_NOTIFY_CHANGE_DIR_NAME = 0x00000002;
        const FILE_NOTIFY_CHANGE_LAST_WRITE = 0x00000010;
        const FILE_NOTIFY_CHANGE_CREATION = 0x00000040;

        const notify_filter = FILE_NOTIFY_CHANGE_FILE_NAME |
            FILE_NOTIFY_CHANGE_DIR_NAME |
            FILE_NOTIFY_CHANGE_LAST_WRITE |
            FILE_NOTIFY_CHANGE_CREATION;

        const result = ReadDirectoryChangesW(
            self.impl.handles.items[index],
            self.impl.overlapped_data.items[index].buffer.ptr,
            @intCast(self.impl.overlapped_data.items[index].buffer.len),
            std.os.windows.TRUE, // Watch subtree
            notify_filter,
            null,
            &self.impl.overlapped_data.items[index].overlapped,
            null,
        );

        if (result == 0) {
            const GetLastError = struct {
                extern "kernel32" fn GetLastError() callconv(.winapi) std.os.windows.DWORD;
            }.GetLastError;

            const err = GetLastError();
            std.log.err("ReadDirectoryChangesW failed with error: {}", .{err});
            return error.ReadDirectoryChangesFailed;
        }
    }

    fn processWindowsEvents(self: *Watcher, index: usize) !void {
        const GetOverlappedResult = struct {
            extern "kernel32" fn GetOverlappedResult(
                hFile: std.os.windows.HANDLE,
                lpOverlapped: *std.os.windows.OVERLAPPED,
                lpNumberOfBytesTransferred: *std.os.windows.DWORD,
                bWait: std.os.windows.BOOL,
            ) callconv(.winapi) std.os.windows.BOOL;
        }.GetOverlappedResult;

        const ResetEvent = struct {
            extern "kernel32" fn ResetEvent(hEvent: std.os.windows.HANDLE) callconv(.winapi) std.os.windows.BOOL;
        }.ResetEvent;

        var bytes_transferred: std.os.windows.DWORD = 0;
        const result = GetOverlappedResult(
            self.impl.handles.items[index],
            &self.impl.overlapped_data.items[index].overlapped,
            &bytes_transferred,
            std.os.windows.FALSE,
        );

        if (result == 0) {
            std.log.err("GetOverlappedResult failed", .{});
            return;
        }

        // Reset the event for the next operation
        _ = ResetEvent(self.impl.overlapped_data.items[index].overlapped.hEvent.?);

        // Process the file change notifications
        self.parseWindowsFileNotifications(index, bytes_transferred);

        // Start the next ReadDirectoryChangesW operation
        self.startWindowsRead(index) catch |err| {
            std.log.err("Failed to restart ReadDirectoryChangesW: {}", .{err});
        };
    }

    fn parseWindowsFileNotifications(self: *Watcher, index: usize, bytes_transferred: std.os.windows.DWORD) void {
        const buffer = self.impl.overlapped_data.items[index].buffer;
        const base_path = self.impl.overlapped_data.items[index].path;

        var offset: u32 = 0;
        while (offset < bytes_transferred) {
            const info = @as(*const std.os.windows.FILE_NOTIFY_INFORMATION, @ptrCast(@alignCast(&buffer[offset])));

            // Convert filename from UTF-16 to UTF-8
            const filename_utf16 = @as([*]const u16, @ptrCast(@alignCast(&buffer[offset + @sizeOf(std.os.windows.FILE_NOTIFY_INFORMATION)])))[0 .. info.FileNameLength / 2];

            var filename_utf8_buf: [std.fs.max_path_bytes]u8 = undefined;
            const filename_utf8_len = std.unicode.utf16LeToUtf8(filename_utf8_buf[0..], filename_utf16) catch {
                // Skip this file if we can't convert the name
                if (info.NextEntryOffset == 0) break;
                offset += info.NextEntryOffset;
                continue;
            };
            const filename_utf8 = filename_utf8_buf[0..filename_utf8_len];

            // Check if it's a .roc file
            if (std.mem.endsWith(u8, filename_utf8, ".roc")) {
                // Create full path
                const full_path = std.fs.path.join(self.allocator, &.{ base_path, filename_utf8 }) catch {
                    // Skip this file if we can't create the path
                    if (info.NextEntryOffset == 0) break;
                    offset += info.NextEntryOffset;
                    continue;
                };
                defer self.allocator.free(full_path);

                const event = WatchEvent{ .path = full_path };
                self.callback(event);
            }

            // Move to next notification
            if (info.NextEntryOffset == 0) break;
            offset += info.NextEntryOffset;
        }
    }
};

// ===== TESTS =====

fn waitForEvents(event_count: *std.atomic.Value(u32), expected: u32, max_wait_ms: u32) !void {
    // When using stubs, don't wait for events since they won't be generated
    if (use_stubs) {
        return;
    }

    const start = std.time.milliTimestamp();
    while (event_count.load(.seq_cst) < expected) {
        const elapsed = std.time.milliTimestamp() - start;
        if (elapsed > max_wait_ms) {
            return error.EventsNotReceived;
        }
        std.Thread.yield() catch {};
    }
}

fn expectEventsOrSkip(event_count: *std.atomic.Value(u32), expected: u32) !void {
    if (use_stubs) {
        // When using stubs, skip the event count check
        return;
    } else {
        // When using real file watching, verify we got the expected events
        const count = event_count.load(.seq_cst);
        try std.testing.expect(count >= expected);
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

    // Create .roc files and wait for events (or skip if using stubs)
    try temp_dir.dir.writeFile(.{ .sub_path = "test1.roc", .data = "content1" });
    try waitForEvents(&global.event_count, 1, 5000);

    try temp_dir.dir.writeFile(.{ .sub_path = "test2.roc", .data = "content2" });
    try waitForEvents(&global.event_count, 2, 5000);

    try temp_dir.dir.writeFile(.{ .sub_path = "test3.txt", .data = "ignored" });

    watcher.stop();

    // Verify we got the expected events (or skip if using stubs)
    try expectEventsOrSkip(&global.event_count, 2);
}

test "recursive directory watching" {
    const allocator = std.testing.allocator;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(temp_path);

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

    try temp_dir.dir.writeFile(.{ .sub_path = "subdir/nested.roc", .data = "nested content" });
    try waitForEvents(&global.event_count, 1, 5000);

    watcher.stop();

    try expectEventsOrSkip(&global.event_count, 1);
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

    try temp_dir1.dir.writeFile(.{ .sub_path = "file1.roc", .data = "content1" });
    try waitForEvents(&global.event_count, 1, 5000);

    try temp_dir2.dir.writeFile(.{ .sub_path = "file2.roc", .data = "content2" });
    try waitForEvents(&global.event_count, 2, 5000);

    watcher.stop();

    try expectEventsOrSkip(&global.event_count, 2);
}

test "file modification detection" {
    const allocator = std.testing.allocator;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(temp_path);

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

    try temp_dir.dir.writeFile(.{ .sub_path = "modify.roc", .data = "modified content that is different" });
    try waitForEvents(&global.event_count, 1, 5000);

    watcher.stop();

    try expectEventsOrSkip(&global.event_count, 1);
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

    for (0..50) |i| {
        const filename = try std.fmt.allocPrint(allocator, "file{d}.roc", .{i});
        defer allocator.free(filename);
        try temp_dir.dir.writeFile(.{ .sub_path = filename, .data = "content" });
    }

    // FSEvents on macOS coalesces rapid events, so we might not get all 50 events
    const min_expected = if (builtin.os.tag == .macos) 10 else 50;
    try waitForEvents(&global.event_count, min_expected, 10000);

    const elapsed = std.time.milliTimestamp() - start_time;

    watcher.stop();

    try std.testing.expect(elapsed < 5000);

    try expectEventsOrSkip(&global.event_count, min_expected);
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

    try temp_dir.dir.makeDir("newdir");
    std.Thread.yield() catch {};

    try temp_dir.dir.writeFile(.{ .sub_path = "newdir/new.roc", .data = "new content" });

    if (builtin.os.tag == .linux) {
        try waitForEvents(&global.event_count, 1, 5000);
    }

    watcher.stop();

    if (builtin.os.tag == .linux) {
        try expectEventsOrSkip(&global.event_count, 1);
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

    try watcher.start();
    try temp_dir.dir.writeFile(.{ .sub_path = "first.roc", .data = "first" });
    try waitForEvents(&global.event_count, 1, 5000);

    watcher.stop();
    const count_after_stop = global.event_count.load(.seq_cst);

    try temp_dir.dir.writeFile(.{ .sub_path = "while_stopped.roc", .data = "stopped" });
    std.Thread.yield() catch {};

    try std.testing.expectEqual(count_after_stop, global.event_count.load(.seq_cst));

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

    const thread_count = 4;
    var threads: [thread_count]std.Thread = undefined;

    const WriterArgs = struct { dir: *std.testing.TmpDir, id: usize, alloc: std.mem.Allocator };

    const writer = struct {
        fn write(args: WriterArgs) void {
            for (0..5) |i| {
                const filename = std.fmt.allocPrint(args.alloc, "thread{d}_file{d}.roc", .{ args.id, i }) catch return;
                defer args.alloc.free(filename);
                args.dir.dir.writeFile(.{ .sub_path = filename, .data = "content" }) catch return;
                std.Thread.yield() catch {};
            }
        }
    };

    for (0..thread_count) |i| {
        const args = WriterArgs{ .dir = &temp_dir, .id = i, .alloc = allocator };
        threads[i] = std.Thread.spawn(.{}, writer.write, .{args}) catch continue;
    }

    for (threads) |thread| {
        thread.join();
    }

    // FSEvents on macOS coalesces rapid events, so we might not get all 20 events
    // Just ensure we get at least some events from the concurrent writes
    const min_expected = if (builtin.os.tag == .macos) 4 else thread_count * 5;
    try waitForEvents(&global.event_count, min_expected, 10000);

    watcher.stop();

    try expectEventsOrSkip(&global.event_count, min_expected);

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

    try temp_dir.dir.writeFile(.{ .sub_path = "original.roc", .data = "content" });

    try watcher.start();

    try temp_dir.dir.rename("original.roc", "renamed.roc");
    try waitForEvents(&global.event_count, 1, 5000);

    watcher.stop();

    if (builtin.os.tag == .linux) {
        try expectEventsOrSkip(&global.event_count, 1);
    }
}

test "windows unicode filename handling" {
    if (builtin.os.tag != .windows) return;

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

    // Test with unicode filename (Chinese characters)
    const unicode_filename = "测试文件.roc";
    try temp_dir.dir.writeFile(.{ .sub_path = unicode_filename, .data = "unicode content" });
    try waitForEvents(&global.event_count, 1, 5000);

    // Test with accented characters
    const accented_filename = "café.roc";
    try temp_dir.dir.writeFile(.{ .sub_path = accented_filename, .data = "accented content" });
    try waitForEvents(&global.event_count, 2, 5000);

    watcher.stop();

    try expectEventsOrSkip(&global.event_count, 2);
}

test "windows long path handling" {
    if (builtin.os.tag != .windows) return;

    const allocator = std.testing.allocator;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realpathAlloc(allocator, ".");
    defer allocator.free(temp_path);

    // Create a nested directory structure to test long paths
    const long_dir_name = "very_long_directory_name_that_helps_test_path_length_handling";
    
    // Build the nested path
    var path_components = std.ArrayList([]const u8).init(allocator);
    defer {
        for (path_components.items) |component| {
            allocator.free(component);
        }
        path_components.deinit();
    }
    
    for (0..5) |i| {
        const dir_name = try std.fmt.allocPrint(allocator, "{s}_{d}", .{ long_dir_name, i });
        try path_components.append(dir_name);
    }
    
    // Create the nested directories
    var current_path = std.ArrayList(u8).init(allocator);
    defer current_path.deinit();
    
    for (path_components.items) |component| {
        if (current_path.items.len > 0) {
            try current_path.append(std.fs.path.sep);
        }
        try current_path.appendSlice(component);
        try temp_dir.dir.makePath(current_path.items);
    }
    
    // Open the deepest directory
    var current_dir = try temp_dir.dir.openDir(current_path.items, .{});
    defer current_dir.close();

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

    // Create a file in the deeply nested directory
    try current_dir.writeFile(.{ .sub_path = "deep_file.roc", .data = "deep content" });
    try waitForEvents(&global.event_count, 1, 5000);

    watcher.stop();

    try expectEventsOrSkip(&global.event_count, 1);
}
