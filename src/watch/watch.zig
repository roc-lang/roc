//! File system watcher for monitoring .roc file changes across platforms.
//! Provides efficient, cross-platform file watching with recursive directory support.

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const build_options = @import("build_options");
const Inputs = @import("Inputs.zig");

// win32.OVERLAPPED and FILE_NOTIFY_INFORMATION were removed in Zig 0.16.
// Define the layouts we need locally; these are only referenced on Windows.
const win32 = struct {
    const DWORD = std.os.windows.DWORD;
    const HANDLE = std.os.windows.HANDLE;
    const ULONG_PTR = std.os.windows.ULONG_PTR;

    const OVERLAPPED = extern struct {
        Internal: ULONG_PTR,
        InternalHigh: ULONG_PTR,
        DUMMYUNIONNAME: extern union {
            DUMMYSTRUCTNAME: extern struct {
                Offset: DWORD,
                OffsetHigh: DWORD,
            },
            Pointer: ?*anyopaque,
        },
        hEvent: ?HANDLE,
    };

    extern "kernel32" fn GetOverlappedResult(
        hFile: HANDLE,
        lpOverlapped: *OVERLAPPED,
        lpNumberOfBytesTransferred: *DWORD,
        bWait: std.os.windows.BOOL,
    ) callconv(.winapi) std.os.windows.BOOL;

    extern "kernel32" fn CancelIoEx(
        hFile: HANDLE,
        lpOverlapped: *OVERLAPPED,
    ) callconv(.winapi) std.os.windows.BOOL;

    const FILE_NOTIFY_INFORMATION = extern struct {
        NextEntryOffset: DWORD,
        Action: DWORD,
        FileNameLength: DWORD,
        // Followed by FileName: [FileNameLength/2]WCHAR at offset @sizeOf(FILE_NOTIFY_INFORMATION)
    };
};

// Use real FSEvents only when building natively for macOS
// Cross-compilation detection: Use build system's isNative() check
const target_is_macos = builtin.os.tag == .macos;
const target_is_native = build_options.target_is_native;
const use_real_fsevents = target_is_macos and target_is_native;
const use_stubs = !use_real_fsevents;
const active_watcher_backend_is_stub = builtin.os.tag == .macos and use_stubs;

fn bumpEventCount(comptime Global: type) void {
    const previous = Global.event_count.fetchAdd(1, .seq_cst);
    if (comptime builtin.mode == .Debug) {
        std.debug.assert(previous != std.math.maxInt(u32));
    } else if (previous == std.math.maxInt(u32)) {
        unreachable;
    }
}

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
        ) callconv(.c) void,
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
        _: CFAllocatorRef,
        _: *const fn (
            streamRef: FSEventStreamRef,
            clientCallBackInfo: ?*anyopaque,
            numEvents: usize,
            eventPaths: *anyopaque,
            eventFlags: [*]const FSEventStreamEventFlags,
            eventIds: [*]const FSEventStreamEventId,
        ) callconv(.c) void,
        _: ?*FSEventStreamContext,
        _: CFArrayRef,
        _: FSEventStreamEventId,
        _: CFAbsoluteTime,
        _: FSEventStreamCreateFlags,
    ) ?FSEventStreamRef {
        return null;
    }

    fn FSEventStreamScheduleWithRunLoop(
        _: FSEventStreamRef,
        _: CFRunLoopRef,
        _: CFStringRef,
    ) void {}

    fn FSEventStreamStart(_: FSEventStreamRef) bool {
        return false;
    }

    fn FSEventStreamStop(_: FSEventStreamRef) void {}

    fn FSEventStreamUnscheduleFromRunLoop(
        _: FSEventStreamRef,
        _: CFRunLoopRef,
        _: CFStringRef,
    ) void {}

    fn FSEventStreamInvalidate(_: FSEventStreamRef) void {}

    fn FSEventStreamRelease(_: FSEventStreamRef) void {}

    fn CFRunLoopGetCurrent() CFRunLoopRef {
        return @ptrFromInt(1);
    }

    fn CFRunLoopRun() void {}

    fn CFRunLoopRunInMode(_: CFStringRef, _: CFAbsoluteTime, _: bool) i32 {
        return 0;
    }

    fn CFRunLoopStop(_: CFRunLoopRef) void {}

    fn CFArrayCreate(
        _: CFAllocatorRef,
        _: [*]const ?*const anyopaque,
        _: CFIndex,
        _: ?*const anyopaque,
    ) ?CFArrayRef {
        return null;
    }

    fn CFStringCreateWithCString(
        _: CFAllocatorRef,
        _: [*:0]const u8,
        _: u32,
    ) ?CFStringRef {
        return null;
    }

    fn CFRelease(_: ?*anyopaque) void {}

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
    /// Coverage may have changed, or the backend lost individual events.
    rescan: bool = false,
};

/// Callback function type for handling file change events
pub const WatchCallback = *const fn (event: WatchEvent) void;

/// Callback function type for handling file change events with caller-owned context.
pub const WatchCallbackWithContext = *const fn (context: ?*anyopaque, event: WatchEvent) void;

const EventFilter = enum {
    roc_files,
    all_files,
};

const WatcherOs = enum {
    macos,
    linux,
    windows,
    kqueue,
};

const watcher_os: WatcherOs = switch (builtin.os.tag) {
    .macos => .macos,
    .linux => .linux,
    .windows => .windows,
    .freebsd, .openbsd, .netbsd, .dragonfly => .kqueue,
    .freestanding,
    .other,
    .contiki,
    .fuchsia,
    .hermit,
    .managarm,
    .haiku,
    .hurd,
    .illumos,
    .plan9,
    .rtems,
    .serenity,
    .driverkit,
    .ios,
    .maccatalyst,
    .tvos,
    .visionos,
    .watchos,
    .uefi,
    .@"3ds",
    .ps3,
    .ps4,
    .ps5,
    .psp,
    .vita,
    .emscripten,
    .wasi,
    .amdhsa,
    .amdpal,
    .cuda,
    .mesa3d,
    .nvcl,
    .opencl,
    .opengl,
    .vulkan,
    => @compileError("Unsupported platform for file watching"),
};

/// High-performance filesystem watcher for .roc files
/// Supports recursive directory watching and exact compiler input coverage.
pub const Watcher = struct {
    allocator: std.mem.Allocator,
    std_io: std.Io,
    paths: [][]const u8,
    callback: ?WatchCallback,
    callback_with_context: ?WatchCallbackWithContext,
    callback_context: ?*anyopaque,
    event_filter: EventFilter,
    recursive: bool,
    input_plan: ?Inputs = null,
    dirty_inputs: []std.atomic.Value(bool) = &.{},
    needs_refresh: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),
    backend_failed: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),
    should_stop: std.atomic.Value(bool),
    is_ready: std.atomic.Value(bool),
    startup_failed: std.atomic.Value(bool),
    thread: ?std.Thread,

    impl: switch (watcher_os) {
        .macos => MacOSData,
        .linux => LinuxData,
        .windows => WindowsData,
        .kqueue => KqueueData,
    },

    const MacOSData = struct {
        stream: ?FSEventStreamRef,
        run_loop: ?CFRunLoopRef,
        exact: KqueueData,
    };

    const LinuxData = struct {
        inotify_fd: i32,
        watch_descriptors: std.array_list.Managed(WatchDescriptor),
        descriptor_indices: std.AutoHashMap(i32, std.ArrayList(usize)),

        const WatchDescriptor = struct {
            wd: i32,
            path: []const u8,
        };
    };

    /// BSD vnode events are delivered per open file descriptor rather than per
    /// path, and a directory descriptor only reports changes to the directory's
    /// own entry list. Detecting writes to an existing file therefore requires a
    /// descriptor for that file as well, so this holds one registration per
    /// watched directory and per watched file.
    const KqueueData = struct {
        kq: i32,
        watches: std.array_list.Managed(VnodeWatch),

        const VnodeWatch = struct {
            fd: i32,
            path: []const u8,
            is_dir: bool,
            mtime: i96 = 0,
            ctime: i96 = 0,
        };
    };

    const WindowsData = struct {
        handles: std.array_list.Managed(std.os.windows.HANDLE),
        overlapped_data: std.array_list.Managed(OverlappedData),
        stop_event: ?std.os.windows.HANDLE,

        const OverlappedData = struct {
            overlapped: win32.OVERLAPPED,
            read_pending: bool = false,
            buffer: []align(@alignOf(win32.FILE_NOTIFY_INFORMATION)) u8,
            path: []const u8,
        };
    };

    /// Initialize a new file watcher
    pub fn init(allocator: std.mem.Allocator, std_io: std.Io, paths: []const []const u8, callback: WatchCallback) Allocator.Error!*Watcher {
        return initWithFilter(allocator, std_io, paths, .{
            .callback = callback,
            .event_filter = .roc_files,
            .recursive = true,
        });
    }

    /// Report immediate entries of these directories, without traversing their
    /// subdirectories. Compiler consumers should use initInputs instead.
    pub fn initAllFiles(
        allocator: std.mem.Allocator,
        std_io: std.Io,
        paths: []const []const u8,
        context: ?*anyopaque,
        callback: WatchCallbackWithContext,
    ) Allocator.Error!*Watcher {
        return initWithFilter(allocator, std_io, paths, .{
            .callback_with_context = callback,
            .callback_context = context,
            .event_filter = .all_files,
        });
    }

    /// Watch explicit files, including missing files and symlink targets.
    /// Directory coverage and event routing derive exclusively from these paths.
    pub fn initInputs(
        allocator: Allocator,
        io: std.Io,
        paths: []const []const u8,
        context: ?*anyopaque,
        callback: WatchCallbackWithContext,
    ) Inputs.Error!*Watcher {
        const self = try initAllFiles(allocator, io, paths, context, callback);
        errdefer self.deinit();
        self.input_plan = try Inputs.init(allocator, io, paths);
        self.dirty_inputs = try allocator.alloc(std.atomic.Value(bool), paths.len);
        for (self.dirty_inputs) |*dirty| dirty.* = std.atomic.Value(bool).init(false);
        return self;
    }

    pub fn takeInputChange(self: *Watcher, index: usize) bool {
        return self.dirty_inputs[index].swap(false, .seq_cst);
    }

    pub fn takeCoverageChange(self: *Watcher) bool {
        return self.needs_refresh.swap(false, .seq_cst);
    }

    pub fn hasBackendFailed(self: *Watcher) bool {
        return self.backend_failed.load(.seq_cst);
    }

    fn failBackend(self: *Watcher) void {
        self.backend_failed.store(true, .seq_cst);
        self.emitEvent(.{ .path = "", .rescan = true });
        self.should_stop.store(true, .seq_cst);
        self.signalWindowsStopEvent();
    }

    fn directoryPaths(self: *Watcher) Allocator.Error![]const []const u8 {
        var paths: std.ArrayList([]const u8) = .empty;
        errdefer paths.deinit(self.allocator);
        if (self.input_plan) |*plan| {
            for (plan.nodes.keys(), plan.nodes.values()) |path, node| {
                if (node.kind == .directory) try paths.append(self.allocator, path);
            }
        } else try paths.appendSlice(self.allocator, self.paths);
        return paths.toOwnedSlice(self.allocator);
    }

    const InitOptions = struct {
        callback: ?WatchCallback = null,
        callback_with_context: ?WatchCallbackWithContext = null,
        callback_context: ?*anyopaque = null,
        event_filter: EventFilter,
        recursive: bool = false,
    };

    fn initWithFilter(
        allocator: std.mem.Allocator,
        std_io: std.Io,
        paths: []const []const u8,
        options: InitOptions,
    ) Allocator.Error!*Watcher {
        const watcher = try allocator.create(Watcher);
        errdefer allocator.destroy(watcher);

        var paths_copy = try allocator.alloc([]const u8, paths.len);
        errdefer allocator.free(paths_copy);

        var copied: usize = 0;
        errdefer for (paths_copy[0..copied]) |path| allocator.free(path);
        for (paths, 0..) |path, i| {
            paths_copy[i] = try allocator.dupe(u8, path);
            copied += 1;
        }

        watcher.* = .{
            .allocator = allocator,
            .std_io = std_io,
            .paths = paths_copy,
            .callback = options.callback,
            .callback_with_context = options.callback_with_context,
            .callback_context = options.callback_context,
            .event_filter = options.event_filter,
            .recursive = options.recursive,
            .should_stop = std.atomic.Value(bool).init(false),
            .is_ready = std.atomic.Value(bool).init(false),
            .startup_failed = std.atomic.Value(bool).init(false),
            .thread = null,
            .impl = switch (watcher_os) {
                .macos => MacOSData{
                    .stream = null,
                    .run_loop = null,
                    .exact = .{ .kq = -1, .watches = std.array_list.Managed(KqueueData.VnodeWatch).init(allocator) },
                },
                .linux => LinuxData{
                    .inotify_fd = -1,
                    .watch_descriptors = std.array_list.Managed(LinuxData.WatchDescriptor).init(allocator),
                    .descriptor_indices = std.AutoHashMap(i32, std.ArrayList(usize)).init(allocator),
                },
                .windows => WindowsData{
                    .handles = std.array_list.Managed(std.os.windows.HANDLE).init(allocator),
                    .overlapped_data = std.array_list.Managed(WindowsData.OverlappedData).init(allocator),
                    .stop_event = null,
                },
                .kqueue => KqueueData{
                    .kq = -1,
                    .watches = std.array_list.Managed(KqueueData.VnodeWatch).init(allocator),
                },
            },
        };

        return watcher;
    }

    fn shouldEmitPath(self: *Watcher, path: []const u8, is_dir: bool) bool {
        return switch (self.event_filter) {
            .roc_files => !is_dir and std.mem.endsWith(u8, path, ".roc"),
            .all_files => true,
        };
    }

    fn emitEvent(self: *Watcher, event: WatchEvent) void {
        if (self.input_plan) |*plan| {
            if (event.path.len == 0) {
                for (self.dirty_inputs) |*dirty| dirty.store(true, .seq_cst);
                self.needs_refresh.store(true, .seq_cst);
            } else {
                if (builtin.os.tag == .windows) {
                    const indices = plan.windows_names.get(event.path) orelse return;
                    for (indices.items) |index| self.markInputNode(plan.nodes.values()[index], event.rescan);
                } else {
                    const node = plan.nodes.get(event.path) orelse return;
                    self.markInputNode(node, event.rescan);
                }
            }
        }
        if (self.callback_with_context) |callback| {
            callback(self.callback_context, event);
        } else if (self.callback) |callback| {
            callback(event);
        }
    }

    fn markInputNode(self: *Watcher, node: Inputs.Node, rescan: bool) void {
        for (node.inputs.items) |index| self.dirty_inputs[index].store(true, .seq_cst);
        if (node.topology or rescan) self.needs_refresh.store(true, .seq_cst);
    }

    fn linuxCoverageChanged(self: *Watcher, path: []const u8, mask: u32) bool {
        if (mask & (std.os.linux.IN.CREATE | std.os.linux.IN.DELETE | std.os.linux.IN.MOVED_FROM | std.os.linux.IN.MOVED_TO |
            std.os.linux.IN.DELETE_SELF | std.os.linux.IN.MOVE_SELF | std.os.linux.IN.IGNORED | std.os.linux.IN.UNMOUNT) != 0) return true;
        if (mask & std.os.linux.IN.ATTRIB != 0) {
            if (self.input_plan) |*plan| return plan.entryChanged(path);
        }
        return false;
    }

    fn markReady(self: *Watcher) void {
        self.is_ready.store(true, .seq_cst);
    }

    fn markStartupFailed(self: *Watcher) void {
        self.startup_failed.store(true, .seq_cst);
    }

    /// Clean up all resources
    pub fn deinit(self: *Watcher) void {
        self.stop();
        if (self.input_plan) |*plan| plan.deinit();
        self.allocator.free(self.dirty_inputs);

        for (self.paths) |path| {
            self.allocator.free(path);
        }
        self.allocator.free(self.paths);

        switch (watcher_os) {
            .macos => self.impl.exact.watches.deinit(),
            .linux => {
                for (self.impl.watch_descriptors.items) |wd| {
                    self.allocator.free(wd.path);
                }
                self.impl.watch_descriptors.deinit();

                var it = self.impl.descriptor_indices.valueIterator();
                while (it.next()) |indices| indices.deinit(self.allocator);
                self.impl.descriptor_indices.deinit();
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
            .kqueue => {
                // stop() already closed the descriptors and emptied the list.
                self.kqueueData().watches.deinit();
            },
        }

        self.allocator.destroy(self);
    }

    /// Start watching for file changes
    pub fn start(self: *Watcher) (std.Thread.SpawnError || error{ AlreadyStarted, WatchBackendFailed })!void {
        while (true) {
            try self.startOnce();
            if (self.input_plan) |*plan| {
                var current = Inputs.init(self.allocator, self.std_io, self.paths) catch |err| {
                    self.stop();
                    return err;
                };
                if (!plan.eql(&current)) {
                    self.stop();
                    plan.deinit();
                    plan.* = current;
                    continue;
                }
                current.deinit();
            }
            return;
        }
    }

    fn startOnce(self: *Watcher) (std.Thread.SpawnError || error{ AlreadyStarted, WatchBackendFailed })!void {
        if (self.thread != null) return error.AlreadyStarted;
        self.should_stop.store(false, .seq_cst);
        self.is_ready.store(false, .seq_cst);
        self.startup_failed.store(false, .seq_cst);
        self.backend_failed.store(false, .seq_cst);

        self.thread = try std.Thread.spawn(.{}, watchLoop, .{self});

        // Wait for the watcher to be ready
        while (!self.is_ready.load(.seq_cst)) {
            if (self.startup_failed.load(.seq_cst)) {
                // Release partial registrations, including any submitted reads,
                // before a caller can retry startup and grow the arrays again.
                self.stop();
                return error.WatchBackendFailed;
            }
            std.Thread.yield() catch {};
        }
    }

    /// Stop watching for file changes
    pub fn stop(self: *Watcher) void {
        self.should_stop.store(true, .seq_cst);
        self.signalWindowsStopEvent();

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

        switch (watcher_os) {
            .macos => {
                if (self.impl.exact.kq >= 0) {
                    _ = std.c.close(self.impl.exact.kq);
                    self.impl.exact.kq = -1;
                    self.clearKqueueWatchData();
                }
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
                    _ = std.os.linux.close(fd);
                }
                self.clearLinuxWatchData();
            },
            .windows => {
                if (self.impl.stop_event) |stop_event| {
                    _ = std.os.windows.CloseHandle(stop_event);
                    self.impl.stop_event = null;
                }

                // Shards have joined, so no thread can rearm these reads. Keep
                // their storage and events alive until cancellation completes.
                for (self.impl.handles.items, 0..) |handle, index| {
                    self.cancelWindowsRead(index);
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
            .kqueue => {
                if (self.kqueueData().kq >= 0) {
                    const kq = self.kqueueData().kq;
                    self.kqueueData().kq = -1;
                    _ = std.c.close(kq);
                }
                self.clearKqueueWatchData();
            },
        }
    }

    fn watchLoop(self: *Watcher) void {
        switch (watcher_os) {
            .macos => if (self.input_plan != null and !use_stubs) self.watchLoopKqueue() else self.watchLoopMacOS(),
            .linux => self.watchLoopLinux(),
            .windows => self.watchLoopWindows(),
            .kqueue => self.watchLoopKqueue(),
        }
    }

    fn signalWindowsStopEvent(self: *Watcher) void {
        if (comptime builtin.os.tag == .windows) {
            if (self.impl.stop_event) |stop_event| {
                const SetEvent = struct {
                    extern "kernel32" fn SetEvent(hEvent: std.os.windows.HANDLE) callconv(.winapi) std.os.windows.BOOL;
                }.SetEvent;
                _ = SetEvent(stop_event);
            }
        }
    }

    fn watchLoopMacOS(self: *Watcher) void {
        // Using stubs - just mark as ready and wait for stop
        // This works for both cross-compilation and testing
        if (use_stubs) {
            self.markReady();
            while (!self.should_stop.load(.seq_cst)) {
                std.Thread.yield() catch {};
            }
            return;
        }
        const watch_paths = self.directoryPaths() catch {
            self.markStartupFailed();
            return;
        };
        defer self.allocator.free(watch_paths);
        // Create CFString paths
        var cf_strings = self.allocator.alloc(CFStringRef, watch_paths.len) catch {
            std.log.warn("Failed to allocate CFString array", .{});
            self.markStartupFailed();
            return;
        };
        defer self.allocator.free(cf_strings);

        for (watch_paths, 0..) |path, i| {
            const path_z = self.allocator.dupeZ(u8, path) catch {
                std.log.warn("Failed to create null-terminated path", .{});
                self.markStartupFailed();
                return;
            };
            defer self.allocator.free(path_z);

            cf_strings[i] = CFStringCreateWithCString(null, path_z, kCFStringEncodingUTF8) orelse {
                std.log.warn("Failed to create CFString for path: {s}", .{path});
                self.markStartupFailed();
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
            std.log.warn("Failed to create CFArray", .{});
            self.markStartupFailed();
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
            std.log.warn("Failed to create FSEventStream", .{});
            self.markStartupFailed();
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
            std.log.warn("Failed to start FSEventStream", .{});
            self.markStartupFailed();
            return;
        }

        // Signal that we're ready to receive events
        self.markReady();

        // Run the run loop with periodic checks for stop signal
        while (!self.should_stop.load(.seq_cst)) {
            // Run for 0.1 seconds at a time to check should_stop periodically
            const run_result = CFRunLoopRunInMode(getKCFRunLoopDefaultMode(), 0.1, false);
            if (comptime builtin.mode == .Debug) {
                std.debug.assert(run_result >= 0);
            } else if (run_result < 0) {
                unreachable;
            }
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
        _: FSEventStreamRef,
        clientCallBackInfo: ?*anyopaque,
        numEvents: usize,
        eventPaths: *anyopaque,
        flags: [*]const FSEventStreamEventFlags,
        _: [*]const FSEventStreamEventId,
    ) callconv(.c) void {
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
            // MustScanSubDirs, UserDropped, KernelDropped, EventIdsWrapped,
            // RootChanged: exact event names are no longer sufficient.
            if (flags[i] & 0x2f != 0) {
                self.emitEvent(.{ .path = "", .rescan = true });
                continue;
            }
            const path = paths[i];
            const path_len = std.mem.len(path);

            if (self.shouldEmitPath(path[0..path_len], false)) {
                const event = WatchEvent{ .path = path[0..path_len], .rescan = flags[i] & (0x100 | 0x200 | 0x800) != 0 };
                self.emitEvent(event);
            }
        }
    }

    fn watchLoopLinux(self: *Watcher) void {
        const init_result = std.os.linux.inotify_init1(std.os.linux.IN.NONBLOCK | std.os.linux.IN.CLOEXEC);
        const init_errno = std.os.linux.errno(init_result);
        if (init_errno != .SUCCESS) {
            std.log.warn("inotify_init1 failed: {}", .{init_errno});
            self.markStartupFailed();
            return;
        }
        self.impl.inotify_fd = @as(i32, @intCast(init_result));

        const watch_paths = self.directoryPaths() catch {
            self.markStartupFailed();
            return;
        };
        defer self.allocator.free(watch_paths);
        // Add watches
        for (watch_paths) |path| {
            self.addWatchRecursiveLinux(path) catch |err| {
                std.log.warn("Failed to watch {s}: {}", .{ path, err });
                self.markStartupFailed();
                return;
            };
        }
        if (self.input_plan) |*plan| {
            // Directory events alone do not cover writes through another hard
            // link. Observe each explicit file inode as well as its directory
            // entry; replacement is still covered by the parent registration.
            for (plan.nodes.keys(), plan.nodes.values()) |path, node| {
                if (node.kind != .file) continue;
                self.addWatchRecursiveLinux(path) catch |err| {
                    std.log.warn("Failed to watch input {s}: {}", .{ path, err });
                    self.markStartupFailed();
                    return;
                };
            }
        }

        // Signal that we're ready to receive events
        self.markReady();

        // Main event loop
        var buffer: [8192]u8 align(@alignOf(std.os.linux.inotify_event)) = undefined;
        var poll_fds = [_]std.posix.pollfd{
            .{ .fd = self.impl.inotify_fd, .events = std.posix.POLL.IN, .revents = 0 },
        };

        while (!self.should_stop.load(.seq_cst)) {
            const poll_result = std.posix.poll(&poll_fds, 50) catch |err| {
                std.log.err("Poll error: {}", .{err});
                self.failBackend();
                return;
            };

            if (poll_result == 0) continue;

            const bytes_read = std.posix.read(self.impl.inotify_fd, &buffer) catch |err| switch (err) {
                error.WouldBlock => continue,
                error.InputOutput,
                error.SystemResources,
                error.IsDir,
                error.ConnectionResetByPeer,
                error.NotOpenForReading,
                error.SocketUnconnected,
                error.AccessDenied,
                error.LockViolation,
                error.Unexpected,
                error.Canceled,
                => {
                    std.log.err("Read error: {}", .{err});
                    self.failBackend();
                    return;
                },
            };

            self.processLinuxEvents(buffer[0..bytes_read]);
        }
    }

    fn clearLinuxWatchData(self: *Watcher) void {
        for (self.impl.watch_descriptors.items) |wd| {
            self.allocator.free(wd.path);
        }
        self.impl.watch_descriptors.clearRetainingCapacity();

        var indices_iter = self.impl.descriptor_indices.valueIterator();
        while (indices_iter.next()) |indices| indices.deinit(self.allocator);
        self.impl.descriptor_indices.clearRetainingCapacity();
    }

    fn processLinuxEvents(self: *Watcher, buffer: []const u8) void {
        var offset: usize = 0;
        while (offset < buffer.len) {
            const event = @as(*const std.os.linux.inotify_event, @ptrCast(@alignCast(&buffer[offset])));
            const event_size = @sizeOf(std.os.linux.inotify_event) + event.len;

            if (!self.recursive) {
                if (event.mask & std.os.linux.IN.Q_OVERFLOW != 0) {
                    self.emitEvent(.{ .path = "", .rescan = true });
                } else {
                    const indices = self.impl.descriptor_indices.get(event.wd) orelse {
                        offset += event_size;
                        continue;
                    };
                    for (indices.items) |index| {
                        const wd = self.impl.watch_descriptors.items[index];
                        if (event.len == 0) {
                            self.emitEvent(.{
                                .path = wd.path,
                                .rescan = self.linuxCoverageChanged(wd.path, event.mask),
                            });
                        } else {
                            const name = std.mem.sliceTo(buffer[offset + @sizeOf(std.os.linux.inotify_event) .. offset + event_size], 0);
                            var path_buffer: [std.fs.max_path_bytes]u8 = undefined;
                            const path = std.fmt.bufPrint(&path_buffer, "{s}{s}{s}", .{ wd.path, if (std.mem.endsWith(u8, wd.path, "/")) "" else "/", name }) catch {
                                self.emitEvent(.{ .path = "", .rescan = true });
                                continue;
                            };
                            self.emitEvent(.{
                                .path = path,
                                .rescan = self.linuxCoverageChanged(path, event.mask),
                            });
                        }
                    }
                }
                offset += event_size;
                continue;
            }

            if (event.len > 0) {
                const name_bytes = buffer[offset + @sizeOf(std.os.linux.inotify_event) .. offset + event_size - 1];
                const name = std.mem.sliceTo(name_bytes, 0);

                const is_dir = event.mask & std.os.linux.IN.ISDIR != 0;
                if (self.shouldEmitPath(name, is_dir)) {
                    for (self.impl.watch_descriptors.items) |wd| {
                        if (wd.wd == event.wd) {
                            const full_path = std.fs.path.join(self.allocator, &.{ wd.path, name }) catch |err| switch (err) {
                                error.OutOfMemory => {
                                    std.log.err("Out of memory building path for changed file: {s}", .{name});
                                    break;
                                },
                            };
                            defer self.allocator.free(full_path);
                            self.emitEvent(.{ .path = full_path });
                            break;
                        }
                    }
                }

                if (event.mask & std.os.linux.IN.CREATE != 0 and event.mask & std.os.linux.IN.ISDIR != 0) {
                    for (self.impl.watch_descriptors.items) |wd| {
                        if (wd.wd == event.wd) {
                            const new_dir = std.fs.path.join(self.allocator, &.{ wd.path, name }) catch |err| switch (err) {
                                error.OutOfMemory => {
                                    std.log.err("Out of memory building path for new directory: {s}", .{name});
                                    break;
                                },
                            };
                            defer self.allocator.free(new_dir);
                            self.addWatchRecursiveLinux(new_dir) catch |err| {
                                std.log.err("Failed to watch new directory: {}", .{err});
                            };

                            // Check if there are already matching files in the new directory
                            // This handles the case where files are created immediately after the directory
                            var dir = std.Io.Dir.openDirAbsolute(self.std_io, new_dir, .{ .iterate = true }) catch break;
                            defer dir.close(self.std_io);
                            var it = dir.iterate();
                            while (it.next(self.std_io) catch null) |entry| {
                                if (entry.kind == .file and self.shouldEmitPath(entry.name, false)) {
                                    const full_path = std.fs.path.join(self.allocator, &.{ new_dir, entry.name }) catch |err| switch (err) {
                                        error.OutOfMemory => {
                                            std.log.err("Out of memory building path for file in new directory: {s}", .{entry.name});
                                            continue;
                                        },
                                    };
                                    defer self.allocator.free(full_path);
                                    self.emitEvent(.{ .path = full_path });
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

    fn addWatchRecursiveLinux(self: *Watcher, path: []const u8) (Allocator.Error || std.Io.Dir.OpenError || std.Io.Dir.Iterator.Error || error{InotifyAddWatchFailed})!void {
        const flags = std.os.linux.IN.CREATE | std.os.linux.IN.DELETE |
            std.os.linux.IN.MODIFY | std.os.linux.IN.MOVED_FROM |
            std.os.linux.IN.MOVED_TO | std.os.linux.IN.CLOSE_WRITE |
            std.os.linux.IN.ATTRIB | std.os.linux.IN.DELETE_SELF | std.os.linux.IN.MOVE_SELF;

        const path_z = try self.allocator.dupeZ(u8, path);
        defer self.allocator.free(path_z);

        const add_result = std.os.linux.inotify_add_watch(self.impl.inotify_fd, path_z, flags);
        const add_errno = std.os.linux.errno(add_result);
        if (add_errno != .SUCCESS) {
            // An ancestor watch covers changes racing plan registration. start()
            // revalidates the plan after registration before exposing readiness.
            if (self.input_plan != null and (add_errno == .NOENT or add_errno == .NOTDIR)) return;
            if (add_errno == .ACCES) {
                if (self.input_plan) |*plan| {
                    // An unreadable file is still covered by its parent entry.
                    // ATTRIB requests fresh coverage when its permissions change.
                    if (plan.nodes.get(path).?.kind == .file) return;
                }
            }
            if (add_errno == .NOSPC) {
                std.log.warn("inotify watch registration failed for {s}: ENOSPC can mean the per-user inotify watch budget (fs.inotify.max_user_watches) is exhausted, or the kernel cannot allocate a watch", .{path});
            }
            std.log.warn("inotify_add_watch failed: {}", .{add_errno});
            return error.InotifyAddWatchFailed;
        }
        const wd = @as(i32, @intCast(add_result));

        if (self.impl.descriptor_indices.get(wd)) |indices| {
            for (indices.items) |index| {
                if (std.mem.eql(u8, self.impl.watch_descriptors.items[index].path, path)) return;
            }
        }

        {
            const path_copy = try self.allocator.dupe(u8, path);
            errdefer self.allocator.free(path_copy);

            try self.impl.watch_descriptors.append(.{
                .wd = wd,
                .path = path_copy,
            });
            errdefer _ = self.impl.watch_descriptors.pop();

            const indices = try self.impl.descriptor_indices.getOrPut(wd);
            if (!indices.found_existing) indices.value_ptr.* = .empty;
            try indices.value_ptr.append(self.allocator, self.impl.watch_descriptors.items.len - 1);
        }

        if (!self.recursive) return;

        var dir = try std.Io.Dir.openDirAbsolute(self.std_io, path, .{ .iterate = true });
        defer dir.close(self.std_io);

        var it = dir.iterate();
        while (try it.next(self.std_io)) |entry| {
            if (entry.kind == .directory) {
                const subdir_path = try std.fs.path.join(self.allocator, &.{ path, entry.name });
                defer self.allocator.free(subdir_path);
                try self.addWatchRecursiveLinux(subdir_path);
            }
        }
    }

    fn kqueueData(self: *Watcher) *KqueueData {
        return if (watcher_os == .macos) &self.impl.exact else &self.impl;
    }

    fn watchLoopKqueue(self: *Watcher) void {
        const kq = std.c.kqueue();
        if (kq < 0) {
            std.log.warn("kqueue failed: {}", .{std.posix.errno(kq)});
            self.markStartupFailed();
            return;
        }
        self.kqueueData().kq = kq;

        const watch_paths = self.directoryPaths() catch {
            self.markStartupFailed();
            return;
        };
        defer self.allocator.free(watch_paths);
        for (watch_paths) |path| {
            self.addWatchRecursiveKqueue(path, .silent) catch |err| {
                std.log.warn("Failed to watch {s}: {}", .{ path, err });
                self.markStartupFailed();
                return;
            };
        }

        if (self.input_plan) |*plan| {
            for (plan.nodes.keys(), plan.nodes.values()) |path, node| {
                if (node.kind != .file) continue;
                self.registerKqueueWatch(path, false) catch {
                    self.markStartupFailed();
                    return;
                };
            }
        }

        // Signal that we're ready to receive events
        self.markReady();

        var events: [16]std.c.Kevent = undefined;
        var no_changes: [0]std.c.Kevent = .{};
        const timeout = std.c.timespec{ .sec = 0, .nsec = 50 * std.time.ns_per_ms };

        while (!self.should_stop.load(.seq_cst)) {
            const count = std.c.kevent(kq, &no_changes, 0, &events, events.len, &timeout);
            if (count < 0) {
                const err = std.posix.errno(count);
                if (err == .INTR) continue;
                std.log.err("kevent error: {}", .{err});
                self.failBackend();
                return;
            }

            for (events[0..@intCast(count)]) |event| {
                self.processKqueueEvent(&event);
            }
        }
    }

    fn clearKqueueWatchData(self: *Watcher) void {
        for (self.kqueueData().watches.items) |watch| {
            _ = std.c.close(watch.fd);
            self.allocator.free(watch.path);
        }
        self.kqueueData().watches.clearRetainingCapacity();
    }

    fn processKqueueEvent(self: *Watcher, event: *const std.c.Kevent) void {
        const fd: i32 = @intCast(event.ident);
        const index = self.findKqueueWatch(fd) orelse return;

        // Copied out because adding watches below can reallocate the list. The
        // path bytes themselves are separately allocated, so the slice stays
        // valid until this watch is removed.
        const watch = self.kqueueData().watches.items[index];
        const gone = event.fflags & (std.c.NOTE.DELETE | std.c.NOTE.RENAME | std.c.NOTE.REVOKE) != 0;

        if (!self.recursive) {
            if (self.input_plan) |*plan| {
                var metadata_changed = plan.entryChanged(watch.path);
                if (!watch.is_dir) {
                    if (std.Io.Dir.cwd().statFile(self.std_io, watch.path, .{})) |stat| {
                        metadata_changed = metadata_changed or watch.mtime != stat.mtime.nanoseconds or watch.ctime != stat.ctime.nanoseconds;
                        self.kqueueData().watches.items[index].mtime = stat.mtime.nanoseconds;
                        self.kqueueData().watches.items[index].ctime = stat.ctime.nanoseconds;
                    } else |_| metadata_changed = true;
                }
                const written = !watch.is_dir and event.fflags & (std.c.NOTE.WRITE | std.c.NOTE.EXTEND) != 0;
                if (gone or written or metadata_changed) {
                    self.emitEvent(.{ .path = watch.path, .rescan = watch.is_dir or gone });
                }
                if (watch.is_dir and !gone) {
                    const node = plan.nodes.get(watch.path).?;
                    for (node.children.items) |child| {
                        const path = plan.nodes.keys()[child];
                        if (plan.entryChanged(path)) self.emitEvent(.{ .path = path, .rescan = true });
                    }
                }
            } else self.emitEvent(.{ .path = watch.path, .rescan = watch.is_dir or gone });
            if (gone) self.removeKqueueWatch(fd);
            return;
        }

        if (watch.is_dir) {
            // A directory reported as written and removed in the same batch has
            // nothing left to scan.
            if (!gone and event.fflags & (std.c.NOTE.WRITE | std.c.NOTE.EXTEND) != 0) {
                self.rescanKqueueDir(watch.path);
            }
        } else if (gone or event.fflags & (std.c.NOTE.WRITE | std.c.NOTE.EXTEND) != 0) {
            if (self.shouldEmitPath(watch.path, false)) {
                self.emitEvent(.{ .path = watch.path });
            }
        }

        // A deleted or renamed vnode never reports again under its old path, and
        // closing the descriptor is what unregisters it from the kqueue. Watches
        // under a directory that moved keep reporting, but their recorded paths
        // no longer name the file that would change, so they go too.
        if (gone) {
            if (watch.is_dir) self.removeKqueueWatchTree(watch.path) else self.removeKqueueWatch(fd);
        }
    }

    /// Re-read a directory whose entry list changed, registering anything that
    /// appeared since the last scan. Entries that vanished are reported through
    /// their own descriptors, so they are not diffed here.
    fn rescanKqueueDir(self: *Watcher, dir_path: []const u8) void {
        var dir = std.Io.Dir.openDirAbsolute(self.std_io, dir_path, .{ .iterate = true }) catch |err| {
            std.log.warn("Failed to reopen watched directory {s}: {}", .{ dir_path, err });
            return;
        };
        defer dir.close(self.std_io);

        var it = dir.iterate();
        while (it.next(self.std_io) catch null) |entry| {
            const full_path = std.fs.path.join(self.allocator, &.{ dir_path, entry.name }) catch |err| switch (err) {
                error.OutOfMemory => {
                    std.log.err("Out of memory building path for directory entry: {s}", .{entry.name});
                    return;
                },
            };
            defer self.allocator.free(full_path);

            if (self.findKqueueWatchByPath(full_path) != null) continue;

            switch (entry.kind) {
                .directory => self.addWatchRecursiveKqueue(full_path, .emit) catch |err| {
                    std.log.err("Failed to watch new directory {s}: {}", .{ full_path, err });
                },
                .file => {
                    if (!self.shouldEmitPath(entry.name, false)) continue;
                    self.registerKqueueWatch(full_path, false) catch |err| {
                        std.log.err("Failed to watch new file {s}: {}", .{ full_path, err });
                        continue;
                    };
                    self.emitEvent(.{ .path = full_path });
                },
                .block_device,
                .character_device,
                .named_pipe,
                .sym_link,
                .unix_domain_socket,
                .whiteout,
                .door,
                .event_port,
                .unknown,
                => {},
            }
        }
    }

    /// Whether files discovered while adding watches are reported as events.
    /// Files present when watching starts are not changes; files found inside a
    /// directory that just appeared are.
    const KqueueScanMode = enum { silent, emit };

    fn addWatchRecursiveKqueue(
        self: *Watcher,
        path: []const u8,
        mode: KqueueScanMode,
    ) (Allocator.Error || std.Io.Dir.OpenError || std.Io.Dir.Iterator.Error || error{ WatchOpenFailed, KeventFailed })!void {
        try self.registerKqueueWatch(path, true);
        if (!self.recursive) return;

        var dir = try std.Io.Dir.openDirAbsolute(self.std_io, path, .{ .iterate = true });
        defer dir.close(self.std_io);

        var it = dir.iterate();
        while (try it.next(self.std_io)) |entry| {
            const child_path = try std.fs.path.join(self.allocator, &.{ path, entry.name });
            defer self.allocator.free(child_path);

            switch (entry.kind) {
                .directory => try self.addWatchRecursiveKqueue(child_path, mode),
                .file => {
                    if (!self.shouldEmitPath(entry.name, false)) continue;
                    try self.registerKqueueWatch(child_path, false);
                    if (mode == .emit) self.emitEvent(.{ .path = child_path });
                },
                .block_device,
                .character_device,
                .named_pipe,
                .sym_link,
                .unix_domain_socket,
                .whiteout,
                .door,
                .event_port,
                .unknown,
                => {},
            }
        }
    }

    fn registerKqueueWatch(
        self: *Watcher,
        path: []const u8,
        is_dir: bool,
    ) (Allocator.Error || error{ WatchOpenFailed, KeventFailed })!void {
        const path_z = try self.allocator.dupeZ(u8, path);
        defer self.allocator.free(path_z);

        var open_flags: std.posix.O = .{ .ACCMODE = .RDONLY, .CLOEXEC = true };
        if (builtin.os.tag == .macos and self.input_plan != null) open_flags.EVTONLY = true;
        const fd = std.posix.openatZ(
            std.posix.AT.FDCWD,
            path_z,
            open_flags,
            0,
        ) catch |err| switch (err) {
            // One descriptor per watched directory and per watched file is
            // inherent to kqueue, so a large tree can need more descriptors
            // than the process is allowed. Say so, because the per-path open
            // error alone reads like a problem with that one path.
            error.ProcessFdQuotaExceeded, error.SystemFdQuotaExceeded => {
                std.log.warn(
                    "Ran out of file descriptors watching {s}: this watcher needs one per directory and per watched file. Raise the open-files limit to watch a tree this large.",
                    .{path},
                );
                return error.WatchOpenFailed;
            },
            error.FileNotFound, error.NotDir => {
                if (self.input_plan != null) return;
                return error.WatchOpenFailed;
            },
            error.AntivirusInterference,
            error.AccessDenied,
            error.PermissionDenied,
            error.SymLinkLoop,
            error.SystemResources,
            error.NoDevice,
            error.NetworkNotFound,
            error.PipeBusy,
            error.FileTooBig,
            error.IsDir,
            error.NoSpaceLeft,
            error.PathAlreadyExists,
            error.ReadOnlyFileSystem,
            error.DeviceBusy,
            error.FileLocksUnsupported,
            error.FileBusy,
            error.WouldBlock,
            error.NameTooLong,
            error.BadPathName,
            error.Canceled,
            error.Unexpected,
            => {
                std.log.warn("Failed to open {s} for watching: {}", .{ path, err });
                return error.WatchOpenFailed;
            },
        };
        errdefer _ = std.c.close(fd);

        // On a directory these report entries appearing or disappearing; on a
        // file they report writes to its contents. NOTE.ATTRIB is deliberately
        // absent: it fires on atime updates, so reading a watched file in
        // response to an event would generate the next event.
        const notes = std.c.NOTE.WRITE | std.c.NOTE.EXTEND | std.c.NOTE.DELETE | std.c.NOTE.RENAME | std.c.NOTE.REVOKE |
            (if (self.input_plan != null) @as(u32, std.c.NOTE.ATTRIB) else 0);

        const change = std.c.Kevent{
            .ident = @intCast(fd),
            .filter = std.c.EVFILT.VNODE,
            .flags = std.c.EV.ADD | std.c.EV.ENABLE | std.c.EV.CLEAR,
            .fflags = notes,
            .data = 0,
            .udata = 0,
        };

        var no_events: [0]std.c.Kevent = .{};
        const rc = std.c.kevent(self.kqueueData().kq, (&change)[0..1], 1, &no_events, 0, null);
        if (rc < 0) {
            std.log.warn("kevent registration failed for {s}: {}", .{ path, std.posix.errno(rc) });
            return error.KeventFailed;
        }

        const path_copy = try self.allocator.dupe(u8, path);
        errdefer self.allocator.free(path_copy);

        try self.kqueueData().watches.append(.{
            .fd = fd,
            .path = path_copy,
            .is_dir = is_dir,
            .mtime = if (self.input_plan) |*plan| plan.nodes.get(path).?.mtime else 0,
            .ctime = if (self.input_plan) |*plan| plan.nodes.get(path).?.ctime else 0,
        });
    }

    fn findKqueueWatch(self: *Watcher, fd: i32) ?usize {
        for (self.kqueueData().watches.items, 0..) |watch, i| {
            if (watch.fd == fd) return i;
        }
        return null;
    }

    fn findKqueueWatchByPath(self: *Watcher, path: []const u8) ?usize {
        for (self.kqueueData().watches.items, 0..) |watch, i| {
            if (std.mem.eql(u8, watch.path, path)) return i;
        }
        return null;
    }

    fn removeKqueueWatch(self: *Watcher, fd: i32) void {
        const index = self.findKqueueWatch(fd) orelse return;
        self.removeKqueueWatchAt(index);
    }

    /// Drop a directory's watch along with every watch beneath it.
    ///
    /// Descendants go first because `dir_path` is owned by the directory's own
    /// watch: removing that entry frees the very bytes the remaining
    /// comparisons read.
    fn removeKqueueWatchTree(self: *Watcher, dir_path: []const u8) void {
        var i: usize = 0;
        while (i < self.kqueueData().watches.items.len) {
            const path = self.kqueueData().watches.items[i].path;
            const under_dir = path.len > dir_path.len and
                std.mem.startsWith(u8, path, dir_path) and
                path[dir_path.len] == std.fs.path.sep;

            if (under_dir) {
                self.removeKqueueWatchAt(i);
                continue;
            }
            i += 1;
        }

        if (self.findKqueueWatchByPath(dir_path)) |index| {
            self.removeKqueueWatchAt(index);
        }
    }

    fn removeKqueueWatchAt(self: *Watcher, index: usize) void {
        const watch = self.kqueueData().watches.swapRemove(index);
        _ = std.c.close(watch.fd);
        self.allocator.free(watch.path);
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

        self.impl.stop_event = CreateEventW(null, std.os.windows.BOOL.TRUE, .FALSE, null) orelse {
            std.log.warn("Failed to create stop event", .{});
            self.markStartupFailed();
            return;
        };

        const watch_paths = self.directoryPaths() catch {
            self.markStartupFailed();
            return;
        };
        defer self.allocator.free(watch_paths);
        // Finish growing the arrays before submitting any I/O: Windows retains
        // pointers to the OVERLAPPED records until each read completes.
        for (watch_paths) |path| {
            self.setupWindowsWatch(path) catch |err| {
                std.log.warn("Failed to set up watch for {s}: {}", .{ path, err });
                self.markStartupFailed();
                return;
            };
        }

        // Debug: check if we have any handles
        if (self.impl.handles.items.len == 0) {
            std.log.warn("No directory handles were created", .{});
            self.markStartupFailed();
            return;
        }

        const watched_count = self.impl.overlapped_data.items.len;
        for (0..watched_count) |index| {
            self.startWindowsRead(index) catch |err| {
                std.log.warn("Failed to start directory read: {}", .{err});
                self.markStartupFailed();
                return;
            };
        }
        const shard_count = std.math.divCeil(usize, watched_count, windows_max_watched_handles_per_thread) catch {
            self.markStartupFailed();
            return;
        };
        var shard_threads = self.allocator.alloc(std.Thread, shard_count) catch {
            std.log.warn("Failed to allocate Windows watcher thread list", .{});
            self.markStartupFailed();
            return;
        };
        defer self.allocator.free(shard_threads);

        var spawned: usize = 0;
        while (spawned < shard_count) : (spawned += 1) {
            const start_index = spawned * windows_max_watched_handles_per_thread;
            const end_index = @min(watched_count, start_index + windows_max_watched_handles_per_thread);
            shard_threads[spawned] = std.Thread.spawn(.{}, watchLoopWindowsShard, .{ self, start_index, end_index }) catch |err| {
                std.log.warn("Failed to spawn Windows watcher shard: {}", .{err});
                self.markStartupFailed();
                self.should_stop.store(true, .seq_cst);
                self.signalWindowsStopEvent();
                for (shard_threads[0..spawned]) |thread| thread.join();
                return;
            };
        }

        self.markReady();
        for (shard_threads) |thread| thread.join();
    }

    const windows_max_wait_handles = 64;
    const windows_max_watched_handles_per_thread = windows_max_wait_handles - 1;
    const windows_wait_timeout = 258; // WAIT_TIMEOUT, 0x102
    const windows_wait_failed = 0xFFFFFFFF; // WAIT_FAILED

    fn watchLoopWindowsShard(self: *Watcher, start_index: usize, end_index: usize) void {
        if (end_index <= start_index) return;

        const WaitForMultipleObjects = struct {
            extern "kernel32" fn WaitForMultipleObjects(
                nCount: std.os.windows.DWORD,
                lpHandles: [*]const std.os.windows.HANDLE,
                bWaitAll: std.os.windows.BOOL,
                dwMilliseconds: std.os.windows.DWORD,
            ) callconv(.winapi) std.os.windows.DWORD;
        }.WaitForMultipleObjects;

        var handles: [windows_max_wait_handles]std.os.windows.HANDLE = undefined;
        for (self.impl.overlapped_data.items[start_index..end_index], 0..) |data, i| {
            handles[i] = data.overlapped.hEvent.?;
        }
        const stop_index = end_index - start_index;
        handles[stop_index] = self.impl.stop_event.?;
        const handle_count = stop_index + 1;

        while (!self.should_stop.load(.seq_cst)) {
            const result = WaitForMultipleObjects(
                @intCast(handle_count),
                handles[0..handle_count].ptr,
                .FALSE,
                100,
            );

            if (result == windows_wait_timeout or result == stop_index) {
                continue;
            }
            if (result == windows_wait_failed) {
                std.log.err("WaitForMultipleObjects failed", .{});
                self.should_stop.store(true, .seq_cst);
                self.signalWindowsStopEvent();
                return;
            }
            if (result < stop_index) {
                const handle_index = start_index + result;
                self.processWindowsEvents(handle_index) catch |err| {
                    std.log.err("Failed to process Windows events: {}", .{err});
                };
            }
        }
    }

    fn setupWindowsWatch(self: *Watcher, path: []const u8) (Allocator.Error || error{ InvalidUtf8, FailedToOpenDirectory, FailedToCreateEvent })!void {
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
            const err = std.os.windows.GetLastError();
            if (self.input_plan != null and (err == .FILE_NOT_FOUND or err == .PATH_NOT_FOUND)) return;
            return error.FailedToOpenDirectory;
        }

        errdefer _ = std.os.windows.CloseHandle(dir_handle);

        // Create event for overlapped I/O
        const CreateEventW = struct {
            extern "kernel32" fn CreateEventW(
                lpEventAttributes: ?*anyopaque,
                bManualReset: std.os.windows.BOOL,
                bInitialState: std.os.windows.BOOL,
                lpName: ?[*:0]const u16,
            ) callconv(.winapi) ?std.os.windows.HANDLE;
        }.CreateEventW;

        const event_handle = CreateEventW(null, std.os.windows.BOOL.TRUE, .FALSE, null) orelse return error.FailedToCreateEvent;
        errdefer _ = std.os.windows.CloseHandle(event_handle);

        // Allocate buffer for ReadDirectoryChangesW
        const buffer_size = 4096;
        const buffer = try self.allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(win32.FILE_NOTIFY_INFORMATION)), buffer_size);

        errdefer self.allocator.free(buffer);
        const owned_path = try self.allocator.dupe(u8, path);
        errdefer self.allocator.free(owned_path);

        // Reserve both slots before transferring ownership of the resources.
        try self.impl.handles.ensureUnusedCapacity(1);
        try self.impl.overlapped_data.ensureUnusedCapacity(1);
        var overlapped_data = WindowsData.OverlappedData{
            .overlapped = std.mem.zeroes(win32.OVERLAPPED),
            .buffer = buffer,
            .path = owned_path,
        };
        overlapped_data.overlapped.hEvent = event_handle;

        self.impl.handles.appendAssumeCapacity(dir_handle);
        self.impl.overlapped_data.appendAssumeCapacity(overlapped_data);
    }

    fn startWindowsRead(self: *Watcher, index: usize) (Allocator.Error || error{ReadDirectoryChangesFailed})!void {
        const ReadDirectoryChangesW = struct {
            extern "kernel32" fn ReadDirectoryChangesW(
                hDirectory: std.os.windows.HANDLE,
                lpBuffer: [*]u8,
                nBufferLength: std.os.windows.DWORD,
                bWatchSubtree: std.os.windows.BOOL,
                dwNotifyFilter: std.os.windows.DWORD,
                lpBytesReturned: ?*std.os.windows.DWORD,
                lpOverlapped: *win32.OVERLAPPED,
                lpCompletionRoutine: ?*anyopaque,
            ) callconv(.winapi) std.os.windows.BOOL;
        }.ReadDirectoryChangesW;

        const FILE_NOTIFY_CHANGE_FILE_NAME = 0x00000001;
        const FILE_NOTIFY_CHANGE_DIR_NAME = 0x00000002;
        const FILE_NOTIFY_CHANGE_LAST_WRITE = 0x00000010;
        const FILE_NOTIFY_CHANGE_CREATION = 0x00000040;
        const FILE_NOTIFY_CHANGE_ATTRIBUTES = 0x00000004;
        const FILE_NOTIFY_CHANGE_SECURITY = 0x00000100;

        const notify_filter = FILE_NOTIFY_CHANGE_FILE_NAME |
            FILE_NOTIFY_CHANGE_DIR_NAME |
            FILE_NOTIFY_CHANGE_LAST_WRITE |
            FILE_NOTIFY_CHANGE_CREATION | FILE_NOTIFY_CHANGE_ATTRIBUTES | FILE_NOTIFY_CHANGE_SECURITY;

        std.debug.assert(!self.impl.overlapped_data.items[index].read_pending);
        const result = ReadDirectoryChangesW(
            self.impl.handles.items[index],
            self.impl.overlapped_data.items[index].buffer.ptr,
            @intCast(self.impl.overlapped_data.items[index].buffer.len),
            if (self.recursive) std.os.windows.BOOL.TRUE else std.os.windows.BOOL.FALSE,
            notify_filter,
            null,
            &self.impl.overlapped_data.items[index].overlapped,
            null,
        );

        if (result == .FALSE) {
            const err = std.os.windows.GetLastError();
            std.log.err("ReadDirectoryChangesW failed with error: {}", .{err});
            return error.ReadDirectoryChangesFailed;
        }
        self.impl.overlapped_data.items[index].read_pending = true;
    }

    fn cancelWindowsRead(self: *Watcher, index: usize) void {
        const data = &self.impl.overlapped_data.items[index];
        if (!data.read_pending) return;
        const handle = self.impl.handles.items[index];
        if (win32.CancelIoEx(handle, &data.overlapped) == .FALSE) {
            const err = std.os.windows.GetLastError();
            // Completion can race cancellation; we must still collect it.
            if (err != .NOT_FOUND) std.log.err("CancelIoEx failed: {}", .{err});
        }
        var bytes_transferred: std.os.windows.DWORD = 0;
        // CancelIoEx only requests cancellation. A blocking completion wait
        // keeps both the OVERLAPPED and buffer alive until Windows releases them.
        // Success, OPERATION_ABORTED, and other completed errors all retire I/O.
        _ = win32.GetOverlappedResult(handle, &data.overlapped, &bytes_transferred, .TRUE);
        data.read_pending = false;
    }

    fn processWindowsEvents(self: *Watcher, index: usize) Allocator.Error!void {
        const ResetEvent = struct {
            extern "kernel32" fn ResetEvent(hEvent: std.os.windows.HANDLE) callconv(.winapi) std.os.windows.BOOL;
        }.ResetEvent;

        var bytes_transferred: std.os.windows.DWORD = 0;
        const result = win32.GetOverlappedResult(
            self.impl.handles.items[index],
            &self.impl.overlapped_data.items[index].overlapped,
            &bytes_transferred,
            .FALSE,
        );

        const completion_error = if (result == .FALSE) std.os.windows.GetLastError() else .SUCCESS;
        // An incomplete poll does not consume the read or permit resetting its
        // event, reusing its OVERLAPPED, or submitting another read.
        if (completion_error == .IO_INCOMPLETE) return;
        self.impl.overlapped_data.items[index].read_pending = false;
        if (result == .FALSE) {
            if (completion_error == .NOTIFY_ENUM_DIR) {
                // Continue through reset/rearm with an empty completion, which
                // requests reconciliation below.
                bytes_transferred = 0;
            } else {
                if (self.should_stop.load(.seq_cst)) return;
                std.log.err("GetOverlappedResult failed: {}", .{completion_error});
                self.failBackend();
                return;
            }
        }

        // Reset the event for the next operation
        _ = ResetEvent(self.impl.overlapped_data.items[index].overlapped.hEvent.?);

        // A zero-byte completion means the notification buffer overflowed.
        if (bytes_transferred == 0) self.emitEvent(.{ .path = "", .rescan = true });
        self.parseWindowsFileNotifications(index, bytes_transferred);

        // Start the next ReadDirectoryChangesW operation
        self.startWindowsRead(index) catch |err| {
            std.log.err("Failed to restart ReadDirectoryChangesW: {}", .{err});
            self.failBackend();
        };
    }

    fn parseWindowsFileNotifications(self: *Watcher, index: usize, bytes_transferred: std.os.windows.DWORD) void {
        const buffer = self.impl.overlapped_data.items[index].buffer;
        const base_path = self.impl.overlapped_data.items[index].path;

        var offset: u32 = 0;
        while (offset < bytes_transferred) {
            const info = @as(*const win32.FILE_NOTIFY_INFORMATION, @ptrCast(@alignCast(&buffer[offset])));

            // Convert filename from UTF-16 to UTF-8
            const filename_utf16 = @as([*]const u16, @ptrCast(@alignCast(&buffer[offset + @sizeOf(win32.FILE_NOTIFY_INFORMATION)])))[0 .. info.FileNameLength / 2];

            var filename_utf8_buf: [std.Io.Dir.max_path_bytes]u8 = undefined;
            const filename_utf8_len = std.unicode.utf16LeToUtf8(filename_utf8_buf[0..], filename_utf16) catch {
                // Skip this file if we can't convert the name
                if (info.NextEntryOffset == 0) break;
                offset += info.NextEntryOffset;
                continue;
            };
            const filename_utf8 = filename_utf8_buf[0..filename_utf8_len];

            if (self.shouldEmitPath(filename_utf8, false)) {
                // Create full path
                var path_buffer: [std.fs.max_path_bytes]u8 = undefined;
                const full_path = std.fmt.bufPrint(&path_buffer, "{s}{s}{s}", .{ base_path, if (std.mem.endsWith(u8, base_path, "\\")) "" else "\\", filename_utf8 }) catch {
                    self.emitEvent(.{ .path = "", .rescan = true });
                    if (info.NextEntryOffset == 0) break;
                    offset += info.NextEntryOffset;
                    continue;
                };

                const event = WatchEvent{ .path = full_path, .rescan = info.Action != 3 };
                self.emitEvent(event);
            }

            // Move to next notification
            if (info.NextEntryOffset == 0) break;
            offset += info.NextEntryOffset;
        }
    }
};

// TESTS

test "exact inputs bound registrations and route only explicit files" {
    if (active_watcher_backend_is_stub) return error.SkipZigTest;
    const a = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.createDirPath(io, "target/generated");
    try tmp.dir.createDirPath(io, "unrelated/deep/tree");
    try tmp.dir.writeFile(io, .{ .sub_path = "Main.roc", .data = "main" });
    try tmp.dir.writeFile(io, .{ .sub_path = "target/generated/asset.txt", .data = "asset" });
    const root = try tmp.dir.realPathFileAlloc(io, ".", a);
    defer a.free(root);
    const main = try std.fs.path.join(a, &.{ root, "Main.roc" });
    defer a.free(main);
    const asset = try std.fs.path.join(a, &.{ root, "target/generated/asset.txt" });
    defer a.free(asset);
    var count = std.atomic.Value(u32).init(0);
    const cb = struct {
        fn call(context: ?*anyopaque, _: WatchEvent) void {
            const counter: *std.atomic.Value(u32) = @ptrCast(@alignCast(context.?));
            _ = counter.fetchAdd(1, .seq_cst);
        }
    }.call;
    const watcher = try Watcher.initInputs(a, io, &.{ main, asset }, &count, cb);
    defer watcher.deinit();
    try watcher.start();
    if (builtin.os.tag == .linux) {
        for (watcher.impl.watch_descriptors.items) |wd| {
            try std.testing.expect(std.mem.find(u8, wd.path, "unrelated") == null);
        }
    }
    // Deterministically verify routing independently of event scheduling.
    const irrelevant = try std.fs.path.join(a, &.{ root, "other.roc" });
    defer a.free(irrelevant);
    watcher.emitEvent(.{ .path = irrelevant });
    try std.testing.expect(!watcher.takeInputChange(0));
    try std.testing.expect(!watcher.takeInputChange(1));
    for (0..128) |i| {
        var buffer: [64]u8 = undefined;
        const directory = try std.fmt.bufPrint(&buffer, "unrelated/{d}/nested", .{i});
        try tmp.dir.createDirPath(io, directory);
    }
    var expanded = try Inputs.init(a, io, &.{ main, asset });
    defer expanded.deinit();
    try std.testing.expect(watcher.input_plan.?.eql(&expanded));
    try tmp.dir.writeFile(io, .{ .sub_path = "target/generated/asset.txt", .data = "changed" });
    try waitForEvents(&count, 1, 5000, io);
    try std.testing.expect(watcher.takeInputChange(1));
    try std.testing.expect(!watcher.takeInputChange(0));
}

test "exact inputs track missing directory chains and symlink targets" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    const a = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", a);
    defer a.free(root);
    try tmp.dir.symLink(io, "generated/deep/file.txt", "asset", .{});
    const asset = try std.fs.path.join(a, &.{ root, "asset" });
    defer a.free(asset);
    const generated = try std.fs.path.join(a, &.{ root, "generated" });
    defer a.free(generated);
    var initial = try Inputs.init(a, io, &.{asset});
    defer initial.deinit();
    try std.testing.expect(initial.nodes.contains(generated));
    try std.testing.expect(initial.nodes.contains(asset));
    try tmp.dir.createDirPath(io, "generated/deep");
    var next = try Inputs.init(a, io, &.{asset});
    defer next.deinit();
    try std.testing.expect(!initial.eql(&next));
    const target = try std.fs.path.join(a, &.{ root, "generated/deep/file.txt" });
    defer a.free(target);
    try std.testing.expect(next.nodes.contains(target));
    // A symlink cycle is still covered at the entries that can repair it.
    try tmp.dir.symLink(io, "loop", "loop", .{});
    const loop = try std.fs.path.join(a, &.{ root, "loop" });
    defer a.free(loop);
    var cycle = try Inputs.init(a, io, &.{loop});
    defer cycle.deinit();
    try std.testing.expect(cycle.nodes.contains(loop));
}

test "exact inputs allocation failure releases partial plans" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const a = std.testing.allocator;
    const root = try tmp.dir.realPathFileAlloc(std.testing.io, ".", a);
    defer a.free(root);
    const path = try std.fs.path.join(a, &.{ root, "missing/deep/asset" });
    defer a.free(path);
    const exercise = struct {
        fn run(allocator: Allocator, input: []const u8) Inputs.Error!void {
            const cb = struct {
                fn call(_: ?*anyopaque, _: WatchEvent) void {}
            }.call;
            const watcher = try Watcher.initInputs(allocator, std.testing.io, &.{input}, null, cb);
            defer watcher.deinit();
        }
    }.run;
    try std.testing.checkAllAllocationFailures(a, exercise, .{path});
}

test "exact inputs revalidate a plan changed before registration" {
    if (builtin.os.tag != .linux) return error.SkipZigTest;
    const a = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", a);
    defer a.free(root);
    const path = try std.fs.path.join(a, &.{ root, "new/input.txt" });
    defer a.free(path);
    const cb = struct {
        fn call(_: ?*anyopaque, _: WatchEvent) void {}
    }.call;
    const watcher = try Watcher.initInputs(a, io, &.{path}, null, cb);
    defer watcher.deinit();
    try tmp.dir.createDirPath(io, "new");
    try tmp.dir.writeFile(io, .{ .sub_path = "new/input.txt", .data = "created before registration" });
    try watcher.start();
    try std.testing.expect(watcher.input_plan.?.nodes.get(path).?.kind == .file);
    watcher.emitEvent(.{ .path = "", .rescan = true });
    try std.testing.expect(watcher.takeCoverageChange());
    try std.testing.expect(watcher.takeInputChange(0));
}

test "exact inputs observe hidden symlinks and writes through unwatched hard links" {
    if (builtin.os.tag != .linux) return error.SkipZigTest;
    const a = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.createDirPath(io, "project");
    try tmp.dir.createDirPath(io, "external");
    try tmp.dir.createDirPath(io, "unrelated");
    try tmp.dir.writeFile(io, .{ .sub_path = "external/asset.txt", .data = "before" });
    try tmp.dir.symLink(io, "../external/asset.txt", "project/.asset", .{});
    try tmp.dir.hardLink("external/asset.txt", tmp.dir, "unrelated/alias", io, .{});
    const root = try tmp.dir.realPathFileAlloc(io, ".", a);
    defer a.free(root);
    const path = try std.fs.path.join(a, &.{ root, "project/.asset" });
    defer a.free(path);
    var count = std.atomic.Value(u32).init(0);
    const cb = struct {
        fn call(context: ?*anyopaque, _: WatchEvent) void {
            const counter: *std.atomic.Value(u32) = @ptrCast(@alignCast(context.?));
            _ = counter.fetchAdd(1, .seq_cst);
        }
    }.call;
    const watcher = try Watcher.initInputs(a, io, &.{path}, &count, cb);
    defer watcher.deinit();
    try watcher.start();
    for (watcher.impl.watch_descriptors.items) |wd| {
        try std.testing.expect(std.mem.find(u8, wd.path, "unrelated") == null);
    }
    try tmp.dir.writeFile(io, .{ .sub_path = "unrelated/alias", .data = "after" });
    try waitForEvents(&count, 1, 5000, io);
    try std.testing.expect(watcher.takeInputChange(0));
}

fn waitForEvents(event_count: *std.atomic.Value(u32), expected: u32, max_wait_ms: u32, io: std.Io) error{EventsNotReceived}!void {
    // When the active backend is stubbed, don't wait for events since they won't be generated.
    if (active_watcher_backend_is_stub) {
        return;
    }

    const start = std.Io.Clock.now(.awake, io);
    while (event_count.load(.seq_cst) < expected) {
        const elapsed = start.durationTo(std.Io.Clock.now(.awake, io)).toMilliseconds();
        if (elapsed > max_wait_ms) {
            return error.EventsNotReceived;
        }
        std.Thread.yield() catch {};
    }
}

fn expectEventsOrSkip(event_count: *std.atomic.Value(u32), expected: u32) error{TestUnexpectedResult}!void {
    if (active_watcher_backend_is_stub) {
        // When the active backend is stubbed, skip the event count check.
        return;
    } else {
        // When using real file watching, verify we got the expected events
        const count = event_count.load(.seq_cst);
        try std.testing.expect(count >= expected);
    }
}

test "basic file watching" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(temp_path);

    const global = struct {
        var event_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
        var last_path: ?[]const u8 = null;
        var mutex: std.Io.Mutex = std.Io.Mutex.init;
        var io_handle: std.Io = undefined;
    };
    global.io_handle = io;

    const callback = struct {
        fn cb(event: WatchEvent) void {
            bumpEventCount(global);
            global.mutex.lockUncancelable(global.io_handle);
            defer global.mutex.unlock(global.io_handle);
            global.last_path = event.path;
        }
    }.cb;

    const watcher = try Watcher.init(allocator, io, &.{temp_path}, callback);
    defer watcher.deinit();

    try watcher.start();

    // Create .roc files and wait for events (or skip if using stubs)
    try temp_dir.dir.writeFile(io, .{ .sub_path = "test1.roc", .data = "content1" });
    try waitForEvents(&global.event_count, 1, 5000, io);

    try temp_dir.dir.writeFile(io, .{ .sub_path = "test2.roc", .data = "content2" });
    try waitForEvents(&global.event_count, 2, 5000, io);

    try temp_dir.dir.writeFile(io, .{ .sub_path = "test3.txt", .data = "ignored" });

    watcher.stop();

    // Verify we got the expected events (or skip if using stubs)
    try expectEventsOrSkip(&global.event_count, 2);
}

test "Linux watcher start reports setup failure" {
    if (builtin.os.tag != .linux) return error.SkipZigTest;

    const allocator = std.testing.allocator;
    const io = std.testing.io;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(temp_path);

    const missing_path = try std.fs.path.join(allocator, &.{ temp_path, "missing" });
    defer allocator.free(missing_path);

    const callback = struct {
        fn cb(_: WatchEvent) void {}
    }.cb;

    const watcher = try Watcher.init(allocator, io, &.{missing_path}, callback);
    defer watcher.deinit();

    try std.testing.expectError(error.WatchBackendFailed, watcher.start());
}

test "recursive directory watching" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(temp_path);

    try temp_dir.dir.createDir(io, "subdir", .default_dir);

    const global = struct {
        var event_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
    };

    const callback = struct {
        fn cb(_: WatchEvent) void {
            bumpEventCount(global);
        }
    }.cb;

    const watcher = try Watcher.init(allocator, io, &.{temp_path}, callback);
    defer watcher.deinit();

    try watcher.start();

    try temp_dir.dir.writeFile(io, .{ .sub_path = "subdir/nested.roc", .data = "nested content" });
    try waitForEvents(&global.event_count, 1, 5000, io);

    watcher.stop();

    try expectEventsOrSkip(&global.event_count, 1);
}

test "multiple directories watching" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;

    var temp_dir1 = std.testing.tmpDir(.{});
    defer temp_dir1.cleanup();
    var temp_dir2 = std.testing.tmpDir(.{});
    defer temp_dir2.cleanup();

    const temp_path1 = try temp_dir1.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(temp_path1);
    const temp_path2 = try temp_dir2.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(temp_path2);

    const global = struct {
        var event_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
    };

    const callback = struct {
        fn cb(_: WatchEvent) void {
            bumpEventCount(global);
        }
    }.cb;

    const watcher = try Watcher.init(allocator, io, &.{ temp_path1, temp_path2 }, callback);
    defer watcher.deinit();

    try watcher.start();

    try temp_dir1.dir.writeFile(io, .{ .sub_path = "file1.roc", .data = "content1" });
    try waitForEvents(&global.event_count, 1, 5000, io);

    try temp_dir2.dir.writeFile(io, .{ .sub_path = "file2.roc", .data = "content2" });
    try waitForEvents(&global.event_count, 2, 5000, io);

    watcher.stop();

    try expectEventsOrSkip(&global.event_count, 2);
}

test "file modification detection" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(temp_path);

    try temp_dir.dir.writeFile(io, .{ .sub_path = "modify.roc", .data = "initial" });

    const global = struct {
        var event_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
    };

    const callback = struct {
        fn cb(_: WatchEvent) void {
            bumpEventCount(global);
        }
    }.cb;

    const watcher = try Watcher.init(allocator, io, &.{temp_path}, callback);
    defer watcher.deinit();

    try watcher.start();

    try temp_dir.dir.writeFile(io, .{ .sub_path = "modify.roc", .data = "modified content that is different" });
    try waitForEvents(&global.event_count, 1, 5000, io);

    watcher.stop();

    try expectEventsOrSkip(&global.event_count, 1);
}

test "rapid file creation" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(temp_path);

    const global = struct {
        var event_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
    };

    const callback = struct {
        fn cb(_: WatchEvent) void {
            bumpEventCount(global);
        }
    }.cb;

    const watcher = try Watcher.init(allocator, io, &.{temp_path}, callback);
    defer watcher.deinit();

    try watcher.start();

    const start_time = std.Io.Clock.now(.awake, io);

    for (0..50) |i| {
        const filename = try std.fmt.allocPrint(allocator, "file{d}.roc", .{i});
        defer allocator.free(filename);
        try temp_dir.dir.writeFile(io, .{ .sub_path = filename, .data = "content" });
    }

    // FSEvents on macOS coalesces rapid events, so we might not get all 50 events
    const min_expected = if (builtin.os.tag == .macos) 10 else 50;
    try waitForEvents(&global.event_count, min_expected, 10000, io);

    const elapsed = start_time.durationTo(std.Io.Clock.now(.awake, io)).toMilliseconds();

    watcher.stop();

    try std.testing.expect(elapsed < 5000);

    try expectEventsOrSkip(&global.event_count, min_expected);
}

test "directory creation and file addition" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(temp_path);

    const global = struct {
        var event_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
    };

    const callback = struct {
        fn cb(_: WatchEvent) void {
            bumpEventCount(global);
        }
    }.cb;

    const watcher = try Watcher.init(allocator, io, &.{temp_path}, callback);
    defer watcher.deinit();

    try watcher.start();

    try temp_dir.dir.createDir(io, "newdir", .default_dir);
    std.Thread.yield() catch {};

    try temp_dir.dir.writeFile(io, .{ .sub_path = "newdir/new.roc", .data = "new content" });

    if (builtin.os.tag == .linux) {
        try waitForEvents(&global.event_count, 1, 5000, io);
    }

    watcher.stop();

    if (builtin.os.tag == .linux) {
        try expectEventsOrSkip(&global.event_count, 1);
    }
}

test "start stop restart" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(temp_path);

    const global = struct {
        var event_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
    };

    const callback = struct {
        fn cb(_: WatchEvent) void {
            bumpEventCount(global);
        }
    }.cb;

    const watcher = try Watcher.init(allocator, io, &.{temp_path}, callback);
    defer watcher.deinit();

    try watcher.start();
    try temp_dir.dir.writeFile(io, .{ .sub_path = "first.roc", .data = "first" });
    try waitForEvents(&global.event_count, 1, 5000, io);

    watcher.stop();
    const count_after_stop = global.event_count.load(.seq_cst);

    try temp_dir.dir.writeFile(io, .{ .sub_path = "while_stopped.roc", .data = "stopped" });
    std.Thread.yield() catch {};

    try std.testing.expectEqual(count_after_stop, global.event_count.load(.seq_cst));

    try watcher.start();
    try temp_dir.dir.writeFile(io, .{ .sub_path = "after_restart.roc", .data = "restarted" });
    try waitForEvents(&global.event_count, count_after_stop + 1, 5000, io);

    watcher.stop();

    const final_count = global.event_count.load(.seq_cst);
    try std.testing.expect(final_count > count_after_stop);
}

test "thread safety" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(temp_path);

    const global = struct {
        var event_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
        var mutex: std.Io.Mutex = std.Io.Mutex.init;
        var events: std.ArrayList([]const u8) = .empty;
        var io_handle: std.Io = undefined;
    };
    global.io_handle = io;

    defer global.events.deinit(allocator);

    const callback = struct {
        fn cb(event: WatchEvent) void {
            bumpEventCount(global);
            global.mutex.lockUncancelable(global.io_handle);
            defer global.mutex.unlock(global.io_handle);
            global.events.append(allocator, allocator.dupe(u8, event.path) catch return) catch return;
        }
    }.cb;

    const watcher = try Watcher.init(allocator, io, &.{temp_path}, callback);
    defer watcher.deinit();

    try watcher.start();

    const thread_count = 4;
    var threads: [thread_count]std.Thread = undefined;

    const WriterArgs = struct { dir: *std.testing.TmpDir, id: usize, alloc: std.mem.Allocator, io: std.Io };

    const writer = struct {
        fn write(args: WriterArgs) void {
            for (0..5) |i| {
                const filename = std.fmt.allocPrint(args.alloc, "thread{d}_file{d}.roc", .{ args.id, i }) catch return;
                defer args.alloc.free(filename);
                args.dir.dir.writeFile(args.io, .{ .sub_path = filename, .data = "content" }) catch return;
                std.Thread.yield() catch {};
            }
        }
    };

    for (0..thread_count) |i| {
        const args = WriterArgs{ .dir = &temp_dir, .id = i, .alloc = allocator, .io = io };
        threads[i] = std.Thread.spawn(.{}, writer.write, .{args}) catch continue;
    }

    for (threads) |thread| {
        thread.join();
    }

    // FSEvents on macOS coalesces rapid events, so we might not get all 20 events
    // Just ensure we get at least some events from the concurrent writes
    const min_expected = if (builtin.os.tag == .macos) 4 else thread_count * 5;
    try waitForEvents(&global.event_count, min_expected, 10000, io);

    watcher.stop();

    try expectEventsOrSkip(&global.event_count, min_expected);

    global.mutex.lockUncancelable(io);
    defer global.mutex.unlock(io);
    for (global.events.items) |path| {
        allocator.free(path);
    }
}

test "file rename detection" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(temp_path);

    const global = struct {
        var event_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
    };

    const callback = struct {
        fn cb(_: WatchEvent) void {
            bumpEventCount(global);
        }
    }.cb;

    const watcher = try Watcher.init(allocator, io, &.{temp_path}, callback);
    defer watcher.deinit();

    try temp_dir.dir.writeFile(io, .{ .sub_path = "original.roc", .data = "content" });

    try watcher.start();

    try temp_dir.dir.rename("original.roc", temp_dir.dir, "renamed.roc", io);
    try waitForEvents(&global.event_count, 1, 5000, io);

    watcher.stop();

    if (builtin.os.tag == .linux) {
        try expectEventsOrSkip(&global.event_count, 1);
    }
}

test "windows incomplete polls preserve pending reads until cancellation" {
    if (builtin.os.tag != .windows) return error.SkipZigTest;
    const a = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", a);
    defer a.free(root);
    var count = std.atomic.Value(u32).init(0);
    const callback = struct {
        fn call(context: ?*anyopaque, _: WatchEvent) void {
            const counter: *std.atomic.Value(u32) = @ptrCast(@alignCast(context.?));
            _ = counter.fetchAdd(1, .seq_cst);
        }
    }.call;
    const watcher = try Watcher.initAllFiles(a, io, &.{root}, &count, callback);
    defer watcher.deinit();
    // Drive an idle read directly so polling before completion is deterministic.
    try watcher.setupWindowsWatch(root);
    try watcher.startWindowsRead(0);
    for (0..3) |_| {
        try watcher.processWindowsEvents(0);
        try std.testing.expect(watcher.impl.overlapped_data.items[0].read_pending);
        try std.testing.expect(!watcher.hasBackendFailed());
        try std.testing.expectEqual(0, count.load(.seq_cst));
    }
    watcher.cancelWindowsRead(0);
    try std.testing.expect(!watcher.impl.overlapped_data.items[0].read_pending);
    // Reusing the same record is safe only after cancellation has completed.
    try watcher.startWindowsRead(0);
    watcher.stop();
}

test "windows exact inputs survive registration growth and pending read shutdown" {
    if (builtin.os.tag != .windows) return error.SkipZigTest;
    const a = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", a);
    defer a.free(root);

    // Exceed both the initial array capacity and a single Windows wait shard.
    var paths: [Watcher.windows_max_wait_handles + 1][]const u8 = undefined;
    var initialized: usize = 0;
    defer for (paths[0..initialized]) |path| a.free(path);
    for (&paths, 0..) |*path, i| {
        var buffer: [64]u8 = undefined;
        const dir = try std.fmt.bufPrint(&buffer, "input-{d}", .{i});
        try tmp.dir.createDirPath(io, dir);
        path.* = try std.fs.path.join(a, &.{ root, dir, "asset.txt" });
        initialized += 1;
        try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = path.*, .data = "initial" });
    }
    const callback = struct {
        fn call(_: ?*anyopaque, _: WatchEvent) void {}
    }.call;
    const watcher = try Watcher.initInputs(a, io, &paths, null, callback);
    defer watcher.deinit();

    // Stop idle reads, then repeatedly complete/rearm reads and stop again.
    try watcher.start();
    watcher.stop();
    for (0..3) |_| {
        try watcher.start();
        for (paths, 0..) |path, i| {
            try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = path, .data = "changed" });
            const start = std.Io.Clock.now(.awake, io);
            while (!watcher.takeInputChange(i)) {
                try std.testing.expect(!watcher.hasBackendFailed());
                if (start.durationTo(std.Io.Clock.now(.awake, io)).toMilliseconds() > 5000) {
                    return error.EventsNotReceived;
                }
                std.Thread.yield() catch {};
            }
        }
        watcher.stop();
        try std.testing.expect(!watcher.hasBackendFailed());
        try std.testing.expectEqual(0, watcher.impl.handles.items.len);
        try std.testing.expectEqual(0, watcher.impl.overlapped_data.items.len);
        for (paths, 0..) |_, i| _ = watcher.takeInputChange(i);
    }
}

test "windows unicode filename handling" {
    if (builtin.os.tag != .windows) return;

    const allocator = std.testing.allocator;
    const io = std.testing.io;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(temp_path);

    const global = struct {
        var event_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
        var last_path: ?[]const u8 = null;
        var mutex: std.Io.Mutex = std.Io.Mutex.init;
        var io_handle: std.Io = undefined;
    };
    global.io_handle = io;

    const callback = struct {
        fn cb(event: WatchEvent) void {
            bumpEventCount(global);
            global.mutex.lockUncancelable(global.io_handle);
            defer global.mutex.unlock(global.io_handle);
            global.last_path = event.path;
        }
    }.cb;

    const watcher = try Watcher.init(allocator, io, &.{temp_path}, callback);
    defer watcher.deinit();

    try watcher.start();

    // Test with unicode filename (Chinese characters)
    const unicode_filename = "测试文件.roc";
    try temp_dir.dir.writeFile(io, .{ .sub_path = unicode_filename, .data = "unicode content" });
    try waitForEvents(&global.event_count, 1, 5000, io);

    // Test with accented characters
    const accented_filename = "café.roc";
    try temp_dir.dir.writeFile(io, .{ .sub_path = accented_filename, .data = "accented content" });
    try waitForEvents(&global.event_count, 2, 5000, io);

    watcher.stop();

    try expectEventsOrSkip(&global.event_count, 2);
}

test "windows long path handling" {
    if (builtin.os.tag != .windows) return;

    const allocator = std.testing.allocator;
    const io = std.testing.io;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(temp_path);

    // Create a nested directory structure to test long paths
    const long_dir_name = "very_long_directory_name_that_helps_test_path_length_handling";

    // Build the nested path
    var path_components = std.array_list.Managed([]const u8).init(allocator);
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
    var current_path = std.ArrayList(u8).empty;
    defer current_path.deinit(allocator);

    for (path_components.items) |component| {
        if (current_path.items.len > 0) {
            try current_path.append(allocator, std.fs.path.sep);
        }
        try current_path.appendSlice(allocator, component);
        try temp_dir.dir.createDirPath(io, current_path.items);
    }

    // Open the deepest directory
    var current_dir = try temp_dir.dir.openDir(io, current_path.items, .{});
    defer current_dir.close(io);

    const global = struct {
        var event_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
    };

    const callback = struct {
        fn cb(_: WatchEvent) void {
            bumpEventCount(global);
        }
    }.cb;

    const watcher = try Watcher.init(allocator, io, &.{temp_path}, callback);
    defer watcher.deinit();

    try watcher.start();

    // Create a file in the deeply nested directory
    try current_dir.writeFile(io, .{ .sub_path = "deep_file.roc", .data = "deep content" });
    try waitForEvents(&global.event_count, 1, 5000, io);

    watcher.stop();

    try expectEventsOrSkip(&global.event_count, 1);
}

// Repro for https://github.com/roc-lang/roc/issues/11644
//
// Watch mode must not register watches for unrelated build, cache, and VCS
// directories merely because they share a parent directory with a source file.
// This test creates a directory tree containing a `Main.roc` file next to
// `.git`, `target`, and `.claude` trees (the build/cache/VCS trees named in the
// issue) and asserts that the Linux backend registers watches only for
// directories that could contain relevant program inputs—no watched
// directory may be inside (or be) one of those unrelated trees.
//
// Currently the recursive Linux registration watches every subdirectory, so
// `.git/nested`, `target/nested`, and `.claude/nested` consume watches and this
// test fails. Once registration scales with relevant program inputs, all
// assertions pass without this test being edited.
test "Linux watching does not register watches for unrelated build, cache, and VCS directories" {
    if (builtin.os.tag != .linux) return error.SkipZigTest;

    const allocator = std.testing.allocator;
    const io = std.testing.io;

    var temp_dir = std.testing.tmpDir(.{});
    defer temp_dir.cleanup();

    const temp_path = try temp_dir.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(temp_path);

    // The source file from the issue's reproduction.
    try temp_dir.dir.writeFile(io, .{
        .sub_path = "Main.roc",
        .data = "Main := [].{\n    answer : U64\n    answer = 42\n}",
    });

    // Unrelated build, cache, and VCS trees, each with a nested subdirectory.
    const unrelated_trees = [_][]const u8{ ".git", "target", ".claude" };
    for (unrelated_trees) |tree| {
        const nested = try std.fs.path.join(allocator, &.{ tree, "nested" });
        defer allocator.free(nested);
        try temp_dir.dir.createDirPath(io, nested);
    }

    const callback = struct {
        fn cb(_: ?*anyopaque, _: WatchEvent) void {}
    }.cb;

    // The CLI's source-input watch setup uses `initAllFiles`.
    const watcher = try Watcher.initAllFiles(allocator, io, &.{temp_path}, null, callback);
    defer watcher.deinit();

    try watcher.start();

    // Wait until the watch thread finished registering watches (or failed).
    const ready_start = std.Io.Clock.now(.awake, io);
    while (!watcher.is_ready.load(.seq_cst)) {
        try std.testing.expect(!watcher.startup_failed.load(.seq_cst));
        const elapsed = ready_start.durationTo(std.Io.Clock.now(.awake, io)).toMilliseconds();
        try std.testing.expect(elapsed <= 5000);
        std.Thread.yield() catch {};
    }

    // The project root itself must be watched so that detection of new and
    // missing imports keeps working.
    const watch_descriptors = watcher.impl.watch_descriptors.items;
    try std.testing.expect(watch_descriptors.len >= 1);

    for (watch_descriptors) |watch| {
        var components = std.mem.splitScalar(u8, watch.path, std.fs.path.sep);
        while (components.next()) |component| {
            for (unrelated_trees) |tree| {
                try std.testing.expect(!std.mem.eql(u8, component, tree));
            }
        }
    }

    watcher.stop();
}
