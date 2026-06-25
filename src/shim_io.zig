//! Minimal std.Io implementation for shim and platform-host code.
//!
//! The shims only need std.Io for std.Io.Mutex and for reading the small
//! shared-memory coordination file. The test platform hosts also use
//! std.debug.print plus simple stdin/stdout/stderr effects. Using
//! std.Io.Threaded in those static archives pulls the full threaded vtable into
//! user program links, including filesystem/stat/timestamp code that user
//! programs should not have to link.

const std = @import("std");
const builtin = @import("builtin");

/// Returns a minimal std.Io implementation for shim and platform-host code.
pub fn io() std.Io {
    return switch (comptime builtin.os.tag) {
        .linux => .{ .userdata = null, .vtable = &linux_vtable },
        .windows => .{ .userdata = null, .vtable = &windows_vtable },
        .driverkit, .ios, .maccatalyst, .macos, .tvos, .visionos, .watchos => .{
            .userdata = null,
            .vtable = &macos_vtable,
        },
        // Any other host (FreeBSD/NetBSD/OpenBSD/Solaris/etc.) falls through to
        // std.Io.failing, whose vtable panics on every method call. This is a
        // trap rather than working IO — those targets aren't supported by the
        // shim yet and need their own vtable added above.
        else => std.Io.failing,
    };
}

/// Returns no ELF debug info search paths, avoiding filesystem access in user programs.
pub fn elfDebugInfoSearchPaths(_: []const u8) switch (builtin.object_format) {
    .elf => std.debug.ElfFile.DebugInfoSearchPaths,
    else => void,
} {
    switch (comptime builtin.object_format) {
        .elf => return std.debug.ElfFile.DebugInfoSearchPaths.none,
        else => return,
    }
}

/// Shared `std.Options` value for shim and platform-host code that needs to disable
/// std stack tracing. Zig 0.16's `std.debug.SelfInfo` on Windows references
/// `ntdll.LdrRegisterDllNotification`, which isn't linked into roc-compiled
/// programs that embed these static archives — leaving stack tracing on would
/// trigger an unresolved-symbol link error. Hosts that need extra fields
/// (e.g. `logFn`, `log_level`) should declare their own `std.Options` literal
/// rather than alias this one.
pub const std_options_no_stack_tracing: std.Options = .{ .allow_stack_tracing = false };

const linux_vtable: std.Io.VTable = blk: {
    var vtable = std.Io.failing.vtable.*;
    vtable.futexWait = linuxFutexWait;
    vtable.futexWaitUncancelable = linuxFutexWaitUncancelable;
    vtable.futexWake = linuxFutexWake;
    vtable.operate = linuxOperate;
    vtable.processExecutablePath = linuxProcessExecutablePath;
    vtable.lockStderr = linuxLockStderr;
    vtable.tryLockStderr = linuxTryLockStderr;
    vtable.unlockStderr = linuxUnlockStderr;
    vtable.swapCancelProtection = linuxSwapCancelProtection;
    vtable.dirOpenFile = linuxDirOpenFile;
    vtable.fileReadPositional = linuxFileReadPositional;
    vtable.fileClose = linuxFileClose;
    break :blk vtable;
};

var stderr_writer: std.Io.File.Writer = undefined;
var stderr_writer_initialized = false;

fn linuxFutexWait(_: ?*anyopaque, ptr: *const u32, expected: u32, _: std.Io.Timeout) std.Io.Cancelable!void {
    linuxFutexWaitUncancelable(null, ptr, expected);
}

fn linuxFutexWaitUncancelable(_: ?*anyopaque, ptr: *const u32, expected: u32) void {
    if (builtin.single_threaded) unreachable;

    const linux = std.os.linux;
    switch (linux.errno(linux.futex_4arg(
        ptr,
        .{ .cmd = .WAIT, .private = true },
        expected,
        null,
    ))) {
        .SUCCESS, .INTR, .AGAIN, .INVAL, .TIMEDOUT => {},
        .FAULT => unreachable,
        else => unreachable,
    }
}

fn linuxFutexWake(_: ?*anyopaque, ptr: *const u32, max_waiters: u32) void {
    if (max_waiters == 0 or builtin.single_threaded) return;

    const linux = std.os.linux;
    switch (linux.errno(linux.futex_3arg(
        ptr,
        .{ .cmd = .WAKE, .private = true },
        @min(max_waiters, std.math.maxInt(i32)),
    ))) {
        .SUCCESS, .INVAL, .FAULT => {},
        else => unreachable,
    }
}

fn linuxOperate(_: ?*anyopaque, operation: std.Io.Operation) std.Io.Cancelable!std.Io.Operation.Result {
    return switch (operation) {
        .file_read_streaming => |op| .{ .file_read_streaming = linuxFileReadStreaming(op.file, op.data) },
        .file_write_streaming => |op| .{ .file_write_streaming = linuxFileWriteStreaming(op.file, op.header, op.data, op.splat) },
        .device_io_control => @panic("device_io_control unsupported in shim"),
        .net_receive => .{ .net_receive = .{ error.NetworkDown, 0 } },
    };
}

fn linuxLockStderr(_: ?*anyopaque, terminal_mode: ?std.Io.Terminal.Mode) std.Io.Cancelable!std.Io.LockedStderr {
    initStderrWriter();
    return .{
        .file_writer = &stderr_writer,
        .terminal_mode = terminal_mode orelse .no_color,
    };
}

fn linuxTryLockStderr(_: ?*anyopaque, _: ?std.Io.Terminal.Mode) std.Io.Cancelable!?std.Io.LockedStderr {
    initStderrWriter();
    return .{
        .file_writer = &stderr_writer,
        .terminal_mode = .no_color,
    };
}

fn linuxUnlockStderr(_: ?*anyopaque) void {
    if (stderr_writer.err == null) stderr_writer.interface.flush() catch {};
    stderr_writer.err = null;
    stderr_writer.interface.end = 0;
    stderr_writer.interface.buffer = &.{};
}

fn linuxSwapCancelProtection(_: ?*anyopaque, _: std.Io.CancelProtection) std.Io.CancelProtection {
    return .unblocked;
}

fn initStderrWriter() void {
    if (stderr_writer_initialized) return;
    stderr_writer = std.Io.File.stderr().writerStreaming(io(), &.{});
    stderr_writer_initialized = true;
}

fn linuxProcessExecutablePath(_: ?*anyopaque, out_buffer: []u8) std.process.ExecutablePathError!usize {
    const linux = std.os.linux;
    const rc = linux.readlink("/proc/self/exe", out_buffer.ptr, out_buffer.len);
    switch (linux.errno(rc)) {
        .SUCCESS => {
            if (rc == out_buffer.len) return error.NameTooLong;
            return rc;
        },
        .ACCES => return error.AccessDenied,
        .LOOP => return error.SymLinkLoop,
        .NAMETOOLONG => return error.NameTooLong,
        .NOENT => return error.FileNotFound,
        .NOMEM => return error.SystemResources,
        .NOTDIR => return error.NotDir,
        else => return error.Unexpected,
    }
}

fn linuxDirOpenFile(
    _: ?*anyopaque,
    dir: std.Io.Dir,
    sub_path: []const u8,
    options: std.Io.Dir.OpenFileOptions,
) std.Io.File.OpenError!std.Io.File {
    const linux = std.os.linux;

    if (std.mem.findScalar(u8, sub_path, 0) != null) return error.BadPathName;
    const sub_path_posix = std.posix.toPosixPath(sub_path) catch return error.NameTooLong;

    var flags: linux.O = .{
        .ACCMODE = switch (options.mode) {
            .read_only => .RDONLY,
            .write_only => .WRONLY,
            .read_write => .RDWR,
        },
    };
    if (@hasField(linux.O, "CLOEXEC")) flags.CLOEXEC = true;
    if (@hasField(linux.O, "NOCTTY")) flags.NOCTTY = !options.allow_ctty;
    if (@hasField(linux.O, "NOFOLLOW")) flags.NOFOLLOW = !options.follow_symlinks;
    if (@hasField(linux.O, "PATH")) flags.PATH = options.path_only;

    const rc = linux.openat(dir.handle, &sub_path_posix, flags, 0);
    switch (linux.errno(rc)) {
        .SUCCESS => return .{
            .handle = @intCast(rc),
            .flags = .{ .nonblocking = false },
        },
        .ACCES => return error.AccessDenied,
        .AGAIN => return error.WouldBlock,
        .BADF => return error.Unexpected,
        .BUSY => return error.DeviceBusy,
        .EXIST => return error.PathAlreadyExists,
        .FBIG, .OVERFLOW => return error.FileTooBig,
        .FAULT => return error.Unexpected,
        .INVAL => return error.BadPathName,
        .IO => return error.Unexpected,
        .ISDIR => return error.IsDir,
        .LOOP => return error.SymLinkLoop,
        .MFILE => return error.ProcessFdQuotaExceeded,
        .NAMETOOLONG => return error.NameTooLong,
        .NFILE => return error.SystemFdQuotaExceeded,
        .NODEV, .NXIO => return error.NoDevice,
        .NOENT, .SRCH => return error.FileNotFound,
        .NOMEM => return error.SystemResources,
        .NOSPC => return error.NoSpaceLeft,
        .NOTDIR => return error.NotDir,
        .PERM => return error.PermissionDenied,
        .ROFS => return error.ReadOnlyFileSystem,
        .TXTBSY => return error.FileBusy,
        else => return error.Unexpected,
    }
}

fn linuxFileReadPositional(
    _: ?*anyopaque,
    file: std.Io.File,
    data: []const []u8,
    offset: u64,
) std.Io.File.ReadPositionalError!usize {
    var total: usize = 0;

    for (data) |buffer| {
        if (buffer.len == 0) continue;
        const n = try linuxPread(file.handle, buffer, offset + total);
        total += n;
        if (n < buffer.len) break;
    }

    return total;
}

fn linuxFileReadStreaming(
    file: std.Io.File,
    data: []const []u8,
) std.Io.Operation.FileReadStreaming.Result {
    for (data) |buffer| {
        if (buffer.len == 0) continue;
        return linuxRead(file.handle, buffer);
    }

    return 0;
}

fn linuxRead(fd: std.posix.fd_t, buffer: []u8) std.Io.Operation.FileReadStreaming.Result {
    const linux = std.os.linux;

    while (true) {
        const rc = linux.read(fd, buffer.ptr, buffer.len);
        switch (linux.errno(rc)) {
            .SUCCESS => return if (rc == 0) error.EndOfStream else rc,
            .INTR => continue,
            .AGAIN => return error.WouldBlock,
            .BADF => return error.NotOpenForReading,
            .IO => return error.InputOutput,
            .ISDIR => return error.IsDir,
            .NOBUFS, .NOMEM => return error.SystemResources,
            .NOTCONN => return error.SocketUnconnected,
            .CONNRESET => return error.ConnectionResetByPeer,
            .INVAL, .FAULT => return error.Unexpected,
            else => return error.Unexpected,
        }
    }
}

fn linuxFileWriteStreaming(
    file: std.Io.File,
    header: []const u8,
    data: []const []const u8,
    splat: usize,
) std.Io.Operation.FileWriteStreaming.Result {
    var total: usize = 0;

    if (header.len > 0) {
        const n = try linuxWrite(file.handle, header);
        total += n;
        if (n < header.len) return total;
    }

    for (data, 0..) |buffer, index| {
        const repeat_count = if (index == data.len - 1) splat else 1;
        for (0..repeat_count) |_| {
            if (buffer.len == 0) continue;
            const n = try linuxWrite(file.handle, buffer);
            total += n;
            if (n < buffer.len) return total;
        }
    }

    return total;
}

fn linuxWrite(fd: std.posix.fd_t, buffer: []const u8) std.Io.Operation.FileWriteStreaming.Result {
    const linux = std.os.linux;

    while (true) {
        const rc = linux.write(fd, buffer.ptr, buffer.len);
        switch (linux.errno(rc)) {
            .SUCCESS => return rc,
            .INTR => continue,
            .AGAIN => return error.WouldBlock,
            .BADF => return error.NotOpenForWriting,
            .BUSY => return error.DeviceBusy,
            .DQUOT => return error.DiskQuota,
            .FBIG => return error.FileTooBig,
            .IO => return error.InputOutput,
            .NODEV, .NXIO => return error.NoDevice,
            .NOSPC => return error.NoSpaceLeft,
            .PERM => return error.PermissionDenied,
            .PIPE => return error.BrokenPipe,
            .TXTBSY => return error.FileBusy,
            .INVAL, .FAULT, .DESTADDRREQ, .CONNRESET => return error.Unexpected,
            else => return error.Unexpected,
        }
    }
}

fn linuxPread(fd: std.posix.fd_t, buffer: []u8, offset: u64) std.Io.File.ReadPositionalError!usize {
    const linux = std.os.linux;
    const signed_offset = std.math.cast(i64, offset) orelse return error.Unseekable;

    while (true) {
        const rc = linux.pread(fd, buffer.ptr, buffer.len, signed_offset);
        switch (linux.errno(rc)) {
            .SUCCESS => return rc,
            .INTR => continue,
            .AGAIN => return error.WouldBlock,
            .BADF => return error.NotOpenForReading,
            .IO => return error.InputOutput,
            .ISDIR => return error.IsDir,
            .NOBUFS, .NOMEM => return error.SystemResources,
            .NXIO, .SPIPE, .OVERFLOW => return error.Unseekable,
            .INVAL, .FAULT => return error.Unexpected,
            else => return error.Unexpected,
        }
    }
}

fn linuxFileClose(_: ?*anyopaque, files: []const std.Io.File) void {
    const linux = std.os.linux;
    for (files) |file| {
        _ = linux.close(file.handle);
    }
}

const windows_vtable: std.Io.VTable = blk: {
    var vtable = std.Io.failing.vtable.*;
    vtable.futexWait = windowsFutexWait;
    vtable.futexWaitUncancelable = windowsFutexWaitUncancelable;
    vtable.futexWake = windowsFutexWake;
    vtable.operate = windowsOperate;
    vtable.processExecutablePath = windowsProcessExecutablePath;
    vtable.lockStderr = windowsLockStderr;
    vtable.tryLockStderr = windowsTryLockStderr;
    vtable.unlockStderr = windowsUnlockStderr;
    vtable.swapCancelProtection = windowsSwapCancelProtection;
    vtable.dirOpenFile = windowsDirOpenFile;
    vtable.fileReadPositional = windowsFileReadPositional;
    vtable.fileClose = windowsFileClose;
    break :blk vtable;
};

const win = struct {
    const BOOL = std.os.windows.BOOL;
    const DWORD = std.os.windows.DWORD;
    const HANDLE = std.os.windows.HANDLE;
    const SRWLOCK = std.os.windows.SRWLOCK;
    const SECURITY_ATTRIBUTES = std.os.windows.SECURITY_ATTRIBUTES;

    const OVERLAPPED = extern struct {
        Internal: usize,
        InternalHigh: usize,
        DUMMY: extern union {
            Pointer: ?*anyopaque,
            Offset: extern struct { Low: DWORD, High: DWORD },
        },
        hEvent: ?HANDLE,
    };

    // CreateFileW dwDesiredAccess
    const GENERIC_READ: DWORD = 0x80000000;
    const GENERIC_WRITE: DWORD = 0x40000000;

    // CreateFileW dwShareMode
    const FILE_SHARE_READ: DWORD = 0x00000001;
    const FILE_SHARE_WRITE: DWORD = 0x00000002;
    const FILE_SHARE_DELETE: DWORD = 0x00000004;

    // CreateFileW dwCreationDisposition
    const OPEN_EXISTING: DWORD = 3;

    // CreateFileW dwFlagsAndAttributes
    const FILE_ATTRIBUTE_NORMAL: DWORD = 0x00000080;
    const FILE_FLAG_BACKUP_SEMANTICS: DWORD = 0x02000000;

    // Returned on failure.
    const INVALID_HANDLE_VALUE: HANDLE = @ptrFromInt(std.math.maxInt(usize));

    extern "kernel32" fn WriteFile(
        hFile: HANDLE,
        lpBuffer: [*]const u8,
        nNumberOfBytesToWrite: DWORD,
        lpNumberOfBytesWritten: ?*DWORD,
        lpOverlapped: ?*OVERLAPPED,
    ) callconv(.winapi) BOOL;

    extern "kernel32" fn ReadFile(
        hFile: HANDLE,
        lpBuffer: [*]u8,
        nNumberOfBytesToRead: DWORD,
        lpNumberOfBytesRead: ?*DWORD,
        lpOverlapped: ?*OVERLAPPED,
    ) callconv(.winapi) BOOL;

    extern "kernel32" fn CloseHandle(hObject: HANDLE) callconv(.winapi) BOOL;

    extern "kernel32" fn GetModuleFileNameW(
        hModule: ?*anyopaque,
        lpFilename: [*]u16,
        nSize: DWORD,
    ) callconv(.winapi) DWORD;

    extern "kernel32" fn CreateFileW(
        lpFileName: [*:0]const u16,
        dwDesiredAccess: DWORD,
        dwShareMode: DWORD,
        lpSecurityAttributes: ?*SECURITY_ATTRIBUTES,
        dwCreationDisposition: DWORD,
        dwFlagsAndAttributes: DWORD,
        hTemplateFile: ?HANDLE,
    ) callconv(.winapi) HANDLE;
};

var windows_stderr_writer: std.Io.File.Writer = undefined;
var windows_stderr_writer_initialized = false;
var windows_stderr_lock: win.SRWLOCK = .{};

fn windowsInitStderrWriter() void {
    if (windows_stderr_writer_initialized) return;
    windows_stderr_writer = std.Io.File.stderr().writerStreaming(io(), &.{});
    windows_stderr_writer_initialized = true;
}

// RtlWaitOnAddress is the Windows futex analogue (available since Win8).
fn windowsFutexWait(_: ?*anyopaque, ptr: *const u32, expected: u32, _: std.Io.Timeout) std.Io.Cancelable!void {
    windowsFutexWaitUncancelable(null, ptr, expected);
}

fn windowsFutexWaitUncancelable(_: ?*anyopaque, ptr: *const u32, expected: u32) void {
    if (builtin.single_threaded) unreachable;
    var compare = expected;
    _ = std.os.windows.ntdll.RtlWaitOnAddress(
        @ptrCast(ptr),
        @ptrCast(&compare),
        @sizeOf(u32),
        null,
    );
}

fn windowsFutexWake(_: ?*anyopaque, ptr: *const u32, max_waiters: u32) void {
    if (max_waiters == 0 or builtin.single_threaded) return;
    if (max_waiters == 1) {
        std.os.windows.ntdll.RtlWakeAddressSingle(@ptrCast(ptr));
    } else {
        std.os.windows.ntdll.RtlWakeAddressAll(@ptrCast(ptr));
    }
}

fn windowsOperate(_: ?*anyopaque, operation: std.Io.Operation) std.Io.Cancelable!std.Io.Operation.Result {
    return switch (operation) {
        .file_read_streaming => |op| .{ .file_read_streaming = windowsFileReadStreaming(op.file, op.data) },
        .file_write_streaming => |op| .{ .file_write_streaming = windowsFileWriteStreaming(op.file, op.header, op.data, op.splat) },
        .device_io_control => @panic("device_io_control unsupported in shim"),
        .net_receive => .{ .net_receive = .{ error.NetworkDown, 0 } },
    };
}

fn windowsLockStderr(_: ?*anyopaque, terminal_mode: ?std.Io.Terminal.Mode) std.Io.Cancelable!std.Io.LockedStderr {
    windowsInitStderrWriter();
    std.os.windows.ntdll.RtlAcquireSRWLockExclusive(&windows_stderr_lock);
    return .{
        .file_writer = &windows_stderr_writer,
        .terminal_mode = terminal_mode orelse .no_color,
    };
}

fn windowsTryLockStderr(_: ?*anyopaque, _: ?std.Io.Terminal.Mode) std.Io.Cancelable!?std.Io.LockedStderr {
    windowsInitStderrWriter();
    if (std.os.windows.ntdll.RtlTryAcquireSRWLockExclusive(&windows_stderr_lock) == .FALSE) return null;
    return .{
        .file_writer = &windows_stderr_writer,
        .terminal_mode = .no_color,
    };
}

fn windowsUnlockStderr(_: ?*anyopaque) void {
    if (windows_stderr_writer.err == null) windows_stderr_writer.interface.flush() catch {};
    windows_stderr_writer.err = null;
    windows_stderr_writer.interface.end = 0;
    windows_stderr_writer.interface.buffer = &.{};
    std.os.windows.ntdll.RtlReleaseSRWLockExclusive(&windows_stderr_lock);
}

fn windowsSwapCancelProtection(_: ?*anyopaque, _: std.Io.CancelProtection) std.Io.CancelProtection {
    return .unblocked;
}

fn windowsProcessExecutablePath(_: ?*anyopaque, out_buffer: []u8) std.process.ExecutablePathError!usize {
    var wbuf: [std.os.windows.PATH_MAX_WIDE]u16 = undefined;
    const n = win.GetModuleFileNameW(null, &wbuf, wbuf.len);
    if (n == 0) return error.Unexpected;
    if (n == wbuf.len) {
        // Buffer too small per Win32 docs; also signaled via GetLastError == ERROR_INSUFFICIENT_BUFFER.
        return error.NameTooLong;
    }
    const wide_slice = wbuf[0..n];
    const required = std.unicode.calcWtf8Len(wide_slice);
    if (required > out_buffer.len) return error.NameTooLong;
    return std.unicode.wtf16LeToWtf8(out_buffer, wide_slice);
}

fn windowsFileReadStreaming(
    file: std.Io.File,
    data: []const []u8,
) std.Io.Operation.FileReadStreaming.Result {
    for (data) |buffer| {
        if (buffer.len == 0) continue;
        return windowsRead(file.handle, buffer);
    }
    return 0;
}

fn windowsRead(handle: std.posix.fd_t, buffer: []u8) std.Io.Operation.FileReadStreaming.Result {
    const want: u32 = @intCast(@min(buffer.len, std.math.maxInt(u32)));
    var read_count: win.DWORD = 0;
    if (win.ReadFile(handle, buffer.ptr, want, &read_count, null) == .FALSE) {
        return switch (std.os.windows.GetLastError()) {
            .BROKEN_PIPE, .HANDLE_EOF => error.EndOfStream,
            .NO_DATA => error.WouldBlock,
            .INVALID_HANDLE => error.NotOpenForReading,
            .ACCESS_DENIED => error.AccessDenied,
            .LOCK_VIOLATION => error.LockViolation,
            .IO_DEVICE, .CRC, .NET_WRITE_FAULT => error.InputOutput,
            .OPERATION_ABORTED => error.Unexpected,
            else => error.Unexpected,
        };
    }
    if (read_count == 0) return error.EndOfStream;
    return read_count;
}

fn windowsFileWriteStreaming(
    file: std.Io.File,
    header: []const u8,
    data: []const []const u8,
    splat: usize,
) std.Io.Operation.FileWriteStreaming.Result {
    var total: usize = 0;

    if (header.len > 0) {
        const n = try windowsWrite(file.handle, header);
        total += n;
        if (n < header.len) return total;
    }

    for (data, 0..) |buffer, index| {
        const repeat_count = if (index == data.len - 1) splat else 1;
        for (0..repeat_count) |_| {
            if (buffer.len == 0) continue;
            const n = try windowsWrite(file.handle, buffer);
            total += n;
            if (n < buffer.len) return total;
        }
    }

    return total;
}

fn windowsWrite(handle: std.posix.fd_t, buffer: []const u8) std.Io.Operation.FileWriteStreaming.Result {
    const want: u32 = @intCast(@min(buffer.len, std.math.maxInt(u32)));
    var written: win.DWORD = 0;
    if (win.WriteFile(handle, buffer.ptr, want, &written, null) == .FALSE) {
        return switch (std.os.windows.GetLastError()) {
            .INVALID_USER_BUFFER => error.SystemResources,
            .NOT_ENOUGH_MEMORY => error.SystemResources,
            .OPERATION_ABORTED => error.Unexpected,
            .NOT_ENOUGH_QUOTA => error.SystemResources,
            .IO_PENDING => error.Unexpected,
            .BROKEN_PIPE => error.BrokenPipe,
            .INVALID_HANDLE => error.NotOpenForWriting,
            .LOCK_VIOLATION => error.LockViolation,
            .NETNAME_DELETED => error.BrokenPipe,
            .ACCESS_DENIED => error.AccessDenied,
            .IO_DEVICE, .CRC, .NET_WRITE_FAULT => error.InputOutput,
            .DISK_FULL, .HANDLE_DISK_FULL => error.NoSpaceLeft,
            .NO_DATA => error.WouldBlock,
            else => error.Unexpected,
        };
    }
    return written;
}

fn windowsDirOpenFile(
    _: ?*anyopaque,
    _: std.Io.Dir,
    sub_path: []const u8,
    options: std.Io.Dir.OpenFileOptions,
) std.Io.File.OpenError!std.Io.File {
    // sub_path is WTF-8 on Windows; convert to a NUL-terminated WTF-16 path.
    // We use the simple Win32 CreateFileW path (rather than NtCreateFile) to
    // keep the shim small. Relative paths are resolved against the process cwd
    // rather than `dir.handle`; this is sufficient for the small coordination
    // file the shim needs, which is always specified by absolute path.
    var path_w_buf: [std.os.windows.PATH_MAX_WIDE + 1]u16 = undefined;
    const path_w_len = std.unicode.wtf8ToWtf16Le(&path_w_buf, sub_path) catch return error.BadPathName;
    if (path_w_len >= path_w_buf.len) return error.NameTooLong;
    path_w_buf[path_w_len] = 0;
    const path_w: [*:0]const u16 = @ptrCast(&path_w_buf);

    var desired_access: win.DWORD = 0;
    switch (options.mode) {
        .read_only => desired_access |= win.GENERIC_READ,
        .write_only => desired_access |= win.GENERIC_WRITE,
        .read_write => desired_access |= win.GENERIC_READ | win.GENERIC_WRITE,
    }

    const share_mode = win.FILE_SHARE_READ | win.FILE_SHARE_WRITE | win.FILE_SHARE_DELETE;

    var flags_and_attrs: win.DWORD = win.FILE_ATTRIBUTE_NORMAL;
    if (options.allow_directory) flags_and_attrs |= win.FILE_FLAG_BACKUP_SEMANTICS;

    const handle = win.CreateFileW(
        path_w,
        desired_access,
        share_mode,
        null,
        win.OPEN_EXISTING,
        flags_and_attrs,
        null,
    );
    if (handle == win.INVALID_HANDLE_VALUE) {
        return switch (std.os.windows.GetLastError()) {
            .FILE_NOT_FOUND, .PATH_NOT_FOUND => error.FileNotFound,
            .ACCESS_DENIED => error.AccessDenied,
            .SHARING_VIOLATION => error.FileBusy,
            .PIPE_BUSY => error.PipeBusy,
            .INVALID_NAME, .BAD_PATHNAME => error.BadPathName,
            // ERROR_DIRECTORY = "The directory name is invalid"; closest match
            // for the OpenError set is NotDir.
            .DIRECTORY => error.NotDir,
            .NOT_ENOUGH_MEMORY, .OUTOFMEMORY => error.SystemResources,
            .TOO_MANY_OPEN_FILES => error.ProcessFdQuotaExceeded,
            .NETNAME_DELETED, .BAD_NETPATH => error.NetworkNotFound,
            else => error.Unexpected,
        };
    }
    return .{
        .handle = handle,
        .flags = .{ .nonblocking = false },
    };
}

fn windowsFileReadPositional(
    _: ?*anyopaque,
    file: std.Io.File,
    data: []const []u8,
    offset: u64,
) std.Io.File.ReadPositionalError!usize {
    var total: usize = 0;
    for (data) |buffer| {
        if (buffer.len == 0) continue;
        const n = try windowsPread(file.handle, buffer, offset + total);
        total += n;
        if (n < buffer.len) break;
    }
    return total;
}

fn windowsPread(handle: std.posix.fd_t, buffer: []u8, offset: u64) std.Io.File.ReadPositionalError!usize {
    const want: u32 = @intCast(@min(buffer.len, std.math.maxInt(u32)));
    var overlapped: win.OVERLAPPED = .{
        .Internal = 0,
        .InternalHigh = 0,
        .DUMMY = .{ .Offset = .{
            .Low = @truncate(offset),
            .High = @truncate(offset >> 32),
        } },
        .hEvent = null,
    };
    var read_count: win.DWORD = 0;
    if (win.ReadFile(handle, buffer.ptr, want, &read_count, &overlapped) == .FALSE) {
        return switch (std.os.windows.GetLastError()) {
            .HANDLE_EOF, .BROKEN_PIPE => 0,
            .INVALID_HANDLE => error.NotOpenForReading,
            .ACCESS_DENIED => error.AccessDenied,
            .LOCK_VIOLATION => error.LockViolation,
            .IO_DEVICE, .CRC, .NET_WRITE_FAULT => error.InputOutput,
            .NO_DATA => error.WouldBlock,
            .OPERATION_ABORTED => error.Unexpected,
            else => error.Unexpected,
        };
    }
    return read_count;
}

fn windowsFileClose(_: ?*anyopaque, files: []const std.Io.File) void {
    for (files) |file| {
        _ = win.CloseHandle(file.handle);
    }
}

const macos_vtable: std.Io.VTable = blk: {
    var vtable = std.Io.failing.vtable.*;
    vtable.futexWait = macosFutexWait;
    vtable.futexWaitUncancelable = macosFutexWaitUncancelable;
    vtable.futexWake = macosFutexWake;
    vtable.operate = macosOperate;
    vtable.processExecutablePath = macosProcessExecutablePath;
    vtable.lockStderr = macosLockStderr;
    vtable.tryLockStderr = macosTryLockStderr;
    vtable.unlockStderr = macosUnlockStderr;
    vtable.swapCancelProtection = macosSwapCancelProtection;
    vtable.dirOpenFile = macosDirOpenFile;
    vtable.fileReadPositional = macosFileReadPositional;
    vtable.fileClose = macosFileClose;
    break :blk vtable;
};

var macos_stderr_writer: std.Io.File.Writer = undefined;
var macos_stderr_writer_initialized = false;
var macos_stderr_lock: std.c.os_unfair_lock = .{};

fn macosInitStderrWriter() void {
    if (macos_stderr_writer_initialized) return;
    macos_stderr_writer = std.Io.File.stderr().writerStreaming(io(), &.{});
    macos_stderr_writer_initialized = true;
}

fn macosFutexWait(_: ?*anyopaque, ptr: *const u32, expected: u32, _: std.Io.Timeout) std.Io.Cancelable!void {
    macosFutexWaitUncancelable(null, ptr, expected);
}

fn macosFutexWaitUncancelable(_: ?*anyopaque, ptr: *const u32, expected: u32) void {
    if (builtin.single_threaded) unreachable;
    const flags: std.c.UL = .{ .op = .COMPARE_AND_WAIT, .NO_ERRNO = true };
    const status = std.c.__ulock_wait(flags, ptr, expected, 0);
    if (status >= 0) return;
    switch (@as(std.c.E, @enumFromInt(-status))) {
        .INTR, .FAULT, .TIMEDOUT => {},
        else => unreachable,
    }
}

fn macosFutexWake(_: ?*anyopaque, ptr: *const u32, max_waiters: u32) void {
    if (max_waiters == 0 or builtin.single_threaded) return;
    const flags: std.c.UL = .{
        .op = .COMPARE_AND_WAIT,
        .NO_ERRNO = true,
        .WAKE_ALL = max_waiters > 1,
    };
    while (true) {
        const status = std.c.__ulock_wake(flags, ptr, 0);
        if (status >= 0) return;
        switch (@as(std.c.E, @enumFromInt(-status))) {
            .INTR => continue,
            .NOENT => return,
            else => unreachable,
        }
    }
}

fn macosOperate(_: ?*anyopaque, operation: std.Io.Operation) std.Io.Cancelable!std.Io.Operation.Result {
    return switch (operation) {
        .file_read_streaming => |op| .{ .file_read_streaming = macosFileReadStreaming(op.file, op.data) },
        .file_write_streaming => |op| .{ .file_write_streaming = macosFileWriteStreaming(op.file, op.header, op.data, op.splat) },
        .device_io_control => @panic("device_io_control unsupported in shim"),
        .net_receive => .{ .net_receive = .{ error.NetworkDown, 0 } },
    };
}

fn macosLockStderr(_: ?*anyopaque, terminal_mode: ?std.Io.Terminal.Mode) std.Io.Cancelable!std.Io.LockedStderr {
    macosInitStderrWriter();
    std.c.os_unfair_lock_lock(&macos_stderr_lock);
    return .{
        .file_writer = &macos_stderr_writer,
        .terminal_mode = terminal_mode orelse .no_color,
    };
}

fn macosTryLockStderr(_: ?*anyopaque, _: ?std.Io.Terminal.Mode) std.Io.Cancelable!?std.Io.LockedStderr {
    macosInitStderrWriter();
    if (!std.c.os_unfair_lock_trylock(&macos_stderr_lock)) return null;
    return .{
        .file_writer = &macos_stderr_writer,
        .terminal_mode = .no_color,
    };
}

fn macosUnlockStderr(_: ?*anyopaque) void {
    if (macos_stderr_writer.err == null) macos_stderr_writer.interface.flush() catch {};
    macos_stderr_writer.err = null;
    macos_stderr_writer.interface.end = 0;
    macos_stderr_writer.interface.buffer = &.{};
    std.c.os_unfair_lock_unlock(&macos_stderr_lock);
}

fn macosSwapCancelProtection(_: ?*anyopaque, _: std.Io.CancelProtection) std.Io.CancelProtection {
    return .unblocked;
}

fn macosProcessExecutablePath(_: ?*anyopaque, out_buffer: []u8) std.process.ExecutablePathError!usize {
    var n: u32 = std.math.cast(u32, out_buffer.len) orelse std.math.maxInt(u32);
    const rc = std.c._NSGetExecutablePath(out_buffer.ptr, &n);
    if (rc != 0) return error.NameTooLong;
    // _NSGetExecutablePath writes a NUL-terminated string; n is set to the
    // required buffer size including NUL on overflow, but on success it does
    // not update n. Determine the actual length from the NUL terminator.
    return std.mem.findScalar(u8, out_buffer, 0) orelse out_buffer.len;
}

fn macosDirOpenFile(
    _: ?*anyopaque,
    dir: std.Io.Dir,
    sub_path: []const u8,
    options: std.Io.Dir.OpenFileOptions,
) std.Io.File.OpenError!std.Io.File {
    if (std.mem.findScalar(u8, sub_path, 0) != null) return error.BadPathName;
    const sub_path_posix = std.posix.toPosixPath(sub_path) catch return error.NameTooLong;

    var flags: std.c.O = .{
        .ACCMODE = switch (options.mode) {
            .read_only => .RDONLY,
            .write_only => .WRONLY,
            .read_write => .RDWR,
        },
        .CLOEXEC = true,
        .NOCTTY = !options.allow_ctty,
        .NOFOLLOW = !options.follow_symlinks,
    };
    _ = &flags;

    const rc = std.c.openat(dir.handle, &sub_path_posix, flags, @as(std.c.mode_t, 0));
    switch (std.c.errno(rc)) {
        .SUCCESS => return .{
            .handle = @intCast(rc),
            .flags = .{ .nonblocking = false },
        },
        .ACCES => return error.AccessDenied,
        .AGAIN => return error.WouldBlock,
        .BADF => return error.Unexpected,
        .BUSY => return error.DeviceBusy,
        .EXIST => return error.PathAlreadyExists,
        .FBIG, .OVERFLOW => return error.FileTooBig,
        .FAULT => return error.Unexpected,
        .INVAL => return error.BadPathName,
        .IO => return error.Unexpected,
        .ISDIR => return error.IsDir,
        .LOOP => return error.SymLinkLoop,
        .MFILE => return error.ProcessFdQuotaExceeded,
        .NAMETOOLONG => return error.NameTooLong,
        .NFILE => return error.SystemFdQuotaExceeded,
        .NODEV, .NXIO => return error.NoDevice,
        .NOENT, .SRCH => return error.FileNotFound,
        .NOMEM => return error.SystemResources,
        .NOSPC => return error.NoSpaceLeft,
        .NOTDIR => return error.NotDir,
        .PERM => return error.PermissionDenied,
        .ROFS => return error.ReadOnlyFileSystem,
        .TXTBSY => return error.FileBusy,
        else => return error.Unexpected,
    }
}

fn macosFileReadPositional(
    _: ?*anyopaque,
    file: std.Io.File,
    data: []const []u8,
    offset: u64,
) std.Io.File.ReadPositionalError!usize {
    var total: usize = 0;
    for (data) |buffer| {
        if (buffer.len == 0) continue;
        const n = try macosPread(file.handle, buffer, offset + total);
        total += n;
        if (n < buffer.len) break;
    }
    return total;
}

fn macosFileReadStreaming(
    file: std.Io.File,
    data: []const []u8,
) std.Io.Operation.FileReadStreaming.Result {
    for (data) |buffer| {
        if (buffer.len == 0) continue;
        return macosRead(file.handle, buffer);
    }
    return 0;
}

fn macosRead(fd: std.posix.fd_t, buffer: []u8) std.Io.Operation.FileReadStreaming.Result {
    while (true) {
        const rc = std.c.read(fd, buffer.ptr, buffer.len);
        if (rc >= 0) return if (rc == 0) error.EndOfStream else @intCast(rc);
        switch (std.c.errno(rc)) {
            .INTR => continue,
            .AGAIN => return error.WouldBlock,
            .BADF => return error.NotOpenForReading,
            .IO => return error.InputOutput,
            .ISDIR => return error.IsDir,
            .NOBUFS, .NOMEM => return error.SystemResources,
            .NOTCONN => return error.SocketUnconnected,
            .CONNRESET => return error.ConnectionResetByPeer,
            .INVAL, .FAULT => return error.Unexpected,
            else => return error.Unexpected,
        }
    }
}

fn macosFileWriteStreaming(
    file: std.Io.File,
    header: []const u8,
    data: []const []const u8,
    splat: usize,
) std.Io.Operation.FileWriteStreaming.Result {
    var total: usize = 0;
    if (header.len > 0) {
        const n = try macosWrite(file.handle, header);
        total += n;
        if (n < header.len) return total;
    }
    for (data, 0..) |buffer, index| {
        const repeat_count = if (index == data.len - 1) splat else 1;
        for (0..repeat_count) |_| {
            if (buffer.len == 0) continue;
            const n = try macosWrite(file.handle, buffer);
            total += n;
            if (n < buffer.len) return total;
        }
    }
    return total;
}

fn macosWrite(fd: std.posix.fd_t, buffer: []const u8) std.Io.Operation.FileWriteStreaming.Result {
    while (true) {
        const rc = std.c.write(fd, buffer.ptr, buffer.len);
        if (rc >= 0) return @intCast(rc);
        switch (std.c.errno(rc)) {
            .INTR => continue,
            .AGAIN => return error.WouldBlock,
            .BADF => return error.NotOpenForWriting,
            .DQUOT => return error.DiskQuota,
            .FBIG => return error.FileTooBig,
            .IO => return error.InputOutput,
            .NODEV, .NXIO => return error.NoDevice,
            .NOSPC => return error.NoSpaceLeft,
            .PERM => return error.PermissionDenied,
            .PIPE => return error.BrokenPipe,
            .INVAL, .FAULT, .DESTADDRREQ, .CONNRESET => return error.Unexpected,
            else => return error.Unexpected,
        }
    }
}

fn macosPread(fd: std.posix.fd_t, buffer: []u8, offset: u64) std.Io.File.ReadPositionalError!usize {
    const signed_offset = std.math.cast(i64, offset) orelse return error.Unseekable;
    while (true) {
        const rc = std.c.pread(fd, buffer.ptr, buffer.len, signed_offset);
        if (rc >= 0) return @intCast(rc);
        switch (std.c.errno(rc)) {
            .INTR => continue,
            .AGAIN => return error.WouldBlock,
            .BADF => return error.NotOpenForReading,
            .IO => return error.InputOutput,
            .ISDIR => return error.IsDir,
            .NOBUFS, .NOMEM => return error.SystemResources,
            .NXIO, .SPIPE, .OVERFLOW => return error.Unseekable,
            .INVAL, .FAULT => return error.Unexpected,
            else => return error.Unexpected,
        }
    }
}

fn macosFileClose(_: ?*anyopaque, files: []const std.Io.File) void {
    for (files) |file| {
        _ = std.c.close(file.handle);
    }
}
