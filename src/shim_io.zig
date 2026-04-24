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
    if (comptime builtin.os.tag != .linux) {
        return std.Io.failing;
    }

    return .{
        .userdata = null,
        .vtable = &linux_vtable,
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
        .device_io_control => std.Io.failing.vtable.operate(null, operation) catch unreachable,
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

    if (std.mem.indexOfScalar(u8, sub_path, 0) != null) return error.BadPathName;
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
