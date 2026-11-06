//! The output executable of a fast dev link, created before the build's
//! compile work and given its name only when it is complete (plan "Output
//! file lifecycle"). The bytes written are the same however the file comes
//! to exist: on Linux as an anonymous file in the destination directory
//! when the filesystem has them, else a named temporary file beside the
//! destination; on macOS and the BSDs a named temporary file, which on
//! macOS may begin as a clone of the cached host record where the
//! filesystem shares blocks; on Windows a named temporary file.
//!
//! The output is written through the compiler host's own system calls,
//! whatever target the executable is for: the patch links write bytes, and
//! this is where the bytes go.
const std = @import("std");
const builtin = @import("builtin");

const Allocator = std.mem.Allocator;
const linux = std.os.linux;

/// The output executable of a fast dev link, written through the compiler
/// host's own system calls.
pub const OutputFile = if (builtin.os.tag == .linux) LinuxOutputFile else if (builtin.os.tag == .windows) WindowsOutputFile else PosixOutputFile;

/// The ways creating, writing or publishing an `OutputFile` can fail.
pub const OutputError = Allocator.Error || error{ AccessDenied, NoSpaceLeft, FileNotFound, Unexpected };

/// The cached host record an output begins as a copy of: its open file,
/// its path, and where in it the host's bytes start.
pub const HostSource = struct {
    handle: std.Io.File.Handle,
    path: []const u8,
    offset: u64,
};

fn failure(errno: anytype) OutputError {
    if (errno == .ACCES or errno == .PERM or errno == .ROFS) return error.AccessDenied;
    if (errno == .NOSPC or errno == .DQUOT) return error.NoSpaceLeft;
    if (errno == .NOENT or errno == .NOTDIR) return error.FileNotFound;
    return error.Unexpected;
}

/// The temporary name beside `dest_path` an output is written under.
fn tempName(arena: Allocator, dest_path: []const u8, pid: anytype) Allocator.Error![:0]const u8 {
    const dir = std.fs.path.dirname(dest_path);
    return std.fmt.allocPrintSentinel(arena, "{s}{s}.{s}.{d}.roc-link", .{ if (dir == null) "" else dir.?, if (dir == null) "" else std.fs.path.sep_str, std.fs.path.basename(dest_path), pid }, 0);
}

/// A Linux output: an anonymous file where the filesystem has them.
pub const LinuxOutputFile = struct {
    fd: linux.fd_t,
    /// The destination path, NUL-terminated.
    dest: [:0]const u8,
    /// The temporary name the file is linked under before the rename.
    temp: [:0]const u8,
    /// Whether the file already has `temp` as its name.
    named: bool,
    /// How far from the start `copyHost` and `writeStart` have written.
    written: u64 = 0,
    /// Whether `map` gave out a buffer instead of a mapping of the file,
    /// which `finish` writes back.
    buffered: bool = false,

    pub const Error = OutputError;

    /// Create the output for `dest_path`. Fails when the destination
    /// directory cannot hold it.
    pub fn create(arena: Allocator, dest_path: []const u8) Error!LinuxOutputFile {
        const dir = std.fs.path.dirname(dest_path) orelse ".";
        const dir_z = try arena.dupeSentinel(u8, dir, 0);
        const dest = try arena.dupeSentinel(u8, dest_path, 0);
        const temp = try tempName(arena, dest_path, linux.getpid());
        const anonymous = linux.openat(linux.AT.FDCWD, dir_z, .{ .ACCMODE = .RDWR, .TMPFILE = true, .DIRECTORY = true, .CLOEXEC = true }, 0o777);
        const anonymous_errno = linux.errno(anonymous);
        if (anonymous_errno == .SUCCESS) return .{ .fd = @intCast(anonymous), .dest = dest, .temp = temp, .named = false };
        // Without anonymous files on this filesystem, a named file stands in.
        if (anonymous_errno != .OPNOTSUPP and anonymous_errno != .ISDIR) return failure(anonymous_errno);
        const named = linux.openat(linux.AT.FDCWD, temp, .{ .ACCMODE = .RDWR, .CREAT = true, .TRUNC = true, .CLOEXEC = true }, 0o777);
        if (linux.errno(named) != .SUCCESS) return failure(linux.errno(named));
        return .{ .fd = @intCast(named), .dest = dest, .temp = temp, .named = true };
    }

    /// Begin the output as `bytes`, which are `source`'s contents at its
    /// offset. Within one filesystem the kernel copies the file range
    /// itself (a reflink where the filesystem shares blocks); across
    /// filesystems, or on one without the operation, the bytes are written
    /// from `bytes`. The output holds the same bytes either way.
    pub fn copyHost(self: *LinuxOutputFile, _: Allocator, source: HostSource, bytes: []const u8) Error!void {
        var in_offset: i64 = @intCast(source.offset);
        var out_offset: i64 = 0;
        var remaining = bytes.len;
        while (remaining != 0) {
            const rc = linux.copy_file_range(source.handle, &in_offset, self.fd, &out_offset, @min(remaining, 1 << 30), 0);
            const errno = linux.errno(rc);
            if (errno == .INTR) continue;
            if (errno == .XDEV or errno == .OPNOTSUPP or errno == .INVAL) {
                const done: usize = @intCast(out_offset);
                return self.writeAt(bytes[done..], done);
            }
            if (errno != .SUCCESS) return failure(errno);
            if (rc == 0) return error.Unexpected;
            remaining -= rc;
        }
        self.written = @max(self.written, bytes.len);
    }

    /// Write `bytes` at the start of the output.
    pub fn writeStart(self: *LinuxOutputFile, bytes: []const u8) Error!void {
        return self.writeAt(bytes, 0);
    }

    fn writeAt(self: *LinuxOutputFile, bytes: []const u8, at: usize) Error!void {
        var written: usize = 0;
        while (written < bytes.len) {
            const rc = linux.pwrite(self.fd, bytes.ptr + written, bytes.len - written, @intCast(at + written));
            const errno = linux.errno(rc);
            if (errno == .INTR) continue;
            if (errno != .SUCCESS) return failure(errno);
            written += rc;
        }
        self.written = @max(self.written, at + bytes.len);
    }

    /// Set the output's size; bytes it grows by read as zero.
    pub fn resize(self: *LinuxOutputFile, size: u64) Error!void {
        const rc = linux.ftruncate(self.fd, @intCast(size));
        if (linux.errno(rc) != .SUCCESS) return failure(linux.errno(rc));
    }

    /// Map the output's first `size` bytes for writing, with the blocks
    /// behind them allocated first, so a filesystem that runs out of room
    /// fails here instead of faulting a write into the mapping. On a
    /// filesystem without block allocation the bytes go to a buffer that
    /// `finish` writes out, which reports the same failure.
    pub fn map(self: *LinuxOutputFile, size: u64) Error![]u8 {
        const allocated = linux.fallocate(self.fd, 0, 0, @intCast(size));
        const allocated_errno = linux.errno(allocated);
        if (allocated_errno != .SUCCESS and allocated_errno != .OPNOTSUPP) return failure(allocated_errno);
        self.buffered = allocated_errno == .OPNOTSUPP;
        const rc = if (self.buffered)
            linux.mmap(null, @intCast(size), .{ .READ = true, .WRITE = true }, .{ .TYPE = .PRIVATE, .ANONYMOUS = true }, -1, 0)
        else
            linux.mmap(null, @intCast(size), .{ .READ = true, .WRITE = true }, .{ .TYPE = .SHARED }, self.fd, 0);
        if (linux.errno(rc) != .SUCCESS) return failure(linux.errno(rc));
        const start: [*]u8 = @ptrFromInt(rc);
        const bytes = start[0..@intCast(size)];
        if (self.buffered) {
            // The bytes already in the file belong in the buffer too.
            var got: usize = 0;
            while (got < self.written) {
                const read = linux.pread(self.fd, start + got, @intCast(self.written - got), @intCast(got));
                const read_errno = linux.errno(read);
                if (read_errno == .INTR) continue;
                if (read_errno != .SUCCESS) return failure(read_errno);
                if (read == 0) return error.Unexpected;
                got += read;
            }
        }
        return bytes;
    }

    /// Complete the bytes of a mapping `map` made and release it.
    pub fn finish(self: *LinuxOutputFile, bytes: []u8) Error!void {
        defer self.unmap(bytes);
        if (self.buffered) try self.writeAt(bytes, 0);
    }

    /// Release a mapping `map` made without completing the output.
    pub fn unmap(_: *LinuxOutputFile, bytes: []u8) void {
        _ = linux.munmap(bytes.ptr, bytes.len);
    }

    /// Give the complete output its destination name.
    pub fn publish(self: *LinuxOutputFile, arena: Allocator) Error!void {
        if (!self.named) {
            const proc_path = try std.fmt.allocPrintSentinel(arena, "/proc/self/fd/{d}", .{self.fd}, 0);
            while (true) {
                const rc = linux.linkat(linux.AT.FDCWD, proc_path, linux.AT.FDCWD, self.temp, linux.AT.SYMLINK_FOLLOW);
                const errno = linux.errno(rc);
                if (errno == .SUCCESS) break;
                if (errno == .INTR) continue;
                if (errno != .EXIST) return failure(errno);
                // A temporary name a killed build left behind.
                const unlinked = linux.unlinkat(linux.AT.FDCWD, self.temp, 0);
                if (linux.errno(unlinked) != .SUCCESS) return failure(linux.errno(unlinked));
            }
            self.named = true;
        }
        const renamed = linux.renameat(linux.AT.FDCWD, self.temp, linux.AT.FDCWD, self.dest);
        if (linux.errno(renamed) != .SUCCESS) return failure(linux.errno(renamed));
        self.named = false;
        _ = linux.close(self.fd);
        self.fd = -1;
    }

    /// Release an output that will not be published.
    pub fn discard(self: *LinuxOutputFile) void {
        if (self.fd < 0) return;
        _ = linux.close(self.fd);
        self.fd = -1;
        if (self.named) _ = linux.unlinkat(linux.AT.FDCWD, self.temp, 0);
    }
};

extern "c" fn clonefile(src: [*:0]const u8, dst: [*:0]const u8, flags: u32) c_int;

/// A macOS or BSD output: a named temporary file beside the destination.
pub const PosixOutputFile = struct {
    fd: c_int,
    /// The destination path, NUL-terminated.
    dest: [:0]const u8,
    /// The temporary name the file has before the rename.
    temp: [:0]const u8,
    size: u64 = 0,

    pub const Error = OutputError;

    /// Create the output for `dest_path`. Fails when the destination
    /// directory cannot hold it.
    pub fn create(arena: Allocator, dest_path: []const u8) Error!PosixOutputFile {
        const dest = try arena.dupeSentinel(u8, dest_path, 0);
        const temp = try tempName(arena, dest_path, std.c.getpid());
        var self: PosixOutputFile = .{ .fd = -1, .dest = dest, .temp = temp };
        try self.open();
        return self;
    }

    fn open(self: *PosixOutputFile) Error!void {
        const fd = std.c.open(self.temp, .{ .ACCMODE = .RDWR, .CREAT = true, .TRUNC = true, .CLOEXEC = true }, @as(std.c.mode_t, 0o777));
        if (fd < 0) return failure(std.posix.errno(fd));
        self.fd = fd;
        self.size = 0;
    }

    /// Begin the output as `bytes`, which are `source`'s contents at its
    /// offset. On macOS, where the host starts the record and the
    /// filesystem shares blocks, the output is a clone of the whole record,
    /// so the caller sizes it; elsewhere the bytes are written. The output
    /// holds the same bytes either way.
    pub fn copyHost(self: *PosixOutputFile, gpa: Allocator, source: HostSource, bytes: []const u8) Error!void {
        if (comptime builtin.os.tag != .macos) return self.writeAt(bytes, 0);
        if (source.offset != 0) return self.writeAt(bytes, 0);
        const source_z = try gpa.dupeSentinel(u8, source.path, 0);
        defer gpa.free(source_z);
        // A clone needs a destination that does not exist yet.
        _ = std.c.close(self.fd);
        self.fd = -1;
        _ = std.c.unlink(self.temp);
        const rc = clonefile(source_z, self.temp, 0);
        if (rc == 0) {
            const fd = std.c.open(self.temp, .{ .ACCMODE = .RDWR, .CLOEXEC = true }, @as(std.c.mode_t, 0));
            if (fd < 0) return failure(std.posix.errno(fd));
            self.fd = fd;
            // A clone keeps the record's mode; the output is an executable
            // created like any other file.
            const mask = std.c.umask(0o022);
            _ = std.c.umask(mask);
            const chmod = std.c.fchmod(fd, @as(std.c.mode_t, 0o777) & ~mask);
            if (chmod != 0) return failure(std.posix.errno(chmod));
            return self.resize(bytes.len);
        }
        const errno = std.posix.errno(rc);
        if (errno != .OPNOTSUPP and errno != .XDEV and errno != .INVAL) return failure(errno);
        try self.open();
        return self.writeAt(bytes, 0);
    }

    /// Write `bytes` at the start of the output.
    pub fn writeStart(self: *PosixOutputFile, bytes: []const u8) Error!void {
        return self.writeAt(bytes, 0);
    }

    fn writeAt(self: *PosixOutputFile, bytes: []const u8, at: usize) Error!void {
        var written: usize = 0;
        while (written < bytes.len) {
            const rc = std.c.pwrite(self.fd, bytes.ptr + written, bytes.len - written, @intCast(at + written));
            if (rc < 0) {
                const errno = std.posix.errno(rc);
                if (errno == .INTR) continue;
                return failure(errno);
            }
            written += @intCast(rc);
        }
        self.size = @max(self.size, at + bytes.len);
    }

    pub fn resize(self: *PosixOutputFile, size: u64) Error!void {
        const rc = std.c.ftruncate(self.fd, @intCast(size));
        if (rc != 0) return failure(std.posix.errno(rc));
        self.size = size;
    }

    pub fn map(self: *PosixOutputFile, size: u64) Error![]u8 {
        const mapped = std.posix.mmap(null, @intCast(size), .{ .READ = true, .WRITE = true }, .{ .TYPE = .SHARED }, self.fd, 0) catch |err| return switch (err) {
            error.OutOfMemory => error.OutOfMemory,
            else => error.Unexpected,
        };
        return mapped;
    }

    /// Complete the bytes of a mapping `map` made, which are the file's
    /// own pages, and release it.
    pub fn finish(self: *PosixOutputFile, bytes: []u8) Error!void {
        self.unmap(bytes);
    }

    /// Release the mapping. The macOS kernel validates an executable's
    /// pages against its signature as it reads them from the file, so the
    /// pages the mapping dirtied are written back and dropped from the
    /// cache first; a page left cached from the write would fail that
    /// validation.
    pub fn unmap(_: *PosixOutputFile, bytes: []u8) void {
        const memory: []align(std.heap.page_size_min) u8 = @alignCast(bytes);
        if (comptime builtin.os.tag == .macos) std.posix.msync(memory, std.c.MSF.SYNC | std.c.MSF.INVALIDATE) catch {};
        std.posix.munmap(memory);
    }

    /// Give the complete output its destination name.
    pub fn publish(self: *PosixOutputFile, _: Allocator) Error!void {
        const rc = std.c.rename(self.temp, self.dest);
        if (rc != 0) return failure(std.posix.errno(rc));
        _ = std.c.close(self.fd);
        self.fd = -1;
    }

    /// Release an output that will not be published.
    pub fn discard(self: *PosixOutputFile) void {
        if (self.fd < 0) return;
        _ = std.c.close(self.fd);
        self.fd = -1;
        _ = std.c.unlink(self.temp);
    }
};

/// The Win32 calls an output on Windows is written with.
const win32 = struct {
    const HANDLE = std.os.windows.HANDLE;
    const DWORD = u32;
    const BOOL = c_int;
    const GENERIC_READ: DWORD = 0x8000_0000;
    const GENERIC_WRITE: DWORD = 0x4000_0000;
    const FILE_SHARE_READ: DWORD = 0x1;
    const FILE_SHARE_DELETE: DWORD = 0x4;
    const CREATE_ALWAYS: DWORD = 2;
    const FILE_ATTRIBUTE_NORMAL: DWORD = 0x80;
    const PAGE_READONLY: DWORD = 0x02;
    const PAGE_READWRITE: DWORD = 0x04;
    const FILE_MAP_WRITE: DWORD = 0x02;
    const FILE_MAP_READ: DWORD = 0x04;
    const FILE_BEGIN: DWORD = 0;
    const MOVEFILE_REPLACE_EXISTING: DWORD = 0x1;
    const ERROR_FILE_NOT_FOUND: DWORD = 2;
    const ERROR_PATH_NOT_FOUND: DWORD = 3;
    const ERROR_ACCESS_DENIED: DWORD = 5;
    const ERROR_SHARING_VIOLATION: DWORD = 32;
    const ERROR_HANDLE_DISK_FULL: DWORD = 39;
    const ERROR_DISK_FULL: DWORD = 112;
    const OVERLAPPED = extern struct { internal: usize = 0, internal_high: usize = 0, offset: DWORD, offset_high: DWORD, event: ?HANDLE = null };

    extern "kernel32" fn CreateFileW(name: [*:0]const u16, access: DWORD, share: DWORD, security: ?*anyopaque, disposition: DWORD, flags: DWORD, template: ?HANDLE) callconv(.winapi) HANDLE;
    extern "kernel32" fn WriteFile(file: HANDLE, buffer: [*]const u8, count: DWORD, written: ?*DWORD, overlapped: ?*OVERLAPPED) callconv(.winapi) BOOL;
    extern "kernel32" fn SetFilePointerEx(file: HANDLE, distance: i64, new_position: ?*i64, method: DWORD) callconv(.winapi) BOOL;
    extern "kernel32" fn SetEndOfFile(file: HANDLE) callconv(.winapi) BOOL;
    extern "kernel32" fn CloseHandle(handle: HANDLE) callconv(.winapi) BOOL;
    extern "kernel32" fn DeleteFileW(name: [*:0]const u16) callconv(.winapi) BOOL;
    extern "kernel32" fn MoveFileExW(from: [*:0]const u16, to: [*:0]const u16, flags: DWORD) callconv(.winapi) BOOL;
    extern "kernel32" fn CreateFileMappingW(file: HANDLE, attributes: ?*anyopaque, protect: DWORD, size_high: DWORD, size_low: DWORD, name: ?[*:0]const u16) callconv(.winapi) ?HANDLE;
    extern "kernel32" fn MapViewOfFile(mapping: HANDLE, access: DWORD, offset_high: DWORD, offset_low: DWORD, size: usize) callconv(.winapi) ?[*]u8;
    extern "kernel32" fn UnmapViewOfFile(address: *const anyopaque) callconv(.winapi) BOOL;
    extern "kernel32" fn FlushViewOfFile(address: *const anyopaque, size: usize) callconv(.winapi) BOOL;
    extern "kernel32" fn GetLastError() callconv(.winapi) DWORD;
    extern "kernel32" fn GetCurrentProcessId() callconv(.winapi) DWORD;
    extern "kernel32" fn Sleep(milliseconds: DWORD) callconv(.winapi) void;

    fn failure(code: DWORD) OutputError {
        if (code == ERROR_ACCESS_DENIED or code == ERROR_SHARING_VIOLATION) return error.AccessDenied;
        if (code == ERROR_DISK_FULL or code == ERROR_HANDLE_DISK_FULL) return error.NoSpaceLeft;
        if (code == ERROR_FILE_NOT_FOUND or code == ERROR_PATH_NOT_FOUND) return error.FileNotFound;
        return error.Unexpected;
    }
};

/// How often a Windows call that a filter driver can transiently deny is
/// retried, with the delay doubling from 10 ms.
const windows_attempts: u32 = 6;

/// A Windows output: a named temporary file beside the destination. Filter
/// drivers (Defender, EDR agents) can transiently hold a just-created file
/// open and deny access to it, so creating and renaming retry with a
/// backoff.
pub const WindowsOutputFile = struct {
    handle: ?win32.HANDLE,
    /// The destination path.
    dest: [:0]const u8,
    dest_w: [:0]const u16,
    /// The temporary name the file has before the rename.
    temp_w: [:0]const u16,
    mapping: ?win32.HANDLE = null,
    size: u64 = 0,

    pub const Error = OutputError;

    /// Create the output for `dest_path`. Fails when the destination
    /// directory cannot hold it.
    pub fn create(arena: Allocator, dest_path: []const u8) Error!WindowsOutputFile {
        const dest = try arena.dupeSentinel(u8, dest_path, 0);
        const temp = try tempName(arena, dest_path, win32.GetCurrentProcessId());
        const dest_w = std.unicode.wtf8ToWtf16LeAllocZ(arena, dest) catch |err| return switch (err) {
            error.OutOfMemory => error.OutOfMemory,
            error.InvalidWtf8 => error.FileNotFound,
        };
        const temp_w = std.unicode.wtf8ToWtf16LeAllocZ(arena, temp) catch |err| return switch (err) {
            error.OutOfMemory => error.OutOfMemory,
            error.InvalidWtf8 => error.FileNotFound,
        };
        var attempt: u32 = 0;
        while (true) : (attempt += 1) {
            const handle = win32.CreateFileW(temp_w, win32.GENERIC_READ | win32.GENERIC_WRITE, win32.FILE_SHARE_READ | win32.FILE_SHARE_DELETE, null, win32.CREATE_ALWAYS, win32.FILE_ATTRIBUTE_NORMAL, null);
            if (handle != std.os.windows.INVALID_HANDLE_VALUE) return .{ .handle = handle, .dest = dest, .dest_w = dest_w, .temp_w = temp_w };
            const err = win32.failure(win32.GetLastError());
            switch (err) {
                error.AccessDenied => if (attempt + 1 >= windows_attempts) return err,
                error.NoSpaceLeft, error.FileNotFound, error.Unexpected, error.OutOfMemory => return err,
            }
            win32.Sleep(@as(u32, 10) << @intCast(attempt));
        }
    }

    /// Begin the output as `bytes`, the host's bytes of `source`.
    pub fn copyHost(self: *WindowsOutputFile, _: Allocator, _: HostSource, bytes: []const u8) Error!void {
        return self.writeAt(bytes, 0);
    }

    /// Write `bytes` at the start of the output.
    pub fn writeStart(self: *WindowsOutputFile, bytes: []const u8) Error!void {
        return self.writeAt(bytes, 0);
    }

    fn writeAt(self: *WindowsOutputFile, bytes: []const u8, at: usize) Error!void {
        var written: usize = 0;
        while (written < bytes.len) {
            const position: u64 = at + written;
            var overlapped: win32.OVERLAPPED = .{ .offset = @truncate(position), .offset_high = @truncate(position >> 32) };
            var count: win32.DWORD = 0;
            const chunk: win32.DWORD = @intCast(@min(bytes.len - written, 1 << 30));
            if (win32.WriteFile(self.handle.?, bytes.ptr + written, chunk, &count, &overlapped) == 0) return win32.failure(win32.GetLastError());
            if (count == 0) return error.Unexpected;
            written += count;
        }
        self.size = @max(self.size, at + bytes.len);
    }

    pub fn resize(self: *WindowsOutputFile, size: u64) Error!void {
        if (win32.SetFilePointerEx(self.handle.?, @intCast(size), null, win32.FILE_BEGIN) == 0) return win32.failure(win32.GetLastError());
        if (win32.SetEndOfFile(self.handle.?) == 0) return win32.failure(win32.GetLastError());
        self.size = size;
    }

    pub fn map(self: *WindowsOutputFile, size: u64) Error![]u8 {
        const mapping = win32.CreateFileMappingW(self.handle.?, null, win32.PAGE_READWRITE, @truncate(size >> 32), @truncate(size), null) orelse return win32.failure(win32.GetLastError());
        const view = win32.MapViewOfFile(mapping, win32.FILE_MAP_WRITE, 0, 0, @intCast(size)) orelse {
            const err = win32.failure(win32.GetLastError());
            _ = win32.CloseHandle(mapping);
            return err;
        };
        self.mapping = mapping;
        return view[0..@intCast(size)];
    }

    /// Complete the bytes of a mapping `map` made, which are the file's
    /// own pages, and release it.
    pub fn finish(self: *WindowsOutputFile, bytes: []u8) Error!void {
        self.unmap(bytes);
    }

    pub fn unmap(self: *WindowsOutputFile, bytes: []u8) void {
        _ = win32.FlushViewOfFile(bytes.ptr, bytes.len);
        _ = win32.UnmapViewOfFile(bytes.ptr);
        if (self.mapping) |mapping| _ = win32.CloseHandle(mapping);
        self.mapping = null;
    }

    /// Give the complete output its destination name.
    pub fn publish(self: *WindowsOutputFile, _: Allocator) Error!void {
        if (self.handle) |handle| _ = win32.CloseHandle(handle);
        self.handle = null;
        var attempt: u32 = 0;
        while (true) : (attempt += 1) {
            if (win32.MoveFileExW(self.temp_w, self.dest_w, win32.MOVEFILE_REPLACE_EXISTING) != 0) return;
            const err = win32.failure(win32.GetLastError());
            switch (err) {
                error.AccessDenied => if (attempt + 1 >= windows_attempts) return err,
                error.NoSpaceLeft, error.FileNotFound, error.Unexpected, error.OutOfMemory => return err,
            }
            win32.Sleep(@as(u32, 10) << @intCast(attempt));
        }
    }

    /// Release an output that will not be published.
    pub fn discard(self: *WindowsOutputFile) void {
        const handle = self.handle orelse return;
        _ = win32.CloseHandle(handle);
        self.handle = null;
        _ = win32.DeleteFileW(self.temp_w);
    }
};

/// Map `size` bytes of an open file read-only, for a cached record.
pub fn mapReadOnly(handle: std.Io.File.Handle, size: usize) error{MapFailed}![]align(std.heap.page_size_min) u8 {
    if (comptime builtin.os.tag == .windows) {
        const mapping = win32.CreateFileMappingW(handle, null, win32.PAGE_READONLY, 0, 0, null) orelse return error.MapFailed;
        // The view keeps the mapping object alive.
        defer _ = win32.CloseHandle(mapping);
        const view = win32.MapViewOfFile(mapping, win32.FILE_MAP_READ, 0, 0, size) orelse return error.MapFailed;
        return @alignCast(view[0..size]);
    }
    return std.posix.mmap(null, size, .{ .READ = true }, .{ .TYPE = .PRIVATE }, handle, 0) catch error.MapFailed;
}

/// Release a mapping `mapReadOnly` made.
pub fn unmapReadOnly(mapping: []align(std.heap.page_size_min) const u8) void {
    if (comptime builtin.os.tag == .windows) {
        _ = win32.UnmapViewOfFile(mapping.ptr);
        return;
    }
    std.posix.munmap(mapping);
}

test {
    std.testing.refAllDecls(OutputFile);
}

test "the temporary name sits beside the destination" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();
    const sep = std.fs.path.sep_str;
    try std.testing.expectEqualStrings("out" ++ sep ++ ".app.7.roc-link", try tempName(arena, "out" ++ sep ++ "app", 7));
    try std.testing.expectEqualStrings(".app.7.roc-link", try tempName(arena, "app", 7));
}
