//! The output executable of a fast dev link, created before the build's
//! compile work and given its name only when it is complete (plan "Output
//! file lifecycle"). The bytes written are the same however the file comes
//! to exist: on Linux as an anonymous file in the destination directory
//! when the filesystem has them, else a named temporary file beside the
//! destination; on macOS a named temporary file, which may begin as a
//! clone of the cached host record where the filesystem shares blocks.
const std = @import("std");
const builtin = @import("builtin");

const Allocator = std.mem.Allocator;
const linux = std.os.linux;

/// The output executable of a fast dev link, written through the compiler
/// host's own system calls.
pub const OutputFile = if (builtin.os.tag == .macos) MacOutputFile else LinuxOutputFile;

/// The ways creating, writing or publishing an `OutputFile` can fail.
pub const OutputError = Allocator.Error || error{ AccessDenied, NoSpaceLeft, FileNotFound, Unexpected };

fn failure(errno: anytype) OutputError {
    if (errno == .ACCES or errno == .PERM or errno == .ROFS) return error.AccessDenied;
    if (errno == .NOSPC or errno == .DQUOT) return error.NoSpaceLeft;
    if (errno == .NOENT or errno == .NOTDIR) return error.FileNotFound;
    return error.Unexpected;
}

/// The temporary name beside `dest_path` an output is written under.
fn tempName(arena: Allocator, dest_path: []const u8, pid: anytype) Allocator.Error![:0]const u8 {
    const dir = std.fs.path.dirname(dest_path);
    return std.fmt.allocPrintSentinel(arena, "{s}{s}.{s}.{d}.roc-link", .{ if (dir == null) "" else dir.?, if (dir == null) "" else "/", std.fs.path.basename(dest_path), pid }, 0);
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
    size: u64 = 0,

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

    /// Copy `bytes`, which are `source`'s contents at `offset`, to the start
    /// of the output. Within one filesystem the kernel copies the file
    /// range itself (a reflink where the filesystem shares blocks); across
    /// filesystems, or on one without the operation, the bytes are written
    /// from `bytes`. The output holds the same bytes either way.
    pub fn copyFrom(self: *LinuxOutputFile, source: linux.fd_t, offset: u64, bytes: []const u8) Error!void {
        var in_offset: i64 = @intCast(offset);
        var out_offset: i64 = 0;
        var remaining = bytes.len;
        while (remaining != 0) {
            const rc = linux.copy_file_range(source, &in_offset, self.fd, &out_offset, @min(remaining, 1 << 30), 0);
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
        self.size = @max(self.size, bytes.len);
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
        self.size = @max(self.size, at + bytes.len);
    }

    pub fn resize(self: *LinuxOutputFile, size: u64) Error!void {
        const rc = linux.ftruncate(self.fd, @intCast(size));
        if (linux.errno(rc) != .SUCCESS) return failure(linux.errno(rc));
        self.size = size;
    }

    pub fn map(self: *LinuxOutputFile, size: u64) Error![]u8 {
        const rc = linux.mmap(null, @intCast(size), .{ .READ = true, .WRITE = true }, .{ .TYPE = .SHARED }, self.fd, 0);
        if (linux.errno(rc) != .SUCCESS) return failure(linux.errno(rc));
        const start: [*]u8 = @ptrFromInt(rc);
        return start[0..@intCast(size)];
    }

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

/// A macOS output: a named temporary file beside the destination.
pub const MacOutputFile = struct {
    fd: c_int,
    /// The destination path, NUL-terminated.
    dest: [:0]const u8,
    /// The temporary name the file has before the rename.
    temp: [:0]const u8,
    size: u64 = 0,

    pub const Error = OutputError;

    /// Create the output for `dest_path`. Fails when the destination
    /// directory cannot hold it.
    pub fn create(arena: Allocator, dest_path: []const u8) Error!MacOutputFile {
        const dest = try arena.dupeSentinel(u8, dest_path, 0);
        const temp = try tempName(arena, dest_path, std.c.getpid());
        var self: MacOutputFile = .{ .fd = -1, .dest = dest, .temp = temp };
        try self.open();
        return self;
    }

    fn open(self: *MacOutputFile) Error!void {
        const fd = std.c.open(self.temp, .{ .ACCMODE = .RDWR, .CREAT = true, .TRUNC = true, .CLOEXEC = true }, @as(std.c.mode_t, 0o777));
        if (fd < 0) return failure(std.posix.errno(fd));
        self.fd = fd;
        self.size = 0;
    }

    /// Begin the output as `bytes`, which are the start of the file at
    /// `source_path`. Where the filesystem shares blocks the output is a
    /// clone of the whole source, so the caller sizes it; elsewhere the
    /// bytes are written. The output holds the same bytes either way.
    pub fn cloneOrWrite(self: *MacOutputFile, gpa: Allocator, source_path: []const u8, bytes: []const u8) Error!void {
        const source = try gpa.dupeSentinel(u8, source_path, 0);
        defer gpa.free(source);
        // A clone needs a destination that does not exist yet.
        _ = std.c.close(self.fd);
        self.fd = -1;
        _ = std.c.unlink(self.temp);
        const rc = clonefile(source, self.temp, 0);
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
    pub fn writeStart(self: *MacOutputFile, bytes: []const u8) Error!void {
        return self.writeAt(bytes, 0);
    }

    fn writeAt(self: *MacOutputFile, bytes: []const u8, at: usize) Error!void {
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

    pub fn resize(self: *MacOutputFile, size: u64) Error!void {
        const rc = std.c.ftruncate(self.fd, @intCast(size));
        if (rc != 0) return failure(std.posix.errno(rc));
        self.size = size;
    }

    pub fn map(self: *MacOutputFile, size: u64) Error![]u8 {
        const mapped = std.posix.mmap(null, @intCast(size), .{ .READ = true, .WRITE = true }, .{ .TYPE = .SHARED }, self.fd, 0) catch |err| return switch (err) {
            error.OutOfMemory => error.OutOfMemory,
            else => error.Unexpected,
        };
        return mapped;
    }

    /// Release the mapping. The kernel validates an executable's pages
    /// against its signature as it reads them from the file, so the pages
    /// the mapping dirtied are written back and dropped from the cache
    /// first; a page left cached from the write would fail that
    /// validation.
    pub fn unmap(_: *MacOutputFile, bytes: []u8) void {
        const memory: []align(std.heap.page_size_min) u8 = @alignCast(bytes);
        std.posix.msync(memory, std.c.MSF.SYNC | std.c.MSF.INVALIDATE) catch {};
        std.posix.munmap(memory);
    }

    /// Give the complete output its destination name.
    pub fn publish(self: *MacOutputFile, _: Allocator) Error!void {
        const rc = std.c.rename(self.temp, self.dest);
        if (rc != 0) return failure(std.posix.errno(rc));
        _ = std.c.close(self.fd);
        self.fd = -1;
    }

    /// Release an output that will not be published.
    pub fn discard(self: *MacOutputFile) void {
        if (self.fd < 0) return;
        _ = std.c.close(self.fd);
        self.fd = -1;
        _ = std.c.unlink(self.temp);
    }
};
