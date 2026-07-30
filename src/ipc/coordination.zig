//! Process coordination utilities for shared memory
//! Handles communication of shared memory info between parent and child processes

const std = @import("std");
const Allocator = std.mem.Allocator;
const platform = @import("platform.zig");

/// Information about shared memory file descriptor or handle
pub const FdInfo = struct {
    fd_str: []u8,
    size: usize,

    pub fn deinit(self: *FdInfo, allocator: std.mem.Allocator) void {
        allocator.free(self.fd_str);
    }
};

/// Errors that can occur during coordination
pub const CoordinationError = error{
    FdInfoReadFailed,
    HandleParsingFailed,
    ArgumentsInvalid,
    FileNotFound,
    FileReadFailed,
    AllocationFailed,
};

/// Read shared memory coordination info, written by the parent `roc` process
/// as a file next to the executable's temp directory.
///
/// Windows used to receive this on the command line instead. That leaked the
/// handle and size into the child's argv, where a platform host cannot
/// distinguish them from the user's arguments and hands them to the Roc
/// application -- so `roc --opt=interpreter app.roc` gave the app three args
/// rather than one. Both platforms now use the same out-of-band file.
pub fn readFdInfo(allocator: std.mem.Allocator, io: std.Io) CoordinationError!FdInfo {
    return readFdInfoFromFile(allocator, io);
}

/// Parse platform-specific handle from string
pub fn parseHandle(handle_str: []const u8) CoordinationError!platform.Handle {
    if (comptime platform.is_windows) {
        const handle_uint = std.fmt.parseInt(usize, handle_str, 10) catch {
            return error.HandleParsingFailed;
        };
        return @as(platform.Handle, @ptrFromInt(handle_uint));
    } else {
        const fd = std.fmt.parseInt(c_int, handle_str, 10) catch {
            return error.HandleParsingFailed;
        };
        return fd;
    }
}

/// Read the fd/handle and size from the coordination file the parent wrote
/// next to the executable's temp directory. Used on every platform.
fn readFdInfoFromFile(allocator: std.mem.Allocator, io: std.Io) CoordinationError!FdInfo {
    // Get our own executable path
    const exe_path = std.process.executablePathAlloc(io, allocator) catch |err| switch (err) {
        error.OutOfMemory => return error.AllocationFailed,
        else => {
            std.log.err("Failed to get executable path", .{});
            return error.FdInfoReadFailed;
        },
    };
    defer allocator.free(exe_path);

    // Get the directory containing our executable (should be "{temp}/roc/{version}/{random}")
    const exe_dir = std.fs.path.dirname(exe_path) orelse {
        std.log.err("Invalid executable path: no directory component", .{});
        return error.FdInfoReadFailed;
    };

    // Verify we're in a roc temp directory structure: {temp}/roc/{version}/{random}
    // The grandparent of the exe directory should be "roc"
    const version_dir = std.fs.path.dirname(exe_dir) orelse {
        std.log.err("Invalid executable path: missing version directory component", .{});
        return error.FdInfoReadFailed;
    };
    const roc_dir = std.fs.path.dirname(version_dir) orelse {
        std.log.err("Invalid executable path: missing roc directory component", .{});
        return error.FdInfoReadFailed;
    };
    const roc_basename = std.fs.path.basename(roc_dir);

    if (!std.mem.eql(u8, roc_basename, "roc")) {
        std.log.err("Unexpected directory structure: expected 'roc' grandparent, got '{s}'", .{roc_basename});
        return error.FdInfoReadFailed;
    }

    // Construct the fd file path by appending .txt to the directory path
    var dir_path = exe_dir;
    while (dir_path.len > 0 and (dir_path[dir_path.len - 1] == '/' or dir_path[dir_path.len - 1] == '\\')) {
        dir_path = dir_path[0 .. dir_path.len - 1];
    }

    const fd_file_path = std.fmt.allocPrint(allocator, "{s}.txt", .{dir_path}) catch {
        std.log.err("Failed to format fd file path", .{});
        return error.AllocationFailed;
    };
    defer allocator.free(fd_file_path);

    // Read the file
    const content = std.Io.Dir.cwd().readFileAlloc(io, fd_file_path, allocator, .limited(128)) catch |err| switch (err) {
        error.OutOfMemory => return error.AllocationFailed,
        else => {
            std.log.err("Failed to read fd file at '{s}'", .{fd_file_path});
            return error.FileReadFailed;
        },
    };
    defer allocator.free(content);

    // Parse the content: first line is fd, second line is size
    var lines = std.mem.tokenizeScalar(u8, content, '\n');
    const fd_line = lines.next() orelse {
        std.log.err("Invalid fd file format: missing fd line", .{});
        return error.FdInfoReadFailed;
    };
    const size_line = lines.next() orelse {
        std.log.err("Invalid fd file format: missing size line", .{});
        return error.FdInfoReadFailed;
    };

    const fd_str = allocator.dupe(u8, std.mem.trim(u8, fd_line, " \r\t")) catch {
        std.log.err("Failed to duplicate fd string", .{});
        return error.AllocationFailed;
    };

    const size = std.fmt.parseInt(usize, std.mem.trim(u8, size_line, " \r\t"), 10) catch {
        std.log.err("Failed to parse size from '{s}'", .{size_line});
        allocator.free(fd_str);
        return error.FdInfoReadFailed;
    };

    return FdInfo{
        .fd_str = fd_str,
        .size = size,
    };
}
