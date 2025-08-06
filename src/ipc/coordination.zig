//! Process coordination utilities for shared memory
//! Handles communication of shared memory info between parent and child processes

const std = @import("std");
const builtin = @import("builtin");
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

/// Read shared memory coordination info from platform-specific source
/// On Windows: reads from command line arguments
/// On POSIX: reads from a file next to the executable
pub fn readFdInfo(allocator: std.mem.Allocator) CoordinationError!FdInfo {
    if (comptime platform.is_windows) {
        return readFdInfoFromCommandLine(allocator);
    } else {
        return readFdInfoFromFile(allocator);
    }
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

/// Windows: Read handle and size from command line arguments
fn readFdInfoFromCommandLine(allocator: std.mem.Allocator) CoordinationError!FdInfo {
    const args = std.process.argsAlloc(allocator) catch {
        std.log.err("Failed to allocate memory for command line arguments", .{});
        return error.AllocationFailed;
    };
    defer std.process.argsFree(allocator, args);

    if (args.len < 3) {
        std.log.err("Invalid command line arguments: expected at least 3 arguments, got {}", .{args.len});
        return error.ArgumentsInvalid;
    }

    const handle_str = args[1];
    const size_str = args[2];

    const fd_str = allocator.dupe(u8, handle_str) catch {
        std.log.err("Failed to duplicate handle string", .{});
        return error.AllocationFailed;
    };

    const size = std.fmt.parseInt(usize, size_str, 10) catch {
        std.log.err("Failed to parse size from '{s}'", .{size_str});
        allocator.free(fd_str);
        return error.ArgumentsInvalid;
    };

    return FdInfo{
        .fd_str = fd_str,
        .size = size,
    };
}

/// POSIX: Read fd and size from temporary file
fn readFdInfoFromFile(allocator: std.mem.Allocator) CoordinationError!FdInfo {
    // Get our own executable path
    const exe_path = std.fs.selfExePathAlloc(allocator) catch {
        std.log.err("Failed to get executable path", .{});
        return error.FdInfoReadFailed;
    };
    defer allocator.free(exe_path);

    // Get the directory containing our executable (should be "roc-tmp-<random>")
    const exe_dir = std.fs.path.dirname(exe_path) orelse {
        std.log.err("Invalid executable path: no directory component", .{});
        return error.FdInfoReadFailed;
    };
    const dir_basename = std.fs.path.basename(exe_dir);

    // Verify it has the expected prefix
    if (!std.mem.startsWith(u8, dir_basename, "roc-tmp-")) {
        std.log.err("Unexpected directory name: expected 'roc-tmp-*', got '{s}'", .{dir_basename});
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
    const file = std.fs.cwd().openFile(fd_file_path, .{}) catch {
        std.log.err("Failed to open fd file at '{s}'", .{fd_file_path});
        return error.FileNotFound;
    };
    defer file.close();

    var buffer: [128]u8 = undefined;
    const bytes_read = file.readAll(&buffer) catch {
        std.log.err("Failed to read fd file", .{});
        return error.FileReadFailed;
    };

    const content = buffer[0..bytes_read];

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

/// Write shared memory coordination info for child process
/// On Windows: returns command line arguments to pass
/// On POSIX: writes a file next to the target executable
pub fn writeFdInfo(
    allocator: std.mem.Allocator,
    handle: platform.Handle,
    size: usize,
    target_path: []const u8,
) ![]const u8 {
    if (comptime platform.is_windows) {
        // On Windows, return command line arguments
        const handle_int = @intFromPtr(handle);
        return std.fmt.allocPrint(allocator, "{} {}", .{ handle_int, size });
    } else {
        // On POSIX, write a coordination file
        const fd = @as(c_int, @intCast(handle));
        
        // Get the directory of the target executable
        const target_dir = std.fs.path.dirname(target_path) orelse {
            return error.InvalidTargetPath;
        };
        
        // Create the coordination file path
        const coord_file_path = std.fmt.allocPrint(allocator, "{s}.txt", .{target_dir}) catch {
            return error.OutOfMemory;
        };
        defer allocator.free(coord_file_path);
        
        // Write the coordination file
        const file = std.fs.cwd().createFile(coord_file_path, .{}) catch |err| {
            std.log.err("Failed to create coordination file at '{s}': {}", .{ coord_file_path, err });
            return err;
        };
        defer file.close();
        
        const content = std.fmt.allocPrint(allocator, "{}\n{}\n", .{ fd, size }) catch {
            return error.OutOfMemory;
        };
        defer allocator.free(content);
        
        file.writeAll(content) catch |err| {
            std.log.err("Failed to write coordination file: {}", .{err});
            return err;
        };
        
        // Return empty string for POSIX (no command line args needed)
        return "";
    }
}