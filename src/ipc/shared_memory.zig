//! Cross-platform shared memory abstraction for IPC
//! Handles Windows vs POSIX differences in shared memory management

const std = @import("std");
const builtin = @import("builtin");

const is_windows = builtin.target.os.tag == .windows;

// Platform-specific imports
const posix = if (!is_windows) struct {
    extern "c" fn mmap(addr: ?*anyopaque, len: usize, prot: c_int, flags: c_int, fd: c_int, offset: std.c.off_t) ?*anyopaque;
    extern "c" fn munmap(addr: *anyopaque, len: usize) c_int;
    extern "c" fn close(fd: c_int) c_int;
} else struct {};

const windows = if (is_windows) struct {
    const HANDLE = *anyopaque;
    const DWORD = u32;
    const BOOL = c_int;
    const LPVOID = ?*anyopaque;
    const LPCWSTR = [*:0]const u16;
    const SIZE_T = usize;

    extern "kernel32" fn MapViewOfFileEx(hFileMappingObject: HANDLE, dwDesiredAccess: DWORD, dwFileOffsetHigh: DWORD, dwFileOffsetLow: DWORD, dwNumberOfBytesToMap: SIZE_T, lpBaseAddress: LPVOID) LPVOID;
    extern "kernel32" fn UnmapViewOfFile(lpBaseAddress: LPVOID) BOOL;
    extern "kernel32" fn CloseHandle(hObject: HANDLE) BOOL;
    extern "kernel32" fn GetLastError() DWORD;

    const FILE_MAP_READ = 0x0004;
    const FILE_MAP_WRITE = 0x0002;

    // Fixed base address for shared memory mapping to avoid ASLR issues
    // Must match the address used in main.zig
    const SHARED_MEMORY_BASE_ADDR = @as(?*anyopaque, @ptrFromInt(0x10000000));
} else struct {};

/// Cross-platform shared memory error types
pub const SharedMemoryError = error{
    FdInfoReadFailed,
    SharedMemoryMappingFailed,
    InvalidMemoryLayout,
    RelocationFailed,
    HandleParsingFailed,
    ArgumentsInvalid,
    FileNotFound,
    FileReadFailed,
    AllocationFailed,
};

/// Information about shared memory file descriptor or handle
pub const FdInfo = struct {
    fd_str: []u8,
    size: usize,

    pub fn deinit(self: *FdInfo, allocator: std.mem.Allocator) void {
        allocator.free(self.fd_str);
    }
};

/// Cross-platform shared memory handle
pub const SharedMemoryHandle = struct {
    platform_handle: PlatformHandle,
    ptr: *anyopaque,
    size: usize,

    const PlatformHandle = if (is_windows) windows.HANDLE else c_int;

    /// Read shared memory coordination info from platform-specific source
    pub fn readFdInfo(allocator: std.mem.Allocator) SharedMemoryError!FdInfo {
        if (comptime is_windows) {
            return readFdInfoFromCommandLine(allocator);
        } else {
            return readFdInfoFromFile(allocator);
        }
    }

    /// Map shared memory using platform-specific mechanisms
    pub fn map(handle: PlatformHandle, size: usize) SharedMemoryError!*anyopaque {
        if (comptime is_windows) {
            return mapWindowsMemory(handle, size);
        } else {
            return mapPosixMemory(handle, size);
        }
    }

    /// Unmap shared memory
    pub fn unmap(self: SharedMemoryHandle) void {
        if (comptime is_windows) {
            _ = windows.UnmapViewOfFile(self.ptr);
        } else {
            _ = posix.munmap(self.ptr, self.size);
        }
    }

    /// Close platform-specific handle (POSIX only - Windows handles belong to parent)
    pub fn closeHandle(handle: PlatformHandle) void {
        if (comptime !is_windows) {
            _ = posix.close(handle);
        }
        // Windows: Don't close inherited handles - they belong to the parent process
    }

    /// Create a SharedMemoryHandle from file descriptor info
    pub fn fromFdInfo(fd_info: FdInfo) SharedMemoryError!SharedMemoryHandle {
        const handle = parseHandle(fd_info.fd_str) catch |err| {
            std.log.err("Failed to parse handle from '{s}'", .{fd_info.fd_str});
            return err;
        };

        const mapped_ptr = map(handle, fd_info.size) catch |err| {
            if (comptime !is_windows) {
                closeHandle(handle);
            }
            return err;
        };

        return SharedMemoryHandle{
            .platform_handle = handle,
            .ptr = mapped_ptr,
            .size = fd_info.size,
        };
    }

    /// Parse platform-specific handle from string
    fn parseHandle(handle_str: []const u8) SharedMemoryError!PlatformHandle {
        if (comptime is_windows) {
            const handle_uint = std.fmt.parseInt(usize, handle_str, 10) catch {
                return error.HandleParsingFailed;
            };
            return @as(windows.HANDLE, @ptrFromInt(handle_uint));
        } else {
            const fd = std.fmt.parseInt(c_int, handle_str, 10) catch {
                return error.HandleParsingFailed;
            };
            return fd;
        }
    }

    /// Map Windows shared memory
    fn mapWindowsMemory(handle: windows.HANDLE, size: usize) SharedMemoryError!*anyopaque {
        const mapped_ptr = windows.MapViewOfFileEx(
            handle,
            windows.FILE_MAP_READ | windows.FILE_MAP_WRITE,
            0,
            0,
            size,
            windows.SHARED_MEMORY_BASE_ADDR,
        ) orelse {
            const error_code = windows.GetLastError();
            std.log.err("Windows: Failed to map shared memory view (size: {}, error: {})", .{ size, error_code });
            return error.SharedMemoryMappingFailed;
        };
        return mapped_ptr;
    }

    /// Map POSIX shared memory
    fn mapPosixMemory(fd: c_int, size: usize) SharedMemoryError!*anyopaque {
        const mapped_ptr = posix.mmap(
            null,
            size,
            0x01 | 0x02, // PROT_READ | PROT_WRITE
            0x0001, // MAP_SHARED
            fd,
            0,
        ) orelse {
            std.log.err("POSIX: Failed to map shared memory (size: {})", .{size});
            return error.SharedMemoryMappingFailed;
        };
        return mapped_ptr;
    }
};

/// Windows: Read handle and size from command line arguments
fn readFdInfoFromCommandLine(allocator: std.mem.Allocator) SharedMemoryError!FdInfo {
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
fn readFdInfoFromFile(allocator: std.mem.Allocator) SharedMemoryError!FdInfo {
    // Get our own executable path
    const exe_path = std.fs.selfExePathAlloc(allocator) catch {
        std.log.err("Failed to get executable path");
        return error.FdInfoReadFailed;
    };
    defer allocator.free(exe_path);

    // Get the directory containing our executable (should be "roc-tmp-<random>")
    const exe_dir = std.fs.path.dirname(exe_path) orelse {
        std.log.err("Invalid executable path: no directory component");
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
        std.log.err("Failed to format fd file path");
        return error.AllocationFailed;
    };
    defer allocator.free(fd_file_path);

    // Read the file
    const file = std.fs.cwd().openFile(fd_file_path, .{}) catch {
        std.log.err("Failed to open fd file");
        return error.FileNotFound;
    };
    defer file.close();

    var buffer: [128]u8 = undefined;
    const bytes_read = file.readAll(&buffer) catch {
        std.log.err("Failed to read fd file");
        return error.FileReadFailed;
    };

    const content = buffer[0..bytes_read];

    // Parse the content: first line is fd, second line is size
    var lines = std.mem.tokenizeScalar(u8, content, '\n');
    const fd_line = lines.next() orelse {
        std.log.err("Invalid fd file format: missing fd line");
        return error.FdInfoReadFailed;
    };
    const size_line = lines.next() orelse {
        std.log.err("Invalid fd file format: missing size line");
        return error.FdInfoReadFailed;
    };

    const fd_str = allocator.dupe(u8, std.mem.trim(u8, fd_line, " \r\t")) catch {
        std.log.err("Failed to duplicate fd string");
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
