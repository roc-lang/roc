//! A shim to read the filepath for our app.roc from shared memory for the interpreter

const std = @import("std");
const builtin = @import("builtin");
const builtins = @import("builtins");

const RocStr = builtins.str.RocStr;

// Platform-specific shared memory implementation
const is_windows = builtin.target.os.tag == .windows;

// POSIX shared memory functions - only what we actually use
const posix = if (!is_windows) struct {
    extern "c" fn mmap(addr: ?*anyopaque, len: usize, prot: c_int, flags: c_int, fd: c_int, offset: std.c.off_t) ?*anyopaque;
    extern "c" fn munmap(addr: *anyopaque, len: usize) c_int;
    extern "c" fn close(fd: c_int) c_int;
} else struct {};

// Windows shared memory functions
const windows = if (is_windows) struct {
    const HANDLE = *anyopaque;
    const DWORD = u32;
    const BOOL = c_int;
    const LPVOID = ?*anyopaque;
    const LPCWSTR = [*:0]const u16;
    const SIZE_T = usize;

    extern "kernel32" fn OpenFileMappingW(dwDesiredAccess: DWORD, bInheritHandle: BOOL, lpName: LPCWSTR) ?HANDLE;
    extern "kernel32" fn MapViewOfFile(hFileMappingObject: HANDLE, dwDesiredAccess: DWORD, dwFileOffsetHigh: DWORD, dwFileOffsetLow: DWORD, dwNumberOfBytesToMap: SIZE_T) LPVOID;
    extern "kernel32" fn UnmapViewOfFile(lpBaseAddress: LPVOID) BOOL;
    extern "kernel32" fn CloseHandle(hObject: HANDLE) BOOL;
    extern "kernel32" fn GetFileSizeEx(hFile: HANDLE, lpFileSize: *i64) BOOL;

    const FILE_MAP_READ = 0x0004;
} else struct {};

/// Info read from the coordination file
const FdInfo = struct {
    fd_str: []u8,
    size: usize,
};

/// Read the fd/handle and size from the filesystem-based communication mechanism
fn readFdInfo() !FdInfo {
    // Get our own executable path
    const exe_path = try std.fs.selfExePathAlloc(std.heap.page_allocator);
    defer std.heap.page_allocator.free(exe_path);

    // Get the directory containing our executable (should be "roc-tmp-<random>")
    const exe_dir = std.fs.path.dirname(exe_path) orelse return error.InvalidExePath;
    const dir_basename = std.fs.path.basename(exe_dir);

    // Verify it has the expected prefix
    if (!std.mem.startsWith(u8, dir_basename, "roc-tmp-")) {
        return error.UnexpectedDirName;
    }

    // Construct the fd file path by appending .txt to the directory path
    // First, remove any trailing slashes from exe_dir
    var dir_path = exe_dir;
    while (dir_path.len > 0 and (dir_path[dir_path.len - 1] == '/' or dir_path[dir_path.len - 1] == '\\')) {
        dir_path = dir_path[0 .. dir_path.len - 1];
    }

    const fd_file_path = try std.fmt.allocPrint(std.heap.page_allocator, "{s}.txt", .{dir_path});
    defer std.heap.page_allocator.free(fd_file_path);

    // Read the fd and size from the file
    const fd_file = std.fs.cwd().openFile(fd_file_path, .{}) catch |err| switch (err) {
        error.FileNotFound => return error.FdFileNotFound,
        else => return err,
    };
    defer fd_file.close();

    const content = try fd_file.readToEndAlloc(std.heap.page_allocator, 128);
    defer std.heap.page_allocator.free(content);

    // Clean up the fd file since we no longer need it
    std.fs.cwd().deleteFile(fd_file_path) catch {};

    // Parse the content: first line is fd, second line is size
    var lines = std.mem.tokenizeScalar(u8, content, '\n');
    const fd_line = lines.next() orelse return error.InvalidFileFormat;
    const size_line = lines.next() orelse return error.InvalidFileFormat;

    const fd_str = try std.heap.page_allocator.dupe(u8, std.mem.trim(u8, fd_line, " \r\t"));
    const size = try std.fmt.parseInt(usize, std.mem.trim(u8, size_line, " \r\t"), 10);


    return FdInfo{
        .fd_str = fd_str,
        .size = size,
    };
}

/// Exported symbol that reads a string from shared memory ROC_FILE_TO_INTERPRET
/// Returns a RocStr to the caller
/// Expected format in shared memory: [usize length][u8... data]
export fn roc_entrypoint(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque) callconv(.C) void {
    const result = if (comptime is_windows)
        readFromWindowsSharedMemory(ops)
    else
        readFromPosixSharedMemory(ops);

    const roc_str_ptr: *RocStr = @ptrCast(@alignCast(ret_ptr));
    roc_str_ptr.* = result;
}

fn readFromWindowsSharedMemory(ops: *builtins.host_abi.RocOps) RocStr {
    const fd_info = readFdInfo() catch {
        return RocStr.empty();
    };
    defer std.heap.page_allocator.free(fd_info.fd_str);

    const handle_int = std.fmt.parseInt(usize, fd_info.fd_str, 10) catch {
        return RocStr.empty();
    };

    const shm_handle = @as(windows.HANDLE, @ptrFromInt(handle_int));

    // Map the shared memory with the exact size
    const mapped_ptr = windows.MapViewOfFile(shm_handle, windows.FILE_MAP_READ, 0, 0, fd_info.size) orelse {
        return RocStr.empty();
    };
    defer _ = windows.UnmapViewOfFile(mapped_ptr);

    // Read the length from the beginning of shared memory
    const length_ptr: *align(1) const usize = @ptrCast(mapped_ptr);
    const string_length = length_ptr.*;

    // Get pointer to string data
    const string_data = @as([*]u8, @ptrCast(mapped_ptr)) + @sizeOf(usize);

    return createRocStrFromData(ops, string_data, string_length);
}

fn readFromPosixSharedMemory(ops: *builtins.host_abi.RocOps) RocStr {
    const fd_info = readFdInfo() catch {
        return RocStr.empty();
    };
    defer std.heap.page_allocator.free(fd_info.fd_str);

    const shm_fd = std.fmt.parseInt(c_int, fd_info.fd_str, 10) catch {
        return RocStr.empty();
    };

    defer _ = posix.close(shm_fd);

    // Map the shared memory with the exact size from the file
    const mapped_ptr = posix.mmap(
        null,
        fd_info.size,
        0x01, // PROT_READ
        0x0001, // MAP_SHARED
        shm_fd,
        0,
    ) orelse {
        return RocStr.empty();
    };
    const mapped_memory = @as([*]u8, @ptrCast(mapped_ptr))[0..fd_info.size];
    defer _ = posix.munmap(mapped_ptr, fd_info.size);

    // Read the length from the beginning of shared memory
    const length_ptr: *align(1) const usize = @ptrCast(mapped_memory.ptr);
    const string_length = length_ptr.*;

    // Check if we have enough data
    if (fd_info.size < @sizeOf(usize) + string_length) {
        return RocStr.empty();
    }

    // Get pointer to string data
    const string_data = mapped_memory.ptr + @sizeOf(usize);

    return createRocStrFromData(ops, string_data, string_length);
}

fn createRocStrFromData(ops: *builtins.host_abi.RocOps, string_data: [*]u8, string_length: usize) RocStr {
    // For small strings, we can create them directly
    if (string_length <= @sizeOf(RocStr) - 1) {
        var result = RocStr.empty();
        @memcpy(result.asU8ptrMut()[0..string_length], string_data[0..string_length]);
        result.setLen(string_length);
        return result;
    }

    // For larger strings, allocate memory using RocOps
    const alignment = @alignOf(usize);
    var roc_alloc = builtins.host_abi.RocAlloc{
        .alignment = alignment,
        .length = string_length,
        .answer = undefined,
    };
    ops.roc_alloc(&roc_alloc, ops.env);
    const alloc_ptr = roc_alloc.answer;

    // Create a RocStr with the allocated memory
    const result = RocStr{
        .bytes = @ptrCast(alloc_ptr),
        .length = string_length,
        .capacity_or_alloc_ptr = string_length,
    };

    // Copy the data into the allocated memory
    @memcpy(@as([*]u8, @ptrCast(alloc_ptr)), string_data[0..string_length]);

    return result;
}
