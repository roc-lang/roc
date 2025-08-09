//! Low-level platform abstractions for shared memory
//! Provides a unified interface for Windows and POSIX shared memory operations

const std = @import("std");
const builtin = @import("builtin");

/// Platform detection
pub const is_windows = builtin.target.os.tag == .windows;

/// Platform-specific handle type
pub const Handle = if (is_windows) *anyopaque else std.posix.fd_t;

/// Fixed base address for shared memory mapping on Windows to avoid ASLR issues
/// Using 0x10000000 (256MB) which is typically available on Windows
pub const SHARED_MEMORY_BASE_ADDR: ?*anyopaque = if (is_windows) @ptrFromInt(0x10000000) else null;

/// Windows API declarations
pub const windows = if (is_windows) struct {
    pub const HANDLE = *anyopaque;
    pub const DWORD = u32;
    pub const BOOL = c_int;
    pub const LPVOID = ?*anyopaque;
    pub const LPCWSTR = [*:0]const u16;
    pub const SIZE_T = usize;

    pub extern "kernel32" fn CreateFileMappingW(
        hFile: HANDLE,
        lpFileMappingAttributes: ?*anyopaque,
        flProtect: DWORD,
        dwMaximumSizeHigh: DWORD,
        dwMaximumSizeLow: DWORD,
        lpName: ?LPCWSTR,
    ) ?HANDLE;

    pub extern "kernel32" fn OpenFileMappingW(
        dwDesiredAccess: DWORD,
        bInheritHandle: BOOL,
        lpName: LPCWSTR,
    ) ?HANDLE;

    pub extern "kernel32" fn MapViewOfFile(
        hFileMappingObject: HANDLE,
        dwDesiredAccess: DWORD,
        dwFileOffsetHigh: DWORD,
        dwFileOffsetLow: DWORD,
        dwNumberOfBytesToMap: SIZE_T,
    ) LPVOID;

    pub extern "kernel32" fn MapViewOfFileEx(
        hFileMappingObject: HANDLE,
        dwDesiredAccess: DWORD,
        dwFileOffsetHigh: DWORD,
        dwFileOffsetLow: DWORD,
        dwNumberOfBytesToMap: SIZE_T,
        lpBaseAddress: LPVOID,
    ) LPVOID;

    pub extern "kernel32" fn UnmapViewOfFile(lpBaseAddress: LPVOID) BOOL;
    pub extern "kernel32" fn CloseHandle(hObject: HANDLE) BOOL;
    pub extern "kernel32" fn GetLastError() DWORD;
    pub extern "kernel32" fn GetSystemInfo(lpSystemInfo: *SYSTEM_INFO) void;

    pub const PAGE_READWRITE = 0x04;
    pub const FILE_MAP_READ = 0x0004;
    pub const FILE_MAP_WRITE = 0x0002;
    pub const FILE_MAP_ALL_ACCESS = 0x001f;
    pub const INVALID_HANDLE_VALUE = @as(HANDLE, @ptrFromInt(std.math.maxInt(usize)));
    pub const FALSE = 0;

    pub const SYSTEM_INFO = extern struct {
        wProcessorArchitecture: u16,
        wReserved: u16,
        dwPageSize: DWORD,
        lpMinimumApplicationAddress: LPVOID,
        lpMaximumApplicationAddress: LPVOID,
        dwActiveProcessorMask: *align(1) DWORD,
        dwNumberOfProcessors: DWORD,
        dwProcessorType: DWORD,
        dwAllocationGranularity: DWORD,
        wProcessorLevel: u16,
        wProcessorRevision: u16,
    };
} else struct {};

/// POSIX shared memory functions
pub const posix = if (!is_windows) struct {
    pub extern "c" fn mmap(
        addr: ?*anyopaque,
        len: usize,
        prot: c_int,
        flags: c_int,
        fd: c_int,
        offset: std.c.off_t,
    ) ?*anyopaque;

    pub extern "c" fn munmap(addr: *anyopaque, len: usize) c_int;
    pub extern "c" fn close(fd: c_int) c_int;
    pub extern "c" fn shm_open(name: [*:0]const u8, oflag: c_int, mode: std.c.mode_t) c_int;
    pub extern "c" fn shm_unlink(name: [*:0]const u8) c_int;

    pub const PROT_READ = 0x01;
    pub const PROT_WRITE = 0x02;
    pub const MAP_SHARED = 0x0001;
} else struct {};

/// Shared memory errors
pub const SharedMemoryError = error{
    CreateFileMappingFailed,
    OpenFileMappingFailed,
    MapViewOfFileFailed,
    ShmOpenFailed,
    MemfdCreateFailed,
    FtruncateFailed,
    MmapFailed,
    InvalidHandle,
    UnsupportedPlatform,
    OutOfMemory,
};

/// Get the system's page size at runtime
pub fn getSystemPageSize() !usize {
    const page_size: usize = switch (builtin.os.tag) {
        .windows => blk: {
            var system_info: windows.SYSTEM_INFO = undefined;
            windows.GetSystemInfo(&system_info);
            break :blk @intCast(system_info.dwPageSize);
        },
        .linux => blk: {
            const result = std.os.linux.getauxval(std.elf.AT_PAGESZ);
            break :blk if (result != 0) result else 4096;
        },
        .macos, .ios, .tvos, .watchos => blk: {
            var page_size_c: usize = undefined;
            var size: usize = @sizeOf(usize);
            _ = std.c.sysctlbyname("hw.pagesize", &page_size_c, &size, null, 0);
            break :blk page_size_c;
        },
        .freebsd, .netbsd, .openbsd, .dragonfly => blk: {
            const result = std.c.getpagesize();
            break :blk @intCast(result);
        },
        else => return error.UnsupportedPlatform,
    };

    // Ensure page_size is a power of 2 (required for alignForward)
    return std.math.ceilPowerOfTwo(usize, page_size) catch 4096;
}

/// Create a new anonymous shared memory mapping
pub fn createMapping(size: usize) SharedMemoryError!Handle {
    switch (builtin.os.tag) {
        .windows => {
            // Handle sizes larger than 4GB properly
            const size_high: windows.DWORD = if (size > std.math.maxInt(u32))
                @intCast(size >> 32)
            else
                0;
            const size_low: windows.DWORD = @intCast(size & 0xFFFFFFFF);

            const handle = windows.CreateFileMappingW(
                windows.INVALID_HANDLE_VALUE,
                null, // default security
                windows.PAGE_READWRITE,
                size_high, // high 32 bits
                size_low, // low 32 bits
                null, // anonymous mapping
            );

            if (handle == null) {
                const error_code = windows.GetLastError();
                std.log.err("Windows: Failed to create shared memory mapping (size: {}, high: {}, low: {}, error: {})", .{ size, size_high, size_low, error_code });
                return error.CreateFileMappingFailed;
            }

            return handle.?;
        },
        .linux => {
            // Use memfd_create for anonymous shared memory on Linux
            const fd_raw = std.os.linux.memfd_create("roc_shm", std.os.linux.MFD.CLOEXEC);
            const fd = std.math.cast(std.posix.fd_t, fd_raw) orelse return error.MemfdCreateFailed;

            // Set the size of the shared memory
            std.posix.ftruncate(fd, size) catch {
                _ = std.posix.close(fd);
                return error.FtruncateFailed;
            };

            return fd;
        },
        .macos, .freebsd, .openbsd, .netbsd => {
            // Use shm_open with a random name
            const random_name = std.fmt.allocPrint(
                std.heap.page_allocator,
                "/roc_shm_{}",
                .{std.crypto.random.int(u64)},
            ) catch {
                return error.OutOfMemory;
            };
            defer std.heap.page_allocator.free(random_name);

            const shm_name = std.fmt.allocPrintZ(
                std.heap.page_allocator,
                "{s}",
                .{random_name},
            ) catch {
                return error.OutOfMemory;
            };
            defer std.heap.page_allocator.free(shm_name);

            const fd = posix.shm_open(
                shm_name.ptr,
                @as(u32, @bitCast(std.posix.O{ .ACCMODE = .RDWR, .CREAT = true, .EXCL = true })),
                0o600,
            );

            if (fd < 0) {
                return error.ShmOpenFailed;
            }

            // Immediately unlink so it gets cleaned up when all references are closed
            _ = posix.shm_unlink(shm_name.ptr);

            // Set the size of the shared memory
            std.posix.ftruncate(fd, size) catch {
                _ = std.posix.close(fd);
                return error.FtruncateFailed;
            };

            return fd;
        },
        else => return error.UnsupportedPlatform,
    }
}

/// Open an existing named shared memory mapping
pub fn openMapping(allocator: std.mem.Allocator, name: []const u8) SharedMemoryError!Handle {
    switch (builtin.os.tag) {
        .windows => {
            const wide_name = std.unicode.utf8ToUtf16LeAllocZ(allocator, name) catch {
                return error.OutOfMemory;
            };
            defer allocator.free(wide_name);

            const handle = windows.OpenFileMappingW(
                windows.FILE_MAP_ALL_ACCESS,
                windows.FALSE,
                wide_name,
            );

            if (handle == null) {
                return error.OpenFileMappingFailed;
            }

            return handle.?;
        },
        .macos, .freebsd, .openbsd, .netbsd => {
            const shm_name = std.fmt.allocPrintZ(allocator, "/{s}", .{name}) catch {
                return error.OutOfMemory;
            };
            defer allocator.free(shm_name);

            const fd = posix.shm_open(
                shm_name.ptr,
                @as(u32, @bitCast(std.posix.O{ .ACCMODE = .RDWR })),
                0,
            );

            if (fd < 0) {
                return error.ShmOpenFailed;
            }

            return fd;
        },
        .linux => {
            // Linux doesn't support named shared memory via shm_open reliably
            // Use the coordination file approach instead
            return error.UnsupportedPlatform;
        },
        else => return error.UnsupportedPlatform,
    }
}

/// Map shared memory into the process address space
pub fn mapMemory(handle: Handle, size: usize, base_addr: ?*anyopaque) SharedMemoryError!*anyopaque {
    switch (builtin.os.tag) {
        .windows => {
            const ptr = if (base_addr) |addr|
                windows.MapViewOfFileEx(
                    handle,
                    windows.FILE_MAP_ALL_ACCESS,
                    0, // offset high
                    0, // offset low
                    size,
                    addr,
                )
            else
                windows.MapViewOfFile(
                    handle,
                    windows.FILE_MAP_ALL_ACCESS,
                    0, // offset high
                    0, // offset low
                    size,
                );

            if (ptr == null) {
                const error_code = windows.GetLastError();
                std.log.err("Windows: Failed to map shared memory view (size: {}, error: {})", .{ size, error_code });
                return error.MapViewOfFileFailed;
            }

            return ptr.?;
        },
        .linux, .macos, .freebsd, .openbsd, .netbsd => {
            const ptr = posix.mmap(
                base_addr,
                size,
                posix.PROT_READ | posix.PROT_WRITE,
                posix.MAP_SHARED,
                handle,
                0,
            ) orelse {
                std.log.err("POSIX: Failed to map shared memory (size: {})", .{size});
                return error.MmapFailed;
            };
            return ptr;
        },
        else => return error.UnsupportedPlatform,
    }
}

/// Unmap shared memory from the process address space
pub fn unmapMemory(ptr: *anyopaque, size: usize) void {
    if (comptime is_windows) {
        unmapWindowsMemory(ptr);
    } else {
        unmapPosixMemory(ptr, size);
    }
}

fn unmapWindowsMemory(ptr: *anyopaque) void {
    if (comptime is_windows) {
        _ = windows.UnmapViewOfFile(ptr);
    }
}

fn unmapPosixMemory(ptr: *anyopaque, size: usize) void {
    if (comptime !is_windows) {
        _ = posix.munmap(ptr, size);
    }
}

/// Close a shared memory handle
/// On Windows: closes the handle
/// On POSIX: closes the file descriptor
/// Note: Child processes on Windows should NOT close inherited handles
pub fn closeHandle(handle: Handle, is_owner: bool) void {
    if (comptime is_windows) {
        closeWindowsHandle(handle, is_owner);
    } else {
        closePosixHandle(handle);
    }
}

fn closeWindowsHandle(handle: Handle, is_owner: bool) void {
    if (comptime is_windows) {
        // On Windows, only the owner should close the handle
        // Inherited handles belong to the parent process
        if (is_owner) {
            _ = windows.CloseHandle(handle);
        }
    }
}

fn closePosixHandle(handle: Handle) void {
    if (comptime !is_windows) {
        // POSIX always closes the fd
        _ = posix.close(handle);
    }
}
