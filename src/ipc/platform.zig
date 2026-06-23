//! Low-level platform abstractions for shared memory
//! Provides a unified interface for Windows and POSIX shared memory operations

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

/// Platform detection
pub const is_windows = builtin.target.os.tag == .windows;

/// Platform-specific handle type
pub const Handle = if (is_windows) *anyopaque else std.posix.fd_t;

/// Base address for shared memory mapping. Set to null to let the OS choose
/// the best address. The payload is an offset-addressed LIR image, so
/// the interpreter shim does not depend on matching parent-process pointers.
pub const SHARED_MEMORY_BASE_ADDR: ?*anyopaque = null;

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
    pub extern "kernel32" fn VirtualProtect(
        lpAddress: LPVOID,
        dwSize: SIZE_T,
        flNewProtect: DWORD,
        lpflOldProtect: *DWORD,
    ) callconv(.winapi) BOOL;

    pub const PAGE_READWRITE = 0x04;
    pub const PAGE_READONLY = 0x02;
    pub const PAGE_EXECUTE_READ = 0x20;
    pub const PAGE_EXECUTE_READWRITE = 0x40;
    pub const FILE_MAP_READ = 0x0004;
    pub const FILE_MAP_WRITE = 0x0002;
    pub const FILE_MAP_EXECUTE = 0x0020;
    pub const FILE_MAP_ALL_ACCESS = 0x001f;
    pub const INVALID_HANDLE_VALUE = @as(HANDLE, @ptrFromInt(std.math.maxInt(usize)));
    pub const FALSE = 0;

    // SEC_RESERVE: Reserve pages without committing them. Pages must be committed
    // via VirtualAlloc before they can be accessed. This allows reserving large
    // virtual address spaces (e.g., 2TB) without requiring immediate page file backing.
    pub const SEC_RESERVE = 0x4000000;

    // Memory allocation constants for VirtualAlloc
    pub const MEM_COMMIT = 0x1000;
    pub const MEM_RESERVE = 0x2000;

    // VirtualAlloc: Commits reserved pages so they can be accessed.
    // When used with MEM_COMMIT on a reserved region, it commits the pages
    // without requiring a new reservation.
    pub extern "kernel32" fn VirtualAlloc(
        lpAddress: ?*anyopaque,
        dwSize: SIZE_T,
        flAllocationType: DWORD,
        flProtect: DWORD,
    ) ?*anyopaque;

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
    // Note: mmap returns MAP_FAILED ((void*)-1) on error, NOT NULL
    // So we declare it as non-optional and check against MAP_FAILED
    pub extern "c" fn mmap(
        addr: ?*anyopaque,
        len: usize,
        prot: c_int,
        flags: c_int,
        fd: c_int,
        offset: std.c.off_t,
    ) *anyopaque;

    pub extern "c" fn munmap(addr: *anyopaque, len: usize) c_int;
    pub extern "c" fn close(fd: c_int) c_int;
    pub extern "c" fn shm_open(name: [*:0]const u8, oflag: c_int, mode: std.c.mode_t) c_int;
    pub extern "c" fn shm_unlink(name: [*:0]const u8) c_int;

    pub const PROT_READ = 0x01;
    pub const PROT_WRITE = 0x02;
    pub const MAP_SHARED = 0x0001;
    pub const MAP_FAILED: *anyopaque = @ptrFromInt(@as(usize, @bitCast(@as(isize, -1))));
} else struct {};

/// Shared memory errors
pub const SharedMemoryError = error{
    CreateFileMappingFailed,
    OpenFileMappingFailed,
    MapViewOfFileFailed,
    TempFileOpenFailed,
    TempFileUnlinkFailed,
    ShmOpenFailed,
    ShmUnlinkFailed,
    MemfdCreateFailed,
    FtruncateFailed,
    MmapFailed,
    InvalidHandle,
    UnsupportedPlatform,
    OutOfMemory,
};

/// Get the system's page size at runtime
pub fn getSystemPageSize() error{ SysctlFailed, UnsupportedPlatform }!usize {
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
            const rc = std.c.sysctlbyname("hw.pagesize", &page_size_c, &size, null, 0);
            if (rc != 0) {
                return error.SysctlFailed;
            }
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
pub fn createMapping(io: std.Io, size: usize) SharedMemoryError!Handle {
    switch (builtin.os.tag) {
        .windows => {
            // Handle sizes larger than 4GB properly
            const size_high: windows.DWORD = if (size > std.math.maxInt(u32))
                @intCast(size >> 32)
            else
                0;
            const size_low: windows.DWORD = @intCast(size & 0xFFFFFFFF);

            // Use SEC_RESERVE to only reserve virtual address space without committing
            // physical memory or page file backing. This allows reserving very large
            // address spaces (e.g., 2TB) without requiring that much RAM or page file.
            //
            // Pages must be committed via VirtualAlloc(MEM_COMMIT) before they can be
            // accessed. The SharedMemoryAllocator handles this in its alloc function.
            //
            // Without SEC_RESERVE, CreateFileMapping with PAGE_READWRITE would require
            // the full size to be backed by the page file immediately, which would fail
            // on systems without enough page file space.
            const handle = windows.CreateFileMappingW(
                windows.INVALID_HANDLE_VALUE,
                null, // default security
                windows.PAGE_EXECUTE_READWRITE | windows.SEC_RESERVE,
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
            if (std.c.ftruncate(fd, @intCast(size)) != 0) {
                _ = std.c.close(fd);
                return error.FtruncateFailed;
            }

            return fd;
        },
        .macos, .freebsd, .openbsd, .netbsd => return createPosixShmMapping(io, size),
        else => return error.UnsupportedPlatform,
    }
}

/// Create shared memory whose pages can later be marked executable.
pub fn createExecutableMapping(io: std.Io, size: usize) SharedMemoryError!Handle {
    return switch (builtin.os.tag) {
        .macos => createUnlinkedTempFileMapping(io, size),
        else => createMapping(io, size),
    };
}

fn createUnlinkedTempFileMapping(io: std.Io, size: usize) SharedMemoryError!Handle {
    var random_buf: [8]u8 = undefined;
    io.random(&random_buf);
    const random_val = std.mem.readInt(u64, &random_buf, .little);

    var file_path_buf: [std.fmt.count("/tmp/roc_shm_{}", .{@as(u64, std.math.maxInt(u64))}) + 1]u8 = undefined;
    const file_path = std.fmt.bufPrintZ(&file_path_buf, "/tmp/roc_shm_{}", .{random_val}) catch unreachable;
    const fd = std.c.open(
        file_path,
        std.c.O{ .ACCMODE = .RDWR, .CREAT = true, .EXCL = true },
        @as(std.c.mode_t, 0o600),
    );
    if (fd < 0) return error.TempFileOpenFailed;

    if (std.c.unlink(file_path) != 0) {
        _ = std.c.close(fd);
        return error.TempFileUnlinkFailed;
    }

    if (std.c.ftruncate(fd, @intCast(size)) != 0) {
        _ = std.c.close(fd);
        return error.FtruncateFailed;
    }

    return fd;
}

fn createPosixShmMapping(io: std.Io, size: usize) SharedMemoryError!Handle {
    // Use shm_open with a random name
    var random_buf: [8]u8 = undefined;
    io.random(&random_buf);
    const random_val = std.mem.readInt(u64, &random_buf, .little);
    // The name is "/roc_shm_" + a u64, so size the buffer to the longest
    // possible such name (largest u64) plus a NUL - it can never overflow.
    var shm_name_buf: [std.fmt.count("/roc_shm_{}", .{@as(u64, std.math.maxInt(u64))}) + 1]u8 = undefined;
    const shm_name_null_terminated = std.fmt.bufPrintZ(&shm_name_buf, "/roc_shm_{}", .{random_val}) catch unreachable;
    const fd = posix.shm_open(
        shm_name_null_terminated,
        @as(u32, @bitCast(std.posix.O{ .ACCMODE = .RDWR, .CREAT = true, .EXCL = true })),
        0o600,
    );

    if (fd < 0) {
        return error.ShmOpenFailed;
    }

    // Immediately unlink so it gets cleaned up when all references are closed.
    if (posix.shm_unlink(shm_name_null_terminated) != 0) {
        _ = std.c.close(fd);
        return error.ShmUnlinkFailed;
    }

    // Set the size of the shared memory.
    if (std.c.ftruncate(fd, @intCast(size)) != 0) {
        _ = std.c.close(fd);
        return error.FtruncateFailed;
    }

    return fd;
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
                windows.FILE_MAP_ALL_ACCESS | windows.FILE_MAP_EXECUTE,
                windows.FALSE,
                wide_name,
            );

            if (handle == null) {
                return error.OpenFileMappingFailed;
            }

            return handle.?;
        },
        .macos, .freebsd, .openbsd, .netbsd => {
            const shm_name = std.fmt.allocPrint(allocator, "/{s}\x00", .{name}) catch {
                return error.OutOfMemory;
            };
            defer allocator.free(shm_name);
            const shm_name_null_terminated = shm_name[0 .. shm_name.len - 1 :0];
            const fd = posix.shm_open(
                shm_name_null_terminated,
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
    return mapMemoryWithLogging(handle, size, base_addr, true);
}

/// Map shared memory without logging failures.
pub fn mapMemoryNoLog(handle: Handle, size: usize, base_addr: ?*anyopaque) SharedMemoryError!*anyopaque {
    return mapMemoryWithLogging(handle, size, base_addr, false);
}

fn mapMemoryWithLogging(
    handle: Handle,
    size: usize,
    base_addr: ?*anyopaque,
    log_failure: bool,
) SharedMemoryError!*anyopaque {
    switch (builtin.os.tag) {
        .windows => {
            const ptr = if (base_addr) |addr|
                windows.MapViewOfFileEx(
                    handle,
                    windows.FILE_MAP_ALL_ACCESS | windows.FILE_MAP_EXECUTE,
                    0, // offset high
                    0, // offset low
                    size,
                    addr,
                )
            else
                windows.MapViewOfFile(
                    handle,
                    windows.FILE_MAP_ALL_ACCESS | windows.FILE_MAP_EXECUTE,
                    0, // offset high
                    0, // offset low
                    size,
                );

            if (ptr == null) {
                const error_code = windows.GetLastError();
                if (log_failure) {
                    std.log.err("Windows: Failed to map shared memory view (size: {}, error: {})", .{ size, error_code });
                }
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
            );
            // mmap returns MAP_FAILED ((void*)-1) on error, not NULL
            if (ptr == posix.MAP_FAILED) {
                const errno = std.c._errno().*;
                if (log_failure) {
                    std.log.err("POSIX: Failed to map shared memory (size: {}, fd: {}, errno: {})", .{ size, handle, errno });
                }
                return error.MmapFailed;
            }
            return ptr;
        },
        else => return error.UnsupportedPlatform,
    }
}

/// Page protection mode for already mapped shared memory.
pub const MemoryProtection = enum {
    read_write,
    read_only,
    read_execute,
};

/// Errors raised while changing protection on mapped shared memory.
pub const MemoryProtectError = error{
    MprotectFailed,
    VirtualProtectFailed,
    UnsupportedPlatform,
};

/// Change page permissions for an already mapped shared-memory range.
pub fn protectMappedMemory(
    ptr: [*]align(1) u8,
    len: usize,
    protection: MemoryProtection,
) MemoryProtectError!void {
    if (len == 0) return;

    switch (builtin.os.tag) {
        .macos, .ios, .tvos, .watchos, .linux, .freebsd, .openbsd, .netbsd => {
            const prot: std.posix.PROT = switch (protection) {
                .read_write => .{ .READ = true, .WRITE = true },
                .read_only => .{ .READ = true },
                .read_execute => .{ .READ = true, .EXEC = true },
            };
            if (std.c.mprotect(@ptrCast(@alignCast(ptr)), len, prot) != 0) return error.MprotectFailed;
        },
        .windows => {
            const protect: windows.DWORD = switch (protection) {
                .read_write => windows.PAGE_READWRITE,
                .read_only => windows.PAGE_READONLY,
                .read_execute => windows.PAGE_EXECUTE_READ,
            };
            var old_protect: windows.DWORD = undefined;
            if (windows.VirtualProtect(ptr, len, protect, &old_protect) == windows.FALSE) {
                return error.VirtualProtectFailed;
            }
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
        const rc = posix.munmap(ptr, size);
        if (rc != 0) {
            if (builtin.mode == .Debug) {
                std.debug.panic("munmap failed with errno {d}", .{std.c._errno().*});
            }
            unreachable;
        }
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
        const rc = posix.close(handle);
        if (rc != 0) {
            if (builtin.mode == .Debug) {
                std.debug.panic("close failed with errno {d}", .{std.c._errno().*});
            }
            unreachable;
        }
    }
}
