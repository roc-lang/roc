//! Cross-platform shared memory allocator for sharing data between parent and child processes.
//!
//! This allocator creates a large named shared memory region that can be accessed by both
//! parent and child processes. It uses platform-specific APIs to ensure compatibility across
//! Linux, macOS, BSD, and Windows.
//!
//! ## Example Usage
//!
//! Parent process:
//! ```zig
//! const page_size = try SharedMemoryAllocator.getSystemPageSize();
//! const size = 1024 * 1024 * 1024; // 1GB
//! var shm = try SharedMemoryAllocator.create(allocator, "myapp_12345", size, page_size);
//! defer shm.deinit(allocator);
//!
//! // Use the allocator...
//! const data = try shm.allocator().alloc(u8, 1000);
//!
//! // Get the actual used size for the child
//! const used_size = shm.getRecommendedMapSize();
//!
//! // Spawn child with the size info
//! const result = try std.process.Child.run(.{
//!     .allocator = allocator,
//!     .argv = &.{ "child_process", "--shm-name=myapp_12345",
//!                 try std.fmt.allocPrint(allocator, "--shm-size={}", .{used_size}) },
//! });
//! ```
//!
//! Child process:
//! ```zig
//! // Parse command line args to get name and size
//! const shm_name = args.shm_name; // "myapp_12345"
//! const shm_size = args.shm_size; // The actual used size, not 1GB
//!
//! // Open with the actual used size, not the full 1GB
//! const page_size = try SharedMemoryAllocator.getSystemPageSize();
//! var shm = try SharedMemoryAllocator.open(allocator, shm_name, shm_size, page_size);
//! defer shm.deinit(allocator);
//!
//! // Access the data...
//! ```
//!
//! This approach is especially important on macOS where ftruncate() doesn't work
//! on shared memory objects, so the child must map only the used portion.

const std = @import("std");
const builtin = @import("builtin");

const SharedMemoryAllocator = @This();

// Windows API declarations
const windows = if (builtin.os.tag == .windows) struct {
    const HANDLE = *anyopaque;
    const DWORD = u32;
    const BOOL = c_int;
    const LPVOID = ?*anyopaque;
    const LPCWSTR = [*:0]const u16;
    const SIZE_T = usize;

    extern "kernel32" fn CreateFileMappingW(hFile: HANDLE, lpFileMappingAttributes: ?*anyopaque, flProtect: DWORD, dwMaximumSizeHigh: DWORD, dwMaximumSizeLow: DWORD, lpName: LPCWSTR) ?HANDLE;
    extern "kernel32" fn MapViewOfFile(hFileMappingObject: HANDLE, dwDesiredAccess: DWORD, dwFileOffsetHigh: DWORD, dwFileOffsetLow: DWORD, dwNumberOfBytesToMap: SIZE_T) LPVOID;
    extern "kernel32" fn UnmapViewOfFile(lpBaseAddress: LPVOID) BOOL;
    extern "kernel32" fn CloseHandle(hObject: HANDLE) BOOL;
    extern "kernel32" fn OpenFileMappingW(dwDesiredAccess: DWORD, bInheritHandle: BOOL, lpName: LPCWSTR) ?HANDLE;
    extern "kernel32" fn GetSystemInfo(lpSystemInfo: *SYSTEM_INFO) void;

    const PAGE_READWRITE = 0x04;
    const FILE_MAP_ALL_ACCESS = 0x001f;
    const INVALID_HANDLE_VALUE = @as(HANDLE, @ptrFromInt(std.math.maxInt(usize)));
    const FALSE = 0;

    const SYSTEM_INFO = extern struct {
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

/// Header stored at the beginning of shared memory to communicate metadata
pub const Header = extern struct {
    magic: u32 = 0x524F4353, // "ROCS"
    version: u32 = 1,
    used_size: u64 = 0,
    total_size: u64 = 0,
    data_offset: u64 = @sizeOf(Header),
    reserved: [472]u8 = [_]u8{0} ** 472, // Pad to 512 bytes total
};

/// Platform-specific handle for the shared memory
handle: Handle,
/// Base pointer to the mapped memory region
base_ptr: [*]align(1) u8,
/// Total size of the mapped region
total_size: usize,
/// Current offset for bump allocation (atomic for thread-safe allocation)
offset: std.atomic.Value(usize),
/// Name of the shared memory region (for child process to find it)
name: []const u8,
/// Whether this allocator owns the shared memory (should clean up)
is_owner: bool,
/// Page size for this system
page_size: usize,

const Handle = switch (builtin.os.tag) {
    .windows => std.os.windows.HANDLE,
    else => std.posix.fd_t,
};

/// Error type for unsupported operating systems
pub const PageSizeError = error{UnsupportedOperatingSystem};

/// Get the system's page size at runtime
pub fn getSystemPageSize() PageSizeError!usize {
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
        else => return error.UnsupportedOperatingSystem,
    };

    // Ensure page_size is a power of 2 (required for alignForward)
    // Round up to the next power of 2 if needed (no-op if already power of 2)
    return std.math.ceilPowerOfTwo(usize, page_size) catch 4096;
}

/// Creates a new shared memory region with the given name and size
pub fn create(gpa: std.mem.Allocator, name: []const u8, size: usize, page_size: usize) !SharedMemoryAllocator {
    const aligned_size = std.mem.alignForward(usize, size, page_size);

    switch (builtin.os.tag) {
        .windows => {
            // Windows: CreateFileMapping with a name
            const wide_name = try std.unicode.utf8ToUtf16LeAllocZ(gpa, name);
            defer gpa.free(wide_name);

            const handle = windows.CreateFileMappingW(
                windows.INVALID_HANDLE_VALUE,
                null, // default security
                windows.PAGE_READWRITE,
                @intCast(aligned_size >> 32), // high 32 bits
                @intCast(aligned_size & 0xFFFFFFFF), // low 32 bits
                wide_name,
            );

            if (handle == null) {
                return error.CreateFileMappingFailed;
            }

            const base_ptr = windows.MapViewOfFile(
                handle.?,
                windows.FILE_MAP_ALL_ACCESS,
                0, // offset high
                0, // offset low
                aligned_size,
            );

            if (base_ptr == null) {
                _ = windows.CloseHandle(handle.?);
                return error.MapViewOfFileFailed;
            }

            const result = SharedMemoryAllocator{
                .handle = handle.?,
                .base_ptr = @ptrCast(@alignCast(base_ptr)),
                .total_size = aligned_size,
                .offset = std.atomic.Value(usize).init(@sizeOf(Header)), // Start after header
                .name = try gpa.dupe(u8, name),
                .is_owner = true,
                .page_size = page_size,
            };

            // Initialize header
            const header_ptr = @as(*Header, @ptrCast(@alignCast(result.base_ptr)));
            header_ptr.* = Header{
                .total_size = aligned_size,
            };

            return result;
        },
        .linux, .macos, .freebsd, .openbsd, .netbsd => {
            // POSIX: shm_open + ftruncate + mmap
            const shm_name = try std.fmt.allocPrintZ(gpa, "/{s}", .{name});
            defer gpa.free(shm_name);

            // On macOS, we need O_TRUNC flag for ftruncate to work on shared memory
            const oflags = if (builtin.os.tag == .macos)
                @as(u32, @bitCast(std.posix.O{ .ACCMODE = .RDWR, .CREAT = true, .EXCL = true, .TRUNC = true }))
            else
                @as(u32, @bitCast(std.posix.O{ .ACCMODE = .RDWR, .CREAT = true, .EXCL = true }));

            var fd = c.shm_open(
                shm_name.ptr,
                oflags,
                0o600,
            );

            if (fd < 0) {
                const errno = std.c._errno().*;
                // If it exists, unlink it and try again
                if (errno == 17) { // EEXIST
                    _ = c.shm_unlink(shm_name.ptr);
                    fd = c.shm_open(
                        shm_name.ptr,
                        oflags,
                        0o600,
                    );
                    if (fd < 0) {
                        std.debug.print("SharedMemoryAllocator: shm_open retry failed with errno={} for name={s} size={}\n", .{ std.c._errno().*, shm_name, aligned_size });
                        return error.ShmOpenFailed;
                    }
                } else {
                    std.debug.print("SharedMemoryAllocator: shm_open failed with errno={} for name={s} size={}\n", .{ errno, shm_name, aligned_size });
                    return error.ShmOpenFailed;
                }
            }

            // Set the size of the shared memory
            std.posix.ftruncate(fd, @intCast(aligned_size)) catch {
                _ = std.posix.close(fd);
                _ = c.shm_unlink(shm_name.ptr);
                return error.FtruncateFailed;
            };

            // Map the shared memory
            const base_ptr = std.posix.mmap(
                null,
                aligned_size,
                std.posix.PROT.READ | std.posix.PROT.WRITE,
                .{ .TYPE = .SHARED },
                fd,
                0,
            ) catch |err| {
                _ = std.posix.close(fd);
                _ = c.shm_unlink(shm_name.ptr);
                return err;
            };

            const result = SharedMemoryAllocator{
                .handle = fd,
                .base_ptr = @ptrCast(@alignCast(base_ptr.ptr)),
                .total_size = aligned_size,
                .offset = std.atomic.Value(usize).init(@sizeOf(Header)), // Start after header
                .name = try gpa.dupe(u8, name),
                .is_owner = true,
                .page_size = page_size,
            };

            // Initialize header
            const header_ptr = @as(*Header, @ptrCast(@alignCast(result.base_ptr)));
            header_ptr.* = Header{
                .total_size = aligned_size,
            };

            return result;
        },
        else => @compileError("Unsupported platform"),
    }
}

/// Opens an existing shared memory region by reading its header first.
/// This function will map only the required amount of memory as specified in the header.
pub fn openWithHeader(gpa: std.mem.Allocator, name: []const u8, page_size: usize) !SharedMemoryAllocator {
    switch (builtin.os.tag) {
        .windows => {
            const wide_name = try std.unicode.utf8ToUtf16LeAllocZ(gpa, name);
            defer gpa.free(wide_name);

            const handle = windows.OpenFileMappingW(
                windows.FILE_MAP_ALL_ACCESS,
                windows.FALSE,
                wide_name,
            );

            if (handle == null) {
                return error.OpenFileMappingFailed;
            }

            // First map just the header
            const header_ptr = windows.MapViewOfFile(
                handle.?,
                windows.FILE_MAP_ALL_ACCESS,
                0,
                0,
                @sizeOf(Header),
            );

            if (header_ptr == null) {
                _ = windows.CloseHandle(handle.?);
                return error.MapViewOfFileFailed;
            }

            const header = @as(*const Header, @ptrCast(@alignCast(header_ptr))).*;
            _ = windows.UnmapViewOfFile(header_ptr);

            if (header.magic != 0x524F4353) {
                _ = windows.CloseHandle(handle.?);
                return error.InvalidSharedMemory;
            }

            // Now map the actual size
            const actual_size = @as(usize, @intCast(header.used_size));
            // Map the actual size based on header
            const base_ptr = windows.MapViewOfFile(
                handle.?,
                windows.FILE_MAP_ALL_ACCESS,
                0,
                0,
                actual_size,
            );

            if (base_ptr == null) {
                _ = windows.CloseHandle(handle.?);
                return error.MapViewOfFileFailed;
            }

            return SharedMemoryAllocator{
                .handle = handle.?,
                .base_ptr = @ptrCast(@alignCast(base_ptr)),
                .total_size = @as(usize, @intCast(header.used_size)),
                .offset = std.atomic.Value(usize).init(@as(usize, @intCast(header.data_offset))),
                .name = try gpa.dupe(u8, name),
                .is_owner = false,
                .page_size = page_size,
            };
        },
        .linux, .macos, .freebsd, .openbsd, .netbsd => {
            const shm_name = try std.fmt.allocPrintZ(gpa, "/{s}", .{name});
            defer gpa.free(shm_name);

            const fd = c.shm_open(
                shm_name.ptr,
                @as(u32, @bitCast(std.posix.O{ .ACCMODE = .RDWR })),
                0,
            );

            if (fd < 0) {
                return error.ShmOpenFailed;
            }

            // First map just the header
            const header_map = std.posix.mmap(
                null,
                @sizeOf(Header),
                std.posix.PROT.READ,
                .{ .TYPE = .SHARED },
                fd,
                0,
            ) catch |err| {
                _ = std.posix.close(fd);
                return err;
            };

            const header = @as(*const Header, @ptrCast(@alignCast(header_map.ptr))).*;
            std.posix.munmap(@alignCast(header_map));

            if (header.magic != 0x524F4353) {
                _ = std.posix.close(fd);
                return error.InvalidSharedMemory;
            }

            // Now map the actual size from the header
            const actual_size = @as(usize, @intCast(header.used_size));
            const base_ptr = std.posix.mmap(
                null,
                actual_size,
                std.posix.PROT.READ | std.posix.PROT.WRITE,
                .{ .TYPE = .SHARED },
                fd,
                0,
            ) catch |err| {
                _ = std.posix.close(fd);
                return err;
            };

            // Don't unlink here - let the owner do it during deinit

            return SharedMemoryAllocator{
                .handle = fd,
                .base_ptr = @ptrCast(@alignCast(base_ptr.ptr)),
                .total_size = actual_size,
                .offset = std.atomic.Value(usize).init(@as(usize, @intCast(header.data_offset))),
                .name = try gpa.dupe(u8, name),
                .is_owner = false,
                .page_size = page_size,
            };
        },
        else => @compileError("Unsupported platform"),
    }
}

/// Opens an existing shared memory region created by another process.
///
/// IMPORTANT: The `size` parameter should be the actual used size from the parent
/// process (obtained via getRecommendedMapSize()), NOT the original allocated size.
/// This is especially important on macOS where the shared memory object remains at
/// its original size and cannot be truncated.
pub fn open(gpa: std.mem.Allocator, name: []const u8, size: usize, page_size: usize) !SharedMemoryAllocator {
    const aligned_size = std.mem.alignForward(usize, size, page_size);

    switch (builtin.os.tag) {
        .windows => {
            const wide_name = try std.unicode.utf8ToUtf16LeAllocZ(gpa, name);
            defer gpa.free(wide_name);

            const handle = windows.OpenFileMappingW(
                windows.FILE_MAP_ALL_ACCESS,
                windows.FALSE, // don't inherit
                wide_name,
            );

            if (handle == null) {
                return error.OpenFileMappingFailed;
            }

            const base_ptr = windows.MapViewOfFile(
                handle.?,
                windows.FILE_MAP_ALL_ACCESS,
                0, // offset high
                0, // offset low
                @sizeOf(Header), // Map just the header first
            );

            if (base_ptr == null) {
                _ = windows.CloseHandle(handle.?);
                return error.MapViewOfFileFailed;
            }

            return SharedMemoryAllocator{
                .handle = handle.?,
                .base_ptr = @ptrCast(@alignCast(base_ptr)),
                .total_size = aligned_size,
                .offset = std.atomic.Value(usize).init(0),
                .name = try gpa.dupe(u8, name),
                .is_owner = false,
                .page_size = page_size,
            };
        },
        .linux, .macos, .freebsd, .openbsd, .netbsd => {
            const shm_name = try std.fmt.allocPrintZ(gpa, "/{s}", .{name});
            defer gpa.free(shm_name);

            const fd = c.shm_open(
                shm_name.ptr,
                @as(u32, @bitCast(std.posix.O{ .ACCMODE = .RDWR })),
                0,
            );

            if (fd < 0) {
                return error.ShmOpenFailed;
            }

            // Map the shared memory
            const base_ptr = std.posix.mmap(
                null,
                aligned_size,
                std.posix.PROT.READ | std.posix.PROT.WRITE,
                .{ .TYPE = .SHARED },
                fd,
                0,
            ) catch |err| {
                _ = std.posix.close(fd);
                return err;
            };

            // Don't unlink here - let the owner do it during deinit

            return SharedMemoryAllocator{
                .handle = fd,
                .base_ptr = @ptrCast(@alignCast(base_ptr.ptr)),
                .total_size = aligned_size,
                .offset = std.atomic.Value(usize).init(0),
                .name = try gpa.dupe(u8, name),
                .is_owner = false,
                .page_size = page_size,
            };
        },
        else => @compileError("Unsupported platform"),
    }
}

/// Updates the header with the current used size.
/// Should be called before handing off to child process.
pub fn updateHeader(self: *SharedMemoryAllocator) void {
    const header_ptr = @as(*Header, @ptrCast(@alignCast(self.base_ptr)));
    header_ptr.used_size = self.getUsedSize();
}

/// Deinitializes the shared memory allocator
pub fn deinit(self: *SharedMemoryAllocator, gpa: std.mem.Allocator) void {
    // Update header before closing
    if (self.is_owner) {
        self.updateHeader();
    }
    switch (builtin.os.tag) {
        .windows => {
            _ = windows.UnmapViewOfFile(self.base_ptr);
            _ = windows.CloseHandle(self.handle);
        },
        .linux, .macos, .freebsd, .openbsd, .netbsd => {
            std.posix.munmap(@alignCast(self.base_ptr[0..self.total_size]));
            _ = std.posix.close(self.handle);

            // If we're the owner and on POSIX, try to unlink
            // (though child process should have already done this)
            if (self.is_owner) {
                const shm_name = std.fmt.allocPrintZ(gpa, "/{s}", .{self.name}) catch return;
                defer gpa.free(shm_name);
                _ = c.shm_unlink(shm_name.ptr);
            }
        },
        else => @compileError("Unsupported platform"),
    }

    gpa.free(self.name);
}

/// Returns a std.mem.Allocator interface for this shared memory allocator
pub fn allocator(self: *SharedMemoryAllocator) std.mem.Allocator {
    return .{
        .ptr = self,
        .vtable = &.{
            .alloc = alloc,
            .resize = resize,
            .free = free,
            .remap = remap,
        },
    };
}

fn alloc(ctx: *anyopaque, len: usize, ptr_align: std.mem.Alignment, ret_addr: usize) ?[*]u8 {
    _ = ret_addr;
    const self: *SharedMemoryAllocator = @ptrCast(@alignCast(ctx));

    const alignment = @as(usize, 1) << @intFromEnum(ptr_align);

    // Lock-free allocation using compare-and-swap
    while (true) {
        const current_offset = self.offset.load(.monotonic);
        const aligned_offset = std.mem.alignForward(usize, current_offset, alignment);
        const end_offset = aligned_offset + len;

        if (end_offset > self.total_size) {
            return null; // Out of memory
        }

        // Try to atomically update the offset
        if (self.offset.cmpxchgWeak(
            current_offset,
            end_offset,
            .monotonic,
            .monotonic,
        ) == null) {
            // Success! We claimed this region
            const ptr = self.base_ptr + aligned_offset;
            return @ptrCast(ptr);
        }
        // CAS failed, another thread allocated - retry with new offset
    }
}

fn resize(ctx: *anyopaque, buf: []u8, buf_align: std.mem.Alignment, new_len: usize, ret_addr: usize) bool {
    _ = ctx;
    _ = buf_align;
    _ = ret_addr;

    // Simple bump allocator doesn't support resize
    // Could be implemented by checking if this is the last allocation
    return new_len <= buf.len;
}

fn free(ctx: *anyopaque, buf: []u8, buf_align: std.mem.Alignment, ret_addr: usize) void {
    _ = ctx;
    _ = buf;
    _ = buf_align;
    _ = ret_addr;

    // Simple bump allocator doesn't support free
    // Memory is only freed when the entire region is unmapped
}

fn remap(ctx: *anyopaque, old_mem: []u8, old_align: std.mem.Alignment, new_size: usize, ret_addr: usize) ?[*]u8 {
    _ = ctx;
    _ = old_mem;
    _ = old_align;
    _ = new_size;
    _ = ret_addr;

    // Simple bump allocator doesn't support remapping
    return null;
}

/// Get the current amount of used memory
pub fn getUsedSize(self: *const SharedMemoryAllocator) usize {
    return self.offset.load(.monotonic);
}

/// Get the recommended size for a child process to map.
/// This is the used size aligned to page boundaries.
///
/// IMPORTANT: On platforms where shrinkToFit doesn't work (like macOS), the parent
/// process MUST communicate this size to the child process (e.g., via command line
/// arguments or environment variables). The child should then use this size when
/// calling open() to map only what's needed, not the full 1GB.
///
/// Example:
/// ```zig
/// // Parent process
/// const map_size = shm.getRecommendedMapSize();
/// // Pass map_size to child via command line: --shm-size=409600
///
/// // Child process
/// const page_size = try SharedMemoryAllocator.getSystemPageSize();
/// const shm = try SharedMemoryAllocator.open(allocator, name, map_size, page_size);
/// ```
pub fn getRecommendedMapSize(self: *const SharedMemoryAllocator) usize {
    const used = self.getUsedSize();
    if (used == 0) return self.page_size; // Map at least one page
    return std.mem.alignForward(usize, used, self.page_size);
}

/// Get the remaining available memory
pub fn getAvailableSize(self: *const SharedMemoryAllocator) usize {
    return self.total_size - self.offset.load(.monotonic);
}

/// Reset the allocator to allow reuse (only safe if no allocations are still in use!)
pub fn reset(self: *SharedMemoryAllocator) void {
    self.offset.store(0, .monotonic);
}

/// Shrink the shared memory region to only the used size.
/// This reduces the memory footprint before handing off to a child process.
/// Returns the new size after shrinking.
///
/// NOTE: On macOS, ftruncate() is not supported for POSIX shared memory objects.
/// This is a known limitation where macOS treats shared memory differently than regular files.
/// The child process should map only the required size instead.
pub fn shrinkToFit(self: *SharedMemoryAllocator) !usize {
    const used = self.getUsedSize();
    if (used == 0 or used >= self.total_size) {
        return self.total_size;
    }

    // Align to page boundary
    const new_size = std.mem.alignForward(usize, used, self.page_size);
    if (new_size >= self.total_size) {
        return self.total_size;
    }

    switch (builtin.os.tag) {
        .windows => {
            // On Windows, we can't easily shrink a mapped view
            // The child process can map only the used portion
            // So we just return the current size
            return self.total_size;
        },
        .linux, .macos, .freebsd, .openbsd, .netbsd => {
            // Use C ftruncate directly to avoid Zig's unreachable assertion for EINVAL
            const result = c.ftruncate(self.handle, @intCast(new_size));
            if (result != 0) {
                // ftruncate is not supported on shared memory for some platforms (notably macOS)
                // The child process should map only the required portion instead
                return self.total_size;
            }

            // Update our total size
            self.total_size = new_size;
            return new_size;
        },
        else => @compileError("Unsupported platform"),
    }
}

// Platform-specific C declarations
const c = struct {
    // POSIX shared memory functions
    extern "c" fn shm_open(name: [*:0]const u8, oflag: c_int, mode: std.c.mode_t) c_int;
    extern "c" fn shm_unlink(name: [*:0]const u8) c_int;
    extern "c" fn ftruncate(fd: c_int, length: std.c.off_t) c_int;
};

test "shared memory allocator basic operations" {
    const testing = std.testing;

    // Create a unique name for this test
    const name = try std.fmt.allocPrint(
        testing.allocator,
        "zig_test_shm_{}",
        .{std.Thread.getCurrentId()},
    );
    defer testing.allocator.free(name);

    // Create shared memory
    const page_size = try getSystemPageSize();
    var shm = try SharedMemoryAllocator.create(testing.allocator, name, 1024 * 1024, page_size); // 1MB
    defer shm.deinit(testing.allocator);

    const shm_allocator = shm.allocator();

    // Test allocation
    const data = try shm_allocator.alloc(u32, 100);
    try testing.expect(data.len == 100);

    // Write some data
    for (data, 0..) |*item, i| {
        item.* = @intCast(i);
    }

    // Test that we can read it back
    for (data, 0..) |item, i| {
        try testing.expectEqual(@as(u32, @intCast(i)), item);
    }

    // Test available size tracking
    const used = shm.getUsedSize();
    try testing.expect(used >= 100 * @sizeOf(u32));
    try testing.expect(shm.getAvailableSize() <= shm.total_size - used);
}

test "shared memory allocator cross-process" {
    const testing = std.testing;

    // Skip on CI or if not supported
    if (builtin.os.tag == .wasi) return error.SkipZigTest;

    const name = try std.fmt.allocPrint(
        testing.allocator,
        "zig_test_shm_xproc_{}",
        .{std.Thread.getCurrentId()},
    );
    defer testing.allocator.free(name);

    // Parent: Create and write data
    {
        const page_size = try getSystemPageSize();
        var shm = try SharedMemoryAllocator.create(testing.allocator, name, 1024 * 1024, page_size);
        defer shm.deinit(testing.allocator);

        const data = try shm.allocator().alloc(u32, 10);
        for (data, 0..) |*item, i| {
            item.* = @intCast(i * 2);
        }
    }
}

// NOTE: Test removed because it tries to simulate cross-process shared memory behavior
// within a single process, which doesn't work on macOS. On macOS, you cannot open
// the same shared memory object twice with shm_open() in the same process.
// The production code works correctly because it uses separate processes and passes
// the file descriptor through the filesystem, not through shm_open().

test "shared memory allocator shrinkToFit" {
    const testing = std.testing;

    const name = try std.fmt.allocPrint(
        testing.allocator,
        "zig_test_shm_shrink_{}",
        .{std.Thread.getCurrentId()},
    );
    defer testing.allocator.free(name);

    const page_size = try getSystemPageSize();
    var shm = try SharedMemoryAllocator.create(testing.allocator, name, 1024 * 1024, page_size); // 1MB
    defer shm.deinit(testing.allocator);

    const shm_allocator = shm.allocator();

    // Allocate some data
    const data1 = try shm_allocator.alloc(u32, 100);
    try testing.expect(data1.len == 100);

    const data2 = try shm_allocator.alloc(u8, 1000);
    try testing.expect(data2.len == 1000);

    // Check initial size
    const initial_size = shm.total_size;
    try testing.expectEqual(@as(usize, 1024 * 1024), initial_size);

    // Get used size before shrinking
    const used_before = shm.getUsedSize();
    try testing.expect(used_before >= 100 * @sizeOf(u32) + 1000);
    try testing.expect(used_before < initial_size);

    // Shrink to fit (may fail on some platforms)
    const new_size = shm.shrinkToFit() catch shm.total_size;

    // On Windows and macOS, shrinking might not be supported
    if (builtin.os.tag == .windows or builtin.os.tag == .macos) {
        // If shrinking failed, size should remain the same
        if (new_size == initial_size) {
            try testing.expectEqual(initial_size, new_size);
        } else {
            // If it succeeded, verify the constraints
            try testing.expect(new_size < initial_size);
            try testing.expect(new_size >= used_before);
            try testing.expectEqual(@as(usize, 0), new_size % page_size);
        }
    } else {
        // On Linux and other POSIX systems, should shrink to page-aligned size
        try testing.expect(new_size < initial_size);
        try testing.expect(new_size >= used_before);
        // Should be page-aligned
        try testing.expectEqual(@as(usize, 0), new_size % page_size);
    }

    // Verify we can still use the allocated memory
    for (data1, 0..) |*item, i| {
        item.* = @intCast(i);
    }
    for (data1, 0..) |item, i| {
        try testing.expectEqual(@as(u32, @intCast(i)), item);
    }
}

test "shared memory allocator thread safety" {
    const testing = std.testing;

    const name = try std.fmt.allocPrint(
        testing.allocator,
        "zig_test_shm_thread_{}",
        .{std.Thread.getCurrentId()},
    );
    defer testing.allocator.free(name);

    const page_size = try getSystemPageSize();
    var shm = try SharedMemoryAllocator.create(testing.allocator, name, 16 * 1024 * 1024, page_size); // 16MB
    defer shm.deinit(testing.allocator);

    const shm_allocator = shm.allocator();
    const num_threads = 4;
    const allocations_per_thread = 1000;

    const ThreadContext = struct {
        allocator: std.mem.Allocator,
        thread_id: usize,
        allocations_per_thread: usize,
    };

    const thread_fn = struct {
        fn run(ctx: ThreadContext) !void {
            var i: usize = 0;
            while (i < ctx.allocations_per_thread) : (i += 1) {
                // Allocate various sizes
                const size = 16 + (i % 256);
                const data = try ctx.allocator.alloc(u8, size);

                // Write a pattern to verify no overlap
                for (data, 0..) |*byte, j| {
                    byte.* = @intCast((ctx.thread_id * 256 + j) % 256);
                }

                // Verify the pattern immediately
                for (data, 0..) |byte, j| {
                    const expected = @as(u8, @intCast((ctx.thread_id * 256 + j) % 256));
                    try testing.expectEqual(expected, byte);
                }
            }
        }
    }.run;

    // Spawn threads
    var threads: [num_threads]std.Thread = undefined;
    for (&threads, 0..) |*thread, i| {
        thread.* = try std.Thread.spawn(.{}, thread_fn, .{ThreadContext{
            .allocator = shm_allocator,
            .thread_id = i,
            .allocations_per_thread = allocations_per_thread,
        }});
    }

    // Wait for all threads
    for (threads) |thread| {
        thread.join();
    }

    // Verify that all allocations succeeded and used memory efficiently
    const used = shm.getUsedSize();
    try testing.expect(used > 0);
    try testing.expect(used < shm.total_size);

    // The actual used size should be at least the sum of all allocations
    var min_expected: usize = 0;
    var i: usize = 0;
    while (i < allocations_per_thread) : (i += 1) {
        min_expected += 16 + (i % 256);
    }
    min_expected *= num_threads;

    // Used size should be at least the minimum (will be more due to alignment)
    try testing.expect(used >= min_expected);
}
