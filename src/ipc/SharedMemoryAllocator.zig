//! Shared memory allocator for sharing data between parent and child processes.
//! This is used to efficiently transfer compiler data structures to a host binary
//! which has been built with a shim that lets it communicate efficiently with the
//! compiler via shared memory.
//!
//! This allocator maps a large anonymous virtual address region (512MB on 64-bit systems,
//! 256MB on 32-bit systems). The size is chosen to be large enough that needing to
//! resize it should never come up in practice, since coordinating resizing with the
//! child process would be complex.
//!
//! ## Cross-platform coordination
//!
//! The allocator uses platform-specific coordination mechanisms:
//!
//! **Windows**: Passes the shared memory handle and size via command line arguments
//! to the child process. Uses anonymous file mapping objects with handle inheritance.
//!
//! **POSIX (Linux/macOS/BSD)**: Creates a coordination file next to the child executable
//! containing the file descriptor and size.
//!
//! An important detail is that it provides access to the child process via a file
//! descriptor rather than using named shared memory. As it turns out, macOS has a
//! security restriction where if a process creates an executable on disk (which Roc's
//! compiler does), and then runs that executable, the child process is not allowed
//! to call shm_open(). (If it tries to, shm_open always fails with errno 13.) If the
//! parent process instead gives a fd to the child for the shared memory, the child
//! process is allowed to use that to map in the shared memory and access it that way.
//!
//! The coordination logic is handled by `src/ipc/coordination.zig`, while the
//! low-level platform operations are abstracted in `src/ipc/platform.zig`.

const std = @import("std");
const platform = @import("platform.zig");
const coordination = @import("coordination.zig");

const SharedMemoryAllocator = @This();

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
/// Whether this allocator owns the shared memory (should clean up)
is_owner: bool,
/// Page size for this system
page_size: usize,

const Handle = platform.Handle;

/// Get the system's page size at runtime
pub fn getSystemPageSize() !usize {
    return platform.getSystemPageSize();
}

/// Creates a new anonymous shared memory region with the given size
pub fn create(size: usize, page_size: usize) !SharedMemoryAllocator {
    const aligned_size = std.mem.alignForward(usize, size, page_size);

    // Create the shared memory mapping
    const handle = try platform.createMapping(aligned_size);
    errdefer platform.closeHandle(handle, true);

    // Map the memory
    const base_ptr = try platform.mapMemory(handle, aligned_size, platform.SHARED_MEMORY_BASE_ADDR);
    errdefer platform.unmapMemory(base_ptr, aligned_size);

    // On Windows with SEC_RESERVE, we must commit pages before accessing them.
    // Commit the first page for the header before we write to it.
    if (comptime platform.is_windows) {
        const commit_result = platform.windows.VirtualAlloc(
            base_ptr,
            @sizeOf(Header),
            platform.windows.MEM_COMMIT,
            platform.windows.PAGE_READWRITE,
        );
        if (commit_result == null) {
            platform.unmapMemory(base_ptr, aligned_size);
            platform.closeHandle(handle, true);
            return error.OutOfMemory;
        }
    }

    const result = SharedMemoryAllocator{
        .handle = handle,
        .base_ptr = @ptrCast(@alignCast(base_ptr)),
        .total_size = aligned_size,
        .offset = std.atomic.Value(usize).init(@sizeOf(Header)), // Start after header
        .is_owner = true,
        .page_size = page_size,
    };

    // Initialize header
    const header_ptr = @as(*Header, @ptrCast(@alignCast(result.base_ptr)));
    header_ptr.* = Header{
        .total_size = aligned_size,
    };

    return result;
}

/// Opens an existing shared memory region by reading its header first.
/// This function will map only the required amount of memory as specified in the header.
pub fn openWithHeader(gpa: std.mem.Allocator, name: []const u8, page_size: usize) !SharedMemoryAllocator {
    // Open the named shared memory
    const handle = try platform.openMapping(gpa, name);
    errdefer platform.closeHandle(handle, false);

    // First map just the header
    const header_ptr = try platform.mapMemory(handle, @sizeOf(Header), platform.SHARED_MEMORY_BASE_ADDR);
    const header = @as(*const Header, @ptrCast(@alignCast(header_ptr))).*;
    platform.unmapMemory(header_ptr, @sizeOf(Header));

    if (header.magic != 0x524F4353) {
        return error.InvalidSharedMemory;
    }

    // Now map the actual size from the header
    const actual_size = @as(usize, @intCast(header.used_size));
    const base_ptr = try platform.mapMemory(handle, actual_size, platform.SHARED_MEMORY_BASE_ADDR);
    errdefer platform.unmapMemory(base_ptr, actual_size);

    return SharedMemoryAllocator{
        .handle = handle,
        .base_ptr = @ptrCast(@alignCast(base_ptr)),
        .total_size = actual_size,
        .offset = std.atomic.Value(usize).init(@as(usize, @intCast(header.data_offset))),
        .is_owner = false,
        .page_size = page_size,
    };
}

/// Opens an existing shared memory region created by another process.
///
/// IMPORTANT: The `size` parameter should be the actual used size from the parent
/// process (obtained via getRecommendedMapSize()), NOT the original allocated size.
/// This is especially important on macOS where the shared memory object remains at
/// its original size and cannot be truncated.
pub fn open(gpa: std.mem.Allocator, name: []const u8, size: usize, page_size: usize) !SharedMemoryAllocator {
    const aligned_size = std.mem.alignForward(usize, size, page_size);

    // Open the named shared memory
    const handle = try platform.openMapping(gpa, name);
    errdefer platform.closeHandle(handle, false);

    // Map the memory
    const base_ptr = try platform.mapMemory(handle, aligned_size, platform.SHARED_MEMORY_BASE_ADDR);
    errdefer platform.unmapMemory(base_ptr, aligned_size);

    return SharedMemoryAllocator{
        .handle = handle,
        .base_ptr = @ptrCast(@alignCast(base_ptr)),
        .total_size = aligned_size,
        .offset = std.atomic.Value(usize).init(@sizeOf(Header)),
        .is_owner = false,
        .page_size = page_size,
    };
}

/// Creates a SharedMemoryAllocator from coordination info.
/// This is a convenience method for child processes that reads coordination info
/// and creates the allocator in one step.
pub fn fromCoordination(gpa: std.mem.Allocator, page_size: usize) !SharedMemoryAllocator {
    // Read coordination info
    var fd_info = try coordination.readFdInfo(gpa);
    defer fd_info.deinit(gpa);

    // Parse the handle and create the allocator
    const handle = try coordination.parseHandle(fd_info.fd_str);
    return fromFd(handle, fd_info.size, page_size);
}

/// Creates a SharedMemoryAllocator from an existing file descriptor.
/// This is used by child processes to access shared memory created by the parent.
pub fn fromFd(fd: Handle, size: usize, page_size: usize) !SharedMemoryAllocator {
    const aligned_size = std.mem.alignForward(usize, size, page_size);

    // Map the memory using the provided handle
    const base_ptr = try platform.mapMemory(fd, aligned_size, platform.SHARED_MEMORY_BASE_ADDR);
    errdefer platform.unmapMemory(base_ptr, aligned_size);

    return SharedMemoryAllocator{
        .handle = fd,
        .base_ptr = @ptrCast(@alignCast(base_ptr)),
        .total_size = aligned_size,
        .offset = std.atomic.Value(usize).init(@sizeOf(Header)),
        .is_owner = false,
        .page_size = page_size,
    };
}

/// Updates the header with the current used size.
/// Should be called before handing off to child process.
pub fn updateHeader(self: *SharedMemoryAllocator) void {
    const header_ptr = @as(*Header, @ptrCast(@alignCast(self.base_ptr)));
    header_ptr.used_size = self.getUsedSize();
}

/// Deinitializes the shared memory allocator
pub fn deinit(self: *SharedMemoryAllocator, gpa: std.mem.Allocator) void {
    _ = gpa; // No longer needed since we don't store the name
    // Update header before closing
    if (self.is_owner) {
        self.updateHeader();
    }
    platform.unmapMemory(self.base_ptr, self.total_size);
    platform.closeHandle(self.handle, self.is_owner);
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

fn alloc(ctx: *anyopaque, len: usize, ptr_align: std.mem.Alignment, _: usize) ?[*]u8 {
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

            // On Windows, pages are reserved but not committed (due to SEC_RESERVE).
            // We must commit pages before they can be accessed.
            if (comptime platform.is_windows) {
                // Commit the pages for this allocation. VirtualAlloc with MEM_COMMIT
                // on already-reserved pages commits them without requiring a new reservation.
                // We commit only the pages needed for this allocation.
                const commit_result = platform.windows.VirtualAlloc(
                    @ptrCast(ptr),
                    len,
                    platform.windows.MEM_COMMIT,
                    platform.windows.PAGE_READWRITE,
                );
                if (commit_result == null) {
                    // Failed to commit memory - this shouldn't happen normally
                    // but could occur if the system is truly out of memory
                    return null;
                }
            }

            return @ptrCast(ptr);
        }
        // CAS failed, another thread allocated - retry with new offset
    }
}

fn resize(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, new_len: usize, _: usize) bool {
    const self: *SharedMemoryAllocator = @ptrCast(@alignCast(ctx));
    const buf_ptr = @intFromPtr(buf.ptr);
    const base = @intFromPtr(self.base_ptr);

    // Check if this is the last allocation by seeing if buf ends at the current offset
    const buf_end = buf_ptr + buf.len;
    const current_offset = self.offset.load(.monotonic);
    const current_end = base + current_offset;

    if (buf_end == current_end) {
        // This is the last allocation, we can resize in place
        if (new_len <= buf.len) {
            // Shrinking - just update the offset
            const shrink_amount = buf.len - new_len;
            _ = self.offset.fetchSub(shrink_amount, .monotonic);
            return true;
        } else {
            // Growing - check if we have room
            const grow_amount = new_len - buf.len;
            const new_end_offset = current_offset + grow_amount;
            if (new_end_offset <= self.total_size) {
                // We have room, extend in place using compare-and-swap
                while (true) {
                    const old_offset = self.offset.load(.monotonic);
                    const old_end = base + old_offset;
                    if (buf_end != old_end) {
                        // Another allocation happened, can't resize in place
                        return false;
                    }
                    if (self.offset.cmpxchgWeak(
                        old_offset,
                        old_offset + grow_amount,
                        .monotonic,
                        .monotonic,
                    ) == null) {
                        // On Windows, commit the additional pages for the growth
                        if (comptime platform.is_windows) {
                            const grow_ptr = self.base_ptr + old_offset;
                            const commit_result = platform.windows.VirtualAlloc(
                                @ptrCast(grow_ptr),
                                grow_amount,
                                platform.windows.MEM_COMMIT,
                                platform.windows.PAGE_READWRITE,
                            );
                            if (commit_result == null) {
                                // Failed to commit - rollback the offset change
                                _ = self.offset.fetchSub(grow_amount, .monotonic);
                                return false;
                            }
                        }
                        return true;
                    }
                    // CAS failed, retry
                }
            }
        }
    }

    // Shrinking is always safe for any allocation
    if (new_len <= buf.len) {
        // For non-last allocations, just report success for shrinking
        // The extra space becomes wasted, but that's unavoidable
        _ = alignment; // Suppress unused warning
        return true;
    }

    // Can't grow a non-last allocation
    return false;
}

fn free(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize) void {
    // Simple bump allocator doesn't support free
    // Memory is only freed when the entire region is unmapped
}

fn remap(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, new_len: usize, return_address: usize) ?[*]u8 {
    // Try to resize in place first
    if (resize(ctx, buf, alignment, new_len, return_address)) {
        return buf.ptr;
    }
    // Can't remap - caller will allocate new, copy, and free (which is a no-op)
    return null;
}

/// Get the current amount of used memory
pub fn getUsedSize(self: *const SharedMemoryAllocator) usize {
    return self.offset.load(.monotonic);
}

/// Get the recommended size for a child process to map.
/// This is the used size aligned to page boundaries.
///
/// IMPORTANT: The parent process MUST communicate this size to the child process
/// (e.g., via command line arguments or environment variables). The child should
/// then use this size when calling open() to map only what's needed.
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

/// Get the platform handle for this shared memory
/// Useful for child processes that need to manage the handle directly
pub fn getHandle(self: *const SharedMemoryAllocator) Handle {
    return self.handle;
}

/// Get the base pointer for this shared memory
pub fn getBasePtr(self: *const SharedMemoryAllocator) [*]align(1) u8 {
    return self.base_ptr;
}

/// Reset the allocator to allow reuse (only safe if no allocations are still in use!)
pub fn reset(self: *SharedMemoryAllocator) void {
    self.offset.store(0, .monotonic);
}

test "shared memory allocator basic operations" {
    const testing = std.testing;

    // Create shared memory
    const page_size = try getSystemPageSize();
    var shm = try SharedMemoryAllocator.create(1024 * 1024, page_size); // 1MB
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

    // Parent: Create and write data
    {
        const page_size = try getSystemPageSize();
        var shm = try SharedMemoryAllocator.create(1024 * 1024, page_size);
        defer shm.deinit(testing.allocator);

        const data = try shm.allocator().alloc(u32, 10);
        for (data, 0..) |*item, i| {
            item.* = @intCast(i * 2);
        }
    }
}

test "shared memory allocator thread safety" {
    const testing = std.testing;

    const page_size = try getSystemPageSize();
    var shm = try SharedMemoryAllocator.create(16 * 1024 * 1024, page_size); // 16MB
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
