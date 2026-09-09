//! Shared size-prefix allocator core used by runtime and generated-ABI test hosts.
const std = @import("std");

/// Bytes reserved before the user data for the stored total size. The prefix
/// is at least `alignment` bytes so the user data keeps its alignment, and at
/// least `@alignOf(usize)` bytes so the size store/load stays aligned.
pub fn sizeStorageBytes(alignment: usize) usize {
    return @max(alignment, @alignOf(usize));
}

/// The alignment the backing allocation is made with: the requested alignment,
/// raised to `@alignOf(usize)` so the size prefix is aligned.
pub fn backingAlignment(alignment: usize) std.mem.Alignment {
    return std.mem.Alignment.fromByteUnits(@max(alignment, @alignOf(usize)));
}

/// Total size (prefix included) stored for the live allocation at `ptr`.
pub fn storedTotalSize(ptr: *const anyopaque) usize {
    const size_ptr: *const usize = @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize));
    return size_ptr.*;
}

/// The backing allocation's base pointer for the user allocation at `ptr`.
pub fn basePtr(ptr: *anyopaque, alignment: usize) [*]u8 {
    return @ptrFromInt(@intFromPtr(ptr) - sizeStorageBytes(alignment));
}

/// Allocate `length` bytes aligned to `alignment` with the size prefix filled
/// in, returning null on OOM.
pub fn alloc(backing: std.mem.Allocator, length: usize, alignment: usize) ?*anyopaque {
    const size_storage_bytes = sizeStorageBytes(alignment);
    const total_size = length + size_storage_bytes;

    const base_ptr = backing.rawAlloc(total_size, backingAlignment(alignment), @returnAddress()) orelse
        return null;

    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;

    const answer: *anyopaque = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);
    std.debug.assert(@intFromPtr(answer) % @max(alignment, 1) == 0);
    return answer;
}

/// Free the allocation at `ptr` using its stored total size. (On an arena
/// backing this is effectively a no-op, which is exactly what arena-based
/// hosts want.)
pub fn dealloc(backing: std.mem.Allocator, ptr: *anyopaque, alignment: usize) void {
    const total_size = storedTotalSize(ptr);
    const base_ptr = basePtr(ptr, alignment);
    backing.rawFree(base_ptr[0..total_size], backingAlignment(alignment), @returnAddress());
}

/// Reallocate the allocation at `ptr` to `new_length` bytes, returning null on
/// OOM (in which case the old allocation stays live).
pub fn realloc(backing: std.mem.Allocator, ptr: *anyopaque, new_length: usize, alignment: usize) ?*anyopaque {
    const size_storage_bytes = sizeStorageBytes(alignment);
    const old_total_size = storedTotalSize(ptr);
    const old_base_ptr = basePtr(ptr, alignment);
    const new_total_size = new_length + size_storage_bytes;

    const new_base_ptr = backing.rawAlloc(new_total_size, backingAlignment(alignment), @returnAddress()) orelse
        return null;

    const copy_size = @min(old_total_size, new_total_size);
    @memcpy(new_base_ptr[0..copy_size], old_base_ptr[0..copy_size]);

    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_base_ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;

    backing.rawFree(old_base_ptr[0..old_total_size], backingAlignment(alignment), @returnAddress());

    const answer: *anyopaque = @ptrFromInt(@intFromPtr(new_base_ptr) + size_storage_bytes);
    return answer;
}
