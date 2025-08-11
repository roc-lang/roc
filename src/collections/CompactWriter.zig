//! CompactWriter provides efficient serialization using scatter-gather I/O operations.
//! It collects multiple memory regions into iovecs and writes them in a single system call
//! using pwritev, minimizing system call overhead for serialization tasks.
//! The writer handles alignment requirements and padding automatically to ensure
//! proper deserialization of the written data.

const std = @import("std");

const CompactWriter = @This();

/// The alignment requirement for buffers used in deserialization.
/// All serialized data must be aligned to this boundary to ensure proper
/// memory access patterns and avoid alignment faults on architectures that
/// require aligned memory access.
pub const SERIALIZATION_ALIGNMENT = 16;

const ZEROS: [16]u8 = [_]u8{0} ** 16;

iovecs: std.ArrayListUnmanaged(Iovec),
total_bytes: usize,
// Track all allocated memory so we can free it in deinit
allocated_memory: std.ArrayListUnmanaged(AllocatedMemory),

pub fn init() CompactWriter {
    return CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
        .allocated_memory = .{},
    };
}

/// Does a pwritev() on UNIX systems.
/// There is no usable equivalent of this on Windows
/// (WriteFileGather has ludicrous alignment requirements that make it useless),
/// so Windows must call
pub fn writeGather(
    self: *@This(),
    allocator: std.mem.Allocator,
    file: std.fs.File,
) !void {
    // Handle partial writes (where pwritev returns that it only wrote some of the bytes)
    var bytes_written: usize = 0;
    var current_iovec: usize = 0;
    var iovec_offset: usize = 0;
    const total_size = self.total_bytes;

    // Early return if nothing to write
    if (total_size == 0 or self.iovecs.items.len == 0) return;

    while (bytes_written < total_size) {
        // Skip any iovecs that have been completely written
        while (current_iovec < self.iovecs.items.len and
            iovec_offset >= self.iovecs.items[current_iovec].iov_len)
        {
            current_iovec += 1;
            iovec_offset = 0;
        }

        // Check if we've processed all iovecs
        if (current_iovec >= self.iovecs.items.len) break;

        // Count valid remaining iovecs (those with data to write)
        var valid_iovec_count: usize = 0;
        for (self.iovecs.items[current_iovec..], 0..) |iovec, j| {
            const offset = if (j == 0) iovec_offset else 0;
            if (iovec.iov_len > offset) {
                valid_iovec_count += 1;
            }
        }

        if (valid_iovec_count == 0) break;

        // Create adjusted iovec array for partial writes
        var adjusted_iovecs = try allocator.alloc(std.posix.iovec_const, valid_iovec_count);
        defer allocator.free(adjusted_iovecs);

        // Copy remaining iovecs, adjusting first one for partial write and filtering out empty ones
        var adjusted_index: usize = 0;
        for (self.iovecs.items[current_iovec..], 0..) |iovec, j| {
            const offset = if (j == 0) iovec_offset else 0;

            // Skip iovecs that have no remaining data
            if (iovec.iov_len <= offset) continue;

            // Handle potential null pointer when adding offset
            const base_addr = @intFromPtr(iovec.iov_base);
            const new_base = if (base_addr == 0 and offset == 0)
                iovec.iov_base // Keep null if already null
            else if (base_addr == 0)
                @as([*]const u8, @ptrFromInt(offset)) // This shouldn't happen, but handle it
            else
                @as([*]const u8, @ptrFromInt(base_addr + offset));

            adjusted_iovecs[adjusted_index] = .{
                .base = new_base,
                .len = iovec.iov_len - offset,
            };
            adjusted_index += 1;
        }

        // Sanity check - we should have filled all slots
        std.debug.assert(adjusted_index == valid_iovec_count);

        const n = try std.posix.pwritev(file.handle, adjusted_iovecs, bytes_written);

        if (n == 0) return error.UnexpectedEof;

        // Update position tracking
        bytes_written += n;
        var remaining = n;

        // Figure out where we are now
        while (remaining > 0 and current_iovec < self.iovecs.items.len) {
            const iovec_remaining = self.iovecs.items[current_iovec].iov_len - iovec_offset;
            if (remaining >= iovec_remaining) {
                remaining -= iovec_remaining;
                current_iovec += 1;
                iovec_offset = 0;
            } else {
                iovec_offset += remaining;
                remaining = 0;
            }
        }
    }
}

/// Allocates some undefined memory with the same size and alignment as the given value,
/// appends (a pointer to) that memory to the writer, and returns the pointer.
///
/// Since this is returning a pointer to uninitialized memory, it's up to the caller to
/// mutate it in-place to turn its nested pointers' memory addresses into offsets for serialization.
///
/// Note: Padding is added BEFORE the data to ensure proper alignment for the type.
pub fn appendAlloc(
    self: *@This(),
    allocator: std.mem.Allocator,
    comptime T: type,
) std.mem.Allocator.Error!*T {
    const size = @sizeOf(T);
    const alignment = @alignOf(T);

    // When we deserialize, we align the bytes we're deserializing into to ALIGNMENT,
    // which means that we can't serialize anything with alignment higher than that.
    std.debug.assert(alignment <= SERIALIZATION_ALIGNMENT);

    // Pad up front to the alignment of T
    try self.padToAlignment(allocator, alignment);

    // Allocate a single item of type T
    const items = try allocator.alignedAlloc(T, alignment, 1);
    const answer = &items[0];

    // Track the allocated memory for cleanup
    try self.allocated_memory.append(allocator, .{
        .ptr = @as([*]u8, @ptrCast(answer)),
        .size = size,
        .alignment = alignment,
    });

    // Add the pointer to uninitialized memory to the iovecs for later.
    try self.iovecs.append(allocator, .{
        .iov_base = @ptrCast(@as([*]u8, @ptrCast(answer))),
        .iov_len = size,
    });
    self.total_bytes += size;

    return answer;
}

/// Never call this as the first append in the writer (e.g. always call appendAlloc first),
/// because that will result in this attempting to return a slice with an offset of 0,
/// which will be interpreted by Zig as an attempt to have a slice with a null pointer.
/// This is not allowed, and so will cause a panic in debug builds.
/// (In practice, this should never happen because we always write a struct as the very
/// first write in the writer, never an array.)
pub fn appendSlice(
    self: *@This(),
    allocator: std.mem.Allocator,
    slice: anytype,
) std.mem.Allocator.Error!@TypeOf(slice) {
    const SliceType = @TypeOf(slice);
    const info = @typeInfo(SliceType);
    const T = if (info == .pointer and info.pointer.size == .one)
        std.meta.Child(std.meta.Child(SliceType))
    else
        std.meta.Child(SliceType);
    const size = @sizeOf(T);
    const alignment = @alignOf(T);
    const len = slice.len;

    // Pad up front to the alignment of T
    try self.padToAlignment(allocator, alignment);

    const offset = self.total_bytes;

    try self.iovecs.append(allocator, .{
        .iov_base = @ptrCast(@as([*]const u8, @ptrCast(slice.ptr))),
        .iov_len = size * len,
    });
    self.total_bytes += size * len;

    // Return the same slice type as the input
    const result = if (info.pointer.is_const)
        @as([*]const T, @ptrFromInt(offset))[0..len]
    else
        @as([*]T, @ptrFromInt(offset))[0..len];

    return result;
}

/// Adds padding bytes to ensure the next write will be aligned to the specified boundary.
/// This is critical for ensuring that serialized data structures maintain their required
/// alignment when written to the output buffer.
pub fn padToAlignment(self: *@This(), allocator: std.mem.Allocator, alignment: usize) std.mem.Allocator.Error!void {
    const padding_bytes_needed = std.mem.alignForward(usize, self.total_bytes, alignment) - self.total_bytes;

    if (padding_bytes_needed > 0) {
        try self.iovecs.append(allocator, .{
            .iov_base = @ptrCast(@as([*]const u8, &ZEROS)),
            .iov_len = padding_bytes_needed,
        });
        self.total_bytes += padding_bytes_needed;
    }
}

/// Write all iovecs to a single contiguous buffer for testing purposes.
/// Returns the slice of buffer that was written to.
pub fn writeToBuffer(
    self: *@This(),
    buffer: []u8,
) ![]u8 {
    if (buffer.len < self.total_bytes) {
        return error.BufferTooSmall;
    }

    var offset: usize = 0;
    for (self.iovecs.items) |iovec| {
        @memcpy(buffer[offset..][0..iovec.iov_len], iovec.iov_base[0..iovec.iov_len]);
        offset += iovec.iov_len;
    }

    return buffer[0..self.total_bytes];
}

/// Deinitialize the CompactWriter, freeing all allocated memory
pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
    // Free all allocated memory slices
    for (self.allocated_memory.items) |memory_slice| {
        const slice = memory_slice.ptr[0..memory_slice.size];
        const alignment_log2 = std.math.log2_int(usize, memory_slice.alignment);
        const alignment: std.mem.Alignment = @enumFromInt(alignment_log2);
        allocator.rawFree(slice, alignment, @returnAddress());
    }
    self.allocated_memory.deinit(allocator);
    self.iovecs.deinit(allocator);
}

const Iovec = extern struct {
    iov_base: [*]const u8,
    iov_len: usize,
};

const AllocatedMemory = struct {
    ptr: [*]u8,
    size: usize,
    alignment: usize,
};
