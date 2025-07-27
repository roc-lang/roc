const std = @import("std");

pub const CompactWriter = struct {
    pub const ALIGNMENT = 16; // Buffer alignment requirement for deserialization

    const ZEROS: [16]u8 = [_]u8{0} ** 16;

    iovecs: std.ArrayListUnmanaged(Iovec),
    total_bytes: usize,

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

        while (bytes_written < total_size) {
            // Create adjusted iovec array for partial writes
            const remaining_iovecs = self.iovecs.items.len - current_iovec;
            var adjusted_iovecs = try allocator.alloc(std.posix.iovec_const, remaining_iovecs);
            defer allocator.free(adjusted_iovecs);

            // Copy remaining iovecs, adjusting first one for partial write
            for (self.iovecs.items[current_iovec..], 0..) |iovec, j| {
                if (j == 0 and iovec_offset > 0) {
                    // Adjust first iovec for partial write
                    adjusted_iovecs[j] = .{
                        .base = @ptrFromInt(@intFromPtr(iovec.iov_base) + iovec_offset),
                        .len = iovec.iov_len - iovec_offset,
                    };
                } else {
                    adjusted_iovecs[j] = .{
                        .base = iovec.iov_base,
                        .len = iovec.iov_len,
                    };
                }
            }

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
        std.debug.assert(alignment <= ALIGNMENT);

        // Pad up front to the alignment of T
        try self.padToAlignment(allocator, alignment);

        // Allocate a single item of type T
        const items = try allocator.alignedAlloc(T, alignment, 1);
        const answer = &items[0];

        // Add the pointer to uninitialized memory to the iovecs for later.
        try self.iovecs.append(allocator, .{
            .iov_base = @ptrCast(@as([*]u8, @ptrCast(answer))),
            .iov_len = size,
        });
        self.total_bytes += size;

        return answer;
    }

    pub fn appendSlice(
        self: *@This(),
        allocator: std.mem.Allocator,
        ptr: anytype,
        len: usize,
    ) std.mem.Allocator.Error!usize {
        const T = @typeInfo(@TypeOf(ptr)).pointer.child;
        const size = @sizeOf(T);
        const alignment = @alignOf(T);

        // Pad up front to the alignment of T
        try self.padToAlignment(allocator, alignment);

        try self.iovecs.append(allocator, .{
            .iov_base = @ptrCast(@as([*]const u8, @ptrCast(ptr))),
            .iov_len = size * len,
        });
        self.total_bytes += size * len;

        return self.total_bytes;
    }

    fn padToAlignment(self: *@This(), allocator: std.mem.Allocator, alignment: usize) std.mem.Allocator.Error!void {
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
};

const Iovec = extern struct {
    iov_base: [*]const u8,
    iov_len: usize,
};
