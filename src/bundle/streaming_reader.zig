const std = @import("std");
const c = @cImport({
    @cDefine("ZSTD_STATIC_LINKING_ONLY", "1");
    @cInclude("zstd.h");
});

/// A reader that decompresses zstd data and verifies hash incrementally
pub const DecompressingHashReader = struct {
    allocator_ptr: *std.mem.Allocator,
    dctx: *c.ZSTD_DCtx,
    hasher: std.crypto.hash.Blake3,
    input_reader: std.io.AnyReader,
    expected_hash: [32]u8,
    in_buffer: []u8,
    out_buffer: []u8,
    out_pos: usize,
    out_end: usize,
    finished: bool,
    hash_verified: bool,

    const Self = @This();
    const Reader = std.io.Reader(*Self, Error, read);
    const Error = error{
        DecompressionFailed,
        UnexpectedEndOfStream,
        HashMismatch,
    } || std.mem.Allocator.Error;

    pub fn init(
        allocator_ptr: *std.mem.Allocator,
        input_reader: std.io.AnyReader,
        expected_hash: [32]u8,
        allocForZstd: *const fn (?*anyopaque, usize) callconv(.C) ?*anyopaque,
        freeForZstd: *const fn (?*anyopaque, ?*anyopaque) callconv(.C) void,
    ) !Self {
        const custom_mem = c.ZSTD_customMem{
            .customAlloc = allocForZstd,
            .customFree = freeForZstd,
            .@"opaque" = @ptrCast(allocator_ptr),
        };

        const dctx = c.ZSTD_createDCtx_advanced(custom_mem) orelse return std.mem.Allocator.Error.OutOfMemory;
        errdefer _ = c.ZSTD_freeDCtx(dctx);

        const in_buffer_size = c.ZSTD_DStreamInSize();
        const in_buffer = try allocator_ptr.alloc(u8, in_buffer_size);
        errdefer allocator_ptr.free(in_buffer);

        const out_buffer_size = c.ZSTD_DStreamOutSize();
        const out_buffer = try allocator_ptr.alloc(u8, out_buffer_size);
        errdefer allocator_ptr.free(out_buffer);

        return Self{
            .allocator_ptr = allocator_ptr,
            .dctx = dctx,
            .hasher = std.crypto.hash.Blake3.init(.{}),
            .input_reader = input_reader,
            .expected_hash = expected_hash,
            .in_buffer = in_buffer,
            .out_buffer = out_buffer,
            .out_pos = 0,
            .out_end = 0,
            .finished = false,
            .hash_verified = false,
        };
    }

    pub fn deinit(self: *Self) void {
        _ = c.ZSTD_freeDCtx(self.dctx);
        self.allocator_ptr.free(self.in_buffer);
        self.allocator_ptr.free(self.out_buffer);
    }

    pub fn reader(self: *Self) Reader {
        return .{ .context = self };
    }

    fn read(self: *Self, dest: []u8) Error!usize {
        if (dest.len == 0) return 0;

        var total_read: usize = 0;

        while (total_read < dest.len) {
            // If we have data in the output buffer, copy it
            if (self.out_pos < self.out_end) {
                const available = self.out_end - self.out_pos;
                const to_copy = @min(available, dest.len - total_read);
                @memcpy(dest[total_read..][0..to_copy], self.out_buffer[self.out_pos..][0..to_copy]);
                self.out_pos += to_copy;
                total_read += to_copy;

                if (total_read == dest.len) {
                    return total_read;
                }
            }

            // If finished and no more data in buffer, we're done
            if (self.finished) {
                break;
            }

            // Read more compressed data
            const bytes_read = self.input_reader.read(self.in_buffer) catch {
                return error.UnexpectedEndOfStream;
            };

            if (bytes_read == 0) {
                // End of input stream - verify final hash
                if (!self.hash_verified) {
                    var actual_hash: [32]u8 = undefined;
                    self.hasher.final(&actual_hash);
                    if (!std.mem.eql(u8, &actual_hash, &self.expected_hash)) {
                        return error.HashMismatch;
                    }
                    self.hash_verified = true;
                }
                self.finished = true;
                break;
            }

            // Update hash with compressed data
            self.hasher.update(self.in_buffer[0..bytes_read]);

            // Decompress
            var in_buf = c.ZSTD_inBuffer{ .src = self.in_buffer.ptr, .size = bytes_read, .pos = 0 };

            while (in_buf.pos < in_buf.size) {
                var out_buf = c.ZSTD_outBuffer{ .dst = self.out_buffer.ptr, .size = self.out_buffer.len, .pos = 0 };

                const result = c.ZSTD_decompressStream(self.dctx, &out_buf, &in_buf);
                if (c.ZSTD_isError(result) != 0) {
                    return error.DecompressionFailed;
                }

                if (out_buf.pos > 0) {
                    self.out_pos = 0;
                    self.out_end = out_buf.pos;

                    // Copy what we can to dest
                    const to_copy = @min(out_buf.pos, dest.len - total_read);
                    @memcpy(dest[total_read..][0..to_copy], self.out_buffer[0..to_copy]);
                    self.out_pos = to_copy;
                    total_read += to_copy;

                    if (total_read == dest.len) {
                        return total_read;
                    }
                }

                // If decompression is complete
                if (result == 0) {
                    break;
                }
            }
        }

        return total_read;
    }

    pub fn verifyComplete(self: *Self) !void {
        // Read any remaining data to ensure we process the entire stream
        var discard_buffer: [1024]u8 = undefined;
        while (true) {
            const n = try self.read(&discard_buffer);
            if (n == 0) break;
        }

        // The hash should have been verified during reading
        if (!self.hash_verified) {
            return error.HashMismatch;
        }
    }
};
