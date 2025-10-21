//! Streaming reader for decompressing zstd data with hash verification
//!
//! This module provides a reader that decompresses zstd-compressed data streams while
//! simultaneously computing and verifying BLAKE3 hashes for data integrity.

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
    input_reader: *std.Io.Reader,
    expected_hash: [32]u8,
    in_buffer: []u8,
    in_pos: usize,
    in_end: usize,
    hash_verified: bool,
    interface: std.Io.Reader,

    const Self = @This();
    const Error = error{
        DecompressionFailed,
        UnexpectedEndOfStream,
        HashMismatch,
    } || std.mem.Allocator.Error;

    pub fn init(
        allocator_ptr: *std.mem.Allocator,
        input_reader: *std.Io.Reader,
        expected_hash: [32]u8,
        allocForZstd: *const fn (?*anyopaque, usize) callconv(.c) ?*anyopaque,
        freeForZstd: *const fn (?*anyopaque, ?*anyopaque) callconv(.c) void,
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

        var result = Self{
            .allocator_ptr = allocator_ptr,
            .dctx = dctx,
            .hasher = std.crypto.hash.Blake3.init(.{}),
            .input_reader = input_reader,
            .expected_hash = expected_hash,
            .in_buffer = in_buffer,
            .in_pos = 0,
            .in_end = 0,
            .hash_verified = false,
            .interface = undefined,
        };
        result.interface = .{
            .vtable = &.{
                .stream = stream,
            },
            .buffer = out_buffer,
            .seek = 0,
            .end = 0,
        };
        return result;
    }

    pub fn deinit(self: *Self) void {
        _ = c.ZSTD_freeDCtx(self.dctx);
        self.allocator_ptr.free(self.in_buffer);
        self.allocator_ptr.free(self.interface.buffer);
    }

    fn stream(r: *std.Io.Reader, w: *std.Io.Writer, limit: std.Io.Limit) std.Io.Reader.StreamError!usize {
        // This implementation just adds the decompressed data to the buffer and returns 0.
        // This simplifies the logic a bit which is encouraged by the Zig reader API.
        _ = w;
        _ = limit;
        if (r.end == r.seek) {
            r.end = 0;
            r.seek = 0;
        }
        const self: *Self = @alignCast(@fieldParentPtr("interface", r));

        var in_writer = std.Io.Writer.fixed(self.in_buffer[self.in_end..]);
        var reached_end = false;
        const bytes_read = self.input_reader.stream(&in_writer, std.Io.Limit.limited(self.in_buffer.len)) catch |err| switch (err) {
            error.EndOfStream => blk: {
                reached_end = true;
                break :blk 0;
            },
            error.ReadFailed => return error.ReadFailed,
            error.WriteFailed => unreachable, // fixed buffer writer doesn't fail
        };

        if (reached_end) {
            // verify hash if not already done
            if (!self.hash_verified) {
                var actual_hash: [32]u8 = undefined;
                self.hasher.final(&actual_hash);
                if (std.mem.eql(u8, &actual_hash, &self.expected_hash)) {
                    self.hash_verified = true;
                }
            }
            return error.EndOfStream;
        }

        // Update hash with compressed data
        self.hasher.update(self.in_buffer[self.in_end..][0..bytes_read]);
        self.in_end += bytes_read;

        // Decompress just to fill the buffer
        var in_buf = c.ZSTD_inBuffer{ .src = self.in_buffer.ptr, .size = self.in_end, .pos = self.in_pos };

        var out_buf = c.ZSTD_outBuffer{ .dst = r.buffer.ptr, .size = r.buffer.len, .pos = r.end };

        const result = c.ZSTD_decompressStream(self.dctx, &out_buf, &in_buf);
        if (c.ZSTD_isError(result) != 0) {
            // this is still a read failed, as we are not writing to the writer but the internal buffer
            return error.ReadFailed;
        }
        if (in_buf.pos < in_buf.size) {
            self.in_pos = in_buf.pos;
            self.in_end = in_buf.size;
        } else {
            self.in_pos = 0;
            self.in_end = 0;
        }

        r.end = out_buf.pos;

        return 0;
    }

    /// Verify that the hash matches. This should be called after reading is complete.
    /// If there is remaining data, it will be discarded.
    pub fn verifyComplete(self: *Self) !void {
        // Read any remaining data to ensure we process the entire stream
        while (true) {
            _ = self.interface.discard(std.Io.Limit.unlimited) catch |err| {
                switch (err) {
                    error.EndOfStream => break,
                    error.ReadFailed => return error.ReadFailed,
                }
            };
        }

        // The hash should have been verified during stream
        if (!self.hash_verified) {
            return error.HashMismatch;
        }
    }
};
