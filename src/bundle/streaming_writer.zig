//! Streaming writer for compressing data with zstd and hash computation
//!
//! This module provides a writer that compresses data streams using zstd compression while
//! simultaneously computing BLAKE3 hashes for data integrity verification.

const std = @import("std");
const c = @cImport({
    @cDefine("ZSTD_STATIC_LINKING_ONLY", "1");
    @cInclude("zstd.h");
});

const WriterError = std.io.Writer.Error;

/// A writer that compresses data with zstd and computes a hash incrementally
pub const CompressingHashWriter = struct {
    allocator_ptr: *std.mem.Allocator,
    ctx: *c.ZSTD_CCtx,
    hasher: std.crypto.hash.Blake3,
    output_writer: *std.io.Writer,
    out_buffer: []u8,
    finished: bool,
    interface: std.io.Writer,

    const Self = @This();

    pub fn init(
        allocator_ptr: *std.mem.Allocator,
        compression_level: c_int,
        output_writer: *std.io.Writer,
        allocForZstd: *const fn (?*anyopaque, usize) callconv(.c) ?*anyopaque,
        freeForZstd: *const fn (?*anyopaque, ?*anyopaque) callconv(.c) void,
    ) !Self {
        const custom_mem = c.ZSTD_customMem{
            .customAlloc = allocForZstd,
            .customFree = freeForZstd,
            .@"opaque" = @ptrCast(allocator_ptr),
        };

        const ctx = c.ZSTD_createCCtx_advanced(custom_mem) orelse return std.mem.Allocator.Error.OutOfMemory;
        errdefer _ = c.ZSTD_freeCCtx(ctx);

        _ = c.ZSTD_CCtx_setParameter(ctx, c.ZSTD_c_compressionLevel, compression_level);

        const out_buffer_size = c.ZSTD_CStreamOutSize();
        const out_buffer = try allocator_ptr.alloc(u8, out_buffer_size);
        errdefer allocator_ptr.free(out_buffer);

        // Allocate buffer for the Io.Writer interface
        const write_buffer_size = c.ZSTD_CStreamInSize();
        const writer_buffer = try allocator_ptr.alloc(u8, write_buffer_size);
        errdefer allocator_ptr.free(writer_buffer);

        var result = Self{
            .allocator_ptr = allocator_ptr,
            .ctx = ctx,
            .hasher = std.crypto.hash.Blake3.init(.{}),
            .output_writer = output_writer,
            .out_buffer = out_buffer,
            .finished = false,
            .interface = undefined,
        };
        result.interface = .{
            .vtable = &.{
                .drain = drain,
                .flush = flush,
            },
            .buffer = writer_buffer,
        };
        return result;
    }

    pub fn deinit(self: *Self) void {
        _ = c.ZSTD_freeCCtx(self.ctx);
        self.allocator_ptr.free(self.out_buffer);
        self.allocator_ptr.free(self.interface.buffer);
    }

    fn flush(w: *std.io.Writer) WriterError!void {
        const self: *Self = @alignCast(@fieldParentPtr("interface", w));
        if (self.finished and w.end != 0) return WriterError.WriteFailed;
        _ = self.compressAndHash(w.buffer[0..w.end], false) catch return error.WriteFailed;
        w.end = 0;
        return;
    }

    fn drain(w: *std.io.Writer, data: []const []const u8, splat: usize) WriterError!usize {
        const self: *Self = @alignCast(@fieldParentPtr("interface", w));
        if (self.finished) return WriterError.WriteFailed;
        _ = self.compressAndHash(w.buffer[0..w.end], false) catch return error.WriteFailed;
        w.end = 0;
        if (data.len == 0) return 0;

        var written: usize = 0;
        for (data[0 .. data.len - 1]) |bytes| {
            const len = self.compressAndHash(bytes, false) catch return error.WriteFailed;
            written += len;
        }

        const pattern = data[data.len - 1];
        for (0..splat) |_| {
            const len = self.compressAndHash(pattern, false) catch return error.WriteFailed;
            written += len;
        }
        return written;
    }

    fn compressAndHash(self: *Self, data: []const u8, end_stream: bool) WriterError!usize {
        var in_buf = c.ZSTD_inBuffer{ .src = data.ptr, .size = data.len, .pos = 0 };
        const mode: c_uint = if (end_stream) c.ZSTD_e_end else c.ZSTD_e_continue;
        while (in_buf.pos < in_buf.size or end_stream) {
            var out_buf = c.ZSTD_outBuffer{ .dst = self.out_buffer.ptr, .size = self.out_buffer.len, .pos = 0 };

            const remaining = c.ZSTD_compressStream2(self.ctx, &out_buf, &in_buf, mode);
            if (c.ZSTD_isError(remaining) != 0) {
                return WriterError.WriteFailed;
            }

            if (out_buf.pos > 0) {
                const chunk = self.out_buffer[0..out_buf.pos];
                self.output_writer.writeAll(chunk) catch return error.WriteFailed;
                self.hasher.update(chunk);
            }

            if (end_stream and remaining == 0) break;
        }
        return 0;
    }

    pub fn finish(self: *Self) WriterError!void {
        if (self.finished) return;
        _ = self.compressAndHash(self.interface.buffer[0..self.interface.end], true) catch return error.WriteFailed;
        self.interface.end = 0;
        self.finished = true;
    }

    pub fn getHash(self: *Self) [32]u8 {
        var hash: [32]u8 = undefined;
        self.hasher.final(&hash);
        return hash;
    }
};
