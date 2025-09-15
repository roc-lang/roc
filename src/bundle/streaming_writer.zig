//! Streaming writer for compressing data with zstd and hash computation
//!
//! This module provides a writer that compresses data streams using zstd compression while
//! simultaneously computing BLAKE3 hashes for data integrity verification.

const std = @import("std");
const c = @cImport({
    @cDefine("ZSTD_STATIC_LINKING_ONLY", "1");
    @cInclude("zstd.h");
});

/// A writer that compresses data with zstd and computes a hash incrementally
pub const CompressingHashWriter = struct {
    allocator_ptr: *std.mem.Allocator,
    ctx: *c.ZSTD_CCtx,
    hasher: std.crypto.hash.Blake3,
    output_writer: std.io.Writer,
    out_buffer: []u8,
    in_buffer: []u8,
    in_pos: usize,
    finished: bool,

    const Self = @This();

    pub fn init(
        allocator_ptr: *std.mem.Allocator,
        compression_level: c_int,
        output_writer: std.io.Writer,
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

        const in_buffer_size = c.ZSTD_CStreamInSize();
        const in_buffer = try allocator_ptr.alloc(u8, in_buffer_size);
        errdefer allocator_ptr.free(in_buffer);

        return Self{
            .allocator_ptr = allocator_ptr,
            .ctx = ctx,
            .hasher = std.crypto.hash.Blake3.init(.{}),
            .output_writer = output_writer,
            .out_buffer = out_buffer,
            .in_buffer = in_buffer,
            .in_pos = 0,
            .finished = false,
        };
    }

    pub fn drain(w: *std.io.Writer, data: []const []const u8, splat: usize) std.io.Error!usize {
        // There is no need to splat
        std.debug.assert(splat == 0);

        const compressing_hash_writer: *CompressingHashWriter = @fieldParentPtr("output_writer", w);

        if (compressing_hash_writer.finished) return error.WriteFailed;

        var written: usize = 0;
        for (data) |bytes| {
            while (written < bytes.len) {
                // Fill input buffer
                const space_available = compressing_hash_writer.in_buffer.len - compressing_hash_writer.in_pos;
                const to_copy = @min(space_available, bytes.len - written);
                @memcpy(compressing_hash_writer.in_buffer[compressing_hash_writer.in_pos..][0..to_copy], bytes[written..][0..to_copy]);
                compressing_hash_writer.in_pos += to_copy;
                written += to_copy;

                // If buffer is full, compress it
                if (compressing_hash_writer.in_pos == compressing_hash_writer.in_buffer.len) {
                    try compressing_hash_writer.compressBuffer(false);
                }
            }
        }
        return written;
    }

    pub fn deinit(self: *Self) void {
        _ = c.ZSTD_freeCCtx(self.ctx);
        self.allocator_ptr.free(self.out_buffer);
        self.allocator_ptr.free(self.in_buffer);
    }

    pub fn writer(self: *Self) std.io.Writer {
        // TODO: Or should this be the out_buffer?
        return .{ .buffer = self.in_buffer, .vtable = &vtable };
    }

    const vtable: std.io.Writer.VTable = .{
        .drain = @This().drain,
    };

    fn compressBuffer(self: *Self, end_stream: bool) std.io.Writer.Error!void {
        if (self.in_pos == 0 and !end_stream) return;

        var in_buf = c.ZSTD_inBuffer{ .src = self.in_buffer.ptr, .size = self.in_pos, .pos = 0 };

        const mode: c_uint = if (end_stream) c.ZSTD_e_end else c.ZSTD_e_continue;

        while (in_buf.pos < in_buf.size or end_stream) {
            var out_buf = c.ZSTD_outBuffer{ .dst = self.out_buffer.ptr, .size = self.out_buffer.len, .pos = 0 };

            const remaining = c.ZSTD_compressStream2(self.ctx, &out_buf, &in_buf, mode);
            if (c.ZSTD_isError(remaining) != 0) {
                return error.WriteFailed;
            }

            if (out_buf.pos > 0) {
                const chunk = self.out_buffer[0..out_buf.pos];
                self.output_writer.writeAll(chunk) catch return error.WriteFailed;
                self.hasher.update(chunk);
            }

            if (end_stream and remaining == 0) break;
        }

        self.in_pos = 0;
    }

    pub fn finish(self: *Self) std.io.Writer.Error!void {
        if (self.finished) return;
        try self.compressBuffer(true);
        self.finished = true;
    }

    pub fn getHash(self: *Self) [32]u8 {
        var hash: [32]u8 = undefined;
        self.hasher.final(&hash);
        return hash;
    }
};
