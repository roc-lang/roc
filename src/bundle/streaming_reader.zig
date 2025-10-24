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
        const self: *Self = @alignCast(@fieldParentPtr("interface", r));

        // fill the input buffer as much as possible
        var vec: [1][]u8 = .{self.in_buffer[self.in_end..]};
        var reached_end = false;
        const bytes_read = self.input_reader.readVec(&vec) catch |err| switch (err) {
            error.EndOfStream => blk: {
                reached_end = true;
                break :blk 0;
            },
            error.ReadFailed => return error.ReadFailed,
        };

        if (reached_end) {
            // verify hash if not already done
            if (!self.hash_verified) {
                var actual_hash: [32]u8 = undefined;
                self.hasher.final(&actual_hash);
                if (!std.mem.eql(u8, &actual_hash, &self.expected_hash)) {
                    return error.ReadFailed;
                }
                self.hash_verified = true;
            }
            // actual end is only reached when the input buffer is also empty
            if (self.in_end == 0) {
                @branchHint(.likely);
                return error.EndOfStream;
            }
        }

        // Update hash with compressed data
        self.hasher.update(self.in_buffer[self.in_end..][0..bytes_read]);
        self.in_end += bytes_read;

        // Decompress data directly into the output writer
        var in_buf = c.ZSTD_inBuffer{ .src = self.in_buffer.ptr, .size = self.in_end, .pos = self.in_pos };

        const out_data = limit.slice(try w.writableSliceGreedy(1));
        var out_buf = c.ZSTD_outBuffer{ .dst = out_data.ptr, .size = out_data.len, .pos = 0 };
        while (in_buf.pos != in_buf.size) {
            const result = c.ZSTD_decompressStream(self.dctx, &out_buf, &in_buf);
            if (c.ZSTD_isError(result) != 0) {
                // this is still a read failed, as we are not writing to the writer but the internal buffer
                return error.ReadFailed;
            }
            if (out_buf.pos == out_buf.size) {
                break;
            }
        }
        if (in_buf.pos < in_buf.size) {
            // TODO we could fill the internal reader buffer here
            self.in_pos = in_buf.pos;
            self.in_end = in_buf.size;
        } else {
            self.in_pos = 0;
            self.in_end = 0;
        }

        w.advance(out_buf.pos);
        return out_buf.pos;
    }

    /// Verify that the hash matches. This should be called after reading is complete.
    /// If there is remaining data, it will be discarded.
    pub fn verifyComplete(self: *Self) !void {
        _ = self.interface.discardRemaining() catch {
            // When the hash does not match, discardRemaining will return a ReadFailed, so we have to ignore it
        };

        // The hash should have been verified during stream
        if (!self.hash_verified) {
            return error.HashMismatch;
        }
    }
};
