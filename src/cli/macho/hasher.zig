//! Parallel file hashing for Mach-O code signature page digests.
//!
//! Adapted from the Zig compiler at https://codeberg.org/ziglang/zig and licensed under the MIT license. Thanks, Zig team!

const std = @import("std");
const Io = std.Io;
const Allocator = std.mem.Allocator;

const trace = @import("tracy").trace;

/// Hashes a file in fixed-size chunks concurrently, one digest per chunk.
pub fn ParallelHasher(comptime Hasher: type) type {
    const hash_size = Hasher.digest_length;

    return struct {
        pub fn hash(gpa: Allocator, io: Io, file: Io.File, out: [][hash_size]u8, opts: struct {
            chunk_size: u64 = 0x4000,
            max_file_size: ?u64 = null,
        }) (Allocator.Error || Io.File.ReadPositionalError || Io.File.LengthError || error{Overflow})!void {
            const tracy = trace(@src());
            defer tracy.end();

            const file_size = blk: {
                const file_size = opts.max_file_size orelse try file.length(io);
                break :blk std.math.cast(usize, file_size) orelse return error.Overflow;
            };
            const chunk_size = std.math.cast(usize, opts.chunk_size) orelse return error.Overflow;

            const buffer = try gpa.alloc(u8, chunk_size * out.len);
            defer gpa.free(buffer);

            const results = try gpa.alloc(Io.File.ReadPositionalError!usize, out.len);
            defer gpa.free(results);

            {
                var group: Io.Group = .init;
                defer group.cancel(io);

                for (out, results, 0..) |*out_buf, *result, i| {
                    const fstart = i * chunk_size;
                    const fsize = if (fstart + chunk_size > file_size)
                        file_size - fstart
                    else
                        chunk_size;
                    group.async(io, worker, .{
                        io,
                        file,
                        fstart,
                        buffer[fstart..][0..fsize],
                        &(out_buf.*),
                        &(result.*),
                    });
                }

                try group.await(io);
            }
            for (results) |result| _ = try result;
        }

        fn worker(
            io: Io,
            file: Io.File,
            fstart: usize,
            buffer: []u8,
            out: *[hash_size]u8,
            err: *Io.File.ReadPositionalError!usize,
        ) void {
            err.* = file.readPositionalAll(io, buffer, fstart);
            Hasher.hash(buffer, out, .{});
        }
    };
}
