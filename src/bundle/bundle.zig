//! Bundle
//!
//! Future work:
//! - Create a zstd dictionary for roc code (using ~1-10MB of representative roc source code, with the zstd cli;
//!   adds about 110KB to our final binary) and use that. It's a backwards-compatible change (we can keep decoding
//!   dictionary-free .zst files even after we introduce the dictionary)
//! - Changing dictionaries after you've started using one is a breaking change (there's an auto-generated
//!   dictionary ID in the binary, so you know when you're trying to decode with a different dictionary than
//!   the one that the binary was compresed with, and zstd will error), and each time we add new dictionaries
//!   in a nonbreaking way, we have to add +110KB to the `roc` binary, so we should avoid this and instead
//!   only introduce a dictionary when we're confident we'll be happy with that being THE dictionary for a long time.
//! - Compress/Decompress large binary blobs (e.g. for host data, or static List(U8) imports) separately
//!   using different compression params and dictionaries (e.g. make a .tar.zst inside the main .tar.zst)

const std = @import("std");
const builtin = @import("builtin");
const c = @cImport({
    @cInclude("zstd.h");
});

pub const BundleError = error{
    OutOfMemory,
    FilePathTooLong,
    FileOpenFailed,
    FileStatFailed,
    FileReadFailed,
    TarWriteFailed,
    CompressionFailed,
    WriteFailed,
    FlushFailed,
} || std.mem.Allocator.Error;

pub const UnbundleError = error{
    OutOfMemory,
    DecompressionFailed,
    InvalidTarHeader,
    UnexpectedEndOfStream,
    FileCreateFailed,
    DirectoryCreateFailed,
    FileWriteFailed,
} || std.mem.Allocator.Error;

/// Bundle files into a compressed tar archive.
///
/// The file_path_iter must yield file paths that are valid for use with `Dir.openFile`.
/// This means paths must be relative (not absolute), must not contain ".." components,
/// and on Windows must use forward slashes. File paths are limited to 255 bytes for
/// tar compatibility. Paths must be encoded as WTF-8 on Windows, UTF-8 elsewhere.
pub fn bundle(
    file_path_iter: anytype,
    compression_level: c_int,
    allocator: std.mem.Allocator,
    output_writer: anytype,
    base_dir: std.fs.Dir,
) BundleError!void {
    try bundleImpl(file_path_iter, compression_level, allocator, output_writer, base_dir);
}

fn bundleImpl(
    file_path_iter: anytype,
    compression_level: c_int,
    allocator: std.mem.Allocator,
    output_writer: anytype,
    base_dir: std.fs.Dir,
) BundleError!void {
    // First create the tar in memory
    var tar_buffer = std.ArrayList(u8).init(allocator);
    defer tar_buffer.deinit();

    // Create tar writer
    var tar_writer = std.tar.writer(tar_buffer.writer());

    // Write files to tar
    while (try file_path_iter.next()) |file_path| {
        if (file_path.len > 255) {
            return error.FilePathTooLong;
        }

        const file = base_dir.openFile(file_path, .{}) catch {
            return error.FileOpenFailed;
        };
        defer file.close();

        const stat = file.stat() catch {
            return error.FileStatFailed;
        };

        const file_content = try allocator.alloc(u8, stat.size);
        defer allocator.free(file_content);

        // Seek to beginning of file before reading
        file.seekTo(0) catch {
            return error.FileReadFailed;
        };
        const bytes_read = file.readAll(file_content) catch {
            return error.FileReadFailed;
        };
        if (bytes_read != stat.size) {
            return error.FileReadFailed;
        }

        // Use mtime of 0 for reproducible builds
        const Options = @TypeOf(tar_writer).Options;
        const options = Options{
            .mode = 0o644,
            .mtime = 0,
        };
        tar_writer.writeFileBytes(file_path, file_content, options) catch {
            return error.TarWriteFailed;
        };
    }

    // Finish the tar archive
    tar_writer.finish() catch {
        return error.TarWriteFailed;
    };

    // Now compress the tar data
    var buffered_writer = std.io.bufferedWriter(output_writer);
    const buffered = buffered_writer.writer();

    const ctx = c.ZSTD_createCCtx() orelse return error.OutOfMemory;
    defer _ = c.ZSTD_freeCCtx(ctx);

    _ = c.ZSTD_CCtx_setParameter(ctx, c.ZSTD_c_compressionLevel, compression_level);

    const out_buffer_size = c.ZSTD_CStreamOutSize();
    var out_buffer = try allocator.alloc(u8, out_buffer_size);
    defer allocator.free(out_buffer);

    // Compress the tar data
    var in_buf = c.ZSTD_inBuffer{ .src = tar_buffer.items.ptr, .size = tar_buffer.items.len, .pos = 0 };
    var out_buf = c.ZSTD_outBuffer{ .dst = out_buffer.ptr, .size = out_buffer.len, .pos = 0 };

    // Compress all data
    while (in_buf.pos < in_buf.size) {
        const result = c.ZSTD_compressStream2(ctx, &out_buf, &in_buf, c.ZSTD_e_continue);
        if (c.ZSTD_isError(result) != 0) {
            return error.CompressionFailed;
        }

        if (out_buf.pos > 0) {
            buffered.writeAll(out_buffer[0..out_buf.pos]) catch {
                return error.WriteFailed;
            };
            out_buf.pos = 0;
        }
    }

    // Finalize compression
    in_buf = c.ZSTD_inBuffer{ .src = "", .size = 0, .pos = 0 };
    while (true) {
        const remaining = c.ZSTD_compressStream2(ctx, &out_buf, &in_buf, c.ZSTD_e_end);
        if (c.ZSTD_isError(remaining) != 0) {
            return error.CompressionFailed;
        }

        if (out_buf.pos > 0) {
            buffered.writeAll(out_buffer[0..out_buf.pos]) catch {
                return error.WriteFailed;
            };
            out_buf.pos = 0;
        }

        if (remaining == 0) break;
    }

    buffered_writer.flush() catch {
        return error.FlushFailed;
    };
}

/// Unbundle files from a compressed tar archive.
///
/// Extracts files to the provided directory, creating subdirectories as needed.
pub fn unbundle(
    input_reader: anytype,
    extract_dir: std.fs.Dir,
    allocator: std.mem.Allocator,
) UnbundleError!void {
    // Buffered reader for input
    var buffered_reader = std.io.bufferedReader(input_reader);
    const buffered = buffered_reader.reader();

    const dctx = c.ZSTD_createDCtx() orelse {
        return error.OutOfMemory;
    };
    defer _ = c.ZSTD_freeDCtx(dctx);

    // Read and decompress data in chunks
    var decompressed_data = std.ArrayList(u8).init(allocator);
    defer decompressed_data.deinit();

    const in_buffer_size = c.ZSTD_DStreamInSize();
    const out_buffer_size = c.ZSTD_DStreamOutSize();
    const in_buffer = try allocator.alloc(u8, in_buffer_size);
    defer allocator.free(in_buffer);
    var out_buffer = try allocator.alloc(u8, out_buffer_size);
    defer allocator.free(out_buffer);

    // Decompress the entire stream
    while (true) {
        const bytes_read = buffered.read(in_buffer) catch {
            return error.UnexpectedEndOfStream;
        };
        if (bytes_read == 0) break;

        var in_buf = c.ZSTD_inBuffer{ .src = in_buffer.ptr, .size = bytes_read, .pos = 0 };

        while (in_buf.pos < in_buf.size) {
            var out_buf = c.ZSTD_outBuffer{ .dst = out_buffer.ptr, .size = out_buffer.len, .pos = 0 };

            const result = c.ZSTD_decompressStream(dctx, &out_buf, &in_buf);
            if (c.ZSTD_isError(result) != 0) {
                return error.DecompressionFailed;
            }

            if (out_buf.pos > 0) {
                try decompressed_data.appendSlice(out_buffer[0..out_buf.pos]);
            }
        }
    }

    // Create a reader from the decompressed data
    var decompressed_stream = std.io.fixedBufferStream(decompressed_data.items);
    const tar_reader = decompressed_stream.reader();

    // Use std.tar to parse the archive
    var file_name_buffer: [256]u8 = undefined;
    var link_name_buffer: [256]u8 = undefined;
    var tar_iter = std.tar.iterator(tar_reader, .{
        .file_name_buffer = &file_name_buffer,
        .link_name_buffer = &link_name_buffer,
    });

    // Process each file in the archive
    while (true) {
        const file = tar_iter.next() catch |err| {
            if (err == error.EndOfStream) break;
            return error.InvalidTarHeader;
        };

        if (file == null) break;
        const tar_file = file.?;

        switch (tar_file.kind) {
            .file => {
                // Create parent directories if needed
                if (std.fs.path.dirname(tar_file.name)) |dir_name| {
                    extract_dir.makePath(dir_name) catch {
                        return error.DirectoryCreateFailed;
                    };
                }

                const out_file = extract_dir.createFile(tar_file.name, .{}) catch {
                    return error.FileCreateFailed;
                };
                defer out_file.close();

                // Copy file contents
                const reader = tar_iter.reader;
                var buf: [8192]u8 = undefined;
                var bytes_remaining = tar_file.size;
                while (bytes_remaining > 0) {
                    const to_read = @min(buf.len, bytes_remaining);
                    const bytes_read = reader.read(buf[0..to_read]) catch {
                        return error.FileWriteFailed;
                    };
                    if (bytes_read == 0) break;
                    out_file.writeAll(buf[0..bytes_read]) catch {
                        return error.FileWriteFailed;
                    };
                    bytes_remaining -= bytes_read;
                }
            },
            .directory => {
                extract_dir.makePath(tar_file.name) catch {
                    return error.DirectoryCreateFailed;
                };
            },
            else => {
                // Skip other file types (symlinks, etc.)
                // std.tar automatically handles skipping the content for us
            },
        }
    }
}
