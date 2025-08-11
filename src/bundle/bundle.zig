//! Bundle and unbundle a roc package and everything it requires, including host object files if the
//! package is a platform, and any files imported via `import` with `Str` or `List(U8)`.
//!
//! Future work:
//! - Canonicalize to discover all the files we actually need to pull in, including non-.roc files.
//! - Create a zstd dictionary for roc code (using ~1-10MB of representative roc source code, with the zstd cli;
//!   adds about 110KB to our final binary) and use that. It's a backwards-compatible change, as we can keep decoding
//!   dictionary-free .zst files even after we introduce the dictionary.
//! - Changing dictionaries after you've started using one is a breaking change (there's an auto-generated
//!   dictionary ID in the binary, so you know when you're trying to decode with a different dictionary than
//!   the one that the binary was compressed with, and zstd will error), and each time we add new dictionaries
//!   in a nonbreaking way, we have to add +110KB to the `roc` binary, so we should avoid this and instead
//!   only introduce a dictionary when we're confident we'll be happy with that being THE dictionary for a long time.
//! - Compress/Decompress large binary blobs (e.g. for host data, or static List(U8) imports) separately
//!   using different compression params and dictionaries (e.g. make a .tar.zst inside the main .tar.zst)

const std = @import("std");
const base58 = @import("base58.zig");
const streaming_writer = @import("streaming_writer.zig");
const streaming_reader = @import("streaming_reader.zig");
const c = @cImport({
    @cDefine("ZSTD_STATIC_LINKING_ONLY", "1");
    @cInclude("zstd.h");
});

// Constants for magic numbers
const SIZE_STORAGE_BYTES: usize = 16; // Extra bytes for storing allocation size; use 16 to preserve alignment.
const TAR_PATH_MAX_LENGTH: usize = 255; // Maximum path length for tar compatibility
pub const STREAM_BUFFER_SIZE: usize = 64 * 1024; // 64KB buffer for streaming operations
const TAR_EXTENSION = ".tar.zst";
pub const DEFAULT_COMPRESSION_LEVEL: c_int = 22; // Maximum compression

// zstd's C library does custom allocations using a slightly different format from Zig's allocator API.
pub fn allocForZstd(opaque_ptr: ?*anyopaque, size: usize) callconv(.C) ?*anyopaque {
    const allocator = @as(*std.mem.Allocator, @ptrCast(@alignCast(opaque_ptr.?)));
    // Allocate extra bytes to store the size
    const total_size = size + SIZE_STORAGE_BYTES;
    const mem = allocator.alloc(u8, total_size) catch return null;

    // Store the size in the first 8 bytes (usize)
    const size_ptr = @as(*usize, @ptrCast(@alignCast(mem.ptr)));
    size_ptr.* = total_size;

    // Return pointer offset by overhead bytes
    return @ptrFromInt(@intFromPtr(mem.ptr) + SIZE_STORAGE_BYTES);
}

pub fn freeForZstd(opaque_ptr: ?*anyopaque, address: ?*anyopaque) callconv(.C) void {
    if (address == null) return;
    const allocator = @as(*std.mem.Allocator, @ptrCast(@alignCast(opaque_ptr.?)));

    // Get the original allocation by subtracting overhead bytes
    const original_ptr = @as([*]u8, @ptrFromInt(@intFromPtr(address) - SIZE_STORAGE_BYTES));

    // Read the size from the first 8 bytes
    const size_ptr = @as(*const usize, @ptrCast(@alignCast(original_ptr)));
    const total_size = size_ptr.*;

    // Free the full allocation
    allocator.free(original_ptr[0..total_size]);
}

/// Errors that can occur during the bundle operation.
pub const BundleError = error{
    FilePathTooLong,
    FileNotFound,
    AccessDenied,
    IsDir,
    FileOpenFailed,
    SystemResources,
    FileStatFailed,
    FileReadFailed,
    FileTooLarge,
    TarWriteFailed,
    CompressionFailed,
    WriteFailed,
    FlushFailed,
} || std.mem.Allocator.Error;

/// Errors that can occur during the unbundle operation.
pub const UnbundleError = error{
    DecompressionFailed,
    InvalidTarHeader,
    UnexpectedEndOfStream,
    FileCreateFailed,
    DirectoryCreateFailed,
    FileWriteFailed,
    HashMismatch,
    InvalidFilename,
    FileTooLarge,
    InvalidPath,
} || std.mem.Allocator.Error;

/// Bundle files into a compressed tar archive.
///
/// The file_path_iter must yield file paths that are valid for use with `Dir.openFile`.
/// This means paths must be relative (not absolute), must not contain ".." components,
/// and on Windows must use forward slashes. File paths are limited to 255 bytes for
/// tar compatibility. Paths must be encoded as WTF-8 on Windows, UTF-8 elsewhere.
///
/// If path_prefix is provided, it will be stripped from the beginning of each file path
/// before adding to the tar archive.
///
/// Compression level should be between 1 (fastest) and 22 (best compression).
/// Level 3 is a good default for speed/size tradeoff.
///
/// Returns the filename (base58-encoded blake3 hash + .tar.zst). Caller must free the returned string.
pub fn bundle(
    file_path_iter: anytype,
    compression_level: c_int,
    allocator: *std.mem.Allocator,
    output_writer: anytype,
    base_dir: std.fs.Dir,
    path_prefix: ?[]const u8,
) BundleError![]u8 {
    // Create a buffered writer for the output
    var buffered_writer = std.io.bufferedWriter(output_writer);
    const buffered = buffered_writer.writer();

    // Create compressing hash writer that chains: tar → compress → hash → output
    var compress_writer = streaming_writer.CompressingHashWriter.init(
        allocator,
        compression_level,
        buffered.any(),
        allocForZstd,
        freeForZstd,
    ) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
    };
    defer compress_writer.deinit();

    // Create tar writer that writes to the compressing writer
    var tar_writer = std.tar.writer(compress_writer.writer());

    // Process files one at a time
    while (try file_path_iter.next()) |file_path| {
        const file = base_dir.openFile(file_path, .{}) catch |err| switch (err) {
            error.FileNotFound => return error.FileNotFound,
            error.AccessDenied => return error.AccessDenied,
            error.IsDir => return error.IsDir,
            else => return error.FileOpenFailed,
        };
        defer file.close();

        const stat = file.stat() catch |err| switch (err) {
            error.SystemResources => return error.SystemResources,
            else => return error.FileStatFailed,
        };

        const file_size = std.math.cast(usize, stat.size) orelse return error.FileTooLarge;

        // Strip path prefix if provided
        const tar_path = if (path_prefix) |prefix| blk: {
            if (std.mem.startsWith(u8, file_path, prefix)) {
                break :blk file_path[prefix.len..];
            } else {
                break :blk file_path;
            }
        } else file_path;

        if (tar_path.len > TAR_PATH_MAX_LENGTH) {
            return error.FilePathTooLong;
        }

        // Write tar header and stream file content
        const Options = @TypeOf(tar_writer).Options;
        const options = Options{
            .mode = 0o644,
            .mtime = 0,
        };

        // Create a reader for the file
        const file_reader = file.reader();

        // Stream the file to tar
        tar_writer.writeFileStream(tar_path, file_size, file_reader, options) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return error.TarWriteFailed,
        };
    }

    // Finish the tar archive
    tar_writer.finish() catch {
        return error.TarWriteFailed;
    };

    // Finish compression
    compress_writer.finish() catch |err| switch (err) {
        error.CompressionFailed => return error.CompressionFailed,
        error.WriteFailed => return error.WriteFailed,
        error.AlreadyFinished => return error.CompressionFailed,
        error.OutOfMemory => return error.OutOfMemory,
    };

    buffered_writer.flush() catch {
        return error.FlushFailed;
    };

    // Get the blake3 hash and encode as base58
    const hash = compress_writer.getHash();
    var base58_buffer: [base58.base58_hash_bytes]u8 = undefined;
    const base58_encoded = base58.encode(&hash, &base58_buffer);
    const base58_hash = try allocator.*.dupe(u8, base58_encoded);
    defer allocator.*.free(base58_hash);

    // Create filename with .tar.zst extension
    const filename = try std.fmt.allocPrint(allocator.*, "{s}{s}", .{ base58_hash, TAR_EXTENSION });
    return filename;
}

/// Validate a base58-encoded hash string and return the decoded hash.
/// Returns null if the hash is invalid.
pub fn validateBase58Hash(base58_hash: []const u8) !?[32]u8 {
    if (base58_hash.len > base58.base58_hash_bytes) {
        return null;
    }

    var hash: [32]u8 = undefined;
    base58.decode(base58_hash, &hash) catch return null;
    return hash;
}

/// Validate a file path to prevent directory traversal attacks.
/// Returns false if the path contains ".." components or absolute paths.
fn isValidPath(path: []const u8) bool {
    // Reject empty paths
    if (path.len == 0) return false;

    // Reject absolute paths
    if (path[0] == '/' or (path.len >= 3 and path[1] == ':' and path[2] == '/')) {
        return false;
    }

    // Check for ".." components
    var it = std.mem.tokenizeAny(u8, path, "/\\");
    while (it.next()) |component| {
        if (std.mem.eql(u8, component, "..")) {
            return false;
        }
    }

    return true;
}

/// Writer interface for extracting files during unbundle
pub const ExtractWriter = struct {
    ptr: *anyopaque,
    makeDirFn: *const fn (ptr: *anyopaque, path: []const u8) anyerror!void,
    streamFileFn: *const fn (ptr: *anyopaque, path: []const u8, reader: std.io.AnyReader, size: usize) anyerror!void,

    pub fn makeDir(self: ExtractWriter, path: []const u8) !void {
        return self.makeDirFn(self.ptr, path);
    }

    pub fn streamFile(self: ExtractWriter, path: []const u8, reader: std.io.AnyReader, size: usize) !void {
        return self.streamFileFn(self.ptr, path, reader, size);
    }
};

/// Directory-based extract writer
pub const DirExtractWriter = struct {
    dir: std.fs.Dir,

    pub fn init(dir: std.fs.Dir) DirExtractWriter {
        return .{ .dir = dir };
    }

    pub fn extractWriter(self: *DirExtractWriter) ExtractWriter {
        return .{
            .ptr = self,
            .makeDirFn = makeDir,
            .streamFileFn = streamFile,
        };
    }

    fn makeDir(ptr: *anyopaque, path: []const u8) anyerror!void {
        const self = @as(*DirExtractWriter, @ptrCast(@alignCast(ptr)));
        try self.dir.makePath(path);
    }

    fn streamFile(ptr: *anyopaque, path: []const u8, reader: std.io.AnyReader, size: usize) anyerror!void {
        const self = @as(*DirExtractWriter, @ptrCast(@alignCast(ptr)));

        // Create parent directories if needed
        if (std.fs.path.dirname(path)) |dir_name| {
            try self.dir.makePath(dir_name);
        }

        const file = try self.dir.createFile(path, .{});
        defer file.close();

        // Stream from reader to file
        var buffer: [STREAM_BUFFER_SIZE]u8 = undefined;
        var total_written: usize = 0;

        while (total_written < size) {
            const to_read = @min(buffer.len, size - total_written);
            const bytes_read = try reader.read(buffer[0..to_read]);

            if (bytes_read == 0) {
                break;
            }

            try file.writeAll(buffer[0..bytes_read]);
            total_written += bytes_read;
        }

        if (total_written != size) {
            return error.UnexpectedEndOfStream;
        }
    }
};

/// Unbundle files from a compressed tar archive stream.
///
/// This is the core streaming unbundle logic that can be used by both file-based
/// unbundling and network-based downloading.
pub fn unbundleStream(
    input_reader: anytype,
    extract_writer: ExtractWriter,
    allocator: *std.mem.Allocator,
    expected_hash: *const [32]u8,
) UnbundleError!void {
    // Buffered reader for input
    var buffered_reader = std.io.bufferedReader(input_reader);
    const buffered = buffered_reader.reader();

    // Create decompressing hash reader that chains: input → verify hash → decompress
    var decompress_reader = streaming_reader.DecompressingHashReader.init(
        allocator,
        buffered.any(),
        expected_hash.*,
        allocForZstd,
        freeForZstd,
    ) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
    };
    defer decompress_reader.deinit();

    // Use std.tar to parse the archive; allocate MAX_LENGTH + 1 for null terminator
    var file_name_buffer: [TAR_PATH_MAX_LENGTH + 1]u8 = undefined;
    var link_name_buffer: [TAR_PATH_MAX_LENGTH + 1]u8 = undefined;
    var tar_iter = std.tar.iterator(decompress_reader.reader(), .{
        .file_name_buffer = &file_name_buffer,
        .link_name_buffer = &link_name_buffer,
    });

    // Process each file in the archive - streaming directly from decompression
    while (true) {
        const file = tar_iter.next() catch |err| {
            if (err == error.EndOfStream) break;
            // Any other error means the tar archive is corrupted or malformed.
            // We don't try to recover because partial extraction could leave
            // the system in an inconsistent state.
            return error.InvalidTarHeader;
        };

        if (file == null) break;
        const tar_file = file.?;

        // Validate path to prevent directory traversal
        if (!isValidPath(tar_file.name)) {
            return error.InvalidPath;
        }

        switch (tar_file.kind) {
            .file => {
                const tar_file_size = std.math.cast(usize, tar_file.size) orelse return error.FileTooLarge;

                // Stream file directly from tar to disk
                extract_writer.streamFile(tar_file.name, tar_file.reader().any(), tar_file_size) catch |err| {
                    switch (err) {
                        error.UnexpectedEndOfStream => return error.UnexpectedEndOfStream,
                        else => return error.FileWriteFailed,
                    }
                };
            },
            .directory => {
                extract_writer.makeDir(tar_file.name) catch {
                    return error.DirectoryCreateFailed;
                };
            },
            else => {
                // Skip other file types (symlinks, etc.)
                // std.tar automatically handles skipping the content for us
            },
        }
    }

    // Ensure all data was read and hash was verified
    decompress_reader.verifyComplete() catch |err| switch (err) {
        error.HashMismatch => return error.HashMismatch,
        error.UnexpectedEndOfStream => return error.UnexpectedEndOfStream,
        error.DecompressionFailed => return error.DecompressionFailed,
        error.OutOfMemory => return error.OutOfMemory,
    };
}

/// Unbundle files from a compressed tar archive.
///
/// Extracts files to the provided directory, creating subdirectories as needed.
/// The filename parameter should be the base58-encoded blake3 hash + .tar.zst extension.
pub fn unbundle(
    input_reader: anytype,
    extract_dir: std.fs.Dir,
    allocator: *std.mem.Allocator,
    filename: []const u8,
) UnbundleError!void {
    // Extract expected hash from filename
    if (!std.mem.endsWith(u8, filename, TAR_EXTENSION)) {
        return error.InvalidFilename;
    }
    const base58_hash = filename[0 .. filename.len - TAR_EXTENSION.len]; // Remove .tar.zst
    const expected_hash = (try validateBase58Hash(base58_hash)) orelse {
        return error.InvalidFilename;
    };

    var dir_writer = DirExtractWriter.init(extract_dir);
    return unbundleStream(input_reader, dir_writer.extractWriter(), allocator, &expected_hash);
}
