//! Bundle a roc package and everything it requires, including host object files if the
//! package is a platform, and any files imported via `import` with `Str` or `List(U8)`.
//!
//! Future work:
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

const builtin = @import("builtin");
const std = @import("std");
const Allocator = std.mem.Allocator;
const base58 = @import("base58");
const streaming_writer = @import("streaming_writer.zig");
const unbundle = @import("unbundle");
const format = unbundle.format;
const c = @cImport({
    @cDefine("ZSTD_STATIC_LINKING_ONLY", "1");
    @cInclude("zstd.h");
});

// Constants for magic numbers
const SIZE_STORAGE_BYTES: usize = 16; // Extra bytes for storing allocation size; use 16 to preserve alignment.
/// Alignment for zstd custom allocations. Must match SIZE_STORAGE_BYTES (16 bytes).
const ZSTD_ALLOC_ALIGNMENT: std.mem.Alignment = .@"16";
/// Size of the buffer used for streaming operations (in bytes)
pub const STREAM_BUFFER_SIZE = format.STREAM_BUFFER_SIZE;
const TAR_EXTENSION = format.TAR_EXTENSION;
/// Default compression level for zstd (22 = maximum compression)
pub const DEFAULT_COMPRESSION_LEVEL: c_int = 22;

/// Custom allocator function for zstd that adds extra bytes to store allocation size
pub fn allocForZstd(context_ptr: ?*anyopaque, size: usize) callconv(.c) ?*anyopaque {
    const allocator = @as(*std.mem.Allocator, @ptrCast(@alignCast(context_ptr.?)));
    // Allocate extra bytes to store the size, with proper alignment to ensure we can
    // store a usize at the start and return properly aligned memory to zstd.
    const total_size = size + SIZE_STORAGE_BYTES;
    const mem = allocator.rawAlloc(total_size, ZSTD_ALLOC_ALIGNMENT, @returnAddress()) orelse return null;

    // Store the size in the first bytes (usize)
    const size_ptr: *usize = @ptrCast(@alignCast(mem));
    size_ptr.* = total_size;

    // Return pointer offset by overhead bytes
    return @ptrFromInt(@intFromPtr(mem) + SIZE_STORAGE_BYTES);
}

/// Custom free function for zstd that retrieves the original allocation size
pub fn freeForZstd(context_ptr: ?*anyopaque, address: ?*anyopaque) callconv(.c) void {
    if (address == null) return;
    const allocator = @as(*std.mem.Allocator, @ptrCast(@alignCast(context_ptr.?)));

    // Get the original allocation by subtracting overhead bytes
    const original_ptr: [*]u8 = @ptrFromInt(@intFromPtr(address) - SIZE_STORAGE_BYTES);

    // Read the size from the first bytes
    const size_ptr: *const usize = @ptrCast(@alignCast(original_ptr));
    const total_size = size_ptr.*;

    // Free with the same alignment used during allocation
    allocator.rawFree(original_ptr[0..total_size], ZSTD_ALLOC_ALIGNMENT, @returnAddress());
}

/// Errors that can occur during the bundle operation.
pub const BundleError = error{
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
    InvalidPath,
} || std.mem.Allocator.Error;

/// Where `bundle` reports the archive path that failed validation.
pub const ErrorContext = unbundle.ErrorContext;

/// A file to read from `base_dir` and the distinct portable path to store in
/// the archive.
pub const Entry = struct {
    source_path: []const u8,
    archive_path: []const u8,
};

/// The content-addressed archive filename and total size of its input files.
pub const Result = struct {
    filename: []u8,
    uncompressed_size: u64,
};

/// Bundle files into a compressed tar archive.
///
/// The entry iterator supplies a source path for `Dir.openFile` and a separate
/// archive path. Archive paths must satisfy `unbundle.pathHasUnbundleErr`, the
/// rules extraction enforces. On Windows, archive paths are converted to
/// forward slashes. Paths must be encoded as WTF-8 on Windows and UTF-8
/// elsewhere.
///
/// Compression level should be between 1 (fastest) and 22 (best compression).
/// Level 3 is a good default for speed/size tradeoff.
///
/// Returns the filename (base58-encoded blake3 hash + .tar.zst) and the total
/// input size. The caller must free `filename`.
/// If an InvalidPath error is returned, error_context will contain details about the invalid path.
pub fn bundle(
    entry_iter: anytype,
    compression_level: c_int,
    allocator: *std.mem.Allocator,
    io: std.Io,
    output_writer: *std.Io.Writer,
    base_dir: std.Io.Dir,
    error_context: ?*ErrorContext,
) BundleError!Result {
    // Create compressing hash writer that chains: tar → compress → hash → output
    var compress_writer = streaming_writer.CompressingHashWriter.init(
        allocator,
        compression_level,
        output_writer,
        allocForZstd,
        freeForZstd,
    ) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
    };
    defer compress_writer.deinit();

    // Create tar writer that writes to the compressing writer
    var tar_writer = std.tar.Writer{ .underlying_writer = &compress_writer.interface };
    var uncompressed_size: u64 = 0;

    // Process files one at a time
    while (try entry_iter.next()) |entry| {
        // Archive names use forward slashes only. Where a backslash is a path
        // separator (Windows) it is rewritten to one; elsewhere the validator
        // below refuses it.
        var normalized_tar_path: ?[]u8 = null;
        defer if (normalized_tar_path) |path| allocator.free(path);
        const tar_path = if (builtin.target.os.tag == .windows and std.mem.findScalar(u8, entry.archive_path, '\\') != null) blk: {
            const path = try allocator.dupe(u8, entry.archive_path);
            std.mem.replaceScalar(u8, path, '\\', '/');
            normalized_tar_path = path;
            break :blk path;
        } else entry.archive_path;

        // The writer's path rules are the reader's validator itself, with
        // nothing stricter layered on top: a path is written exactly when
        // extraction accepts it.
        if (unbundle.pathHasUnbundleErr(tar_path)) |validation_error| {
            // Report the caller-owned path rather than the temporary
            // forward-slash copy used on Windows.
            if (error_context) |ctx| ctx.* = .{ .path = entry.archive_path, .reason = validation_error.reason };
            return error.InvalidPath;
        }

        const file = base_dir.openFile(io, entry.source_path, .{}) catch |err| switch (err) {
            error.AntivirusInterference,
            error.BadPathName,
            error.Canceled,
            error.DeviceBusy,
            error.FileBusy,
            error.FileLocksUnsupported,
            error.FileTooBig,
            error.NameTooLong,
            error.NetworkNotFound,
            error.NoDevice,
            error.NoSpaceLeft,
            error.NotDir,
            error.PathAlreadyExists,
            error.PermissionDenied,
            error.PipeBusy,
            error.ProcessFdQuotaExceeded,
            error.ReadOnlyFileSystem,
            error.SymLinkLoop,
            error.SystemFdQuotaExceeded,
            error.SystemResources,
            error.Unexpected,
            error.WouldBlock,
            => return error.FileOpenFailed,
            error.FileNotFound => return error.FileNotFound,
            error.AccessDenied => return error.AccessDenied,
            error.IsDir => return error.IsDir,
        };
        defer file.close(io);

        const stat = file.stat(io) catch |err| switch (err) {
            error.AccessDenied,
            error.Canceled,
            error.PermissionDenied,
            error.Streaming,
            error.Unexpected,
            => return error.FileStatFailed,
            error.SystemResources => return error.SystemResources,
        };

        const file_size = std.math.cast(usize, stat.size) orelse return error.FileTooLarge;
        uncompressed_size = std.math.add(u64, uncompressed_size, stat.size) catch return error.FileTooLarge;

        // Write tar header and stream file content
        const Options = @TypeOf(tar_writer).Options;
        const options = Options{
            .mode = 0o644,
            .mtime = 0,
        };

        // Create a reader for the file
        var reader_buffer: [4096]u8 = undefined;
        var file_reader = file.reader(io, &reader_buffer);

        // Stream the file to tar
        tar_writer.writeFileStream(tar_path, file_size, &file_reader.interface, options) catch {
            return error.TarWriteFailed;
        };
    }

    // Finish the tar archive
    tar_writer.finishPedantically() catch {
        return error.TarWriteFailed;
    };

    // Finish compression, also flushes the writer
    compress_writer.finish() catch return error.WriteFailed;

    // Get the blake3 hash and encode as base58
    const hash = compress_writer.getHash();
    var base58_buffer: [base58.base58_hash_bytes]u8 = undefined;
    const base58_encoded = base58.encode(hash, &base58_buffer);
    const base58_hash = try allocator.*.dupe(u8, base58_encoded);
    defer allocator.*.free(base58_hash);

    // Create filename with .tar.zst extension
    const filename = try std.fmt.allocPrint(allocator.*, "{s}{s}", .{ base58_hash, TAR_EXTENSION });
    return .{ .filename = filename, .uncompressed_size = uncompressed_size };
}
