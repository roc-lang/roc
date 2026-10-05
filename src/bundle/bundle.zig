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
const format = @import("unbundle").format;
const c = @cImport({
    @cDefine("ZSTD_STATIC_LINKING_ONLY", "1");
    @cInclude("zstd.h");
});

// Constants for magic numbers
const SIZE_STORAGE_BYTES: usize = 16; // Extra bytes for storing allocation size; use 16 to preserve alignment.
/// Alignment for zstd custom allocations. Must match SIZE_STORAGE_BYTES (16 bytes).
const ZSTD_ALLOC_ALIGNMENT: std.mem.Alignment = .@"16";
const TAR_PATH_MAX_LENGTH: usize = 255; // Maximum path length for tar compatibility
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
    InvalidPath,
} || std.mem.Allocator.Error;

/// Context for error reporting during bundle operations
pub const ErrorContext = struct {
    path: []const u8,
    reason: PathValidationReason,
};

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
/// archive path. Archive paths must be relative, must not contain `..`
/// components, and are limited to 255 bytes for tar compatibility. On Windows,
/// archive paths are converted to forward slashes. Paths must be encoded as
/// WTF-8 on Windows and UTF-8 elsewhere.
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
        // Standardize archive names on forward slashes. Valid Unix source
        // paths can contain backslashes, but archive names remain portable.
        var normalized_tar_path: ?[]u8 = null;
        defer if (normalized_tar_path) |path| allocator.free(path);
        const has_backslash = std.mem.find(u8, entry.archive_path, "\\") != null;
        const tar_path = if (builtin.target.os.tag == .windows and has_backslash) blk: {
            const path = try allocator.dupe(u8, entry.archive_path);
            std.mem.replaceScalar(u8, path, '\\', '/');
            normalized_tar_path = path;
            break :blk path;
        } else if (!has_backslash) entry.archive_path else {
            if (error_context) |ctx| {
                ctx.path = entry.archive_path;
                ctx.reason = .contained_backslash_on_unix;
            }
            return error.InvalidPath;
        };

        if (pathHasBundleErr(tar_path)) |validation_error| {
            if (error_context) |ctx| {
                // Keep the caller-owned path rather than the temporary
                // forward-slash copy used on Windows.
                ctx.path = entry.archive_path;
                ctx.reason = validation_error.reason;
            }
            return error.InvalidPath;
        }

        if (tar_path.len > TAR_PATH_MAX_LENGTH) {
            return error.FilePathTooLong;
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

/// Characters that are reserved/illegal in file paths on various operating systems.
/// We disallow all of these to ensure cross-platform compatibility and security.
const RESERVED_PATH_CHARS = [_]u8{
    0, // NUL (disallowed on all systems)
    ':', // Drive separator on Windows, used in Mac OS classic
    '*', // Wildcard on Windows
    '?', // Wildcard on Windows
    '"', // Quote character on Windows
    '<', // Redirection on Windows
    '>', // Redirection on Windows
    '|', // Pipe on Windows
};

/// Windows reserved filenames (case-insensitive)
const WINDOWS_RESERVED_NAMES = [_][]const u8{
    "CON",  "PRN",  "AUX",  "NUL",
    "COM1", "COM2", "COM3", "COM4",
    "COM5", "COM6", "COM7", "COM8",
    "COM9", "LPT1", "LPT2", "LPT3",
    "LPT4", "LPT5", "LPT6", "LPT7",
    "LPT8", "LPT9",
};

/// Specific reason why a path validation failed
pub const PathValidationReason = union(enum) {
    empty_path,
    path_too_long,
    windows_reserved_char: u8,
    absolute_path,
    path_traversal,
    current_directory_reference,
    windows_reserved_name,
    contained_backslash_on_unix,
    component_ends_with_space,
    component_ends_with_period,
};

/// Error type for path validation failures
pub const PathValidationError = struct {
    path: []const u8,
    reason: PathValidationReason,
};

/// Validates a path for bundling, checking for cross-platform compatibility issues
///
/// We only do these validations on bundle, not on unbundle.
/// Note that the path ALREADY should have all backslashes converted
/// to forward slashes.
///
/// The reason we do this validation is to prevent Windows users
/// from encountering unpleasant surprises when they try to
/// unbundle paths that bundled just fine on a non-Windows OS but.
/// which are invalid on Windows.
///
/// We don't do the validation on unbundle because it's costly and
/// there's no security concern; if the OS doesn't accept the path,
/// it will give an error.
pub fn pathHasBundleErr(path: []const u8) ?PathValidationError {
    std.debug.assert(std.mem.find(u8, path, "\\") == null);

    // Start by doing the validation checks we'd do on unbundle.
    // If unbundling would fail, then bundling should too!
    if (pathHasUnbundleErr(path)) |err| {
        return err;
    }

    // Check for reserved characters
    for (path) |byte| {
        inline for (RESERVED_PATH_CHARS) |reserved| {
            if (byte == reserved) {
                return PathValidationError{
                    .path = path,
                    .reason = .{ .windows_reserved_char = reserved },
                };
            }
        }
    }

    // Check each path component for Windows reserved names and trailing spaces/periods
    var component_iter = std.mem.tokenizeScalar(u8, path, '/');

    while (component_iter.next()) |component| {
        // Check for Windows reserved names (case-insensitive)
        for (WINDOWS_RESERVED_NAMES) |reserved| {
            // Check base name without extension
            const dot_pos = std.mem.findScalar(u8, component, '.');
            const base_name = if (dot_pos) |pos| component[0..pos] else component;

            if (base_name.len == reserved.len) {
                var matches = true;
                for (base_name, reserved) |a, b| {
                    if (std.ascii.toUpper(a) != b) {
                        matches = false;
                        break;
                    }
                }
                if (matches) {
                    return PathValidationError{
                        .path = path,
                        .reason = .windows_reserved_name,
                    };
                }
            }
        }

        // Reject components ending with space or period (Windows restriction)
        if (component.len > 0) {
            const last_char = component[component.len - 1];
            if (last_char == ' ') {
                return PathValidationError{
                    .path = path,
                    .reason = .component_ends_with_space,
                };
            } else if (last_char == '.') {
                return PathValidationError{
                    .path = path,
                    .reason = .component_ends_with_period,
                };
            }
        }
    }

    return null;
}

/// Validate a file path to prevent directory traversal attacks and other security issues.
/// Returns null if the path is valid, or a PathValidationError describing the problem.
pub fn pathHasUnbundleErr(path: []const u8) ?PathValidationError {
    // Reject empty paths
    if (path.len == 0) {
        return PathValidationError{
            .path = path,
            .reason = .empty_path,
        };
    }

    // Reject paths that are too long for tar format
    if (path.len > TAR_PATH_MAX_LENGTH) {
        return PathValidationError{
            .path = path,
            .reason = .path_too_long,
        };
    }

    // Reject paths considered absolute on any OS we support
    if (std.fs.path.isAbsolutePosix(path) or std.fs.path.isAbsoluteWindows(path)) {
        return PathValidationError{
            .path = path,
            .reason = .absolute_path,
        };
    }

    // Check for ".." and "." path components
    var idx: usize = 0;
    var component_start: usize = 0;

    while (idx <= path.len) {
        // Check if we're at a separator or the end
        const at_separator = idx < path.len and (path[idx] == '/' or path[idx] == '\\');
        const at_end = idx == path.len;

        if (at_separator or at_end) {
            if (idx > component_start) {
                const component = path[component_start..idx];

                // Check for "." component
                if (std.mem.eql(u8, component, ".")) {
                    return PathValidationError{
                        .path = path,
                        .reason = .current_directory_reference,
                    };
                }

                // Check for ".." component
                if (std.mem.eql(u8, component, "..")) {
                    return PathValidationError{
                        .path = path,
                        .reason = .path_traversal,
                    };
                }
            }

            if (at_separator) {
                component_start = idx + 1;
            }
        }

        if (!at_end) {
            idx += 1;
        } else {
            break;
        }
    }

    return null;
}
