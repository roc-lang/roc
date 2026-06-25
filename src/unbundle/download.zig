//! Download and extract bundled tar.zst files over https
//! (or http if the URL host is `localhost`, `127.0.0.1`, or `::1`)

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const base = @import("base");
const unbundle = @import("unbundle.zig");
const localhost = @import("localhost.zig");

// Buffer size for file I/O operations (8KB is efficient for typical filesystem block sizes)
const IO_BUFFER_SIZE: usize = 8 * 1024;

// Maximum retries for temp file creation (handles rare collisions)
const MAX_TEMP_FILE_RETRIES: usize = 10;

// Length of random suffix for temp filenames
const RANDOM_SUFFIX_LEN: usize = 16;

/// Generate a random alphanumeric suffix for unique temp filenames.
/// Uses cryptographically secure random bytes mapped to alphanumeric characters.
fn generateRandomSuffix(io: std.Io, buf: *[RANDOM_SUFFIX_LEN]u8) void {
    const charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
    io.random(buf);
    for (buf) |*byte| {
        byte.* = charset[byte.* % charset.len];
    }
}

/// Get a handle to the system temp directory.
/// Checks TMPDIR (Unix), TEMP, TMP environment variables, falls back to /tmp on Unix.
fn getTempDir(allocator: std.mem.Allocator, io: std.Io) Allocator.Error!std.Io.Dir {
    // Try a named env var; returns an opened dir or null if env var is unset.
    const tryEnv = struct {
        fn call(alloc: std.mem.Allocator, io_inner: std.Io, name: []const u8) Allocator.Error!?std.Io.Dir {
            const path = std.process.getEnvVarOwned(alloc, name) catch |err| switch (err) {
                error.EnvironmentVariableNotFound => return null,
                error.InvalidWtf8 => return null,
                error.OutOfMemory => return error.OutOfMemory,
            };
            defer alloc.free(path);
            return std.Io.Dir.cwd().openDir(io_inner, path, .{}) catch return error.FileError;
        }
    }.call;

    // Check TMPDIR first (standard on Unix)
    if (try tryEnv(allocator, io, "TMPDIR")) |dir| return dir;

    // Check TEMP (common on Windows)
    if (try tryEnv(allocator, io, "TEMP")) |dir| return dir;

    // Check TMP (fallback on Windows)
    if (try tryEnv(allocator, io, "TMP")) |dir| return dir;

    // Fall back to /tmp on Unix-like systems
    if (comptime builtin.os.tag != .windows) {
        return std.Io.Dir.cwd().openDir(io, "/tmp", .{}) catch return error.FileError;
    }

    return error.FileError;
}

/// Errors that can occur during the download operation.
pub const DownloadError = error{
    InvalidUrl,
    InvalidVersion,
    LocalhostWasNotLoopback,
    InvalidHash,
    HttpError,
    NoHashInUrl,
    NetworkError,
    FileError,
} || unbundle.UnbundleError || std.mem.Allocator.Error;

pub const Version = base.url.Version;

pub const ParsedUrl = base.url.ParsedUrl;

pub const parseUrlPath = base.url.parseUrlPath;

/// Parse URL and validate it meets our security requirements.
/// Returns the parsed hash and optional version from the URL if valid.
pub fn validateUrl(url: []const u8) DownloadError!ParsedUrl {
    // Check for https:// prefix
    if (std.mem.startsWith(u8, url, "https://")) {
        // This is fine, extract hash from last segment
    } else if (std.mem.startsWith(u8, url, "http://127.0.0.1:") or std.mem.startsWith(u8, url, "http://127.0.0.1/")) {
        // This is allowed for local testing (IPv4 loopback)
    } else if (std.mem.startsWith(u8, url, "http://[::1]:") or std.mem.startsWith(u8, url, "http://[::1]/")) {
        // This is allowed for local testing (IPv6 loopback)
    } else if (std.mem.startsWith(u8, url, "http://localhost:") or std.mem.startsWith(u8, url, "http://localhost/")) {
        // This is allowed but will require verification that localhost resolves to loopback
    } else {
        return error.InvalidUrl;
    }

    return parseUrlPath(url);
}

/// Options controlling download and extraction.
pub const DownloadOptions = struct {
    /// Maximum allowed decompressed size of the bundle in bytes, or null for
    /// no limit.
    max_expanded_bytes: ?u64 = null,
};

/// Download and extract a bundled tar.zst file from a URL.
/// Returns the total decompressed size of the bundle in bytes.
///
/// The URL must:
/// - Start with "https://" or "http://127.0.0.1"
/// - Optionally have a MAJOR.MINOR.PATCH path segment before the hash
/// - Have the base58-encoded blake3 hash as the last path segment
/// - Point to a tar.zst file created with `roc bundle`
///
/// `dest_path` must be an existing directory path. Downloads to a temp file
/// first, then streams from that file for extraction.
pub fn downloadAndExtract(
    allocator: *std.mem.Allocator,
    io: std.Io,
    url: []const u8,
    dest_path: []const u8,
    options: DownloadOptions,
) DownloadError!u64 {
    var extract_dir = std.Io.Dir.cwd().openDir(io, dest_path, .{}) catch return error.FileError;
    defer extract_dir.close(io);

    // Validate URL and extract hash
    const parsed_url = try validateUrl(url);
    const base58_hash = parsed_url.hash;

    // Validate the hash before starting any I/O
    const expected_hash = (try unbundle.validateBase58Hash(base58_hash)) orelse {
        return error.InvalidHash;
    };

    // Construct the final filename (for caching after successful extraction)
    var filename_buf: [64]u8 = undefined;
    const filename = std.fmt.bufPrint(&filename_buf, "{s}.tar.zst", .{base58_hash}) catch {
        return error.InvalidHash;
    };

    // Download to a temp file with unique random suffix
    // Buffer size: . + hash(~44) + _ + random(16) + .tmp + null = ~70 bytes, use 96 for safety
    var temp_filename_buf: [96]u8 = undefined;
    const temp_filename = try downloadToFile(allocator, io, url, extract_dir, base58_hash, &temp_filename_buf);

    // Open the downloaded file for reading
    var temp_file = extract_dir.openFile(io, temp_filename, .{}) catch {
        return error.FileError;
    };
    defer temp_file.close(io);

    // Create a buffered reader from the file
    var read_buffer: [IO_BUFFER_SIZE]u8 = undefined;
    var file_reader = temp_file.reader(io, &read_buffer);

    // Setup directory extract writer
    var dir_writer = unbundle.DirExtractWriter.init(extract_dir, io, allocator.*);
    defer dir_writer.deinit();

    // Extract the content using the streaming architecture
    const expanded_bytes = unbundle.unbundleStream(allocator.*, &file_reader.interface, dir_writer.extractWriter(), &expected_hash, null, .{
        .max_expanded_bytes = options.max_expanded_bytes,
    }) catch |err| {
        // Clean up temp file on error
        extract_dir.deleteFile(io, temp_filename) catch {};
        return err;
    };

    // Rename temp file to final name (keeps bundle cached)
    extract_dir.rename(temp_filename, extract_dir, filename, io) catch {
        // If rename fails, just delete the temp file
        extract_dir.deleteFile(io, temp_filename) catch {};
    };

    return expanded_bytes;
}

/// Download HTTP response body to a file with a unique random suffix.
/// Creates a temp file with format: .{prefix}_{random16}.tmp
/// Uses exclusive file creation to prevent race conditions between processes.
/// Returns the actual filename created via the filename_out buffer.
fn downloadToFile(
    allocator: *std.mem.Allocator,
    io: std.Io,
    url: []const u8,
    dir: std.Io.Dir,
    filename_prefix: []const u8,
    filename_out: []u8,
) DownloadError![]const u8 {
    // Create HTTP client
    var client = std.http.Client{ .allocator = allocator.*, .io = io };
    defer client.deinit();

    // Parse the URL
    const uri = std.Uri.parse(url) catch return error.InvalidUrl;

    // Check if we need to resolve localhost and verify loopback
    if (uri.host) |host| {
        if (std.mem.eql(u8, host.percent_encoded, "localhost")) {
            // Security: resolve "localhost" and require at least one loopback result.
            try localhost.requireLoopback();
        }
    }

    // Try to create temp file with unique random suffix
    var attempts: usize = 0;
    while (attempts < MAX_TEMP_FILE_RETRIES) : (attempts += 1) {
        // Generate random suffix for this attempt
        var random_suffix: [RANDOM_SUFFIX_LEN]u8 = undefined;
        generateRandomSuffix(io, &random_suffix);

        // Build filename: .{prefix}_{random}.tmp
        const filename = std.fmt.bufPrint(filename_out, ".{s}_{s}.tmp", .{
            filename_prefix,
            random_suffix,
        }) catch return error.FileError;

        // Try to create file with exclusive flag (fails if file already exists)
        var file = dir.createFile(io, filename, .{ .exclusive = true }) catch |err| switch (err) {
            error.PathAlreadyExists => continue, // Retry with new random suffix
            else => return error.FileError,
        };
        var file_closed = false;
        errdefer {
            if (!file_closed) file.close(io);
            dir.deleteFile(io, filename) catch {};
        }

        // Create a writer for the file
        var write_buffer: [IO_BUFFER_SIZE]u8 = undefined;
        var file_writer = file.writer(io, &write_buffer);

        // Use fetch API with response_writer to write directly to file
        const fetch_result = client.fetch(.{
            .location = .{ .uri = uri },
            .response_writer = &file_writer.interface,
        }) catch {
            return error.HttpError;
        };

        // Flush the writer before closing
        file_writer.interface.flush() catch {
            return error.FileError;
        };

        // Close file after fetch completes
        file.close(io);
        file_closed = true;

        // Check for successful response
        if (fetch_result.status != .ok) {
            dir.deleteFile(io, filename) catch {};
            return error.HttpError;
        }

        return filename;
    }

    // Exhausted all retries (extremely unlikely with 16-char random suffix)
    return error.FileError;
}

/// Download and extract a bundled tar.zst file to memory buffers.
///
/// Returns a BufferExtractWriter containing all extracted files and directories.
/// The caller owns the returned writer and must call deinit() on it.
pub fn downloadAndExtractToBuffer(
    allocator: *std.mem.Allocator,
    io: std.Io,
    url: []const u8,
    options: DownloadOptions,
) DownloadError!unbundle.BufferExtractWriter {
    // Validate URL and extract hash
    const parsed_url = try validateUrl(url);
    const base58_hash = parsed_url.hash;

    // Validate the hash before starting any I/O
    const expected_hash = (try unbundle.validateBase58Hash(base58_hash)) orelse {
        return error.InvalidHash;
    };

    // Use a temp directory for downloading
    var tmp_dir = getTempDir(allocator.*, io) catch {
        return error.FileError;
    };
    defer tmp_dir.close(io);

    // Build prefix for temp filename
    var prefix_buf: [64]u8 = undefined;
    const prefix = std.fmt.bufPrint(&prefix_buf, "roc_{s}", .{base58_hash}) catch {
        return error.InvalidHash;
    };

    // Download to temp file with unique random suffix
    var temp_filename_buf: [96]u8 = undefined;
    const temp_filename = try downloadToFile(allocator, io, url, tmp_dir, prefix, &temp_filename_buf);
    defer tmp_dir.deleteFile(io, temp_filename) catch {};

    // Open the downloaded file for reading
    var temp_file = tmp_dir.openFile(io, temp_filename, .{}) catch {
        return error.FileError;
    };
    defer temp_file.close(io);

    // Create a buffered reader from the file
    var read_buffer: [IO_BUFFER_SIZE]u8 = undefined;
    var file_reader = temp_file.reader(io, &read_buffer);

    // Setup buffer extract writer
    var buffer_writer = unbundle.BufferExtractWriter.init(allocator);
    errdefer buffer_writer.deinit();

    // Extract the content using the streaming architecture
    _ = try unbundle.unbundleStream(allocator.*, &file_reader.interface, buffer_writer.extractWriter(), &expected_hash, null, .{
        .max_expanded_bytes = options.max_expanded_bytes,
    });

    return buffer_writer;
}
