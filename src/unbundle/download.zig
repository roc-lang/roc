//! Download and extract bundled tar.zst files over https
//! (or http if the URL host is `localhost`, `127.0.0.1`, or `::1`)

const std = @import("std");
const builtin = @import("builtin");
const unbundle = @import("unbundle.zig");

// Network constants
const HTTP_DEFAULT_PORT: u16 = 80;

// Buffer size for file I/O operations (8KB is efficient for typical filesystem block sizes)
const IO_BUFFER_SIZE: usize = 8 * 1024;

// IPv4 loopback address 127.0.0.1 in network byte order
const IPV4_LOOPBACK_BE: u32 = 0x7F000001; // Big-endian
const IPV4_LOOPBACK_LE: u32 = 0x0100007F; // Little-endian

// Maximum retries for temp file creation (handles rare collisions)
const MAX_TEMP_FILE_RETRIES: usize = 10;

// Length of random suffix for temp filenames
const RANDOM_SUFFIX_LEN: usize = 16;

/// Generate a random alphanumeric suffix for unique temp filenames.
/// Uses cryptographically secure random bytes mapped to alphanumeric characters.
fn generateRandomSuffix(buf: *[RANDOM_SUFFIX_LEN]u8) void {
    const charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
    std.crypto.random.bytes(buf);
    for (buf) |*byte| {
        byte.* = charset[byte.* % charset.len];
    }
}

/// Get a handle to the system temp directory.
/// Checks TMPDIR (Unix), TEMP, TMP environment variables, falls back to /tmp on Unix.
fn getTempDir() !std.fs.Dir {
    // Check TMPDIR first (standard on Unix)
    if (std.posix.getenv("TMPDIR")) |tmpdir| {
        return std.fs.cwd().openDir(tmpdir, .{}) catch return error.FileError;
    }

    // Check TEMP (common on Windows)
    if (std.posix.getenv("TEMP")) |temp| {
        return std.fs.cwd().openDir(temp, .{}) catch return error.FileError;
    }

    // Check TMP (fallback on Windows)
    if (std.posix.getenv("TMP")) |tmp| {
        return std.fs.cwd().openDir(tmp, .{}) catch return error.FileError;
    }

    // Fall back to /tmp on Unix-like systems
    if (comptime builtin.os.tag != .windows) {
        return std.fs.cwd().openDir("/tmp", .{}) catch return error.FileError;
    }

    return error.FileError;
}

/// Errors that can occur during the download operation.
pub const DownloadError = error{
    InvalidUrl,
    LocalhostWasNotLoopback,
    InvalidHash,
    HttpError,
    NoHashInUrl,
    NetworkError,
    FileError,
} || unbundle.UnbundleError || std.mem.Allocator.Error;

/// Parse URL and validate it meets our security requirements.
/// Returns the hash from the URL if valid.
pub fn validateUrl(url: []const u8) DownloadError![]const u8 {
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

    // Extract the last path segment (should be the hash)
    const last_slash = std.mem.lastIndexOf(u8, url, "/") orelse return error.NoHashInUrl;
    const hash_part = url[last_slash + 1 ..];

    // Remove .tar.zst extension if present
    const hash = if (std.mem.endsWith(u8, hash_part, ".tar.zst"))
        hash_part[0 .. hash_part.len - 8]
    else
        hash_part;

    if (hash.len == 0) {
        return error.NoHashInUrl;
    }

    return hash;
}

/// Download and extract a bundled tar.zst file from a URL.
///
/// The URL must:
/// - Start with "https://" or "http://127.0.0.1"
/// - Have the base58-encoded blake3 hash as the last path segment
/// - Point to a tar.zst file created with `roc bundle`
///
/// Downloads to a temp file first, then streams from that file for extraction.
pub fn downloadAndExtract(
    allocator: *std.mem.Allocator,
    url: []const u8,
    extract_dir: std.fs.Dir,
) DownloadError!void {
    // Validate URL and extract hash
    const base58_hash = try validateUrl(url);

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
    const temp_filename = try downloadToFile(allocator, url, extract_dir, base58_hash, &temp_filename_buf);

    // Open the downloaded file for reading
    var temp_file = extract_dir.openFile(temp_filename, .{}) catch {
        return error.FileError;
    };
    defer temp_file.close();

    // Create a buffered reader from the file
    var read_buffer: [IO_BUFFER_SIZE]u8 = undefined;
    var file_reader = temp_file.reader(&read_buffer);

    // Setup directory extract writer
    var dir_writer = unbundle.DirExtractWriter.init(extract_dir, allocator.*);
    defer dir_writer.deinit();

    // Extract the content using the streaming architecture
    unbundle.unbundleStream(allocator.*, &file_reader.interface, dir_writer.extractWriter(), &expected_hash, null) catch |err| {
        // Clean up temp file on error
        extract_dir.deleteFile(temp_filename) catch {};
        return err;
    };

    // Rename temp file to final name (keeps bundle cached)
    extract_dir.rename(temp_filename, filename) catch {
        // If rename fails, just delete the temp file
        extract_dir.deleteFile(temp_filename) catch {};
    };
}

/// Download HTTP response body to a file with a unique random suffix.
/// Creates a temp file with format: .{prefix}_{random16}.tmp
/// Uses exclusive file creation to prevent race conditions between processes.
/// Returns the actual filename created via the filename_out buffer.
fn downloadToFile(
    allocator: *std.mem.Allocator,
    url: []const u8,
    dir: std.fs.Dir,
    filename_prefix: []const u8,
    filename_out: []u8,
) DownloadError![]const u8 {
    // Create HTTP client
    var client = std.http.Client{ .allocator = allocator.* };
    defer client.deinit();

    // Parse the URL
    const uri = std.Uri.parse(url) catch return error.InvalidUrl;

    // Check if we need to resolve localhost and verify loopback
    if (uri.host) |host| {
        if (std.mem.eql(u8, host.percent_encoded, "localhost")) {
            // Security: We must resolve "localhost" and verify it points to a loopback address.
            const address_list = std.net.getAddressList(allocator.*, "localhost", uri.port orelse HTTP_DEFAULT_PORT) catch {
                return error.NetworkError;
            };
            defer address_list.deinit();

            if (address_list.addrs.len == 0) {
                return error.LocalhostWasNotLoopback;
            }

            // Check that at least one address is a loopback
            var found_loopback = false;
            for (address_list.addrs) |addr| {
                switch (addr.any.family) {
                    std.posix.AF.INET => {
                        const ipv4_addr = addr.in.sa.addr;
                        if (ipv4_addr == IPV4_LOOPBACK_BE or ipv4_addr == IPV4_LOOPBACK_LE) {
                            found_loopback = true;
                            break;
                        }
                    },
                    std.posix.AF.INET6 => {
                        const ipv6_addr = addr.in6.sa.addr;
                        var is_loopback = true;
                        for (ipv6_addr[0..15]) |byte| {
                            if (byte != 0) {
                                is_loopback = false;
                                break;
                            }
                        }
                        if (is_loopback and ipv6_addr[15] == 1) {
                            found_loopback = true;
                            break;
                        }
                    },
                    else => {},
                }
            }

            if (!found_loopback) {
                return error.LocalhostWasNotLoopback;
            }
        }
    }

    // Try to create temp file with unique random suffix
    var attempts: usize = 0;
    while (attempts < MAX_TEMP_FILE_RETRIES) : (attempts += 1) {
        // Generate random suffix for this attempt
        var random_suffix: [RANDOM_SUFFIX_LEN]u8 = undefined;
        generateRandomSuffix(&random_suffix);

        // Build filename: .{prefix}_{random}.tmp
        const filename = std.fmt.bufPrint(filename_out, ".{s}_{s}.tmp", .{
            filename_prefix,
            random_suffix,
        }) catch return error.FileError;

        // Try to create file with exclusive flag (fails if file already exists)
        var file = dir.createFile(filename, .{ .exclusive = true }) catch |err| switch (err) {
            error.PathAlreadyExists => continue, // Retry with new random suffix
            else => return error.FileError,
        };
        errdefer {
            file.close();
            dir.deleteFile(filename) catch {};
        }

        // Create a writer for the file
        var write_buffer: [IO_BUFFER_SIZE]u8 = undefined;
        var file_writer = file.writer(&write_buffer);

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
        file.close();

        // Check for successful response
        if (fetch_result.status != .ok) {
            dir.deleteFile(filename) catch {};
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
    url: []const u8,
) DownloadError!unbundle.BufferExtractWriter {
    // Validate URL and extract hash
    const base58_hash = try validateUrl(url);

    // Validate the hash before starting any I/O
    const expected_hash = (try unbundle.validateBase58Hash(base58_hash)) orelse {
        return error.InvalidHash;
    };

    // Use a temp directory for downloading
    var tmp_dir = getTempDir() catch {
        return error.FileError;
    };
    defer tmp_dir.close();

    // Build prefix for temp filename
    var prefix_buf: [64]u8 = undefined;
    const prefix = std.fmt.bufPrint(&prefix_buf, "roc_{s}", .{base58_hash}) catch {
        return error.InvalidHash;
    };

    // Download to temp file with unique random suffix
    var temp_filename_buf: [96]u8 = undefined;
    const temp_filename = try downloadToFile(allocator, url, tmp_dir, prefix, &temp_filename_buf);
    defer tmp_dir.deleteFile(temp_filename) catch {};

    // Open the downloaded file for reading
    var temp_file = tmp_dir.openFile(temp_filename, .{}) catch {
        return error.FileError;
    };
    defer temp_file.close();

    // Create a buffered reader from the file
    var read_buffer: [IO_BUFFER_SIZE]u8 = undefined;
    var file_reader = temp_file.reader(&read_buffer);

    // Setup buffer extract writer
    var buffer_writer = unbundle.BufferExtractWriter.init(allocator);
    errdefer buffer_writer.deinit();

    // Extract the content using the streaming architecture
    try unbundle.unbundleStream(allocator.*, &file_reader.interface, buffer_writer.extractWriter(), &expected_hash, null);

    return buffer_writer;
}
