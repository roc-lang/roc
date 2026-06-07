//! Download and extract bundled tar.zst files over https
//! (or http if the URL host is `localhost`, `127.0.0.1`, or `::1`)

const std = @import("std");
const base = @import("base");
const bundle = @import("bundle.zig");
const unbundle_mod = @import("unbundle");

const localhost = unbundle_mod.localhost;

// Network constants
const SERVER_HEADER_BUFFER_SIZE: usize = 16 * 1024;

/// Errors that can occur during the download operation.
pub const DownloadError = error{
    InvalidUrl,
    InvalidHash,
    HttpError,
    NoHashInUrl,
} || localhost.Error || bundle.UnbundleError || std.mem.Allocator.Error;

/// Parse URL and validate it meets our security requirements.
/// Returns the hash from the URL if valid.
pub fn validateUrl(url: []const u8) DownloadError![]const u8 {
    if (!base.url.isSafeUrl(url)) {
        return error.InvalidUrl;
    }

    // Extract the last path segment (should be the hash)
    const last_slash = std.mem.findLast(u8, url, "/") orelse return error.NoHashInUrl;
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
pub fn download(
    allocator: *std.mem.Allocator,
    io: std.Io,
    url: []const u8,
    extract_dir: std.Io.Dir,
) DownloadError!void {
    // Validate URL and extract hash
    const base58_hash = try validateUrl(url);

    // Validate the hash before starting any I/O
    const expected_hash = (try bundle.validateBase58Hash(base58_hash)) orelse {
        return error.InvalidHash;
    };

    // Create HTTP client
    var client = std.http.Client{ .allocator = allocator.*, .io = io };
    defer client.deinit();

    // Parse the URL
    var uri = std.Uri.parse(url) catch return error.InvalidUrl;

    // Check if we need to resolve localhost
    var extra_headers: []const std.http.Header = &.{};
    if (uri.host) |host| {
        if (std.mem.eql(u8, host.percent_encoded, "localhost")) {
            const family = try localhost.resolveLoopback();
            uri.host = switch (family) {
                .ip4 => .{ .percent_encoded = "127.0.0.1" },
                .ip6 => .{ .percent_encoded = "[::1]" },
            };
            // Set Host header to preserve original hostname
            extra_headers = &.{
                .{ .name = "Host", .value = "localhost" },
            };
        }
    }

    // Start the request with the potentially modified URI
    var request = client.request(.GET, uri, .{
        .redirect_behavior = .unhandled,
        .extra_headers = extra_headers,
    }) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.HttpError,
    };
    defer request.deinit();

    // Send just the request head (no body)
    request.sendBodiless() catch return error.HttpError;

    // Receive headers into a temporary buffer
    var head_buffer: [SERVER_HEADER_BUFFER_SIZE]u8 = undefined;
    var response = request.receiveHead(&head_buffer) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.HttpError,
    };

    // Check response status
    if (response.head.status != .ok) {
        return error.HttpError;
    }

    // Prepare buffered reader for response body
    var reader_buffer: [1024]u8 = undefined;
    const reader = response.reader(&reader_buffer);

    // Stream directly to unbundleStream
    var dir_writer = bundle.DirExtractWriter.init(extract_dir, io);
    try bundle.unbundleStream(reader, dir_writer.extractWriter(), allocator, &expected_hash, null);
}
