//! Download and extract bundled tar.zst files over https
//! (or http if the URL host is `localhost`, `127.0.0.1`, or `::1`)

const std = @import("std");
const base = @import("base");
const bundle = @import("bundle.zig");
const unbundle_mod = @import("unbundle");

const localhost = unbundle_mod.localhost;

// Network constants
const SERVER_HEADER_BUFFER_SIZE: usize = 16 * 1024;

pub const Version = unbundle_mod.download.Version;
pub const ParsedUrl = unbundle_mod.download.ParsedUrl;

/// Errors that can occur during the download operation.
pub const DownloadError = error{
    InvalidUrl,
    InvalidVersion,
    InvalidHash,
    HttpError,
    NoHashInUrl,
} || localhost.Error || bundle.UnbundleError || std.mem.Allocator.Error;

/// Parse URL and validate it meets our security requirements.
/// Returns the parsed hash and optional version from the URL if valid.
pub fn validateUrl(url: []const u8) DownloadError!ParsedUrl {
    if (!base.url.isSafeUrl(url)) {
        return error.InvalidUrl;
    }

    return unbundle_mod.download.parseUrlPath(url) catch |err| switch (err) {
        error.InvalidUrl => error.InvalidUrl,
        error.InvalidVersion => error.InvalidVersion,
        error.NoHashInUrl => error.NoHashInUrl,
    };
}

/// Download and extract a bundled tar.zst file from a URL.
///
/// The URL must:
/// - Start with "https://" or "http://127.0.0.1"
/// - Optionally have a MAJOR.MINOR.PATCH path segment before the hash
/// - Have the base58-encoded blake3 hash as the last path segment
/// - Point to a tar.zst file created with `roc bundle`
pub fn download(
    allocator: *std.mem.Allocator,
    io: std.Io,
    url: []const u8,
    extract_dir: std.Io.Dir,
) DownloadError!void {
    // Validate URL and extract hash
    const parsed_url = try validateUrl(url);
    const base58_hash = parsed_url.hash;

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
