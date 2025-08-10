//! Download and extract bundled tar.zst files over HTTPS

const std = @import("std");
const bundle = @import("bundle.zig");

pub const DownloadError = error{
    InvalidUrl,
    AttemptedLocalhost, // User should use 127.0.0.1 instead, since that is guaranteed to stay on the machine and is not an alias that can be overridden
    InvalidHash,
    HttpError,
    NoHashInUrl,
} || bundle.UnbundleError || std.mem.Allocator.Error;

/// Parse URL and validate it meets our security requirements.
/// Returns the hash from the URL if valid.
pub fn validateUrl(url: []const u8) DownloadError![]const u8 {
    // Check for https:// prefix
    if (std.mem.startsWith(u8, url, "https://")) {
        // This is fine, extract hash from last segment
    } else if (std.mem.startsWith(u8, url, "http://127.0.0.1:") or std.mem.startsWith(u8, url, "http://127.0.0.1/")) {
        // This is allowed for local testing
    } else if (std.mem.startsWith(u8, url, "http://localhost:") or std.mem.startsWith(u8, url, "http://localhost/")) {
        return error.AttemptedLocalhost;
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
pub fn download(
    allocator: std.mem.Allocator,
    url: []const u8,
    extract_dir: std.fs.Dir,
) DownloadError!void {
    // Validate URL and extract hash
    const base58_hash = try validateUrl(url);
    
    // Validate the hash before starting any I/O
    const expected_hash = (try bundle.validateBase58Hash(allocator, base58_hash)) orelse {
        return error.InvalidHash;
    };
    
    
    // Create HTTP client
    var client = std.http.Client{ .allocator = allocator };
    defer client.deinit();
    
    // Parse the URL
    const uri = std.Uri.parse(url) catch return error.InvalidUrl;
    
    // Start the request
    var server_header_buffer: [16 * 1024]u8 = undefined;
    var request = client.open(.GET, uri, .{
        .server_header_buffer = &server_header_buffer,
        .redirect_behavior = .unhandled,
    }) catch return error.HttpError;
    defer request.deinit();
    
    // Send the request
    request.send() catch return error.HttpError;
    request.finish() catch return error.HttpError;
    request.wait() catch return error.HttpError;
    
    // Check response status
    if (request.response.status != .ok) {
        return error.HttpError;
    }
    
    // Get the response reader
    const reader = request.reader();
    
    // Stream directly to unbundleStream
    try bundle.unbundleStream(reader, extract_dir, allocator, &expected_hash);
}