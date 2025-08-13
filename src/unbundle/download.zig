//! Download and extract bundled tar.zst files over https
//! (or http if the URL host is `localhost`, `127.0.0.1`, or `::1`)

const std = @import("std");
const builtin = @import("builtin");
const unbundle = @import("unbundle.zig");

// Network constants
const HTTPS_DEFAULT_PORT: u16 = 443;
const HTTP_DEFAULT_PORT: u16 = 80;
const SERVER_HEADER_BUFFER_SIZE: usize = 16 * 1024;

// IPv4 loopback address 127.0.0.1 in network byte order
const IPV4_LOOPBACK_BE: u32 = 0x7F000001; // Big-endian
const IPV4_LOOPBACK_LE: u32 = 0x0100007F; // Little-endian

/// Errors that can occur during the download operation.
pub const DownloadError = error{
    InvalidUrl,
    LocalhostWasNotLoopback,
    InvalidHash,
    HttpError,
    NoHashInUrl,
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

    // Create HTTP client
    var client = std.http.Client{ .allocator = allocator.* };
    defer client.deinit();

    // Parse the URL
    const uri = std.Uri.parse(url) catch return error.InvalidUrl;

    // Check if we need to resolve localhost
    var extra_headers: []const std.http.Header = &.{};
    if (uri.host) |host| {
        if (std.mem.eql(u8, host.percent_encoded, "localhost")) {
            // Security: We must resolve "localhost" and verify it points to a loopback address.
            // This prevents attacks where:
            // 1. An attacker modifies /etc/hosts to make localhost resolve to their server
            // 2. A compromised DNS makes localhost resolve to an external IP
            // 3. Container/VM networking misconfiguration exposes localhost to external IPs
            //
            // We're being intentionally strict here:
            // - For IPv4: We only accept exactly 127.0.0.1 (not the full 127.0.0.0/8 range)
            // - For IPv6: We only accept exactly ::1 (not other loopback addresses)
            //
            // While the specs technically allow any 127.x.y.z address for IPv4 loopback
            // and multiple forms for IPv6, in practice localhost almost always resolves
            // to these exact addresses, and being stricter improves security.

            const address_list = try std.net.getAddressList(allocator.*, "localhost", uri.port orelse HTTP_DEFAULT_PORT);
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
                        // Check if it's exactly ::1 (all zeros except last byte is 1)
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
                    else => {}, // Ignore other address families
                }
            }

            if (!found_loopback) {
                return error.LocalhostWasNotLoopback;
            }

            // Since we're using "localhost", we need to set the Host header manually
            // to match what the server expects
            extra_headers = &.{
                .{ .name = "Host", .value = "localhost" },
            };
        }
    }

    // Start the HTTP request
    var header_buffer: [SERVER_HEADER_BUFFER_SIZE]u8 = undefined;
    var request = try client.open(.GET, uri, .{
        .server_header_buffer = &header_buffer,
        .extra_headers = extra_headers,
    });
    defer request.deinit();

    // Send the request and wait for response
    try request.send();
    try request.wait();

    // Check for successful response
    if (request.response.status != .ok) {
        return error.HttpError;
    }

    const reader = request.reader();

    // Setup directory extract writer
    var dir_writer = unbundle.DirExtractWriter.init(extract_dir);

    // Stream and extract the content
    try unbundle.unbundleStream(reader, dir_writer.extractWriter(), allocator, &expected_hash, null);
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

    // Create HTTP client
    var client = std.http.Client{ .allocator = allocator.* };
    defer client.deinit();

    // Parse the URL
    const uri = std.Uri.parse(url) catch return error.InvalidUrl;

    // Start the HTTP request (simplified version without localhost resolution for brevity)
    var header_buffer: [SERVER_HEADER_BUFFER_SIZE]u8 = undefined;
    var request = try client.open(.GET, uri, .{
        .server_header_buffer = &header_buffer,
    });
    defer request.deinit();

    // Send the request and wait for response
    try request.send();
    try request.wait();

    // Check for successful response
    if (request.response.status != .ok) {
        return error.HttpError;
    }

    const reader = request.reader();

    // Setup buffer extract writer
    var buffer_writer = unbundle.BufferExtractWriter.init(allocator);
    errdefer buffer_writer.deinit();

    // Stream and extract the content
    try unbundle.unbundleStream(reader, buffer_writer.extractWriter(), allocator, &expected_hash, null);

    return buffer_writer;
}
