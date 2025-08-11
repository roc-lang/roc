//! Download and extract bundled tar.zst files over https
//! (or http if the URL host is `localhost`, `127.0.0.1`, or `::1`)

const std = @import("std");
const builtin = @import("builtin");
const bundle = @import("bundle.zig");

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
} || bundle.UnbundleError || std.mem.Allocator.Error;

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
pub fn download(
    allocator: *std.mem.Allocator,
    url: []const u8,
    extract_dir: std.fs.Dir,
) DownloadError!void {
    // Validate URL and extract hash
    const base58_hash = try validateUrl(url);

    // Validate the hash before starting any I/O
    const expected_hash = (try bundle.validateBase58Hash(base58_hash)) orelse {
        return error.InvalidHash;
    };

    // Create HTTP client
    var client = std.http.Client{ .allocator = allocator.* };
    defer client.deinit();

    // Parse the URL
    var uri = std.Uri.parse(url) catch return error.InvalidUrl;

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
            // to exactly 127.0.0.1 or ::1. By being conservative, we:
            // 1. Match real-world usage patterns (no practical downside)
            // 2. Avoid potential edge cases in networking stack implementations
            // 3. Reduce attack surface by accepting only the most common values

            const port = uri.port orelse (if (std.mem.eql(u8, uri.scheme, "https")) HTTPS_DEFAULT_PORT else HTTP_DEFAULT_PORT);

            const address_list = std.net.getAddressList(allocator.*, "localhost", port) catch {
                return error.LocalhostWasNotLoopback;
            };
            defer address_list.deinit();

            if (address_list.addrs.len == 0) {
                return error.LocalhostWasNotLoopback;
            }

            // Take the first address and verify it's loopback
            const first_addr = address_list.addrs[0];
            const is_loopback = switch (first_addr.any.family) {
                std.posix.AF.INET => blk: {
                    // Check if IPv4 address is exactly 127.0.0.1
                    const addr = first_addr.in.sa.addr;
                    // IPv4 addresses are in network byte order (big-endian)
                    const expected = if (comptime builtin.cpu.arch.endian() == .little)
                        IPV4_LOOPBACK_LE
                    else
                        IPV4_LOOPBACK_BE;
                    break :blk addr == expected;
                },
                std.posix.AF.INET6 => blk: {
                    // Check if IPv6 address is ::1
                    const addr = first_addr.in6.sa.addr;
                    for (addr[0..15]) |byte| {
                        if (byte != 0) break :blk false;
                    }
                    break :blk addr[15] == 1;
                },
                else => false,
            };

            if (!is_loopback) {
                return error.LocalhostWasNotLoopback;
            }

            // Update the URI to use the resolved IP instead of "localhost"
            // We need to format the address correctly
            if (first_addr.any.family == std.posix.AF.INET) {
                // IPv4: just use "127.0.0.1" as the host
                uri.host = .{ .percent_encoded = "127.0.0.1" };
            } else {
                // IPv6: use "[::1]" as the host
                uri.host = .{ .percent_encoded = "[::1]" };
            }

            // Set Host header to preserve original hostname
            extra_headers = &.{
                .{ .name = "Host", .value = "localhost" },
            };
        }
    }

    // Start the request with the potentially modified URI
    var server_header_buffer: [SERVER_HEADER_BUFFER_SIZE]u8 = undefined;
    var request = client.open(.GET, uri, .{
        .server_header_buffer = &server_header_buffer,
        .redirect_behavior = .unhandled,
        .extra_headers = extra_headers,
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
    var dir_writer = bundle.DirExtractWriter.init(extract_dir);
    try bundle.unbundleStream(reader, dir_writer.extractWriter(), allocator, &expected_hash);
}
