//! Download and extract bundled tar.zst files over https
//! (or http if the URL host is `localhost`, `127.0.0.1`, or `::1`)

const std = @import("std");
const base = @import("base");
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
    if (!base.url.isSafeUrl(url)) {
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

            // Resolve "localhost" using getaddrinfo and verify it's a loopback address
            const AF_INET: i32 = 2;
            const AF_INET6: i32 = if (builtin.os.tag == .linux) 10 else 30;

            var result: ?*std.c.addrinfo = null;
            const rc = std.c.getaddrinfo("localhost", null, null, &result);
            if (@intFromEnum(rc) != 0 or result == null) {
                return error.LocalhostWasNotLoopback;
            }
            defer std.c.freeaddrinfo(result.?);

            // Check if the resolved address is a loopback address
            const addr_info = result.?;
            const is_loopback = if (addr_info.family == AF_INET) blk: {
                const sockaddr_in: *const std.posix.sockaddr.in = @ptrCast(@alignCast(addr_info.addr.?));
                const addr = sockaddr_in.addr;
                const expected: u32 = if (comptime builtin.cpu.arch.endian() == .little)
                    IPV4_LOOPBACK_LE
                else
                    IPV4_LOOPBACK_BE;
                break :blk addr == expected;
            } else if (addr_info.family == AF_INET6) blk: {
                const sockaddr_in6: *const std.posix.sockaddr.in6 = @ptrCast(@alignCast(addr_info.addr.?));
                const addr = sockaddr_in6.addr;
                for (addr[0..15]) |byte| {
                    if (byte != 0) break :blk false;
                }
                break :blk addr[15] == 1;
            } else false;

            if (!is_loopback) {
                return error.LocalhostWasNotLoopback;
            }

            // Update the URI to use the resolved IP instead of "localhost"
            if (addr_info.family == AF_INET) {
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
    var request = client.request(.GET, uri, .{
        .redirect_behavior = .unhandled,
        .extra_headers = extra_headers,
    }) catch return error.HttpError;
    defer request.deinit();

    // Send just the request head (no body)
    request.sendBodiless() catch return error.HttpError;

    // Receive headers into a temporary buffer
    var head_buffer: [SERVER_HEADER_BUFFER_SIZE]u8 = undefined;
    var response = request.receiveHead(&head_buffer) catch return error.HttpError;

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
