//! Validation of URLs for security purposes.

const std = @import("std");

/// Compact slice coordinates for a package URL id inside a full URL.
pub const UrlId = struct {
    start: u32,
    len: u32,

    pub fn slice(self: UrlId, url: []const u8) []const u8 {
        const start: usize = self.start;
        return url[start..][0..self.len];
    }
};

/// Checks if a URL is safe. Used for platform specification.
///
/// Allows:
/// - HTTPS URLs (any host)
/// - HTTP URLs to localhost variants: localhost, 127.0.0.1, [::1]
///
/// Rejects all other HTTP URLs for security.
pub fn isSafeUrl(url: []const u8) bool {
    return std.mem.startsWith(u8, url, "https://") or
        std.mem.startsWith(u8, url, "http://localhost:") or
        std.mem.startsWith(u8, url, "http://localhost/") or
        std.mem.startsWith(u8, url, "http://127.0.0.1:") or
        std.mem.startsWith(u8, url, "http://127.0.0.1/") or
        std.mem.startsWith(u8, url, "http://[::1]:") or
        std.mem.startsWith(u8, url, "http://[::1]/");
}

test "isSafeUrl" {
    const testing = std.testing;

    // Should return true for HTTPS URLs
    try testing.expect(isSafeUrl("https://example.com/path"));

    // Should return true for localhost HTTP URLs
    try testing.expect(isSafeUrl("http://localhost:8080/path"));
    try testing.expect(isSafeUrl("http://localhost/path"));
    try testing.expect(isSafeUrl("http://127.0.0.1:8080/path"));
    try testing.expect(isSafeUrl("http://127.0.0.1/path"));
    try testing.expect(isSafeUrl("http://[::1]:8080/path"));
    try testing.expect(isSafeUrl("http://[::1]/path"));

    // Should return false for non-localhost HTTP URLs
    try testing.expect(!isSafeUrl("http://example.com/path"));
    try testing.expect(!isSafeUrl("http://192.168.1.100/platform.tar.zst"));

    // Should return false for non-URLs
    try testing.expect(!isSafeUrl("./relative/path"));
    try testing.expect(!isSafeUrl("/absolute/path"));
    try testing.expect(!isSafeUrl("platform.roc"));
}

test "UrlId returns slice from full URL" {
    const url = "https://example.com/foo/bar/1.2.3/hash";
    const id = UrlId{ .start = 8, .len = 19 };

    try std.testing.expectEqualStrings("example.com/foo/bar", id.slice(url));
}
