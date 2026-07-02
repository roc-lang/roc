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

/// A package version parsed from a package URL path segment.
///
/// Versions with major 0 (including the 0.0.0 "no version" sentinel) are
/// rejected by URL parsing; the lowest publishable version is 1.0.0.
pub const Version = struct {
    major: u32,
    minor: u32,
    patch: u32,

    pub const none: Version = .{
        .major = 0,
        .minor = 0,
        .patch = 0,
    };

    pub fn isPresent(self: Version) bool {
        return self.major != 0 or self.minor != 0 or self.patch != 0;
    }

    /// Order two versions within the same major version: by minor, then patch.
    /// Asserts that the major versions match, since different major versions
    /// are different packages for solving purposes and must never be compared.
    pub fn orderWithinMajor(self: Version, other: Version) std.math.Order {
        std.debug.assert(self.major == other.major);
        const minor_order = std.math.order(self.minor, other.minor);
        if (minor_order != .eq) return minor_order;
        return std.math.order(self.patch, other.patch);
    }

    pub fn eql(self: Version, other: Version) bool {
        return self.major == other.major and self.minor == other.minor and self.patch == other.patch;
    }

    pub fn format(self: Version, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.print("{d}.{d}.{d}", .{ self.major, self.minor, self.patch });
    }

    pub fn bumpMajor(self: Version) Version {
        return .{ .major = self.major + 1, .minor = 0, .patch = 0 };
    }

    pub fn bumpMinor(self: Version) Version {
        return .{ .major = self.major, .minor = self.minor + 1, .patch = 0 };
    }

    pub fn bumpPatch(self: Version) Version {
        return .{ .major = self.major, .minor = self.minor, .patch = self.patch + 1 };
    }
};

/// The components parsed out of a package URL: the trailing content hash, the
/// optional version path segment, and the span of the package's url id (the
/// part between the scheme and the version/hash segments).
pub const ParsedUrl = struct {
    hash: []const u8,
    version: Version,
    url_id: UrlId,

    pub fn urlId(self: ParsedUrl, url: []const u8) []const u8 {
        return self.url_id.slice(url);
    }
};

fn parseVersionPart(part: []const u8) ?u32 {
    if (part.len == 0) return null;

    for (part) |char| {
        if (!std.ascii.isDigit(char)) return null;
    }

    return std.fmt.parseInt(u32, part, 10) catch null;
}

/// Parse a strict MAJOR.MINOR.PATCH version string (digits only, exactly
/// three components). Returns null if the string is not a version.
pub fn parseVersionComponent(component: []const u8) ?Version {
    var parts = std.mem.splitScalar(u8, component, '.');

    const major_part = parts.next() orelse return null;
    const minor_part = parts.next() orelse return null;
    const patch_part = parts.next() orelse return null;
    if (parts.next() != null) return null;

    return .{
        .major = parseVersionPart(major_part) orelse return null,
        .minor = parseVersionPart(minor_part) orelse return null,
        .patch = parseVersionPart(patch_part) orelse return null,
    };
}

fn schemeContentStart(url: []const u8) ?usize {
    const scheme_marker = std.mem.find(u8, url, "://") orelse return null;
    return scheme_marker + 3;
}

fn makeUrlId(url: []const u8, start: usize, end: usize) error{InvalidUrl}!UrlId {
    var trimmed_end = end;
    while (trimmed_end > start and url[trimmed_end - 1] == '/') {
        trimmed_end -= 1;
    }

    if (trimmed_end <= start) return error.InvalidUrl;

    return .{
        .start = std.math.cast(u32, start) orelse return error.InvalidUrl,
        .len = std.math.cast(u32, trimmed_end - start) orelse return error.InvalidUrl,
    };
}

/// Parse a package URL's path into its trailing content hash, optional
/// MAJOR.MINOR.PATCH version segment, and url id span.
pub fn parseUrlPath(url: []const u8) error{ InvalidUrl, InvalidVersion, NoHashInUrl }!ParsedUrl {
    const url_id_start = schemeContentStart(url) orelse return error.InvalidUrl;
    const last_slash = std.mem.findLast(u8, url, "/") orelse return error.NoHashInUrl;
    if (last_slash < url_id_start) return error.NoHashInUrl;

    const hash_part = url[last_slash + 1 ..];

    const hash = if (std.mem.endsWith(u8, hash_part, ".tar.zst"))
        hash_part[0 .. hash_part.len - 8]
    else
        hash_part;

    if (hash.len == 0) {
        return error.NoHashInUrl;
    }

    const before_hash = url[0..last_slash];
    const version_parse = if (std.mem.findLast(u8, before_hash, "/")) |version_slash|
        if (version_slash >= url_id_start) parseVersionComponent(before_hash[version_slash + 1 ..]) else null
    else
        null;
    if (version_parse) |parsed_version| {
        // Package versions start at 1.0.0; major 0 is rejected (0.0.0 doubles
        // as the no-version sentinel).
        if (parsed_version.major == 0) return error.InvalidVersion;
    }
    const version = version_parse orelse Version.none;
    const url_id_end = if (version_parse != null)
        std.mem.findLast(u8, before_hash, "/").?
    else
        last_slash;

    const url_id = makeUrlId(url, url_id_start, url_id_end) catch return error.InvalidUrl;

    return .{
        .hash = hash,
        .version = version,
        .url_id = url_id,
    };
}

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

test "parseUrlPath extracts url id" {
    {
        const url = "https://example.com/foo/bar/1.2.3/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
        const parsed = try parseUrlPath(url);

        try std.testing.expectEqualStrings("example.com/foo/bar", parsed.urlId(url));
        try std.testing.expectEqual(Version{ .major = 1, .minor = 2, .patch = 3 }, parsed.version);
    }

    {
        const url = "https://example.com/foo/bar/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
        const parsed = try parseUrlPath(url);

        try std.testing.expectEqualStrings("example.com/foo/bar", parsed.urlId(url));
        try std.testing.expectEqual(Version.none, parsed.version);
    }

    {
        const url = "http://127.0.0.1:8000/1.2.3/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
        const parsed = try parseUrlPath(url);

        try std.testing.expectEqualStrings("127.0.0.1:8000", parsed.urlId(url));
    }

    {
        const url = "https://example.com/foo/1.2.x/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
        const parsed = try parseUrlPath(url);

        try std.testing.expectEqualStrings("example.com/foo/1.2.x", parsed.urlId(url));
    }

    {
        const url = "https://example.com/foo/1.0.1/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
        const parsed = try parseUrlPath(url);

        try std.testing.expectEqualStrings("example.com/foo", parsed.urlId(url));
        try std.testing.expectEqual(Version{ .major = 1, .minor = 0, .patch = 1 }, parsed.version);
    }
}

test "parseUrlPath rejects versions below 1.0.0" {
    try std.testing.expectError(
        error.InvalidVersion,
        parseUrlPath("https://example.com/0.0.0/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst"),
    );
    try std.testing.expectError(
        error.InvalidVersion,
        parseUrlPath("https://example.com/foo/bar/0.0.0/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst"),
    );
    try std.testing.expectError(
        error.InvalidVersion,
        parseUrlPath("https://example.com/foo/0.0.1/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst"),
    );
    try std.testing.expectError(
        error.InvalidVersion,
        parseUrlPath("https://example.com/foo/0.9.9/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst"),
    );
    try std.testing.expectError(
        error.InvalidVersion,
        parseUrlPath("https://example.com/foo/0.20.0/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst"),
    );
}

test "parseUrlPath rejects URLs without a hash path segment" {
    try std.testing.expectError(error.NoHashInUrl, parseUrlPath("https://example.com"));
    try std.testing.expectError(error.NoHashInUrl, parseUrlPath("https://example.com/"));
}

test "Version.format prints MAJOR.MINOR.PATCH" {
    var buf: [32]u8 = undefined;
    const rendered = try std.fmt.bufPrint(&buf, "{f}", .{Version{ .major = 1, .minor = 22, .patch = 333 }});
    try std.testing.expectEqualStrings("1.22.333", rendered);
}

test "Version bump helpers reset lower components" {
    const v = Version{ .major = 1, .minor = 2, .patch = 3 };

    try std.testing.expectEqual(Version{ .major = 2, .minor = 0, .patch = 0 }, v.bumpMajor());
    try std.testing.expectEqual(Version{ .major = 1, .minor = 3, .patch = 0 }, v.bumpMinor());
    try std.testing.expectEqual(Version{ .major = 1, .minor = 2, .patch = 4 }, v.bumpPatch());
}

test "parseVersionComponent parses strict three-part versions" {
    try std.testing.expectEqual(Version{ .major = 1, .minor = 2, .patch = 3 }, parseVersionComponent("1.2.3").?);
    try std.testing.expectEqual(@as(?Version, null), parseVersionComponent("1.2"));
    try std.testing.expectEqual(@as(?Version, null), parseVersionComponent("v1.2.3"));
    try std.testing.expectEqual(@as(?Version, null), parseVersionComponent("1.2.x"));
}

test "Version.orderWithinMajor orders by minor then patch" {
    const v1_2_3 = Version{ .major = 1, .minor = 2, .patch = 3 };
    const v1_3_1 = Version{ .major = 1, .minor = 3, .patch = 1 };
    const v1_2_4 = Version{ .major = 1, .minor = 2, .patch = 4 };

    try std.testing.expectEqual(std.math.Order.lt, v1_2_3.orderWithinMajor(v1_3_1));
    try std.testing.expectEqual(std.math.Order.lt, v1_2_3.orderWithinMajor(v1_2_4));
    try std.testing.expectEqual(std.math.Order.gt, v1_3_1.orderWithinMajor(v1_2_4));
    try std.testing.expectEqual(std.math.Order.eq, v1_2_3.orderWithinMajor(v1_2_3));
}
