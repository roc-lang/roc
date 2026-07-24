//! Validation of URLs for security purposes.

const std = @import("std");

/// Compact slice coordinates for a package URL id inside a full URL.
///
/// The url id is everything between the scheme and the security hash with the
/// version number removed. Since the version may appear anywhere in that
/// range, the id is the concatenation of the spans before and after it (the
/// suffix is empty for versionless URLs).
pub const UrlId = struct {
    prefix_start: u32,
    prefix_len: u32,
    suffix_start: u32 = 0,
    suffix_len: u32 = 0,

    pub fn prefix(self: UrlId, url: []const u8) []const u8 {
        const start: usize = self.prefix_start;
        return url[start..][0..self.prefix_len];
    }

    pub fn suffix(self: UrlId, url: []const u8) []const u8 {
        const start: usize = self.suffix_start;
        return url[start..][0..self.suffix_len];
    }
};

/// A package version parsed from a package URL path segment.
///
/// 0.0.0 is reserved as the "no version" sentinel and is rejected by URL
/// parsing; the lowest publishable version is 0.0.1.
///
/// For versions with major 0, the minor number is the compatibility boundary:
/// in 0.X.Y, bumping X signals a breaking change and bumping Y signals a
/// compatible one. From 1.0.0 onwards, normal SemVer semantics apply.
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

    /// Order two versions within the same compatibility group: by minor, then
    /// patch. Asserts that the major versions match — and for major 0, that
    /// the minors match too — since different compatibility groups are
    /// different packages for solving purposes and must never be compared.
    pub fn orderWithinMajor(self: Version, other: Version) std.math.Order {
        std.debug.assert(self.major == other.major);
        std.debug.assert(self.major != 0 or self.minor == other.minor);
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
/// optional version (which may appear anywhere before the hash), and the
/// spans of the package's url id (everything else).
pub const ParsedUrl = struct {
    hash: []const u8,
    version: Version,
    url_id: UrlId,

    pub fn urlIdPrefix(self: ParsedUrl, url: []const u8) []const u8 {
        return self.url_id.prefix(url);
    }

    pub fn urlIdSuffix(self: ParsedUrl, url: []const u8) []const u8 {
        return self.url_id.suffix(url);
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

/// Whether a character is in the Bitcoin base58 alphabet (alphanumerics
/// without 0, O, I, and l). Must match `base58.base58_alphabet`, which this
/// module cannot import; a test in the `unbundle` module (which sees both)
/// asserts these ranges accept exactly that alphabet.
pub fn isBase58Char(char: u8) bool {
    return switch (char) {
        '1'...'9', 'A'...'H', 'J'...'N', 'P'...'Z', 'a'...'k', 'm'...'z' => true,
        else => false,
    };
}

const VersionOccurrence = struct {
    /// Absolute url index of the version's first character.
    start: usize,
    /// Absolute url index one past the version's last character.
    end: usize,
    version: Version,
};

/// Find the single MAJOR.MINOR.PATCH occurrence in url[region_start..region_end].
///
/// An occurrence is three all-digit runs joined by dots whose surrounding
/// characters (if any) are neither digits nor dots, so a component of a
/// four-number sequence like 1.2.3.4 never matches. Returns null when the
/// region contains no version, and errors when it contains more than one:
/// per the package URL design, a URL is only versioned when exactly one part
/// of it parses as a version.
fn findVersionOccurrence(url: []const u8, region_start: usize, region_end: usize) error{AmbiguousVersion}!?VersionOccurrence {
    var found: ?VersionOccurrence = null;
    var i = region_start;
    while (i < region_end) {
        if (!std.ascii.isDigit(url[i]) or
            (i > region_start and (std.ascii.isDigit(url[i - 1]) or url[i - 1] == '.')))
        {
            i += 1;
            continue;
        }

        const occurrence = matchVersionAt(url, i, region_end) orelse {
            i += 1;
            continue;
        };
        if (found != null) return error.AmbiguousVersion;
        found = occurrence;
        i = occurrence.end;
    }
    return found;
}

/// Try to match D+.D+.D+ starting exactly at url[start], bounded on the right
/// by region_end or a character that is neither a digit nor a dot.
fn matchVersionAt(url: []const u8, start: usize, region_end: usize) ?VersionOccurrence {
    var i = start;
    var components: [3]u32 = undefined;
    for (0..3) |component_index| {
        if (component_index > 0) {
            if (i >= region_end or url[i] != '.') return null;
            i += 1;
        }
        const digits_start = i;
        while (i < region_end and std.ascii.isDigit(url[i])) i += 1;
        if (i == digits_start) return null;
        components[component_index] = std.fmt.parseInt(u32, url[digits_start..i], 10) catch return null;
    }
    if (i < region_end and (std.ascii.isDigit(url[i]) or url[i] == '.')) return null;
    return .{
        .start = start,
        .end = i,
        .version = .{ .major = components[0], .minor = components[1], .patch = components[2] },
    };
}

fn makeUrlId(url: []const u8, prefix_start: usize, prefix_end: usize, suffix_start: usize, suffix_end: usize) error{InvalidUrl}!UrlId {
    var trimmed_suffix_end = suffix_end;
    while (trimmed_suffix_end > suffix_start and url[trimmed_suffix_end - 1] == '/') {
        trimmed_suffix_end -= 1;
    }

    // With no suffix, the prefix is the tail of the id and gets the same trim.
    var trimmed_prefix_end = prefix_end;
    if (trimmed_suffix_end == suffix_start) {
        while (trimmed_prefix_end > prefix_start and url[trimmed_prefix_end - 1] == '/') {
            trimmed_prefix_end -= 1;
        }
    }

    if (trimmed_prefix_end <= prefix_start and trimmed_suffix_end <= suffix_start) return error.InvalidUrl;

    return .{
        .prefix_start = std.math.cast(u32, prefix_start) orelse return error.InvalidUrl,
        .prefix_len = std.math.cast(u32, trimmed_prefix_end - prefix_start) orelse return error.InvalidUrl,
        .suffix_start = std.math.cast(u32, suffix_start) orelse return error.InvalidUrl,
        .suffix_len = std.math.cast(u32, trimmed_suffix_end - suffix_start) orelse return error.InvalidUrl,
    };
}

/// Parse a package URL's path into its trailing content hash, optional
/// MAJOR.MINOR.PATCH version, and url id spans.
///
/// The hash is the trailing run of base58 characters in the final path
/// segment (ignoring a .tar.zst extension), so filenames may carry a prefix
/// like "roc-thing-" as long as it ends with a non-base58 separator. The
/// version may appear anywhere between the scheme and the hash — as its own
/// path segment, a filename prefix, or embedded in a segment like
/// "v1.9.0-rc2" — but at most once.
pub fn parseUrlPath(url: []const u8) error{ InvalidUrl, InvalidVersion, AmbiguousVersion, NoHashInUrl }!ParsedUrl {
    const url_id_start = schemeContentStart(url) orelse return error.InvalidUrl;
    const last_slash = std.mem.findLast(u8, url, "/") orelse return error.NoHashInUrl;
    if (last_slash < url_id_start) return error.NoHashInUrl;

    const filename = url[last_slash + 1 ..];

    const stem = if (std.mem.endsWith(u8, filename, ".tar.zst"))
        filename[0 .. filename.len - 8]
    else
        filename;

    var hash_start_in_stem = stem.len;
    while (hash_start_in_stem > 0 and isBase58Char(stem[hash_start_in_stem - 1])) {
        hash_start_in_stem -= 1;
    }
    const hash = stem[hash_start_in_stem..];
    if (hash.len == 0) {
        return error.NoHashInUrl;
    }
    const hash_start = last_slash + 1 + hash_start_in_stem;

    const occurrence = try findVersionOccurrence(url, url_id_start, hash_start);

    var version = Version.none;
    var prefix_end = hash_start;
    var suffix_start = hash_start;
    if (occurrence) |occ| {
        // 0.0.0 is reserved as the no-version sentinel; the lowest publishable
        // version is 0.0.1.
        if (!occ.version.isPresent()) return error.InvalidVersion;
        version = occ.version;
        prefix_end = occ.start;
        suffix_start = occ.end;
    }

    const url_id = makeUrlId(url, url_id_start, prefix_end, suffix_start, hash_start) catch return error.InvalidUrl;

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

test "UrlId returns prefix and suffix slices from full URL" {
    const url = "https://example.com/foo/bar/1.2.3/hash";
    const id = UrlId{ .prefix_start = 8, .prefix_len = 20, .suffix_start = 33, .suffix_len = 1 };

    try std.testing.expectEqualStrings("example.com/foo/bar/", id.prefix(url));
    try std.testing.expectEqualStrings("/", id.suffix(url));
}

test "parseUrlPath extracts url id" {
    {
        const url = "https://example.com/foo/bar/1.2.3/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
        const parsed = try parseUrlPath(url);

        try std.testing.expectEqualStrings("example.com/foo/bar", parsed.urlIdPrefix(url));
        try std.testing.expectEqualStrings("", parsed.urlIdSuffix(url));
        try std.testing.expectEqual(Version{ .major = 1, .minor = 2, .patch = 3 }, parsed.version);
    }

    {
        const url = "https://example.com/foo/bar/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
        const parsed = try parseUrlPath(url);

        try std.testing.expectEqualStrings("example.com/foo/bar", parsed.urlIdPrefix(url));
        try std.testing.expectEqualStrings("", parsed.urlIdSuffix(url));
        try std.testing.expectEqual(Version.none, parsed.version);
    }

    {
        const url = "http://127.0.0.1:8000/1.2.3/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
        const parsed = try parseUrlPath(url);

        try std.testing.expectEqualStrings("127.0.0.1:8000", parsed.urlIdPrefix(url));
        try std.testing.expectEqualStrings("", parsed.urlIdSuffix(url));
        try std.testing.expectEqual(Version{ .major = 1, .minor = 2, .patch = 3 }, parsed.version);
    }

    {
        const url = "https://example.com/foo/1.2.x/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
        const parsed = try parseUrlPath(url);

        try std.testing.expectEqualStrings("example.com/foo/1.2.x", parsed.urlIdPrefix(url));
        try std.testing.expectEqual(Version.none, parsed.version);
    }
}

test "parseUrlPath finds the version anywhere before the hash" {
    // Version as a filename prefix.
    {
        const url = "https://example.com/pkg/2.5.10-4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
        const parsed = try parseUrlPath(url);

        try std.testing.expectEqual(Version{ .major = 2, .minor = 5, .patch = 10 }, parsed.version);
        try std.testing.expectEqualStrings("4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf", parsed.hash);
        try std.testing.expectEqualStrings("example.com/pkg/", parsed.urlIdPrefix(url));
        try std.testing.expectEqualStrings("-", parsed.urlIdSuffix(url));
    }

    // Version as its own path segment, GitHub releases style.
    {
        const url = "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
        const parsed = try parseUrlPath(url);

        try std.testing.expectEqual(Version{ .major = 0, .minor = 7, .patch = 0 }, parsed.version);
        try std.testing.expectEqualStrings("github.com/roc-lang/basic-cli/releases/download", parsed.urlIdPrefix(url));
        try std.testing.expectEqualStrings("", parsed.urlIdSuffix(url));
    }

    // Version embedded in a tag segment with a prefixed filename, GitLab
    // releases style.
    {
        const url = "https://gitlab.com/repo/user/-/releases/v1.9.0-rc2/downloads/roc-thing-4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
        const parsed = try parseUrlPath(url);

        try std.testing.expectEqual(Version{ .major = 1, .minor = 9, .patch = 0 }, parsed.version);
        try std.testing.expectEqualStrings("4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf", parsed.hash);
        try std.testing.expectEqualStrings("gitlab.com/repo/user/-/releases/v", parsed.urlIdPrefix(url));
        try std.testing.expectEqualStrings("-rc2/downloads/roc-thing-", parsed.urlIdSuffix(url));
    }

    // Two URLs for different versions of the same package share their url id.
    {
        const url_a = "https://gitlab.com/repo/user/-/releases/v1.9.0-rc2/downloads/roc-thing-4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
        const url_b = "https://gitlab.com/repo/user/-/releases/v2.0.0-rc2/downloads/roc-thing-5ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXg.tar.zst";
        const parsed_a = try parseUrlPath(url_a);
        const parsed_b = try parseUrlPath(url_b);

        try std.testing.expectEqualStrings(parsed_a.urlIdPrefix(url_a), parsed_b.urlIdPrefix(url_b));
        try std.testing.expectEqualStrings(parsed_a.urlIdSuffix(url_a), parsed_b.urlIdSuffix(url_b));
    }
}

test "parseUrlPath extracts a trailing base58 hash from a prefixed filename" {
    const url = "https://example.com/pkg/roc-thing-4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
    const parsed = try parseUrlPath(url);

    try std.testing.expectEqualStrings("4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf", parsed.hash);
    try std.testing.expectEqual(Version.none, parsed.version);
    try std.testing.expectEqualStrings("example.com/pkg/roc-thing-", parsed.urlIdPrefix(url));
}

test "parseUrlPath rejects URLs with more than one version" {
    try std.testing.expectError(
        error.AmbiguousVersion,
        parseUrlPath("https://example.com/1.2.3/pkg-1.2.3-4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst"),
    );
}

test "parseUrlPath ignores four-part number sequences" {
    const url = "https://example.com/foo/1.2.3.4/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
    const parsed = try parseUrlPath(url);

    try std.testing.expectEqual(Version.none, parsed.version);
    try std.testing.expectEqualStrings("example.com/foo/1.2.3.4", parsed.urlIdPrefix(url));
}

test "parseUrlPath rejects the reserved 0.0.0 version" {
    try std.testing.expectError(
        error.InvalidVersion,
        parseUrlPath("https://example.com/0.0.0/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst"),
    );
}

test "parseUrlPath accepts 0.x versions" {
    const url = "https://example.com/foo/0.0.1/4ZGqXJtqH5n9wMmQ7nPQTU8zgHBNfZ3kcVnNcL3hKqXf.tar.zst";
    const parsed = try parseUrlPath(url);
    try std.testing.expectEqualStrings("example.com/foo", parsed.urlIdPrefix(url));
    try std.testing.expectEqual(Version{ .major = 0, .minor = 0, .patch = 1 }, parsed.version);
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

    // For major 0, ordering happens within a 0.X compatibility group.
    const v0_5_2 = Version{ .major = 0, .minor = 5, .patch = 2 };
    const v0_5_3 = Version{ .major = 0, .minor = 5, .patch = 3 };
    try std.testing.expectEqual(std.math.Order.lt, v0_5_2.orderWithinMajor(v0_5_3));
}
