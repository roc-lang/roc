const std = @import("std");

/// Example:
///
///         |-----(the directory the file will be downloaded into)----| |--(the filename)--|
/// https://github.com/roc-lang/basic-cli/releases/download/123.456.789/du6cQJie4gupZ.tar.br#entrypoint.roc
///                                                         |-version-| |---hash----| |ext-| |--fragment--|
///
pub const InvalidUrl = error{
    InvalidProtocol,
    MissingFilename,
    MissingExtension,
    UnsupportedExtension,
};

pub const VersionResult = union(enum) {
    ok: Version,
    err: VersionProblem,
};

/// The file extensions we support.
pub const Extension = enum {
    tar_br,
    tar_gz,
};

const Version = struct {
    major: u16,
    minor: u16,
    patch: u16,
};

pub const VersionProblem = enum {
    MissingVersion,
    VersionNumberTooLarge,
    EmptyVersionComponent,
    TooManyVersionComponents,
    IncompleteVersion,
    InvalidVersionCharacter,
};

pub const Url = struct {
    /// The version portion of the URL must be MAJOR.MINOR.PATCH if it's present.
    /// Platforms don't use this, but it's required for packages. So if it's missing or malformed, we
    /// record that...but don't error out. This way, packages and platforms can handle it differently.
    version: VersionResult,
    /// The filename's extension as an enum, which determines how we will decompress and unpack it.
    ext: Extension,
    /// The filename, including extension - this is what we actually save to the directory, after creating it.
    filename: []const u8,
    /// Optional entrypoint file to use (defaults to `main.roc`).
    fragment: []const u8,

    /// The entire URL consists of slices into these src bytes, so it must not outlive these bytes.
    pub fn parse(src: []const u8) InvalidUrl!Url {
        // Protocol must be "https://"
        const PROTOCOL = "https://";
        if (!std.mem.startsWith(u8, src, PROTOCOL)) {
            return error.InvalidProtocol;
        }

        // Search from the end to find the filename and extension (and optionally fragment while we're at it)
        var fragment: []const u8 = .{}; // By default, assume no fragment.
        // Start off with the extension being at the end, and empty to begin with.
        var extension_start: usize = src.len - 1;
        var extension_end: usize = src.len;
        // Read from the end and advance towards the start.
        var idx: usize = src.len - 1;

        // Documenting our assumptions: this loop will only do in-bounds reads, and will eventually
        // terminate because we alread verified that we're starting after the "https://" and moving
        // backwards until we hit a "/", which the protocol does end with.
        std.debug.assert(idx >= PROTOCOL.len);
        std.debug.assert(src.len >= PROTOCOL.len);
        std.debug.assert(PROTOCOL.len > 0);
        std.debug.assert(src[PROTOCOL.len - 1] == '/');

        while (src[idx] != '/') : (idx -= 1) {
            // TODO this can all be made branchless aside from error cases, which shouldn't mispredict unless there's an error.
            if (src[idx] == '#') {
                if (fragment.len > 0) {
                    // The fragment doesn't include the '#' itself.
                    fragment = src[idx + 1 .. src.len];

                    if (fragment.len == 0) {
                        // TODO return an error of EmptyFragment (empty fragments are not allowed; just omit it!)
                    }

                    // The extension now ends right before the '#', and is empty for now.
                    extension_start = idx - 1;
                    extension_end = idx;
                } else {
                    // TODO return error: found multiple fragments
                }
            }

            if (src[idx] == '.') {
                // Move the start of the extension, but not the end. The end will either have been set to
                // the beginning of the fragment or the end of the string (if there was no fragment).
                extension_start = idx + 1;
            }
        }

        const ext = try Extension.fromStr(src[extension_start..extension_end]);
        // We broke out of the loop because idx was a '/', so the filename starts right after that.
        const filename = src[idx + 1 .. extension_end];

        // Documenting our assumptions: this loop will only do in-bounds reads, and will eventually
        // terminate because we alread verified that we're starting after the "https:/" portion of "https://"
        // and moving backwards until we hit another '/'.
        std.debug.assert(idx >= PROTOCOL.len - 1);
        std.debug.assert(PROTOCOL.len > 1);
        std.debug.assert(src[PROTOCOL.len - 2] == '/');

        // Parse version (MAJOR.MINOR.PATCH) backwards from end until we hit another '/'
        var version: [3]u16 = undefined;
        var component: u8 = 0; // 2=major, 1=minor, 0=patch
        var current_num: u16 = 0;
        var digit_count: u8 = 0;
        var version_result = .{ .err = .missing_version };

        while (src[idx] != '/') : (idx += 1) {
            const c = src[idx];

            if (c >= '0' and c <= '9') {
                digit_count += 1;
                if (digit_count > 5) return error.VersionNumberTooLarge;

                const digit = c - '0';
                const mul_result = @mulWithOverflow(current_num, 10);
                if (mul_result[1] != 0) return error.VersionNumberTooLarge;
                const add_result = @addWithOverflow(mul_result[0], digit);
                if (add_result[1] != 0) return error.VersionNumberTooLarge;
                current_num = add_result[0];
            } else if (c == '.') {
                if (digit_count == 0) return error.EmptyVersionComponent;

                switch (component) {
                    0 => major = current_num,
                    1 => minor = current_num,
                    else => return error.TooManyVersionComponents,
                }

                component += 1;
                current_num = 0;
                digit_count = 0;
            } else {
                // TODO it's an error

                // Ignore the rest of the malformed version and skip to the '/'
                while (src[idx] != '/') : (idx += 1) {}
            }
        }

        return Url{
            .download_dir = download_dir,
            .filename = filename,
            .version = version_result,
            .ext = ext,
            .fragment = fragment,
        };
    }
};

const testing = std.testing;

test "parse valid URL with fragment" {
    const url_str = "https://github.com/roc-lang/basic-cli/releases/download/123.456.789/du6cQJie4gupZ.tar.br#entrypoint.roc";
    const url = try Url.parse(url_str);

    try testing.expectEqualStrings("https://github.com/roc-lang/basic-cli/releases/download/123.456.789/du6cQJie4gupZ.tar.br", url.without_fragment);
    try testing.expectEqualStrings("github.com/roc-lang/basic-cli/releases/download/123.456.789", url.download_dir);
    try testing.expectEqualStrings("du6cQJie4gupZ.tar.br", url.filename);
    try testing.expectEqual(@as(u16, 123), url.version.major);
    try testing.expectEqual(@as(u16, 456), url.version.minor);
    try testing.expectEqual(@as(u16, 789), url.version.patch);
    try testing.expectEqual(Extension.tar_br, url.ext);
    try testing.expectEqualStrings("entrypoint.roc", url.fragment);
}

test "parse valid URL without fragment" {
    const url_str = "https://example.com/packages/1.0.0/mypackage.tar.gz";
    const url = try Url.parse(url_str);

    try testing.expectEqualStrings(url_str, url.without_fragment);
    try testing.expectEqualStrings("example.com/packages/1.0.0", url.download_dir);
    try testing.expectEqualStrings("mypackage.tar.gz", url.filename);
    try testing.expectEqual(@as(u16, 1), url.version.major);
    try testing.expectEqual(@as(u16, 0), url.version.minor);
    try testing.expectEqual(@as(u16, 0), url.version.patch);
    try testing.expectEqual(Extension.tar_gz, url.ext);
    try testing.expectEqualStrings("", url.fragment);
}

test "parse errors" {
    // Invalid protocol
    try testing.expectError(error.InvalidProtocol, Url.parse("http://example.com/1.0.0/file.tar.gz"));

    // Missing filename
    try testing.expectError(error.MissingFilename, Url.parse("https://example.com/1.0.0/"));

    // Missing extension
    try testing.expectError(error.MissingExtension, Url.parse("https://example.com/1.0.0/file"));

    // Unsupported extension
    try testing.expectError(error.UnsupportedExtension, Url.parse("https://example.com/1.0.0/file.zip"));

    // Missing path structure (no version)
    try testing.expectError(error.MissingFilename, Url.parse("https://example.com"));

    // File at root (no version in path)
    try testing.expectError(error.MissingVersion, Url.parse("https://example.com/file.tar.gz"));

    // Invalid version format
    try testing.expectError(error.IncompleteVersion, Url.parse("https://example.com/1.0/file.tar.gz"));
    try testing.expectError(error.EmptyVersionComponent, Url.parse("https://example.com/1..0/file.tar.gz"));
    try testing.expectError(error.InvalidVersionCharacter, Url.parse("https://example.com/1.0.beta/file.tar.gz"));
    try testing.expectError(error.TooManyVersionComponents, Url.parse("https://example.com/1.0.0.0/file.tar.gz"));

    // Version number too large
    try testing.expectError(error.VersionNumberTooLarge, Url.parse("https://example.com/123456.0.0/file.tar.gz"));
}

test "version overflow protection" {
    // Test that we handle potential u16 overflow correctly
    try testing.expectError(error.VersionNumberTooLarge, Url.parse("https://example.com/65536.0.0/file.tar.gz"));

    // Maximum valid u16 value should work
    const url = try Url.parse("https://example.com/65535.65535.65535/file.tar.gz");
    try testing.expectEqual(@as(u16, 65535), url.version.major);
    try testing.expectEqual(@as(u16, 65535), url.version.minor);
    try testing.expectEqual(@as(u16, 65535), url.version.patch);
}
