const std = @import("std");

// gzip should be the most widely supported, and brotli offers the highest compression.
// flate2 gets us both gzip and deflate, so there's no harm in offering deflate too.
//
// Here are all the officially supported options: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept-Encoding
// We can consider supporting more, but that would bloat the `roc` binary more, so
// let's try to avoid doing that.
const BROTLI_BUFFER_BYTES: usize = 8 * 1_000_000; // MB

/// Valid URLs must end in one of these:
///
/// - .tar
/// - .tar.gz
/// - .tar.br
const VALID_EXTENSION_SUFFIXES: [2][]u8 = .{ ".gz", ".br" };

const ROC_EXTENSION = ".roc";

/// Since the TLD (top level domain) `.zip` is now available, there is a new attack
/// vector where malicous URLs can be used to confuse the reader.
/// Example of a URL which would take you to example.zip:
/// https://github.com∕kubernetes∕kubernetes∕archive∕refs∕tags∕@example.zip
/// roc employs a checksum mechanism to prevent tampering with packages.
/// Nevertheless we should avoid such issues earlier.
/// You can read more here: https://medium.com/@bobbyrsec/the-dangers-of-googles-zip-tld-5e1e675e59a5
const MISLEADING_CHARACTERS_IN_URL: [5]u32 = .{
    '@', // @ - For now we avoid usage of the @, to avoid the "tld zip" attack vector
    '\u{2044}', // U+2044 ==  ⁄ Fraction Slash
    '\u{2215}', // U+2215 ==  ∕ Division Slash
    '\u{FF0F}', // U+2215 == ／ Fullwidth Solidus
    '\u{29F8}', // U+29F8 == ⧸ Big Solidus
};

pub const PackageMetadata = struct {
    /// The BLAKE3 hash of the tarball's contents. Also the .tar filename on disk.
    content_hash: []u8,
    /// On disk, this will be the subfolder inside the cache dir where the package lives
    cache_subdir: []u8,
    /// Other code will default this to main.roc, but this module isn't concerned with that default.
    root_module_filename: ?[]u8,

    pub const ParseUrlResult = union(enum) {
        Success: PackageMetadata,
        InvalidExtensionSuffix: []u8,
        MissingTarExt,
        InvalidFragment: []u8,
        MissingHash,
        MissingHttps,
        MisleadingCharacter,
    };

    fn new(url: []u8) ParseUrlResult {
        // First, verify that the URL starts with https://
        const HTTPS_PREFIX = "https://";
        const starts_with_https = std.mem.eql(u8, url[0..HTTPS_PREFIX.len], HTTPS_PREFIX);
        const without_protocol = if (starts_with_https) url[0..HTTPS_PREFIX.len] else {
            return .MissingHttps;
        };

        // Next, check if there are misleading characters in the URL
        for (url) |url_char| {
            for (MISLEADING_CHARACTERS_IN_URL) |misleading_char| {
                if (url_char == misleading_char) {
                    return .MisleadingCharacter;
                }
            }
        }

        const without_fragment: []u8 = undefined;
        const fragment: ?[]u8 = undefined;

        var char_index = without_protocol.len;
        while (char_index > 0) {
            char_index -= 1;
            if (char_index != '#') continue;

            fragment = without_protocol[(char_index + 1)..];
            if (fragment.*.len > ROC_EXTENSION.len and
                std.mem.eql(u8, ROC_EXTENSION, fragment[(fragment.*.len - ROC_EXTENSION.len)..]))
            {
                without_fragment = without_protocol[0..char_index];
            } else {
                return .{ .InvalidFragment = fragment };
            }
        } else {
            without_fragment = without_protocol;
            fragment = null;
        }

        // The tarball name is everything after the "/" (without the .tar extension)
        // The URL must end in .tar followed optionally by ".gz", ".br", etc. (excluding the fragment)
        // const without_ext = match without_fragment.rsplit_once(".tar") {
        //     Some((before_ext, after_ext)) => {
        //         if after_ext.is_empty() || VALID_EXTENSION_SUFFIXES.contains(&after_ext) {
        //             before_ext
        //         } else {
        //             return Err(UrlProblem::InvalidExtensionSuffix(after_ext.to_string()));
        //         }
        //     }
        //     None => {
        //         // The URL didn't end in .tar at all
        //         return Err(UrlProblem::MissingTarExt);
        //     }
        // };

        // const (path, tarball_name) = match without_ext.rsplit_once('/') {
        //     Some((path, hash)) if !hash.is_empty() => (path, hash),
        //     _ => {
        //         return Err(UrlProblem::MissingHash);
        //     }
        // };

        return .{
            .Success = PackageMetadata{
                // cache_subdir: path,
                // content_hash: tarball_name,
                // root_module_filename: fragment,
            },
        };
    }
};

// #[test]
// fn url_problem_missing_https() {
//     let expected = Err(UrlProblem::MissingHttps);
//     assert_eq!(PackageMetadata::try_from("http://example.com"), expected);
// }

// #[test]
// fn url_problem_misleading_characters() {
//     let expected = Err(UrlProblem::MisleadingCharacter);

//     for misleading_character_example in [
//         "https://user:password@example.com/",
//         //"https://example.com⁄path",
//         "https://example.com\u{2044}path",
//         //"https://example.com∕path",
//         "https://example.com\u{2215}path",
//         //"https://example.com／path",
//         "https://example.com\u{ff0f}path",
//         //"https://example.com⧸path",
//         "https://example.com\u{29f8}path",
//     ] {
//         assert_eq!(
//             PackageMetadata::try_from(misleading_character_example),
//             expected
//         );
//     }
// }

// #[test]
// fn url_problem_invalid_fragment_not_a_roc_file() {
//     let expected = Err(UrlProblem::InvalidFragment("filename.sh".to_string()));
//     assert_eq!(
//         PackageMetadata::try_from("https://example.com/#filename.sh"),
//         expected
//     );
// }

// #[test]
// fn url_problem_invalid_fragment_empty_roc_filename() {
//     let expected = Err(UrlProblem::InvalidFragment(".roc".to_string()));
//     assert_eq!(
//         PackageMetadata::try_from("https://example.com/#.roc"),
//         expected
//     );
// }

// #[test]
// fn url_problem_not_a_tar_url() {
//     let expected = Err(UrlProblem::MissingTarExt);
//     assert_eq!(
//         PackageMetadata::try_from("https://example.com/filename.zip"),
//         expected
//     );
// }

// #[test]
// fn url_problem_invalid_tar_suffix() {
//     let expected = Err(UrlProblem::InvalidExtensionSuffix(".zip".to_string()));
//     assert_eq!(
//         PackageMetadata::try_from("https://example.com/filename.tar.zip"),
//         expected
//     );
// }

// #[test]
// fn url_problem_missing_hash() {
//     let expected = Err(UrlProblem::MissingHash);
//     assert_eq!(
//         PackageMetadata::try_from("https://example.com/.tar.gz"),
//         expected
//     );
// }

// #[test]
// fn url_without_fragment() {
//     let expected = Ok(PackageMetadata {
//         cache_subdir: "example.com/path",
//         content_hash: "hash",
//         root_module_filename: None,
//     });
//     assert_eq!(
//         PackageMetadata::try_from("https://example.com/path/hash.tar.gz"),
//         expected
//     );
// }

// #[test]
// fn url_with_fragment() {
//     let expected = Ok(PackageMetadata {
//         cache_subdir: "example.com/path",
//         content_hash: "hash",
//         root_module_filename: Some("filename.roc"),
//     });
//     assert_eq!(
//         PackageMetadata::try_from("https://example.com/path/hash.tar.gz#filename.roc"),
//         expected
//     );
// }
