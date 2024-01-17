use std::{
    io::{self, Read, Write},
    path::Path,
};

use crate::tarball::Compression;

// gzip should be the most widely supported, and brotli offers the highest compression.
// flate2 gets us both gzip and deflate, so there's no harm in offering deflate too.
//
// Here are all the officially supported options: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept-Encoding
// We can consider supporting more, but that would bloat the `roc` binary more, so
// let's try to avoid doing that.
const BROTLI_BUFFER_BYTES: usize = 8 * 1_000_000; // MB

#[derive(Debug, PartialEq, Eq)]
pub struct PackageMetadata<'a> {
    /// The BLAKE3 hash of the tarball's contents. Also the .tar filename on disk.
    pub content_hash: &'a str,
    /// On disk, this will be the subfolder inside the cache dir where the package lives
    pub cache_subdir: &'a str,
    /// Other code will default this to main.roc, but this module isn't concerned with that default.
    pub root_module_filename: Option<&'a str>,
}

/// Valid URLs must end in one of these:
///
/// - .tar
/// - .tar.gz
/// - .tar.br
const VALID_EXTENSION_SUFFIXES: [&str; 2] = [".gz", ".br"];

/// Since the TLD (top level domain) `.zip` is now available, there is a new attack
/// vector where malicous URLs can be used to confuse the reader.
/// Example of a URL which would take you to example.zip:
/// https://github.com∕kubernetes∕kubernetes∕archive∕refs∕tags∕@example.zip
/// roc employs a checksum mechanism to prevent tampering with packages.
/// Nevertheless we should avoid such issues earlier.
/// You can read more here: https://medium.com/@bobbyrsec/the-dangers-of-googles-zip-tld-5e1e675e59a5
const MISLEADING_CHARACTERS_IN_URL: [char; 5] = [
    '@',        // @ - For now we avoid usage of the @, to avoid the "tld zip" attack vector
    '\u{2044}', // U+2044 ==  ⁄ Fraction Slash
    '\u{2215}', // U+2215 ==  ∕ Division Slash
    '\u{FF0F}', // U+2215 == ／ Fullwidth Solidus
    '\u{29F8}', // U+29F8 == ⧸ Big Solidus
];

#[derive(Debug, PartialEq, Eq)]
pub enum UrlProblem {
    InvalidExtensionSuffix(String),
    MissingTarExt,
    InvalidFragment(String),
    MissingHash,
    MissingHttps,
    MisleadingCharacter,
}

impl<'a> TryFrom<&'a str> for PackageMetadata<'a> {
    type Error = UrlProblem;

    fn try_from(url: &'a str) -> Result<Self, Self::Error> {
        PackageMetadata::new(url)
    }
}

impl<'a> PackageMetadata<'a> {
    fn new(url: &'a str) -> Result<Self, UrlProblem> {
        // First, verify that the URL starts with https://
        let without_protocol = match url.split_once("https://") {
            Some((_, without_protocol)) => without_protocol,
            None => {
                return Err(UrlProblem::MissingHttps);
            }
        };

        // Next, check if there are misleading characters in the URL
        if url
            .chars()
            .any(|ch| MISLEADING_CHARACTERS_IN_URL.contains(&ch))
        {
            return Err(UrlProblem::MisleadingCharacter);
        }

        // Next, get the (optional) URL fragment, which must be a .roc filename
        let (without_fragment, fragment) = match without_protocol.rsplit_once('#') {
            Some((before_fragment, fragment)) => {
                const EXT: &str = ".roc";

                // The fragment must be a .roc file, and the part before ".roc" can't be empty
                if fragment.ends_with(EXT) && fragment.len() > EXT.len() {
                    (before_fragment, Some(fragment))
                } else {
                    return Err(UrlProblem::InvalidFragment(fragment.to_string()));
                }
            }
            None => (without_protocol, None),
        };

        // The tarball name is everything after the "/" (without the .tar extension)
        // The URL must end in .tar followed optionally by ".gz", ".br", etc. (excluding the fragment)
        let without_ext = match without_fragment.rsplit_once(".tar") {
            Some((before_ext, after_ext)) => {
                if after_ext.is_empty() || VALID_EXTENSION_SUFFIXES.contains(&after_ext) {
                    before_ext
                } else {
                    return Err(UrlProblem::InvalidExtensionSuffix(after_ext.to_string()));
                }
            }
            None => {
                // The URL didn't end in .tar at all
                return Err(UrlProblem::MissingTarExt);
            }
        };

        let (path, tarball_name) = match without_ext.rsplit_once('/') {
            Some((path, hash)) if !hash.is_empty() => (path, hash),
            _ => {
                return Err(UrlProblem::MissingHash);
            }
        };

        Ok(PackageMetadata {
            cache_subdir: path,
            content_hash: tarball_name,
            root_module_filename: fragment,
        })
    }
}

#[test]
fn url_problem_missing_https() {
    let expected = Err(UrlProblem::MissingHttps);
    assert_eq!(PackageMetadata::try_from("http://example.com"), expected);
}

#[test]
fn url_problem_misleading_characters() {
    let expected = Err(UrlProblem::MisleadingCharacter);

    for misleading_character_example in [
        "https://user:password@example.com/",
        //"https://example.com⁄path",
        "https://example.com\u{2044}path",
        //"https://example.com∕path",
        "https://example.com\u{2215}path",
        //"https://example.com／path",
        "https://example.com\u{ff0f}path",
        //"https://example.com⧸path",
        "https://example.com\u{29f8}path",
    ] {
        assert_eq!(
            PackageMetadata::try_from(misleading_character_example),
            expected
        );
    }
}

#[test]
fn url_problem_invalid_fragment_not_a_roc_file() {
    let expected = Err(UrlProblem::InvalidFragment("filename.sh".to_string()));
    assert_eq!(
        PackageMetadata::try_from("https://example.com/#filename.sh"),
        expected
    );
}

#[test]
fn url_problem_invalid_fragment_empty_roc_filename() {
    let expected = Err(UrlProblem::InvalidFragment(".roc".to_string()));
    assert_eq!(
        PackageMetadata::try_from("https://example.com/#.roc"),
        expected
    );
}

#[test]
fn url_problem_not_a_tar_url() {
    let expected = Err(UrlProblem::MissingTarExt);
    assert_eq!(
        PackageMetadata::try_from("https://example.com/filename.zip"),
        expected
    );
}

#[test]
fn url_problem_invalid_tar_suffix() {
    let expected = Err(UrlProblem::InvalidExtensionSuffix(".zip".to_string()));
    assert_eq!(
        PackageMetadata::try_from("https://example.com/filename.tar.zip"),
        expected
    );
}

#[test]
fn url_problem_missing_hash() {
    let expected = Err(UrlProblem::MissingHash);
    assert_eq!(
        PackageMetadata::try_from("https://example.com/.tar.gz"),
        expected
    );
}

#[test]
fn url_without_fragment() {
    let expected = Ok(PackageMetadata {
        cache_subdir: "example.com/path",
        content_hash: "hash",
        root_module_filename: None,
    });
    assert_eq!(
        PackageMetadata::try_from("https://example.com/path/hash.tar.gz"),
        expected
    );
}

#[test]
fn url_with_fragment() {
    let expected = Ok(PackageMetadata {
        cache_subdir: "example.com/path",
        content_hash: "hash",
        root_module_filename: Some("filename.roc"),
    });
    assert_eq!(
        PackageMetadata::try_from("https://example.com/path/hash.tar.gz#filename.roc"),
        expected
    );
}

#[derive(Debug)]
pub enum Problem {
    UnsupportedEncoding(String),
    MultipleEncodings(String),
    InvalidContentHash {
        expected: String,
        actual: String,
    },
    IoErr(io::Error),
    FsExtraErr(fs_extra::error::Error),
    HttpErr(reqwest::Error),
    InvalidUrl(UrlProblem),
    /// The Content-Length header of the response exceeded max_download_bytes
    DownloadTooBig(u64),
    NotFound,
}

pub fn download_and_hash(
    url: &str,
    dest_dir: &Path,
    max_download_bytes: u64,
) -> Result<String, Problem> {
    // TODO apparently it really improves performance to construct a Client once and then reuse it,
    // instead of making a new Client for every request.
    // Per https://github.com/seanmonstar/reqwest/issues/1454#issuecomment-1026076701
    let resp = reqwest::blocking::Client::new()
        .get(url)
        .send()
        .map_err(Problem::HttpErr)?;

    if resp.status() == reqwest::StatusCode::NOT_FOUND {
        return Err(Problem::NotFound);
    }

    // Some servers don't return Content-Length - e.g. Netlify seems to only sometimes return it.
    // If they do, and if it says the file is going to be too big, don't bother downloading it!
    if let Some(content_len) = resp.content_length() {
        if content_len > max_download_bytes {
            return Err(Problem::DownloadTooBig(content_len));
        }
    }

    // The server can respond with multiple encodings, per
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Encoding
    // ...but we don't support that.
    let encoding = {
        let content_encoding = match resp.headers().get("content-encoding") {
            Some(header) => header.to_str().unwrap_or_default(),
            None => "",
        };

        Encoding::new(content_encoding, url)?
    };

    let content_length = resp.content_length().map(|n| n as usize);

    // Use .take to prevent a malicious server from sending back bytes
    // until system resources are exhausted!
    let resp = ProgressReporter::new(resp.take(max_download_bytes), content_length);
    decompress_into(dest_dir, encoding, resp)
}

/// The content encodings we support
#[derive(Debug, Clone, Copy, PartialEq)]
enum Encoding {
    Gzip,
    Brotli,
    Deflate,
    Uncompressed,
}

impl Encoding {
    pub fn new(content_encoding: &str, url: &str) -> Result<Self, Problem> {
        use Encoding::*;

        // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Encoding#directives
        match content_encoding {
            "br" => Ok(Brotli),
            "gzip" => Ok(Gzip),
            "deflate" => Ok(Deflate),
            "" => {
                // There was no Content-Encoding header, but we can infer the encoding
                // from the file extension in the URL.
                let end_of_ext = url.rfind('#').unwrap_or(url.len());

                // Drop the URL fragment when determining file extension
                match url[0..end_of_ext].rsplit_once('.') {
                    Some((_, after_dot)) => match Compression::from_file_ext(after_dot) {
                        Some(Compression::Brotli) => Ok(Self::Brotli),
                        Some(Compression::Gzip) => Ok(Self::Gzip),
                        Some(Compression::Uncompressed) | None => Ok(Self::Uncompressed),
                    },
                    None => Ok(Uncompressed),
                }
            }
            other => {
                if other.contains(',') {
                    // We don't support multiple encodings (although the spec for the HTTP header
                    // permits a comma-separated list)
                    Err(Problem::MultipleEncodings(other.to_string()))
                } else {
                    // We don't support other encodings
                    Err(Problem::UnsupportedEncoding(other.to_string()))
                }
            }
        }
    }
}

#[test]
fn encoding_from_tar_br() {
    let actual = Encoding::new(
        "",
        "https://example.com/jDRlAFAA3738vu3-vMpLUoyxtA86Z7CaZneoOKrihbE.tar.br",
    )
    .unwrap();

    assert_eq!(Encoding::Brotli, actual);
}

fn hash_and_unpack(dest_dir: &Path, reader: impl Read) -> Result<String, Problem> {
    let mut hash_reader = HashReader::new(reader);

    tar::Archive::new(&mut hash_reader)
        .unpack(dest_dir)
        .map_err(Problem::IoErr)?;

    let mut buf = Vec::with_capacity(1024);

    // Archive::new() doesn't always read all the bytes, but we need to read them all
    // in order to get the correct hash!
    hash_reader.read_to_end(&mut buf).map_err(Problem::IoErr)?;

    Ok(base64_url::encode(hash_reader.finalize().as_bytes()))
}

/// Read from the given reader, decompress the bytes using the given Content-Encoding string,
/// write them to the given writer, and return the base64url-encoded BLAKE3 hash of what was written.
/// This both writes and hashes incrementally as it reads, so the only extra work that's done
/// at the end is base64url-encoding the final hash.
fn decompress_into(
    dest_dir: &Path,
    encoding: Encoding,
    reader: impl Read,
) -> Result<String, Problem> {
    match encoding {
        Encoding::Brotli => hash_and_unpack(
            dest_dir,
            brotli::Decompressor::new(reader, BROTLI_BUFFER_BYTES),
        ),
        Encoding::Gzip => {
            // Note: GzDecoder::new immediately parses the gzip header (so, calls read())
            hash_and_unpack(dest_dir, flate2::read::GzDecoder::new(reader))
        }
        Encoding::Deflate => hash_and_unpack(dest_dir, flate2::read::DeflateDecoder::new(reader)),
        Encoding::Uncompressed => hash_and_unpack(dest_dir, reader),
    }
}

/// Read something while calculating its BLAKE3 hash
struct HashReader<R: Read> {
    reader: R,
    hasher: blake3::Hasher,
}

impl<R: Read> HashReader<R> {
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            hasher: blake3::Hasher::new(),
        }
    }

    pub fn finalize(&self) -> blake3::Hash {
        self.hasher.finalize()
    }
}

impl<R: Read> Read for HashReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let bytes_read = self.reader.read(buf)?;

        self.hasher.update(&buf[0..bytes_read]);

        Ok(bytes_read)
    }
}

/// Prints download progress to stdout
struct ProgressReporter<R: Read> {
    read: usize,
    total: Option<usize>,
    reader: R,
}

impl<R: Read> ProgressReporter<R> {
    fn new(reader: R, total: Option<usize>) -> Self {
        ProgressReporter {
            read: 0,
            total,
            reader,
        }
    }
}

impl<R: Read> Read for ProgressReporter<R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let size = self.reader.read(buf)?;

        self.read += size;

        if let Some(total) = self.total {
            let total = total as f32 / 1_000_000.0;
            let read = self.read as f32 / 1_000_000.0;

            if total < 1.0 {
                eprint!(
                    "\u{001b}[2K\u{001b}[G[{:.1} / {:.1} KB]",
                    // Convert MB to KB
                    read * 1000.0,
                    total * 1000.0,
                );
            } else {
                eprint!("\u{001b}[2K\u{001b}[G[{:.1} / {:.1} MB]", read, total,);
            }
        } else {
            eprint!(
                "\u{001b}[2K\u{001b}[G[{:.1} MB]",
                self.read as f32 / 1_000_000.0,
            );
        }
        std::io::stderr().flush()?;

        if self.total.is_some_and(|total| self.read >= total) {
            eprintln!();
        }

        Ok(size)
    }
}
