use blake3::Hasher;
use flate2;
use std::io::{self, ErrorKind, Read, Write};

// gzip should be the most widely supported, and brotli offers the highest compession.
// flate2 gets us both gzip and deflate, so there's no harm in offering deflate too.
//
// Here are all the officially supported options: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept-Encoding
// We can consider supporting more, but that would bloat the `roc` binary more, so
// let's try to avoid doing that.
const ACCEPT_ENCODING: &str = "br, gzip, deflate";
const BROTLI_BUFFER_BYTES: usize = 8 * 1_000_000; // MB
const DOWNLOAD_CHUNK_SIZE: usize = 4096;

pub struct ValidUrl<'a> {
    pub path: &'a str,
    pub tarball_name: &'a str,
    pub fragment: Option<&'a str>,
}

/// Valid URLs must end in one of these:
///
///     .tar
///     .tar.gz
///     .tar.br
const VALID_EXTENSION_SUFFIXES: [&str; 2] = [".gz", ".br"];

pub enum UrlProblem {
    InvalidExtensionSuffix(String),
    MissingTarExt,
    InvalidFragment(String),
    MissingHash,
    MissingHttps,
}

impl<'a> ValidUrl<'a> {
    pub fn new(url: &'a str) -> Result<Self, UrlProblem> {
        // First, verify that the URL starts with https://
        let without_protocol = match url.split_once("https://") {
            Some((_, without_protocol)) => without_protocol,
            None => {
                return Err(UrlProblem::MissingHttps);
            }
        };

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

        Ok(ValidUrl {
            path,
            tarball_name,
            fragment,
        })
    }
}

pub enum Problem {
    UnsupportedEncoding(String),
    InvalidContentHash {
        expected: String,
        actual: String,
    },
    IoErr(io::Error),
    HttpErr(ureq::Error),
    UrlProblem(UrlProblem),
    /// The Content-Length header of the response exceeded MAX_DOWNLOAD_SIZE
    DownloadTooBig(usize),
    InvalidContentLengthHeader,
    MissingContentLengthHeader,
    MissingContentEncodingHeader,
}

/// Download and decompress the given URL, verifying its contents against the hash in the URL.
/// Downloads it into a tempfile.
pub fn download_and_verify<'a>(
    url: &'a str,
    dest: &mut impl Write,
    max_download_bytes: u64,
) -> Result<ValidUrl<'a>, Problem> {
    let valid_url = ValidUrl::new(url).map_err(Problem::UrlProblem)?;
    let resp = ureq::get(url)
        .set("Accept-Encoding", ACCEPT_ENCODING)
        .call()
        .map_err(Problem::HttpErr)?;

    match (
        resp.header("Content-Length").map(str::parse),
        resp.header("Content-Encoding"),
    ) {
        (Some(Ok(content_len)), Some(content_encoding)) => {
            if content_len as u64 > max_download_bytes {
                return Err(Problem::DownloadTooBig(content_len));
            }
            let encoding = content_encoding.try_into()?;

            // Use .take to prevent a malicious server from sending back bytes
            // until system resources are exhausted!
            let mut reader = resp.into_reader().take(max_download_bytes);

            // The server can respond with multiple encodings, per
            // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Encoding
            // ...but we don't support that.
            let hash = download(encoding, &mut reader, dest)?;

            // The tarball name is the hash of its contents
            if hash == valid_url.tarball_name {
                Ok(valid_url)
            } else {
                Err(Problem::InvalidContentHash {
                    expected: valid_url.tarball_name.to_string(),
                    actual: hash,
                })
            }
        }
        (Some(Err(_)), _) => {
            // The Content-Length header wasn't an integer
            Err(Problem::InvalidContentLengthHeader)
        }
        (None, _) => Err(Problem::MissingContentLengthHeader),
        (_, None) => Err(Problem::MissingContentEncodingHeader),
    }
}

/// The content encodings we support
enum Encoding {
    Gzip,
    Brotli,
    Deflate,
}

impl TryFrom<&str> for Encoding {
    type Error = Problem;

    fn try_from(content_encoding: &str) -> Result<Self, Self::Error> {
        use Encoding::*;

        // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Encoding#directives
        match content_encoding {
            "br" => Ok(Brotli),
            "gzip" => Ok(Gzip),
            "deflate" => Ok(Deflate),
            other => {
                // We don't support other encodings, including mutliple encodings (although
                // the spec for the HTTP header permits a comma-separated list.
                Err(Problem::UnsupportedEncoding(other.to_string()))
            }
        }
    }
}

/// Read from the given reader, decompress the bytes using the given Content-Encoding string,
/// write them to the given writer, and return the base64url-encoded BLAKE3 hash of what was written.
/// This both writes and hashes incrementally as it reads, so the only extra work that's done
/// at the end is base64url-encoding the final hash.
fn download<R: Read, W: Write>(
    encoding: Encoding,
    reader: &mut R,
    writer: &mut W,
) -> Result<String, Problem> {
    match encoding {
        Encoding::Brotli => {
            let mut brotli_reader = brotli::Decompressor::new(reader, BROTLI_BUFFER_BYTES);

            write_and_hash(&mut brotli_reader, writer).map_err(Problem::IoErr)
        }
        Encoding::Gzip => {
            // Note: GzDecoder::new immediately parses the gzip header (so, calls read())
            let mut gzip_reader = flate2::read::GzDecoder::new(reader);

            write_and_hash(&mut gzip_reader, writer).map_err(Problem::IoErr)
        }
        Encoding::Deflate => {
            let mut deflate_reader = flate2::read::DeflateDecoder::new(reader);

            write_and_hash(&mut deflate_reader, writer).map_err(Problem::IoErr)
        }
    }
}

/// Download the data from the reader into the writer, while hashing
/// along the way, then return the base64url-enceoded hash once it's done.
pub fn write_and_hash<R: Read, W: Write>(reader: &mut R, writer: &mut W) -> io::Result<String> {
    let mut buf = Vec::with_capacity(DOWNLOAD_CHUNK_SIZE);
    let mut hasher = Hasher::new();

    loop {
        match reader.read(&mut buf) {
            Ok(0) => {
                // We ran out of bytes to read, so we're done!
                return Ok(base64_url::encode(hasher.finalize().as_bytes()));
            }
            Ok(_) => {
                // Incorporate the bytes we just read into the hash.
                hasher.update(&buf);

                // Write all the bytes we just read until they've all been written.
                {
                    let mut to_write = buf.as_slice();

                    loop {
                        match writer.write(to_write) {
                            Ok(0) => {
                                // We wrote everything. All done writing!
                                break;
                            }
                            Ok(bytes_written) => {
                                // Advance the buffer so we don't write the same bytes again!
                                to_write = &to_write[bytes_written..];
                            }
                            Err(err) if err.kind() == ErrorKind::Interrupted => {
                                // No action needed, just retry on the next iteration of the loop.
                            }
                            Err(err) => return Err(err),
                        }
                    }
                }

                // Reset the buffer for the next read.
                buf.clear();
            }
            Err(err) if err.kind() == ErrorKind::Interrupted => {
                // No action needed, just retry on the next iteration of the loop.
            }
            Err(err) => return Err(err),
        }
    }
}
