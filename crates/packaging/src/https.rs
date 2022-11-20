use blake3::Hasher;
use brotli_decompressor;
use core::slice::SlicePattern;
use flate2;
use std::fs::File;
use std::io::{self, ErrorKind, Read, Write};
use std::path::Path;

use crate::tarball;

// gzip should be the most widely supported, and brotli offers the highest compession.
// flate2 gets us both gzip and deflate, so there's no harm in offering deflate too.
//
// Here are all the officially supported options: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept-Encoding
// We can consider supporting more, but that would bloat the `roc` binary more, so
// let's try to avoid doing that.
const ACCEPT_ENCODING: &str = "br, gzip, deflate";

const TARBALL_BUFFER_BYTES: usize = 8 * 1_000_000; // MB
const BROTLI_BUFFER_BYTES: usize = 8 * 1_000_000; // MB

pub struct ValidUrl<'a> {
    path: &'a str,
    tarball_name: &'a str,
    fragment: Option<&'a str>,
}

/// Valid URLs must end in one of these:
///
///     .tar
///     .tar.gz
///     .tar.br
const VALID_EXTENSION_SUFFIXES: [&str; 2] = [".gz", ".br"];
const EXPECTED_HASH_LENGTH: usize = 5;

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
        let (without_fragment, fragment) = match without_protocol.rsplit_once("#") {
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

        let (path, tarball_name) = match without_ext.rsplit_once("/") {
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

pub fn get(url: &str, dest: &Path) -> io::Result<Vec<u8>> {
    let (mut file, hash) = download_tarball(url, dest)?;

    file.read_to_end(&mut bytes).unwrap();

    let expected_hash = todo!();
}

/// Download and decompress the given URL
fn download_tarball(url: &str, dest: &Path) -> io::Result<File> {
    let result = ureq::get(url)
        .set("Accept-Encoding", ACCEPT_ENCODING)
        .call();

    match result {
        Ok(resp) => {
            let mut bytes;
            let encoding;

            match (
                resp.header("Content-Length").map(str::parse),
                resp.header("Content-Encoding"),
            ) {
                (Some(Ok(len)), Some(content_encoding)) => {
                    bytes = Vec::with_capacity(len);
                    encoding = content_encoding.to_string();
                }
                (_, _) => todo!(),
            }

            // Use .take to prevent a malicious server from sending back bytes
            // until we exhaust system resources!
            let tarball_reader = resp.into_reader().take(10_000_000);

            // Download the tarball into a tempfile, which we will return; the caller can
            // move it once it's been verified.
            let mut tarball_dest = tempfile::tempfile()?;

            match encoding.trim().to_lowercase().as_str() {
                "br" => {
                    let brotli_reader =
                        brotli_decompressor::Decompressor::new(tarball_reader, BROTLI_BUFFER_BYTES);

                    let downloader = Downloader::new(brotli_reader, tarball_dest);

                    downloader.read_to_end(buf)
                }
                _ => {
                    // We don't support other encodings, including mutliple encodings (although
                    // the spec for the HTTP header permits a comma-separated list.
                }
            }

            // The server can respond with multiple encodings, per
            // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Encoding

            Ok(tempfile)
        }
        Err(_) => todo!(),
    }
}

/// Downloads a file while also computing a hash of its contents.
struct Downloader<R: Read, W: Write> {
    hasher: Hasher,
    reader: R,
    writer: W,
}

impl<R: Read, W: Write> Downloader<R, W> {
    pub fn new(reader: R, writer: W) -> Self {
        Self {
            reader,
            writer,
            hasher: blake3::Hasher::new(),
        }
    }

    /// Download the data from the reader into the writer, while hashing
    /// along the way, then return the base64url-enceoded hash once it's done.
    pub fn run(&mut self, buf_size: usize) -> io::Result<String> {
        let mut buf = Vec::with_capacity(buf_size);
        let reader = &mut self.reader;
        let writer = &mut self.writer;
        let hasher = &mut self.hasher;

        loop {
            match reader.read(&mut buf) {
                Ok(0) => {
                    break;
                }
                Ok(bytes_read) => {
                    writer.write(buf.as_slice())?;

                    // Incorporate the bytes we just read into the hash.
                    hasher.update(&buf);

                    // Reset the buffer for the next read.
                    buf.clear();
                }
                Err(e) if e.kind() == ErrorKind::Interrupted => continue,
                Err(e) => return Err(e),
            }
        }

        Ok(base64_url::encode(hasher.finalize().as_bytes()))
    }
}

impl<R: Read, W: Write> Read for Downloader<R, W> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let initial_buf_len = buf.len();
        let bytes_read = self.reader.read(buf)?;

        // Update the hasher with the bytes we just read.
        self.hasher
            .update(&buf[initial_buf_len..initial_buf_len.checked_add(bytes_read).unwrap()]);

        Ok(bytes_read)
    }
}

impl<R: Read, W: Write> Write for Downloader<R, W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.writer.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.writer.flush()
    }
}
