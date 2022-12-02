use brotli::enc::BrotliEncoderParams;
use bumpalo::Bump;
use flate2::write::GzEncoder;
use roc_parse::ast::{Header, Module};
use roc_parse::header::PlatformHeader;
use roc_parse::module::parse_header;
use roc_parse::state::State;
use std::ffi::OsStr;
use std::fs::File;
use std::io::{self, Read, Write};
use std::path::Path;
use tar;
use walkdir::WalkDir;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Compression {
    Brotli,
    Gzip,
    Uncompressed,
}

impl Compression {
    const fn file_ext(&self) -> &'static str {
        match self {
            Compression::Brotli => ".tar.br",
            Compression::Gzip => ".tar.gz",
            Compression::Uncompressed => ".tar",
        }
    }

    pub fn from_file_ext(ext: &str) -> Option<Self> {
        match ext {
            "tar" => Some(Self::Uncompressed),
            "gz" => Some(Self::Gzip),
            "br" => Some(Self::Brotli),
            _ => None,
        }
    }
}

impl<'a> TryFrom<&'a str> for Compression {
    type Error = ();

    fn try_from(extension: &'a str) -> Result<Self, Self::Error> {
        if extension.ends_with(".br") {
            Ok(Compression::Brotli)
        } else if extension.ends_with(".gz") {
            Ok(Compression::Gzip)
        } else if extension.ends_with(".tar") {
            Ok(Compression::Uncompressed)
        } else {
            Err(())
        }
    }
}

/// Given a path to a .roc file, write a .tar file to disk.
///
/// The .tar file will be in the same directory, and its filename
/// will be the hash of its contents. This function returns
/// the name of that filename (including the .tar extension),
/// so the caller can obtain the path to the file by calling
/// Path::with_file_name(returned_string) on the Path argument it provided.
pub fn build(path_to_main: &Path, compression: Compression) -> io::Result<String> {
    let mut archive_bytes = Vec::new();

    write_archive(path_to_main, &mut archive_bytes)?;

    // Now that we have our compressed archive, get its BLAKE3 hash
    // and base64url encode it. Use base64url encoding because:
    // - It's more concise than hex encoding, so the URL can be shorter
    // - Unlike base64 encoding, it's URL-frienly (e.g. won't include slashes)
    let hash = base64_url::encode(blake3::hash(&archive_bytes).as_bytes());
    let mut filename = hash;

    filename.push_str(compression.file_ext());

    // Write the bytes to disk.
    {
        let dest_path = path_to_main.with_file_name(&filename);
        let mut file = File::create(&dest_path).unwrap_or_else(|err| {
            panic!(
                "Unable to open {} for writing - error was: {:?}",
                dest_path.to_string_lossy(),
                err
            );
        });

        match compression {
            Compression::Brotli => {
                brotli::BrotliCompress(
                    &mut archive_bytes.as_slice(),
                    &mut file,
                    &BrotliEncoderParams {
                        quality: 11,
                        use_dictionary: true,
                        ..Default::default()
                    },
                )?;
            }
            Compression::Gzip => {
                let mut encoder = GzEncoder::new(&mut file, flate2::Compression::fast());
                encoder.write_all(&archive_bytes)?;
                encoder.finish()?;
            }
            Compression::Uncompressed => file.write_all(&archive_bytes)?,
        };
    }

    Ok(filename)
}

/// Write an uncompressed tar archive to the given writer.
fn write_archive<W: Write>(path: &Path, writer: W) -> io::Result<()> {
    let root_dir = if let Some(parent) = path.parent() {
        parent
    } else {
        eprintln!(
            "{} is a directory, not a .roc file. Please specify a .roc file!",
            path.to_string_lossy()
        );
        std::process::exit(1);
    };
    let mut builder = tar::Builder::new(writer);
    let arena = Bump::new();
    let mut buf = Vec::new();

    // TODO use this when finding .roc files by discovering them from the root module.
    // let other_modules: &[Module<'_>] =
    match read_header(&arena, &mut buf, path)?.header {
        Header::Interface(_) => {
            todo!();
            // TODO report error
        }
        Header::App(_) => {
            todo!();
            // TODO report error
        }
        Header::Hosted(_) => {
            todo!();
            // TODO report error
        }
        Header::Package(_) => {
            add_dot_roc_files(root_dir, &mut builder)?;
        }
        Header::Platform(PlatformHeader { imports: _, .. }) => {
            // Add all the prebuilt host files to the archive.
            // These should all be in the same directory as the platform module.
            for entry in std::fs::read_dir(root_dir)? {
                let path = entry?.path();

                if [
                    // surgical linker format
                    Some("rh1"),
                    // metadata file
                    Some("rm1"),
                    // legacy linker formats
                    Some("o"),
                    Some("obj"),
                    Some("wasm"),
                    // optimized wasm builds compile to .zig for now,
                    // because zig can't emit .bc for wasm yet.
                    Some("zig"),
                ]
                .contains(&path.extension().and_then(OsStr::to_str))
                {
                    builder.append_path_with_name(
                        &path,
                        // Store it without the root path, so that (for example) we don't store
                        // `examples/cli/main.roc` and therefore end up with the root of the tarball
                        // being an `examples/cli/` dir instead of having `main.roc` in the root.
                        path.strip_prefix(root_dir).unwrap(),
                    )?;
                }
            }

            add_dot_roc_files(root_dir, &mut builder)?;
        }
    };

    // TODO: This will be necessary when bundling packages (not platforms, since platforms just
    // slurp up the whole directory at the moment) and also platforms in a future where they
    // have precompiled hosts, and we only need to grab the .roc files and the precompiled hostfiles!
    // {
    //     // Repeat this process on each of the root module's imports.
    //     let mut stack = Vec::from_iter_in(other_modules, &arena);
    //     let mut visited_paths = HashSet::from_iter([path]);

    //     // We could do this all in parallel, but a simple stack seems fast enough for this use case.
    //     while let Some(path) = stack.pop() {
    //         let other_modules = match read_header(&arena, &mut buf, path) {
    //             Module::Interface { .. } => {
    //                 // TODO use header.imports
    //                 builder.append_path(path)?;
    //             }
    //             Module::App { .. } => {
    //                 // TODO report error
    //             }
    //             Module::Hosted { header } => {
    //                 // TODO report error
    //             }
    //             Module::Platform { header } => {
    //                 // TODO report error
    //             }
    //         };
    //         let other_paths = todo!("infer from other_modules");

    //         // Recurse on the other paths in the header
    //         for other_path in other_paths {
    //             if !visited_paths.contains(other_path) {
    //                 stack.push(other_path);
    //             }
    //         }

    //         paths_visited.insert(path);
    //     }
    // }

    builder.finish()
}

fn add_dot_roc_files<W: Write>(
    root_dir: &Path,
    builder: &mut tar::Builder<W>,
) -> Result<(), io::Error> {
    Ok(
        for entry in WalkDir::new(root_dir).into_iter().filter_entry(|entry| {
            let path = entry.path();

            // Ignore everything except directories and .roc files
            path.is_dir() || path.extension().and_then(OsStr::to_str) == Some("roc")
        }) {
            let entry = entry?;
            let path = entry.path();

            // Only include files, not directories or symlinks.
            // Symlinks may not work on Windows, and directories will get automatically
            // added based on the paths of the files inside anyway. (In fact, if we don't
            // filter out directories in this step, then empty ones can sometimes be added!)
            if path.is_file() {
                builder.append_path_with_name(
                    path,
                    // Store it without the root path, so that (for example) we don't store
                    // `examples/cli/main.roc` and therefore end up with the root of the tarball
                    // being an `examples/cli/` dir instead of having `main.roc` in the root.
                    path.strip_prefix(root_dir).unwrap(),
                )?;
            }
        },
    )
}

fn read_header<'a>(
    arena: &'a Bump,
    buf: &'a mut Vec<u8>,
    path: &'a Path,
) -> io::Result<Module<'a>> {
    // Read all the bytes into the buffer.
    {
        let mut file = File::open(path)?;
        buf.clear();
        file.read_to_end(buf)?;
    }

    // TODO avoid copying the contents of the file into a Bumpalo arena by doing multiple
    // https://doc.rust-lang.org/std/io/trait.Read.html#tymethod.read calls instead of
    // using the much more convenient file.read_to_end - which requires a std::vec::Vec.
    // (We can't use that for the parser state and still return Module<'a> unfortunately.)
    let arena_buf = bumpalo::collections::Vec::from_iter_in(buf.iter().copied(), arena);
    let parse_state = State::new(arena_buf.into_bump_slice());
    let (module, _) = parse_header(arena, parse_state).unwrap_or_else(|_err| {
        todo!(); // TODO report a nice error and exit 1 - or maybe just return Err, for better testability?
    });

    Ok(module)
}
