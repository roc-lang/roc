use bumpalo::Bump;
use roc_parse::header::PlatformHeader;
use roc_parse::module::parse_header;
use roc_parse::state::State;
use std::fs::File;
use std::io::{self, Read, Write};
use std::path::Path;
use tar;
use walkdir::DirEntry;

use roc_parse::ast::Module;

const EXTENSION: &str = "rp1";

/// Given a path to a .roc file, write a .rp1 file to disk.
///
/// The .rp1 file will be in the same directory, and its filename
/// will be the hash of its contents. This function returns
/// the name of that filename (including the .rp1 extension),
/// so the caller can obtain the path to the file by calling
/// Path::with_file_name(returned_string) on the Path argument it provided.
pub fn build(path_to_main: &Path) -> io::Result<String> {
    let archive_bytes = build_compressed_archive(path_to_main)?;

    // Now that we have our compressed archive, get its BLAKE3 hash
    // and base64url encode it. Use base64url encoding because:
    // - It's more concise than hex encoding, so the URL can be shorter
    // - Unlike base64 encoding, it's URL-frienly (e.g. won't include slashes)
    let hash = base64_url::encode(blake3::hash(&archive_bytes).as_bytes());
    let mut filename = hash;

    filename.push('.');
    filename.push_str(EXTENSION);

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

        file.write_all(&archive_bytes).unwrap_or_else(|err| {
            panic!(
                "Unable to write to {} - error was: {:?}",
                dest_path.to_string_lossy(),
                err
            );
        });
    }

    Ok(filename)
}

fn build_compressed_archive(path_to_main: &Path) -> io::Result<Vec<u8>> {
    let mut buf = Vec::new();

    // As we write each archive entry to the buffer, LZ4 compress it.
    let writer = lz4_flex::frame::FrameEncoder::new(&mut buf);

    write_archive(path_to_main, writer)?;

    Ok(buf)
}

/// Write an uncompressed rp1 archive to the given writer.
fn write_archive<W: Write>(path: &Path, writer: W) -> io::Result<()> {
    let mut builder = tar::Builder::new(writer);
    let arena = Bump::new();
    let mut buf = Vec::new();

    let _other_modules: &[Module<'_>] = match read_header(&arena, &mut buf, path)? {
        Module::Interface { .. } => {
            todo!();
            // TODO report error
        }
        Module::App { .. } => {
            todo!();
            // TODO report error
        }
        Module::Hosted { .. } => {
            todo!();
            // TODO report error
        }
        Module::Platform {
            header: PlatformHeader { imports: _, .. },
        } => {
            // TODO use header.imports to find dependencies to import.
            // We don't need to bother with that at the moment, since
            // we currently just slurp up the entire directory anyway.
            // builder.append_path(path)?;

            // Since this is a platform, we want all of its sources.
            // TODO in the future, don't do this! Instead, just get the .roc files we need
            // by traversing the root's imports, and then also get the precompiled hosts.
            use walkdir::WalkDir;

            // Returns true iff we should keep the given directory entry, or else skip it
            // (including not recursing into it, if it was a directory.) This lets us skip
            // zig-cache, target, etc.
            fn is_keeper(entry: &DirEntry) -> bool {
                let path = entry.path();

                // Ignore all hidden files and directories.
                if path.starts_with(".") {
                    return false;
                }

                if path.is_dir() {
                    let filename = entry.file_name().to_str();

                    // Skip these. None of this will be necessary once we have
                    // precompiled hosts, but for now, this is prudent!
                    filename != Some("target") && filename != Some("zig-cache")
                } else {
                    // keep all other files
                    true
                }
            }

            for result in WalkDir::new(path.parent().unwrap())
                .into_iter()
                .filter_entry(is_keeper)
            {
                match result {
                    Ok(entry) => {
                        builder.append_path(entry.path())?;
                    }
                    Err(_) => todo!(), // TODO gracefully handle error traversing directory
                }
            }

            &[]
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
