use bumpalo::Bump;
use roc_parse::ast::Module;
use roc_parse::header::PlatformHeader;
use roc_parse::module::parse_header;
use roc_parse::state::State;
use std::ffi::OsStr;
use std::fs::File;
use std::io::{self, Read, Write};
use std::path::Path;
use tar;

/// Given a path to a .roc file, write a .tar file to disk.
///
/// The .tar file will be in the same directory, and its filename
/// will be the hash of its contents. This function returns
/// the name of that filename (including the .tar extension),
/// so the caller can obtain the path to the file by calling
/// Path::with_file_name(returned_string) on the Path argument it provided.
pub fn build(path_to_main: &Path) -> io::Result<String> {
    let mut archive_bytes = Vec::new();

    write_archive(path_to_main, &mut archive_bytes)?;

    // Now that we have our compressed archive, get its BLAKE3 hash
    // and base64url encode it. Use base64url encoding because:
    // - It's more concise than hex encoding, so the URL can be shorter
    // - Unlike base64 encoding, it's URL-frienly (e.g. won't include slashes)
    let hash = base64_url::encode(blake3::hash(&archive_bytes).as_bytes());
    let mut filename = hash;

    filename.push_str(".tar");

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
            use walkdir::WalkDir;

            let platform_dir = path.parent().unwrap();

            // Add all the prebuild host files to the archive.
            // These should all be in the same directory as the platform module.
            for entry in std::fs::read_dir(platform_dir)? {
                let path = entry?.path();

                if [
                    // surgical linker format
                    Some("rh1"),
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
                    builder.append_path(path)?;
                }
            }

            // Recursively find all the .roc files and add them.
            // TODO we can do this more efficiently by parsing the platform module and finding
            // all of its dependencies. See below for a commented-out WIP sketch of this.
            //
            // The WalkDir approach is easier to implement, but has the downside of doing things
            // like traversing target/ and zig-cache/ which can be large but will never have
            // any .roc files in them!
            for entry in WalkDir::new(path.parent().unwrap())
                .into_iter()
                .filter_entry(|entry| {
                    entry.path().extension().and_then(OsStr::to_str) == Some("roc")
                })
            {
                builder.append_path(entry?.path())?;
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
