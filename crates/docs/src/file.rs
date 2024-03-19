use crate::problem::Problem;
use bumpalo::Bump;
use std::{fs, io::ErrorKind, path::Path};

pub struct Assets<S: AsRef<str>> {
    pub search_js: S,
    pub styles_css: S,
    pub favicon_svg: S,
    pub raw_template_html: S,
}

pub fn populate_build_dir<'a, S: AsRef<str>>(
    arena: &'a Bump,
    build_dir: &Path,
    assets: &Assets<S>,
) -> Result<(), Problem> {
    // Clear out the generated-docs dir (we'll create a fresh one at the end)

    // TODO it would be better UX if, instead of removing the dir and then
    // recreating it, we:
    // 1. Try to create the dir (recursively creating intermediate dirs as necessary)
    // 2. If the final dir already existed, recursively delete its contents, but don't delete the dir itself
    //
    // This way, you could leave a http-server instance running in the dir while regenerating its contents,
    // instead of today where it has to be restarted peridoically because the dir it's running in gets deleted.

    remove_dir_all(arena, build_dir)?;
    create_dir_all(arena, build_dir)?;

    // Write CSS, JS, and favicon
    // (The HTML requires more work!)
    write(
        arena,
        &build_dir.join("search.js"),
        assets.search_js.as_ref(),
    )?;
    write(
        arena,
        &build_dir.join("styles.css"),
        assets.styles_css.as_ref(),
    )?;
    write(
        arena,
        &build_dir.join("favicon.svg"),
        assets.favicon_svg.as_ref(),
    )?;

    Ok(())
}

pub fn create_dir_all<'a>(_arena: &'a Bump, dir: &Path) -> Result<(), Problem> {
    // TODO in the future, this will make use of the arena when we're using bump-allocated paths.
    fs::create_dir_all(dir).or_else(|io_err| {
        // If we failed to create the directory because it already exists, that's fine.
        match io_err.kind() {
            ErrorKind::AlreadyExists => Ok(()),
            _ => Err(Problem::FailedToCreateDir(dir.to_path_buf(), io_err)),
        }
    })
}

pub fn remove_dir_all<'a>(_arena: &'a Bump, dir: &Path) -> Result<(), Problem> {
    // TODO in the future, this will make use of the arena when we're using bump-allocated paths.
    fs::remove_dir_all(dir).or_else(|io_err| {
        // If we failed to delete the directory because it already doesn't exist, that's fine.
        match io_err.kind() {
            ErrorKind::NotFound => Ok(()),
            _ => return Err(Problem::FailedToDeleteDir(dir.to_path_buf(), io_err)),
        }
    })
}

pub fn write<'a>(_arena: &'a Bump, file: &Path, contents: impl AsRef<[u8]>) -> Result<(), Problem> {
    // TODO in the future, this will make use of the arena when we're using bump-allocated paths.
    fs::write(&file, contents.as_ref())
        .map_err(|io_err| Problem::FailedToWrite(file.to_path_buf(), io_err))
}
