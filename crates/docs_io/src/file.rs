use crate::problem::Problem;
use bumpalo::Bump;
use std::{
    fs,
    io::{self, ErrorKind},
    path::Path,
};

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

    if let CreationOutcome::AlreadyExisted = create_dir_all(arena, build_dir)? {
        delete_dir_contents(arena, build_dir)?;
    }

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

#[derive(Debug)]
pub enum CreationOutcome {
    AlreadyExisted,
    DidNotExist,
}

fn create_dir_all<'a>(_arena: &'a Bump, dir: &Path) -> Result<CreationOutcome, Problem> {
    // TODO in the future, this will make use of the arena when we're using bump-allocated paths.
    match fs::create_dir_all(dir) {
        Ok(_) => Ok(CreationOutcome::DidNotExist),
        Err(io_err) => {
            // If we failed to create the directory because it already exists, that's fine.
            match io_err.kind() {
                ErrorKind::AlreadyExists => Ok(CreationOutcome::AlreadyExisted),
                _ => Err(Problem::FailedToCreateDir(dir.to_path_buf(), io_err)),
            }
        }
    }
}

fn delete_dir_contents<'a>(arena: &'a Bump, dir: &Path) -> Result<(), Problem> {
    let to_problem = |io_err: io::Error| Problem::FailedToDeleteDir(dir.to_path_buf(), io_err);

    if dir.is_dir() {
        for entry in fs::read_dir(dir).map_err(to_problem)? {
            let entry = entry.map_err(to_problem)?;
            let path = entry.path();

            if path.is_dir() {
                delete_dir_contents(arena, &path)?;
                fs::remove_dir(&path).map_err(to_problem)?;
            } else {
                fs::remove_file(&path).map_err(to_problem)?;
            }
        }
    } else {
        fs::remove_dir_all(dir)
            .or_else(|io_err| {
                // If we failed to delete the directory because it already doesn't exist, that's fine.
                match io_err.kind() {
                    ErrorKind::NotFound => Ok(()),
                    _ => Err(io_err),
                }
            })
            .map_err(to_problem)?;

        create_dir_all(arena, dir)?;
    }

    Ok(())
}

pub fn write<'a>(
    _arena: &'a Bump,
    file_path: impl AsRef<Path>,
    contents: impl AsRef<[u8]>,
) -> Result<(), Problem> {
    let file_path = file_path.as_ref();

    // TODO in the future, this will make use of the arena when we're using bump-allocated paths.
    fs::write(file_path, contents.as_ref())
        .map_err(|io_err| Problem::FailedToWrite(file_path.to_path_buf(), io_err))
}
