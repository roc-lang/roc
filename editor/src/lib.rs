use std::{io, path::Path};

pub mod event_loop;
pub mod gfx;
pub mod text_state;
pub mod window;

/// The editor is actually launched from the CLI if you pass it zero arguments,
/// or if you provide it 1 or more files or directories to open on launch.
pub fn launch(_filepaths: &[&Path]) -> io::Result<()> {
    event_loop::run();

    Ok(())
}
